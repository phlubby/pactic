import re
import string


class LuaLexer(object):
    def __init__(self, log_level):
        self.all_ids = {}
        self.log_level = log_level

    @staticmethod
    def is_valid_identifier_start_char(c):
        return c.isalpha() or c == '_'

    @staticmethod
    def is_valid_identifier_char(c):
        return LuaLexer.is_valid_identifier_start_char(c) or c.isdigit()

    @staticmethod
    def is_valid_function_identifier_char(c):
        return LuaLexer.is_valid_identifier_char(c) or c == '.'

    def is_concat_safe_id(self, id):
        return self.all_ids[id]['concat_safe']

    def analyze(self, source):
        # source = self.source
        s = source.decode('utf-8')
        s += ' '

        immutable_pos = '|'
        quoted_pos = '"'
        # First (try to) mask quoted string.
        # [TODO] single-quoted strings (everywhere).
        i = 0
        t = ''
        while i < len(s):
            if s[i] == '"':
                match = 'load'
                quoted_str_is_code = (i >= len(match)
                                      and s[i-len(match):].startswith(match))

                if quoted_str_is_code:
                    t = t[:-len(match)] + immutable_pos * len(match)
                t += quoted_pos  # '|' if quoted_str_is_code else s[i]
                i += 1

                while s[i] != '"' or s[i-1] == '\\':
                    t += s[i] if quoted_str_is_code else quoted_pos
                    i += 1

                end = i + 1

                t += quoted_pos  # '|' if quoted_str_is_code else s[i]

                i += 1
                continue
            t += s[i]
            i += 1
        s = t

        def extract_id(s):
            for i, c in enumerate(s):
                if LuaLexer.is_valid_identifier_start_char(c):
                    break
            else:
                return None

            begin = i
            while i < len(s):
                if not LuaLexer.is_valid_identifier_char(s[i]):
                    break
                i += 1

            return s[begin:i]

        def set_id_(id, prop=None, value=None):
            if id not in all_ids:
                all_ids[id] = {}
                all_ids[id]['writes'] = 0
                all_ids[id]['offsets'] = []
                all_ids[id]['concat_safe'] = True

            if prop:
                if prop not in all_ids[id]:
                    all_ids[id][prop] = value if value else 0

                if not value:
                    all_ids[id][prop] += 1

        def set_id_prop(id, prop):
            return set_id_(id, prop, True)

        def inc_id_prop(id, prop):
            return set_id_(id, prop)

        def extract_and_set_id_prop(id, prop):
            id = extract_id(id)
            if id:
                set_id_(id, prop)
            return id

        def extract_id_reverse(s):
            begin = len(s)
            while start > 0:
                begin -= 1
                if not LuaLexer.is_valid_identifier_char(s[begin]):
                    break
            else:
                return None

            begin += 1

            if begin == len(s):
                return None

            if not LuaLexer.is_valid_identifier_start_char(s[begin]):
                begin += 1

            if begin == len(s):
                return None

            return s[begin:]

        all_ids = {}
        self.token_concat_safe_offsets = {}
        for m in re.finditer('[A-Za-z_][A-Za-z0-9_]*', s):
            name = m.group()

            start = m.start()

            # False positive for a hex number?
            do_value_check = False
            if name[0] in ['x', 'X']:
                if (start and s[start-1] == '0'):
                    if start > 1 and s[start-2] in string.hexdigits:
                        pass
                    else:
                        do_value_check = True
            elif name[0] in ['e', 'E']:
                if start and (s[start-1].isdigit()) or s[start-1] == '.':
                    do_value_check = True

            if do_value_check:
                end = m.end()
                i = start + 1
                while i < end:
                    # hex values not allowed with decimal scientific notation,
                    # but since that's an error check for them instead of just
                    # digits (which doesn't work for hex notation).
                    if s[i] not in string.hexdigits:
                        break
                    i = i + 1

                assert i != start
                if i == end:
                    # print("Discarding:", name)
                    continue
                start = i
                name = s[start:end]

            set_id_(name)

            concat_safe = True
            if start > 0:
                left_ofs = start-1
                prev_c = s[left_ofs]
                if prev_c == '.':
                    set_id_prop(name, 'member')
                    parent = extract_id_reverse(s[:left_ofs])
                    if parent:
                        all_ids[name]['parent'] = parent

                concat_safe = not prev_c.isnumeric()
                if not concat_safe:
                    all_ids[name]['concat_safe'] = False

            self.token_concat_safe_offsets[start] = concat_safe

            all_ids[name]['offsets'].append(start)

        def fetch_left(ofs):
            while ofs > 0:
                if s[ofs] != ' ':
                    return s[ofs]
                ofs = ofs - 1
            # assert False
            return ' '

        def fetch_right_offset(ofs):
            while ofs < len(s):
                c = s[ofs]
                if c != ' ':
                    next_c = s[ofs+1] if ofs + 1 < len(s) else ' '
                    if c == '.' and next_c == '.':
                        # String concatenation, not an instance.
                        c = ' '
                    if c == '=' and next_c == '=':
                        # Comparing, not writing
                        c = ' '
                    return c, ofs
                ofs = ofs + 1

            # assert False
            return ' ', ofs

        def fetch_right(ofs):
            return fetch_right_offset(ofs)[0]

        for k in all_ids:
            for ofs in all_ids[k]['offsets']:
                ofs += len(k)
                c = fetch_right(ofs)
                prop = None
                incr = False
                if c == '=':
                    set_id_prop(k, 'declared')
                    prop = 'writes'
                    incr = True
                elif c == '.':
                    prop = 'class'
                elif c == '(':
                    prop = 'function'
                # elif c in [')', ',']:
                #     prop = 'declared'  # more like used, for unknown vars

                if prop:
                    if incr:
                        all_ids[k][prop] += 1
                    else:
                        all_ids[k][prop] = True

        keywords = ('and,break,do,else,elseif,end,false,for,function,if,in,'
                    'local,nil,not,or,repeat,return,'
                    'then,true,until,while'.split(','))

        for k in keywords:
            set_id_prop(k, 'reserved')
            set_id_prop(k, 'keyword')

        funcs = ('btn,btnp,circ,cirb,cls,exit,font,load,line,'
                 'map,memcpy,music,peek,peek4,'
                 'pix,pmem,poke,poke4,print,rect,rectb,'
                 'spr,textri,TIC,time,tri'.split(','))

        tic_classes = 'math,string,table'.split(',')
        for k in all_ids:
            if k in funcs or k in tic_classes:
                all_ids[k]['reserved'] = True

        reserved = ['OVR', 'SCN', 'TIC']

        for m in re.finditer(r'function ([A-Za-z_.: 0-9]+)\(', s):
            name = m.group(1)

            parent = None
            splits = [':', '.']
            for split in splits:
                if split in name:
                    splits = name.split(split)
                    parent, func = splits[0], splits[1]
                    break
            else:
                func = name

            if parent:
                parent = extract_and_set_id_prop(parent, 'class')
                # Don't treat it as declared!

            func = extract_and_set_id_prop(func, 'function')

            if not func:
                continue

            set_id_prop(func, 'declared')

            if parent:
                set_id_prop(func, 'member')

            if func in reserved:
                assert func != 's'
                set_id_prop(func, 'reserved')

            paren_count = 1
            # TODO
            ofs = m.end()
            while True:
                c, ofs = fetch_right_offset(ofs)
                if c == '(':
                    paren_count += 1
                elif c == ')':
                    paren_count -= 1
                    if not paren_count:
                        break
                    else:
                        ofs += 1
                        continue
                elif c == ',':
                    ofs += 1
                    continue
                elif not LuaLexer.is_valid_identifier_start_char(c):
                    break

                name = extract_id(s[ofs:])
                set_id_prop(name, 'local')
                set_id_prop(name, 'declared')

                ofs += len(name)

        for k in all_ids:
            try:
                parent = all_ids[k]['parent']
                if 'declared' not in all_ids[parent]:
                    set_id_prop(k, 'reserved')
            except KeyError:
                continue

        # tic_classes = 'math,string,table'.split(',')
        # for kw in tic_classes.split(','):
        #     match = kw + '.'
        #     s = s.replace(match, fixed_pos * len(kw) + '.')

        masked = '*'

        def mask(s, prop=None):
            for k in all_ids:
                # if prop and prop not in all_ids[k]:
                #     continue
                for offset in all_ids[k]['offsets']:
                    s = s[:offset] + s[offset:].replace(k, masked * len(k), 1)
            return s

        t = s
        t = mask(t, 'keyword')
        # t = mask(t, 'reserved')
        # t = mask(t, 'function')
        # t = mask(t, 'declared')
        # t = mask(t, 'writes')
        t = mask(s)

        for m in re.finditer('[A-Za-z_]+', t):
            for c in m.group():
                assert c in string.hexdigits or c in ['X', 'x'], c

        # methods = 'abs,atan,cos,insert,min,max,' \
        #     'randomseed,random,remove,sin,tan'

        called_funcs = {}
        # print("CALLS:")
        for m in re.finditer(r'([A-Za-z_][A-Za-z0-9_]*) *\(', s):
            name = extract_id(m.group(1))
            if name in keywords:
                continue

            if name not in called_funcs:
                called_funcs[name] = []
            called_funcs[name].append(m.start(1))

        for func in called_funcs:
            assert func in all_ids, func
            assert 'function' in all_ids[func], func + ":" + str(all_ids[func])

        # print("Unknown classes:")
        # for k in all_ids:
        #     if 'class' in all_ids[k]:
        #         if 'declared' not in all_ids[k]:
        #             print(k)
        # print("Unknown funcs:")
        unknown_funcs = []
        for k in all_ids:
            if 'function' in all_ids[k]:
                # print(k)
                if 'declared' not in all_ids[k]:
                    # print(k)
                    unknown_funcs.append(k)
        # print(unknown_funcs)
        # print("---------------------------------------")
        # print("Declared funcs:")
        declared_funcs = []
        for k in all_ids:
            if 'function' in all_ids[k]:
                # print(k)
                if 'declared' in all_ids[k]:
                    declared_funcs.append(k)
        # print(declared_funcs)
        # print("---------------------------------------")

        # print("Declared tokens:")
        declared_ids = []
        for k in all_ids:
            if 'reserved' in all_ids[k]:
                continue
            if 'declared' in all_ids[k]:
                declared_ids.append(k)
        # print(declared_tokens)
        # print("---------------------------------------")

        # print("Unknown vars:")
        unknown_vars = []
        for k in all_ids:
            if 'reserved' in all_ids[k]:
                continue
            if 'declared' in all_ids[k]:
                continue
            if 'function' in all_ids[k]:
                if not all_ids[k]['writes']:
                    continue
            if 'class' in all_ids[k]:
                continue
                # assert 'declared' in all_ids[k]
            # print(k, all_ids[k])
            unknown_vars.append(k)
        known_ids = declared_ids
        # print(unknown_vars)
        # print("---------------------------------------")

        known_ids += unknown_vars

        ids_weight = {}
        for k in known_ids:
            num_refs = len(all_ids[k]['offsets'])
            # [TICKLE]
            # ids_weight[k] = num_refs + num_refs * (len(k) - 1)
            ids_weight[k] = num_refs * (len(k))

        known_ids_freq = sorted(ids_weight,
                                key=lambda k: ids_weight[k], reverse=True)

        self.ids_weight = ids_weight

        self.new_id_offsets = {}
        # [TODO] Feed and update all id offsets, or just the changed ones?
        # for k in all_ids:
        for k in known_ids:
            self.new_id_offsets[k] = all_ids[k]['offsets'][:]

        self.ids_concat_safe = {k: all_ids[k]['concat_safe']
                                for k in known_ids_freq}

        self.known_ids = known_ids
        self.all_ids = all_ids
        self.known_ids_freq = known_ids_freq

        return s.strip()
