import re
import string

from collections import OrderedDict

class LuaLexer(object):
    def __init__(self, log_level):
        self.char_identifier_count = len(string.ascii_letters + '_')
        self.log_level = log_level

    valid_dec_chars = string.hexdigits + '.'
    valid_hex_chars = valid_dec_chars + 'Pp'

    @staticmethod
    def can_dec_concat(c):
        return c in LuaLexer.valid_dec_chars

    @staticmethod
    def can_hex_concat(c):
        return c in LuaLexer.valid_hex_chars

    def is_concat_safe_slot(self, slot):
        info = self.slots_concat_info[slot]
        return not ('d' in info or 'x' in info)

    @staticmethod
    def is_valid_identifier_start_char(c):
        return c.isalpha() or c == '_'

    @staticmethod
    def is_valid_identifier_char(c):
        return LuaLexer.is_valid_identifier_start_char(c) or c.isdigit()

    @staticmethod
    def quotes():
        return '"' + "'"

    @staticmethod
    def opp_quote(c):
        quotes = LuaLexer.quotes()
        return quotes[quotes.find(c)-1]

    @staticmethod
    def mask_source(s):
        immutable_pos = '|'
        i = 0
        t = ''
        quotes = LuaLexer.quotes()
        while i < len(s):
            if s[i] in quotes:
                main_quote = quoted_pos = s[i]
                other_quote = LuaLexer.opp_quote(main_quote)
                match = 'load'
                quoted_str_is_code = (i >= len(match)
                                      and s[i-len(match):].startswith(match))

                if quoted_str_is_code:
                    t = t[:-len(match)] + immutable_pos * len(match)
                t += quoted_pos  # '|' if quoted_str_is_code else s[i]
                i += 1

                in_other_quote = False
                while s[i] != main_quote or s[i-1] == '\\':
                    if s[i] == other_quote and s[i-1] != '\\':
                        in_other_quote = not in_other_quote
                        quoted_pos = (other_quote if in_other_quote
                                      else main_quote)

                    if quoted_str_is_code:
                        t += quoted_pos if in_other_quote else s[i]
                    else:
                        t += quoted_pos
                    i += 1

                t += quoted_pos

                i += 1
                continue
            t += s[i]
            i += 1

        return t

    @staticmethod
    def unescaped_quote_presence(s):
        quotes = LuaLexer.quotes()
        for q in quotes:
            s = s.replace('\\' + q, '@@')

        present = ''
        for q in quotes:
            if s.count(q):
                present += q

        return present

    def check_whitespace(self, s):
        masked = LuaLexer.mask_source(s)

        s = ' ' + s
        masked = ' ' + masked

        begin_offset = 1
        t = s[:begin_offset]

        seen = ' '  # (a)lphabet, (d)ecimal, he(x)a. ' ' for all else.
        num_exp_seen = length = 0

        # k=offset, v=space_required? (as-is)
        self.ws_required = {}
        self.seen = s[0]  # space
        i = 0

        for i, c in enumerate(masked[begin_offset:-1]):
            i += begin_offset

            prev = t[-1:]
            prev_seen = seen
            reset = False
            if c != ' ':
                c = s[i]
                t += c
                if c in 'xX':
                    if prev == '0' and length < 2:
                        # 0 in front and only one digit -> hex
                        seen = 'x'
                    else:
                        # No 0 in front or e.g. "10" -> id
                        reset = True
                elif c.isdigit():
                    if seen == ' ':
                        seen = 'd'
                elif c in 'eE':
                    if seen == 'd':
                        num_exp_seen += 1
                        if num_exp_seen == 2:
                            reset = True
                    elif seen != 'x':
                        reset = True
                elif c in string.hexdigits:
                    if seen != 'x':
                        reset = True
                elif c in 'pP':
                    if seen == 'x':
                        num_exp_seen += 1
                        if num_exp_seen == 2:
                            reset = True
                    else:
                        reset = True
                elif c == '.':
                    if seen not in 'dx':
                        reset = True
                elif c.isalpha() or c in '_':
                    reset = True
                else:
                    reset = True

                if not reset and prev_seen != seen:
                    length = 0

                length += 1
            else:
                def next_non_space():
                    j = i+1
                    while j < len(s):
                        c = s[j]
                        if c != ' ':
                            return c
                        j += 1

                    # assert False
                    return ' '

                next = next_non_space()

                required = False
                if seen == 'a':
                    if self.is_valid_identifier_char(next):
                        required = True
                elif seen == 'd':
                    if self.can_dec_concat(next):
                        required = True
                    elif next in 'xX':
                        if prev == '0' and length == 1:
                            required = True
                elif seen == 'x':
                    if self.can_hex_concat(next):
                        required = True

                index = i - begin_offset
                assert index not in self.ws_required

                assert seen in ' adx'
                self.ws_required[i-begin_offset] = required

                if required:
                    reset = True
                    t += ' '

            if reset:
                if self.is_valid_identifier_start_char(c):
                    seen = 'a'
                else:
                    seen = ' '
                length = num_exp_seen = 0

            self.seen += seen

        # Repeat last purpose for bounds checking convenience.
        self.seen += seen

        t += s[i+begin_offset]

    def analyze(self, source):
        s = source.decode('utf-8')
        self.check_whitespace(s)
        s = self.mask_source(s)
        s += ' '

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
                all_ids[id]['concat_info'] = ' '

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

        all_ids = OrderedDict()
        prev_name = ''
        # K=offset V= body-start, body-end, num-ends-needed, full-name, name
        self.func_offsets = func_offsets = {}
        func_stack = []
        for m in re.finditer('[A-Za-z_][A-Za-z0-9_]*', s):
            name = m.group()

            start = m.start()

            if name[0] in 'EXex':
                # Check for a concatenated ID from the right.
                end = m.end()
                i = end
                while i >= start and self.seen[i] == 'a':
                    i -= 1

                if i == end:
                    continue

                start = i

                name = s[start:end]

            set_id_(name)

            if start > 0:
                left_ofs = start-1
                prev_c = s[left_ofs]
                if prev_c == '.':
                    set_id_prop(name, 'member')
                    if start and 'a' in self.seen[start - 1]:
                        all_ids[name]['parent'] = prev_name
                        if name in ['abs', 'atan', 'cos', 'floor',
                                    'max', 'min', 'pi', 'random', 'sin', 'sqrt', ]:
                            set_id_prop(name, 'reserved')

            if len(func_stack):
                f = func_offsets[func_stack[-1]]

                if name in ['for', 'if', 'while']:
                    f['num-ends-needed'] += 1
                elif name == 'end':
                    f['num-ends-needed'] -= 1
                    if not f['num-ends-needed']:
                        f['body-end'] = start

                        del func_stack[-1]

            if name == 'function':
                f = func_offsets[start] = {}
                f['num-ends-needed'] = 1

                c = fetch_right(m.end())
                if self.is_valid_identifier_start_char(c):
                    f['name'] = ''
                else:
                    f['name'] = None

                func_stack += [start]

                if len(func_stack) > 1:
                    func_offsets[func_stack[-1]]['inner'] = True

            seen = self.seen[start]
            assert seen != 'a', s[start]

            if seen not in all_ids[name]['concat_info']:
                all_ids[name]['concat_info'] += seen

            all_ids[name]['offsets'].append(start)

            prev_name = name

        # Not all functions ended? (could be a problem with the source)
        # assert not func_stack

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

        funcs = ('btn,btnp,circ,cirb,clip,cls,exit,fget,font,fset,'
                 'key,keyp,line,load,'
                 'map,memcpy,mget,mouse,mset,music,OVR,peek,peek4,'
                 'pix,pmem,poke,poke4,print,rect,rectb,reset,'
                 'SCN,sfx,spr,sync,'
                 'textri,TIC,time,trace,tri,tstamp'.split(','))

        tic_classes = 'math,string,table'.split(',')
        for k in all_ids:
            if k in funcs or k in tic_classes:
                all_ids[k]['reserved'] = True

        func_match = 'function '
        for m in re.finditer(func_match + r'([A-Za-z_.: 0-9]+)\(', s):
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

            start = m.start()
            assert start in func_offsets
            func_offsets[start]['name'] = func
            full_name = s[start+len(func_match):m.end() - 1]
            func_offsets[start]['full-name'] = full_name

            set_id_prop(func, 'declared')

            if parent:
                set_id_prop(func, 'member')

            paren_count = 1
            # TODO
            ofs = m.end()
            argc = 0
            while True:
                c, ofs = fetch_right_offset(ofs)
                ofs += 1
                if c == '(':
                    paren_count += 1
                elif c == ')':
                    paren_count -= 1
                    if not paren_count:
                        func_offsets[start]['body-start'] = ofs
                        f = func_offsets[start]
                        break
                    else:
                        continue
                elif c == ',':
                    argc += 1
                    continue
                elif c in ' .':
                    continue
                elif not LuaLexer.is_valid_identifier_start_char(c):
                    assert False, func
                    break

                ofs -= 1
                name = extract_id(s[ofs:])
                set_id_prop(name, 'local')
                set_id_prop(name, 'declared')

                ofs += len(name)
            func_offsets[start]['argc'] = argc

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

        def mask_mutable_ids(s, prop=None):
            for k in all_ids:
                if 'reserved' in all_ids[k]:
                    continue
                if prop and prop not in all_ids[k]:
                    continue
                for offset in all_ids[k]['offsets']:
                    s = s[:offset] + s[offset:].replace(k, '@' * len(k), 1)
            return s

        masked = mask_mutable_ids(source.decode('utf-8'))
        self.masked_source = masked

        for m in re.finditer('[A-Za-z_]+', t):
            for c in m.group():
                assert c in self.valid_hex_chars or c in 'Xx', c

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

        unknown_funcs = []
        for k in all_ids:
            if 'function' in all_ids[k]:
                if 'declared' not in all_ids[k]:
                    unknown_funcs.append(k)

        declared_funcs = []
        for k in all_ids:
            if 'function' in all_ids[k]:
                if 'declared' in all_ids[k]:
                    declared_funcs.append(k)

        declared_ids = []
        for k in all_ids:
            if 'reserved' in all_ids[k]:
                continue
            if 'declared' in all_ids[k]:
                declared_ids.append(k)

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
            unknown_vars.append(k)
        known_ids = declared_ids

        known_ids += unknown_vars

        ids_weight = OrderedDict()
        for k in known_ids:
            num_refs = len(all_ids[k]['offsets'])
            # [TICKLE]
            ids_weight[k] = num_refs * (len(k))

        known_ids_freq = sorted(ids_weight,
                                key=lambda k: ids_weight[k], reverse=True)

        self.ids_weight = ids_weight

        self.id_offsets = []
        # [TODO] Feed and update all id offsets, or just the changed ones?
        # for k in all_ids:
        for k in known_ids:
            self.id_offsets.append(all_ids[k]['offsets'][:])

        self.slots_concat_info = []
        for k in known_ids:
            self.slots_concat_info.append(all_ids[k]['concat_info'])

        self.known_ids = known_ids
        self.all_ids = all_ids
        self.known_ids_freq = known_ids_freq
        self.analyzed_source = source

        return s.strip()

    def mutable_id_count(self):
        return len(self.known_ids)
