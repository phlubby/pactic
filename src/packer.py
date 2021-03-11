# -*- coding: utf-8 -*-

from __future__ import print_function

import itertools
import math
import operator
import os
import re
import string
import zlib

from src.common import log, log_deep, log_deeper, log_deepest, \
    log_error, set_log_info, byte_length
from src.ticfile import Tic
from src.zopfli import zopfli_compress

import src.lualexer


def compress_zlib(uncomp):
    # comp = zlib.compress(uncomp, zlib.Z_BEST_COMPRESSION)
    # create baseline of no compression
    # comp = zlib.compress(uncomp, 0)
    # comp_size = len(comp)

    new_compressed_data = None
    new_compressed_size = 0

    comp_size = 256 + len(uncomp)

    # testing with different compression_levels
    # for i in range(0, 10):
    #     new_compressed_data = zlib.compress(uncomp, i)
    #     new_compressed_size = len(new_compressed_data)
    #     if new_compressed_size < comp_size:
    #         bestcompression_level = i
    #         comp = new_compressed_data
    #         comp_size = new_compressed_size

    # testing with compressobj, compressionlevels, and strategies
    # https://docs.python.org/3/library/zlib.html
    for i1 in range(0, 9 + 1):
        for i2 in range(0, 5):
            # i2 = zlib.Z_DEFAULT_STRATEGY (0), zlib.Z_FILTERED,
            #  zlib.Z_HUFFMAN_ONLY, zlib.Z_RLE, zlib.Z_FIXED (4)
            # 9 = zlib.DEF_MEM_LEVEL (default 8)
            compress_obj = zlib.compressobj(i1, zlib.DEFLATED, 15, 9, i2)
            new_compressed_data = compress_obj.compress(uncomp)
            new_compressed_data += compress_obj.flush()
            new_compressed_size = len(new_compressed_data)

            if new_compressed_size < comp_size:

                comp = new_compressed_data
                comp_size = new_compressed_size
                comp_info = 'level {}, strat {}'.format(i1, i2)

    return comp, comp_info


class Compressor(object):
    def __init__(self):
        self.data_out = None
        self.checksum = None
        self.info = ''

    def data(self):
        return self.data_out

    def length(self):
        if self.data_out:
            return len(self.data_out)

        return len(self.data)

    def compress(self, data, perm_info=''):
        self.data = data
        data_out, info = self.do_compress(data)
        checksum = None
        if not data_out:
            # assert False
            return None

        # we dont want/need the checksum, so lets remove it
        checksum_size = 4
        checksum = data_out[-checksum_size:]
        data_out = data_out[:-checksum_size]

        size = len(data_out)

        if size < self.length():
            self.data_out = data_out[:]
            if checksum is not None:
                self.checksum = checksum

            has_desc = len(info) or len(perm_info)
            s = ''
            if has_desc:
                s = '(' + info

                if perm_info:
                    if info:
                        s += ' '
                    s += perm_info
                s += ')'
            self.info = s

        return data_out

    def decompressed_source(self):
        if self.checksum:
            # 15 = wbits
            data = zlib.decompress(self.data_out + self.checksum, 15)
        else:
            data = self.data_out
        return data


class CompressorZlib(Compressor):
    def __str__(self):
        return 'zlib'

    @staticmethod
    def do_compress(data):
        return compress_zlib(data)


class CompressorZopfli(Compressor):
    def __str__(self):
        return 'Zopfli'

    @staticmethod
    def do_compress(data):
        return zopfli_compress(data), ""


def strip_lua_comments(data):
    s = ''
    lines = data.decode('utf-8').splitlines()

    lua_begin_multi_comment = '--[['

    in_multi_comment = False
    re_multi_begin = re.compile(r'--\[=*\[')
    num_equals = 0

    def masked_line(line):
        quotes = "'" + '"'

        masked = line
        for c in quotes:
            masked = masked.replace('\\' + c, '@@')
        return masked

    # Strip the comments
    for line_nr, line in enumerate(lines):
        ori_line = line.strip()
        line = masked_line(ori_line)

        multi_comment_end = ']{}]'.format('=' * num_equals)

        def find_end_comment():
            return line.find(multi_comment_end)

        if in_multi_comment:
            idx = find_end_comment()
            if idx == -1:
                continue
            else:
                in_multi_comment = False
                line = line[idx + len(multi_comment_end):].strip()

        comment = '--'

        idx = 0
        new_line = ''
        in_quote_char = False
        while True:
            if in_quote_char:
                while idx < len(line) and line[idx] != in_quote_char:
                    new_line += ori_line[idx]
                    idx += 1

                try:
                    new_line += ori_line[idx]
                    idx += 1
                except IndexError:
                    pass
                # [TODO] What else can be done, Lua wants quote to end on line.
                in_quote_char = False

            begin = idx
            idx = line.find(comment, idx)
            if idx == -1:
                idx = len(line)
                new_line += ori_line[begin:idx]
                break

            # part of a quote?
            in_quote_char = '"'
            quote_count = line[begin:idx].count(in_quote_char)
            if quote_count & 1:
                quote_idx = ori_line.find(in_quote_char, begin, idx)
                idx = quote_idx + 1
                new_line += ori_line[begin:idx]
                continue
            in_quote_char = False
            m = re_multi_begin.search(line[idx:])

            # No match for multi-line comment or it appears AFTER a "--".
            if not m or m.start(0) != 0:
                new_line += ori_line[begin:idx]
                break

            # "---[[" (with triple dashes) does not start a comment.
            if idx and line[idx-1] == '-':
                new_line += ori_line[begin:idx]
                continue

            new_line += ori_line[begin:idx]

            in_multi_comment = True
            num_equals = m.end(0) - m.start(0) - len(lua_begin_multi_comment)
            multi_comment_end = ']{}]'.format('=' * num_equals)

            idx_end_comment = line.find(multi_comment_end, idx + m.end(0))
            if idx_end_comment != -1:
                in_multi_comment = False

                end = idx_end_comment+len(multi_comment_end)
                # new_line += line[begin:idx]# + line[end:]
                idx = end

                continue
            # else: Keep everything before the start of the comment.

            new_line += ori_line[begin:idx]
            break

        line = new_line

        line = line.strip()
        if line:
            s += line + ' '
            # s += line + '\n'

    # [TODO] Don't blindly replace these.
    # Replace all whitespace and EOLs with space (0x20) to consistently use
    # the same whitespace char (and variate with that).
    for find in ['\r\n', '\t', '\r', '\n']:
        s = s.replace(find, ' ')

    # Remove self added ending space at end of lines.
    s = s[:-1]

    return s.encode('utf-8')


packers = [CompressorZlib, CompressorZopfli]


def packer_names():
    return [str(k()) for k in packers]


class Packer(src.lualexer.LuaLexer):
    def __init__(self, args, tic):
        self.args = args
        self.tic = tic
        self.log_level = args.verbose
        self.ran_minify = False
        super(Packer, self).__init__(self.log_level)

        self.best = None

        self.packers = []
        for packer in packers:
            packer_name = str(packer())
            arg_name = 'no_' + packer_name.lower()
            if not args.__dict__[arg_name]:
                pack = packer()
                data, info = pack.do_compress(bytes())
                if data:
                    self.packers.append(pack)

    def write_tic(self, verbose=0):
        # if self.best:
        #     assert self.best_source == self.best.decompressed_source()
        self.tic.write(self.args.filename_out,
                       self.best_source,
                       self.best.data_out,
                       verbose)

    def data_in(self):
        return self.data

    def compare(self, s1, s2):
        # list of [compressed-length, string]
        packed = []
        sources = [s1, s2]
        best_index = 0

        for s in sources:
            for pack in self.packers:
                data_out, info = pack.do_compress(s)
                length = len(data_out)
                packed.append([length, s])
                if length < packed[best_index][0]:
                    best_index = len(packed) - 1

        num_packers = len(self.packers)

        # [TODO] This can fail (zlib < Zopfli)
        # if num_packers > 1:
        #     if not (best_index & 1):
        #         assert packed[best_index+1][1] == packed[best_index][1]

        other_best_index = (best_index + num_packers) % len(packed)

        if packed[other_best_index][0] == packed[best_index][0]:
            return packed[best_index], None

        return packed[best_index], packed[other_best_index]

    def compress(self, data=None, perm_info=''):
        if data:
            self.data = data
        else:
            assert False

        best_length = self.best.length() if self.best else 0
        improved = []
        for pack in self.packers:
            pack.compress(data, perm_info)

            # if pack.length() <= old_best:
            #     pack.info = perm_info
            #     pack.log_new_best(self.log_level)
            length = pack.length()
            if not self.best or length < best_length:
                best_length = length
                self.best = pack
                if pack.data_out:
                    self.best.data_out = pack.data_out[:]
                    improved.append(pack)

                self.best_source = data[:]

        for pack in improved[:-1]:
            self.log_new_best(pack)

        if improved:
            self.write_tic()
            self.log_new_best(improved[-1])

        return self.best.data_out

    def log_new_best(self, pack):
        if not self.ran_minify:
            return

        s = "New best {}".format(byte_length(pack.length()))

        if self.log_level:
            s += ": {}".format(pack)

        if self.log_level > 1:
            s += " " + pack.info

        print(s)

    def replace(self, s, old_id, new_id, var_iter=None):
        t = ''
        prev_offset = 0

        offsets = None

        for j, offset in enumerate(self.new_id_offsets[old_id]):
            t += s[prev_offset:offset]

            final_replacement = new_id[:]
            assert ' ' not in final_replacement, final_replacement

            t += final_replacement
            prev_offset = offset + len(old_id)

            if len(old_id) != len(final_replacement):
                if not offsets:
                    offsets = {}
                    for k in self.new_id_offsets:
                        offsets[k] = self.new_id_offsets[k][:]
                for k in offsets:
                    for i, ofs in enumerate(self.new_id_offsets[k]):
                        if i == 0 and k == old_id:
                            continue

                        if ofs > offset:
                            offsets[k][i] -= (len(old_id)
                                              - len(final_replacement))

        t += s[prev_offset:]

        if offsets:
            return t, offsets
        else:
            return t, self.new_id_offsets

    def strip(self, source):
        source = strip_lua_comments(source)
        self.analyze(source)

        s = source.decode('utf-8')
        masked = self.mask_source(s)

        t = ''

        for i, c in enumerate(masked):
            if c != ' ':
                assert i not in self.ws_required, i
                t += s[i]
                continue

            assert i in self.ws_required, i

            required = self.ws_required[i]
            if required:
                t += ' '

        t = t.strip()
        return t.encode('utf-8')

    def xform_funcs(self, source):
        self.analyze(source)
        s = source.decode('utf-8')
        m = self.mask_source(s)

        func_ofs = [k for k in self.func_offsets
                    if 'inner' not in self.func_offsets[k]]
        t = ''
        i = 0
        num_xformed = 0
        for offset in func_ofs:
            t += s[i:offset]
            f = self.func_offsets[offset]

            i = offset

            if 'body-start' not in f or 'body-end' not in f:
                assert not f['name'], f
                continue

            if f['argc']:
                continue

            start = f['body-start']
            end = f['body-end']

            if end - start <= 0:
                continue

            quote_char = "'"
            quotes = self.unescaped_quote_presence(m[start:end])
            if len(quotes) > 1:
                continue
            elif len(quotes) == 1:
                quote_char = self.opp_quote(quotes[0])

            t += (f['full-name'] + '=load'
                  + quote_char + s[start:end] + quote_char)

            i = end + len("end")
            num_xformed += 1

        t += s[i:]

        return num_xformed, self.strip(t.encode('utf-8'))

    def minify(self, source):
        if not self.args.no_transform:
            num_xformed, source_xformed = self.xform_funcs(source)
            if num_xformed:
                # [TODO] Compare minified versions instead?
                best, next = self.compare(source, source_xformed)
                if not next:
                    log_deep("Same (un)transformed lengths ({}), using original"
                             .format(byte_length(best[0])))
                elif best[1] == source_xformed:
                    source = best[1]
                    log_deep("Using {} transformation(s) ({} < {})".format(
                        num_xformed,
                        byte_length(best[0]),
                        byte_length(next[0]),
                    ))

        self.analyze(source)

        source = source.decode('utf-8')
        all_ids = self.all_ids
        known_ids = self.known_ids
        known_ids_freq = self.known_ids_freq

        pr = ' '.join(["{}: {},".format(
            v, self.ids_weight[v]) for v in known_ids_freq])[:-1]
        log_deeper("Mutable identifier freq ({0}): {1}"
                   .format(len(self.ids_weight), pr))

        freq_immutable_chars = {}
        for c in source:
            if c not in freq_immutable_chars:
                freq_immutable_chars[c] = 0
            freq_immutable_chars[c] += 1

        for id in known_ids:
            occurrences = len(all_ids[id]['offsets'])
            for c in id:
                freq_immutable_chars[c] -= occurrences

        freq_immutable_chars = sorted(freq_immutable_chars.items(),
                                      key=operator.itemgetter(1), reverse=True)

        self.char_freq = [c for c in freq_immutable_chars
                          if self.is_valid_identifier_start_char(c[0])]

        pr = str(dict(self.char_freq))[1:-1]
        log_deeper("Immutable char reference# ({0}): {1}"
                   .format(len(self.char_freq), pr))

        other_freq = [c for c in freq_immutable_chars
                      if c not in self.char_freq]
        pr = str(dict(other_freq))[1:-1]
        log_deeper("Remaining char reference# ({0}): {1}"
                   .format(len(other_freq), pr))

        # [TODO]
        # - use digits in identifiers
        # - always use concat-safe vars for at least vars of len 2+?
        def create_id_pool(num_wanted_ids):
            sorted_chars = "".join([c[0] for c in freq_immutable_chars])
            for c in string.ascii_lowercase + string.ascii_uppercase + '_':
                if c not in sorted_chars:
                    sorted_chars += c
            valid_chars = ''
            for c in sorted_chars:
                if not self.is_valid_identifier_start_char(c):
                    continue
                if c in all_ids and 'declared' not in all_ids[c]:
                    # assert False, c
                    continue
                valid_chars += c

            # [TICKLE]
            num_freq_chars_used = len(valid_chars) - 0
            assert num_freq_chars_used > 0

            valid_chars = valid_chars[:num_freq_chars_used]

            pool = list(valid_chars)

            var_length = 2
            while len(pool) < num_wanted_ids:

                for c in valid_chars:
                    pool.append(c * var_length)
                num_already_added = len(valid_chars)

                num_needed = num_wanted_ids - len(pool) + num_already_added
                if num_needed <= 0:
                    return pool[:num_wanted_ids]

                # [TICKLE] Use as little freq used chars as required.
                iters = int(math.sqrt(num_needed)) + 1
                perms = itertools.product(valid_chars[:iters],
                                          repeat=var_length)

                for var in sorted(perms):
                    name = ''.join(var)
                    if name in pool or name in all_ids:
                        continue

                    pool.append(name)
                    if len(pool) == num_wanted_ids:
                        return pool

                var_length += 1

            return pool

        # [TICKLE]
        DEF_CHAR_DEPTH = self.args.depth
        DEF_ID_DEPTH = DEF_CHAR_DEPTH

        DEF_CHAR_DEPTH = max(1, DEF_CHAR_DEPTH)
        DEF_ID_DEPTH = max(1, DEF_ID_DEPTH)

        self.chars_depth = min(DEF_CHAR_DEPTH, len(self.char_freq))
        self.id_depth = min(DEF_ID_DEPTH, len(self.known_ids))
        # [TODO]
        self.replace_index = 0

        id_rename_count = len(known_ids_freq) - 0

        ids = [k for k in self.ids_concat_info if not self.is_concat_safe(k)]
        num_separated_identifiers_required = len(ids)

        # [TICKLE]
        if num_separated_identifiers_required < 10:
            self.chars_depth += 1
            self.id_depth += 1

        self.id_depth = min(self.id_depth, len(self.known_ids))

        # [TICKLE]
        prefill = []
        for id in self.known_ids_freq:
            if len(id) > 1:
                break

            if id[0] not in string.hexdigits:
                continue

            prefill += id[0]
            break

        diff = self.chars_depth - len(self.known_ids) - 3
        if diff > 0:
            self.chars_depth += diff

        pool_length = max(self.chars_depth, len(known_ids))  # [TODO]
        full_id_pool = create_id_pool(pool_length)

        new_id_pool = []

        i = 0
        num_wanted_ids = pool_length

        while i < num_wanted_ids:
            if len(prefill):
                j = 0
                while (j < len(full_id_pool)
                       and (full_id_pool[j] != prefill[0])):
                    j += 1
                if j < len(full_id_pool):
                    new_id_pool.append(prefill[0])
                    i += 1
                    del full_id_pool[j]

                del prefill[0]
                continue

            v = full_id_pool[0]

            must_replace_var = i < id_rename_count
            if not must_replace_var and i < len(known_ids_freq):
                # If a lesser used identifier is already in the pool then
                # it really should be replaced.
                if known_ids_freq[i] in new_id_pool:
                    must_replace_var = True
                else:
                    v = known_ids_freq[i]

            del_index = 0
            if must_replace_var:

                j = 0

                while (j < len(full_id_pool)
                       and not self.is_concat_compatible_replacement(
                        known_ids_freq[i], full_id_pool[j])):
                    j += 1
                # assert j != len(full_id_pool)
                if j != len(full_id_pool):
                    v = full_id_pool[j]
                    del_index = j

            del full_id_pool[del_index]

            new_id_pool.append(v)
            i += 1

        s = source[:]

        for i, var in enumerate(known_ids_freq):
            s, self.new_id_offsets = self.replace(s, var, new_id_pool[i])

        self.new_id_pool = new_id_pool

        return s.encode('utf-8')

    def variate(self, source):
        self.ran_minify = True
        sep = ", "

        def list_s(list):
            return "".join(k + sep for k in list)[:-len(sep)]

        log_deeper("Original identifiers used in permutations:  "
                   + list_s(self.known_ids_freq[:self.chars_depth]))
        log_deeper("Replaced identifiers used for permutations: "
                   + list_s(self.new_id_pool[:self.chars_depth]))

        source = source.decode('utf-8')

        known_ids_freq = self.known_ids_freq

        new_id_view_base = {}
        for i, k in enumerate(known_ids_freq):
            new_id_view_base[self.new_id_pool[i]] = self.new_id_offsets[k][:]

        id_replacements = []

        for i, k in enumerate(known_ids_freq[self.replace_index:]):
            id_replacements.append((k, self.new_id_pool[i], ))

        s = "{} → {}"
        desc = (sep.join("{} → {}".format(k[0], k[1])
                for i, k in enumerate(id_replacements[self.chars_depth:])))
        log_deeper("Permanent identifier replacements: " + desc)

        # [TODO] This can be empty initially now?
        self.new_id_pool = self.new_id_pool[self.replace_index:]
        var_names_occupied = self.new_id_pool[
            self.id_depth:len(self.known_ids)]

        num_chars_perm = min(len(self.new_id_pool), self.chars_depth)
        chars_perm = ''.join([self.new_id_pool[i]
                             for i in range(num_chars_perm)])

        log_deeper("Chars used in permutation: " + chars_perm)
        log_deeper("Var names occupied: " + str(var_names_occupied))

        perms = itertools.permutations(chars_perm, self.id_depth)
        total_perms = 0
        for x in perms:
            total_perms += 1
        perms = itertools.permutations(chars_perm, self.id_depth)

        self.new_id_offsets = dict(new_id_view_base)
        for k in self.new_id_offsets:
            self.new_id_offsets[k] = new_id_view_base[k][:]

        skip_count = 0
        perm_count = 1
        log_deepest("Trying {} variations".format(total_perms))
        for perm_index, perm in enumerate(perms):
            s = source[:]
            desc = "#{}: ".format(perm_count)
            vars_used = var_names_occupied[:]

            self.new_id_offsets = dict(new_id_view_base)

            skip = False

            for i, new_id in enumerate(perm):
                old_id = self.new_id_pool[i]
                origin_id_name = id_replacements[i][0]

                assert new_id not in vars_used

                if not self.is_concat_compatible_replacement(
                     known_ids_freq[i], new_id):
                    # [TODO] Don't bother trying with space preprended?
                    # new_id = ' ' + new_id
                    skip = True
                    break

            if skip:
                # log_deepest("Skipping: " + desc)
                skip_count += 1
                continue

            for i, new_id in enumerate(perm):
                old_id = self.new_id_pool[i]
                origin_id_name = id_replacements[i][0]

                if origin_id_name != new_id:
                    desc += "{} → {}, ".format(
                        id_replacements[i][0], new_id)

                assert new_id not in vars_used

                vars_used.append(new_id)

                assert (new_id[0] not in string.hexdigits
                        or self.all_ids[known_ids_freq[i]]['concat_info']
                        not in 'dx')

                s, self.new_id_offsets = self.replace(
                    s, old_id, new_id, i)
                # new_s, new_offsets = self.replace(
                #     s, old_id, new_id, i)
                # if new_offsets:
                #     s = new_s
                #     self.new_id_offsets = new_offsets
                # else:
                #     skip = True
                # if skip:
                #     break

            # if skip:
            #     log_deepest("Skipping: " + desc)
            #     skip_count += 1
            #     continue
            assert not skip

            if not (perm_count % 16):
                progress = " {0:.2f}%".format(100 * perm_index / total_perms)
                if self.log_level >= 2:
                    extra = " ({}/{})".format(perm_index, total_perms)
                    if self.log_level >= 3 and skip_count:
                        extra += " skipped {}".format(skip_count)
                else:
                    extra = ""
                print(progress + extra, end='\r')

            # Don't bother trying with \r and \n, it will result in invalid
            # code with quoted strings which then do need spaces, making it
            # rather useless to bother.
            whitespace_chars = {
                ' ': 'space', '\t': 'tab',
                # '\r': 'CR', '\n': 'LF'
            }

            for spacing in whitespace_chars:
                final_desc = desc
                if spacing != ' ':
                    final_s = s.replace(' ', spacing)
                    final_desc += "space → {}, ".format(
                        whitespace_chars[spacing])
                else:
                    final_s = s

                if self.log_level < 2:
                    final_desc = ''
                self.compress(bytes(final_s.encode('utf-8')), final_desc[:-2])

            perm_count += 1
        print(end='')

        return self.best.data_out

    def is_concat_compatible_replacement(self, ori_id, new_id):
        concat_info = self.ids_concat_info[ori_id]
        first_new_id_char = new_id[0]

        if 'x' in concat_info:
            return not self.can_hex_concat(first_new_id_char)
        elif 'd' in concat_info:
            return not self.can_dec_concat(first_new_id_char)

        concat_info = concat_info.replace(
            'd', '').replace('x', '').strip()
        assert not concat_info, ori_id + ":" + concat_info

        return True


def pack(args):
    try:
        print("Packing {} => {}".format(os.path.basename(args.filename_in),
              os.path.basename(args.filename_out)))
        file = open(args.filename_in, mode='rb')
    except FileNotFoundError:
        log_error("No such file: '{}'".format(args.filename_in))
        return None

    data = file.read()
    file.close()

    global pedantic
    log_level = args.verbose
    pedantic = args.pedantic

    set_log_info(log_level, pedantic)

    tic = Tic(args, data)
    source = tic.source()

    def compress_info(pack_info, extra_bytes_info=None):
        original_source_length = len(source)
        mod_source_length = pack_info.original_length

        new_length = pack_info.best_length
        name = pack_info.name
        if '(' in name:
            # Desc already contains size info
            size_info = ''
        else:
            size_info = " ({})".format(byte_length(mod_source_length))

        s = "{}{}:".format(name, size_info)
        s += " " * max(1, 21 - len(s))
        s += byte_length(new_length)

        if new_length != original_source_length:
            ratio = 100.0 * (original_source_length
                             - new_length) / original_source_length
            s += " ({:2.2f}%)".format(ratio)

        s += " " + pack_info.best_info
        print(s)

    class PackStage:
        def __init__(self, name, original_data, new_data):
            self.name = name
            self.original_length = len(original_data)
            if new_data:
                self.best_length = len(new_data)
            else:
                self.best_length = self.original_length
            if log_level < 2 or 'inal' in name:
                self.best_info = ""
            else:
                self.best_info = str(packer.best) + " " + packer.best.info

    stages = []

    def add_stage(name, source, packer, data=None):
        if not data:
            data = packer.compress(source)
        stage = PackStage(name, source, data)
        stages.append(stage)
        compress_info(stage)

    packer = Packer(args, tic)

    if not len(packer.packers):
        log_error("hehe")
        exit(1)

    if not args.shown_break_msg and not args.single_pass:
        print()
        print("Intermediate best results are saved. Use Ctrl-C to stop.")
        print()
        args.shown_break_msg = True

    add_stage("Original", source, packer)

    stripped = packer.strip(source)
    add_stage("Stripped", stripped, packer)

    minified = packer.minify(stripped[:])

    add_stage("Minified", minified, packer)

    if not args.single_pass:
        variated = packer.variate(minified)
        add_stage("Variated", minified, packer, variated)

    for p in packer.packers:
        log_deep("Best {}: {} {}"
                 .format(p, byte_length(p.length()), p.info))

    size_header = 4
    size_desc = "+{}".format(size_header)

    if args.default_chunk:
        size_default_chunk = 4 - (not args.pedantic) * 3
        size_desc += "+{}".format(size_default_chunk)
    else:
        size_default_chunk = 0

    info = "Finalized ({})".format(size_desc)

    full_data = packer.best.data_out if packer.best.data_out else stripped
    # [TODO]
    full_data += bytes(4 + size_default_chunk)

    add_stage(info, source, packer, full_data)

    # if args.single_pass:
    #     packer.write_tic()
    packer.write_tic(verbose=1)

    return packer.best
