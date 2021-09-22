# -*- coding: utf-8 -*-

from __future__ import print_function

import errno
import math
import multiprocessing
import operator
import os
import random
import re
import string
import sys
import zlib

from collections import OrderedDict
from copy import copy
from threading import Thread

from src.common import log, log_deep, log_deeper, log_deepest, \
    log_error, set_log_info, byte_length
from src.config import CONFIG
from src.ticfile import Tic
from src.zopfli import zopfli_compress

import src.lualexer


def CONFIG_get(s, default=None):
    if s in CONFIG.__dict__:
        return CONFIG.__dict__[s]
    return default


zlib_try_optimal_settings_only = CONFIG_get('zlib_try_optimal_settings_only')
optimal_zlib_settings = {}

for setting in [
    [zlib.Z_BEST_SPEED, zlib.Z_DEFAULT_STRATEGY],
    [2, zlib.Z_DEFAULT_STRATEGY],
    [4, zlib.Z_DEFAULT_STRATEGY],
    [5, zlib.Z_DEFAULT_STRATEGY],
    [6, zlib.Z_DEFAULT_STRATEGY],
    [7, zlib.Z_DEFAULT_STRATEGY],
    [8, zlib.Z_DEFAULT_STRATEGY],
    [4, zlib.Z_FILTERED],
]:

    optimal_zlib_settings[setting[1] * 10 + setting[0]] = setting
if zlib_try_optimal_settings_only:
    used_zlib_settings = optimal_zlib_settings
else:
    used_zlib_settings = {}
    for i in range(10 * 5):
        level = i % 10
        if level:
            used_zlib_settings[i] = [level, int(i/10)]

zopfli_iter_count = -1
iter_count = 0

best_hits = {}

Cat_Match = 'matches'
Cat_Zlib = 'zlib'
Cat_Zopfli = 'Zopfli'
cats = [Cat_Match, Cat_Zlib, Cat_Zopfli]

Info_Cache_Hits = 'Cache hits'
Info_Cache_Accesses = 'Cache accesses'
search_info = {}


def increase_hits(type):
    if type not in search_info:
        search_info[type] = 0

    search_info[type] += 1


for setting in used_zlib_settings:
    best_hits[setting] = 0


def compress_zlib(uncomp):
    new_compressed_data = None
    new_compressed_size = 0

    comp_size = 256 + len(uncomp)

    results = {}

    for k in used_zlib_settings:
        level, strat = used_zlib_settings[k]
        compress_obj = zlib.compressobj(level, zlib.DEFLATED, zlib.MAX_WBITS, 9, strat)
        new_compressed_data = compress_obj.compress(uncomp)
        new_compressed_data += compress_obj.flush()
        new_compressed_size = len(new_compressed_data)

        results[k] = new_compressed_size

        if (not zlib_try_optimal_settings_only and k not in optimal_zlib_settings):
            continue

        if new_compressed_size < comp_size:
            comp = new_compressed_data
            comp_size = new_compressed_size
            comp_info = 'level {}, strat {}'.format(level, strat)

    if not zlib_try_optimal_settings_only:
        for k in results:
            size = results[k]
            # A zlib setting that gives better results, add to optimal settings
            assert size >= comp_size, "{}: {} {}".format(k, size, comp_size)

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
        return zopfli_compress(data, zopfli_iter_count), ""


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


class XForm(object):
    def __init__(self, packer, max_replacements=40):
        self.packer = packer
        # k = slot index, v = replacement id
        self.slots_to_id = {}
        self.whitespace_sep = '?'
        self.iter = None
        self.length = -1
        self.max_replacements = max_replacements

    def copy(self):
        new = copy(self)
        new.slots_to_id = dict(new.slots_to_id)
        return new

    def id_pool(self):
        return self.packer.chars_sorted

    def used_slots(self):
        return list(self.slots_to_id.keys())

    def used_ids(self):
        return list(self.slots_to_id.values())

    def used_id(self, slot):
        assert self.is_slot_in_use(slot), slot
        return self.slots_to_id[slot]

    def is_slot_in_use(self, slot):
        return slot in self.slots_to_id

    def is_id_in_use(self, id):
        return id in self.slots_to_id.values()

    def set_slot(self, slot, replacement_id):
        self.slots_to_id[slot] = str(replacement_id)

    def set_slot_to_default(self, slot):
        self.set_slot(slot, self.default_replacement(slot))

    def free_slot(self, slot):
        if slot in self.slots_to_id:
            del self.slots_to_id[slot]

    def default_replacement(self, slot):
        # Use new id pool here which has concat compatible layout,
        # unlike chars sorted by freq (chars_sorted).
        return self.packer.new_id_pool[slot]

    def is_default_replacement(self, slot):
        return self.used_id(slot) == self.default_replacement(slot)

    def valid_for_slot(self, slot, id, len1):
        if len1 and len(id) > 1:
            return False

        if not self.packer.is_concat_compatible_replacement_slot(slot, id):
            return False

        return True

    def friendly_hash(self):
        if self.slots_to_id:
            max_index = max(self.used_slots()) + 1
        else:
            max_index = 1

        hash = [' '] * max_index

        for slot in self.slots_to_id:
            repl = self.slots_to_id[slot]
            if self.is_default_replacement(slot):
                repl = '.'
            hash[slot] = repl

        return "".join(hash)

    def desc(self, with_length=True):
        s = self.friendly_hash() + ' '
        ws = {'\t': 'tab', ' ': 'spc', '?': '', 'm': '   '}

        if self.length == -1:
            assert self.whitespace_sep == '?'
            return s

        assert self.whitespace_sep != '?'

        assert self.whitespace_sep in ws
        s += ws[self.whitespace_sep]

        if with_length:
            s += ' {}b'.format(self.length)

        return s

    def compr_hash(self):
        used_slots = self.used_slots()
        max_index = self.max_replacements
        if used_slots:
            max_index = max(max_index, max(used_slots))

        xform_hash = [' '] * (max_index + 1)

        for i in self.slots_to_id:
            xform_hash[i] = self.slots_to_id[i]
            assert xform_hash[i] == self.used_id(i)
        s = "".join(xform_hash)
        return str(s)


class Packer(src.lualexer.LuaLexer):
    def __init__(self, args, tic):
        self.args = args
        self.tic = tic
        self.log_level = args.verbose
        self.ran_minify = False
        super(Packer, self).__init__(self.log_level)
        self.hint_categories = {}
        self.best_hints = {}
        self.compr_hashes = {}
        self.new_id_pool = []

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

        best_length = self.atb()
        improved = []
        for pack in self.packers:
            pack.compress(data, perm_info)

            length = pack.length()
            if best_length is None or length < best_length:
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

    def atb(self):
        return self.best.length() if self.best else None

    def best_pack(self, data=None, perm_info=''):
        if data:
            self.data = data
        else:
            assert False

        atb = self.atb()
        best_length = None
        best_data = None
        for pack in self.packers:
            if self.fast_compression_only and str(pack) != 'zlib':
                continue

            data_out = pack.compress(data, perm_info)

            length = len(data_out)

            if best_length is None or length < best_length:
                best_length = length
                best_data = data_out[:]
                # if pack.data_out:
                #     if not self.best:
                #         self.best = pack
                #     self.best.data_out = pack.data_out[:]
                #     improved.append(pack)

                # self.best_source = data[:]

            if atb is None or length < atb:
                self.best = pack

                if pack.data_out:
                    self.best.data_out = pack.data_out[:]

                self.best_source = data[:]

        return best_data

    def log_new_best(self, pack):
        if not self.ran_minify:
            return

        s = "New best {}".format(byte_length(pack.length()))

        if self.log_level:
            s += ": {}".format(pack)

        if self.log_level > 1:
            s += " " + pack.info

        print(s)

    def replace_slot(self, s, slot, new_id):
        t = ''
        prev_offset = 0

        old_id = self.known_ids[slot]
        for offset in self.id_offsets[slot]:
            t += s[prev_offset:offset]

            final_replacement = new_id[:]
            assert ' ' not in final_replacement, final_replacement

            t += final_replacement
            prev_offset = offset + len(old_id)

            assert len(old_id) == len(final_replacement), "Only change identifier length during minifying?"

        t += s[prev_offset:]

        return t

    def strip(self, source):
        source = strip_lua_comments(source)
        self.analyze(source)
        self.original_id_names = self.known_ids[:]

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

    def try_xform(self, xform):
        global iter_count
        iter_count += 1
        mutable_id_count = self.mutable_id_count()

        replacements = []
        for i in range(mutable_id_count):
            if len(self.known_ids[i]) > 1:
                break
            repl_id = (xform.used_id(i) if xform.is_slot_in_use(i) else self.new_id_pool[i])
            if repl_id == '_':
                continue
            replacements.append(repl_id)

        assert len(set(replacements)) == len(replacements), "Dupes in replacement '{}'".format(replacements)

        desc = "#{}: ".format(iter_count)

        best_length = self.atb()

        xform_hash = xform.compr_hash()

        increase_hits(Info_Cache_Accesses)
        if xform_hash in self.compr_hashes:
            increase_hits(Info_Cache_Hits)

            cached_xform = self.compr_hashes[xform_hash]
            assert xform.friendly_hash() == cached_xform.friendly_hash()

            xform.whitespace_sep = cached_xform.whitespace_sep
            xform.iter = cached_xform.iter
            xform.length = cached_xform.length
            xform.from_cache = True

            return

        s = self.analyzed_source.decode('utf-8')
        s = self.replace_multiple_xform(s, xform)

        for slot in xform.used_slots():
            new_id = xform.used_id(slot)
            if not xform.is_default_replacement(slot):
                old_id = self.original_id_names[slot]
                desc += "{} → {}, ".format(old_id, new_id)

        xform.iter = iter_count

        # Don't bother trying with \r and \n, it will result in invalid
        # code with quoted strings which then do need spaces, making it
        # rather useless to bother.
        whitespace_chars = {
            ' ': 'space', '\t': 'tab',
            # '\r': 'CR', '\n': 'LF'
        }

        best_whitespace = None
        comp_len = -1

        ws_results = {}
        for i, spacing in enumerate(whitespace_chars):
            final_desc = desc
            if spacing != ' ':
                # assert i == 1
                final_s = s.replace(' ', spacing)
                final_desc += "space → {}, ".format(
                    whitespace_chars[spacing])
            else:
                final_s = s

            if self.log_level < 2:
                final_desc = ''
            comp = self.best_pack(bytes(final_s.encode('utf-8')), final_desc[:-2])
            comp_len = len(comp)

            if not best_whitespace or comp_len < len(best_whitespace):
                best_whitespace = comp[:]

            if best_length is None or comp_len < best_length:
                best_length = comp_len

            ws_results[spacing] = comp_len

        best_spacing = ''

        this_length = len(best_whitespace)
        num_same_best = 0
        for ws in ws_results:
            if ws_results[ws] == this_length:
                best_spacing = ws
                num_same_best += 1
        assert num_same_best
        if num_same_best > 1:
            best_spacing = 'm'

        xform.whitespace_sep = best_spacing
        xform.length = this_length
        xform.from_cache = False

        assert xform_hash == xform.compr_hash()

        self.compr_hashes[xform_hash] = xform.copy()

    def next_slot_replacement(self, xform, slot, freq_index):
        old_id = self.new_id_pool[slot]
        if len(old_id) > 1:
            return None

        for id in xform.id_pool()[freq_index:]:
            id = id[0]

            if xform.is_id_in_use(id):
                continue

            if not xform.valid_for_slot(slot, id, len1=True):
                continue

            return id

        log_deepest("Almost xhausted searching repl for '{}' slot {}, freq_index {}"
                    .format(old_id, slot, freq_index))

        for id in xform.id_pool()[:freq_index]:
            id = id[0]

            if xform.is_id_in_use(id):
                continue

            if not xform.valid_for_slot(slot, id, len1=True):
                continue

            return id

        # assert False, old_id + " " + str(freq_index)
        log_deepest("Exhausted searching repl for '{}' slot {}, freq_index {}"
                    .format(old_id, slot, freq_index))
        return None

    def closest_slot_replacement(self, xform, slot):
        if xform.is_slot_in_use(slot):
            assert False
            return xform.used_id(slot)

        default_slot_id = xform.default_replacement(slot)
        if not xform.is_id_in_use(default_slot_id):
            # Default is available and the closest
            return default_slot_id

        repl_id = default_slot_id
        if len(repl_id) > 1:
            return repl_id
            return None

        freq_index = 0 # slot
        for id in self.new_id_pool[freq_index:]:
            if xform.is_id_in_use(id):
                continue

            if not xform.valid_for_slot(slot, id, len1=True):
                continue

            return id

        print("Exhausted searching repl for '{}' findindex:{}"
              .format(self.new_id_pool[slot], freq_index))
        assert False
        return None

    def fetch_slot_candidates_by_match(self, slot, candi2):
        def match_count(s, match_length=3):
            matches = {}

            # Usually close enough
            for i, c in enumerate(s):
                if i < match_length:
                    continue
                match = s[i-match_length:i]
                if match not in matches:
                    matches[match] = 0
                matches[match] += 1

            return len(matches)

        def results_with_match_length(xmatch_length, candi):
            ids = self.chars_sorted

            xf = XForm(self)
            for id in ids:
                if (id in self.freq_immutable_chars
                   and self.freq_immutable_chars[id] < 1):
                    continue

                if not xf.valid_for_slot(slot, id, len1=True):
                    continue

                xf.set_slot(slot, id)

                source = self.replace_multiple_xform(self.masked_source[:], xf)[:]

                num_matches = match_count(source, match_length)
                candi[id] = num_matches

        res = {}
        for match_length in range(3, 2, -1):
            results_with_match_length(match_length, res)
            max_matches = max(res.values())

            for id in res:
                res[id] = max_matches - res[id]

        for k in res:
            weight = res[k]
            if weight > 0:
                candi2[k] = weight

    def log_hints(self, cat):
        log_deepest("Slot hints for {} category:".format(cat))
        for slot in self.hint_categories[cat]:
            log_deepest("   {}: {}".format(slot,
                        str(self.hint_categories[cat][slot])[1:-1]))

    def probe_matches(self):
        begin = 0

        # [TICKLE]
        end = min(self.mutable_id_count(), 10)
        end = min(end, self.char_identifier_count)

        self.fast_compression_only = True
        cat = Cat_Match
        self.hint_categories[cat] = {}

        for slot in range(begin, end):
            if len(self.new_id_pool[slot]) > 1:
                break
            res2 = {}
            self.fetch_slot_candidates_by_match(slot, res2)
            if not res2:
                continue

            self.update_slot_candidates(cat, slot, res2)

        self.fast_compression_only = False

        self.best_hints[cat] = self.get_best_slot_hints(cat)
        self.log_hints(cat)

    def get_best_slot_hints(self, cat):
        slots = {}

        final_perm_slots = {}
        for slot in self.hint_categories[cat]:
            slots[slot] = dict(self.hint_categories[cat][slot])

        while slots:
            max_per_slot = {}
            for slot in slots:
                if not slots[slot]:
                    continue

                candi = slots[slot]
                max_per_slot[slot] = max(candi.values())

            if not max_per_slot:
                break

            max_weight = max(max_per_slot.values())

            def slots_with_max(max_val):
                slot = {}
                for i, slot_idx in enumerate(max_per_slot):
                    if max_per_slot[slot_idx] == max_val:
                        slot[slot_idx] = slots[slot_idx]
                return slot

            slots_with_max = slots_with_max(max_weight)
            chars_with_max = []

            slot_candi = []
            for slot in slots_with_max:
                for id in slots_with_max[slot]:
                    if slots[slot][id] == max_weight:
                        chars_with_max.append(id)
                        slot_candi.append([slot, id])

            # [TICKLE][TODO]
            si = 0

            slot = slot_candi[si][0]
            id = slot_candi[si][1]

            assert len(id) < 2, id

            assert slot not in final_perm_slots
            final_perm_slots[slot] = (id, max_weight,)
            del slots[slot]
            for slot in slots:
                if id in slots[slot]:
                    del slots[slot][id]

        # print(final_perm_slots)
        return final_perm_slots

    def try_one_freq_closest_repl(self, orig_xform, main_slot, desired_repl_id):
        others_fixed = None

        mutable_id_count = self.mutable_id_count()
        assert main_slot < mutable_id_count

        if not self.is_concat_compatible_replacement_slot(main_slot, desired_repl_id):
            return

        for slot in range(main_slot):
            if others_fixed:
                continue
            if orig_xform.is_slot_in_use(slot):
                if orig_xform.used_id(slot) == desired_repl_id:
                    return
                continue

            if self.new_id_pool[slot] == desired_repl_id:
                default_id = self.new_id_pool[main_slot]
                orig_xform.set_slot(slot, default_id)
                if not self.is_concat_compatible_replacement_slot(
                     slot, default_id):
                    return

            if not orig_xform.is_slot_in_use(slot):
                orig_xform.set_slot_to_default(slot)

        if orig_xform.is_id_in_use(desired_repl_id):
            return

        assert not orig_xform.is_id_in_use(desired_repl_id)

        if orig_xform.is_slot_in_use(main_slot):
            return

        if desired_repl_id in self.chars_indexed:
            freq_index = self.chars_indexed[desired_repl_id]
            actual_repl_id = self.next_slot_replacement(orig_xform, main_slot, freq_index)
        else:
            actual_repl_id = desired_repl_id
            if orig_xform.is_id_in_use(actual_repl_id):
                return

        if not actual_repl_id:
            assert False, main_slot
            return

        orig_xform.set_slot(main_slot, actual_repl_id)

        for slot in range(main_slot+1, mutable_id_count):
            if others_fixed:
                continue

            if orig_xform.is_slot_in_use(slot):
                assert False
                continue

            id = self.closest_slot_replacement(orig_xform, slot)

            if id:
                assert(self.is_valid_identifier_start_char(id))
                orig_xform.set_slot(slot, id)
            else:
                assert len(id) > 1, id
                pass

        if others_fixed:
            for slot in range(mutable_id_count):
                if slot == main_slot:
                    continue

                if orig_xform.is_slot_in_use(slot):
                    continue

                orig_xform.set_slot(i, others_fixed)

        self.try_xform(orig_xform)

        assert orig_xform.length != -1

    def try_one_freq(self, orig_xform, main_slot, desired_repl_id):
        mutable_id_count = self.mutable_id_count()
        assert main_slot < mutable_id_count

        if not orig_xform.valid_for_slot(main_slot, desired_repl_id, len1=True):
            return

        if orig_xform.is_slot_in_use(main_slot):
            return

        for slot in range(main_slot):
            if not orig_xform.is_slot_in_use(slot):
                orig_xform.set_slot_to_default(slot)

        assert not orig_xform.is_slot_in_use(main_slot)

        if desired_repl_id in self.chars_indexed:
            freq_index = self.chars_indexed[desired_repl_id]
            actual_repl_id = self.next_slot_replacement(orig_xform, main_slot, freq_index)
        else:
            actual_repl_id = desired_repl_id
            if orig_xform.is_id_in_use(actual_repl_id):
                return

        if not actual_repl_id:
            assert False, main_slot
            return

        orig_xform.set_slot(main_slot, actual_repl_id)

        for slot in range(main_slot+1, mutable_id_count):
            if orig_xform.is_slot_in_use(slot):
                continue

            id = self.next_slot_replacement(orig_xform, slot, 0)
            if id:
                orig_xform.set_slot(slot, id)
            else:
                # [TODO] many ids
                # assert False, main_slot
                break

        self.try_xform(orig_xform)

    def get_slot_candidates(self, xform, main_slot):

        max_chars = min(self.char_depth, len(xform.id_pool()))

        ids = []

        def add_id(id):
            if id in ids:
                return

            if xform.is_id_in_use(id):
                return

            if not xform.valid_for_slot(main_slot, id, True):
                return

            ids.append(str(id))
            return

        def add_all_from_cat(cat):
            if cat not in self.hint_categories:
                return

            group = self.hint_categories[cat]
            for id in group[main_slot]:
                add_id(id)

        def add_best_from_cat(cat, all=False):
            if cat not in self.hint_categories:
                return

            hints = self.best_hints[cat]
            if main_slot not in hints:
                return

            id, weight = hints[main_slot]

            # [TICKLE]
            if weight < 2:
                return

            add_id(id)

        def add_multiple_from_cat(cat, max_ids=3):
            if cat not in self.hint_categories:
                return

            hints = self.hint_categories[cat]
            for id in hints[main_slot][:max_ids]:
                add_id(id)

            # add_multiple_from_cat(cat)

        # for cat in cats:
        #     add_best_from_cat(cat)

        # [TICKLE]
        slot = max(0, main_slot + 0)

        # [TICKLE]
        if main_slot == len(self.known_ids) - 1:
            # Actually reached the end (few ids?), try more chars
            max_chars = int(max_chars * 1.5)
        while len(ids) < max_chars:
            if slot >= len(self.new_id_pool):
                break
            id = self.new_id_pool[slot]
            if len(id) > 1:
                break
            add_id(id)
            slot += 1

        for cat in cats:
            add_best_from_cat(cat)

        # [TICKLE] Introducing a new char can be useful (or q is special, sometimes)
        add_id('q')

        return ids

    def all_results(self, orig_xform, slot, repl_ids, try_func, res):
        xform = orig_xform.copy()

        best = worst = None

        results = []

        atb = prev_atb = self.atb()

        use_threads = CONFIG_get('use_threads', True)

        if use_threads:
            threads = []

            xform_results = {}
            repl_index = 0
            id_count = len(repl_ids)
            thread_count = multiprocessing.cpu_count()
            while repl_index < id_count:

                for i in range(thread_count):
                    xf = xform.copy()
                    repl_id = repl_ids[repl_index]
                    xform_results[repl_id] = xf

                    thread = Thread(
                        target=try_func,
                        args=(xform_results[repl_id], slot,
                              repl_ids[repl_index], ))

                    threads.append(thread)
                    thread.start()
                    repl_index += 1
                    if repl_index == id_count:
                        break

                for i, thread in enumerate(threads):
                    thread.join()

        for repl_id in repl_ids:
            if use_threads:
                applied_xform = xform_results[repl_id]
            else:
                applied_xform = orig_xform.copy()
                try_func(applied_xform, slot, repl_id)

            if applied_xform.length == -1:
                continue

            length = applied_xform.length

            if length < atb:
                atb = length
                msg = " new best!"
                self.write_tic()
            elif length == atb:
                # self.bes
                msg = " same as best"
            else:
                msg = ""

            cache_status = " (cached)" if applied_xform.from_cache else ""
            log_deepest("→ {} {}{}b{}{}".format(
                applied_xform.desc(with_length=False),
                " " * (11 + length - prev_atb),
                length,
                msg,
                cache_status
                ))

            if best is None or length < best:
                best = length
            if worst is None or length > worst:
                worst = length

            results.append([repl_id, applied_xform])

        if not results:
            # assert False
            return None, None

        for result in results:
            expected_id, applied_xform = result

            actual_char = applied_xform.used_id(slot)
            assert expected_id == actual_char

            if actual_char in res:
                if applied_xform.length > res[actual_char].length:
                    continue

            res[actual_char] = applied_xform

        return worst, best

    def try_with_hints(self, cat):
        static_xform = XForm(self)

        log_deepest("Trying with probe from " + cat)

        slot_hints = self.best_hints[cat]
        for slot in slot_hints:
            id, weight = slot_hints[slot]

            static_xform.set_slot(slot, id)

        for slot in range(self.mutable_id_count()):
            if len(self.new_id_pool[slot]) > 1:
                break

            if static_xform.is_slot_in_use(slot):
                continue

            id = self.closest_slot_replacement(static_xform, slot)
            assert id

            static_xform.set_slot(slot, id)

        self.try_xform(static_xform)
        log_deepest("With {} hints: {}".format(cat, static_xform.length))

    def probe_slots(self, cat):
        if cat in self.best_hints:
            return

        if self.log_level > 2:
            print("Probing with {}".format(cat))
            log_error("Probing...")
        elif self.log_level > 1:
            log("Probing...")

        fast = (cat == 'zlib')

        self.fast_compression_only = fast

        begin = 0
        end = self.mutable_id_count()
        if not fast:
            end = min(end, 20)
        end = min(end, self.char_identifier_count)

        for slot in range(begin, end):
            xform = XForm(self)
            if xform.is_slot_in_use(slot):
                continue

            candi = {}

            worst, best = self.all_results(
                xform,
                slot,
                list(self.chars_sorted),
                try_func=self.try_one_freq_closest_repl,
                res=candi)

            final_slot_candi = {}
            for k in candi:
                weight = worst - candi[k].length
                if weight <= 1:
                    continue
                final_slot_candi[k] = weight

            self.update_slot_candidates(cat, slot, final_slot_candi)

        self.fast_compression_only = False
        # self.best = None
        self.log_hints(cat)
        self.best_hints[cat] = self.get_best_slot_hints(cat)

    def apply_best_hints_to_xform(self, cat, xform, main_slot=0):
        ids_seen = []

        hints = self.best_hints[cat]
        for slot in hints:
            ids = hints[slot]

            if not ids:
                continue

            for id in ids:
                if slot < main_slot:
                    continue

                if id in ids_seen:
                    continue

                if xform.is_id_in_use(id):
                    continue

                log_deepest("Perm slot {}: '{}'".format(slot, id))
                xform.set_slot(slot, id)
                assert id not in ids_seen
                if id not in ids_seen:
                    ids_seen.append(id)
                break

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
                    log("Same (un)transformed lengths ({}), using original"
                        .format(byte_length(best[0])))
                elif best[1] == source_xformed:
                    source = best[1]
                    log("Using {} transformation(s) ({} < {})".format(
                        num_xformed,
                        byte_length(best[0]),
                        byte_length(next[0]),
                    ))

        self.analyze(source)

        source = str(source.decode('utf-8'))
        all_ids = self.all_ids
        known_ids = self.known_ids
        known_ids_freq = self.known_ids_freq

        pr = ' '.join(["{}: {}x,".format(
            v, len(all_ids[v]['offsets'])) for v in known_ids_freq])[:-1]
        log_deeper("Mutable identifier freq ({0}): {1}"
                   .format(len(self.ids_weight), pr))

        freq_immutable_chars = OrderedDict()
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
        self.freq_immutable_chars = {c[0]:c[1] for c in freq_immutable_chars}

        self.char_freq = [c for c in freq_immutable_chars
                          if self.is_valid_identifier_start_char(c[0])]

        self.chars_sorted = [c[0] for c in self.char_freq]
        self.chars_indexed = {char: index for index, char in enumerate(self.chars_sorted)}

        pr = ''
        for c in self.char_freq:
            pr += "'{}': {}, ".format(c[0], c[1])
        log_deeper("Immutable char reference# ({0}): {1}"
                   .format(len(self.char_freq), pr[:-2]))

        other_freq = [c for c in freq_immutable_chars
                      if c not in self.char_freq]
        pr = str(dict(other_freq))[1:-1]
        log_deeper("Remaining char reference# ({0}): {1}"
                   .format(len(other_freq), pr))

        # [TODO]
        # - use digits in identifiers
        # - always use concat-safe vars for at least vars of len 2+?
        def create_id_pool(id_count):
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
                    log_deepest("Undeclared identifier '{}'?".format(c))
                    # continue
                    continue
                valid_chars += c

            # [TICKLE]
            num_freq_chars_used = len(valid_chars) - 0
            assert num_freq_chars_used > 0

            valid_chars = valid_chars[:num_freq_chars_used]

            pool = list(valid_chars)

            var_length = 2
            while len(pool) < id_count:
                num_already_added = len(valid_chars)

                num_needed = id_count - len(pool) + num_already_added
                if num_needed <= 0:
                    return pool[:id_count]

                # [TICKLE] Use as little freq used chars as required.
                num_freq_chars_used = int(math.sqrt(num_needed)) + 1

                iters = int(pow(num_freq_chars_used, var_length))

                def new_id_by_index(var_length, index):
                    s = ''
                    mod = 1
                    for i in range(var_length):
                        idx = int((index / mod)) % num_freq_chars_used
                        mod *= num_freq_chars_used
                        s = valid_chars[idx] + s

                    return s

                for i in range(iters):
                    name = new_id_by_index(var_length, i)

                    if name in pool or name in all_ids:
                        continue

                    pool.append(name)

                var_length += 1

            return pool

        # [TICKLE]
        char_depth = CONFIG_get('max_char_depth', 10)
        char_depth = max(1, char_depth)
        self.char_depth = min(char_depth, len(self.char_freq))

        id_depth = self.args.depth
        if id_depth == -1:
            id_depth = CONFIG_get('max_id_depth', 3)
        id_depth = max(1, id_depth)
        self.id_depth = min(id_depth, len(self.known_ids))

        # [TODO]
        self.replace_index = 0

        id_rename_count = len(known_ids_freq) - 0

        full_id_pool = create_id_pool(len(known_ids))

        def find_concat_compatible_replacement_for_slot(slot):
            j = 0
            while (j < len(full_id_pool)
                    and not self.is_concat_compatible_replacement_slot(
                    slot, full_id_pool[j])):
                j += 1

            if j == len(full_id_pool):
                assert False, slot
                return -1

            return j

        new_id_pool = []

        i = 0
        num_wanted_ids = len(full_id_pool)

        while i < num_wanted_ids:
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
                j = find_concat_compatible_replacement_for_slot(i)
                v = full_id_pool[j]
                del_index = j

            del full_id_pool[del_index]

            new_id_pool.append(v)
            i += 1

        s = source[:]

        s_list = []
        used_indices = []
        for c in s:
            s_list.append(str(c))
            used_indices.append(True)

        for slot, var in enumerate(known_ids):
            new_id = new_id_pool[slot]

            assert self.is_concat_compatible_replacement_slot(
                slot, new_id), "concat incompatible replacement '{}' for '{}'".format(new_id, var)

            for offset in self.id_offsets[slot]:
                s_list[offset] = [new_id[:]][:]
                # for i in range(len(new_id), len(var)):
                for i in range(1, len(var)):
                    assert used_indices[offset + i] is True
                    used_indices[offset + i] = False

        t = ""
        for i, used in enumerate(used_indices):
            if used:
                t += "".join(s_list[i])

        self.new_id_pool = new_id_pool
        self.new_id_to_index = {}
        for i, id in enumerate(self.new_id_pool):
            self.new_id_to_index[id] = i

        self.analyze(t.encode('utf-8'))

        # [TODO] Quickly try with (somewhat) left-over probing for single pass.
        single_pass = self.args.single_pass

        if single_pass or CONFIG_get('use_match_probe') or CONFIG_get('replace_ids_outside_of_search_range_with_match_hints'):
            self.probe_matches()

        if single_pass or CONFIG_get('use_zlib_probe'):
            self.probe_slots(Cat_Zlib)

        # if single_pass or CONFIG_get('use_zopfli_probe'):
        if CONFIG_get('use_zopfli_probe'):
            self.probe_slots(Cat_Zopfli)

        if single_pass:
            self.try_with_hints(Cat_Match)
            self.try_with_hints(Cat_Zlib)
            # self.try_with_hints(Cat_Zopfli)

        return t.encode('utf-8')

    def replace_multiple_xform(self, s, xform):
        for slot in xform.used_slots():

            if slot >= len(self.new_id_pool):
                assert False, slot
                # continue

            s = self.replace_slot(s, slot, xform.used_id(slot))

        return s

    # replacements = dict. key = id, value = weight
    def update_slot_candidates(self, cat, slot, replacements):
        if cat not in self.hint_categories:
            self.hint_categories[cat] = {}

        assert slot not in self.hint_categories[cat]
        self.hint_categories[cat][slot] = replacements

    def variate(self, source):
        self.analyze(source)
        self.ran_minify = True
        sep = ", "

        def list_s(list):
            return "".join(k + sep for k in list)[:-len(sep)]

        categories = {}
        for i, cat in enumerate(self.slots_concat_info):
            for c in cat:
                if c == ' ':
                    continue

                if c not in categories:
                    categories[c] = []

                k = self.known_ids_freq[i]
                categories[c] += [k]

        if categories:
            log_deeper("Concatenation info:")

        for cat in categories:
            s = ''
            if cat == 'd':
                s = 'decimal'
            elif cat == 'x':
                s = 'hexadecimal'
            else:
                assert False, c

            log_deeper("    Sticky with {}: {}".format(s, ", ".join(categories[cat])))

        id_count = len(self.original_id_names)
        log_deeper("Original identifiers found: "
                   + list_s(self.original_id_names))
        log_deeper("Default replacement identifiers used: "
                   + list_s(self.new_id_pool[:id_count]))

        source = source.decode('utf-8')

        new_id_view_base = []
        self.new_var_names = []

        for i, ofs in enumerate(self.id_offsets):
            new_id_view_base.append(ofs[:])
            self.new_var_names.append(self.new_id_pool[i])

        # [TODO] This can be empty initially now?
        self.new_id_pool = self.new_id_pool[self.replace_index:]

        global iter_count
        iter_count = 1

        self.fast_compression_only = False

        xform = XForm(self)

        def best_results(orig_xform, slot, repl_ids, try_func):
            id_results = {}

            ids = []
            for id in repl_ids:
                if orig_xform.is_id_in_use(id):
                    continue
                ids.append(id)

            # note("Checking maze '{}' with path(s) ".format(prefix) + list_s(ids))

            atb = self.atb()
            worst, best = self.all_results(orig_xform, slot, ids, try_func, id_results)

            if not id_results:
                return None

            def improved(length):
                return atb is None or length < atb

            s_xform = orig_xform.compr_hash().rstrip()
            prefix = s_xform if len(s_xform) else ''
            action = "Checked maze '{}': ".format(prefix)

            # if worst == best and not improved(best):
            if worst == best:
                increase_hits("Skipped empty")
                note(action + "Discarding all same worse results ({})".format(list_s(ids)))
                return None

            best_results = {}

            path_info = ''
            for id in id_results:
                applied_xform = id_results[id]

                length = applied_xform.length

                # [TICKLE]
                # if length == worst and worst > best:
                if length == worst and length > atb:
                    continue

                desc = str(id)
                discardable = True
                if improved(length):
                    discardable = False
                    desc += "*"
                elif length == atb:
                    discardable = False
                    desc += "~"

                if discardable:
                    if length > (best - worst) / 2:
                        continue
                    if c in self.freq_immutable_chars and self.freq_immutable_chars[c] < 3:
                        continue

                    if abs(length - best) > 2:
                        continue

                best_results[id] = applied_xform
                path_info += desc + " "

                # # [TODO]
                # if len(best_results) > <x>:
                #     log_deepest("Mostly ({}) of same/best results".format(len(best_results)))
                #     break

            if path_info:
                note(action + "Took notes of path(s) " + path_info)
            else:
                note(action + "Discarded ({})".format(list_s(ids)))
            return best_results

        def note(s):
            level = self.log_level

            if level < 3:
                r = ""
                if '*' in s:
                    if level < 1:
                        r = "\\*/"
                    else:
                        r = "\\{}b/".format(self.best.length())
                elif '~' in s:
                    r = "~"
                elif 'iscard' in s:
                    r = "x"
                elif 'ontinu' in s:
                    r = ">>"
                else:
                    pass

                    def rand():
                        chars = "/\\"
                        return chars[random.randint(0, len(chars) - 1)]
                    r += rand()
                    if random.randint(0, 1):
                        r += rand()
                print(r, end='')
                sys.stdout.flush()  # Required for py2.

            else:
                log_deeper('    ' * self.depth + s)

        def enter_maze(orig_xform, main_slot=0, prefix_hashes=None, new_game=False):
            if prefix_hashes is None:
                prefix_hashes = {'': 0}
            if new_game or (not main_slot and not prefix_hashes):
                self.depth = 0
                s = "Entering maze"
                if self.log_level > 0:
                    ori_phrase = "Stay a while, stay forever?"

                    phrase = ori_phrase
                    if self.log_level > 1:
                        phrase = ori_phrase[:random.randint(0, len(ori_phrase))]

                    if self.log_level > 2:
                        li = list(phrase)
                        for i in range(random.randint(0, int(len(phrase) / 2))):
                            a = random.randint(0, len(li) - 1)
                            b = random.randint(0, len(li) - 1)
                            li[a], li[b] = li[b], li[a]
                        phrase = "".join(li)

                    s += ". " + phrase

                print(s)

                extra_slots = 0
                for i, si in enumerate(orig_xform.used_slots()):
                    extra_slots += 1
                self.last_slot = main_slot + self.char_depth + extra_slots
            else:
                note("Continuing in maze '{}' (slot {})".format(orig_xform.compr_hash().rstrip(), main_slot))

                self.depth += 1
                for s in prefix_hashes:
                    assert len(s) < main_slot + 1

            log_deepest("Search slot {} ({} prefix(es), {})"
                        .format(main_slot,
                                len(prefix_hashes) if prefix_hashes else "@root",
                                prefix_hashes if prefix_hashes else "none"))

            num_slots = self.mutable_id_count()

            ret_reason = "Exiting maze (slot {}) : ".format(main_slot)
            if main_slot >= num_slots:
                log_deepest(ret_reason + "last slot reached")
                return

            if main_slot > self.last_slot:
                log_deepest(ret_reason + "last slot allowed")
                return

            xform = orig_xform.copy()
            # prefixed_hash2 = xform.compr_hash()

            next_hashes = []
            xform_results_len = {}
            char_hits_per_len = {}

            def inc_char_hit(len, char, freqs):
                if len not in freqs:
                    freqs[len] = {}

                if char not in freqs[len]:
                    freqs[len][char] = 0

                freqs[len][char] += 1

            all_results = []

            for prefix in prefix_hashes:
                for slot, c in enumerate(prefix[:main_slot]):
                    if c == ' ':
                        c = xform.default_replacement(slot)
                    xform.set_slot(slot, c)

                best_slot_ids = best_results(xform, main_slot, self.get_slot_candidates(xform, main_slot), try_func=self.try_one_freq)

                if best_slot_ids:
                    for id in best_slot_ids:
                        length = best_slot_ids[id].length

                        inc_char_hit(length, id, char_hits_per_len)

                        def get_xform():
                            xf = best_slot_ids[id].copy()
                            xf.slot_replacement = (main_slot, id,)
                            return xf

                        xf = get_xform()

                        all_results.append(xf.copy())

                        if length not in xform_results_len:
                            xform_results_len[length] = []

                        xform_results_len[length].append(xf)

            if not all_results:
                return

            next_hashes = []

            best = min(char_hits_per_len)
            # worst = max(char_hits_per_len)

            sorted_lengths = sorted(char_hits_per_len)
            # for length in sorted_lengths:
            #     print(sorted(char_hits_per_len[length].values()))
            #     print(length, char_hits_per_len[length])

            for xform in all_results:
                length = xform.length

                def keep_xform(xform):
                    discard_offset = 2
                    if length in sorted_lengths[discard_offset:]:
                        return False

                    # min_hits = min(char_hits_per_len[length].values())
                    max_hits = max(char_hits_per_len[length].values())
                    hits_sorted = sorted(char_hits_per_len[length].values())
                    median_hits = hits_sorted[int(len(hits_sorted) / 2)]

                    slot, repl_id = xform.slot_replacement
                    hits = char_hits_per_len[length][repl_id]

                    if length in sorted_lengths[1:]:
                        if hits != max_hits:
                            return False
                        return True

                    assert length == best

                    if hits == max_hits:
                        return True

                    # [TICKLE][TODO]
                    # if hits < median_hits:
                    if hits <= median_hits:
                        return False

                    return True

                if not keep_xform(xform):
                    continue

                xf = xform.copy()
                slot, id = xf.slot_replacement
                xf.set_slot(slot, id)

                next_hashes.append(xf.compr_hash()[:slot+1])

            next_slot = main_slot + 1
            new_xform = orig_xform.copy()

            # Free upcoming slot(s) (they may have been set to a fixed value)
            for slot in range(next_slot, next_slot + 1):
                new_xform.free_slot(next_slot)

            enter_maze(new_xform, next_slot, next_hashes)
            note("Returning from maze (slot {})".format(next_slot))

            return

        xform = XForm(self)

        if CONFIG_get('replace_ids_outside_of_search_range_with_match_hints'):
            self.apply_best_hints_to_xform(Cat_Match, xform, self.id_depth)

        start_slot = CONFIG_get('start_slot', 0)
        enter_maze(xform, start_slot, None, new_game=True)
        print()

        log_deeper("--")
        log_deeper("Search log")

        num_best_found = 0
        atb_length = self.best.length()
        xforms = []
        for k in self.compr_hashes:
            xform = self.compr_hashes[k]
            if xform.length == atb_length:
                num_best_found += 1
                xforms.append(xform.friendly_hash())

        log_deeper("{}/{} iteration(s) with best length ({}b [{}])"
                   .format(num_best_found, iter_count, atb_length,
                           list_s(xforms[:10])))

        for k in search_info:
            log_deeper(k + ": " + str(search_info[k]))

        if Info_Cache_Hits in search_info:
            hits = search_info[Info_Cache_Hits]
            accesses = search_info[Info_Cache_Accesses]
            log_deeper("Cache hits {:.2f}% ({}/{})".format(
                (hits * 100.0 / accesses), hits, accesses))

        log_deeper("--")

        return self.best.data_out

    def is_concat_compatible_replacement_slot(self, slot, new_id):
        concat_info = self.slots_concat_info[slot]
        first_new_id_char = new_id[0]

        if first_new_id_char in 'Xx':
            if 'd' in concat_info:
                return False
        if 'x' in concat_info:
            return not self.can_hex_concat(first_new_id_char)
        elif 'd' in concat_info:
            return not self.can_dec_concat(first_new_id_char)

        concat_info = concat_info.replace(
            'd', '').replace('x', '').strip()
        assert not concat_info, self.original_id_names[slot] + ":" + concat_info

        return True


def pack(args):
    try:
        print("Packing {} => {}".format(os.path.basename(args.filename_in),
              os.path.basename(args.filename_out)))
        file = open(args.filename_in, mode='rb')
    except EnvironmentError as e:
        if e.errno != errno.ENOENT:
            raise
        log_error("No such file: '{}'".format(args.filename_in))
        return None

    data = file.read()
    file.close()

    global pedantic, zopfli_iter_count
    log_level = args.verbose
    pedantic = args.pedantic

    zopfli_iter_count = -1
    if args.extreme is not None and args.extreme >= 0:
        zopfli_iter_count = args.extreme

    set_log_info(log_level, pedantic)

    tic = Tic(args).read(data, requires_source=True)

    if not tic:
        return

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

    tic_file_content = packer.tic.create(packer.best_source,
                                         packer.best.data_out)

    size_others = len(tic_file_content) - len(packer.best.data_out)
    size_desc = "+{}b".format(size_others)

    info = "Finalized ({})".format(size_desc)

    add_stage(info, source, packer, tic_file_content)

    # if args.single_pass:
    #     packer.write_tic()
    packer.write_tic(verbose=1)

    return packer.best
