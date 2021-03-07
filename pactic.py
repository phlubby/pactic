#!/usr/bin/env python

# based on this one, from Gargaj
#
# https://gist.github.com/Gargaj/5bf66c128c6c6c47f4c78de630e56569
#
# TIC-80 packer
#
#  Uses the zlib code chunk to crunch down your source
#  https://github.com/nesbox/TIC-80/wiki/tic-File-Format
#
from __future__ import print_function

import argparse
import sys

from src.packer import pack, packer_names

prog = sys.argv[0]
PROG_VER = "0.3b"
parser = argparse.ArgumentParser(prog=prog)

parser.add_argument('luafile', nargs='*',
                    help="Lua file to compress and use for TIC file")

parser.add_argument('-o', dest='ticfile', default=None,
                    help="Optional TIC output filename (default is "
                         "based on source file)")

parser.add_argument('--verbose', '-v', action='count', default=0,
                    help="Increase verbosity level")

parser.add_argument('--single-pass', '-s', action='store_true',
                    help="Don't perform variations stage")

parser.add_argument('--default-chunk', '-d', action='store_true',
                    help="Write default chunk for sweety16 palette usage")

parser.add_argument('--pedantic', action='store_true',
                    help="Write fully compliant files")

parser.add_argument('--version', '-V', action='version',
                    version='%(prog)s version ' + PROG_VER)

group = parser.add_argument_group('Compression exclusion options')

for s in packer_names():
    arg_name = '--no-' + s.lower()
    group.add_argument(arg_name, action='store_true', default=False)

args = parser.parse_args()

if not args.luafile:

    s = "usage: {} file.lua\n".format(sys.argv[0])
    s += "Creates file.tic by compressing file.lua\n"
    s += "Use -h for more help".format(prog)
    print(s, file=sys.stderr)
    exit()

args.shown_break_msg = False
for filename in args.luafile:
    with open(filename, mode='rb') as file:
        args.filename_in = filename
        source = file.read()

        pack(source, args)
