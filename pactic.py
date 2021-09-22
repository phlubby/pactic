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
import os
import sys

from src.common import log_error
from src.packer import pack, packer_names

prog = 'pactic'
PROG_VER = "0.6c"
parser = argparse.ArgumentParser(prog=prog)

parser.add_argument('inputfile', nargs='*',
                    help="Lua or TIC input file to compress")

parser.add_argument('-o', dest='ticfile', default=None,
                    help="TIC output filename or directory")

parser.add_argument('--verbose', '-v', action='count', default=0,
                    help="Increase verbosity level")

parser.add_argument('--default-chunk', '-c', action='store_true',
                    help="Write default chunk for Sweetie 16 palette usage")

parser.add_argument('--single-pass', '-s', action='store_true',
                    help="Don't perform variations stage")

default_depth = 3
parser.add_argument('--depth', '-d', type=int, default=-1,
                    help="Change initial search depth of "+str(default_depth))

extreme_zopfli_iter_count = 400
parser.add_argument('--extreme', '-x', nargs='?', type=int, const=extreme_zopfli_iter_count,
                    help="Use extreme Zopfli settings by changing iteration count (defaults to {})".format(extreme_zopfli_iter_count))

parser.add_argument('--no-transform', '-t', action='store_true',
                    help="Don't attempt to transform functions")

parser.add_argument('--pedantic', action='store_true',
                    help="Write fully compliant files")

parser.add_argument('--version', '-V', action='version',
                    version='%(prog)s version ' + PROG_VER)

group = parser.add_argument_group('Compression exclusion options')

for s in packer_names():
    arg_name = '--no-' + s.lower()
    group.add_argument(arg_name, action='store_true', default=False)

args = parser.parse_args()

if not args.inputfile:

    s = "usage:\n".format(prog)
    ex = [("file.lua", "Create file.tic from file.lua")]
    ex += [("cart.tic -o packed.tic", "Create packed.tic from cart.tic")]
    ex += [("*.lua -o packed/", "Process multiple files and store in packed/")]
    for e in ex:
        s += "{} {}{}# {}\n".format(prog, e[0], ' ' * (25 - len(e[0])), e[1])

    s += "\nUse -h for more help".format(prog)
    print(s, file=sys.stderr)
    exit()

if not args.ticfile:
    single = len(args.inputfile) == 1
    for filename in args.inputfile:
        if '.tic' == os.path.splitext(filename)[1].lower():
            s = ("Input file '{}' is a TIC file and would be overwritten.\n\n"
                 "Use -o to specify explicit output ")
            if single:
                s += "TIC file or "
            s += "directory."
            log_error(s.format(filename))
            exit()

args.shown_break_msg = False
for file_in in args.inputfile:
    path_out = args.ticfile

    default_out = os.path.splitext(file_in)[0] + '.tic'
    if not path_out:
        path_out = default_out
    elif os.path.isdir(path_out):
        path_out = os.path.join(path_out, os.path.basename(default_out))
    elif not os.path.splitext(path_out)[1]:
        log_error("No such directory: '{}'".format(path_out))
        exit()

    args.filename_in = file_in
    args.filename_out = path_out

    pack(args)
