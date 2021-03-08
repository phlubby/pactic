from __future__ import print_function

import sys

log_level = 0
log_pedantic = False


def set_log_info(level, pedantic):
    global log_level, log_pedantic
    log_level = level
    log_pedantic = pedantic


def byte_length(byte_count):
    if log_pedantic:
        return "{0} byte{1}".format(byte_count, "s" if byte_count else "")
    else:
        return str(byte_count) + "b"


def log(s="", file=sys.stdout, min_log_level=1, extra="", end="\n"):
    if log_level >= min_log_level or file != sys.stdout:
        print(s + (extra if log_level >= 2 else ''), file=file, end=end)


def log_deep(s):
    return log(s, min_log_level=2)


def log_deeper(s):
    return log(s, min_log_level=3)


def log_deepest(s):
    return log(s, min_log_level=4)


def log_error(s, min_log_level=0):
    log("Error: " + s, sys.stderr, min_log_level)
