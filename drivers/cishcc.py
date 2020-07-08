#!/usr/bin/env python3

import argparse
from enum import auto, IntEnum
import os
from tempfile import NamedTemporaryFile
import subprocess as proc
import shlex
import sys
try:
    from termcolor import colored
except ImportError:
    pass


CLANG_C = '@CMAKE_C_COMPILER@'
CLANG_CXX = '@CMAKE_CXX_COMPILER@'


def error(msg: str) -> None:
    try:
        print(colored('error:', 'red', attrs=['bold']),
              colored(msg, attrs=['bold']),
              file=sys.stderr)
    except NameError:
        print('ERROR:', msg)
    sys.exit(1)


def warning(msg: str) -> None:
    try:
        print(colored('warning:', 'magenta', attrs=['bold']),
              colored(msg, attrs=['bold']),
              file=sys.stderr)
    except NameError:
        print('WARNING:', msg)


def convert_optname(k: str) -> str:
    return shlex.quote(k.replace('_', '-'))


def convert_optval(v: str) -> str:
    return shlex.quote(v)


def main() -> int:
    prg = os.path.basename(sys.argv[0])
    cish = os.path.join(os.path.abspath(os.path.dirname(__file__)), 'cish')
    lang = None
    compiler = None
    if prg == 'cishcc':
        lang, compiler = 'C', shlex.quote(CLANG_C)
    elif prg == 'cish++':
        lang, compiler = 'C++', shlex.quote(CLANG_CXX)
    else:
        error('Unexpected CISH wrapper')

    ap = argparse.ArgumentParser(
        'Wrapper script for CISH ({})'.format(lang))
    ap.add_argument('-strip-casts', type=str,
                    help='The type of casts to ignore')
    ap.add_argument('-prefix', type=str,
                    help='The prefix for any auto-generated names')
    ap.add_argument('-indent-style', type=str,
                    help='The indentation style to use')
    ap.add_argument('-indent-offset', type=int,
                    help='The indentation offset to use')
    ap.add_argument('-parens', type=str,
                    help='How to parenthetize')
    ap.add_argument('-annotate', type=str,
                    help='Which annotations to use in the output')
    ap.add_argument('-log', type=str,
                    help='Create logs of the conversion '
                    '(WARNING - Creates many files in the current directory)')
    ap.add_argument('-log-dir',
                    help='Create the log files in the specified directory')
    ap.add_argument('-verbose', action='store_true',
                    help='Print messages during the conversion')
    ap.add_argument('-o', type=str,
                    help='File in which to write the output')
    ap.add_argument('file', type=str,
                    help='The input file')

    # Disallowed compiler options
    # FIXME: There are probably more flags that are incompatible
    incompatible = ['c', 'S', 'E', 'emit-llvm']
    for flag in incompatible:
        ap.add_argument('-' + flag, action='store_true')

    cish_args, compiler_args = ap.parse_known_args()

    for flag in incompatible:
        if getattr(cish_args, flag.replace('-', '_')):
            error('Incompatible flag -{}'.format(flag))

    with NamedTemporaryFile(delete=False) as tmpf:
        tmp = shlex.quote(tmpf.name)

        cmd_compile = ' '.join(
            [compiler, '-c -emit-llvm -o', tmp, shlex.quote(cish_args.file)]
            + [shlex.quote(str(s)) for s in compiler_args])

        if os.system(cmd_compile):
            error('Could not compile file')

        cmd_cish = [cish] + ['-{}={}'.format(convert_optname(k),
                                             convert_optval(str(v)))
                             for k, v in vars(cish_args).items()
                             if v and (k != 'file')] + [tmp]

        if os.system(' '.join(cmd_cish)):
            os.remove(tmp)
            error('Could not convert file')
        os.remove(tmp)

    return 0


if __name__ == '__main__':
    sys.exit(main())
