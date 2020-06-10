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
    ap.add_argument('-strip-casts', type=str, default='never',
                    help='The type of casts to ignore')
    ap.add_argument('-prefix', type=str, default='c__',
                    help='The prefix for any auto-generated names')
    ap.add_argument('-indent-style', type=str, default='kr',
                    help='The indentation style to use')
    ap.add_argument('-indent-offset', type=int, default=4,
                    help='The indentation offset to use')
    ap.add_argument('-annotate', type=str, default='cish',
                    help='Which annotations to use in the output')
    ap.add_argument('-o', type=str, default='-',
                    help='File in which to write the output')
    ap.add_argument('file', type=str,
                    help='The input file')
    cish_args, compiler_args = ap.parse_known_args()

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
                             if k != 'file'] + [tmp]

        if os.system(' '.join(cmd_cish)):
            os.remove(tmp)
            error('Could not convert file')
        os.remove(tmp)

    return 0


if __name__ == '__main__':
    sys.exit(main())
