"""Tests for PLCOpen inspections."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, filter_warns  # noqa
from python.dump import DumpManager  # noqa


def test_cp1():
    f = 'st/plcopen-cp1.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f)
    assert rc == 0
    assert len(checker_warnings) == 2
    cv = checker_warnings[0]
    assert cv.id == 'PLCOPEN-CP1'
    assert cv.linenr == 5
    assert cv.column == 13
    with DumpManager(fdump):
        pass


def test_cp13():
    f = 'st/plcopen-cp13.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f)
    assert rc == 0
    assert len(checker_warnings) >= 1
    cv = checker_warnings[0]
    assert cv.id == 'PLCOPEN-CP13'
    assert cv.linenr == 8
    assert cv.column == 30
    with DumpManager(fdump):
        pass


def test_l17():
    f = 'st/plcopen-l17.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f)
    assert rc == 0
    assert len(checker_warnings) >= 1
    cv = checker_warnings[0]
    assert cv.id == 'PLCOPEN-L17'
    assert cv.linenr == 10
    assert cv.column == 4
    with DumpManager(fdump):
        pass


def test_n3():
    f = 'st/plcopen-n3.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker(f)
    assert rc == 0
    assert len(checker_warnings) >= 1
    cv = checker_warnings[0]
    assert cv.id == 'PLCOPEN-N3'
    assert cv.linenr == 6
    assert cv.column == 7
    with DumpManager(fdump):
        pass


def test_cp9():
    f = 'st/plcopen-cp9.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker(f)
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-CP9')) == 1
    with DumpManager(fdump):
        pass
