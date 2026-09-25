"""Tests for the checks based on expression types."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, filter_warns  # noqa
from python.dump import DumpManager  # noqa


def check_marked(f, warn_id):
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if f'(* {warn_id} *)' in line]
    assert sorted(w.linenr for w in filter_warns(warns, warn_id)) == expected
    return filter_warns(warns, warn_id)


def test_narrowing_assignment():
    ws = check_marked('st/narrowing-assignment.st', 'NarrowingAssignment')
    msgs = {w.linenr: w.msg for w in ws}
    assert msgs[38] == 'DINT value assigned to INT variable I may not fit; convert it explicitly'
    assert msgs[44] == 'Value 300 is out of range for SINT variable S (-128..127)'
    assert msgs[46] == 'Value -1 is out of range for UINT variable U (0..65535)'


def test_mixed_type_arithmetic():
    ws = check_marked('st/mixed-type-arithmetic.st', 'MixedTypeArithmetic')
    assert ws[0].msg == 'Arithmetic on INT and DINT; convert one operand explicitly'
