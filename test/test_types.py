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


def test_mixed_type_arithmetic():
    ws = check_marked('st/mixed-type-arithmetic.st', 'MixedTypeArithmetic')
    assert ws[0].msg == 'Arithmetic on INT and DINT; convert one operand explicitly'
