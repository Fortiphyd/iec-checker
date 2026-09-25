"""Tests for the TaintedVariable analysis pass."""
import sys
import os
import re
import json
from collections import Counter

import pytest

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, filter_warns  # noqa
from python.dump import DumpManager  # noqa

TAINT_RE = re.compile(r'Variable (\S+) has been tainted by variable (\S+)')

# Two located sources that are never written, plus some plain locals.
DECLS = """
PROGRAM p
VAR
  in1 AT %IW0 : INT;
  in2 AT %IW1 : INT;
  y : INT;
  z : INT;
  c : BOOL;
  arr : ARRAY [0..3] OF INT;
END_VAR
"""


def check_taint(tmp_path, source, args=[]):
    """Run the checker on [source] and return a Counter of
    (tainted_var, source_var) pairs from TaintedVariable warnings."""
    f = tmp_path / 'input.st'
    f.write_text(source)
    warns, rc = run_checker([str(f)], args=args)
    assert rc == 0, warns
    with DumpManager(f'{f}.dump.json'):
        pass
    return Counter(TAINT_RE.match(w.msg).groups()
                   for w in filter_warns(warns, 'TaintedVariable'))


def check_body(tmp_path, body):
    return check_taint(tmp_path, f'{DECLS}{body}\nEND_PROGRAM\n')


# {{{ Supported flows
def test_direct_assignment(tmp_path):
    assert check_body(tmp_path, 'y := in1;') == Counter({('Y', 'IN1'): 1})


def test_nested_expression(tmp_path):
    assert check_body(tmp_path, 'y := (in1 + 1) * 2;') == \
        Counter({('Y', 'IN1'): 1})


def test_unary_expression(tmp_path):
    assert check_body(tmp_path, 'y := -in1;') == Counter({('Y', 'IN1'): 1})


def test_multiple_sources(tmp_path):
    assert check_body(tmp_path, 'y := in1 + in2;') == \
        Counter({('Y', 'IN1'): 1, ('Y', 'IN2'): 1})


def test_function_call_argument(tmp_path):
    assert check_body(tmp_path, 'y := MAX(in1, 3);') == \
        Counter({('Y', 'IN1'): 1})


def test_array_element_lhs(tmp_path):
    assert check_body(tmp_path, 'arr[1] := in1;') == \
        Counter({('ARR', 'IN1'): 1})


def test_function_return_value(tmp_path):
    src = """
FUNCTION f : INT
VAR
  in1 AT %IW0 : INT;
END_VAR
f := in1;
END_FUNCTION
"""
    assert check_taint(tmp_path, src) == Counter({('F', 'IN1'): 1})


def test_function_block(tmp_path):
    src = """
FUNCTION_BLOCK fb
VAR
  in1 AT %IW0 : INT;
  y : INT;
END_VAR
y := in1;
END_FUNCTION_BLOCK
"""
    assert check_taint(tmp_path, src) == Counter({('Y', 'IN1'): 1})
# }}}


# {{{ No false positives
def test_constant_assignment(tmp_path):
    assert check_body(tmp_path, 'y := 42;') == Counter()


def test_unlocated_variable_is_not_a_source(tmp_path):
    assert check_body(tmp_path, 'z := 1;\ny := z;') == Counter()


def test_written_located_variable_is_not_a_source(tmp_path):
    # A located variable written by the program is treated as an output.
    assert check_body(tmp_path, 'in1 := 0;\ny := in1;') == Counter()
# }}}


# {{{ Integration with the driver
def test_sample_file():
    f = 'st/dvar.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    tainted = filter_warns(warns, 'TaintedVariable')
    assert Counter((w.linenr, TAINT_RE.match(w.msg).groups())
                   for w in tainted) == Counter({
                       (16, ('I', 'HEAD')): 1,
                       (18, ('I', 'HEAD')): 1,
                       (19, ('I', 'UNUSED_VAR')): 1,
                       (20, ('I', 'HEAD')): 1,
                       (21, ('I', 'HEAD')): 1,
                   })


def test_disabled_by_config(tmp_path):
    cfg = tmp_path / 'iec_checker.json'
    cfg.write_text(json.dumps(
        {'detectors': {'disabled': ['TaintedVariable']}}))
    assert check_taint(tmp_path, f'{DECLS}y := in1;\nEND_PROGRAM\n',
                       args=['-c', str(cfg)]) == Counter()
# }}}


# {{{ Known limitations
@pytest.mark.xfail(strict=True, reason='statements nested in IF are visited twice')
def test_no_duplicate_warnings_in_if_body(tmp_path):
    assert check_body(tmp_path, 'IF c THEN\n  y := in1;\nEND_IF;') == \
        Counter({('Y', 'IN1'): 1})


@pytest.mark.xfail(strict=True, reason='taint is not propagated between variables')
def test_transitive(tmp_path):
    assert check_body(tmp_path, 'y := in1;\nz := y;') == \
        Counter({('Y', 'IN1'): 1, ('Z', 'IN1'): 1})


@pytest.mark.xfail(strict=True, reason='named call parameters are not followed')
def test_named_function_parameter(tmp_path):
    assert check_body(tmp_path, 'y := MAX(IN1 := in1, IN2 := 3);') == \
        Counter({('Y', 'IN1'): 1})


@pytest.mark.xfail(strict=True, reason='array subscripts are not inspected')
def test_array_index(tmp_path):
    assert check_body(tmp_path, 'y := arr[in1];') == \
        Counter({('Y', 'IN1'): 1})


@pytest.mark.xfail(strict=True, reason='analysis is flow-insensitive')
def test_source_written_after_use(tmp_path):
    assert check_body(tmp_path, 'y := in1;\nin1 := 0;') == \
        Counter({('Y', 'IN1'): 1})


@pytest.mark.xfail(strict=True, reason='implicit flows are not tracked')
def test_implicit_flow(tmp_path):
    assert check_body(tmp_path, 'IF in1 > 5 THEN\n  y := 1;\nEND_IF;') == \
        Counter({('Y', 'IN1'): 1})


@pytest.mark.xfail(strict=True, reason='parser rejects direct addresses in expressions')
def test_bare_direct_address(tmp_path):
    assert check_body(tmp_path, 'y := %IW2;') == Counter({('Y', '%IW2'): 1})
# }}}
