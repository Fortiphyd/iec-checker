"""Tests for the TaintedVariable analysis pass.

Model:
  * sources: physical inputs (%I) and network-writable memory (%M);
  * sinks: outputs (%Q);
  * sanitizers: bounds checks (LIMIT, MAX/MIN clamps, IF range guards).

A finding is reported at the assignment to the sink. Each test program marks
the lines that must be reported with a ``(* TAINTED *)`` comment.
"""
import sys
import os
import json

import pytest

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, filter_warns  # noqa
from python.dump import DumpManager  # noqa

MARKER = '(* TAINTED *)'

DECLS = """PROGRAM p
VAR
  level  AT %IW0   : INT;   (* physical input *)
  sp     AT %MW100 : INT;   (* network-writable setpoint *)
  sp_max AT %MW101 : INT;   (* network-writable limit *)
  drive  AT %QW0   : INT;   (* output *)
  drive2 AT %QW1   : INT;   (* output *)
  tmp  : INT;
  tmp2 : INT;
  alarm : BOOL;
END_VAR
"""

# The current implementation predates the source/sink model.
pending = pytest.mark.xfail(
    strict=True, reason='source/sink/sanitizer model not implemented yet')


def run_taint(tmp_path, source, args=[]):
    """Run the checker on [source] and return its TaintedVariable warnings."""
    f = tmp_path / 'input.st'
    f.write_text(source)
    warns, rc = run_checker([str(f)], args=args)
    assert rc == 0, warns
    with DumpManager(f'{f}.dump.json'):
        pass
    return filter_warns(warns, 'TaintedVariable')


def check(tmp_path, source, args=[]):
    """Assert the reported lines are exactly those marked in [source]."""
    expected = sorted(i for i, line in enumerate(source.splitlines(), 1)
                      if MARKER in line)
    reported = sorted(w.linenr for w in run_taint(tmp_path, source, args))
    assert reported == expected


def check_body(tmp_path, body, args=[]):
    check(tmp_path, f'{DECLS}{body}\nEND_PROGRAM\n', args)


# {{{ Untrusted data reaching an output
def test_physical_input_to_output(tmp_path):
    check_body(tmp_path, f'drive := level; {MARKER}')


def test_network_memory_to_output(tmp_path):
    check_body(tmp_path, f'drive := sp; {MARKER}')


def test_expression(tmp_path):
    check_body(tmp_path, f'drive := sp * 2 + 10; {MARKER}')


def test_non_sanitizing_function(tmp_path):
    check_body(tmp_path, f'drive := ABS(sp); {MARKER}')


def test_direct_address_sink(tmp_path):
    check_body(tmp_path, f'%QW2 := sp; {MARKER}')


@pending
def test_through_intermediate(tmp_path):
    check_body(tmp_path, f'tmp := sp;\ndrive := tmp; {MARKER}')


@pending
def test_through_chain_of_intermediates(tmp_path):
    check_body(tmp_path, f'tmp := sp;\ntmp2 := tmp + 1;\ndrive := tmp2; {MARKER}')


@pending
def test_network_memory_written_by_program_is_still_untrusted(tmp_path):
    # The network can still write %MW100 even if the program also does.
    check_body(tmp_path,
               f'drive := sp; {MARKER}\nIF alarm THEN\n  sp := 0;\nEND_IF;')


@pending
def test_bound_from_untrusted_source(tmp_path):
    # A limit an attacker can set is not a bounds check.
    check_body(tmp_path, f'drive := LIMIT(0, sp, sp_max); {MARKER}')


@pending
def test_retainted_after_sanitizing(tmp_path):
    check_body(tmp_path,
               f'tmp := LIMIT(0, sp, 1500);\ntmp := sp;\ndrive := tmp; {MARKER}')


def test_in_function_block(tmp_path):
    check(tmp_path, f"""FUNCTION_BLOCK fb
VAR
  sp    AT %MW100 : INT;
  drive AT %QW0   : INT;
END_VAR
drive := sp; {MARKER}
END_FUNCTION_BLOCK
""")


def test_message_names_sink_and_source(tmp_path):
    [w] = run_taint(tmp_path, f'{DECLS}drive := sp;\nEND_PROGRAM\n')
    assert 'DRIVE' in w.msg.upper() and 'SP' in w.msg.upper()
# }}}


# {{{ No finding
def test_constant_to_output(tmp_path):
    check_body(tmp_path, 'drive := 100;')


def test_trusted_local_to_output(tmp_path):
    check_body(tmp_path, 'tmp := 5;\ndrive := tmp;')


@pending
def test_untrusted_to_non_output(tmp_path):
    check_body(tmp_path, 'tmp := sp;')


@pending
def test_output_is_not_a_source(tmp_path):
    check_body(tmp_path, 'drive := drive2;')


@pending
def test_limit(tmp_path):
    check_body(tmp_path, 'drive := LIMIT(0, sp, 1500);')


@pending
def test_max_min_clamp(tmp_path):
    check_body(tmp_path, 'drive := MAX(0, MIN(sp, 1500));')


@pending
def test_sanitized_intermediate(tmp_path):
    check_body(tmp_path, 'tmp := LIMIT(0, sp, 1500);\ndrive := tmp;')


@pending
def test_if_range_guard(tmp_path):
    check_body(tmp_path, 'IF sp >= 0 AND sp <= 1500 THEN\n  drive := sp;\nEND_IF;')


@pending
def test_if_clamp(tmp_path):
    check_body(tmp_path, '\n'.join([
        'tmp := sp;',
        'IF tmp > 1500 THEN',
        '  tmp := 1500;',
        'ELSIF tmp < 0 THEN',
        '  tmp := 0;',
        'END_IF;',
        'drive := tmp;',
    ]))
# }}}


def test_disabled_by_config(tmp_path):
    cfg = tmp_path / 'iec_checker.json'
    cfg.write_text(json.dumps(
        {'detectors': {'disabled': ['TaintedVariable']}}))
    check_body(tmp_path, 'drive := sp;', args=['-c', str(cfg)])
