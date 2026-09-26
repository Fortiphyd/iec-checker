"""Tests for the DuplicateCode and InconsistentCopy analysis passes."""
import sys
import os
import json

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, filter_warns  # noqa
from python.dump import DumpManager  # noqa


def check(tmp_path, source, args=[]):
    f = tmp_path / 'input.st'
    f.write_text(source)
    warns, rc = run_checker([str(f)], args=args)
    assert rc == 0, warns
    with DumpManager(f'{f}.dump.json'):
        pass
    return (filter_warns(warns, 'DuplicateCode'),
            filter_warns(warns, 'InconsistentCopy'))


def pump(n, fault=None, sp_max=1500):
    """Control logic for one pump, as it would be copied for each pump."""
    fault = fault or f'P{n}_FAULT'
    return f"""  IF P{n}_START AND NOT P{n}_FAULT AND NOT E_STOP THEN
    P{n}_RUN := TRUE;
  ELSIF P{n}_STOP OR {fault} THEN
    P{n}_RUN := FALSE;
  END_IF;
  P{n}_ALARM := P{n}_FAULT AND P{n}_RUN;
  P{n}_SPEED := LIMIT(0, P{n}_SP, {sp_max});
"""


def program(*bodies):
    decls = '\n'.join(
        f'    P{n}_START, P{n}_STOP, P{n}_FAULT, P{n}_RUN, P{n}_ALARM : BOOL;\n'
        f'    P{n}_SPEED, P{n}_SP : INT;' for n in range(1, 5))
    return f"""PROGRAM pumps
  VAR
    E_STOP, P10_FAULT : BOOL;
{decls}
  END_VAR
{''.join(bodies)}END_PROGRAM
"""


# Line numbers in program(): the first body starts on line 13 and each pump
# body is 7 lines.


# {{{ DuplicateCode
def test_duplicate_within_pou(tmp_path):
    dups, _ = check(tmp_path, program(pump(1), pump(2)))
    assert [(w.linenr, w.msg) for w in dups] == [
        (20, 'Lines 20-26 duplicate lines 13-19')]


def test_duplicate_across_pous(tmp_path):
    fb = """FUNCTION_BLOCK {name}
  VAR_INPUT
    start, stop, fault : BOOL;
    sp : INT;
  END_VAR
  VAR_OUTPUT
    run, alarm : BOOL;
    speed : INT;
  END_VAR
  IF start AND NOT fault THEN
    run := TRUE;
  ELSIF stop OR fault THEN
    run := FALSE;
  END_IF;
  alarm := fault AND run;
  speed := LIMIT(0, sp, 1500);
END_FUNCTION_BLOCK
"""
    dups, _ = check(tmp_path, fb.format(name='fb1') + fb.format(name='fb2'))
    assert [w.msg for w in dups] == ['Lines 27-33 duplicate lines 10-16 in FB1']


def test_different_literals_are_duplicates(tmp_path):
    dups, _ = check(tmp_path, program(pump(1, sp_max=1500), pump(2, sp_max=3000)))
    assert len(dups) == 1


def test_three_copies_reported_against_first(tmp_path):
    dups, _ = check(tmp_path, program(pump(1), pump(2), pump(3)))
    assert [w.msg for w in dups] == [
        'Lines 20-26 duplicate lines 13-19',
        'Lines 27-33 duplicate lines 13-19',
    ]


def test_consecutive_copies_reported_once_each(tmp_path):
    # Copies 1-2 also match copies 3-4; only single copies are reported.
    dups, _ = check(tmp_path, program(pump(1), pump(2), pump(3), pump(4)))
    assert [w.msg for w in dups] == [
        'Lines 20-26 duplicate lines 13-19',
        'Lines 27-33 duplicate lines 13-19',
        'Lines 34-40 duplicate lines 13-19',
    ]


def test_nested_duplicate_reported_once(tmp_path):
    # The IF bodies are also duplicates of each other, but are part of the
    # duplicated IF statements.
    dups, _ = check(tmp_path, program(pump(1), pump(2)))
    assert len(dups) == 1


def test_small_duplicate_not_reported(tmp_path):
    dups, _ = check(tmp_path, program(
        '  P1_RUN := P1_START AND NOT P1_FAULT;\n',
        '  P2_RUN := P2_START AND NOT P2_FAULT;\n'))
    assert dups == []


def test_different_structure_not_reported(tmp_path):
    dups, _ = check(tmp_path, program(pump(1), """  IF P2_START AND P2_FAULT THEN
    P2_RUN := FALSE;
  END_IF;
  P2_ALARM := P2_FAULT OR P2_RUN;
  P2_SPEED := MIN(P2_SP, 1500);
"""))
    assert dups == []


def test_repeated_statement_not_reported(tmp_path):
    dups, _ = check(tmp_path, program(''.join(
        f'  P{n}_{v} := FALSE;\n' for n in range(1, 4)
        for v in ('START', 'STOP', 'FAULT', 'RUN', 'ALARM'))))
    assert dups == []


def test_minimum_size_from_config(tmp_path):
    cfg = tmp_path / 'iec_checker.json'
    cfg.write_text(json.dumps({'thresholds': {'duplicate_code_size': 100}}))
    dups, _ = check(tmp_path, program(pump(1), pump(2)), args=['-c', str(cfg)])
    assert dups == []
# }}}


# {{{ InconsistentCopy
def test_name_not_renamed(tmp_path):
    _, bad = check(tmp_path, program(pump(1), pump(2, fault='P1_FAULT')))
    assert [(w.linenr, w.msg) for w in bad] == [
        (22, 'P1_FAULT may be a copy-paste error: this copy of lines 13-19 '
             'renames P1 to P2, so P2_FAULT was expected')]


def test_name_renamed_differently(tmp_path):
    _, bad = check(tmp_path, program(pump(1), pump(2, fault='P3_FAULT')))
    assert [w.linenr for w in bad] == [22]
    assert 'P2_FAULT was expected' in bad[0].msg


def test_consistent_copy(tmp_path):
    _, bad = check(tmp_path, program(pump(1), pump(2)))
    assert bad == []


def test_shared_variable_is_not_inconsistent(tmp_path):
    # E_STOP is used unchanged by both copies.
    _, bad = check(tmp_path, program(pump(1), pump(2), pump(3)))
    assert bad == []


def test_rename_matches_whole_words(tmp_path):
    # P10_FAULT is shared; it isn't P1 followed by 0.
    _, bad = check(tmp_path, program(pump(1, fault='P10_FAULT'), pump(2, fault='P10_FAULT')))
    assert bad == []


def test_inconsistent_copy_among_consecutive_copies(tmp_path):
    _, bad = check(tmp_path, program(pump(1), pump(2), pump(3, fault='P2_FAULT'), pump(4)))
    assert [w.linenr for w in bad] == [29]
    assert 'P3_FAULT was expected' in bad[0].msg


def test_inconsistent_copy_reported_once(tmp_path):
    # The error in the third copy is seen against both other copies.
    _, bad = check(tmp_path, program(pump(1), pump(2), pump(3, fault='P1_FAULT')))
    assert [w.linenr for w in bad] == [29]


def test_passes_disabled_separately(tmp_path):
    cfg = tmp_path / 'iec_checker.json'
    cfg.write_text(json.dumps({'detectors': {'disabled': ['DuplicateCode']}}))
    dups, bad = check(tmp_path, program(pump(1), pump(2, fault='P1_FAULT')),
                      args=['-c', str(cfg)])
    assert dups == [] and len(bad) == 1
# }}}
