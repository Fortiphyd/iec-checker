"""Tests for PLCOpen inspections."""
import sys
import os
import json
from collections import Counter

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, filter_warns  # noqa
from python.dump import DumpManager  # noqa


def test_cp1():
    f = 'st/plcopen-cp1.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(checker_warnings, 'PLCOPEN-CP1')) == 1
    with DumpManager(fdump):
        pass


def test_cp3():
    f = 'st/plcopen-cp3.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(fdump):
        pass
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* PLCOPEN-CP3 *)' in line]
    ws = filter_warns(warns, 'PLCOPEN-CP3')
    assert sorted(w.linenr for w in ws) == expected
    assert ws[0].msg.startswith('Variable B is read before it is initialized')


def test_cp6():
    f = 'st/plcopen-cp6.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(checker_warnings, 'PLCOPEN-CP6')) == 3
    with DumpManager(fdump):
        pass


def test_cp4():
    f = 'st/plcopen-cp4.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(fdump):
        pass
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if 'PLCOPEN-CP4' in line]
    assert sorted(w.linenr for w in filter_warns(warns, 'PLCOPEN-CP4')) == expected


def test_cp12():
    f = 'st/plcopen-cp12.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(fdump):
        pass
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if 'PLCOPEN-CP12' in line]
    assert sorted(w.linenr for w in filter_warns(warns, 'PLCOPEN-CP12')) == expected
    assert any('written in a loop' in w.msg for w in warns)


def test_cp20():
    f = 'st/plcopen-cp20.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(fdump):
        pass
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if 'PLCOPEN-CP20' in line]
    assert sorted(w.linenr for w in filter_warns(warns, 'PLCOPEN-CP20')) == expected
    assert any('called in a loop' in w.msg for w in warns)


def test_cp8():
    f = 'st/plcopen-cp8.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if 'PLCOPEN CP-8' in line]
    assert sorted(w.linenr for w in filter_warns(checker_warnings, 'PLCOPEN-CP8')) == expected
    with DumpManager(fdump):
        pass


def test_no_duplicate_warnings_in_nested_statements():
    """Expressions nested in statement bodies and function arguments are
    reported once."""
    f = 'st/nested-exprs.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(fdump):
        pass
    ids = ('PLCOPEN-CP8', 'PLCOPEN-CP28', 'PLCOPEN-N1')
    actual = Counter((w.id, w.linenr) for w in warns if w.id in ids)
    assert actual == Counter({
        ('PLCOPEN-CP8', 9): 1,
        ('PLCOPEN-CP28', 12): 1,
        ('PLCOPEN-N1', 15): 1,
        ('PLCOPEN-N1', 17): 1,
    })


def test_cp28():
    f = 'st/plcopen-cp28.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if 'PLCOPEN CP-28' in line]
    assert sorted(w.linenr for w in filter_warns(checker_warnings, 'PLCOPEN-CP28')) == expected
    with DumpManager(fdump):
        pass


def test_cp13():
    f = 'st/plcopen-cp13.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(checker_warnings, 'PLCOPEN-CP13')) == 1
    with DumpManager(fdump):
        pass


def test_cp25():
    f = 'st/plcopen-cp25.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(fdump):
        pass
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* PLCOPEN-CP25 *)' in line]
    ws = filter_warns(warns, 'PLCOPEN-CP25')
    assert sorted(w.linenr for w in ws) == expected
    msgs = {w.linenr: w.msg for w in ws}
    # The rule's own example.
    assert msgs[54] == ('Implicit conversion from REAL to INT variable I may lose '
                        'information; convert it explicitly')
    assert msgs[60] == 'Value 300 is out of range for SINT variable S (-128..127)'


def test_l10():
    f = 'st/plcopen-l10.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(checker_warnings, 'PLCOPEN-L10')) == 3
    with DumpManager(fdump):
        pass


def test_l17():
    f = 'st/plcopen-l17.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    l17_warns = filter_warns(checker_warnings, 'PLCOPEN-L17')
    assert any(w.linenr == 10 and w.column == 4 for w in l17_warns)
    with DumpManager(fdump):
        pass


def test_cp16():
    f = 'st/plcopen-cp16.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    cp16_warns = filter_warns(warns, 'PLCOPEN-CP16')
    assert len(cp16_warns) == 1
    with DumpManager(fdump):
        pass


def test_cp17():
    f = 'st/plcopen-cp17.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    cp17_warns = filter_warns(warns, 'PLCOPEN-CP17')
    expected = Counter([
        (4, 21), (5, 25), (5, 25), (6, 25), (9, 26), (9, 26),
        (10, 23), (12, 22), (12, 22), (17, 22), (18, 25), (25, 21),
    ])
    actual = Counter((w.linenr, w.column) for w in cp17_warns)
    assert actual == expected
    with DumpManager(fdump):
        pass


def test_cp26():
    f = 'st/plcopen-cp26.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    cp26_warns = filter_warns(warns, 'PLCOPEN-CP26')
    assert len(cp26_warns) == 2
    with DumpManager(fdump):
        pass


def test_l13():
    f = 'st/plcopen-l13.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    l13_warns = filter_warns(warns, 'PLCOPEN-L13')
    # Cases 1 (two uses, one as an array subscript), 4 and 5 of the sample.
    assert sorted(w.linenr for w in l13_warns) == [14, 15, 36, 43]
    with DumpManager(fdump):
        pass


def test_l22():
    f = 'st/plcopen-l22.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    l22_warns = filter_warns(warns, 'PLCOPEN-L22')
    assert len(l22_warns) == 3
    with DumpManager(fdump):
        pass


def test_n3():
    f = 'st/plcopen-n3.st'
    fdump = f'{f}.dump.json'
    checker_warnings, rc = run_checker([f])
    assert rc == 0
    n3_warns = filter_warns(checker_warnings, 'PLCOPEN-N3')
    assert [(w.linenr, w.column) for w in n3_warns] == [(6, 7)]
    with DumpManager(fdump):
        pass


def test_n3_names():
    """Reserved words are avoided in all names, not only variables."""
    f = 'st/plcopen-n3-names.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    names = sorted(w.msg.split(' is a reserved word')[0].split()[-1]
                   for w in filter_warns(warns, 'PLCOPEN-N3'))
    assert names == sorted(['MAX', 'TP', 'SEL', 'LEFT', 'LIMIT', 'LOG', 'LE', 'LEN', 'CTUD'])


def test_cp9():
    f = 'st/plcopen-cp9.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    # 27 statements; its McCabe complexity is 8, below the default of 15.
    [w] = filter_warns(warns, 'PLCOPEN-CP9')
    assert w.msg == 'CHARCURVE is too complex (27 statements)'
    with DumpManager(fdump):
        pass


def test_cp9_mccabe(tmp_path):
    cfg = tmp_path / 'iec_checker.json'
    cfg.write_text(json.dumps({'thresholds': {'mccabe_complexity': 0}}))
    cases = {
        'x := 1;': 1,
        'IF c THEN x := 1; ELSE x := 2; END_IF;': 2,
        'IF c THEN x := 1; END_IF; IF c THEN x := 2; END_IF;': 3,
        'IF c THEN x := 1; ELSIF x > 2 THEN x := 2; ELSE x := 3; END_IF;': 3,
        'CASE x OF 1: x := 2; 2: x := 3; 3: x := 4; END_CASE;': 4,
        'WHILE c DO IF c THEN EXIT; END_IF; END_WHILE;': 3,
        'RETURN; x := 1;': 1,
    }
    for body, expected in cases.items():
        f = tmp_path / 'p.st'
        f.write_text(f'PROGRAM p\nVAR x : INT; c : BOOL; END_VAR\n{body}\nEND_PROGRAM\n')
        warns, rc = run_checker([str(f)], args=['-c', str(cfg)])
        assert rc == 0
        with DumpManager(f'{f}.dump.json'):
            pass
        [w] = [w for w in filter_warns(warns, 'PLCOPEN-CP9') if 'McCabe' in w.msg]
        assert w.msg == f'p is too complex ({expected} McCabe complexity)', body


def test_n1():
    f = 'st/plcopen-n1.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    [w] = filter_warns(warns, 'PLCOPEN-N1')
    assert '%MW10.2.4.1' in w.msg
    with DumpManager(fdump):
        pass


def test_n1_in_array_subscript(tmp_path):
    f = tmp_path / 'n1.st'
    f.write_text('PROGRAM p\nVAR w : ARRAY [0..9] OF INT; x : INT; END_VAR\n'
                 'x := w[%MW6];\nEND_PROGRAM\n')
    warns, rc = run_checker([str(f)])
    assert rc == 0
    [w] = filter_warns(warns, 'PLCOPEN-N1')
    assert w.linenr == 3 and '%MW6' in w.msg
    with DumpManager(f'{f}.dump.json'):
        pass


def test_n2():
    f = 'st/plcopen-n2.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f], args=['-c', 'st/plcopen-n2.config.json'])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N2')) == 2
    with DumpManager(fdump):
        pass


def test_n4():
    f = 'st/plcopen-n4.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f], args=['-c', 'st/plcopen-n4.config.json'])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N4')) == 2
    with DumpManager(fdump):
        pass


def test_n5():
    f = 'st/plcopen-n5.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N5')) == 1
    with DumpManager(fdump):
        pass


def test_n6():
    f = 'st/plcopen-n6.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f], args=['-c', 'st/plcopen-n6.config.json'])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N6')) == 2
    with DumpManager(fdump):
        pass


def test_n8():
    f = 'st/plcopen-n8.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N8')) == 0
    with DumpManager(fdump):
        pass


def test_n9():
    f = 'st/plcopen-n9.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N9')) == 2
    with DumpManager(fdump):
        pass


def test_n10():
    f = 'st/plcopen-n10.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f], args=['-c', 'st/plcopen-n10.config.json'])
    assert rc == 0
    assert len(filter_warns(warns, 'PLCOPEN-N10')) == 2
    with DumpManager(fdump):
        pass


# {{{ Names of programs, classes, interfaces and types
NAMES = """TYPE motor_state : (RUN, STOP); END_TYPE
TYPE AnalogSignalRange : STRUCT lo : INT; END_STRUCT END_TYPE
TYPE stPoint : STRUCT x : INT; END_STRUCT END_TYPE
FUNCTION_BLOCK fb_Pump
VAR x : INT; END_VAR
x := 1;
END_FUNCTION_BLOCK
PROGRAM mainProgram
VAR y : INT; END_VAR
y := 1;
END_PROGRAM
"""


def check_names(tmp_path, config):
    f = tmp_path / 'names.st'
    f.write_text(NAMES)
    cfg = tmp_path / 'iec_checker.json'
    cfg.write_text(json.dumps({'naming_conventions': config}))
    warns, rc = run_checker([str(f)], args=['-c', str(cfg)])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    return warns


def test_n4_uses_names_as_written(tmp_path):
    warns = check_names(tmp_path, {'case': {'pou': 'lowerCamelCase', 'type': 'lower_snake_case'}})
    flagged = {(w.linenr, w.msg.split()[1]) for w in filter_warns(warns, 'PLCOPEN-N4')}
    # mainProgram and motor_state match; the other names are reported where declared.
    assert flagged == {(2, 'AnalogSignalRange'), (3, 'stPoint'), (4, 'fb_Pump')}


def test_n10_prefix_is_case_sensitive(tmp_path):
    warns = check_names(tmp_path, {'udt_prefixes': {'STRUCT': 'st', 'FUNCTION_BLOCK': 'FB_'}})
    flagged = {(w.linenr, w.msg.split()[1]) for w in filter_warns(warns, 'PLCOPEN-N10')}
    # stPoint has the prefix as written; fb_Pump doesn't.
    assert flagged == {(2, 'AnalogSignalRange'), (4, 'fb_Pump')}


def test_n9_reports_type_position(tmp_path):
    f = tmp_path / 'n9.st'
    f.write_text('TYPE Counter : STRUCT v : INT; END_STRUCT END_TYPE\n'
                 'PROGRAM p\nVAR Counter : INT; END_VAR\nCounter := 1;\nEND_PROGRAM\n')
    warns, rc = run_checker([str(f)])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    assert sorted(w.linenr for w in filter_warns(warns, 'PLCOPEN-N9')) == [1, 3]


def test_cp9_names_the_pou():
    f = 'st/plcopen-cp9.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    ws = filter_warns(warns, 'PLCOPEN-CP9')
    assert ws and all(w.linenr > 0 and w.msg.startswith('CHARCURVE is too complex') for w in ws)
# }}}


def test_cross_pou():
    """Rules about the whole application, checked across programs of one task
    and the function blocks they call."""
    f = 'st/cross-pou.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    ids = ('PLCOPEN-CP1', 'PLCOPEN-CP4', 'PLCOPEN-CP12', 'PLCOPEN-CP20', 'PLCOPEN-CP26')
    actual = sorted((w.id, w.linenr) for w in warns if w.id in ids)
    assert actual == sorted([
        # %MW600 is the global HEAD: written and read by address.
        ('PLCOPEN-CP1', 49), ('PLCOPEN-CP1', 50),
        # Located at the same output or memory in different POUs and globals;
        # the input %IX0.0 shared by both programs is fine.
        ('PLCOPEN-CP4', 11), ('PLCOPEN-CP4', 33), ('PLCOPEN-CP4', 34),
        ('PLCOPEN-CP4', 60), ('PLCOPEN-CP4', 61),
        # %QX0.0 and gOut written by both programs of the task, gOut also in
        # the FB P1 calls after writing it.
        ('PLCOPEN-CP12', 25), ('PLCOPEN-CP12', 51), ('PLCOPEN-CP12', 67),
        # The global instance gT called by both programs and by the FB; the
        # CTU called in a loop is an exception of the rule.
        ('PLCOPEN-CP20', 27), ('PLCOPEN-CP20', 51), ('PLCOPEN-CP20', 68),
        ('PLCOPEN-CP20', 71),
        # Written by both programs: through the FB, a struct member and an
        # output parameter.
        ('PLCOPEN-CP26', 25), ('PLCOPEN-CP26', 26), ('PLCOPEN-CP26', 69),
        ('PLCOPEN-CP26', 70),
    ])


def test_n5_scopes():
    """VAR_EXTERNAL refers to the global; resource globals and tasks are
    global names too."""
    f = 'st/plcopen-n5-scopes.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* N5 *)' in line]
    assert sorted(w.linenr for w in filter_warns(warns, 'PLCOPEN-N5')) == expected


def test_cp17_struct_members():
    """Accessing a member reads or writes the parameter."""
    f = 'st/plcopen-cp17-members.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    assert [(w.linenr, w.msg) for w in filter_warns(warns, 'PLCOPEN-CP17')] == [
        (5, "Input parameter 'OTHER' of function block F should not be written")]


def test_cp13_indirect():
    """Recursion through other POUs, FB instances and call arguments."""
    f = 'st/plcopen-cp13-indirect.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* CP13 *)' in line]
    ws = filter_warns(warns, 'PLCOPEN-CP13')
    assert sorted(w.linenr for w in ws) == expected
    assert ws[0].msg.endswith('FA calls itself through FB_')


def test_cp4_64_bit_types():
    """LTIME, LDT and DATE_AND_TIME are 8 bytes; the next address after them
    doesn't overlap."""
    f = 'st/plcopen-cp4-sizes.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    assert filter_warns(warns, 'PLCOPEN-CP4') == []


def test_cp6_constant_globals():
    """Referencing VAR_GLOBAL CONSTANT is an exception of the rule."""
    f = 'st/plcopen-cp6-constants.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* CP6 *)' in line]
    assert [w.linenr for w in filter_warns(warns, 'PLCOPEN-CP6')] == expected
