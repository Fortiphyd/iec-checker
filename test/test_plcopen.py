"""Tests for PLCOpen inspections."""
import sys
import os
import json
from collections import Counter

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, run_checker_full_out, binary_default, filter_warns  # noqa
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
    ints = [w.linenr for w in filter_warns(checker_warnings, 'PLCOPEN-CP28')
            if w.msg.endswith('(the integer holds a time)')]
    assert ints == [27, 77, 82, 85, 88, 91]
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
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* CP16 *)' in line]
    assert sorted(w.linenr for w in cp16_warns) == expected
    msgs = {w.linenr: w.msg for w in cp16_warns}
    assert msgs[36] == "Task FAST should call PROGRAM, not FUNCTION 'MYFUN'"
    assert msgs[46] == ("Task SLOW_1 should call PROGRAM, not FUNCTION_BLOCK instance "
                        "'FB1' of program P2")
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
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* L22 *)' in line]
    assert sorted(w.linenr for w in l22_warns) == expected
    msgs = {w.linenr: w.msg for w in l22_warns}
    assert msgs[61] == ("Loop variable 'I' should not be modified inside a FOR loop "
                        "(passed to VAR_IN_OUT POS of ST)")
    assert msgs[64].endswith('(by output NEXT of ST)')
    assert msgs[72] == ("Variable 'N' of the final value or increment of a FOR loop "
                        "should not be modified inside the loop")
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
    # Both are within the default thresholds of 15 and 25.
    assert filter_warns(warns, 'PLCOPEN-CP9') == []
    with DumpManager(fdump):
        pass


def test_cp9_rule_examples(tmp_path):
    """The values of the rule's examples: statements, and McCabe complexity in
    both variants."""
    f = 'st/plcopen-cp9.st'
    for variant, suffix, mccabe in [
            ('standard', '', (8, 6)),
            ('plcopen', ", weighted as in PLCopen's examples", (12, 8))]:
        cfg = tmp_path / 'iec_checker.json'
        cfg.write_text(json.dumps({'thresholds': {
            'mccabe_complexity': 0, 'statements_count': 0, 'mccabe_variant': variant}}))
        warns, rc = run_checker([f], args=['-c', str(cfg)])
        assert rc == 0
        with DumpManager(f'{f}.dump.json'):
            pass
        assert sorted(w.msg for w in filter_warns(warns, 'PLCOPEN-CP9')) == sorted([
            f'CHARCURVE is too complex ({mccabe[0]} McCabe complexity{suffix})',
            'CHARCURVE is too complex (18 statements)',
            f'CHARCURVE2 is too complex ({mccabe[1]} McCabe complexity{suffix})',
            'CHARCURVE2 is too complex (12 statements)'])


def test_cp9_unknown_variant(tmp_path):
    cfg = tmp_path / 'iec_checker.json'
    cfg.write_text(json.dumps({'thresholds': {'mccabe_variant': 'myers'}}))
    rc, out = run_checker_full_out(['st/plcopen-cp9.st'], binary_default, '-c', str(cfg))
    assert rc != 0
    assert 'thresholds.mccabe_variant: unknown variant "myers"' in out


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
    ws = filter_warns(warns, 'PLCOPEN-N8')
    with open(f, encoding='utf-8') as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* PLCOPEN-N8 *)' in line]
    assert sorted(w.linenr for w in ws) == expected
    by_line = {w.linenr: w for w in ws}
    assert by_line[14].msg == ('Identifier départ contains characters outside ASCII '
                               'letters, digits and underscores (é)')
    # Columns count characters, not bytes.
    assert (by_line[14].start_column, by_line[14].column) == (5, 10)
    assert by_line[16].msg == 'Identifier double__underscore contains consecutive underscores'
    assert by_line[17].msg == 'Identifier trailing_ contains a trailing underscore'
    with DumpManager(fdump):
        pass


def test_n8_national_character_set(tmp_path):
    """With allow_non_ascii, only the underscores are reported."""
    cfg = tmp_path / 'c.json'
    cfg.write_text(json.dumps({'naming_conventions': {'allow_non_ascii': True}}))
    f = 'st/plcopen-n8.st'
    warns, rc = run_checker([f], args=['-c', str(cfg)])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    assert sorted(w.linenr for w in filter_warns(warns, 'PLCOPEN-N8')) == [16, 17, 29]


def test_n9():
    f = 'st/plcopen-n9.st'
    fdump = f'{f}.dump.json'
    warns, rc = run_checker([f])
    assert rc == 0
    ws = filter_warns(warns, 'PLCOPEN-N9')
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* PLCOPEN-N9 *)' in line]
    assert sorted(w.linenr for w in ws) == expected
    msgs = {w.linenr: w.msg for w in ws}
    assert msgs[9] == ('Name MyCalculation of this FUNCTION_BLOCK is also used for '
                       'a variable on line 11, a global variable on line 35')
    assert msgs[17] == 'Name Scale of this FUNCTION is also used for a FUNCTION_BLOCK on line 22'
    assert msgs[41] == 'Name slow of this task is also used for a global variable on line 36'
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


# {{{ Naming conventions: annotated samples with their configurations
def check_annotated(sample, rule):
    """Run [sample] with its configuration and assert the reported lines of
    [rule] are exactly those marked ``(* <rule> *)``; return the warnings."""
    f = f'st/{sample}.st'
    warns, rc = run_checker([f], args=['-c', f'st/{sample}.config.json'])
    assert rc == 0, warns
    with DumpManager(f'{f}.dump.json'):
        pass
    ws = filter_warns(warns, rule)
    marker = f'(* {rule} *)'
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if marker in line]
    assert sorted(w.linenr for w in ws) == expected
    return {w.linenr: w.msg for w in ws}


def test_n2_scopes_and_kinds():
    """Scope and type prefixes combine; prefixes end at a word boundary;
    arrays, structs, enums, FB instances and aliases have prefixes too."""
    msgs = check_annotated('plcopen-n2-scopes', 'PLCOPEN-N2')
    assert msgs[9] == 'Variable xEnable (input, of type BOOL) should start with prefix "px"'
    assert msgs[17] == 'Variable xylophone (of type BOOL) should start with prefix "x"'
    assert msgs[53] == 'Variable level (global, of type INT) should start with prefix "gi"'


def test_n4_members_and_values():
    """Struct members take the variable style and enum values the constant
    style; all capitals isn't UpperCamelCase."""
    msgs = check_annotated('plcopen-n4-members', 'PLCOPEN-N4')
    assert msgs[9] == 'Identifier MIN_SCALE does not match required case lowerCamelCase'
    assert msgs[20] == 'Identifier STARTMOTOR2 does not match required case UpperCamelCase'


def test_n6_locals_and_loop_counters():
    msgs = check_annotated('plcopen-n6-locals', 'PLCOPEN-N6')
    assert msgs[8] == 'Identifier j is too short (1 char, minimum 3)'
    assert msgs[36] == 'Identifier fast is too short (4 chars, minimum 8)'


def test_n10_all_kinds():
    msgs = check_annotated('plcopen-n10-kinds', 'PLCOPEN-N10')
    assert msgs[4] == 'NAMED ELevel should start with prefix "EN_"'
    assert msgs[7] == 'UDT Speed2 should start with prefix "T_"'
    assert msgs[32] == 'PROGRAM Main should start with prefix "PRG"'


def test_n4_case_after_n10_prefix(tmp_path):
    """PRG_Main is UpperCamelCase after its prefix PRG_."""
    f = tmp_path / 'p.st'
    f.write_text('PROGRAM PRG_Main\nVAR x : INT; END_VAR\nx := 1;\nEND_PROGRAM\n'
                 'PROGRAM PRG_main2\nVAR x : INT; END_VAR\nx := 1;\nEND_PROGRAM\n'
                 'PROGRAM Other\nVAR x : INT; END_VAR\nx := 1;\nEND_PROGRAM\n')
    cfg = tmp_path / 'c.json'
    cfg.write_text(json.dumps({'naming_conventions': {
        'case': {'pou': 'UpperCamelCase'}, 'udt_prefixes': {'PROGRAM': 'PRG_'}}}))
    warns, rc = run_checker([str(f)], args=['-c', str(cfg)])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    assert [w.linenr for w in filter_warns(warns, 'PLCOPEN-N4')] == [5]
    # PRG_main2 has the prefix; only its case is wrong.
    assert [w.linenr for w in filter_warns(warns, 'PLCOPEN-N10')] == [9]


def test_example_config_loads():
    rc, out = run_checker_full_out(['st/plcopen-n4.st'], binary_default,
                                   '-c', '../iec_checker.example.json')
    assert rc == 0, out


def test_naming_config_errors(tmp_path):
    """Unknown case styles and prefix keys are errors, not silently
    ignored."""
    f = tmp_path / 'p.st'
    f.write_text('PROGRAM p\nVAR x : INT; END_VAR\nx := 1;\nEND_PROGRAM\n')
    for naming, expected in [
            ({'case': {'variable': 'camelCase'}},
             'naming_conventions.case.variable: unknown style "camelCase"'),
            ({'udt_prefixes': {'STRUCTURE': 'ST'}},
             'naming_conventions.udt_prefixes: unknown key "STRUCTURE"'),
            ({'scope_prefixes': {'global': 'g', 'module': 'm'}},
             'naming_conventions.scope_prefixes: unknown key "MODULE"')]:
        cfg = tmp_path / 'c.json'
        cfg.write_text(json.dumps({'naming_conventions': naming}))
        rc, out = run_checker_full_out([str(f)], binary_default, '-c', str(cfg))
        assert rc != 0
        assert expected in out
# }}}


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


def test_cp9_names_the_pou(tmp_path):
    f = 'st/plcopen-cp9.st'
    cfg = tmp_path / 'iec_checker.json'
    cfg.write_text(json.dumps({'thresholds': {'statements_count': 0}}))
    warns, rc = run_checker([f], args=['-c', str(cfg)])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    ws = filter_warns(warns, 'PLCOPEN-CP9')
    assert {w.msg.split(' is ')[0] for w in ws} == {'CHARCURVE', 'CHARCURVE2'}
    assert all(w.linenr > 0 for w in ws)
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


def test_cp2():
    """Dead code after jumps, including loops, and under constant
    conditions; IF FALSE is the rule's bypass idiom."""
    f = 'st/plcopen-cp2.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* CP2 *)' in line]
    # The sample has no entry point for its functions; see the test below.
    ws = [w for w in filter_warns(warns, 'PLCOPEN-CP2') if 'unreachable code' in w.msg]
    assert sorted(w.linenr for w in ws) == expected
    assert ws[0].msg.endswith('unreachable code (it follows RETURN, EXIT or CONTINUE)')


def test_cp2_unreferenced():
    """POUs nothing reachable from the programs the configuration runs uses:
    calls in expressions and arguments, FB types of arrays, struct members
    and globals count as uses; self-recursion and uses by dead POUs don't."""
    f = 'st/plcopen-cp2-unreferenced.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* CP2 *)' in line]
    ws = filter_warns(warns, 'PLCOPEN-CP2')
    assert sorted(w.linenr for w in ws) == expected
    msgs = {w.linenr: w.msg for w in ws}
    assert msgs[21] == ('All code shall be used in the application: Function Unused '
                        'is never used (nothing in the application uses it)')
    assert msgs[70].endswith('Program Spare is never used (no configuration runs it)')


def test_cp2_library_not_reported():
    """Without a program, the POUs are a library used elsewhere."""
    f = 'st/plcopen-cp2-library.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    assert not [w for w in filter_warns(warns, 'PLCOPEN-CP2') if 'never used' in w.msg]


def test_e_rules():
    """E1: dynamic allocation; E2: pointer arithmetic; E3: ordering
    comparisons of pointers, as in the rule's example."""
    f = 'st/plcopen-e.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    with open(f) as fp:
        lines = fp.readlines()
    for rule in ['PLCOPEN-E1', 'PLCOPEN-E2', 'PLCOPEN-E3']:
        expected = [i for i, line in enumerate(lines, 1) if f'(* {rule} *)' in line]
        assert sorted(w.linenr for w in filter_warns(warns, rule)) == expected, rule
    msgs = {(w.id, w.linenr): w.msg for w in warns}
    assert msgs[('PLCOPEN-E1', 25)] == ('Dynamic memory allocation shall not be used: '
                                        '__NEW allocates memory at run time')
    assert msgs[('PLCOPEN-E3', 45)].endswith('not >=')


def test_cp7():
    """Error outputs of instances and functions that are never read; reading
    one of them, directly or through a connected variable, is enough."""
    f = 'st/plcopen-cp7.st'
    warns, rc = run_checker([f])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    ws = filter_warns(warns, 'PLCOPEN-CP7')
    with open(f) as fp:
        expected = [i for i, line in enumerate(fp, 1) if '(* PLCOPEN-CP7 *)' in line]
    assert sorted(w.linenr for w in ws) == expected
    msgs = {w.linenr: w.msg for w in ws}
    # The rule's example
    assert msgs[43] == ('Error information shall be tested: xError, iError of '
                        'instance (Scaler) are never read')
    assert msgs[64].endswith('Error, ErrorID of move (MC_MOVEABSOLUTE) are never read')


def test_cp7_error_names(tmp_path):
    names = {'Error': True, 'ErrorID': True, 'xError': True, 'iErrorCode': True,
             'ERR': True, 'nErrId': True, 'Error_ID': True,
             'Status': False, 'Errand': False, 'terror': False, 'Done': False}
    decls = '\n'.join(f'    {n} : INT;' for n in names)
    f = tmp_path / 'p.st'
    f.write_text(f'FUNCTION_BLOCK Fb\n  VAR_OUTPUT\n{decls}\n  END_VAR\n  Done := 1;\nEND_FUNCTION_BLOCK\n'
                 + ''.join(f'FUNCTION_BLOCK Only{i}\n  VAR_OUTPUT {n} : INT; END_VAR\n  {n} := 1;\n'
                           f'END_FUNCTION_BLOCK\n' for i, n in enumerate(names))
                 + 'PROGRAM p\n  VAR\n'
                 + ''.join(f'    i{i} : Only{i};\n' for i, _ in enumerate(names))
                 + '  END_VAR\n'
                 + ''.join(f'  i{i}();\n' for i, _ in enumerate(names))
                 + 'END_PROGRAM\n')
    warns, rc = run_checker([str(f)])
    assert rc == 0
    with DumpManager(f'{f}.dump.json'):
        pass
    flagged = {w.msg.split(': ')[1].split(' of ')[0] for w in filter_warns(warns, 'PLCOPEN-CP7')}
    assert flagged == {n for n, is_error in names.items() if is_error}
