import sys
import os
import json

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import check_program, run_checker, run_checker_full_out, binary_default, filter_warns  # noqa
from python.dump import DumpManager  # noqa


def test_unused_local_variable():
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        PROGRAM p
        VAR
          a : INT;
          b : INT;
          c : INT;
        END_VAR
        b := 1 + c;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    assert len(filter_warns(warns, 'UnusedVariable')) == 1
    with DumpManager(fdump) as dm:
        scheme = dm.scheme
        assert scheme


def test_struct_member_access_counts_as_use():
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        TYPE MyStruct : STRUCT A : INT; B : INT; END_STRUCT; END_TYPE
        PROGRAM p
        VAR ms : MyStruct; END_VAR
        ms.B := 1;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    unused = filter_warns(warns, 'UnusedVariable')
    assert len(unused) == 0, (
        f'ms is used via member access, must not be reported unused; '
        f'got: {[(w.linenr, w.msg) for w in unused]}')
    with DumpManager(fdump):
        pass


def test_struct_nested_member_access_counts_as_use():
    """Accessing `a.b.c` must count as a use of `a` (deep chain)."""
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        TYPE
          Inner : STRUCT x : INT; END_STRUCT;
          Outer : STRUCT inn : Inner; END_STRUCT;
        END_TYPE
        PROGRAM p
        VAR o : Outer; END_VAR
        o.inn.x := 42;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    unused = filter_warns(warns, 'UnusedVariable')
    assert len(unused) == 0, (
        f'o used via nested member access, must not be reported unused; '
        f'got: {[(w.linenr, w.msg) for w in unused]}')
    with DumpManager(fdump):
        pass


def test_truly_unused_struct_still_warned():
    """A struct never referenced at all must still be flagged."""
    fdump = f'stdin.dump.json'
    warns, rc = check_program(
        """
        TYPE MyStruct : STRUCT A : INT; END_STRUCT; END_TYPE
        PROGRAM p
        VAR ms : MyStruct; other : INT; END_VAR
        other := 1;
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    unused = filter_warns(warns, 'UnusedVariable')
    assert len(unused) == 1, (
        f'ms is never accessed, must be flagged; got {len(unused)} warnings')
    assert 'MS' in unused[0].msg
    with DumpManager(fdump):
        pass


def test_variable_used_only_as_array_index():
    fdump = 'stdin.dump.json'
    warns, rc = check_program(
        'PROGRAM p VAR w : ARRAY [0..9] OF INT; k : INT := 1; END_VAR '
        'w[k + 1] := 5; END_PROGRAM')
    assert rc == 0
    names = [w.msg for w in filter_warns(warns, 'UnusedVariable')]
    assert not any('K' in m.split(': ')[-1] for m in names)
    with DumpManager(fdump):
        pass


# {{{ Global variables, and the PLCopen rule CP24
GLOBALS = """CONFIGURATION cfg
  VAR_GLOBAL
    g_used : INT;
    g_unused : INT;
    g_connected : INT;
    g_trigger : BOOL;
    g_valve : Valve;
  END_VAR
  RESOURCE res ON PLC
    VAR_GLOBAL r_unused : INT; END_VAR
    TASK t(SINGLE := g_trigger, PRIORITY := 1);
    PROGRAM inst WITH t : Main(y => g_connected);
  END_RESOURCE
END_CONFIGURATION

FUNCTION_BLOCK Valve
  VAR_OUTPUT open : BOOL; END_VAR
  open := TRUE;
END_FUNCTION_BLOCK

PROGRAM Main
  VAR_EXTERNAL g_used : INT; g_valve : Valve; END_VAR
  VAR_OUTPUT y : INT; END_VAR
  y := g_used;
  g_valve();
END_PROGRAM
"""


def run_globals(tmp_path, source=GLOBALS, args=[]):
    f = tmp_path / 'g.st'
    f.write_text(source)
    warns, rc = run_checker([str(f)], args=args)
    assert rc == 0, warns
    with DumpManager(f'{f}.dump.json'):
        pass
    return filter_warns(warns, 'UnusedVariable')


def test_unused_globals(tmp_path):
    """Globals used by POUs, calls, connections or tasks are used."""
    ws = run_globals(tmp_path)
    assert sorted(w.msg for w in ws) == ['Found unused global variable: g_unused',
                                         'Found unused global variable: r_unused']
    assert all(w.plcopen_rule == 'PLCOPEN-CP24' for w in ws)


def test_unused_globals_need_every_program(tmp_path):
    """A program declared elsewhere may use the globals."""
    source = GLOBALS.replace('PROGRAM inst WITH t : Main(y => g_connected);',
                             'PROGRAM inst WITH t : Main(y => g_connected);\n'
                             '    PROGRAM other WITH t : Elsewhere;')
    assert run_globals(tmp_path, source) == []


def test_enabled_by_plcopen_id(tmp_path):
    cfg = tmp_path / 'c.json'
    cfg.write_text(json.dumps({'detectors': {'enabled': ['PLCOPEN-CP24']}}))
    assert len(run_globals(tmp_path, args=['-c', str(cfg)])) == 2
    cfg.write_text(json.dumps({'detectors': {'disabled': ['PLCOPEN-CP24']}}))
    assert run_globals(tmp_path, args=['-c', str(cfg)]) == []


def test_plain_output_names_the_rule(tmp_path):
    f = tmp_path / 'g.st'
    f.write_text(GLOBALS)
    rc, out = run_checker_full_out([str(f)], binary_default, '--no-color')
    assert 'UnusedVariable (PLCOPEN-CP24) [low]: Found unused global variable: g_unused' in out
# }}}


def test_called_instance_is_used():
    """A function block instance that is only called is used."""
    warns, rc = check_program(
        """
        PROGRAM p
        VAR t : TON; u : TON; END_VAR
        t(IN := TRUE, PT := T#1S);
        END_PROGRAM
        """.replace('\n', ''))
    assert rc == 0
    assert [w.msg for w in filter_warns(warns, 'UnusedVariable')] == [
        'Found unused local variable: U']
