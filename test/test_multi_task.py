"""Tests for the MultiTaskWrite analysis pass."""
import sys
import os

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, filter_warns  # noqa
from python.dump import DumpManager  # noqa

MARKER = '(* RACE *)'


def check(tmp_path, source):
    """Assert the reported lines are exactly those marked in [source], and
    return the warnings."""
    f = tmp_path / 'input.st'
    f.write_text(source)
    warns, rc = run_checker([str(f)])
    assert rc == 0, warns
    with DumpManager(f'{f}.dump.json'):
        pass
    ws = filter_warns(warns, 'MultiTaskWrite')
    expected = [i for i, line in enumerate(source.splitlines(), 1) if MARKER in line]
    assert sorted(w.linenr for w in ws) == expected
    return ws


def config(programs, globals_='', resource_globals=''):
    return f"""CONFIGURATION cfg
  VAR_GLOBAL
    g : INT;
    g_out AT %QW0 : INT;
{globals_}
  END_VAR
  RESOURCE res ON PLC
    VAR_GLOBAL
      r_dummy : INT;
{resource_globals}
    END_VAR
    TASK fast(INTERVAL := T#10MS, PRIORITY := 1);
    TASK slow(INTERVAL := T#100MS, PRIORITY := 5);
{programs}
  END_RESOURCE
END_CONFIGURATION
"""


def program(name, body, decls=''):
    return f"""
PROGRAM {name}
  VAR_EXTERNAL
    g : INT;
    g_out : INT;
  END_VAR
  VAR
    x : INT;
{decls}
  END_VAR
{body}
END_PROGRAM
"""


TWO_TASKS = """    PROGRAM p1 WITH fast : prog_a;
    PROGRAM p2 WITH slow : prog_b;"""


def test_global_written_from_two_tasks(tmp_path):
    [w1, w2] = check(tmp_path, config(TWO_TASKS)
                     + program('prog_a', f'  g := 1; {MARKER}')
                     + program('prog_b', f'  g := 2; {MARKER}'))
    assert w1.msg == ('Global variable G is written by programs in different tasks: '
                      'P1 (task RES.FAST), P2 (task RES.SLOW)')


def test_same_task_is_not_reported(tmp_path):
    check(tmp_path, config("""    PROGRAM p1 WITH fast : prog_a;
    PROGRAM p2 WITH fast : prog_b;""")
          + program('prog_a', '  g := 1;')
          + program('prog_b', '  g := 2;'))


def test_one_program_type_in_two_tasks(tmp_path):
    [w] = check(tmp_path, config("""    PROGRAM p1 WITH fast : prog_a;
    PROGRAM p2 WITH slow : prog_a;""")
                + program('prog_a', f'  g := 1; {MARKER}'))
    assert 'P1 (task RES.FAST), P2 (task RES.SLOW)' in w.msg


def test_program_without_task(tmp_path):
    check(tmp_path, config("""    PROGRAM p1 WITH fast : prog_a;
    PROGRAM p2 : prog_b;""")
          + program('prog_a', f'  g := 1; {MARKER}')
          + program('prog_b', f'  g := 2; {MARKER}'))


def test_read_only_global(tmp_path):
    check(tmp_path, config(TWO_TASKS)
          + program('prog_a', '  g := 1;')
          + program('prog_b', '  x := g;'))


def test_local_variable_with_global_name(tmp_path):
    check(tmp_path, config(TWO_TASKS)
          + program('prog_a', '  g := 1;')
          + """
PROGRAM prog_b
  VAR
    g : INT;
  END_VAR
  g := 2;
END_PROGRAM
""")


def test_physical_output(tmp_path):
    [w, _] = check(tmp_path, config(TWO_TASKS)
                   + program('prog_a', f'  %QW1 := 1; {MARKER}')
                   + program('prog_b', f'  %QW1 := 2; {MARKER}'))
    assert w.msg.startswith('Physical output %QW1 ')


def test_located_global_and_direct_address(tmp_path):
    check(tmp_path, config(TWO_TASKS)
          + program('prog_a', f'  g_out := 1; {MARKER}')
          + program('prog_b', f'  %QW0 := 2; {MARKER}'))


def test_write_in_called_function_block(tmp_path):
    check(tmp_path, config(TWO_TASKS) + f"""
FUNCTION_BLOCK reset
  VAR_EXTERNAL
    g : INT;
  END_VAR
  g := 0; {MARKER}
END_FUNCTION_BLOCK
""" + program('prog_a', f'  g := g + 1; {MARKER}')
          + program('prog_b', '  r();', decls='    r : reset;'))


def test_output_parameter(tmp_path):
    check(tmp_path, config(TWO_TASKS) + """
FUNCTION_BLOCK source
  VAR_OUTPUT
    out : INT;
  END_VAR
  out := 1;
END_FUNCTION_BLOCK
""" + program('prog_a', f'  g := 1; {MARKER}')
          + program('prog_b', f'  s(out => g); {MARKER}', decls='    s : source;'))


def test_function_result_is_not_a_global(tmp_path):
    check(tmp_path, config(TWO_TASKS) + """
FUNCTION f : INT
  f := 1;
END_FUNCTION
""" + program('prog_a', '  x := f();')
          + program('prog_b', '  x := f();'))


def test_globals_of_different_resources(tmp_path):
    check(tmp_path, """CONFIGURATION cfg
  RESOURCE res1 ON PLC
    VAR_GLOBAL
      g : INT;
    END_VAR
    TASK fast(INTERVAL := T#10MS, PRIORITY := 1);
    PROGRAM p1 WITH fast : prog_a;
  END_RESOURCE
  RESOURCE res2 ON PLC
    VAR_GLOBAL
      g : INT;
    END_VAR
    TASK fast(INTERVAL := T#10MS, PRIORITY := 1);
    PROGRAM p2 WITH fast : prog_b;
  END_RESOURCE
END_CONFIGURATION
""" + program('prog_a', '  g := 1;') + program('prog_b', '  g := 2;'))


def test_different_configurations(tmp_path):
    other = config('    PROGRAM p2 WITH slow : prog_b;').replace('CONFIGURATION cfg', 'CONFIGURATION cfg2')
    check(tmp_path, config('    PROGRAM p1 WITH fast : prog_a;') + other
          + program('prog_a', '  g := 1;')
          + program('prog_b', '  g := 2;'))


def test_no_configuration(tmp_path):
    check(tmp_path, program('prog_a', '  g := 1;') + program('prog_b', '  g := 2;'))
