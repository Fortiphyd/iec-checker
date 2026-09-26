"""Tests for warning severities and output formats."""
import sys
import os
import json
import subprocess
from collections import Counter

sys.path.append(os.path.join(os.path.dirname(
    os.path.abspath(__file__)), "../src"))
from python.core import run_checker, filter_warns, binary_default  # noqa
from python.dump import DumpManager  # noqa

CP12 = 'st/plcopen-cp12.st'
CP20 = 'st/plcopen-cp20.st'


def run(paths, args=[]):
    warns, rc = run_checker(paths, args=args)
    for f in paths:
        with DumpManager(f'{f}.dump.json'):
            pass
    return warns, rc


def run_sarif(paths, args=[]):
    p = subprocess.run([binary_default, '-o', 'sarif', *args, *paths],
                       capture_output=True, text=True)
    return json.loads(p.stdout), p.returncode


# {{{ Severities
def test_warnings_have_severity():
    warns, rc = run([CP12])
    assert rc == 0
    severities = {w.id: w.severity for w in warns}
    assert severities['PLCOPEN-CP12'] == 'high'
    assert severities['PLCOPEN-CP3'] == 'medium'
    assert severities['UnusedVariable'] == 'low'


def test_min_severity_option():
    warns, _ = run([CP12], args=['--min-severity', 'medium'])
    assert warns
    assert {w.severity for w in warns} <= {'medium', 'high'}
    warns, _ = run([CP12], args=['--min-severity', 'high'])
    assert {w.id for w in warns} == {'PLCOPEN-CP12'}


def test_min_severity_from_config(tmp_path):
    cfg = tmp_path / 'iec_checker.json'
    cfg.write_text(json.dumps({'output': {'min_severity': 'high'}}))
    warns, _ = run([CP12], args=['-c', str(cfg)])
    assert {w.id for w in warns} == {'PLCOPEN-CP12'}


def test_errors_ignore_min_severity(tmp_path):
    f = tmp_path / 'bad.st'
    f.write_text('PROGRAM p\nVAR y : %IW2; END_VAR\nEND_PROGRAM\n')
    warns, _ = run_checker([str(f)], args=['--min-severity', 'high'])
    [w] = warns
    assert (w.id, w.severity) == ('ParserError', 'high')


def test_unknown_min_severity():
    p = subprocess.run([binary_default, '--min-severity', 'urgent', CP12],
                       capture_output=True, text=True)
    assert p.returncode != 0
    assert "Unknown severity 'urgent'" in p.stderr


def test_list_checks_shows_severity():
    p = subprocess.run([binary_default, '--list-checks'], capture_output=True, text=True)
    lines = {line.split()[0]: line.split()[1] for line in p.stdout.splitlines()
             if line.startswith(('PLCOPEN-', 'TaintedVariable'))}
    assert lines['PLCOPEN-CP12'] == 'high'
    assert lines['PLCOPEN-N4'] == 'low'
    assert lines['TaintedVariable'] == 'high'
# }}}


def test_json_single_document_for_several_files():
    p = subprocess.run([binary_default, '-o', 'json', CP12, CP20], capture_output=True, text=True)
    warns = json.loads(p.stdout)
    assert {w['file'] for w in warns} == {CP12, CP20}


# {{{ SARIF
def test_sarif_document():
    doc, rc = run_sarif([CP12])
    assert rc == 0
    assert doc['version'] == '2.1.0'
    [sarif_run] = doc['runs']
    assert sarif_run['tool']['driver']['name'] == 'iec-checker'
    warns, _ = run([CP12])
    assert len(sarif_run['results']) == len(warns)


def test_sarif_single_document_for_several_files():
    doc, _ = run_sarif([CP12, CP20])
    [sarif_run] = doc['runs']
    uris = {r['locations'][0]['physicalLocation']['artifactLocation']['uri']
            for r in sarif_run['results']}
    assert uris == {CP12, CP20}


def test_sarif_levels_and_locations():
    doc, _ = run_sarif([CP20])
    results = doc['runs'][0]['results']
    levels = {r['ruleId']: r['level'] for r in results}
    assert levels['PLCOPEN-CP20'] == 'error'
    assert levels['PLCOPEN-CP3'] == 'warning'
    assert levels['UnusedVariable'] == 'note'
    cp20 = [r for r in results if r['ruleId'] == 'PLCOPEN-CP20']
    regions = [r['locations'][0]['physicalLocation']['region'] for r in cp20]
    assert Counter(r['startLine'] for r in regions) == Counter({29: 1, 37: 1})


def test_sarif_rules_describe_results():
    doc, _ = run_sarif([CP12, CP20])
    driver = doc['runs'][0]['tool']['driver']
    rules = {r['id']: r for r in driver['rules']}
    for r in doc['runs'][0]['results']:
        assert r['ruleId'] in rules
    assert rules['PLCOPEN-CP12']['defaultConfiguration']['level'] == 'error'
    assert rules['PLCOPEN-CP12']['helpUri'].endswith('PLCOPEN-CP12')


def test_sarif_min_severity():
    doc, _ = run_sarif([CP12], args=['--min-severity', 'high'])
    assert {r['ruleId'] for r in doc['runs'][0]['results']} == {'PLCOPEN-CP12'}
# }}}


# {{{ Columns
def region_text(f, region):
    with open(f) as fp:
        line = fp.read().splitlines()[region['startLine'] - 1]
    return line[region['startColumn'] - 1:region['endColumn'] - 1]


def test_sarif_region_covers_token():
    doc, _ = run_sarif([CP12])
    regions = {
        r['locations'][0]['physicalLocation']['region']['startLine']:
        r['locations'][0]['physicalLocation']['region']
        for r in doc['runs'][0]['results'] if r['ruleId'] == 'PLCOPEN-CP12'}
    assert region_text(CP12, regions[48]) == 'g_valve'
    assert region_text(CP12, regions[32]) == '%QW1'


def test_json_start_column():
    warns, _ = run([CP12])
    [w] = [w for w in warns if w.id == 'PLCOPEN-CP12' and w.linenr == 48]
    # g_valve on "  g_valve := alarm;": columns of its first and last characters.
    assert (w.start_column, w.column) == (3, 9)


def test_parser_error_start_column(tmp_path):
    f = tmp_path / 'bad.st'
    f.write_text('PROGRAM p\nVAR y : %IW2; END_VAR\nEND_PROGRAM\n')
    p = subprocess.run([binary_default, '-o', 'sarif', str(f)], capture_output=True, text=True)
    [r] = json.loads(p.stdout)['runs'][0]['results']
    assert region_text(str(f), r['locations'][0]['physicalLocation']['region']) == '%IW2'
# }}}


# {{{ PLCopen importance
def test_warnings_have_plcopen_importance():
    warns, _ = run([CP12, CP20])
    importance = {w.id: w.plcopen_importance for w in warns}
    assert importance['PLCOPEN-CP12'] == 'high'
    assert importance['PLCOPEN-CP20'] == 'medium'
    assert importance['PLCOPEN-L17'] == 'low'
    assert importance['UnusedVariable'] == ''


def test_min_plcopen_importance():
    warns, _ = run([CP12, CP20], args=['--min-plcopen-importance', 'high'])
    assert warns
    assert all(w.id.startswith('PLCOPEN-') and w.plcopen_importance == 'high' for w in warns)
    warns, _ = run([CP20], args=['--min-plcopen-importance', 'medium'])
    assert 'PLCOPEN-CP20' in {w.id for w in warns}
    assert {w.plcopen_importance for w in warns} <= {'medium', 'high'}


def test_min_plcopen_importance_from_config(tmp_path):
    cfg = tmp_path / 'iec_checker.json'
    cfg.write_text(json.dumps({'output': {'min_plcopen_importance': 'high'}}))
    warns, _ = run([CP20], args=['-c', str(cfg)])
    assert 'PLCOPEN-CP20' not in {w.id for w in warns}


def test_unknown_min_plcopen_importance():
    p = subprocess.run([binary_default, '--min-plcopen-importance', 'huge', CP12],
                       capture_output=True, text=True)
    assert p.returncode != 0
    assert "Unknown PLCopen importance 'huge'" in p.stderr


def test_list_checks_shows_plcopen_importance():
    p = subprocess.run([binary_default, '--list-checks'], capture_output=True, text=True)
    rows = {line.split()[0]: line.split()[2] for line in p.stdout.splitlines()
            if line.startswith(('PLCOPEN-', 'TaintedVariable'))}
    assert rows['PLCOPEN-CP26'] == 'low'
    assert rows['PLCOPEN-N4'] == 'high'
    assert rows['TaintedVariable'] == '-'


def test_sarif_plcopen_importance():
    doc, _ = run_sarif([CP20])
    sarif_run = doc['runs'][0]
    rules = {r['id']: r for r in sarif_run['tool']['driver']['rules']}
    assert rules['PLCOPEN-CP20']['properties'] == {'plcopen-importance': 'medium'}
    assert 'properties' not in rules['TaintedVariable']
    cp20 = [r for r in sarif_run['results'] if r['ruleId'] == 'PLCOPEN-CP20']
    assert all(r['properties'] == {'plcopen-importance': 'medium'} for r in cp20)
# }}}
