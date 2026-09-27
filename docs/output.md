# Output

## Formats

`-o plain` (the default) prints one block per warning:

```
PLCOPEN-CP8 [medium]: Floating point comparison shall not be equality or inequality
  --> program.st:3:4
  See: https://github.com/Fortiphyd/iec-checker/blob/master/docs/detectors.md#plcopen-cp8
```

A built-in check that implements a PLCopen rule names it after its own ID:
`UnusedVariable (PLCOPEN-CP24) [low]: …`. `--no-color` turns colors off.

`-o json` prints one JSON array with the warnings of all input files:

| Field | |
|---|---|
| `id` | The check, e.g. `PLCOPEN-CP8` or `TaintedVariable` |
| `msg` | The message |
| `file` | The input file |
| `linenr` | Line, from 1; 0 if the warning has no position |
| `start_column`, `column` | First and last column of the reported token, from 1, in characters |
| `severity` | `low`, `medium` or `high` |
| `plcopen_importance` | Importance of the PLCopen rule, or `null` |
| `plcopen_rule` | The PLCopen rule, e.g. `PLCOPEN-CP24`, or `null` |
| `type` | `["Inspection"]` for warnings, `["InternalError"]` for errors of the checker |
| `context` | For parser errors, the source line with the error marked |

`-o sarif` prints a [SARIF 2.1.0](https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html)
log for code scanning tools such as GitHub code scanning. Severities map to
levels (`high` → `error`, `medium` → `warning`, `low` → `note`), and results
and rules carry `plcopen-importance` and `plcopen-rule` properties. Each rule
has a `helpUri` into the [detectors reference](detectors.md).

## Filtering

- `--min-severity low|medium|high` hides warnings below a severity.
- `--min-plcopen-importance low|medium|high` only reports PLCopen rules of at
  least that importance, including the built-in checks that implement one.

Both can be set in the [configuration](configuration.md) (`output`). Parser
errors are always reported.

## Exit codes

| Code | |
|---|---|
| 0 | The files were analysed, whether or not there are warnings |
| 1 | A file failed to parse, or the options or configuration are invalid |
| 127 | An input file doesn't exist |

To fail a CI job on warnings, check the output, e.g.
`test "$(iec_checker -o json src/*.st | jq length)" -eq 0`.

## Dumps

`-d` writes `<file>.dump.json` next to each input: the parsed program, for
tools such as the Python helpers in `src/python`.
