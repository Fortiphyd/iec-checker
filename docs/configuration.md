# Configuration

The checker reads `iec_checker.json` from the current directory or the
nearest of its parents, or the file given with `-c`. `--generate-config`
writes one with the defaults, and `--dump-config` prints the configuration in
effect. Command-line options take precedence. Unknown values of the options
below that take a fixed set of values are errors.

A complete example is [`iec_checker.example.json`](../iec_checker.example.json).

```json
{
  "detectors": { "disabled": [], "enabled": [] },
  "thresholds": {
    "mccabe_complexity": 15,
    "mccabe_variant": "standard",
    "statements_count": 25,
    "max_string_length": 4096,
    "duplicate_code_size": 25
  },
  "output": { "format": "plain", "color": true, "min_severity": "low",
              "min_plcopen_importance": "" },
  "input": { "format": "st", "merge": false, "exclude_paths": [] },
  "analysis": { "dump": false, "verbose": false },
  "naming_conventions": { … }
}
```

## `detectors`

- `disabled`: checks not to run.
- `enabled`: if not empty, only these checks run.

Checks are named by their IDs, as in the [detectors reference](detectors.md).
`MultiTaskWrite` and `UnusedVariable` can also be named `PLCOPEN-CP10` and
`PLCOPEN-CP24`.

## `thresholds`

- `mccabe_complexity`, `statements_count`: limits of [CP9](detectors.md#plcopen-cp9).
- `mccabe_variant`: `"standard"` or `"plcopen"`, see [CP9](detectors.md#plcopen-cp9).
- `max_string_length`: the size of `STRING` and `WSTRING` without a length.
- `duplicate_code_size`: the smallest [duplicate](detectors.md#duplicatecode)
  to report, in syntax tree nodes.

## `output`

- `format`: `plain`, `json` or `sarif`.
- `color`: colors in plain output.
- `min_severity`, `min_plcopen_importance`: see [Output](output.md#filtering).

## `input`

- `format`: `st` (Structured Text), `xml` (PLCopen XML) or `selxml`
  (Schweitzer Engineering Laboratories XML).
- `merge`: analyse all input files as one program.
- `exclude_paths`: glob patterns of input files to skip.

## `analysis`

- `dump`: write dump files, as `-d`.
- `verbose`: print progress.

## `naming_conventions`

The naming rules N2, N4, N6 and N10 only run when their options are set; the
guidelines leave the conventions to each project.

| Option | Rule | |
|---|---|---|
| `type_prefixes` | [N2](detectors.md#plcopen-n2) | Prefixes of variables by type, e.g. `{"BOOL": "x", "INT": "i", "ARRAY": "a"}` |
| `scope_prefixes` | [N2](detectors.md#plcopen-n2) | Prefixes by scope, before the type prefix, e.g. `{"GLOBAL": "g"}` |
| `case` | [N4](detectors.md#plcopen-n4) | Case styles for `variable`, `constant`, `pou`, `type`, `struct_member` and `enum_value` |
| `min_length`, `min_length_local`, `max_length` | [N6](detectors.md#plcopen-n6) | Name lengths; 0 means no limit |
| `udt_prefixes` | [N10](detectors.md#plcopen-n10) | Prefixes of types and POUs by kind, e.g. `{"STRUCT": "ST_", "FUNCTION_BLOCK": "FB_"}` |
| `allow_non_ascii` | [N8](detectors.md#plcopen-n8) | Allow a national character set in names |
