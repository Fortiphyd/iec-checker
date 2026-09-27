# IEC Checker

A static analyzer for [IEC 61131-3](https://en.wikipedia.org/wiki/IEC_61131-3)
PLC programs. It finds bugs and security problems, such as untrusted inputs
driving outputs, races between tasks and unchecked errors, and checks the
[PLCopen Coding Guidelines](https://plcopen.org/downloads).

This is Fortiphyd Logic's fork of [iec-checker](https://github.com/iec-checker/iec-checker)
by Georgiy Komarov. It has diverged from it: see [what's different](#differences-from-upstream).

```st
PROGRAM Pump
  VAR
    level AT %IW0 : INT;
    setpoint AT %MW100 : INT := 0;
    speed AT %QW0 : INT;
    axis : MC_MoveAbsolute;
  END_VAR
  speed := setpoint * 2;
  axis(Execute := TRUE);
  speed := LIMIT(0, level, 1000);
END_PROGRAM
```

```
$ iec_checker pump.st
TaintedVariable [high]: Output SPEED is driven by untrusted SETPOINT without a bounds check
  --> pump.st:8:7
  See: https://github.com/Fortiphyd/iec-checker/blob/master/docs/detectors.md#taintedvariable

PLCOPEN-CP7 [medium]: Error information shall be tested: Error, ErrorID of axis (MC_MOVEABSOLUTE) are never read
  --> pump.st:9:6
  See: https://github.com/Fortiphyd/iec-checker/blob/master/docs/detectors.md#plcopen-cp7

PLCOPEN-CP12 [high]: Physical output SPEED (%QW0) is written more than once per PLC cycle: it is already written on line 8
  --> pump.st:10:7
  See: https://github.com/Fortiphyd/iec-checker/blob/master/docs/detectors.md#plcopen-cp12
```

## What it checks

- **Untrusted data reaching outputs.** Values from physical inputs (`%I`) and
  network-writable memory (`%M`) that drive physical outputs (`%Q`) without
  a bounds check, followed through function blocks, functions and program
  connections. See [Taint analysis](docs/taint-analysis.md).
- **Tasks and cycles.** Outputs and globals written from several tasks,
  outputs written or function block instances called more than once per
  cycle, including by programs sharing a task.
- **Memory and pointers.** Dynamic allocation, pointer arithmetic and
  ordering comparisons of pointers.
- **34 of the 64 PLCopen rules**, including dead code, uninitialized
  variables, overlapping addresses, recursion, unchecked error outputs,
  implicit conversions, floating point and time comparisons, loop variables,
  complexity and naming conventions. Most of the rest apply to graphical
  languages or to comments and layout.
- **Duplicated code**, and copies that missed a rename, a likely copy-paste
  error.

See the [detectors reference](docs/detectors.md) for every check.

Input can be Structured Text, [PLCopen XML](https://plcopen.org/technical-activities/xml-exchange)
or SEL XML. The ST dialect follows IEC 61131-3 3rd edition, with some
extensions of CODESYS and TwinCAT (`POINTER TO`, `__NEW`).

## Installation

Download a binary for Linux or macOS from the
[releases](https://github.com/Fortiphyd/iec-checker/releases), or use the
Docker image:

```bash
docker run --rm -v "$PWD:/src" -w /src ghcr.io/fortiphyd/iec-checker:latest program.st
```

There is no native Windows binary for now, as a library we depend on doesn't
support Windows. Use the Linux binary under WSL, or the Docker image.

### Building from source

With [opam](https://opam.ocaml.org/doc/Install.html) and OCaml 5.1 or later:

```bash
opam install --deps-only .
make
```

The binary is `bin/iec_checker`.

## Usage

```bash
iec_checker src/*.st                  # check ST files
iec_checker -i xml project.xml        # check PLCopen XML
iec_checker -m src/*.st               # analyse several files as one program
iec_checker -o sarif src/*.st > iec.sarif
iec_checker --min-severity medium src/*.st
iec_checker --list-checks             # every check, its severity and PLCopen importance
```

Settings, such as checks to disable, thresholds and naming conventions, go in
an `iec_checker.json` file: see [Configuration](docs/configuration.md) and
[`iec_checker.example.json`](iec_checker.example.json). For JSON and SARIF
output, filtering and exit codes, see [Output](docs/output.md).

### GitHub code scanning

```yaml
- run: iec_checker -o sarif src/*.st > iec.sarif
- uses: github/codeql-action/upload-sarif@v3
  with:
    sarif_file: iec.sarif
```

## Differences from upstream

Among others:

- the taint analysis, and the checks of tasks and cycles: MultiTaskWrite
  (PLCopen CP10), CP12, CP20, and CP26 across programs;
- the PLCopen rules CP7, E1, E2 and E3, CP24 as part of UnusedVariable, and
  many fixes to the others after an audit against the guidelines;
- duplicate and inconsistent-copy detection;
- expression types, used by CP8, CP25, CP28 and mixed-type arithmetic;
- severities, PLCopen importance and SARIF output;
- parser support for references, pointers, program connections, task
  settings and non-ASCII names.

See [CHANGES.md](CHANGES.md).

## Development

```bash
make            # build
make test       # run the tests (pip install -r requirements-dev.txt first)
make spell      # codespell, as in CI
```

[Releasing](docs/releasing.md) describes how releases are made.

## License

[LGPL-3.0-or-later](LICENSE), like the upstream project. The PLCopen Coding
Guidelines are © PLCopen; rule names and texts quoted here are theirs.
