# Detectors reference

Every warning names the check that reported it and links here. Checks named
`PLCOPEN-…` implement rules of the
[PLCopen Coding Guidelines v1.0](https://plcopen.org/downloads) (a free
download). The others are our own; two of them implement PLCopen rules as
well (see the *PLCopen* column).

Each check has a **severity**: how likely a warning is to point at a real
bug or security problem. PLCopen rules also have the **importance** the
guidelines give them. `--min-severity` and `--min-plcopen-importance` filter
on these. See [Output](output.md).

Checks can be turned off with `detectors.disabled` in the
[configuration](configuration.md), or run alone with `detectors.enabled`. A
built-in check that implements a PLCopen rule can be named by either ID.

| Check | Severity | PLCopen importance | Rule |
|---|---|---|---|
| [PLCOPEN-CP1](#plcopen-cp1) | medium | high | Access to a member shall be by name |
| [PLCOPEN-CP2](#plcopen-cp2) | medium | high | All code shall be used in the application |
| [PLCOPEN-CP3](#plcopen-cp3) | medium | high | Variables shall be initialized before being used |
| [PLCOPEN-CP4](#plcopen-cp4) | high | high | Direct addressing should not overlap |
| [PLCOPEN-CP6](#plcopen-cp6) | low | high | Avoid external variables in functions, function blocks and classes |
| [PLCOPEN-CP7](#plcopen-cp7) | medium | high | Error information shall be tested |
| [PLCOPEN-CP8](#plcopen-cp8) | medium | high | Floating point comparison shall not be equality or inequality |
| [PLCOPEN-CP9](#plcopen-cp9) | low | high | Limit the complexity of POU code |
| [MultiTaskWrite](#multitaskwrite) (CP10) | high | high | Avoid multiple writes from multiple tasks |
| [PLCOPEN-CP12](#plcopen-cp12) | high | high | Physical outputs shall be written once per PLC cycle |
| [PLCOPEN-CP13](#plcopen-cp13) | high | high | POUs shall not call themselves directly or indirectly |
| [PLCOPEN-CP16](#plcopen-cp16) | medium | high | Tasks shall only call program POUs and not function blocks |
| [PLCOPEN-CP17](#plcopen-cp17) | medium | high | Usage of parameters shall match their declaration mode |
| [PLCOPEN-CP20](#plcopen-cp20) | high | medium | Function block instances should be called only once |
| [UnusedVariable](#unusedvariable) (CP24) | low | medium | Do not declare variables that are not used |
| [PLCOPEN-CP25](#plcopen-cp25) | medium | medium | Data type conversion should be explicit |
| [PLCOPEN-CP26](#plcopen-cp26) | high | low | A global variable may be written only by one PROGRAM |
| [PLCOPEN-CP28](#plcopen-cp28) | medium | high | Time and physical measures comparisons shall not be equality or inequality |
| [PLCOPEN-L10](#plcopen-l10) | low | medium | Usage of CONTINUE and EXIT instructions should be avoided |
| [PLCOPEN-L13](#plcopen-l13) | medium | medium | FOR loop variable should not be used outside the FOR loop |
| [PLCOPEN-L17](#plcopen-l17) | low | low | Each IF instruction should have an ELSE clause |
| [PLCOPEN-L22](#plcopen-l22) | medium | medium | Loop variables should not be modified inside a FOR loop |
| [PLCOPEN-N1](#plcopen-n1) | low | high | Avoid physical addresses |
| [PLCOPEN-N2](#plcopen-n2) | low | low | Define type prefixes for variables |
| [PLCOPEN-N3](#plcopen-n3) | low | high | Define the names to avoid |
| [PLCOPEN-N4](#plcopen-n4) | low | high | Define the use of case (capitals) |
| [PLCOPEN-N5](#plcopen-n5) | medium | high | Local names shall not shadow global names |
| [PLCOPEN-N6](#plcopen-n6) | low | medium | Define an acceptable name length |
| [PLCOPEN-N8](#plcopen-n8) | low | medium | Define the acceptable character set |
| [PLCOPEN-N9](#plcopen-n9) | low | medium | Different element types should not bear the same name |
| [PLCOPEN-N10](#plcopen-n10) | low | low | Define name prefixes for user defined types |
| [PLCOPEN-E1](#plcopen-e1) | medium | high | Dynamic memory allocation shall not be used |
| [PLCOPEN-E2](#plcopen-e2) | high | high | Pointer arithmetic shall not be used |
| [PLCOPEN-E3](#plcopen-e3) | medium | high | Some comparator instructions shall not be used for pointer or reference manipulation |
| [TaintedVariable](#taintedvariable) | high | – | Output driven by an untrusted input without a bounds check |
| [DuplicateCode](#duplicatecode) | medium | – | Duplicated code |
| [InconsistentCopy](#inconsistentcopy) | high | – | A copy of code that missed a rename |
| [MixedTypeArithmetic](#mixedtypearithmetic) | low | – | Arithmetic operands should have the same type |
| [OutOfBounds](#outofbounds) | high | – | Array index or initial value out of bounds |

The guidelines have 64 rules. The rest are out of reach: some only apply to
Ladder, FBD or SFC, which aren't parsed; some are about comments or layout,
which the parser doesn't keep; and some are matters of design judgement.

**Names.** IEC 61131-3 names aren't case-sensitive, so they are compared in
upper case. Warnings show names as written where the position of the name is
known.

**What is analysed.** Checks that follow calls or look at the whole
application (CP2, CP10, CP12, CP20, CP26, TaintedVariable and others) only
see the files given on the command line. Use `--merge` or pass all files of
a project together.

---

## PLCopen rules

### PLCOPEN-CP1

**Access to a member shall be by name**

Reports direct accesses, read or written, to an address that a configuration
or resource has declared a named variable for. Use the name instead.

### PLCOPEN-CP2

**All code shall be used in the application**

Reports dead code:

- statements after `RETURN`, `EXIT` or `CONTINUE`, or after a statement all
  of whose branches end with one;
- code guarded by a condition that is always false, and the other branches of
  one that is always true (`IF 1 > 2`, `WHILE FALSE`, a `FOR` loop with
  constant bounds that never runs);
- functions and function blocks that nothing in the application uses, and
  programs no configuration runs.

Each dead region is reported once, at its first statement. The rule allows a
bypassed section explained by a comment, with `IF FALSE THEN` as its example.
Comments aren't kept, so a literal `IF FALSE` counts as such a bypass.

Unused POUs are only reported when the analysed code has an entry point: a
configuration that runs programs, or a program. A file with only functions
and function blocks is taken to be a library.

### PLCOPEN-CP3

**Variables shall be initialized before being used**

Reports variables read before they are assigned on every path from the start
of the POU, when they have no initial value. This happens in the first cycle,
whatever later cycles do.

Exempt: inputs, in-outs, externals and globals; `RETAIN` and `CONSTANT`
variables; variables at physical inputs; function block instances;
references and pointers, which are `NULL`; and variables of types with a
default initial value. Writing an element or a member counts as initializing
the variable.

### PLCOPEN-CP4

**Direct addressing should not overlap**

Reports located variables whose memory overlaps, using the size of their
types (for example, a `DINT` at `%MW10` also covers `%MW11`). Addresses with
different size prefixes (`%MB`, `%MW`, …) aren't compared, as how they map
onto each other depends on the PLC.

### PLCOPEN-CP6

**Avoid external variables in functions, function blocks and classes**

Reports `VAR_EXTERNAL` declarations in functions, function blocks and
classes. As the rule allows, references to global constants are fine.

### PLCOPEN-CP7

**Error information shall be tested**

Reports calls whose error outputs are never read by the calling POU. Error
outputs are outputs named like error information: `Error`, `ErrorID`,
`ErrorCode`, `Err`, also with a Hungarian prefix (`xError`, `iErrorCode`).
Reading one of them, such as the `Error` flag, is enough.

An error output is tested when the POU reads it (`inst.Error`), or when it is
connected with `=>` to a variable that the POU reads or passes on as its own
output, in-out or global. The outputs of an instance keep their values, so
each instance is checked once; each call of a function is checked on its own.

The error outputs of function blocks and functions declared in the analysed
code are known, and PLCopen Motion Control blocks (`MC_…`) have `Error` and
`ErrorID`. Other library blocks aren't checked.

### PLCOPEN-CP8

**Floating point comparison shall not be equality or inequality**

Reports `=` and `<>` between `REAL` or `LREAL` values, found from the types of
variables, literals and function results. As the rule allows, comparing with
`0.0` is fine. Use a tolerance instead: `ABS(a - b) < EPSILON`.

### PLCOPEN-CP9

**Limit the complexity of POU code**

Reports POUs whose McCabe complexity or number of statements exceeds a
threshold (`thresholds.mccabe_complexity`, default 15, and
`thresholds.statements_count`, default 25).

Statements are counted once each: assignments, calls, `IF` and each `ELSIF`,
`CASE`, loops, `EXIT`, `CONTINUE` and `RETURN`. This gives the rule's own
counts for its examples (18 and 12).

The rule doesn't fix a metric. `thresholds.mccabe_variant` chooses one:

- `"standard"` (default): textbook McCabe, the number of decisions plus one.
  Each `IF`, `ELSIF`, `CASE` branch and loop is a decision.
- `"plcopen"`: weighted to give the rule's values for its examples (12 and 8,
  where standard McCabe gives 8 and 6). Each `AND` and `OR` in a condition
  counts 1, a `FOR` loop 2 and each `EXIT` 1. The examples only determine
  that much; everything else counts as in standard McCabe.

### PLCOPEN-CP12

**Physical outputs shall be written once per PLC cycle**

Reports outputs that can be written more than once in a cycle: twice on some
path through a POU, in a loop, or by two programs that run in the same task.
An output is written by assigning to its address, to a variable located at
it, or through an output parameter (`OUT => %QW1`), including in called
function blocks. Writes to array elements with non-constant indexes are
skipped, as which output they write isn't known.

### PLCOPEN-CP13

**POUs shall not call themselves directly or indirectly**

Reports recursion, direct or through other POUs, with the path of calls.
Calling a function block instance calls its type.

### PLCOPEN-CP16

**Tasks shall only call program POUs and not function blocks**

Reports program configurations whose type is a function, a function block
declared in the analysed code or a standard function block, and function
block instances of programs assigned to tasks of their own
(`PROGRAM P : T(FB1 WITH task)`, as in the rule's example). Types that aren't
declared in the analysed code may be programs from other files and aren't
reported.

### PLCOPEN-CP17

**Usage of parameters shall match their declaration mode**

Reports inputs that are never read or that are written, outputs that are
never written, and in-outs that are never used. Accessing a member or an
element uses the parameter.

### PLCOPEN-CP20

**Function block instances should be called only once**

Reports instances that can be called more than once in a cycle: twice on
some path, in a loop, or by two programs in the same task. Counters (`CTU`,
`CTD`, `CTUD`) are exempt, as the rule allows a counter counting several
events in one cycle.

### PLCOPEN-CP25

**Data type conversion should be explicit**

Reports implicit conversions that may lose value or precision: in
assignments, in call arguments and between the operands of an operation
(`DINT` and `REAL`, `INT` and `UINT`, a literal out of the range of its
target). Conversions without loss, such as `INT` to `DINT` or `INT` to
`REAL`, are allowed (IEC 61131-3, Table 11).

### PLCOPEN-CP26

**A global variable may be written only by one PROGRAM**

Reports writes of a global by a program other than the first one that writes
it. Writes made by called function blocks and functions count, and so do
writes through output connections in the configuration
(`PROGRAM p : T(OUT => g)`).

### PLCOPEN-CP28

**Time and physical measures comparisons shall not be equality or inequality**

Reports `=` and `<>` on times, including times held in integers, which the
rule covers too: integers converted from a time (`TIME_TO_DINT(t.ET)`),
computed from one, or assigned one (a variable, a function block output or a
function result). Dates aren't included: comparing them for equality makes
sense. Times converted to reals are left to CP8.

Physical measures in integers, such as raw analog inputs, can't be told apart
from other integers by their types and aren't reported.

### PLCOPEN-L10

**Usage of CONTINUE and EXIT instructions should be avoided**

Reports `CONTINUE` and `EXIT`.

### PLCOPEN-L13

**FOR loop variable should not be used outside the FOR loop**

Reports uses of a `FOR` loop's control variable after the loop, including in
array indexes, until another `FOR` loop uses it again.

### PLCOPEN-L17

**Each IF instruction should have an ELSE clause**

Reports `IF` statements without `ELSE`.

### PLCOPEN-L22

**Loop variables should not be modified inside a FOR loop**

This is rule L12 on its page of the guidelines, and L22 in their table of
contents.

Reports changes, in the body of a `FOR` loop, of its control variable and of
the variables its final value and increment are computed from. The standard
says they "shall not be altered by any of the repeated statements". Changes
are assignments, call outputs (`=>`), arguments passed to `VAR_IN_OUT`
parameters of declared POUs, and nested `FOR` loops that reuse the variable.
The initial value is evaluated once, so the variables it is computed from may
change.

### PLCOPEN-N1

**Avoid physical addresses**

Reports direct addresses (`%IX0.0`, `%MW10`, …) used in code. Declaring a
variable at an address (`AT %IW0`) is fine.

### PLCOPEN-N2

**Define type prefixes for variables**

Only runs with a configuration. Reports variables whose names don't start
with the configured prefixes: a scope prefix followed by a type prefix, such
as `gxReady` for a global `BOOL`. Either can be configured alone.

- `naming_conventions.type_prefixes`: by elementary type (`"BOOL": "x"`), by
  the name of a user-defined type, by kind (`STRUCT`, `ENUM`, `NAMED`,
  `ARRAY`, `SUBRANGE`, `REF`, `ALIAS`, `FUNCTION_BLOCK`), or `UDT` for any
  user-defined type. An alias of an elementary type falls back to that type.
- `naming_conventions.scope_prefixes`: `GLOBAL`, `LOCAL`, `TEMP`, `INPUT`,
  `OUTPUT`, `IN_OUT`, or `PARAMETER` for the last three.

A prefix must be followed by the start of a word (an upper-case letter, a
digit or `_`), so `xylophone` doesn't have the prefix `x`. Keys aren't
case-sensitive, and `TOD` is `TIME_OF_DAY`.

### PLCOPEN-N3

**Define the names to avoid**

Reports names that are reserved words of IEC 61131-3 (3rd edition): names of
variables, POUs, types, enum values and struct members.

### PLCOPEN-N4

**Define the use of case (capitals)**

Only runs with a configuration. Checks names against the style configured in
`naming_conventions.case` for their kind: `variable`, `constant`, `pou`,
`type`, `struct_member` (the variable style if not set) and `enum_value` (the
constant style if not set).

Styles are those the rule lists: `UpperCamelCase`, `lowerCamelCase`,
`UPPER_SNAKE_CASE`, `lower_snake_case`, `alllowercase` and `ALLUPPERCASE`.
All capitals isn't UpperCamelCase (`STARTMOTOR`, a "Don't" of the rule),
except for names of two letters such as `IO`. The names of POUs and types are
checked after their [N10](#plcopen-n10) prefix, so `PRG_Main` is
UpperCamelCase.

### PLCOPEN-N5

**Local names shall not shadow global names**

Reports local variables with the name of a global variable (declared in a
configuration, a resource or a `VAR_GLOBAL` block) or of a task.
`VAR_EXTERNAL` refers to the global and isn't shadowing. Local variables with
the name of a POU or a type are reported by [N9](#plcopen-n9).

### PLCOPEN-N6

**Define an acceptable name length**

Only runs with a configuration. Reports names of tasks, POUs, types and
variables shorter than `naming_conventions.min_length` or longer than
`max_length`. Local variables (`VAR`, `VAR_TEMP`) have their own minimum,
`min_length_local`. The rule proposes 8 characters, or 3 for local names,
and at most 25. Loop counters are exempt from the minimum, and struct members
aren't checked, as the rule allows.

### PLCOPEN-N8

**Define the acceptable character set**

Reports names with characters outside ASCII letters, digits and underscores,
such as accented or national letters, and names with consecutive or trailing
underscores, which IEC 61131-3 (6.1.2) doesn't allow. The rule's exception
for a national character set is `naming_conventions.allow_non_ascii`. Names
starting with a digit aren't valid in the language and fail to parse.

### PLCOPEN-N9

**Different element types should not bear the same name**

Reports names shared by elements of different kinds in the same scope:
tasks, program instances, programs, function blocks, functions, classes,
interfaces, variables and user-defined types. POUs and types are global;
variables belong to their POU; tasks, program instances and global variables
to their configuration. So a local variable can't clash with a task, but a
POU can clash with anything.

### PLCOPEN-N10

**Define name prefixes for user defined types**

Only runs with a configuration. Reports types and POUs whose names don't
start with the prefix in `naming_conventions.udt_prefixes` for their kind:
`ENUM`, `NAMED` (an enum with a base type), `SUBRANGE`, `ARRAY`, `STRUCT`,
`REF`, `ALIAS`, `UDT` (any user-defined type), `FUNCTION`, `FUNCTION_BLOCK`,
`PROGRAM`, `CLASS` and `INTERFACE`. Prefixes are compared with the name as
written and end at a word boundary, like those of [N2](#plcopen-n2).

### PLCOPEN-E1

**Dynamic memory allocation shall not be used**

Reports calls that allocate memory at run time: `__NEW` (CODESYS, TwinCAT)
and `SysMemAlloc…`/`SysMemRealloc…` of the CODESYS SysMem library. An
allocator the application implements itself, which the rule forbids too,
can't be told apart from other code.

### PLCOPEN-E2

**Pointer arithmetic shall not be used**

Reports arithmetic on pointers, references and addresses, such as
`ADR(buf) + 2` or `p := p + 2`. Addresses are `POINTER TO` and `REF_TO`
variables, `REF(x)`, `NULL`, `ADR(x)`, `__NEW(…)`, and integers assigned an
address. A chain of arithmetic is reported once. Use an array and its index
instead.

### PLCOPEN-E3

**Some comparator instructions shall not be used for pointer or reference manipulation**

Reports `<`, `>`, `<=` and `>=` on pointers, references and addresses; their
order depends on how the PLC lays out memory. `=` and `<>` are allowed, and
comparing the values they point to (`p^ < q^`) is fine.

---

## Other checks

### TaintedVariable

**Output driven by an untrusted input without a bounds check**

Reports physical outputs (`%Q`) driven by values from physical inputs (`%I`)
or network-writable memory (`%M`) that haven't been bounded on both sides.
See [Taint analysis](taint-analysis.md).

### MultiTaskWrite

**Avoid multiple writes from multiple tasks** (PLCopen CP10)

Reports outputs, memory and global variables written by program instances
that run in different tasks. Such tasks can preempt each other, and the value
left depends on which one ran last. Writes by called function blocks and
functions count as writes of the program, and so do writes through output
connections (`PROGRAM p : T(OUT => g)`). Each configuration is a separate
PLC, so only instances of one configuration are compared.

### UnusedVariable

**Do not declare variables that are not used** (PLCopen CP24)

Reports local variables that are never used, and global variables of a
configuration that no POU, program connection or task setting uses. A
located global is also used through its address. Calling a function block
instance uses it. Globals are only reported when the analysed code declares
every program the configuration runs, as other code may use them otherwise.

### DuplicateCode

**Duplicated code**

Reports runs of statements that repeat elsewhere with the same shape: the same
syntax tree with names and literal values left out, so a copy with renamed
variables or different setpoints still matches. Runs smaller than
`thresholds.duplicate_code_size` syntax tree nodes (default 25) aren't
reported.

### InconsistentCopy

**A copy of code that missed a rename**

In duplicated code whose names follow one renaming (`P1` → `P2`), reports a
name that should follow it but doesn't: a likely copy-paste error.

### MixedTypeArithmetic

**Arithmetic operands should have the same type**

Reports arithmetic on operands of different numeric types, such as `INT` and
`REAL`, or an integer and a real literal. Untyped integer literals take the
type of the other operand and aren't reported.

### OutOfBounds

**Array index or initial value out of bounds**

Reported by the `DeclarationAnalysis` and `UseDefine` passes: constant array
indexes outside the declared range or with the wrong number of dimensions,
array initializers with more values than the array has elements, string
initializers longer than the string, and subrange initial values outside the
range.
