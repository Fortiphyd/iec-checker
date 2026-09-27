# Taint analysis

The `TaintedVariable` check follows untrusted values to physical outputs.

- **Sources** are physical inputs (`%I`), which can fail or be manipulated,
  and network-writable memory (`%M`). `BOOL` inputs are skipped: a bounds
  check doesn't apply to them.
- **Sinks** are physical outputs (`%Q`).
- A value is **cleaned** by bounding it on both sides with trusted bounds.

A warning is an assignment of an untrusted value to an output without both
bounds applied on the way:

```
Output %QW0 is driven by untrusted %IW0 without a bounds check
```

## Bounds checks

These bound a value, when the limits are themselves trusted (constants, or
values not derived from a source):

- `LIMIT(lo, x, hi)`.
- `MIN(x, hi)` bounds above and `MAX(x, lo)` below; nested, they bound both
  sides.
- A condition such as `IF x >= 0 AND x <= 100 THEN` bounds `x` in the branch;
  `IF x < 0 OR x > 100 THEN RETURN; END_IF` bounds it after. `ELSIF` and
  `ELSE` branches get the opposite of the conditions before them.
- A `CASE` branch whose labels are all constants bounds the selector.
- Unsigned types can't be negative, so their lower bound is given.
- The elapsed time `ET` of a timer (`TON`, `TOF`, `TP`) is between 0 and its
  `PT`, so it is trusted when `PT` is.

Arithmetic on a bounded value doesn't keep its bounds. A bound on one array
element or struct member doesn't bound the whole variable.

## What is followed

- Variables, through assignments, along each path; states are joined after
  `IF` and `CASE`, and loops are followed until nothing changes. Programs and
  function blocks keep their variables between cycles, so their bodies are
  followed until nothing changes too.
- Located global variables, declared in configurations, resources and
  `VAR_GLOBAL` blocks. Writing network-writable memory doesn't make it
  trusted: the network can write it again.
- Calls of function blocks and functions declared in the analysed code: a
  summary of each says which outputs depend on which inputs, and which
  outputs it drives from its inputs. Unknown callees pass the taint of all
  their inputs to all their outputs.
- Program connections in configurations: `PROGRAM p : T(u := %IW0, y => %QW0)`
  makes the input untrusted and the output a sink.

Taint isn't followed through global variables that aren't located, or from
one program to another through them.
