# Advent of Code 2022

In [Lean](https://leanprover.github.io).

# Building

Install elan: https://github.com/leanprover/elan

```bash
curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh
```

and use `lake build` to build.

## M1 Toolchain

if on an M1 use the m1 nightly 

```bash
elan toolchain install leanprover/lean4:nightly
elan default leanprover/lean4:nightly
```

# Running

You can run the exe with `lake exe advent`