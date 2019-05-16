# Evaluation

This is split into two main sections:
- `performance_test` which contains the OCaml code containing basic implementations of maildir and mbox to compare against.
- `scripts` which contains a mixture of bash scripts to perform the actual timing and python scripts to generate the dummy data for testing. 

## Timing test naming scheme

All test names are formed by: `[mailbox_type]_[test_name]` where the test names are:
- tdp  : time deliver parallel
- tpdd : time deliver parallel testing parallelism
- tds  : time deliver sequential
- tmp  : time move parallel
- tmpp : time move parallel testing parallelism

The mailbox type names are:
- mb    : mbox
- md    : maildir
- empty : gitmaildir

