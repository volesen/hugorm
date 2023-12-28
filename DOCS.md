# Documentation

## Values

Currently, we have 3 types of values:
- 63-bit numbers
- Booleans
- Pointers

The tag bits are used to distinguish between these types. The tag bits are the 2 least significant bits of the value, and are `0b0` for numbers, `0b11` for booleans, and `0b01` for pointers (see table below).

| Bit                      | 64 | 63..4 | 3 |   2   |   1   |
|--------------------------|:--:|:-----:|:-:|:-----:|:-----:|
| 63-bit number            |  - |   -   | - |   -   | **0** |
| Boolean false            |  0 | 1...1 | 1 | **1** | **1** |
| Boolean true             |  1 | 1...1 | 1 | **1** | **1** |
| Pointer (8-byte aligned) |  - |   -   | 0 | **0** | **1** |