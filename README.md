
## Build

Install the dependencies:

```sh
$ sudo apt install cmake llvm-10-dev libgtest-dev
```

Build rin:

```sh
$ mkdir build && cd build
$ cmake .. && make
```

Repl:

```sh
$ make repl && repl/repl
$ > 1 + 2
$ [i32] i32 3
$ > 1U + 2U * 512U
$ [u32] i32 1025
$ > 233 != 234
$ [bool] i1 true
```
