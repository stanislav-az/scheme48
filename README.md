# Scheme REPL

```bash
stack run
Lisp>>> (load "scm/stdlib.scm")
```

Exit with "quit" command or `Ctrl+D`.

# Scheme file execution

```bash
stack run -- "scm/stdlib.scm"
```

# Measuring runtime statistics

```bash
./.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.4.1.0/build/scheme48-exe/scheme48-exe "scm/factorial.scm" +RTS -sstderr
```
