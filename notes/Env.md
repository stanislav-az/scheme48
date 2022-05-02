# Original Env implementation versus pure state implementation

In original implementation there is a new scope for each function execution in attempt to get lexical scoping:

```bash
Lisp>>> (load "scm/factorial.scm")
(lambda ("n" "p") ...)
Lisp>>> (factorial 7 8)
7
Lisp>>> p
Getting an unbound variable: p
Lisp>>> n
Getting an unbound variable: n
Lisp>>> factorial
(lambda ("n" "p") ...)
```

It behaves here correctly because `newIORef` is called for entire `Env` in `bindVars`.
It is like running new `State` monad each time `bindVars` is called.
Quoting the wikibook:
> Unfortunately, the state monad doesn't work well for us, because the type of data we need to store is fairly complex. For a simple top-level environment, we could get away with [(String, LispVal)], storing mappings from variable names to values. However, when we start dealing with function calls, these mappings become a stack of nested environments, arbitrarily deep. And when we add closures, environments might get saved in an arbitrary Function value, and passed around throughout the program. In fact, they might be saved in a variable and passed out of the runState monad entirely, something we're not allowed to do.

But, in original Env we get accidental dynamic scoping instead of lexical scoping:

```bash
Lisp>>> (define x 3)
3
Lisp>>> (define (f n) (define x n) x)
(lambda ("n") ...)
Lisp>>> (f 5)
5
Lisp>>> x
5
Lisp>>> (define (g k) (set! x 7) k)
(lambda ("k") ...)
Lisp>>> (g 9)
9
Lisp>>> x
7
```

This happened because inside `bindVars` we are creating a new scope (`newIORef`) with old links to variables.

This is not the case in pure state implementation cause there are no old links and new scope is live only during one function body execution.

```bash
Lisp>>> (define x 3)
3
Lisp>>> (define (f n) (define x n) x)
(lambda ("n") ...)
Lisp>>> (f 5)
5
Lisp>>> x
3
Lisp>>> (define (g k) (set! x 7) k)
(lambda ("k") ...)
Lisp>>> (g 9)
9
Lisp>>> x
7
```
