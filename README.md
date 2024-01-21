# Scheme Implementation in Haskell

Just learning Haskell.

Prerequisites:

> GHC
> Cabal

How to Run:
> This will take a filename as argument and interpret it.
```
cabal run [projectname] [filename]
```
>Using cabal run with no arguments will just run the REPL.
```
cabal run
```
>  Use the executable.
```
cabal install --installdir=.
```

## Code Examples

> Variable Declarations
```scheme
(define x 10)
(define x "John")
```
> Function Declarations
```scheme
(define (add x y) (+ x y))
```
> Function Calls
```scheme
(add 1 2)
```
