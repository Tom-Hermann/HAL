# HAL

The goal of this project is to implement an interpreter for a minimalist dialect of [LISP](https://en.wikipedia.org/wiki/Lisp_(programming_language)) in Haskell.

Our dialect of Lisp is functionnal (almost no side effects or mutable states), and a subset of Scheme.

Therefore, an expression evaluated by your interpreter must give the same result as the same expression evaluated by a [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)) interpreter (the reference implementation being [Chez-Scheme](https://github.com/cisco/chezscheme))

## Invocation

Your interpreter must take a list of files as command line arguments and interpret them sequentially. Symbols defined in a file must be kept defined in subsequent files.

```bash
∼/HAL> cat foo.scm
(define foo 21)
∼/HAL> cat bar.scm
(* foo 2)
∼/HAL> ./hal foo.scm bar.scm
42
```

> When more than one expression is evaluated in a file or multiple files, only the very last one must be printed on the standard output

>Your interpreter should support an interactive mode (REPL). For convenience most of the examples in this subject uses this mode. See REPL section bellow for details.

## Error handling

You must stop the exectution as soon as an error occurs and return a 84 status code. You’re free to display any meaninful information on the standard output or error output.

```
∼/HAL> ./hal error.scm
*** ERROR : variable foo is not bound.
∼/HAL> echo $?
84
```

## Types

Your interpreter must support the following types:
- Signed integers (64 bits or more)
- Symbols (unique identifiers)
- Lists as linked lists of cons cells, an empty list being represented by **’()**

## Builtins

Here are the procedures you must implement in your interpreter as builtins. If in doubt they must perform as in Chez-Scheme. You are free to implement other builtins as long as they don’t conflict with Chez-Scheme default library.

### cons

Takes two arguments, construct a new list cell with the first argument in the first place (car) and the second
argument is the second place (cdr).
```
> (cons 1 2)
(1 . 2)
> (cons 1 (cons 2 (cons 3 ’())))
(1 2 3)
```

### car

Takes a cons as argument, returns its first element (the car).

```
> (car (cons 1 2))
1
```

### cdr

Takes a cons as argument, returns its second element (the cdr).

```
> (cdr (cons 1 2))
2
> (cdr ’(1 2 3))
(2 3)
```

> **’** is syntactic sugar for the quote special form, documented bellow

### eq?

Returns “#t” if its first and second arguments are equal. Lists are never equals, except for the empty list.
Symbols equals to themselves only. Intergers behave as expected.

```
> (eq? 1 1)
#t
> (eq? (+ 1 1) 2)
#t
> (eq? ‘foo (car ’(foo bar)))
#t
> (eq? ‘foo ’bar)
#f
> (eq? ’() ’())
#t
```

### atom?

Returns **“#t”** if it’s first argument is atomic, that is if it’s not a non-empty list.

```
> (atom? ‘foo)
#t
> (atom? ’(1 2 3))
#f
> (atom? ’())
#t
```

## Arithmetics builtins

you also have to implement “+”, “-”, “*”, “div”, “mod” and “<”. “+”, “-”, “*” take a variable number of arguments, while div, mod and < take exactly two arguments.

```
> (div (* 5 2) (- 3))
-3
> (< (* 2 2) 5)
#t
> (mod (+ 5 5) 3)
1
```

## Special Forms

Special forms are expressions where the arguments are not necessarily evaluated all the time (contrary to the case of a regular procedure call).

### quote

Takes one argument, returns it without evaluating it.

```
> (quote toto)
toto
> (quote (+ 1 2))
(+ 1 2)
```

As syntactic sugar, quote can also be noted as a leading **’** character:

```
> ‘toto
toto
>’(+ 1 2)
(+ 1 2)
```

### lambda

Takes a list of parameters as first argument, and an expresion to evaluate as second argument, returns a lambda (procedure) which can be subsequently called.

```
> (lambda (a b) (+ a b))
#<procedure>
> ((lambda (a b) (+ a b)) 1 2)
3
```

### define

If it’s first argument is a symbol, associate the symbol to its second argument, and returns it’s name.

If it’s first argument is a list, defines a function which name is the first elemnt of the list, the rest of the list its parameters, and the second argument the function’s body.

>alternatively, it’s acceptable to mimic Chez-Scheme behavior and to return / display nothing.

```
> foo
*** ERROR : variable foo is not bound.
> (define foo 42)
foo
> foo
42
> (define add (lambda (a b) (+ a b)))
add
> (add 1 3)
4
> (define (sub a b) (- a b))
sub
> (sub 3 1)
2
```
>The second form of define is easily expressed as a rewrite of the expression using lambda: (define (name arg1 arg2 . . . ) body) => (define name (lambda (arg1 arg2 . . . ) body))

> You only have to support define when placed at the top level. Supporting define inside functions and lambdas (like Chez-Scheme) is considered a bonus.

### let

Takes a list of key/values as first argument, and an expression as a second argument, evaluate this second argument within an environement where the key / value pairs are bound.
```
> (let ((a 2) (b (+ 1 2))) (+ a b))
5
```
> Let can also be expressed as a rewrite involving lambda: (let ((n1 v1) (n2 v2) (n3 v3) . . . ) body) => ((lambda (n1 n2 n3) body) v1 v2 v3)

### cond

Allows to conditionally evaluate expressions. It takes a variable number of arguments. Each argument is a list. “cond” successively evaluates the first element of each list. If its return value is true, it evaluates the
second element of the list and returns it’s value. Otherwise, it tries the next expression.

```
> (cond (#f 1) (#t (+ 1 1)))
2
> (cond ((eq? ‘foo (car ’(foo bar))) ’here) ((eq? 1 2) ’there) (#t ’nope))
her
```

> Contrary to Chez-Scheme’s behavior, your interpreter can consider as invalid a cond which doesn’t have a catch-all condition in last position **(#t)**. Both behaviors are accepted.


## REPL

Your interprer should implement a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) (as all LISPs do).
In this case, if invoked without arguments your interpreter must launch the REPL. Additionally, the REPL can be launched using “-i” as argument, in conjunction with files.

```
∼/HAL> cat foo.scm
(define foo 21)
∼/HAL> ./hal foo.scm -i
> (* foo 2)
42
>
```

> You can simply use getLine, or you’re allowed to use the package haskeline
>
> If an error occurs in interactive mode, you may give control back to the user instead ofterminating the execution, or enter a debug mode.


## Examples

Your lisp interpreter should be able to process the following programs:

### Factorial (fact.lisp)
```
(define (fact x)
    (cond ((eq? x 1) 1)
        (#t (* x (fact (- x 1))))))
```
```
∼/HAL> hal fact.scm -i
> (fact 10)
3628800
```

Find the N’th number in the Fibonacci sequence (fib.lisp)
```
(define (fib x)
    (cond ((eq? x 0) 0)
        ((eq? x 1) 1)
        (#t (+ (fib (- x 1)) (fib (- x 2))))))

```
```
∼/HAL> hal fib.scm -i
> (fib 21)
10946
```
