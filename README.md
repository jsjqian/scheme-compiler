# scheme-compiler

A compiler that takes in Scheme and outputs LLVM. Project designed by Thomas Gilray for CMSC430 at the University of Maryland, College Park.

### Academic Integrity Declaration
I, Jack Qian, pledge on my honor that I have not given or received any unauthorized assistance on this assignment.

## Dependencies

To use this compiler, install [racket](https://docs.racket-lang.org/) and [LLVM](http://llvm.org/docs/).

## Compilation Passes

1. **Top-Level:** In this stage, all implicit begin forms are made explicit and defines are nested in ```letrec*```. It also quotes all datums and desugars quasiquote and unquote for use in later passes.
2. **Desugar:** This pass translates the result from the previous pass into a smaller language that only allows ```let```, lambda calculus, conditionals, ```set!```, ```call/cc```, and prims. 
3. **Assignment Convert (Alphatize), ANF, and CPS:** Once the output is in the smaller core language, the next three steps remove ```set!``` while also alphatizing to make each variable unique, convert to administrative normal form so that there is an explicit order of evaluation, and then translate to continuation-passing style where the continuation is passed forward and functions no longer return. 
4. **Closure Conversion and Emitting LLVM:** Afterwards, all lambda abstractions are removed and are replaced with ```make-closure``` and ```env-ref``` forms in preparation for conversion in LLVM functions that can be called from the top level. To emit LLVM, the compiler combines the converted code with a ```header.ll``` file that contains implementations of Scheme functions in C++ converted into LLVM. The resulting file is combined with the result from the compilers conversion into a ```combined.ll``` file that is then compiled with ```clang++``` and exectued.

## Testing

To test the code, a ```tests.rkt``` file is provided. To run all tests, run ```racket tests.rkt "all"```. Tests can be run individually by running ```racket tests.rkt <test-name>```. Sets of tests can be run as well. These are the current test sets available:

* **Public** ```racket tests.rkt "public"```
* **Release** ```racket tests.rkt "release"```
* **Secret** ```racket tests.rkt "secret"```
* **Error** ```racket tests.rkt "error"```

## Documentation of Primitive Operations

Based on documentation from [racket-lang](https://racket-lang.org/).

```scheme
(null? v) → boolean?
  v: any/c
```
Returns ```#t``` if ```v``` is the empty list, ```#f``` otherwise.


```scheme
(car p) → any/c
  p : pair?
```
Returns the first element of the pair ```p```.


```scheme
(cdr p) → any/c
  p : pair?
 ```
 Returns the second element of the pair ```p```.
 

```scheme
(cons a d) → pair?
  a : any/c
  d : any/c
```
Returns a newly allocated pair whose first element is ```a``` and second element is ```d```.


```scheme
(append lst ...) → list?
  lst : list?
(append lst ... v) → any/c
  lst : list?
  v : any/c
```
When given all list arguments, results in a list that contains all elements of the given lists in order. The last argument is used directly in the tail of the result. If the last argument is not a list, the result is an "improper list."


```scheme
(number? v) → boolean?
  v : any/c
```
Returns ```#t``` if ```v``` is a number, ```#f``` otherwise.


```scheme
(= z w ...+) → boolean?
  z : number?
  w : number?
```
Returns ```#t``` if all of the arguments are numerically equal, ```#f``` otherwise.


```scheme
(<= x y ...+) → boolean?
  x : real?
  y : real?
```
Returns ```#t``` if the arguments in the given order are non-decreasing, ```#f``` otherwise.


```scheme
(> x y ...+) → boolean?
  x : real?
  y : real?
```
Returns ```#t``` if the arguments in the given order are strictly decreasing, ```#f``` otherwise.


```scheme
(+ z ...) → number?
  z : number?
```
Returns the sum of ```z```s, adding pairwise from left to right. If no arguments are provided, the result is ```0```.


```scheme
(- z) → number?
  z : number?
(- z w ...+) → number?
  z : number?
  w : number?
```
When no ```w```s are supplied, returns ```(- 0 z)```. Otherwise, returns the subtraction of the ```w```s from ```z``` working pairwise from left to right.


```scheme
(* z ...) → number?
  z : number?
```
Returns the product of the ```z```s, multiplying pairwise from left to right. If no arguments are provided, the result is 1. Multiplying any number by exact ```0``` produces exact ```0```.


```scheme
(/ z) → number?
  z : number?
(/ z w ...+) → number?
  z : number?
  w : number?
```
When no ```w```s are given, returns ```(/ 1 z)```. Otherwise, returns the division of ```z``` by the ```w```s working pairwise from left to right. If any ```w``` is 0, the program will halt and an error message will appear indicating where the computation was made.


```scheme
(not v) → boolean?
  v : any/c
```
Returns ```#t``` if ```v``` is ```#f```, ```#f``` otherwise.


## Run-time Errors

### Handled

During run-time, some errors will be caught and a message about the error will be displayed. If an error does occur during run-time, the program will halt. Listed are the errors and related tests that catch these errors:

* **Function is provided too many arguments.**
  * If a function is provided too many arguments, an error message will be returned stating that too many arguments were provided. No additional information is provided.
  * Associated tests: ```err-many-args-0.scm```, ```err-many-args-1.scm```

* **Function is provided too few arguments.**
  * If a function is provided too few arguments, an error message will be returned stating that too few arugments were provided and the number of arguments missing.
  * Associated tests: ```err-few-args-0.scm```, ```err-few-args-1.scm```

* **Non-function value is applied.**
  * If a non-function value is applied, an error message will be returned stating that a non-function value was applied and then the application that caused the error.
  * Associated tests: ```err-not-func-0.scm```, ```err-not-func-1.scm```

* **Use of not-yet-initialized letrec or letrec* variable.**
  * If a not-yet-initialized variable is used, an error message will be returned stating the variable that was not initialized.
  * Associated tests: ```err-uninit-vars-0.scm```, ```err-uninit-vars-1.scm```
  
* **Division by zero.**
  * If a division by zero occurs, an error message will be returned with the expression that caused the error.
  * Associated tests: ```err-div-0.scm```, ```err-div-1.scm```

### Unhandled

There are several run-time errors that are not handled, including:
* Integer overflows
* Memory limits
* Correct number of arguments for primitives
Given more time, hopefully these run-time errors could be handled and increase the versatility of this compiler

### Testing
Currently, error tests can be run through the command given above. These tests simply check to see if the returned value is a string and returns true if so, false otherwise. This may be changed in the future.
