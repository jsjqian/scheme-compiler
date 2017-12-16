# scheme-compiler

A compiler that takes in Scheme and outputs LLVM. Project designed by Thomas Gilray for CMSC430 at the University of Maryland, College Park.

### Academic Integrity Declaration
I, Jack Qian, pledge on my honor that I have not given or received any unauthorized assistance on this assignment.

## Compilation Passes

1. **Top-Level:** In this stage, all implicit begin forms are made explicit and defines are nested in ```letrec*```. It also quotes all datums and desugars quasiquote and unquote for use in later passes.
2. **Desugar:** This pass translates the result from the previous pass into a smaller language that only allows ```let```, lambda calculus, conditionals, ```set!```, ```call/cc```, and prims. 
3. **Assignment Convert (Alphatize), ANF, and CPS:** Once the output is in the smaller core language, the next three steps remove ```set!``` while also alphatizing to make each variable unique, convert to administrative normal form so that there is an explicit order of evaluation, and then translate to continuation-passing style where the continuation is passed forward and functions no longer return. 
4. **Closure Conversion and Emitting LLVM:** Afterwards, all lambda abstractions are removed and are replaced with ```make-closure``` and ```env-ref``` forms in preparation for conversion in LLVM functions that can be called from the top level. To emit LLVM, the compiler combines the converted code with a ```header.ll``` file that contains implementations of Scheme functions in C++ converted into LLVM. The resulting file is combined with the result from the compilers conversion into a ```combined.ll``` file that is then compiled with ```clang++``` and exectued.

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
(* z ...) → number?
  z : number?
```
Returns the product of the ```z```s, multiplying pairwise from left to right. If no arguments are provided, the result is 1. Multiplying any number by exact ```0``` produces exact ```0```.

```scheme
(= z w ...+) → boolean?
  z : number?
  w : number?
```
Returns ```#t``` if all of the arguments are numerically equal, ```#f``` otherwise.

```scheme
(not v) → boolean?
  v : any/c
```
Returns ```#t``` if ```v``` is ```#f```, ```#f``` otherwise.

