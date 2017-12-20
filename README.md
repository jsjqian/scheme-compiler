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
  * If in a letrec or letrec*, a variable is uninitialized when it is used, an error message will be returned with the expression that has the improper usage..
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

## Additional Features

There are two main features that are included in this compiler: hash tables and hash sets.

### Hash Tables

A hash table allows storage of key value pairs. The hash table implementaion for this compiler is based on HAMT, meaning hash tables are immutable. Methods that modify the hash table will return a new hash table with the updated values. These are the current operations supported:

```scheme
(hash key val ... ...) → hash?
  key : any/c
  val : any/c
```
Creates an immutable hash table with each given key mapped to the following val. Each key must have a val, so the total number of arguments to ```hash``` must be even.

```scheme
(hash-set hash key v) → hash?
  hash : hash?
  key : any/c
  v : any/c
```
Functionally extends ```hash``` by mapping key to v, overwriting any existing mapping for ```key```, and returning the extended hash table.

```scheme
(hash-ref hash key [failure-result]) → any
  hash : hash?
  key : any/c
  failure-result : any/c
```
Returns the value for ```key``` in ```hash```. If no value is found for ```key```, then ```failure-result``` determines the result:
* If ```failure-result``` is a procedure, it is called with no arguments to produce the result.
* Otherwise, ```failure-result``` is returned as the result.

```scheme
(hash-has-key? hash key) → boolean?
  hash : hash?
  key : any/c
```
Returns ```#t``` if ```hash ``` contains a value for the given ```key```, ```#f``` otherwise.

```scheme
(hash-remove hash key) → hash?
  hash : hash?
  key : any/c
```
Functionally removes any existing mapping for ```key``` in ```hash```, returning the fresh hash table.

### Hash Sets

A hash set stores values. The hash set implementaion for this compiler is based on HAMT, meaning hash sets are immutable. Methods that modify the hash set will return a new hash set with the updated values. These are the current operations supported:

```scheme
(set v ...) → set?
  v : any/c
```
Creates a hash set with the given ```v```s as elements.

```scheme
(set-member? st v) → boolean?
  st : set?
  v : any/c
```
Returns ```#t``` if ```v``` is in ```st```, ```#f``` otherwise.

```scheme
(set-add st v) → set?
  st : set?
  v : any/c
```
Produces a set that includes ```v``` plus all elements of ```st```.

```scheme
(set-remove st v) → set?
  st : set?
  v : any/c
```
Produces a set that includes all elements of ```st``` except ```v```.

## Bugs and Areas for Improvement

This compiler, while functional, is still in early stages and has several issues that affect its performance. Here are major bugs that have not been addressed yet:

* **Inefficient Code**
  * The length of the code produced by this code is quite long. Just the ```header.ll``` file, which contains implemetations of the primitive operations supported by this compiler, is already over 10,000 lines long. This makes the compiled binary code take a decent amount of time to actually execute and is not ideal from an efficiency perspective.

* **Run-time Error Handling**
  * The way run-time errors are handled by this compiler is by either adding checks during compilation that make sure no invalid operations take place or by guessing where an error could occur and placing a halt statement in case execution reaches there. While this may be satisfactory at a basic level, under more stringent testing with more diverse test cases, it is possible that the run-time errors will no longer be handled properly. In addition, some of these checks could be left as compile time errors instead of waiting until execution which could save a user time during development.

* **Memory Usage**
  * This compiler has little regard for efficient usage of memory. There are no calls to free allocated memory and it does not make usage of any form of garbage collection. There was difficulty associated with actually installing the Boehm garbage collector so it has been left as a future improvement. However, this means that given large enough programs, the resulting binary could consume staggering amounts of memory that is not ideal for usage.

* **Usage of HAMT**
  * For the ```set-remove``` function, there was difficulty in getting the given HAMT implementation to actually remove an element from the set. It's possible that with some further investigation the problem can be resolved, but for now it limits the usefulness of the hash set.

* **Tagging**
  * Currently, hash tables and hash sets are not tagged in any special way besides the OTHER_TAG. This makes it difficult to create the ```hash?``` and ```set?``` functions. There are no checks in place in the event that an invalid argument is passed to the hash functions and there can also be uncertain effects when misusing one for the other.
