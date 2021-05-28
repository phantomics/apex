<!-- TITLE/ -->

# April

<!-- /TITLE -->

#### Array Programming Re-Imagined in Lisp

---

Ken Iverson's masterpiece reflected in the medium of Lisp.

April compiles a subset of the APL programming language into Common Lisp. Leveraging Lisp's powerful macros and numeric processing faculties, it brings APL's expressive potential to bear for Lisp developers. Replace hundreds of lines of number-crunching code with a single line of APL.

## Why April?

APL veritably hums with algorithmic power. As a handful of characters run past the lexer, vast fields of data grow, morph and distil to reveal their secrets. However, APL has hitherto dwelt in an ivory tower, secluded inside monolithic runtime environments. If you have a store of data you'd like to process with APL, getting it there can be an ordeal akin to hauling tons of cargo on donkeys' backs through a narrow mountain pass. The original APL interpreters ran on mainframes whose only input was the keyboard and only output was the printer, and the legacy of that implementation approach has persisted to this day, limiting the reach of the language.

But no longer. Lisp is the great connector of the software world, digesting and transforming semantic patterns in much the same way that APL works upon numeric patterns. With APL inside of Lisp, databases, streams, binary files and other media are just a few lines of code away from processing with APL.

### Discussion

For the time being, discussion of April and its development is happening on the `##phantomics` channel on irc.freenode.net.

## Automatic Installation

April is supplied by the Quicklisp library manager, so the easiest way to install April is through Quicklisp. April depends on Common Lisp, ASDF and Quicklisp. It has been tested with Steel Bank Common Lisp (SBCL), Clozure Common Lisp (CCL), Embeddable Common Lisp (ECL), Armed Bear Common Lisp (ABCL) and LispWorks.

**Note:** Some special configuration may be needed to use April with the LispWorks IDE due to the use of UTF-8 characters. [Click here for a guide to configuring LispWorks for compatibility with April.](./lispworks-howto.md) Currently April can only be used with the LispWorks IDE, not the CLI environment, as the LispWorks CLI edition does not support UTF-8.

To install April with Quicklisp, evaluate:

```lisp
(ql:quickload 'april)
```

## Manual Installation

If you'd like to install April manually from this repository, you can follow these instructions.

### Cloning the Repository

First, clone the repository to a location on your system. For this example, let's say you cloned it to the directory ~/mystuff/april.

### Preparing Quicklisp

Enter your Quicklisp local-projects directory (usually ~/quicklisp/local-projects) and create a symbolic link to the directory where you cloned the April repository. For example, if you cloned the repo to ~/mystuff/april and your Quicklisp directory is ~/quicklisp/, enter:

```
cd ~/quicklisp/local-projects
ln -s ~/mystuff/april
```

### Installing Dependencies

To complete the installation, just start a Common Lisp REPL and enter:

```lisp
(ql:quickload 'april)
```

This will download and install April's dependencies, and with that the package will be built and ready.

## APL Functions and Operators

The APL language uses single characters to represent its primitive functions and operators. Most of these symbols are not part of the standard ASCII character set but are unique to APL. To see a list of the glyphs that are supported by April, visit the link below.

#### [See the complete April APL lexicon here.](./lexicon.md)

Some APL functions and operators won't be added to April since they don't make sense for April's design as a compiler from APL to Lisp. Others may be added in the future. [See the list of features not implemented here.](#whats-not-planned-for-implementation)

## Getting to Know APL

A full guide to the APL language is far beyond the scope of this file, but here are links to some good sources.

[A high-level introduction to APL.](http://archive.vector.org.uk/art10011550)

[This is a detailed language tutorial covering most of the functions and operators in April.](http://microapl.com/APL/tutorial_contents.html)

[The original paper by Ken Iverson, creator of APL, detailing the language's underlying philosophy.](https://www.jsoftware.com/papers/tot.htm)

If you would like a quick tour of the language, April includes a function that will print demos of all the commands and many APL syntax features. To see the demos, enter:

```lisp
* (april (demo))
```

The * indicates a REPL prompt. Prepare for a long read. The demo content that gets printed will tell you the name(s) of the operations that correspond to each symbol and will hopefully give you some idea of what each one does.

### How to Enter APL Characters

In order to write APL programs you'll need a way to use the language's special character set.

[Click here for information on enabling APL input within Emacs.](#enabling-apl-input-in-emacs)

[Click here for information on enabling APL input within Vim.](#enabling-apl-input-in-vim)

[Click here for information on enabling APL input universally within GNU/Linux.](#enabling-apl-input-universally-in-gnulinux)

## Basic Evaluation: (april) and (april-f)

Evaluating an APL expression is as simple as:

```lisp
* (april-f "1+2 3 4")
3 4 5
#(3 4 5)
```

As above, the * indicates a REPL prompt and the text below is the expression's output.

The macro `(april-f)` (short for april-format) will evaluate any APL string passed to it as the sole argument, returning the final result. Using `(april-f)` will also produce a printout of the output in APL's traditional array printing style, which appears before the actual output value. You can see above how the `3 4 5` is printed out before the value `#(3 4 5)`. APL-style printed arrays are easier to read than Lisp's style of printing arrays; APL can use a simpler style to express its output because it doesn't have as many different data types and structures as Lisp.

If you don't need to see the printout, you can use the plain `(april)` macro. Like this:

```lisp
* (april "1+2 3 4")
#(3 4 5)
```

You should use `(april)` if you're using April to do calculations inside of a larger program and don't need the printout. Otherwise, especially if you're working with large data sets, the system may consume significant resources printing out the results of calculations.

Also note that if the output of an April expression is a single number, `(april-f)` will not print it since the Lisp representation of the number will look the same or very similar. For example:

```lisp
* (april-f "1+2")
3
```

Since the result of 1+2 is 3, a single number, no value printout is provided.

Setting state properties for the APL instance can be done like this:

```lisp
* (april-f (with (:state :count-from 0)) "⍳9")
0 1 2 3 4 5 6 7 8
#(0 1 2 3 4 5 6 7 8)
```

Instead of an APL string, the first argument to `(april)` or `(april-f)` may be a list of parameters for the APL environment. The APL expression is then passed in the second argument.

For example, you can use the `:count-from` parameter to determine whether functions in the evaluated APL code will start counting from 0 or 1. We'll get into more detail on how these parameters work later.

```lisp
* (april-f (with (:state :count-from 1)) "⍳9")
1 2 3 4 5 6 7 8 9
#(1 2 3 4 5 6 7 8 9)

* (april-f (with (:state :count-from 0)) "⍳9")
0 1 2 3 4 5 6 7 8
#(0 1 2 3 4 5 6 7 8)
```

More APL expressions:

```lisp
* (april-f "⍳12")
1 2 3 4 5 6 7 8 9 10 11 12
#(1 2 3 4 5 6 7 8 9 10 11 12)

* (april-f "3 4⍴⍳12")
1  2  3  4
5  6  7  8
9 10 11 12
#2A((1 2 3 4) (5 6 7 8) (9 10 11 12))

* (april-f "+/3 4⍴⍳12")
10 26 42
#(10 26 42)

* (april-f "+⌿3 4⍴⍳12")
15 18 21 24
#(15 18 21 24)

* (april-f "+/[1]3 4⍴⍳12")
15 18 21 24
#(15 18 21 24)

* (april-f "⌽3 4⍴⍳12")
 4  3  2 1
 8  7  6 5
12 11 10 9
#2A((4 3 2 1) (8 7 6 5) (12 11 10 9))

* (april-f "1⌽3 4⍴⍳12")
 2  3  4 1
 6  7  8 5
10 11 12 9
#2A((2 3 4 1) (6 7 8 5) (10 11 12 9))
```

## Compact Function Calls: The (april-c) Macro

Want to invoke April functions on some variables with less code? You can use the `(april-c)` macro. For example:

```lisp
* (april-c "{⍺×⍵}" 2 8)
16

* (april-c "{[a;b;c;d] d↑c⍴a+b}" 3 5 6 10)
#(8 8 8 8 8 8 0 0 0 0)
```

After the string where the April function is written, pass the variables that will be input to the function and you'll receive the result with no need for a long `(with (:state ...))` clause. If you wish to pass parameters in a `(with)` clause, you can still do it with `(april-c)`.

```lisp
* (april-c (with (:state :count-from 0)) "{⍳⍵}" 7)
#(0 1 2 3 4 5 6)
```

### A note on escaping characters

April uses the backslash character `\` to implement the expand function and the scan operator. Because of the way Lisp strings work, this character must be escaped with a second `\` before it in order to enter APL code containing backslashes. For example:

```lisp
* (april-f "+\\⍳5")
1 3 6 10 15
#(1 3 6 10 15)
```

The inside the `"string"`, the two backslashes evaluate to a single backslash. If you forget about this, you can experience confusing errors.

## Unique Language Features in April

For the most part, April's syntax and functions follow standard APL conventions. But there are a few areas where April differs from typical APL implementations along with some unique language features. Most notably:

```lisp
;; k-style if-statements
* (april "x←5 ⋄ $[x>3;8;12]")
8

;; k-style functions with any number of named arguments
* (april "monthlyPayment←{[amt;int;len] (len÷⍨amt×int×0.1)+amt÷len} ⋄ monthlyPayment[5000;0.8;12]")
450.0

;; numbered branch points instantiated with →⎕ syntax
* (april "x←1 ⋄ →1+1 ⋄ x×←11 ⋄ 1→⎕ ⋄ x×←3 ⋄ 2→⎕ ⋄ x×←5 ⋄ 3→⎕ ⋄ x×←7")
35

;; symbol-referenced branch points and a branch function with expression-determined branch symbol choice
* (april "x←1 ⋄ (5-3)→two three ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7")
7
```

The biggest difference between April and other APLs lies in its implementation of the `→ branch` function, as shown in the latter two examples above. April also allows you to use if-statements and functions with any number of named arguments in the style of Arthur Whitney's k programming language.

### Using rational numbers

April is one of a few APL implementations to include rational numbers. They are printed with a lowercase `r` separating the numerator and denominator. Examples:

```lisp
* (april-f "÷⍳5")
1 1r2 1r3 1r4 1r5
#(1 1/2 1/3 1/4 1/5)

* (april-f "2r3×⍳4")
2r3 4r3 2 8r3
#(2/3 4/3 2 8/3)
```

Rational numbers can also be used as parts of complex numbers:

```lisp
* (april-f "3r4J9r5×⍳4")
3r4J9r5 3r2J18r5 9r4J27r5 3J36r5
#(#C(3/4 9/5) #C(3/2 18/5) #C(9/4 27/5) #C(3 36/5))
```

### Underscores within numbers

In April, you can use underscores to separate parts of a number:

```lisp
* (april "1_000_000+5")
1000005

* (april "1__000_000__000_000+5")
1000000000005

* (april "1_00___0_0__00_0+5")
10000005
```

As shown above, you can use any number of underscores anywhere within a number, they are simply ignored by the reader. Underscores are also used by the printer when printing columns of mixed complex floats and rationals:

```lisp
* (april-f "⍪12.2J99.11 3J8 19r13J5r2")
12.20J99.11
 3___J_8   
19r13J_5r_2
#2A((#C(12.2 99.11)) (#C(3 8)) (#C(19/13 5/2)))
```

Using the underscores as filler keeps the decimal points, rs and Js properly aligned for printing.

### Strings and escaped quotes

In April, either single or double quotes can be used to enclose character strings:

```lisp
* (april "'abc','def'")
"abcdef"

* (april "\"ghi\",\"jkl\"")
"ghijkl"
```

Note that you must use backslashes to escape double quotes used within Lisp strings, making double quotes a less desirable choice unless you're loading April code from files using `april-load`. In order to escape quote characters within an April string, you can either enter a backslash before the quote character, as in Lisp and many other languages, or enter the quote character twice in the traditional APL style. For example:

```lisp
* (april "'\'abc'\'")
"'abc'"

* (april "'''abc'''")
"'abc'"
```

## Parameter reference

When `(april)` or `(april-f)` is called, you may pass it either a single text string:

```lisp
* (april-f "1+1 2 3")
```

Or a parameter object followed by a text string:

```lisp
* (april-f (with (:state :count-from 0)) "⍳9")
```

This section details the parameters you can pass to April.

### (test)

To run April's test suite, just enter:

```lisp
* (april (test))
```

### (demo)

As mentioned before, you can see demos of April's functions with:

```lisp
* (april (demo))
```

### (with)

`(with)` is the workhorse of April parameters, allowing you to specify many options for an April invocation. The most common sub-parameter passed via `(with)` is `(:state)`. To wit:

```lisp
* (april (with (:state :count-from 0
                       :in ((a 3) (b 5))
                       :out (a c)))
         "c←a+⍳b")
3
#(3 4 5 6 7)
```

### (:state) sub-parameters

Let's learn some more about what's going on in that code. The sub-parameters of `(:state)` are:

#### :count-from

Sets the index from which April counts. Almost always set to 0 or 1. The default value is 1. In the code above, `⍳b` with `b` equal to 5 counts from 0 to 4, whereas with the default `:count-from` value of 1, `⍳b` would count from 1 to 5. When you set :count-from, it only affects the APL code evaluated in the expression to which the :count-from option is passed. For example:

```lisp

* (april (with (:state :count-from 0)) "⍳9")
#(0 1 2 3 4 5 6 7 8)

* (april "⍳9")
#(1 2 3 4 5 6 7 8 9)

```

#### :in

Passes variables into the April instance that may be used when evaluating the subsequent expressions. In the example above, the variables `a` and `b` are set in the code, with values 1 and 2 respectively. You can use `:in` to pass values from Lisp into the April instance.

```lisp
* (april-f (with (:state :in ((a 5) (b 10))))
           "1+2+a×b")
53
```

Please note that April variables follow a stricter naming convention than Lisp variables. When naming the input variables, only alphanumeric characters, underscores and dashes may be used. In keeping with APL tradition, the delta/triangle characters ∆ and ⍙ can be used in variable names as well. Punctuation marks like ?, >, . and ! may not be used as they have separate meanings in April.

These characters are allowed in variable names within April:
```
0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_∆⍙
```

These variable names are ok for use with the `:in` parameter:
```
a var my_var another-var
```

These are not ok:
```
true! this->that pass/fail? var.name
```

If you use dashes in the names of Lisp variables you pass into April, note that inside April they will be converted to camel case. For example:

```lisp
* (april-f (with (:state :in ((one-var 2)
                              (other-var 5))))
           "oneVar×otherVar+5")
20
```

The dash character `-` is used to denote the subtraction function inside April, so you may not use dashes in variable names within the language.

One more caveat: it's best to avoid using input variable names with a dash before a number or other non-letter symbol. The dash will be removed and the character following it will cannot be capitalized so information will have been lost. For example:

```
my-var-2 → myVar2
my-var-∆ → myVar∆
```

#### :out

Lists variables to be output when the code has finished evaluating. By default, the value of the last evaluated expression is passed back after an April evaluation is finished. For example:

```lisp
* (april-f "1+2
            2+3
            3+4")
7
```

The last value calculated is displayed. The `:out` sub-parameter allows you to list a set of variables that whose values will be returned once evaluation is complete. For example:

```lisp
* (april-f (with (:state :out (a b c)))
           "a←9+2
            b←5+3
            c←2×9")
11
8
18
```

#### :index-origin

This is another, more technical name for the `:count-from` sub-parameter. You can use it instead of `:count-from`:

```lisp
* (april-f (with (:state :index-origin 0)) "⍳9")
0 1 2 3 4 5 6 7 8
#(0 1 2 3 4 5 6 7 8)
```

#### :print-precision

This controls the maximal precision at which April prints floating point numbers. Its default value is 10. For example:

```lisp
* (april-f "○1 2 3")
3.141592654 6.283185307 9.424777961	
#(3.141592653589793d0 6.283185307179586d0 9.42477796076938d0)

* (april-f (with (:state :print-precision 6)) "○1 2 3")
3.14159 6.28319 9.42478
#(3.141592653589793d0 6.283185307179586d0 9.42477796076938d0)

* (april-f (with (:state :print-precision 3)) "○1 2 3")
3.14 6.28 9.42
#(3.141592653589793d0 6.283185307179586d0 9.42477796076938d0)
```

Note that `:print-precision` doesn't affect the Lisp values output by April, only the printed output.

#### :print-to

When using `(april-f)`, the formatted array content is output to the `*standard-output*` stream. When using `(april)`, no formatted output is printed. You can change this using the `:print-to` sub-parameter. For example:

```lisp
* (april (with (:state :print-to *standard-output*)) "2 3⍴⍳9")
1 2 3
4 5 6
#2A((1 2 3) (4 5 6))

* (april-f (with (:state :print-to nil)) "2 3⍴⍳9")
#2A((1 2 3) (4 5 6))
```

Using the `:print-to` parameter effectively erases the distinction between `(april)` and `(april-f)`. The two different macros are provided as a courtesy so you don't need to pass a `:print-to` parameter to get printed output. You can also pass a different stream than `*standard-output*` to `:print-to` to have the printed output directed there.

#### :output-printed

When the `:output-printed` sub-parameter is passed, the string of APL-formatted data that gets printed will also be returned as the last output value by the April invocation. For example:

```lisp
* (april (with (:state :output-printed t)) "2 3⍴⍳9")
#2A((1 2 3) (4 5 6))
"1 2 3
4 5 6
"
```

If you don't want to receive the Lisp value output by April and only want the formatted string as output, you can pass the `:only` option to `:output-printed`, like this:

```lisp
* (april (with (:state :output-printed :only)) "2 3⍴⍳9")
"1 2 3
4 5 6
"
```

This way, the formatted string will be the only returned value.

#### :unformat-output

APL and Common Lisp use different models for nested arrays. [Read more about it here.](#differences-between-apl-and-common-lisp-nested-array-models) If you'd like to receive nested array output from April where the nested arrays are not wrapped in 0-rank scalar arrays, you can use the `:unformat-output` sub-parameter. Here's an example of the difference this sub-parameter makes:

```lisp
* (april "2 2⍴⊂2 2⍴⍳4")
#2A((#0A#2A((1 2) (3 4)) #0A#2A((1 2) (3 4)))
    (#0A#2A((1 2) (3 4)) #0A#2A((1 2) (3 4))))

* (april (with (:state :unformat-output t)) "2 2⍴⊂2 2⍴⍳4")
#2A((#2A((1 2) (3 4)) #2A((1 2) (3 4))) (#2A((1 2) (3 4)) #2A((1 2) (3 4))))
```


### (:space) parameter

If you want to create a persistent workspace where the functions and variables you've created are stored and can be used in multiple calls to April, you must first create the workspace. Here's how:

```
* (april-create-workspace space1)
"Successfully created workspace ｢SPACE1｣."
```

Then, to evaluate April code within use the `(:space)` parameter. For example:

```lisp
* (april-f (with (:space space1)) "a←5+2 ⋄ b←3×9")
27

* (april-f (with (:space space1)) "c←{⍵+2}")
#<FUNCTION ... >

* (april-f (with (:space space1)) "c a+b")
36
```

In the above example, a workspace called `space1` is created, two variables and a function are stored within it, and then the function is called on the sum of the variables.

When you invoke `(april)` without naming a workspace, a workspace called `common` is used.

```lisp
* (april (with (:space common)) "a←5")
5

* (april "a+5")
10
```

### (:store-val) and (:store-fun) parameters

If you'd like to add values and functions from the Lisp instance into an April workspace, you can use the `(:store-val)` and `(:store-fun)` parameters. Here is an example:

```lisp
* (april (with (:store-val (a 12) (b 45))) "a+b+10")
67

* (april "3×a")
36

* (april (with (:store-fun (add-ten (lambda (x) (+ x 10))))) "")
NIL ;; the result of not running code after the function is stored

* (april "addTen 20")
30
```

As shown above, dash-separated variable and function names are converted to camel case, just as when passing input values with the `:in` sub-parameter of `(:state)`. Note also that functions passed into April this way are not adapted for use with arrays the way that functions created within April are. For instance, if you enter:

```lisp
* (april (with (:store-fun (add-ten (lambda (x) (+ x 10))))) "")
NIL

* (april "addTen 1 2 3 4 5")
```

You will get an error stating `The value #(1 2 3 4 5) is not of type NUMBER ...`. That's because in Lisp, you can't add 10 to the vector `#(1 2 3 4 5)`, which is what the function is attempting to do. Use caution when adding arbitrary Lisp functions into an April workspace.

If you want to store functions or variables with names that are read literally rather than being converted to camel case, you can do this by passing strings as the variable names. If a dash is found in such a string-expressed variable name it will cause an error. For example:

```lisp
(april (with (:store-fun ("aBcDe" (lambda (x) (+ x 10))))) "aBcDe 5")
15

;; the presence of a dash causes an error
(april (with (:store-fun ("ab-cd" (lambda (x) (+ x 10))))) "abcd 6")
Error: Invalid characters present in symbol aBC-deF passed to :STORE-FUN.
```


### (:compile-only) parameter

If you just want to compile the code you enter into April without running it, use this option. For example:

```lisp
* (april (with (:compile-only)) "1+1 2 3")
(IN-APRIL-WORKSPACE COMMON
  (LET* ((OUTPUT-STREAM *STANDARD-OUTPUT*))
    (DECLARE (IGNORABLE OUTPUT-STREAM))
    (SYMBOL-MACROLET ((INDEX-ORIGIN 𝕊*INDEX-ORIGIN*)
                      (PRINT-PRECISION 𝕊*PRINT-PRECISION*))
      (APL-OUTPUT (APL-CALL + (SCALAR-FUNCTION +) (AVECTOR 1 2 3) 1)
                  :PRINT-PRECISION PRINT-PRECISION))))
```

## Clearing Workspaces: The (april-clear-workspace) Macro

You can use this macro to clear a workspace, removing all user-created variables within it and returning it to its default state. For example, to clear a workspace called `space1`, enter:

```lisp
* (april-clear-workspace space1)
"The workspace ｢SPACE1｣ has been cleared."
```

## Sharing Scope: The (with-april-context) Macro

Perhaps you'd like to make multiple calls to April using the same workspace and other parameters and you don't want to have to enter the same parameters over and over again. The `(with-april-context)` macro can help. For example:

```lisp
* (april-create-workspace space1)
"Successfully created workspace ｢SPACE1｣."

* (with-april-context ((:space space1) (:state :index-origin 0))
    (april "g←5")
    (april "g×3+⍳9"))
#(15 20 25 30 35 40 45 50 55)
```

Inside the body of the `(with-april-context)` macro, each of the `(april)` invocations act as if they were passed the options `(with (:space space1) (:state :index-origin 0))`. 

```lisp
* (april-create-workspace space1)
"Successfully created workspace ｢SPACE1｣."

* (with-april-context ((:space space1) (:state :index-origin 0))
    (april "x←⍳3")
    (april (with (:state :index-origin 1)) "x,⍳5"))
#(0 1 2 1 2 3 4 5)
```

Options passed for one of the `(april)` invocations inside the context will override the options for the context. Here, the second `(april)` invocation has its index origin set to 1 which overrides the context's 0 value.

## Console Output Using the Quad Character

The `(april-f)` macro is one way to view the printed output of APL expressions. What if you want to see the result of an evaluation that occurs in the middle of your code instead of the end, or if you want to print the contents of multiple arrays within a single expression? At times like these, you can use the `⎕` character, also called "quad." In APL, console output can be produced by "assigning" values to `⎕` like this:

```lisp
* (april "a←1 2 3 ⋄ b←3+⎕←2+a ⋄ ⎕←c←4+b ⋄ c+5")
3 4 5
10 11 12
#(15 16 17)
```

Both of the values assigned to `⎕` are printed in order before the expression's final result is output. Because `(april)` is used instead of `(april-f)`, no formatted values are printed by default; only the values assigned to `⎕` are printed. Using `⎕`, it's easy to debug complex functions.


## Loading Code Directly From Files

Perhaps you'd like to write files containing pure APL code rather than passing strings to `(april)` within Lisp code. The `(april-load)` macro has you covered. For example:

```apl
⍝ contents of file test.apl

v  ← ⍳9
fn ← {⍵+10}
⎕  ← fn v
```

```lisp
* (april-load #P"/path/to/test.apl")
11 12 13 14 15 16 17 18 19
#(11 12 13 14 15 16 17 18 19)

* (april "fn 3 4 5")
#(13 14 15)
```

The variable `v` and the function `fn` have been loaded into the default workspace.

Note that the argument to `(april-load)` must be a pathname, not merely a string. The argument to `(april-load)` may also be an expression that evaluates to a pathname. For instance:

```lisp
* (april-load (pathname (format nil "~a/test.apl" "/test/directory")))
11 12 13 14 15 16 17 18 19
#(11 12 13 14 15 16 17 18 19)
```

The `(april-load)` macro may take a first argument containing the same parameters that can be passed to `(april)`. For example:

```lisp
* (april-create-workspace space1)
"Successfully created workspace ｢SPACE1｣."

* (april-load (with (:space space1) (:state :index-origin 0)) #P"/path/to/test.apl")
10 11 12 13 14 15 16 17 18
#(10 11 12 13 14 15 16 17 18)

* (april (with (:space space1)) "fn 9 8 7")
#(19 18 17)
```

Source code from files can thus be loaded into any workspace.

## APL System Variables and Functions in April

April makes available the following APL system variables and functions:

```
⎕IO ⎕CT ⎕TS ⎕PP ⎕A ⎕D
```

Additionally, April exposes this special system variable not found in other APL implementations:

```
⎕OST
```

[Click here to read the names and descriptions of these symbols.](./environmental-symbols.md)

## About Workspace Variables: Index Origin and Print Precision

Above, you learned how to use the `:count-from`/`:index-origin` and `:print-precision` sub-parameters to control how April counts and prints. Using these parameters with an April invocation will affect *only* the code passed to that particular April invocation. What if you want to create a change in these parameters that will persist in a given workspace until it's changed again?

Traditional APL dialects use the `⎕IO` and `⎕PP` system variables to set the index origin and print precision in a workspace, and using them in April will make a change that persists in the workspace. For example:

```lisp
* (april-create-workspace space1)
"Successfully created workspace ｢SPACE1｣."

* (april-create-workspace space2)
"Successfully created workspace ｢SPACE2｣."

* (april-f (with (:space space1)) "⎕IO←0 ⋄ ⍳9")
0 1 2 3 4 5 6 7 8
#(0 1 2 3 4 5 6 7 8)

* (april-f (with (:space space1)) "⍳9")
0 1 2 3 4 5 6 7 8
#(0 1 2 3 4 5 6 7 8)

* (april-f (with (:space space2)) "⍳9")
1 2 3 4 5 6 7 8 9
#(1 2 3 4 5 6 7 8 9)
```

Switching to the workspace `space2`, the default index origin of 1 is used again.

If you pass an `:index-origin`, `:count-from` or `:print-precision` sub-parameter to an APL invocation, it will override whatever value is present in the active workspace. However, it will only affect the code passed to the individual `(april)` invocation that has the sub-parameter(s) passed.

```lisp
* (april-create-workspace space1)
"Successfully created workspace ｢SPACE1｣."

* (april-f (with (:space space1)) "⎕IO←0 ⋄ ⍳9")
0 1 2 3 4 5 6 7 8
#(0 1 2 3 4 5 6 7 8)

* (april-f (with (:state :count-from 1)) "⍳9")
1 2 3 4 5 6 7 8 9
#(1 2 3 4 5 6 7 8 9)

* (april-f (with (:space space1)) "⍳9")
0 1 2 3 4 5 6 7 8
#(0 1 2 3 4 5 6 7 8)
```

## Setting a Custom Output Stream

April has a system variable called `⎕ost` that you can use to set a custom destination for printed output. Normally, data output using `(april-f)` or values assigned to the quad character like `⎕←1 2 3` are sent to the `*standard-output*` stream. You can change this as follows:

```lisp
* (let* ((out-str (make-string-output-stream))
	 (vector (april-f "a←1 2 3 ⋄ ⎕ost←'OUT-STR' ⋄ ⎕←a+5 ⋄ ⎕←3 4 5 ⋄ ⎕ost←'*STANDARD-OUTPUT*' ⋄ 3+a")))
    (princ (get-output-stream-string out-str))
    vector)
4 5 6
6 7 8
3 4 5
#(4 5 6)
```

Within the APL expression, the output stream is set to `OUT-STR`, two vectors are output to that stream, and then the stream is reset to `*STANDARD-OUTPUT*` before the expression ends and prints its final output. When the code runs, first the APL-formatted output from the `(april-f)` expression is printed. Then, the two APL-formatted strings output to the `out-str` stream are printed. Finally, the Lisp vector that resulted from the `(april-f)` expression is printed.

Remember to use all caps when setting the `⎕ost` variable, unless your desired output stream is referenced by a literal lowercase symbol like `|output-stream|`.

The syntax above assumes that the symbol representing the output stream is internal to the current package. For instance:

```lisp
(in-package #:pkg-one)

(defvar out-str (make-string-output-stream))

(april-f "⎕ost←'OUT-STR' ⋄ 5+10")
```

In this code, the `OUT-STR` output stream is interned in the package `PKG-ONE`. What if you want to use an output stream whose symbol belongs to a package other than the current one?

```lisp
(in-package #:pkg-one)

(defvar out-str (make-string-output-stream))

(in-package #:pkg-two)

(april-f "⎕ost←('PKG-ONE' 'OUT-STR') ⋄ 5+10")
```

If you assign to `⎕ost` a vector of two strings, the first string is the name of a package and the second string is the name of a symbol belonging to that package. In this way, you can reference an output stream whose symbol is interned in a package other than the current one.

## What's Not Planned for Implementation

#### Functions:

```
⍇ File read
⍈ File write
⍐ File hold
⍗ File drop
⎕ Evaluated input
⎕ Output with newline
⍞ Character input
⍞ Bare output
```

#### Operators:

```
& Spawn
⌶ I-Beam
```

[(Click here to see the functions and operators that have been implemented.)](./lexicon.md)

See a pattern? The functions not planned for implentation are all those that manifest low-level interactions between the APL instance and the underlying computer system. Common Lisp already has powerful tools for system interaction, so it's presumed that developers will do things like this outside of April.

## Also Not Implemented

APL's function editor system and control flow statements are not implemented; this type of functionality is also readily accessible through standard Common Lisp.

## April's Lexicon Compared to Other APLs

APL has multiple implementations, and there are subtle but significant variations between the lexical functions they offer. April's set of functions is closest to those offered by Dyalog APL in its default mode. For instance, in April, dyadic `⊂` implements the partitioned enclose function while dyadic `⊆` implements the partition function, as in Dyalog. In IBM APL2, however, there is no partitioned enclose function and dyadic `⊂` implements the partition function. The same is true in GNU APL, whose design primarily follows APL2.

The other major lexical difference between APL2-family languages and April is that in April, monadic `⊃` implements the disclose function and monadic `↑` implements the mix function; the converse is true in APL2.

Dyalog APL offers users the option of using multiple lexical modes, some of which are more similar to APL2. The variable controlling these modes is referred to as the "migration level." The implementation of migration levels in April is not planned at this time.

## Tests and Demo

If you missed it earlier, you can run tests for the implemented APL functions and operators by entering:

```lisp
* (april (test))
```

And you can see a demonstration of April language features by entering:

```lisp
* (april (demo))
```

## Enabling APL Input in Emacs

Most Lisp developers interact with the language through Emacs, so Emacs is also the most convenient tool to write April programs. The best way to input APL characters in Emacs is using the gnu-apl-mode Emacs plugin. You can get it [from the repository here](https://github.com/lokedhs/gnu-apl-mode) or install it directly via the MELPA Emacs package repository.

[Click here for information on using MELPA.](http://ergoemacs.org/emacs/emacs_package_system.html)

Once gnu-apl-mode is installed, you can switch to the APL input mode by typing `M-x toggle-input-method` or `C-\`. You will be prompted to enter the input mode to use, so enter `APL-Z` and then you'll be able to toggle APL input on and off by typing `C-\`. While in APL-Z input mode, you can enter APL characters by prefixing the key with a `.` period character.

## Enabling APL Input in Vim

For Lisp developers who interact with the language through Vim, a plugin called "vim-apl" allows one to input APL characters. You can get it [from this git repository](https://github.com/justin2004/vim-apl). Using a Vim plugin manager called [Vundle](https://github.com/VundleVim/Vundle.vim) it is easy to add this plugin by adding the single line `Plugin 'justin2004/vim-apl'` to your .vimrc and following the Vundle instructions. With vim-apl installed, while editing an .apl file you can enter the iota character `⍳` by typing `` `i `` (backtick and i), enter the rho character `⍴` by typing `` `r``, and so on.

## Enabling APL Input Universally in GNU/Linux

For GNU/Linux users who'd like use APL characters outside of a customized editor, refer to [this page](https://aplwiki.com/wiki/Typing_glyphs_on_Linux) on the APL Wiki. After following the instructions there you'll be able to use your keyboard's right Alt key as a modifier to enter APL characters. For instance, you enter can the iota character `⍳` by pressing the `right Alt + i`, the rho character `⍴` by pressing the `right Alt + r` and so on.

## Thanks to:

Tamas K. Papp, creator of [array-operations](https://github.com/tpapp/array-operations), of which April makes heavy use.

Max Rottenkolber, creator of [MaxPC](https://github.com/eugeneia/maxpc), the heart of April's parsing engine.

[justin2004](https://github.com/justin2004) and [Nikolai Matiushev](https://github.com/egao1980), contributors of many bug reports and suggestions.
