== Introduction

This markdown file is at the same time a Haskell script. It can be typeset
using the shell command:

`pandoc --from=markdown+lhs` *this file name* 

Haskell code has been placed in `LaTeX`-like `code` environments
in the script. For example, the following equations

~~~~~
\begin{code}
        fix f  = f(fix f)
        fact   = fix(\ fact n -> if n==0 then 1 else n*fact(n-1))
\end{code}
~~~~~

define a fixed point function, `fix` as well as a function `fact`
-- the latter as the fixed point of the (non-recursive) "generator"

        \ fact n -> if n==0 then 1 else n*fact(n-1)

We can check that the fixed point - let us call it `fp` - has exactly
the property we want of a factorial function, and thus justifies
its name: `fact`.

        fp m
       = { the fixed point was called fp 
        fix(\ fact n -> if n==0 then 1 else n*fact(n-1)) m
       = { definition of fix
        ((\ fact n -> if n==0 then 1 else n*fact(n-1)) fp) m
       = { application to fp
        (\ n -> if n==0 then 1 else n*fp(n-1)) m
       = { application to m
        if m==0 then 1 else m * fp(m-1)

*If you really must*, you can also place code to the right of
"Bird-tracks". For example:

~~~~~

>       fact20 = fact 20
>       fact10 = fact 10

~~~~~


The definitions above this paragraph were themselves placed in
markdown code-quotes, but we dispense with markdown code quotes in
what follows so as to produce colourized Haskell code.


== Tabulating a function as a list

One way of avoiding recomputing the value of a function `f` from
nonnegative integer arguments is to make a lazy list of its values:

        tab = map f [0..]

then in place of `f n`, use `tab!!n`. Haskell's policy of lazy
evaluation means that it is only when `tab!!n` is evaluated for the
first time that `f n` gets evaluated; and that the `tab` only ever
gets to be as long as the highest index at which it is evaluated.
Moreover, for a given `n` the second and subsequent evaluations of
`tab n` no longer require the evaluation of `f n`.

Thus the cost of computing `f` at `n` can be transformed to the
cost of looking up the `n`th value in the list: $n$. Of course this
will be pointless if the cost of computing `f n` is less than this.
Nevertheless, we could "package" the idea of tabulation in the
definition:

>       tabular:: (Int->a) -> (Int->a)
>       tabular f = fun where fun n = tab!!n
>                             tab   = map f [0..]

If we define `factorial` by the usual "naturally recursive"
specification; and define

        fact = tabular factorial

then for any specific `n`, the cost of invoking a second and
subsequent 

        fact n

is the cost of the lookup, not the cost of the recursive computations
of `factorial n`.

But we can do better. Suppose `f` is known to satisfy the equation

     f = fix generator

Then from the definition of `tabular` we see

       tabular f
     = fun
       where fun n = tab!!n
             tab   = map (fix generator) [0..]


In principle we could have defined `tabular` this way, too. But we
have made things more complicated by invoking `fix`, and using both
`f` and its generator.

It is a remarkable fact that given the generator alone we can
dispense with `fix`, as well as computing `fun`; for recalling that

       fix generator
     = { fix definition 
       generator(fix generator)
     = { definition of fun
       generator fun

we define 
\begin{code}
        tabulate:: ((Int->a) -> (Int->a)) -> Int -> a
        tabulate generator = fun
                             where fun n = tab!!n
                                   tab   = map (generator fun) [0..]
\end{code}

Now the function `tabulate g` is the same as the function `fix g`, because

          tabulate g i
        =
          fun i   where ...
        =
          tab!!i  where ...
        =
          g fun i
        = { fix g = g fun
          fix g i

Notice that if we were to tabulate into an array not a list, `tab!!n`
would take constant time, not  `O(n)` time. The only problematic
question might be that of being able to build "flexible" arrays,
but I imagine this has been solved in well-performing Haskell
implementation.

== Peformance

To derive empirical proof of speedup we shall consider the fibonacci
function, whose naturally recursive implementation costs $O(n^2)$

\begin{code}
        recfib n = if n==0 then 0 else
                   if n==1 then 1 else recfib(n-1)+recfib(n-2)
                   
        tabfib   = tabulate (\ fib n -> if n==0 then 0 else
                                        if n==1 then 1 else fib(n-1)+fib(n-2))
\end{code}

The tabulated implementation is considerably more efficient than
the recursive implementation not just because the function has been
tabulated, but because at least one of the evaluations of `fib(n-2)`
that arise during the computation of `fib(n)` for $n>1$ has already
been tabulated.

The following comparisons were done using the `hugs` Haskell
interpreter that permits space and time performance to be measured.


Tabulation yields a striking performance increase from the start

     >  recfib 20
     6765
     (351260 reductions, 543493 cells)
     
     >  tabfib 20
     6765
     (8601 reductions, 10892 cells)
     
     
Using the same table subsequently enhances performance even more

     >  tabfib 20
     6765
     (417 reductions, 531 cells)
     
     >  tabfib 25
     75025
     (4932 reductions, 6242 cells)
     
     >  tabfib 25
     75025
     (512 reductions, 652 cells)
     
     >  tabfib 5
     5
     (132 reductions, 168 cells)
     
     >  recfib 25
     75025
     (3895523 reductions, 6027470 cells, 6 garbage collections)

Using the fixpoint is of comparable efficiency to using the recursive definition

     >  (fix (\ fib n -> if n==0 then 0 else
     >                   if n==1 then 1 else fib(n-1)+fib (n-2))) 20 
     6765
     (311660 reductions, 362218 cells)
     
