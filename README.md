# Short overview of salad

Salad is a package for Automatic Differentiation. Its name could stand for Simple And
Light Automatic Differentiation, but it stands well by itself (eat salad, salad is good).

Lots of efforts have been done to allow re-using with salad functions
written without automatic differentiation in mind. 

## Examples

We are going to illustrate some of the things salad can do by a series of short examples.

### A simple function

Consider for example the following function, $f(x) = \sin(x^2)$ :


``` r
f1 <- function(x) sin(x**2)
```

The value of its derivative for a given value of $x$. We just need to apply 
the function to a dual number created by `salad::dual`:


``` r
x <- dual(pi)
y <- f1(x)
y 
```

```
## [1] -0.4303012
## [has derivatives in x1]
```

``` r
d(y)
```

```
## $x1
## [1] -5.671739
```

And it works with vectors too:


``` r
x <- dual(c(0, 1, sqrt(pi)))
y <- f1(x)
y
```

```
## [1] 0.000000e+00 8.414710e-01 5.665539e-16
## [has derivatives in x1 x2 x3]
```

``` r
# get value and derivative
value(y)
```

```
## [1] 0.000000e+00 8.414710e-01 5.665539e-16
```

``` r
d(y)
```

```
## $x1
## [1] 0 0 0
## 
## $x2
## [1] 0.000000 1.080605 0.000000
## 
## $x3
## [1]  0.000000  0.000000 -3.544908
```

### Matrix arithmetic

A second example will be given by matrix arithmetic. First define a dual object from a matrix.


``` r
x <- dual( matrix( c(1, 2, 4, 7), 2, 2))
x
```

```
##      [,1] [,2]
## [1,]    1    4
## [2,]    2    7
## [has derivatives in x1.1 x2.1 x1.2 x2.2]
```

The default behavior of `dual` is to name the variables `x1.1`, `x1.2`, etc.


``` r
varnames(x)
```

```
## [1] "x1.1" "x2.1" "x1.2" "x2.2"
```

``` r
# derivative along x1.1
d(x, "x1.1")
```

```
## $x1.1
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    0
```

Methods have been defined in salad to handle matrix product:


``` r
y <- x %*% x
y
```

```
##      [,1] [,2]
## [1,]    9   32
## [2,]   16   57
## [has derivatives in x1.1 x2.1 x1.2 x2.2]
```

``` r
d(y, "x1.1")
```

```
## $x1.1
##      [,1] [,2]
## [1,]    2    4
## [2,]    2    0
```

The determinant can be computed as well: 


``` r
det(x)
```

```
## [1] -1
## [has derivatives in x1.1 x2.1 x1.2 x2.2]
```

``` r
d(det(x))
```

```
## $x1.1
## [1] 7
## 
## $x2.1
## [1] -4
## 
## $x1.2
## [1] -2
## 
## $x2.2
## [1] 1
```

And the inverse:


``` r
z <- solve(x)
z
```

```
##      [,1] [,2]
## [1,]   -7    4
## [2,]    2   -1
## [has derivatives in x1.1 x2.1 x1.2 x2.2]
```

``` r
d(z, "x1.1")
```

```
## $x1.1
##      [,1] [,2]
## [1,]  -49   28
## [2,]   14   -8
```

### Using `ifelse`, `apply` etc.

As a last example, consider this function, which does nothing very interesting, 
except using `ifelse`, `cbind`, `apply`, and `crossprod`:


``` r
f2 <- function(x) {
  a <- x**(1:2)
  b <- ifelse(a > 1, sin(a), 1 - a) 
  C <- crossprod( cbind(a,b) )
  apply(C, 2, function(x) sum(x^2))
}
```

It works ok:


``` r
# creating a dual number for x = 0.2
x <- dual(0.2)
y <- f2(x)
y
```

```
##          a          b 
## 0.04109312 2.47795712 
## [has derivatives in x1]
```

``` r
# get value and the derivative 
value(y)
```

```
##          a          b 
## 0.04109312 2.47795712
```

``` r
d(y)
```

```
## $x1
##          a          b 
##  0.4200448 -7.0116352
```

# What salad doesn't do well

You need to be aware of the following limitations of salad.

## Salad doesn't check variable names

Checking the variable along which the derivatives are defined
would have slowed the computations an awful lot. The consequence
is that if you don't take care of that yourselves, it may give
inconsistent results.

### Illustrating the problem

Let's define to dual numbers with derivatives along `x` and `y`.


``` r
a <- dual(c(1,2), dx = list("x" = c(1,1)))
b <- dual(c(2,1), dx = list("y" = c(2,1)))
```

It would be neat if `a + b` had a derivative along `x` and one along `y`. It doesn't.


``` r
a + b
```

```
## [1] 3 3
## [has derivatives in x]
```

``` r
d(a + b)
```

```
## $x
## [1] 3 2
```

### A possible solution 

A simple way to deal with this is to define `a` and `b` with the appropriate
list of derivatives.


``` r
a <-  dual(c(1,2), dx = list("x" = c(1,1), "y" = c(0,0)))
b <-  dual(c(2,1), dx = list("x" = c(0,0), "y" = c(2,1)))
```

It now works as intended:


``` r
a + b
```

```
## [1] 3 3
## [has derivatives in x y]
```

``` r
d(a + b)
```

```
## $x
## [1] 1 1
## 
## $y
## [1] 2 1
```

### Best (?) solution

My prefered solution is to first define a dual vector with the two variables `x` and `y`,
this can be done like this:


``` r
v <- dual( c(1,1), varnames = c("x", "y"))
v
```

```
## [1] 1 1
## [has derivatives in x y]
```

``` r
d(v)
```

```
## $x
## [1] 1 0
## 
## $y
## [1] 0 1
```

Equivalently, one could define `v` as `dual(c(x = 1, y = 1))`. Once this is done, you can
proceed as follows. First isolate the variables `x` and `y` :


``` r
x <- v[1]
y <- v[2]
```

Then create `a` with the wanted derivatives:

``` r
a <- c(x, x+1)
d(a)
```

```
## $x
## [1] 1 1
## 
## $y
## [1] 0 0
```

Same thing for `b`:

``` r
b <- c(2*y, y)
d(b)
```

```
## $x
## [1] 0 0
## 
## $y
## [1] 2 1
```

And now eveything works ok.

``` r
a + b
```

```
## [1] 3 3
## [has derivatives in x y]
```

``` r
d(a + b)
```

```
## $x
## [1] 1 1
## 
## $y
## [1] 2 1
```

**As a general advice, a computation should begin with a single `dual()` call,
creating all the variables along which one needs to derive.**
This should avoid all problems related to this limitation of salad.

## Beware of `as.vector` and `as.matrix`

The functions `as.vector` and `as.matrix` return base (constant) vector and matrix objects.


``` r
x <- dual( matrix( c(1, 2, 4, 7), 2, 2))
as.vector(x)
```

```
## Warning in as.vector(x, mode): Dropping derivatives in as.vector. See ?salad to change this behaviour
```

```
## [1] 1 2 4 7
```

This behavior can be changed using `salad(drop.derivatives = FALSE)`, but this may break
some things. The prefered way to changed the shape of an object is to use `dim<-' :


``` r
dim(x) <- NULL
x
```

```
## [1] 1 2 4 7
## [has derivatives in x1.1 x2.1 x1.2 x2.2]
```

You may need to rewrite partially some functions due to this behavior.

## Other caveats : `abs`, `max`, `min`

The derivative of `abs` has been defined to `sign`. It might not be a good idea:

``` r
x <- dual(0) + c(-1,0,1)
x
```

```
## [1] -1  0  1
## [has derivatives in x1]
```

``` r
d(x)
```

```
## $x1
## [1] 1 1 1
```

``` r
abs(x)
```

```
## [1] 1 0 1
## [has derivatives in x1]
```

``` r
d(abs(x))
```

```
## $x1
## [1] -1  0  1
```

Also, the derivative of `max` relies on `which.max`: it works well when there 
are no ties, that is, when it is well defined. In the presence of ties, it
is false.

When there are no ties, the result is correct:


``` r
y <- max( dual(c(1, 2)) )
y
```

```
## [1] 2
## [has derivatives in x1 x2]
```

``` r
d(y)
```

```
## $x1
## [1] 0
## 
## $x2
## [1] 1
```

But in presence of ties, the derivatives should be undefined.

``` r
y <- max( dual(c(2, 2)) )
y
```

```
## [1] 2
## [has derivatives in x1 x2]
```

``` r
d(y)
```

```
## $x1
## [1] 1
## 
## $x2
## [1] 0
```

# What salad does

It must be clear from the previous examples that salad can handle both vector and matrices.
In addition to the simple arithmetic operations, most mathematical functions have been 
defined in `salad`: trigonometic functions, hyperbolic trigonometric functions, etc 
(see the manual for an exhaustive list of functions of method). Many functions such
as `ifelse`, `apply`, `outer`, etc, have been defined.

In addition to the simple matrix arithmetic, salad can also handle `det` and `solve`.
Currently, matrix decomposition with `eigen` and `qr` are not implemented (but this may
change in the near future).

# Defining new derivation rules

Assume you're using salad to compute the derivative of a quadratic function:


``` r
f <- function(x) x**2 + x + 1
x <- dual(4)
f(x)
```

```
## [1] 21
## [has derivatives in x1]
```

``` r
d(f(x))
```

```
## $x1
## [1] 9
```

This works, but in the other hand, you know that this derivative is `2*x + 1`. You 
can tell salad about it with `dualFun1`. 


``` r
f1 <- dualFun1(f, \(x) 2*x + 1)
f1(x)
```

```
## [1] 21
## [has derivatives in x1]
```

``` r
d(f1(x))
```

```
## $x1
## [1] 9
```

This allows you to use special functions that salad can't handle; moreover, even
for simple functions like this one, it saves some time:


``` r
system.time( for(i in 1:500) f(x) )
```

```
##    user  system elapsed 
##   0.015   0.000   0.015
```

``` r
system.time( for(i in 1:500) f1(x) )
```

```
##    user  system elapsed 
##   0.006   0.000   0.006
```

It can thus be useful to define the derivatives of the some of the functions you
are using in this way.

# Contributing to salad

You may e-mail the author if for bug reports, feature requests,
or contributions. The source of the package is on [github](https://github.com/HervePerdry/salad).
 
