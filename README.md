# NonParamLines

<!-- badges: start -->
[![R-CMD-check](https://github.com/nhart985/NonParamLines/workflows/R-CMD-check/badge.svg)](https://github.com/nhart985/NonParamLines/actions)

[![codecov](https://codecov.io/gh/nhart985/NonParamLines/branch/main/graph/badge.svg?token=HY81882KXH)](https://codecov.io/gh/nhart985/NonParamLines)
<!-- badges: end -->


The NonParamLines package provides implementations of two popular nonparametric regression estimators: LOESS (Cleveland, 1979) and Friedman's running lines estimator (Friedman, 1984). The two main functions in this package are loess_fit() and supsmu_fit(), which generate response predictions given the values of a continuous predictor variable. See help pages and vignettes for details and examples. 

The R implementations in the NonParamLines package are slower than existing implementations in the stats package, which use Fortran and C++. This is expected since each algorithm require a loop, but all of the code within the loop is vectorized and efficient. See source code for details. 

REFERENCES

Cleveland, W.S. (1979) Robust Locally Weighted Regression and Smoothing Scatterplots, Journal of the American Statistical Association, 74:368, 829-836, DOI: 10.1080/01621459.1979.10481038

Friedman, J.H. (1984) A variable span scatterplot smoother. Laboratory for Computational Statistics, Stanford University Technical Report No. 5.

Luedicke, J. (2021) Friedman’s Super Smoother. Available at: http://fmwww.bc.edu/RePEc/bocode/s/supsmooth_doc.pdf (Accessed November, 2021)










