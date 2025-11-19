#CosineSimilarity

An R package provides efficient cosine similarity calculations between vectors and matrix using Rcpp, 
with benchmarking to Pure R and correctness verification

## Features

-Fast calculation via Rcpp

-Pure R implementation for correctness comparison and benchmarking

-Includes simulated dataset examples

## Installation

You can install the package from GitHub:

```r
# Install devtools if you haven't already
# install.packages("devtools")

devtools::install_github("IlexHai/CosineSimilarity")
```

## Examples

Vector cosine similarity
```{r}
library(CosineSimilarity)

x <- c(1, 2, 3)
y <- c(2, 4, 6)

cosine_similarity_rcpp(x, y)
#> 1
```

Matrix cosine similarity
```{r}
library(CosineSimilarity)

mat <- matrix(rnorm(20), nrow = 5)

cosine_similarity_matrix_rcpp(mat)
```

## Correcness Verification
```{r}
set.seed(42)
x <- rnorm(1000)
y <- rnorm(1000)

all.equal(
  cosine_similarity_r(x, y),
  cosine_similarity_rcpp(x, y)
)
#> TRUE
```

## Efficiency Benchmark
```{r}
library(bench)

x <- rnorm(1000000)
y <- rnorm(1000000)

bench::mark(
  R = cosine_similarity_r(x, y),
  Rcpp = cosine_similarity_rcpp(x, y),
  iterations = 5
)
```

Matrix benchmark
```{r}
mat <- matrix(rnorm(200 * 50), nrow = 200)

bench::mark(
  R = cosine_similarity_matrix_r(mat),
  Rcpp = cosine_similarity_matrix_rcpp(mat),
  iterations = 3
)
```

## Documentation

For detailed examples and performance analysis, please see the package vignette

## License

This package is licensed under the MIT license

## Author

Haoyang Zhou
University of Michigan