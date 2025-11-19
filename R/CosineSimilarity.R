#' Compute Cosine Similarity between Two Vectors (Rcpp)
#'
#' Calculates the cosine similarity between two numeric vectors using Rcpp.
#'
#' @param x,y Numeric vectors of the same length
#' @return Cosine similarity value x and y, value between -1 and 1
#' @export
#' @examples
#' x <- c(1, 2, 3)
#' y <- c(4, 5, 6)
#' cosine_similarity_rcpp(x, y)
cosine_similarity_rcpp <- function(x, y) {
  .Call(`_CosineSimilarity_cosine_similarity_cpp`, x, y)
}

#' Compute Cosine Similarity Matrix (Rcpp)
#'
#' Calculates pairwise cosine similarity matrix for rows of a matrix using Rcpp.
#'
#' @param mat A numeric matrix where rows represent observations
#' @return A symmetric matrix of cosine similarities, with diagnal elements be 1
#' @export
#' @examples
#' mat <- matrix(rnorm(12), nrow = 4, ncol = 3)
#' cosine_similarity_matrix_rcpp(mat)
cosine_similarity_matrix_rcpp <- function(mat) {
  .Call(`_CosineSimilarity_cosine_similarity_matrix_cpp`, mat)
}

#' Compute Cosine Similarity between Two Vectors (Pure R)
#'
#' Pure R implementation for comparison with Rcpp version.
#'
#' @param x,y Numeric vectors of the same length
#' @return Cosine similarity between x and y, value between -1 and 1
#' @export
#' @examples
#' x <- c(1, 2, 3)
#' y <- c(4, 5, 6)
#' cosine_similarity_r(x, y)
cosine_similarity_r <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Vectors must have the same length")
  }
  
  dot_product <- sum(x * y)
  norm_x <- sqrt(sum(x^2))
  norm_y <- sqrt(sum(y^2))
  
  if (norm_x == 0 || norm_y == 0) {
    return(0)
  }
  
  dot_product / (norm_x * norm_y)
}

#' Compute Cosine Similarity Matrix (Pure R)
#'
#' Pure R implementation for comparison with Rcpp version.
#'
#' @param mat A numeric matrix where rows represent observations
#' @return A symmetric matrix of cosine similarities, with diagnal elements be 1
#' @export
#' @examples
#' mat <- matrix(rnorm(12), nrow = 4, ncol = 3)
#' cosine_similarity_matrix_r(mat)
cosine_similarity_matrix_r <- function(mat) {
  n_rows <- nrow(mat)
  result <- matrix(0, n_rows, n_rows)
  
  for (i in 1:n_rows) {
    result[i, i] <- 1
    for (j in (i + 1):n_rows) {
      if (j <= n_rows) {
        similarity <- cosine_similarity_r(mat[i, ], mat[j, ])
        result[i, j] <- similarity
        result[j, i] <- similarity
      }
    }
  }
  
  return(result)
}

#' Benchmark Cosine Similarity Performance
#'
#' Comprehensive performance comparison between Rcpp and pure R implementations.
#' This function generates test data and runs timing comparisons to demonstrate
#' the efficiency improvements of the Rcpp implementation.
#'
#' @param sizes Vector of matrix sizes to test (number of rows)
#' @param n_cols Number of columns in test matrices (default: 30)
#' @param n_times Number of benchmark iterations (default: 10)
#' @return List containing benchmark results and correctness checks for each size
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("bench", quietly = TRUE)) {
#'   # Quick benchmark with small matrices
#'   results <- benchmark_cosine_similarity(c(50, 100), n_cols = 20, n_times = 5)
#'   
#'   # Print results for each size
#'   for (size in names(results)) {
#'     cat("Matrix size:", results[[size]]$matrix_size, "\n")
#'     print(results[[size]]$benchmark)
#'     #'     cat("Correctness:", results[[size]]$correctness, "\n\n")
#'   }
#' }
#' }
#'
#' @seealso
#' \code{\link{cosine_similarity_matrix_rcpp}} for the Rcpp implementation
#' \code{\link{cosine_similarity_matrix_r}} for the R implementation
benchmark_cosine_similarity <- function(sizes = c(50, 100, 200), n_cols = 30, n_times = 10) {
  # nocov start
  if (!requireNamespace("bench", quietly = TRUE)) {
    stop("Please install the 'bench' package: install.packages('bench')")
  }
  # nocov end
  
  results <- list()
  
  for (size in sizes) {
    # Generate test data
    set.seed(42)
    test_mat <- matrix(rnorm(size * n_cols), nrow = size, ncol = n_cols)
    # Run performance test
    bench_result <- bench::mark(
      Rcpp_method = cosine_similarity_matrix_rcpp(test_mat),
      R_method = cosine_similarity_matrix_r(test_mat),
      iterations = n_times,
      check = FALSE
    )
    
    # Verify result consistency
    result_rcpp <- cosine_similarity_matrix_rcpp(test_mat)
    result_r <- cosine_similarity_matrix_r(test_mat)
    
    # Check correctness
    correctness_check <- all.equal(result_rcpp, result_r, tolerance = 1e-10)
    
    results[[as.character(size)]] <- list(
      benchmark = bench_result,
      correctness = correctness_check,
      matrix_size = paste0(size, "x", n_cols)
    )
  }
  return(results)
}