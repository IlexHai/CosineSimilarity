test_that("cosine similarity calculation correctness verification", {
  # Test identical vectors
  expect_equal(cosine_similarity_rcpp(c(1, 1), c(1, 1)), 1)
  expect_equal(cosine_similarity_r(c(1, 1), c(1, 1)), 1)
  
  # Test orthogonal vectors
  expect_equal(cosine_similarity_rcpp(c(1, 0), c(0, 1)), 0)
  expect_equal(cosine_similarity_r(c(1, 0), c(0, 1)), 0)
  
  # Test opposite vectors
  expect_equal(cosine_similarity_rcpp(c(1, 1), c(-1, -1)), -1)
  expect_equal(cosine_similarity_r(c(1, 1), c(-1, -1)), -1)
})

test_that("Rcpp and R implementations gives same results", {
  set.seed(42)
  
  test_sizes <- list(c(5, 3), c(10, 5), c(20, 10))
  
  for (size in test_sizes) {
    n_rows <- size[1]
    n_cols <- size[2]
    
    test_mat <- matrix(rnorm(n_rows * n_cols), nrow = n_rows, ncol = n_cols)
    
    result_rcpp <- cosine_similarity_matrix_rcpp(test_mat)
    result_r <- cosine_similarity_matrix_r(test_mat)
    
    expect_equal(result_rcpp, result_r, tolerance = 1e-10,
                 info = paste("Matrix size:", n_rows, "x", n_cols))
  }
})

test_that("similarity matrices have correct properties", {
  set.seed(42)
  test_mat <- matrix(rnorm(25), nrow = 5, ncol = 5)
  
  result_rcpp <- cosine_similarity_matrix_rcpp(test_mat)
  result_r <- cosine_similarity_matrix_r(test_mat)
  
  # Check symmetry
  expect_equal(result_rcpp, t(result_rcpp))
  expect_equal(result_r, t(result_r))
  
  # Check diagonal is 1
  expect_equal(diag(result_rcpp), rep(1, 5))
  expect_equal(diag(result_r), rep(1, 5))
  
  # Check values are in [-1, 1]
  expect_true(all(result_rcpp >= -1 - 1e-10 & result_rcpp <= 1 + 1e-10))
  expect_true(all(result_r >= -1 - 1e-10 & result_r <= 1 + 1e-10))
})

test_that("edge cases are handled correctly", {
  # Zero vectors
  expect_equal(cosine_similarity_rcpp(c(0, 0), c(0, 0)), 0)
  expect_equal(cosine_similarity_r(c(0, 0), c(0, 0)), 0)
  
  # Single element vectors
  expect_equal(cosine_similarity_rcpp(5, 3), 1)
  expect_equal(cosine_similarity_r(5, 3), 1)
  
  # Different length vectors should error
  expect_error(cosine_similarity_rcpp(c(1, 2), c(1, 2, 3)))
  expect_error(cosine_similarity_r(c(1, 2), c(1, 2, 3)))
})

test_that("benchmark function works correctly", {
  skip_if_not_installed("bench")
  
  # Test that benchmark function returns expected structure
  results <- benchmark_cosine_similarity(c(10, 20), n_cols = 5, n_times = 2)
  
  expect_type(results, "list")
  expect_length(results, 2)
  expect_true(all(c("10", "20") %in% names(results)))
  
  # Check each result has expected components
  for (size in names(results)) {
    expect_true("benchmark" %in% names(results[[size]]))
    expect_true("correctness" %in% names(results[[size]]))
    expect_true("matrix_size" %in% names(results[[size]]))
  }
})