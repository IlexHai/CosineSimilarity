#' CosineSimilarity: Efficient Cosine Similarity Calculations with Rcpp
#'
#' This package provides efficient cosine similarity calculations for vectors 
#' and matrices using Rcpp. It includes both Rcpp and pure R implementations
#' for performance comparison, with comprehensive benchmarking and correctness
#' verification.
#'
#' The main functions include:
#' \itemize{
#'   \item \code{\link{cosine_similarity_rcpp}}: Rcpp version for vectors
#'   \item \code{\link{cosine_similarity_matrix_rcpp}}: Rcpp version for matrices  
#'   \item \code{\link{cosine_similarity_r}}: Pure R version for vectors
#'   \item \code{\link{cosine_similarity_matrix_r}}: Pure R version for matrices
#'   \item \code{\link{benchmark_cosine_similarity}}: Performance comparison
#' }
#'
#' @keywords internal
#' @useDynLib CosineSimilarity, .registration = TRUE
#' @importFrom Rcpp evalCpp
"_PACKAGE"