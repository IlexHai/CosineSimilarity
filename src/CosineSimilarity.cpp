#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

//calculate the cosine similarity between two vectors
// [[Rcpp::export]]
double cosine_similarity_cpp(NumericVector x, NumericVector y){
  int n = x.size();
  if(n != y.size()){
    stop("Vectors must have same length to calculate cosine similarity");
  }
  
  double dot_product = 0.0;
  double norm_x = 0.0;
  double norm_y = 0.0;
  
  for (int i = 0; i < n; i++) {
    dot_product += x[i] * y[i];
    norm_x += x[i] * x[i];
    norm_y += y[i] * y[i];
  }
  
  if (norm_x == 0.0 || norm_y == 0.0) {
    return 0.0;
  }
  
  return dot_product / (std::sqrt(norm_x) * std::sqrt(norm_y));
}

//calculate the cosine similarity between two matrix
// [[Rcpp::export]]
NumericMatrix cosine_similarity_matrix_cpp(NumericMatrix mat) {
  int n_rows = mat.nrow();
  int n_cols = mat.ncol();
  NumericMatrix result(n_rows, n_rows);
  
  //calculate norm for each row
  NumericVector row_norms(n_rows);
  for (int i = 0; i < n_rows; i++) {
    double norm_sq = 0.0;
    for (int j = 0; j < n_cols; j++) {
      norm_sq += mat(i, j) * mat(i, j);
    }
    row_norms[i] = (norm_sq > 0.0) ? std::sqrt(norm_sq) : 1.0;
  }
  
  //calculate the similarity matrix
  for (int i = 0; i < n_rows; i++) {
    //set the diagnal as 1
    result(i, i) = 1.0;
    
    for (int j = i + 1; j < n_rows; j++) {
      double dot_product = 0.0;
      for (int k = 0; k < n_cols; k++) {
        dot_product += mat(i, k) * mat(j, k);
      }
      
      double similarity = dot_product / (row_norms[i] * row_norms[j]);
      result(i, j) = similarity;
      result(j, i) = similarity;
    }
  }
  
  return result;
}
