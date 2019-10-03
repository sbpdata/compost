#' Find average similarity value in region, weighted by RCA
#'
#' In the rca matrix, each region (row) has a number of
#' units (col) with the value 1. From these units, the function
#' finds the average value of similarity to each unit, weighted by the
#' RCA value in the rca_mat.
#'
#' @param rca_mat non-binary rca matrix with region rows, units in columns
#' @param sim_mat similarity matrix with units on both dimensions.
#'
#' @return matrix with regions in rows, units in columns. The elements are
#' the weighted average similarity-value in the region (row) to the given unit (column).
#'
#' @export

get_weighted_average_sim <- function(rca_mat, sim_mat) {

  ## Test for matrix type input
  if (!is.matrix(rca_mat) | !is.matrix(sim_mat)) {
    stop("One of the input matrices is not a matrix. Try again.")
  }

  ## Test for dimensions
  if (ncol(rca_mat) != ncol(sim_mat)) {
    stop("The rca_mat does not have the same number of columns as the sim_mat. Something is off. Try again.")
  }

  ## Test for square sim matrix
  if (ncol(sim_mat) != nrow(sim_mat)) {
    stop("sim mat is not a square matrix. Try again.")
  }

  ## Check for ordering of columns
  if (!identical(colnames(rca_mat), colnames(sim_mat))) {
    stop("The ordering of columns in rca_mat and sim_mat are not the same. This means that the element-wise multiplication of the vectors does not multiply the correct elements to each other. Stopping.")
  }

  ## Save row and column names
  names_rows <- rownames(rca_mat)
  names_cols <- colnames(sim_mat)

  ## Test for NA values
  if (sum(is.na(rca_mat)) != 0 | sum(is.na(sim_mat)) != 0) {
    stop("One or more of the input matrices contain NA values. This is not allowed. Stopping.")
  }

  ## Test for non-binary rca_mat
  if (sum(rca_mat == 0 | rca_mat == 1) == length(rca_mat)) {
    stop("The rca_mat seems to be binary. This is not allowed. Use get_average_sim for getting unweighted average.")
  }

  ## define empty return mat
  w_average_similarity_mat <- matrix(NA, ncol = ncol(rca_mat), nrow = nrow(rca_mat))
  rownames(w_average_similarity_mat) <- names_rows
  colnames(w_average_similarity_mat) <- names_cols

  ## zeroes are allowed (it does not make sense to privilege regions with 0 rca over 0.1).
    for(j in 1:nrow(rca_mat)) {
      for(i in 1:ncol(rca_mat)) {

        region_sim <- rca_mat[j, ] * sim_mat[i, ]
        w_average_similarity_mat[j, i] <- mean(region_sim, na.rm = FALSE)

      }
    }

  return(w_average_similarity_mat)
}
