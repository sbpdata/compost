#' Find highest similarity value in region
#'
#' In the rca matrix, each region (row) has a number of
#' units (col) with the value 1. From these units, the function
#' finds the highest value of similarity to each unit.
#'
#' @param rca_mat binary rca matrix with region rows, units in columns
#' @param sim_mat similarity matrix with units on both dimensions.
#'
#' @return matrix with regions in rows, units in columns. The elements are
#' the highest similarity-value in the region to the given unit (column).
#'
#' @export

get_closest_sim <- function(rca_mat, sim_mat) {

    ## Test for matrix type input
    if (!is.matrix(rca_mat) | !is.matrix(sim_mat)) {
        stop("One of the input matrices is not a matrix. Try again.")
    }

    ## Test for dimensions
    if (ncol(rca_mat) != ncol(sim_mat)) {
        stop("The rca_mat does not have the same number of columns as the sim_mat. Something is off. Try again.")
    }

  ## Check for ordering of columns
  if (!identical(colnames(rca_mat), colnames(sim_mat))) {
    stop("The ordering of columns in rca_mat and sim_mat are not the same. Stopping.")
  }

  ## Save row and column names
  names_rows <- rownames(rca_mat)
  names_cols <- colnames(sim_mat)

  ## Test for square sim matrix
  if (ncol(sim_mat) != nrow(sim_mat)) {
    stop("sim mat is not a square matrix. Try again.")
  }

  ## Test for NA values
  if(sum(is.na(rca_mat)) != 0 | sum(is.na(sim_mat)) != 0) {
    stop("One or more of the input matrices contain NA values. This is wrong. Stopping.")
  }

  ## Test that rca_mat is binary
  if (sum(rca_mat != 1 & rca_mat != 0) != 0) {
    stop("rca_mat is not binary. Stopping.")
  }
  # run function
  closest_similarity_mat <- matrix(NA, nrow = nrow(rca_mat), ncol = ncol(sim_mat))

  for(j in 1:nrow(rca_mat)) {
    for(i in 1:ncol(rca_mat)) {

      region_sim <- rca_mat[j, ] * sim_mat[i, ]
      closest_similarity_mat[j, i] <- max(region_sim)

    }
  }

  colnames(closest_similarity_mat) <- names_cols
  rownames(closest_similarity_mat) <- names_rows

  return(closest_similarity_mat)
}
