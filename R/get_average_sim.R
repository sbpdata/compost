#' Find average similarity value in region
#'
#' In the rca matrix, each region (row) has a number of
#' units (col) with the value 1. From these units, the function
#' finds the average value of similarity to each unit.
#'
#' @param rca_mat binary rca matrix with region rows, units in columns
#' @param sim_mat similarity matrix with units on both dimensions.
#' @param exclude_0s logical If TRUE, each 0 in the binary RCA matrix
#' is converted to NAs. This means that units not present in a region
#' does not pull the average down. That is, it is not the average value,
#' but the average value of units presents in the region. This pulls
#' non-diverse regions up.
#'
#' @return matrix with regions in rows, units in columns. The elements are
#' the average similarity-value in the region to the given unit (column).
#'
#' @export


get_average_sim <- function(rca_mat, sim_mat, exclude_0s = FALSE) {

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
      stop("The ordering of columns (by colnames) in rca_mat and sim_mat are not the same. This means that the element-wise multiplication of the vectors does not multiply the correct elements to each other. Stopping.")
    }

    ## Save row and column names
    names_rows <- rownames(rca_mat)
    names_cols <- colnames(sim_mat)


    ## Test for NA values
    if (sum(is.na(rca_mat)) != 0 | sum(is.na(sim_mat)) != 0) {
      stop("One or more of the input matrices contain NA values. This is wrong. Stopping.")
    }

    ## Test for non-binary rca_mat
    if (sum(rca_mat != 0 & rca_mat != 1) != 0) {
      stop("The rca_mat does not seem to be binary. Use get_weighted_average_sim for using RCA as weights.")
    }

    ## define empty return mat
    average_similarity_mat <- matrix(NA, ncol = ncol(rca_mat), nrow = nrow(rca_mat))
    rownames(average_similarity_mat) <- names_rows
    colnames(average_similarity_mat) <- names_cols

    ## zeroes allowed
    if (exclude_0s == FALSE) {

      for(j in 1:nrow(rca_mat)) {
        for(i in 1:ncol(rca_mat)) {

          region_sim <- rca_mat[j, ] * sim_mat[i, ]
          average_similarity_mat[j, i] <- mean(region_sim, na.rm = FALSE)
        }
      }

    }

    ## zeroes not allowed
    if (exclude_0s == TRUE) {
      rca_mat <- ifelse(rca_mat == 0, NA, rca_mat)

      for(j in 1:nrow(rca_mat)) {
        for(i in 1:ncol(rca_mat)) {

          region_sim <- rca_mat[j, ] * sim_mat[i, ]
          average_similarity_mat[j, i] <- mean(region_sim, na.rm = TRUE)
        }
      }

    }

    return(average_similarity_mat)
  }
