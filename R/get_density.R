#' Calculates density for regions around units.
#'
#' Takes an incidence matrix with regions in rows and units in columns,
#' where elements are a binary incidence (RCA), and unit-unit similarity
#' matrix. Calculates the density around each nit for each region, based on
#' units present in the region in the incidence matrix. Outputs a region x unit
#' matrix with density in elements.
#'
#' General method: density of a region around a unit U is defined as the
#' sum of all similarity-values between the units contained in the region
#' (the 1s in the incidence matrix) and the unit U, divided by the sum of
#' Us similarity-values to all other individual units. By taking the dot product
#' between the incidence matrix and the relatedness matrix, we get a matrix
#' the shape of the incidence matrix, but for each unit (column) the value
#' is the sum of contained prducts relatedness to that product.
#' This matrix is divided by the columnsums of the relatedness_matrix
#' (which is the sum of all global unit relatedness scores).
#'
#' @param rca_mat Binary matrix that contains regions in rows, units in columns and
#' presence/incidence in elements.
#' @param sim_mat Unit x unit matrix with similarity-values in elements
#' @param binary logical. If false, a non-binary RCA matrix is allowed as input.
#' This means that RCA is used as weights to similarity values.
#'
#' @return Region x unit matrix that contains density for regions around units
#' in elements.
#'
#' @export

get_density <- function (rca_mat, sim_mat, binary = TRUE) {

  # test that each inputs is a matrix
  if (!is.matrix(rca_mat) | !is.matrix(sim_mat)) {
    stop("At least one input is not a matrix. Stopping.")
  }

  # test that mats does not contain NA values
    if (!(sum(is.na(rca_mat)) == 0 & sum(is.na(sim_mat)) == 0)) {
        stop("One of either `rca_mat` or `sim_mat` contains NA values")
    }

  # test that rca_mat is binary
  if (sum(rca_mat != 0 & rca_mat != 1) != 0) {
    stop("rca_mat is not binary. Stopping.")
  }

  # test that dimensions fit
  if (!(ncol(rca_mat) == ncol(sim_mat))) {
    stop("The number of columns in `rca_mat` does not match the number of columns in `sim_mat`.")
  }

# test that sim mat is square
    if (ncol(sim_mat) != nrow(sim_mat)) {
        stop("`sim_mat` does not seem to be a square matrix.")
    }

   # test that rownames and colnames exists
    if (length(colnames(rca_mat)) != ncol(rca_mat)) {
        stop("colnames seems to be missing on a least one of the input matrices.")
    }

   # test that rca_mat and sim_mat has the same colnames
    if (!identical(colnames(rca_mat), colnames(sim_mat))) {
        stop("The colnames of `rca_mat` and `sim_mat` does not seem to be identical (or in the right order).")
    }

  # test for binary
  if (binary == TRUE) {
    if (sum(rca_mat == 0 | rca_mat == 1) != length(rca_mat)) {
      stop("It seems that `rca_mat` is not binary. See `binary` argument.")
    }
  }

  mat <- rca_mat
  rel_mat <- sim_mat
  diag(rel_mat) <- 0 # set diagonal to 0, so that colSums does not include relatedness to self.
  rel_sums <- colSums(rel_mat) # vector where each element is the sum of all relatedness scores from the unit to other units.

    # for some reason mat / vector is col-wise. t(t(mat) / vector) is rowwise (has to do with turning vector)
  density_mat <- t(t(mat %*% rel_mat) / rel_sums)


  if(sum(is.na(density_mat)) > 0) stop("density_mat contains NA values.")

  ## density_mat values are between 0 and 1
   if (sum(density_mat > 1 | density_mat < 0) != 0) {
     stop("Values of density calculations are not inside 0 to 1. Something is off.")
   }

  return(density_mat)

}



