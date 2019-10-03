#' Creates similarity matrices based on an input matrix.
#'
#' Takes an input matrix with regions in rows and units in columns,
#' where elements are a binary incidence (fx RCA). Calculates the
#' one of three possible similarity-metrics: proximity, Association
#' Strength, or Jaccard coefficient. Returns a unit x unit matrix
#' with pairwise similarity values as elements.
#'
#' @param mat Binary matrix that contains regions in rows, units in columns and
#' presence/incidence in elements.
#' @param method String that lists the method to calculate similarity. Options
#' are: "proximty", "association", or jaccard". Only takes one argument. For
#' more similarity-values, run function again.
#' @param diag0 Logical. Decides whether diagonal in rel_mat should be set to 0.
#'
#' @return rel_mat Unit x unit matrix that contains pair-wise similarity
#' value is elements. The diagonal is set to 0.
#'
#' @export

get_similarity <- function (mat, method, diag0 = TRUE) {

    method <- tolower(method)

  # test for input type = matrix
  if (!is.matrix(mat)) {
    stop("`mat` is not a matrix. Stopping.")
  }

# test for NA values
    if (sum(is.na(mat)) != 0) {
        stop("`mat` contains NA values. Stopping.")
    }

  # test for binary
    if (sum(mat == 1 | mat == 0) != length(mat)) {
        stop("`mat` seems not to be binary. Stopping.")
    }

  # test for colnames and rownames
    if (is.null(colnames(mat)) | is.null(rownames(mat))) {
        stop("`mat` is missing either colnames or rownames. Stopping")
    }


# test for missing method
    if (missing(method)) stop("`method` argument is missing. Try again.")
    if (length(method) != 1) stop("`method` argument has to be of length 1. Try again.")
    if (!(method %in% c("jaccard", "proximity", "association"))) stop("`method` does not match one of the possible methods. Options are `jaccard`, `proximity`, or `association`.")


  # JACCARD --------------------------------------------------------

  if ('jaccard' %in% method){
    # General method: Jaccard similariy between two sets, A and B is found through:
    # (A insection B) / (A union B); this can be re-written as: (A intersection B) / (A + B - A intersection B)
    # I first compute cooccurrance matrix between rows in input matrix. This represents A intersection B.
    # Second, the colsums of input matrix gives a vector of full sets A, B, ...
    # These are repeated row and col-wise in two matrices, representing A + B)

    cooc_ij <- t(mat) %*% mat # co-occurrence matrix
    set_i <- colSums(mat)
    set_j <- colSums(mat)

    set_i <- matrix(set_i, nrow = length(set_i), ncol = length(set_i), byrow = TRUE) # col 1 is A, col 2 is B, etc
    set_j <- matrix(set_j, nrow = length(set_j), ncol = length(set_j), byrow = FALSE) # row 1 is A, row 2 is B, etc.

    jaccard_mat <- (cooc_ij) / (set_i + set_j - cooc_ij) # calculate jaccard similarity

    if (sum(is.na(jaccard_mat)) == sum((set_i + set_j - cooc_ij) == 0)) {
      jaccard_mat[is.na(jaccard_mat)] <- 0
    } else {
      stop("NAs can be introduced if there are 0s in the denominator in division.
           However, there is a different number of NA values in the Jaccard-matrix
           than in the denominator of the intersection-unity division.
           Somthing is wrong. Stopping.")
    }

    rel_mat <- jaccard_mat

    ## values are between 0 and 1
  if (sum(rel_mat > 1 | rel_mat < 0) != 0) {
    stop(paste0("Values of similarity calculations (", method, ") are not between 0 to 1. Somthing is off"))
  }

  }

  # ASSOCIATION STRENGTH -------------------------------------------

  if ("association" %in% method) {

    # General method: Association Strength between two sets, A and B is found through:
    # (A insection B) / AB
    # I first compute cooccurrance matrix between rows in input matrix. This represents A intersection B.
    # Second, the colsums of input matrix gives a vector of full sets A, B, ...
    # These are repeated row and col-wise in two matrices, representing A * B)

    cooc_ij <- t(mat) %*% mat # cooccurance matrix
    set_i <- colSums(mat)
    set_j <- colSums(mat)

    set_i <- matrix(set_i, nrow = length(set_i), ncol = length(set_i), byrow = TRUE) # col 1 is A, col 2 is B, etc
    set_j <- matrix(set_j, nrow = length(set_j), ncol = length(set_j), byrow = FALSE) # row 1 is A, row 2 is B, etc.

    assoc_mat <- (cooc_ij) / (set_i * set_j) # calculate association strength


    if(sum(is.na(assoc_mat)) == sum((set_i * set_j) == 0)) {
      assoc_mat[is.na(assoc_mat)] <- 0
    } else {
      stop("NAs can be introduced if there are 0s in the denominator in
           division. However, there is a different number of NA values in
           the assoc-matrix than in the denominator of the formula.
           Somthing is wrong. Stopping function.")
    }

      rel_mat <- assoc_mat

  }


  # PROXIMITY ------------------------------------------------------
  if ('proximity' %in% method) {
    # General method: Proximity-score between two sets, A and B is found through:
    # (A insection B) / max(A, B)
    # I first compute cooccurrance matrix between cols in the input matrix. This represents A intersection B.
    # Second, the colsums of the input matrix gives a vector of full sets A, B,
    # These are repeated row and col-wise in two matrices, representing A and B.
    # I then create af max_mat that carries that highest value between each corresponding element in the matrix.
    # This means that new_mat[i, j] is highest of the values between set_i and set_j

    cooc_ij <- t(mat) %*% mat
    set_i <- colSums(mat)
    set_j <- colSums(mat)

    max_mat <- matrix(0L, nrow = length(set_i), ncol = length(set_i)) # empty matrix to populate.

    set_i <- matrix(set_i, nrow = length(set_i), ncol = length(set_i), byrow = TRUE) # col 1 is A, col 2 is B, etc
    set_j <- matrix(set_j, nrow = length(set_j), ncol = length(set_j), byrow = FALSE) # row 1 is A, row 2 is B, etc.


    for(i in 1:nrow(set_i)){
      for(j in 1:ncol(set_j)){
        max_mat[i, j] <- max(set_i[i, j], set_j[i, j])
      }
    }

    prox_mat <- cooc_ij / max_mat

    if (sum(is.na(prox_mat)) == sum(max_mat == 0)) {
      prox_mat[is.na(prox_mat)] <- 0
    } else {
      stop("NAs can be introduced if there are 0s in the denominator in
           division. However, there is a different number of NA values in
           the prox-matrix than in the denominator of the formula.
           Somthing is wrong. Stopping function.")
    }

    rel_mat <- prox_mat

    ## values are between 0 and 1
  if (sum(rel_mat > 1 | rel_mat < 0) != 0) {
    stop(paste0("Values of similarity calculations (", method, ") are not between 0 to 1. Somthing is off"))
  }

  }

  # Diagonal in rel_mat is similarity to itself, which is not meaningful.
  # In addtion, it is a problem when calculating density. Setting diag to 0.
  if (diag0 == TRUE) {
  diag(rel_mat) <- 0
  }

  # Return values
  return(rel_mat)

}


