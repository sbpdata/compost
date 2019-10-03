#' Creates an incidence matrix from a region-unit intensity matrix using RCA
#'
#' Takes a matrix with regions in rows and units in columns, where elements are
#' intensity. This could be countries as regions, products as units and export
#' value as intensity. Calculates the revealed comparative advantage for each
#' region-unit pair. Returns the incidence matrix.
#'
#' @param mat Matrix that contains regions on rows, units on columns and
#' intensity value in elements.
#' @param binary logical TRUE returns a binarized RCA matrix, where
#' values of at least 1 = 1, values below 1 = 0.
#' @return Matrix that contains the revealed comparative
#' of region (rows) in unit (columns).
#'
#' @export

com_rca <- function (mat, binary = TRUE) {
  # input: mat is a region x unit matrix where elements are intensity values
  # output: region x unit matrix where elements are rca values

  if (!is.matrix(mat)){
    stop("Input `mat` is not a matrix. Stopping.")
  }

  if (sum(is.na(mat)) != 0) {
    stop("Input `mat` contains NA values. Stopping.")
  }

    if (is.null(colnames(mat)) | is.null(rownames(mat))) {
        stop("`mat` seems to lack either colnames or rownames. Stopping.")
    }

  # numerator
  share_unit_region <- mat / rowSums(mat)

  # denominator
  share_unit_global <- colSums(mat) / sum(mat)

  incidence_mat <- t(t(share_unit_region) / share_unit_global)

  # NAs can be introduced in there are years where certain products are not exported at all (divide-by-zero problem)
  incidence_mat[is.na(incidence_mat)] <- 0

  if (binary == TRUE) {
    incidence_mat <- ifelse(incidence_mat >= 1, 1, 0)
  }

  return(incidence_mat)

}


#' Wrapper around compute_rca for long data
#'
#'
#'


## take in data that is long and has multiple years

get_rca

help_cmpute_rca <- function(int_df) {
    int_df <- int_df %>%
        select(region, unit, intensity)

    ## turn into matrix to be able to apply functions
    int_mat <- int_df %>%
        spread(key = unit, value = intensity) %>%
        column_to_rownames(var = "region") %>%
        as.matrix()

    ## apply function
    rca_mat <- compute_rca(int_mat, binary = args$binary)

    ## add time back in, make long
    out_df <- rca_mat %>%
        as.data.frame() %>%
        rownames_to_column(var = "region") %>%
        gather(-c(region), key = unit, value = rca) %>%
        select(region, unit, rca) %>%
        as_tibble()

    ## return
    return(out_df)
}


return_df <- int_df %>%
    group_by(time) %>%
    nest() %>%
    mutate(
        rca_frames = purrr::map(data, help_get_rca)
    ) %>%
    unnest(rca_frames)

return(return_df)
