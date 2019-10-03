#' Function that counts the number of NAs in a data frame or a vector
#'
#' @param x The data frame or vector that should have its NA values counted.
#' @export

count_na <- function(x) {
  number_of_na <- sum(is.na(x))
  return(number_of_na)
}

#' Create list of matrices most used in the tests
#'
#' @import dplyr
#' @import tidyr
#'
#' @export

matrix_test_list <- function(){

  library(tidyr)
  library(dplyr)
  library(tibble)
                                        # intensity matrix
    int_matrix <- matrix(
        c(127, 101, 82, 96,
          94, 132, 59, 106,
          74, 84, 81, 63),
        nrow = 3, ncol = 4, byrow = TRUE,
        dimnames = list(c("R1", "R2", "R3"),
                        c("U1", "U2", "U3", "U4")))

                                        # rca matrix_bin
    rca_matrix <- matrix(
        c(1, 0, 0, 1,
          1, 1, 1, 1,
          1, 0, 1, 1),
        ncol = 4, nrow = 3, byrow = TRUE,
        dimnames = list(c("R1", "R2", "R3"),
                        c("U1", "U2", "U3", "U4")))

                                        # correct sim matrix (four rows, four cols)
    sim_matrix <- matrix(
        c(0.00, 0.20, 0.63, 0.69,
          0.20, 0.00, 0.06, 0.38,
          0.63, 0.06, 0.00, 0.77,
          0.69, 0.38, 0.77, 0.00),
        ncol = 4, nrow = 4, byrow = TRUE,
        dimnames = list(c("U1", "U2", "U3", "U4"),
                        c("U1", "U2", "U3", "U4")))

                                        # rca matrix with wrong dimensions (4 rows, 3 cols)
    rca_matrix_wrong_col_n <- matrix(
        c(1, 0, 0,
          1, 1, 1,
          1, 1, 1,
          0, 1, 1),
        ncol = 3, nrow = 4, byrow = TRUE,
        dimnames = list(c("R1", "R2", "R3", "R4"),
                        c("U1", "U2", "U3")))

                                        # rca matrix has NA value
    rca_matrix_NA <- matrix(
        c(1, 0, 0, 1,
          1, 1, NA, 1,
          1, 0, 1, 1),
        ncol = 4, nrow = 3, byrow = TRUE,
        dimnames = list(c("R1", "R2", "R3"),
                        c("U1", "U2", "U3", "U4")))

                                        # sim matrix has NA value
    sim_matrix_NA <- matrix(
        c(0.00, 0.20, 0.63, 0.69,
          0.20, 0.00, 0.06, 0.38,
          0.63, 0.06, NA, 0.77,
          0.69, 0.38, 0.77, 0.00),
        ncol = 4, nrow = 4, byrow = TRUE,
        dimnames = list(c("U1", "U2", "U3", "U4"),
                        c("U1", "U2", "U3", "U4")))

                                        # non-binary rca
    rca_matrix_nb <- matrix(
        c(0.80, 2.72, 2.83, 0.19,
          1.12, 0.61, 1.98, 0.62,
          1.72, 2.70, 1.89, 0.53),
        ncol = 4, nrow = 3, byrow = TRUE,
        dimnames = list(c("R1", "R2", "R3"),
                        c("U1", "U2", "U3", "U4")))

                                        # rca matrix wrong col names
    rca_matrix_names <- matrix(
        c(1, 0, 0, 1,
          1, 1, 1, 1,
          1, 0, 1, 1),
        ncol = 4, nrow = 3, byrow = TRUE,
        dimnames = list(c("R1", "R2", "R3"),
                        c("F1", "U2", "U3", "U4")))

                                        # rca matrix wrong col names (not binary)
    rca_matrix_names_nb <- matrix(
        c(0.80, 2.72, 2.83, 0.19,
          1.12, 0.61, 1.98, 0.62,
          1.72, 2.70, 1.89, 0.53),
        ncol = 4, nrow = 3, byrow = TRUE,
        dimnames = list(c("R1", "R2", "R3"),
                        c("F1", "U2", "U3", "U4")))


    rca_long <- rca_matrix %>%
        as.data.frame() %>%
        rownames_to_column(var = "region") %>%
        gather(-region, key = unit, value = rca) %>%
        mutate(time = 2000)


    int_long <- int_matrix %>%
        as.data.frame() %>%
        rownames_to_column(var = "region") %>%
        gather(-region, key = unit, value = intensity) %>%
        mutate(time = 2000)

    sim_long <- sim_matrix %>%
        as.data.frame() %>%
        rownames_to_column(var = "from_unit") %>%
        gather(-from_unit, key = to_unit, value = similarity) %>%
        mutate(time = 2000)

    matrix_list <- list(
        "int_matrix" = int_matrix,
        "rca_matrix" = rca_matrix,
        "sim_matrix" = sim_matrix,
        "rca_matrix_wrong_col_n" = rca_matrix_wrong_col_n,
        "rca_matrix_NA" = rca_matrix_NA,
        "sim_matrix_NA" = sim_matrix_NA,
        "rca_matrix_nb" = rca_matrix_nb,
       "rca_matrix_names" = rca_matrix_names,
       "rca_matrix_names_nb" = rca_matrix_names_nb,
       "rca_long" = rca_long,
       "int_long" = int_long,
       "sim_long" = sim_long
    )

    return(matrix_list)

}


