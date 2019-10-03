context("Average similarity for region to unit")
library(compost)
                                        # Define test matrices

mat_ls <- matrix_test_list()

                                        # start tests
test_that("If input is wrong, throw error", {

    ## DONE correct input gives matrix-type output (allow 0s)
    expect_identical(
        class(get_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix, exclude_0s = FALSE)),
        class(matrix())
        )

    ## DONE correct input gives matrix-type output (exclude 0s)
    expect_identical(
      class(get_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix, exclude_0s = TRUE)),
      class(matrix())
      )

    ## DONE  rca_mat is not a matrix
    expect_error(
      get_average_sim(rca_mat = tibble(), sim_mat = mat_ls$sim_matrix),
      "not a matrix"
      )

    ## DONE sim_mat is not a matrix
    expect_error(
      get_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = tibble()),
      "not a matrix"
      )

    ## DONE rca_mat and sim_mat does not have same number of columns
    expect_error(
      get_average_sim(rca_mat = mat_ls$rca_matrix_wrong_col_n, sim_mat = mat_ls$sim_matrix),
      "number of columns"
      )

    ## DONE rca_mat contains NA values
    expect_error(
      get_average_sim(rca_mat = mat_ls$rca_matrix_NA, sim_mat = mat_ls$sim_matrix),
      "contain NA"
      )

    ## DONE sim_mat contains NA values
    expect_error(
      get_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix_NA),
      "contain NA"
      )

    ## DONE sim_mat is not square
    expect_error(
      get_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$rca_matrix),
      "not a square"
      )

    ## DONE rca_mat is not binary
    expect_error(
      get_average_sim(rca_mat = mat_ls$rca_matrix_nb, sim_mat = mat_ls$sim_matrix),
      "does not seem to be binary"
      )

    ## DONE rca_mat and sim_mat does not have same colnames
    expect_error(
      get_average_sim(rca_mat = mat_ls$rca_matrix_names, sim_mat = mat_ls$sim_matrix),
      "ordering"
      )

    ## DONE output of function has same colnames as rca_mat
    expect_identical(
      colnames(get_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)),
      colnames(mat_ls$rca_matrix)
    )

    ## DONE output of functions has same dimensions as rca_mat
    expect_identical(
      dim(get_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)),
      dim(mat_ls$rca_matrix)
    )

}
)


test_that("Output values are correct", {

    ## DONE value 1, 1 in output matrix, allow 0s
    expect_identical(
        get_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix, exclude_0s = FALSE)[1, 1],
        mean(c((1 * 0.0), (0 * 0.20), (0 * 0.63), (1 * 0.69)))
    )

    ## DONE value 2, 3 in output matrix, allow 0s
    expect_identical(
        get_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix, exclude_0s = FALSE)[2, 3],
        mean(mat_ls$rca_matrix[2, ] * mat_ls$sim_matrix[3, ])
    )

    ## DONE value 1, 1 in output matrix, exclude 0s
    expect_identical(
        get_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix, exclude_0s = TRUE)[1, 1],
        mean(c((1 * 0.0), (NA * 0.20), (NA * 0.63), (1 * 0.69)), na.rm = TRUE)
    )

    ## DONE 2, 3 in output matrix, exclude 0s
    expect_identical(
        get_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix, exclude_0s = TRUE)[2, 3],
        mean(c((1 * 0.63), (1 * 0.06), (1 * 0.00), (1 * 0.77)))
    )

})
