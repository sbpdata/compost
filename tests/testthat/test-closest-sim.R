context("Highest similarity value to unit in region")
library(compost)

# Define test matrices
mat_ls <- matrix_test_list()

# start tests
test_that("If input is wrong, throw error", {
    
    ## DONE correct input gives matrix-type output
    expect_equal(
        class(get_closest_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)),
        class(matrix())
    )

    ## DONE rca_mat is not a matrix
    expect_error(
        get_closest_sim(rca_mat = c(1, 2, 3), sim_mat = mat_ls$sim_matrix),
        "not a matrix"
    )

    ## DONE sim_mat is not a matrix
    expect_error(
        get_closest_sim(rca_mat = mat_ls$rca_matrix, sim_mat = c(1, 2, 3)),
        "not a matrix"
    )

    ## DONE rca_mat and sim_mat does not have same number of columns
    expect_error(
        get_closest_sim(rca_mat = mat_ls$rca_matrix_wrong_col_n, sim_mat = mat_ls$sim_matrix),
        "number of columns"
    )

    ## DONE rca_mat contains NA values
    expect_error(
        get_closest_sim(rca_mat = mat_ls$rca_matrix_NA, sim_mat = mat_ls$sim_matrix),
        "NA values"
    )

    ## DONE sim_mat contains NA values
    expect_error(
        get_closest_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix_NA),
        "NA values"
    )

    ## DONE sim_mat is not square
    expect_error(
        get_closest_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$rca_matrix),
        "square matrix"
    )

    ## DONE rca_mat is not binary
    expect_error(
        get_closest_sim(rca_mat = mat_ls$rca_matrix_nb, sim_mat = mat_ls$sim_matrix),
        "not binary"
    )

    ## DONE rca_mat and sim_mat does not have same colnames
    expect_error(
        get_closest_sim(rca_mat = mat_ls$rca_matrix_names, sim_mat = mat_ls$sim_matrix),
        "ordering"
    )

    ## DONE output of function has same colnames as rca_mat
    expect_identical(
        colnames(get_closest_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)),
        colnames(mat_ls$rca_matrix)
    )

    ## DONE output of functions has same dimensions as rca_mat
    expect_identical(
        dim(get_closest_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)),
        dim(mat_ls$rca_matrix)
    )

})


test_that("Output values are correct", {
    ## value 1, 1 in output matrix
    expect_identical(get_closest_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)[1, 1], max(c((1 * 0.0), (0 * 0.20), (0 * 0.63), (1 * 0.69))))
    ## value 2, 3 in output matrix
    expect_identical(get_closest_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)[2, 3], max(c((1 * 0.63), (1 * 0.06), (1 * 0.00), (1 * 0.77))))
})
