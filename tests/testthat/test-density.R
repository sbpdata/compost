context("Density around units")
library(compost)
# Define test matrices

mat_ls <- matrix_test_list()

# start tests
test_that("If input is wrong, throw error", {

    ## DONE correct input gives matrix-type output
    expect_identical(
        class(get_density(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)),
        class(matrix())
        )

    ## DONE rca_mat is not a matrix
    expect_error(
        get_density(rca_mat = data.frame(), sim_mat = mat_ls$sim_matrix),
        "not a matrix"
    )

    ## DONE sim_mat is not a matrix
    expect_error(
        get_density(rca_mat = mat_ls$rca_matrix, sim_mat = data.frame()),
        "not a matrix"
    )

    ## DONE rca_mat and sim_mat does not have same number of columns
    expect_error(
        get_density(rca_mat = mat_ls$rca_matrix_wrong_col_n, sim_mat = mat_ls$sim_matrix),
        "number of columns"
    )

    ## DONE rca_mat contains NA values
    expect_error(
        get_density(rca_mat = mat_ls$rca_matrix_NA, sim_mat = mat_ls$sim_matrix),
        "contains NA"
    )

    ## DONE sim_mat contains NA values
    expect_error(
        get_density(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix_NA),
        "contains NA"
    )


    ## DONE sim_mat is not square
    expect_error(
        get_density(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$rca_matrix),
        "square"
        )

    ## DONE rca_mat is not binary
    expect_error(
        get_density(rca_mat = mat_ls$rca_matrix_nb, sim_mat = mat_ls$sim_matrix),
        "not binary"
    )

    ## DONE rca_mat and sim_mat does not have same colnames
    expect_error(
        get_density(rca_mat = mat_ls$rca_matrix_names, sim_mat = mat_ls$sim_matrix),
        "right order"
    )

    ## DONE output of function has same colnames as rca_mat
    expect_identical(
        colnames(get_density(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)),
        colnames(mat_ls$rca_matrix)
    )


    ## DONE output of functions has same dimensions as rca_mat
    expect_identical(
        dim(get_density(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)),
        dim(mat_ls$rca_matrix)
    )


})

test_that("Output values are correct", {
    ## DONE value 1, 1 in output matrix
    expect_identical(
        get_density(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)[1, 1],
        ((1 * 0.00) + (0 * 0.20) + (0 * 0.63) + (1 * 0.69)) / (0.00 + 0.20 + 0.63 + 0.69)
    )

    ## DONE value 2, 3 in output matrix
    expect_identical(
        get_density(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix)[2, 3],
        ((1 * 0.63) + (1 * 0.06) + (1 * 0.00) + (1 * 0.77)) / (0.63 + 0.06 + 0.00 + 0.77)
    )
})
