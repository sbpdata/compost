context("Weighted average similarity for region to unit")
library(compost)
# Define test matrices

mat_ls <- matrix_test_list()

# start tests
test_that("If input is wrong, throw error", {

    ## DONE correct input gives matrix-type output
    expect_identical(
        class(get_weighted_average_sim(rca_mat = mat_ls$rca_matrix_nb, sim_mat = mat_ls$sim_matrix)),
        class(matrix())
    )

    ## DONE rca_mat is not a matrix
    expect_error(
        get_weighted_average_sim(rca_mat = c(1, 2, 3), sim_mat = mat_ls$sim_matrix),
        "not a matrix"
    )

    ## DONE sim_mat is not a matrix
    expect_error(
        get_weighted_average_sim(rca_mat = mat_ls$rca_matrix_nb, sim_mat = c(1, 2, 3)),
        "not a matrix"
    )

    ## DONE rca_mat and sim_mat does not have same number of columns
    expect_error(
        get_weighted_average_sim(rca_mat = mat_ls$rca_matrix_wrong_col_n, sim_mat = mat_ls$sim_matrix),
                 "number of columns"
                 )
    
    ## DONE rca_mat contains NA values
    expect_error(
        get_weighted_average_sim(rca_mat = mat_ls$rca_matrix_NA, sim_mat = mat_ls$sim_matrix),
                 "contain NA"
                 )
    
    ## DONE sim_mat contains NA values
    expect_error(
        get_weighted_average_sim(rca_mat = mat_ls$rca_matrix_nb, sim_mat = mat_ls$sim_matrix_NA),
        "contain NA"
    )

    ## DONE sim_mat is not square
    expect_error(
        get_weighted_average_sim(rca_mat = mat_ls$rca_matrix_nb, sim_mat = mat_ls$rca_matrix_nb),
        "not a square"
    )

    ## DONE rca_mat is binary
    expect_error(
        get_weighted_average_sim(rca_mat = mat_ls$rca_matrix, sim_mat = mat_ls$sim_matrix),
        "binary"
        )

    ## DONE rca_mat and sim_mat does not have same colnames
    expect_error(
        get_weighted_average_sim(rca_mat = mat_ls$rca_matrix_names_nb, sim_mat = mat_ls$sim_matrix),
        "ordering"
    )

    ## DONE output of function has same colnames as rca_mat
    expect_identical(
        colnames(get_weighted_average_sim(rca_mat = mat_ls$rca_matrix_nb, sim_mat = mat_ls$sim_matrix)),
        colnames(mat_ls$rca_matrix_nb)
    )

    ## DONE output of functions has same dimensions as rca_mat
    expect_identical(
        dim(get_weighted_average_sim(rca_mat = mat_ls$rca_matrix_nb, sim_mat = mat_ls$sim_matrix)),
        dim(mat_ls$rca_matrix_nb)
    )

})

test_that("Output values are correct", {

    ## DONE value 1, 1 in output matrix
    expect_identical(
        get_weighted_average_sim(rca_mat = mat_ls$rca_matrix_nb, mat_ls$sim_matrix)[1, 1],
        mean(c((0.80 * 0.00), (2.72 * 0.20), (2.83 * 0.63), (0.19 * 0.69)))
        )

    ## DONE value 2, 3 in output matrix
    expect_identical(
        get_weighted_average_sim(rca_mat = mat_ls$rca_matrix_nb, mat_ls$sim_matrix)[2, 3],
        mean(c((1.12 * 0.63), (0.61 * 0.06), (1.98 * 0.00), (0.62 * 0.77)))
    )

})
