context("Revealed comparative advantage from intensity matrix")
library(compost)
# Define test matrices

mat_ls <- matrix_test_list()

# start tests
test_that("If input is wrong, throw error", {

    ## DONE mat is not a matrix
    expect_error(
        get_rca(mat = data.frame(), binary = TRUE),
        "not a matrix"
    )

    ## DONE mat contains NA values
    expect_error(
        get_rca(mat = matrix(c(1, NA, 3)), binary = TRUE),
        "contains NA"
    )

    ## DONE mat contains dimnames
    expect_error(
        get_rca(mat = matrix(c(1, 2, 3)), binary = TRUE),
        "seems to lack either colnames or rownames"
    )

})

test_that("Output has the right format", {

## DONE correct input gives matrix-type output (binary = TRUE)
expect_identical(
    class(get_rca(mat = mat_ls$int_matrix, binary = TRUE)),
    class(matrix())
)

    ## DONE output of function has same dimnames as int_mat (binary = TRUE)
expect_identical(
    dimnames(get_rca(mat = mat_ls$int_matrix, binary = TRUE)),
    dimnames(mat_ls$int_matrix)
    )

    ## DONE output of function has same dimensions as int_mat (binary = TRUE)
expect_identical(
    dim(get_rca(mat = mat_ls$int_matrix, binary = TRUE)),
    dim(mat_ls$rca_matrix)
)

## DONE output of function is binary when binary = TRUE
expect_equal(
    sum(
        !(get_rca(mat = mat_ls$int_matrix, binary = TRUE)) %in% c(1, 0)
    ),
    0
)

## DONE correct input gives matrix-type output (binary = FALSE)
expect_identical(
    class(get_rca(mat = mat_ls$int_matrix, binary = FALSE)),
    class(matrix())
)

## DONE output of function has same dimnames as int_mat (binary = TRUE)
expect_identical(
    dimnames(get_rca(mat = mat_ls$int_matrix, binary = FALSE)),
    dimnames(mat_ls$int_matrix)
)

## DONE output of functions has same dimensions as int_mat (binary = TRUE)
expect_identical(
    dim(get_rca(mat = mat_ls$int_matrix, binary = FALSE)),
    dim(mat_ls$rca_matrix)
)

## DONE output of function is not binary when binary = FALSE
expect_equal(
    sum(
        get_rca(mat = mat_ls$int_matrix, binary = FALSE) %in% c(1, 0)
    ),
    0
)

})

test_that("Output values are correct", {
    ## DONE binary == TRUE: value 1, 1 in output matrix
    expect_identical(
        get_rca(mat = mat_ls$int_matrix, binary = TRUE)[1, 1],
        ifelse(
        (mat_ls$int_matrix[1, 1] / sum(mat_ls$int_matrix[ , 1])) / (sum(mat_ls$int_matrix[1, ])/ sum(mat_ls$int_matrix)) >= 1, 1, 0
        )
    )



    ## DONE binary == TRUE: value 2, 3 in output matrix
    expect_identical(
        get_rca(mat = mat_ls$int_matrix, binary = TRUE)[2, 3],
        ifelse(
        (mat_ls$int_matrix[2, 3] / sum(mat_ls$int_matrix[ , 3])) / (sum(mat_ls$int_matrix[2, ])/ sum(mat_ls$int_matrix)) >= 1, 1, 0
        )
    )

    ## DONE binary == FALSE: value 1, 1 in output matrix
    expect_identical(
        get_rca(mat = mat_ls$int_matrix, binary = FALSE)[1, 1],
        (mat_ls$int_matrix[1, 1] / sum(mat_ls$int_matrix[ , 1])) / (sum(mat_ls$int_matrix[1, ])/ sum(mat_ls$int_matrix))
    )


    ## DONE binary == FALSE: value 2, 3 in output matrix
    expect_identical(
        get_rca(mat = mat_ls$int_matrix, binary = FALSE)[2, 3],
        (mat_ls$int_matrix[2, 3] / sum(mat_ls$int_matrix[ , 3])) / (sum(mat_ls$int_matrix[2, ])/ sum(mat_ls$int_matrix))
    )

})

