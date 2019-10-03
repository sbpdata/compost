context("Similarity metrics")
library(compost)

mat_ls <- matrix_test_list()

## DONE input
test_that("If input is wrong, throw error" , {

    ## DONE mat is a matrix
    expect_error(
        get_similarity(mat = data.frame(c(1, 2, 3)), method = "jaccard"),
        "not a matrix"
    )

    ## DONE mat is binary
    expect_error(
        get_similarity(mat = matrix(c(1, 2, 3)), method = "jaccard"),
        "seems not to be binary"
    )

    ## DONE mat has colnames and rownames
    expect_error(
        get_similarity(mat = matrix(c(1, 0, 1), dimnames = list(NULL, NULL)), method = "jaccard"),
        "missing either colnames or rownames" ## use is.null()
    )

    ## DONE mat contain NA values
    expect_error(
        get_similarity(mat = matrix(c(1, NA, 1)), method = "jaccard"),
        "contains NA"
    )

    ## DONE method is missing
    expect_error(
        get_similarity(mat = mat_ls$rca_matrix, method = NULL),
        "length 1"
    )

    ## DONE method is longer than 1
    expect_error(
        get_similarity(mat = mat_ls$rca_matrix, method = c("jaccard", "proximity")),
        "length 1"
    )

    ## DONE method is not understood
    expect_error(
        get_similarity(mat = mat_ls$rca_matrix, method = "volapyk"),
        "does not match"
    )

    ## DONE method argument is not affected by capitalization
    expect_identical(
        get_similarity(mat = mat_ls$rca_matrix, method = "JaCCarD"),
        get_similarity(mat = mat_ls$rca_matrix, method = "jaccard")
    )

})

## output
test_that("Output format is correct for method = jaccard" , {
    ## jaccard
    ## DONE output is a matrix
    expect_identical(
        class(get_similarity(mat = mat_ls$rca_matrix, method = "jaccard")),
        class(matrix())
    )

    ## DONE output has same number of columns as input
    expect_identical(
        ncol(get_similarity(mat = mat_ls$rca_matrix, method = "jaccard")),
        ncol(mat_ls$rca_matrix)
    )

    ## DONE output is square
    expect_identical(
        ncol(get_similarity(mat = mat_ls$rca_matrix, method = "jaccard")),
        nrow(get_similarity(mat = mat_ls$rca_matrix, method = "jaccard"))
    )

    ## DONE has same order of column names as input
    expect_identical(
        colnames(get_similarity(mat = mat_ls$rca_matrix, method = "jaccard")),
        colnames(mat_ls$rca_matrix)
    )

    ## DONE output has same order of row names as column names
    expect_identical(
        colnames(get_similarity(mat = mat_ls$rca_matrix, method = "jaccard")),
        rownames(get_similarity(mat = mat_ls$rca_matrix, method = "jaccard"))
    )

    ## DONE output has a value between 0 and 1
    expect_equal(
        sum(get_similarity(mat = mat_ls$rca_matrix, method = "jaccard") < 0 & get_similarity(mat = mat_ls$rca_matrix, method = "jaccard") > 1),
        0
    )

    ## DONE output has 0s on all diagonal elements
    expect_equal(
        sum(diag(get_similarity(mat = mat_ls$rca_matrix, method = "jaccard")) == 0),
        ncol(mat_ls$rca_matrix)
    )

    ## DONE test for symmetry
    expect_equal(
        isSymmetric.matrix(get_similarity(mat = mat_ls$rca_matrix, method = "jaccard")),
        TRUE
    )

})

test_that("Output format is correct for method = association" , {
    ## association
    ## DONE output is a matrix
    expect_identical(
        class(get_similarity(mat = mat_ls$rca_matrix, method = "association")),
        class(matrix())
    )

    ## DONE output has same number of columns as input
    expect_identical(
        ncol(get_similarity(mat = mat_ls$rca_matrix, method = "association")),
        ncol(mat_ls$rca_matrix)
    )

    ## DONE output is square
    expect_identical(
        ncol(get_similarity(mat = mat_ls$rca_matrix, method = "association")),
        nrow(get_similarity(mat = mat_ls$rca_matrix, method = "association"))
    )

    ## DONE output has same order of column names as input
    expect_identical(
        colnames(get_similarity(mat = mat_ls$rca_matrix, method = "association")),
        colnames(mat_ls$rca_matrix)
    )

    ## DONE output has same order of row names as column names
    expect_identical(
        colnames(get_similarity(mat = mat_ls$rca_matrix, method = "association")),
        rownames(get_similarity(mat = mat_ls$rca_matrix, method = "association"))
    )

    ## DONE output has a value between 0 and 1
    expect_equal(
        sum(get_similarity(mat = mat_ls$rca_matrix, method = "association") < 0 & get_similarity(mat = mat_ls$rca_matrix, method = "association") > 1),
        0
    )

    ## DONE output has 0s on all diagonal elements
    expect_equal(
        sum(diag(get_similarity(mat = mat_ls$rca_matrix, method = "association")) == 0),
        ncol(mat_ls$rca_matrix)
    )

    ## DONE test for symmetry
    expect_equal(
        isSymmetric.matrix(get_similarity(mat = mat_ls$rca_matrix, method = "association")),
        TRUE
    )
})


test_that("Output format is correct for method = proximity" , {
    ## proximity
    ## DONE output is a matrix
    expect_identical(
        class(get_similarity(mat = mat_ls$rca_matrix, method = "proximity")),
        class(matrix())
    )

    ## DONE output has same number of columns as input
    expect_identical(
        ncol(get_similarity(mat = mat_ls$rca_matrix, method = "proximity")),
        ncol(mat_ls$rca_matrix)
    )

    ## DONE output is square
    expect_identical(
        ncol(get_similarity(mat = mat_ls$rca_matrix, method = "proximity")),
        nrow(get_similarity(mat = mat_ls$rca_matrix, method = "proximity"))
    )

    ## DONE output has same order of column names as input
    expect_identical(
        colnames(get_similarity(mat = mat_ls$rca_matrix, method = "proximity")),
        colnames(mat_ls$rca_matrix)
    )

    ## DONE output has same order of row names as column names
    expect_identical(
        colnames(get_similarity(mat = mat_ls$rca_matrix, method = "proximity")),
        rownames(get_similarity(mat = mat_ls$rca_matrix, method = "proximity"))
    )

    ## DONE output has a value between 0 and 1
    expect_equal(
        sum(get_similarity(mat = mat_ls$rca_matrix, method = "proximity") < 0 & get_similarity(mat = mat_ls$rca_matrix, method = "proximity") > 1),
        0
    )

    ## DONE output has 0s on all diagonal elements
    expect_equal(
        sum(diag(get_similarity(mat = mat_ls$rca_matrix, method = "proximity")) == 0),
        ncol(mat_ls$rca_matrix)
    )

    ## DONE test for symmetry
    expect_equal(
        isSymmetric.matrix(get_similarity(mat = mat_ls$rca_matrix, method = "proximity")),
        TRUE
    )

})


## output values
test_that("Output value is correct for each method" , {

    ## JACCARD

    ## DONE jaccard U1 to U2 (ie [1, 2] and [2, 1])
    ## (|A n B|) / (|A| + |B| - |A n B|)
    ## |A n B|: sum(mat_ls$rca_matrix[ , 1] == 1 & mat_ls$rca_matrix[ , 2] == 1)
    ## |A|: sum(mat_ls$rca_matrix[ , 1]
    ## |B|: sum(mat_ls$rca_matrix[ , 2]
    expect_identical(
        get_similarity(mat = mat_ls$rca_matrix, method = "jaccard")[1, 2],
        (sum(mat_ls$rca_matrix[ , 1] == 1 & mat_ls$rca_matrix[ , 2] == 1)) / (sum(mat_ls$rca_matrix[ , 1] + sum(mat_ls$rca_matrix[ , 2]) - sum(mat_ls$rca_matrix[ , 1] == 1 & mat_ls$rca_matrix[ , 2] == 1)))
    )

    ## DONE jaccard U3 to U4 (ie [3, 4] and [4, 3])
    ## (|A n B|) / (|A| + |B| - |A n B|)
    ## |A n B|: sum(mat_ls$rca_matrix[ , 3] == 1 & mat_ls$rca_matrix[ , 4] == 1)
    ## |A|: sum(mat_ls$rca_matrix[ , 3]
    ## |B|: sum(mat_ls$rca_matrix[ , 4]
    expect_identical(
        get_similarity(mat = mat_ls$rca_matrix, method = "jaccard")[3, 4],
        (sum(mat_ls$rca_matrix[ , 3] == 1 & mat_ls$rca_matrix[ , 4] == 1)) / (sum(mat_ls$rca_matrix[ , 3]) + sum(mat_ls$rca_matrix[ , 4]) - sum(mat_ls$rca_matrix[ , 3] == 1 & mat_ls$rca_matrix[ , 4] == 1))
    )


    ## ASSOCIATION

    ## DONE association U1 to U2 (ie [1, 2], [2, 1])
    ## |A n B| / (|A| * |B|)
    ## |A n B|: sum(mat_ls$rca_matrix[ , 1] == 1 & mat_ls$rca_matrix[ , 2] == 1)
    ## |A|: sum(mat_ls$rca_matrix[ , 1]
    ## |B|: sum(mat_ls$rca_matrix[ , 2]
    expect_identical(
        get_similarity(mat = mat_ls$rca_matrix, method = "association")[1, 2],
        sum(mat_ls$rca_matrix[ , 1] == 1 & mat_ls$rca_matrix[ , 2] == 1) / (sum(mat_ls$rca_matrix[ , 1]) * sum(mat_ls$rca_matrix[ , 2]))
    )

    ## DONE association U3 to U4 (ie [3, 4], [4, 3])
    ## |A n B| / (|A| * |B|)
    ## |A n B|: sum(mat_ls$rca_matrix[ , 3] == 1 & mat_ls$rca_matrix[ , 4] == 1)
    ## |A|: sum(mat_ls$rca_matrix[ , 3]
    ## |B|: sum(mat_ls$rca_matrix[ , 4]
    expect_identical(
        get_similarity(mat = mat_ls$rca_matrix, method = "association")[3, 4],
        sum(mat_ls$rca_matrix[ , 3] == 1 & mat_ls$rca_matrix[ , 4] == 1) / (sum(mat_ls$rca_matrix[ , 3]) * sum(mat_ls$rca_matrix[ , 4]))
    )

    ## PROXIMITY

    ## DONE proximity U1 to U2
    ## |A n B| / max(|A|, |B|)
    ## |A n B|: sum(mat_ls$rca_matrix[ , 1] == 1 & mat_ls$rca_matrix[ , 2] == 1)
    ## |A|: sum(mat_ls$rca_matrix[ , 1]
    ## |B|: sum(mat_ls$rca_matrix[ , 2]
    expect_identical(
        get_similarity(mat = mat_ls$rca_matrix, method = "proximity")[1, 2],
        sum(mat_ls$rca_matrix[ , 1] == 1 & mat_ls$rca_matrix[ , 2] == 1) / max(sum(mat_ls$rca_matrix[ , 1]), sum(mat_ls$rca_matrix[ , 2]))
    )

    ## DONE proximity U3 to U4
    ## |A n B| / max(|A|, |B|)
    ## |A n B|: sum(mat_ls$rca_matrix[ , 3] == 1 & mat_ls$rca_matrix[ , 4] == 1)
    ## |A|: sum(mat_ls$rca_matrix[ , 3]
    ## |B|: sum(mat_ls$rca_matrix[ , 4]
    expect_identical(
        get_similarity(mat = mat_ls$rca_matrix, method = "proximity")[3, 4],
        sum(mat_ls$rca_matrix[ , 3] == 1 & mat_ls$rca_matrix[ , 4] == 1) / max(sum(mat_ls$rca_matrix[ , 3]), sum(mat_ls$rca_matrix[ , 4]))
    )

})

