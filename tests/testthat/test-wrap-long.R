library(testthat)
library(compost)
context("Test wrap_long functionality")

### STRUCTURE
### 1: Define matrices and data frames
### 2: Tests


###########################################################################
### Define matrices and data frames                                     ###
###########################################################################

## intensity matrix
int_matrix <- matrix(
    c(127, 101, 82, 96,
      94, 132, 59, 106,
      100, 84, 81, 63),
    nrow = 3, ncol = 4, byrow = TRUE,
    dimnames = list(
        c("R1", "R2", "R3"),
        c("U1", "U2", "U3", "U4")
    )
)

## rca matrix
rca_matrix <- get_rca(mat = int_matrix, binary = TRUE)
rca_matrix_NB <- get_rca(mat = int_matrix, binary = FALSE)

## similarity matrix
sim_matrix <- get_similarity(mat = rca_matrix, method = "jaccard", diag0 = TRUE)

## relatedness metric matrices
density_matrix <- get_density(rca_mat = rca_matrix, sim_mat = sim_matrix)
closest_matrix <- get_closest_sim(rca_mat = rca_matrix, sim_mat = sim_matrix)
average_matrix_inc0 <- get_average_sim(rca_mat = rca_matrix, sim_mat = sim_matrix, exclude_0s = FALSE)
average_matrix_exc0 <- get_average_sim(rca_mat = rca_matrix, sim_mat = sim_matrix, exclude_0s = TRUE)
w_average_matrix <- get_weighted_average_sim(rca_mat = rca_matrix_NB, sim_mat = sim_matrix)

### -----------------------------------------------------------------------
### 1.2 Transform to long data
### -----------------------------------------------------------------------

int_long <- int_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "region") %>%
    gather(-region, key = unit, value = intensity) %>%
    as_tibble() %>%
    mutate(time = 2000) %>%
    select(time, region, unit, intensity)

rca_long <- rca_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "region") %>%
    gather(-region, key = unit, value = rca) %>%
    as_tibble() %>%
    mutate(time = 2000) %>%
    select(time, region, unit, rca)

rca_NB_long <- rca_matrix_NB %>%
    as.data.frame() %>%
    rownames_to_column(var = "region") %>%
    gather(-region, key = unit, value = rca) %>%
    as_tibble() %>%
    mutate(time = 2000) %>%
    select(time, region, unit, rca)

sim_long <- sim_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "from_unit") %>%
    gather(-from_unit, key = to_unit, value = similarity) %>%
    as_tibble() %>%
    mutate(time = 2000) %>%
    select(time, from_unit, to_unit, similarity)

density_long <- density_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "region") %>%
    gather(-region, key = unit, value = density) %>%
    as_tibble() %>%
    mutate(time = 2000) %>%
    select(time, region, unit, density)

closest_long <- closest_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "region") %>%
    gather(-region, key = unit, value = closest_similarity) %>%
    as_tibble() %>%
    mutate(time = 2000) %>%
    select(time, region, unit, closest_similarity)

average_inc0_long <- average_matrix_inc0 %>%
    as.data.frame() %>%
    rownames_to_column(var = "region") %>%
    gather(-region, key = unit, value = avg_similarity) %>%
    as_tibble() %>%
    mutate(time = 2000) %>%
    select(time, region, unit, avg_similarity)

average_exc0_long <- average_matrix_exc0 %>%
    as.data.frame() %>%
    rownames_to_column(var = "region") %>%
    gather(-region, key = unit, value = avg_similarity) %>%
    as_tibble() %>%
    mutate(time = 2000) %>%
    select(time, region, unit, avg_similarity)

w_avg_long <- w_average_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "region") %>%
    gather(-region, key = unit, value = w_avg_similarity) %>%
    as_tibble() %>%
    mutate(time = 2000) %>%
    select(time, region, unit, w_avg_similarity)

###########################################################################
### Test outputs
###
###
###########################################################################

### -----------------------------------------------------------------------
### Output is identical to output from matrix-functions made long
### -----------------------------------------------------------------------

test_that("Output is the same as the output from matrix functions (made long)", {

## DONE get rca
expect_identical(
    wrap_long(fun = "get_rca", int_df = int_long, args = list(binary = TRUE)),
    rca_long
)

## DONE get rca not binary
expect_identical(
    wrap_long(fun = "get_rca", int_df = int_long, args = list(binary = FALSE)),
    rca_NB_long
)


## DONE get similarity
expect_identical(
    wrap_long(fun = "get_similarity", rca_df = rca_long, args = list(method = "jaccard")),
    sim_long
)

## DONE get density
expect_identical(
    wrap_long(fun = "get_density", rca_df = rca_long, sim_df = sim_long, args = list(binary = TRUE)),
    density_long
)

## DONE get closest sim
expect_identical(
    wrap_long(fun = "get_closest_sim", rca_df = rca_long, sim_df = sim_long),
    closest_long
)

## DONE get average sim, include 0s
expect_identical(
    wrap_long(fun = "get_average_sim", rca_df = rca_long, sim_df = sim_long, args = list(exclude0 = FALSE)),
    average_inc0_long
)

## DONE get average sim, exclude 0s
expect_identical(
    wrap_long(fun = "get_average_sim", rca_df = rca_long, sim_df = sim_long, args = list(exclude0 = TRUE)),
    average_exc0_long
)

## DONE get weighted avg sim
expect_identical(
    wrap_long(fun = "get_weighted_average_sim", rca_df = rca_NB_long, sim_df = sim_long),
    w_avg_long
)

}
)

