#' Wrapper for long data (tidy) for a variety of functions
#'
#' Takes input in long data format and applies the compost-package
#' function listed in `fun`. Outputs long format data as well.
#'
#' @param fun selects the function to apply to the input data. Possible
#' values are: `get_rca`, `get_similarity`
#' @param args list with arguments to the function listed in fun. Possible
#' arguments depends on the function.
#'
#' `get_rca`:
#'
#'  - `binary` (logical) decides whether RCA values are binarized. Possible values
#' are `TRUE` or `FALSE`.
#'
#' `get_similarity`:
#'
#' - `method` (string) selects equation to use for similarity calculations.
#' Possible values are `jaccard`, `association`, or `proximity`
#'
#' `get_average_sim`:
#'
#'  - `exclude0` (logical) decides if 0s should be excluded (as NA values)
#' when taking average_sim. Possible values are `TRUE` or `FALSE`
#'
#' `get_density`:
#'
#' - `binary` (logical) decides if it is allowed for RCA matrix to be
#' non-binary. If `FALSE` RCA values (non binary) can act as weights for
#' similarity values.
#'
#' Note: `get_closest_sim`, and `get_weighted_average_sim` does
#' not have any additional arguments.
#'
#' @param int_df data frame that contains four variables: time, region, unit, intensity.
#' Default is NULL.
#' @param rca_df data frame that contains four variables: time, region, unit, rca.
#' Default is NULL.
#' @param sim_df data frame that contains four variables: time, from_unit, to_unit, similarity.
#' Default is NULL.
#'
#' @import purrr
#' @import tidyr
#' @import dplyr
#' @import tibble
#' 
#' @export

wrap_long <- function(fun, args = NULL, int_df = NULL, rca_df = NULL, sim_df = NULL) {

###########################################################################
### STRUCTURE:
### 1: TODO Test input format
### 2: TODO Apply function
### 3: TODO Test output format
### 4: DONE Return output
###########################################################################

###########################################################################
### 1: Test input format                                                ###
### 1.1: Check fun                                                      ###
### 1.2: Check input data frames                                        ###
###########################################################################

    ## --------------------------------------------------------------------
    ## 1.1: Check fun
    ## --------------------------------------------------------------------

    ## length of fun
    if (length(fun) != 1) {
        stop("`fun` has a length of more than 1. Only one function is allowed.")
    }

    ## to lower caps
    fun <- tolower(fun)

    ## content of fun
    possible_funs <- c(
        "get_rca",
        "get_similarity",
        "get_density",
        "get_closest_sim",
        "get_average_sim",
        "get_weighted_average_sim"
    )

    if (!fun %in% possible_funs) {
        stop("`fun` is not among the possible functions. Options are: `get_rca`, `get_similarity`, `get_density`, `get_closest_sim`, `get_average_sim`, `get_weighted_average_sim`, `get_density`.")
    }

    ## check that args is a list
    if (!is.null(args)) {
        if (!is.list(args)) stop("`args` has to be a list of the type `argument = value`, fx `binary = TRUE`.")
    }

    ## check that args contains the right arguments
    if ("get_rca" %in% fun) {
        if (!is.logical(args$binary)) {
            stop("For function `get_rca`, `binary` has to be supplied in args as a logical.")
        }
    }

    if ("get_similarity" %in% fun) {
        if (!is.character(args$method)) {
            stop("For the function `get_similarity`, `method` has to be supplied in args as character string of length 1.")
        }
    }

    if ("get_average_sim" %in% fun) {
        if(!is.logical(args$exclude0)) {
            stop("For the function `get_average_sim`, `exclude0` has to be supplied as a logical of length 1.")
        }

    }

    ## --------------------------------------------------------------------
    ## 1.2: Check format of input df
    ## -------------------------------------------------------------------

    ## int_df -------------------------------------------------------------
    if (!is.null(int_df)) {

        if (!(tibble::is_tibble(int_df) | is.data.frame(int_df))) {
            stop("`int_df` needs to be a data.frame or a tibble.")
        }

        ## convert to tibble
        int_df <- as_tibble(int_df)

        ## correct variables
        if (!all(names(int_df) %in% c("time", "region", "unit", "intensity"))) {
            stop("`time`, `region`, `unit`, and `intensity` variables are not all present in `int_df`.")
        }

        ## complete data (balanced panel) and check for NAs
        int_complete <- int_df %>%
            group_by(time) %>%
            complete(region, unit)

        ## check NAs introduced after complete
        if (sum(is.na(int_complete)) != 0) {
            stop("NA values have been introduced into int_df after completing data. Stopping.")

        }

    }

    ## rca_df -------------------------------------------------------------
    if (!is.null(rca_df)) {

        if (!(tibble::is_tibble(rca_df) | is.data.frame(rca_df))) {
            stop("`rca_df` needs to be a data.frame or a tibble.")
        }

        ## convert to tibble
        rca_df <- as_tibble(rca_df)

        ## correct variables
        if (!all(names(rca_df) %in% c("time", "region", "unit", "rca"))) {
            stop("`time`, `region`, `unit`, and `rca` variables are not all present in `rca_df`.")
        }

        ## complete data (balanced panel) and check for NAs
        rca_complete <- rca_df %>%
            group_by(time) %>%
            complete(region, unit)

        ## check NAs introduced after complete
        if (sum(is.na(rca_complete)) != 0) {
            stop("NA values have been introduced into `rca_df` after completing data. Stopping.")

        }
    }

    ## sim_df -------------------------------------------------------------
    if (!is.null(sim_df)) {

        if (!(tibble::is_tibble(sim_df) | is.data.frame(sim_df))) {
            stop("`rca_df` needs to be a data.frame or a tibble.")
        }

        ## convert to tibble
        sim_df <- as_tibble(sim_df)

        ## correct variables
        if (!all(names(sim_df) %in% c("time", "from_unit", "to_unit", "similarity"))) {
            stop("`time`, `from_unit`, `to_unit`, and `similarity` variables are not all present in `sim_df`.")
        }

        ## complete data (balanced panel) and check for NAs
        sim_complete <- sim_df %>%
            group_by(time) %>%
            complete(from_unit, to_unit)

        ## check NAs introduced after complete
        if (sum(is.na(sim_complete)) != 0) {
            stop("NA values have been introduced into `sim_df` after completing data. Stopping.")

        }
    }



###########################################################################
### 2: Apply function                                                   ###
### 2.1: Define helper functions
### 2.1.1: help_get_rca DONE
### 2.1.2: help_get_similarity DONE
### 2.1.3: get_similarity_metrics DONE
### 2.2: Create nested frame, apply help_  DONE
###########################################################################

### -----------------------------------------------------------------------
### 2.1 Create helper functions
### -----------------------------------------------------------------------

    ## 2.1.1 help_get_rca

    help_get_rca <- function(int_df) {
        int_df <- int_df %>%
            select(region, unit, intensity)

        ## turn into matrix to be able to apply functions
        int_mat <- int_df %>%
            spread(key = unit, value = intensity) %>%
            column_to_rownames(var = "region") %>%
            as.matrix()

        ## apply function
        rca_mat <- get_rca(int_mat, binary = args$binary)

        ## add time back in, make long
        output_df <- rca_mat %>%
            as.data.frame() %>%
            rownames_to_column(var = "region") %>%
            gather(-c(region), key = unit, value = rca) %>%
            select(region, unit, rca) %>%
            as_tibble()

        ## return
        return(output_df)
    }

    ## 2.1.2 help_get_similarity

    help_get_similarity <- function(rca_df) {

        rca_df <- rca_df %>%
            select(region, unit, rca)

        ## turn into matrix to be able to apply functions
        rca_mat <- rca_df %>%
            spread(key = unit, value = rca) %>%
            column_to_rownames(var = "region") %>%
            as.matrix()

        ## apply function
        sim_mat <- get_similarity(mat = rca_mat, method = args$method, diag0 = TRUE)

        ## add time back in, make long
        output_df <- sim_mat %>%
            as.data.frame() %>%
            rownames_to_column(var = "from_unit") %>%
            gather(-c(from_unit), key = to_unit, value = similarity) %>%
            select(from_unit, to_unit, similarity) %>%
            as_tibble()

    }


    ## 2.1.3

    help_relatedness_metrics <- function(rca_df, sim_df) {

        ## prepare rca data
        rca_df <- rca_df %>%
            select(region, unit, rca)

        ## turn into matrix to be able to apply functions
        rca_matrix <- rca_df %>%
            spread(key = unit, value = rca) %>%
            column_to_rownames(var = "region") %>%
            as.matrix()

        ## prepare sim data
        sim_df <- sim_df %>%
            select(from_unit, to_unit, similarity)

        sim_matrix <- sim_df %>%
            spread(key = to_unit, value = similarity) %>%
            column_to_rownames(var = "from_unit") %>%
            as.matrix()


        ## apply function
        if (fun == "get_density") {

          if (!is.null(args$binary)) {
            binarize <- args$binary
          }

            density_mat <- get_density(rca_mat = rca_matrix, sim_mat = sim_matrix, binary = binarize)

            output_df <- density_mat %>%
                as.data.frame() %>%
                rownames_to_column(var = "region") %>%
                gather(-region, key = unit, value = density)

        }

        if (fun == "get_closest_sim") {

            closest_sim_mat <- get_closest_sim(rca_mat = rca_matrix, sim_mat = sim_matrix)

            output_df <- closest_sim_mat %>%
                as.data.frame() %>%
                rownames_to_column(var = "region") %>%
                gather(-region, key = unit, value = closest_similarity)

        }

        if (fun == "get_average_sim") {

            average_sim_mat <- get_average_sim(rca_mat = rca_matrix, sim_mat = sim_matrix, exclude_0s = args$exclude0)

            output_df <- average_sim_mat %>%
                as.data.frame() %>%
                rownames_to_column(var = "region") %>%
                gather(-region, key = unit, value = avg_similarity)

        }

        if (fun == "get_weighted_average_sim"){

            weighted_average_sim_mat <- get_weighted_average_sim(rca_mat = rca_matrix, sim_mat = sim_matrix)

            output_df <- weighted_average_sim_mat %>%
                as.data.frame() %>%
                rownames_to_column(var = "region") %>%
                gather(-region, key = unit, value = w_avg_similarity)
        }

        return(output_df)
    }

### -----------------------------------------------------------------------
### 2.2 Create nested frame, apply helper_function
### -----------------------------------------------------------------------

    ## DONE Apply helper for get_rca
    if (fun == "get_rca") {
        output_df <- int_df %>%
            group_by(time) %>%
            nest() %>%
            mutate(
                rca_frames = purrr::map(data, help_get_rca)
            ) %>%
            unnest(rca_frames)
    }

    ## DONE Apply helper for get_similarity
    if (fun == "get_similarity") {
        output_df <- rca_df %>%
            group_by(time) %>%
            nest() %>%
            mutate(
                sim_frames = purrr::map(data, help_get_similarity)
            ) %>%
            unnest(sim_frames)
    }

    ## DONE Apply helper for relatedness_metrics
    if (fun %in% c("get_density", "get_closest_sim", "get_average_sim", "get_weighted_average_sim")) {

        ## relatedness metrics requires both rca and sim matrices
        ## so first we need to nest them, and join the nested frames
        ## in the same data frame, so we can iterate over the different
        ## years with map2.

        sim_nest <- sim_df %>%
            group_by(time) %>%
            nest(.key = "sim")

        rca_nest <- rca_df %>%
            group_by(time) %>%
            nest(.key = "rca")

        ## joins by "time" (the only common column)
        nested <- rca_nest %>%
            right_join(sim_nest)

        output_df <- nested %>%
            mutate(
                metric = purrr::map2(rca, sim, help_relatedness_metrics)
            ) %>%
            unnest(metric)

        ## brug map double input
        ## nest df, nest sim og cbind

    }

###########################################################################
### 3: Test output                                                      ###
### 3.1: test if input dfs and output_df same units, regions, years     ###
### 3.2: test if the number of obs match between input and output       ###
###########################################################################

### -----------------------------------------------------------------------
### 3.1 DONE Test if input and out has same number of units, regions, years
### -----------------------------------------------------------------------


    ## TODO Redo so it is by year
    ## get_rca
    if (fun == "get_rca") {

        if (!identical(n_distinct(int_df$region), n_distinct(output_df$region))) {
            stop("The number of different regions in int_df and output_df is not identical.")
        }

        if (!identical(n_distinct(int_df$unit), n_distinct(output_df$unit))) {
            stop("The number of different units in int_df and output_df is not identical.")
        }

    }

    ## get_similarity
    if (fun == "get_similarity") {

#        if (!identical(n_distinct(rca_df$unit), n_distinct(output_df$from_unit))) {
#            stop("The number of different units in rca_df and output_df is not identical.")
#        }

    }

    ## relatedness metrics
    if (fun %in% c("get_density", "get_closest_sim", "get_average_sim", "get_weighted_average_sim")) {

        if (!identical(unique(rca_df$region), unique(output_df$region))) {
            stop("The different regions in rca_df and output_df is not identical.")
        }

        if (!all(unique(rca_df$unit) %in% unique(output_df$unit))) {
            stop("The different units in rca_df and output_df is not identical.")
        }
    }

### -----------------------------------------------------------------------
### 3.2: TODO test if the number of obs match between input and output
### -----------------------------------------------------------------------

    if (fun == "get_rca") {

    }

    ## get_similarity
    if (fun == "get_similarity") {

    }

    ## relatedness metrics
    if (fun %in% c("get_density", "get_closest_sim", "get_average_sim", "get_weighted_average_sim")) {

        }

    ## ## number of observation, region + time
    ## input_r_obs <- input_df %>%
    ##     group_by(region, time) %>%
    ##     tally()

    ## output_r_obs <- output_df %>%
    ##     group_by(region, time) %>%
    ##     tally()

    ## if (!identical(input_r_obs, output_r_obs)) {
    ##     stop("The number of observations per region does not match between input and output.")
    ## }

    ## ## number of observation, unit + time
    ## input_u_obs <- input_df %>%
    ##     group_by(unit, time) %>%
    ##     tally()

    ## output_u_obs <- output_df %>%
    ##     group_by(unit, time) %>%
    ##     tally()

    ## if (!identical(input_u_obs, output_u_obs)) {
    ##     stop("The number of observations per unit does not match between input and output.")
    ## }

###########################################################################
### 4. Return output                                                    ###
###########################################################################

    return(output_df)

    ## function ends

}
