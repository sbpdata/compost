#' Apply Fitness (2018) algorithm to RCA data
#'
#' @param df data frame that contains columns "region", "unit", "rca" (binary).
#' @param N number of iterations the algorithm should run over.
#' @param delta delta value used in function. default is 0.
#'
#' @return dataframe that contains both unit complexity and region fitness.
#'
#' @export

fitness2018 <- function(df, N, delta = 0) {

    df <- select(df, region, unit, rca)

    ##################################
    ### DEFINE ALGORITHM VARIABLES ###
    ##################################

    ## country-product RCA matrix: M
    df_spread <- df %>%
        spread(key = unit, value = rca) %>%
        column_to_rownames(var = 'region') %>%
        as.matrix()

    ## constants
    P <- ncol(M) # number of products
    C <- nrow(M) # number of countries

    ## define return frames
    fitness_df <- matrix(NA, nrow = N, ncol = C) %>%
        as_tibble()
    colnames(fitness_df) <- rownames(M)

    complexity_df <- matrix(NA, nrow = N, ncol = P) %>%
        as_tibble()
    colnames(complexity_df) <- colnames(M)

    #######################
    ### APPLY ALGORITHM ###
    #######################

    ## initial conditions
    F_N <- rep(1, C)
    Q_N <- rep(1, P)

    for(i in 1:N) {

        ## the sweep function makes performs element-wise vector-matrix multiplication,
        ## one row at a time
        F_tilde <- (delta^2) + rowSums(sweep(M, MARGIN = 2, (1 / Q_N)))
        Q_tilde <- 1 + rowSums(sweep(t(M), MARGIN = 2, (1 / F_N), '*'))

        ## normalize
        F_N <- F_tilde
        Q_N <- Q_tilde

        ## collect
        fitness_df[i, ] <- F_N
        complexity_df[i, ] <- Q_N

    }

    #####################
    ### FORMAT RETURN ###
    #####################

    fitness_tidy <- fitness_df %>%
        gather(key = entity, value = value) %>%
        mutate(
            ent_type = "region",
            val_metric = "fitness"
        )

    complexity_tidy <- complexity_df %>%
        gather(key = entity, value = value) %>%
        mutate(
            ent_type = "unit",
            val_metric = "complexity"
        )

    return_tidy <- bind_rows(fitness_tidy, complexity_tidy) %>%
        select(entity, ent_type, value, val_metric)

    return(return_tidy)
}
