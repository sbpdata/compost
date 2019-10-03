#' Helper function for fitness-algorithm functions for multi-year data
#'
#' Nests data by year and applies the chosen algorithm
#'
#' @param df data frame containing the necessary columns:
#' time, region, unit, rca
#' @param algorithm character listing the algorithm to be applied.
#' Possible values are: "fitness2012" or "fitness2018"
#' @param N number of iterations to run algorithm
#'
#' @return see algorithm functions
#'
#' @export

get_fitness <- function(df, algorithm, N) {

    nest_df <- df %>%
        group_by(time) %>%
        nest()

    if(algorithm == "fitness2012") {
        nest_df <- nest_df %>%
            mutate(fitness = map(data, fitness2012, N = N))

    } else if (algorithm == "fitness2018") {
        nest_df <- nest_df %>%
            mutate(fitness = map(data, fitness2018, N = N))
    }

    return_df <- nest_df %>%
        unnest(fitness)

    return(return_df)
}
