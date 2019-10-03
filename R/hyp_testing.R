#' Non-parametric means test
#'
#' @param df data frame from the rel_* set
#' @param t0_threshold the t0_threshold used to compute is_new
#' @param trials the number samples to be drawn in the monte carlo.
#'
#' @import purrr
#' @import dplyr
#' @import tidyr
#' @import foreach
#' @import doRNG
#' @import iterators
#'
#' @export


null_samples <- function(df, t0_threshold, trials) {

  ## drop eval year
dframe <- df %>%
  dplyr::filter(time == range(df$time[1]))

# select set of new units
new_units <- dframe %>% dplyr::filter(is_new == 1)

# select set of potentially new units
potential_units <- dframe %>% dplyr::filter(rca < t0_threshold)

# create sampling_list
sampling_list <- new_units %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    n_new = dplyr::n()
  )

# create nest of region-wise pot units
potential_nest <- potential_units %>%
  group_by(region) %>%
  nest() %>%
  left_join(sampling_list) %>%
  drop_na(n_new)

sample_fun <- function(x, y) {
  dplyr::sample_n(tbl = x, size = y, replace = FALSE)
}

## run loop
null_samplings <- foreach(i = 1:trials, .packages = c("dplyr", "purrr")) %dorng% {

current_trial <- potential_nest %>%
  dplyr::mutate(
    samples = purrr::map2(data, n_new, sample_fun)
    )
  current_trial %>% tidyr::unnest(samples)

  }

return(null_samplings)

}
