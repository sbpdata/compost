#' Find "new" entries in time series data
#'
#' Calculates if a unit is 'new' in a region through two threshold conditions.
#' t0 threshold refer to the current time period, t1 refer
#' to the next. A product is given value 1 for "new" in the t0 period. In other
#' words, if the unit is newly introduced in t1, the t0 value has a "new" value.
#' This seems unintutive, but this makes it easier to extract the relatedness
#' metrics for the t0-period in analysis. The input data must contain
#' the four variables listed.
#'
#' @param data Data frame containing variables for time (fx year),
#' region (fx country), unit (fx product) and rca (revealed comparative
#' advantage or location quotient). All variables must be present. Data
#' should follow the tidy format.
#' @param t0_threshold Numeric of length 1. The value that rca should be below in
#' current period, in order to be 'new'.
#' @param t1_threshold Numeric of length 1. The value that rca should be above in
#' next period, in order to be 'new' (only if t0_threshold also holds).
#'
#' @return data frame that contains the original values plus two new:
#' previous_rca, which is the rca value for the region in the unit the
#' previous time-period, and is_new, which takes the value 1 if the
#' threshold conditions are passed, 0 if not. is_new is NA if there is no
#' next rca value present (i.e. for all values in the latest time period).
#'
#' @import dplyr
#' @import tidyr
#'
#' @export

is_new <- function(data, t0_threshold = 0.5, t1_threshold = 1) {

  # 1A: check for missing variables
    if (!all(c("time", "region", "unit", "rca") %in% names(data))) {
        stop("`data` must contain `time`, `region`, `unit` and `rca` columns")
    }

  # 1B: check for missing values in variables
  # Force NA's for incomplete data
    data <- data %>%
        group_by(time) %>%
        complete(region, unit)


    if (sum(is.na(data)) != 0) {
        stop("There are NA values in `data`. This introduces problems in grouping and arranging.")
    }

  # 2: check for variable types
  if (!(data %>% pull(time) %>% is.numeric())) {
    stop("time is not of numeric type. Stopping.")
  }

  if (!(data %>% pull(region) %>% is.character())) {
    stop("region is not of character type. Stopping.")
  }

  if (!(data %>% pull(unit) %>% is.character())) {
    stop("unit is not of character type. Stopping.")
  }

  if (!(data %>% pull(rca) %>% is.numeric())) {
    stop("rca is not of numeric type. Stopping.")
  }

  # 3: check thresholds
  if (length(t0_threshold) != 1 & !is.numeric(t0_threshold)) {
    stop("t0_threshold is not a single integer. Stopping")
  }

  if (length(t1_threshold) != 1 & !is.numeric(t1_threshold)) {
      stop("t1_threshold is not a single integer. Stopping")
  }

 # 4: classify values as 1 for new, or 0 for not new
    df <- data %>%
        group_by(region, unit) %>%
        arrange(region, unit, time) %>%
        mutate(
            next_rca = lead(rca, n = 1),
            is_new = case_when(
                is.na(next_rca) ~ NA_real_,
                rca < t0_threshold & next_rca >= t1_threshold ~ 1,
                TRUE ~ 0
                )
            )

    return(df)
}
