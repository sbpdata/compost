#' Compute and write all complexity metrics for a data set
#'
#' Function that applies a number of intermediary functions (mostly varieties of wrap_long) to
#' produces a series of data frames, that are written to file. The data frames that are written
#' contains A) RCA values, B) similarity values, C) relatedness values (for all five metrics).
#'
#' @param df a data frame with four variables: time, region, unit, intensity
#' @param country the country from where the data comes (fx "denmark")
#' @param output_dir the directory the produced files should be written to (fx here("data/produced/country/"))
#' @param sim_method the method used to compute similarities. Options are "proximity", "jaccard", "association"
#' @param t0_year reference year (fx 2000)
#' @param t1_year evaluation year (fx 2005)
#' @param t0_threshold RCA value that units should be under in t0_year to be eligible for "new" in t1.
#' @param t1_threshold RCA value that units should be equal to or over in t1_year to be eligible for "new" in t1.
#'
#' @import dplyr
#' @export

collect_metrics <- function(df, country, output_dir, sim_method, t0_year, t1_year, t0_threshold, t1_threshold) {


###########################################################################
### check input
###########################################################################

  country <- tolower(country)
  sim_method <- tolower(sim_method)

  stopifnot(is.data.frame(df))

  stopifnot(sim_method %in% c("proximity", "jaccard", "association"))

  stopifnot(is.numeric(t0_year) & length(t0_year) == 1)
  stopifnot(is.numeric(t1_year) & length(t1_year) == 1)

  stopifnot(is.numeric(t0_threshold) & length(t0_threshold) == 1)
  stopifnot(is.numeric(t1_threshold) & length(t1_threshold) == 1)

###########################################################################
### compute metrics
###########################################################################

  similarity_type <- sim_method

  ref_year <- t0_year
  eval_year <- t1_year

  t0_th <- t0_threshold
  t1_th <- t1_threshold

    ## filter to period
  df <- filter(df, time %in% c(ref_year, eval_year))

    ## compute RCAs
  binary_rca <- wrap_long(fun = "get_rca", int_df = df, args = list(binary = TRUE))
  non_binary_rca <- wrap_long(fun = "get_rca", int_df = df, args = list(binary = FALSE))

    ## compute is-new
  df_new <- is_new(data = non_binary_rca, t0_threshold = t0_th, t1_threshold = t1_th)

    ## compute similarities
  similarity_df <- wrap_long(fun = "get_similarity", rca_df = binary_rca, args = list(method = similarity_type))

    ## compute relatedness

  closest <- wrap_long(fun = "get_closest_sim", rca_df = binary_rca, sim_df = similarity_df) %>%
      rename(closest_sim = closest_similarity)

  avg_i0 <- wrap_long(fun = "get_average_sim", rca_df = binary_rca, sim_df = similarity_df, args = list(exclude0 = FALSE)) %>%
    rename(avg_sim_i0 = avg_similarity)

  avg_e0 <- wrap_long(fun = "get_average_sim", rca_df = binary_rca, sim_df = similarity_df, args = list(exclude0 = TRUE)) %>%
    rename(avg_sim_e0 = avg_similarity)

  w_avg <- wrap_long(fun = "get_weighted_average_sim", rca_df = non_binary_rca, sim_df = similarity_df) %>%
    rename(w_avg_sim = w_avg_similarity)

  density_df <- wrap_long(fun = "get_density", rca_df = binary_rca, sim_df = similarity_df, args = list(binary = TRUE))

    ## join rel metrics and is_new
    join_rel <- closest %>%
        left_join(avg_i0) %>%
        left_join(avg_e0) %>%
        left_join(w_avg) %>%
        left_join(density_df) %>%
        left_join(df_new)

    join_rca <- binary_rca %>%
        rename(binary_rca = rca) %>%
        left_join(non_binary_rca)

###########################################################################
    ## write files
###########################################################################

    ## write join rca
    rca_path <- paste0(output_dir, "rca_", country, "_", ref_year, eval_year, ".csv")
    write_csv(join_rca, rca_path)


    ## write sim
    sim_path <- paste0(output_dir, similarity_type, "_", country, "_", ref_year, eval_year, ".csv")
    write_csv(similarity_df, sim_path)

    ## write joined
    rel_path <- paste0(output_dir, "rel_", country, "_", ref_year, eval_year, ".csv")
    write_csv(join_rel, rel_path)

    paste0("RCA file is saved as '", rca_path, "'. Similarity file is saved as '", sim_path, "'. Relatedness file is saved as '", rel_path, "'")
}
