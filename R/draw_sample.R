#' Function to draw a spatially balanced sample from a sf object containing at
#' least a geometry column with point coordinates
#'
#' This is just a wrapper around `SamplingBigData::lpm2_kdtree` to return the
#' sample as a `sf` object
#'
#' @param sampling_frame A `sf` object with the spatial coordinates and possibly
#' other numeric variables from which a multidimensional balanced and
#' well-spread sample will be drawn.
#' The first column must be called `pointid` and is an identifier.
#' Each row is a candidate sampling unit.
#' Coordinates must be in projected coordinate system.
#' @param sample_size positive integer which must be less than the number of
#' rows in `sampling_frame`
#' @param ips Vector of inclusion probabilities (must sum to sample size).
#' Vector length equal to number of rows in `sampling_frame`.
#' @seed Integer. Random seed number.
#' @param ... passed on to SamplingBigData::lpm2_kdtree
#'
#' @importFrom BalancedSampling probabilities
#' @importFrom SamplingBigData lpm2_kdtree
#' @importFrom sf st_coordinates st_drop_geometry
draw_sample <- function(
  sampling_frame,
  sample_size = round(nrow(sampling_frame) / 20),
  ips,
  seed = 1234,
  ...
) {

  if (missing(ips)) {
    ips <- rep(sample_size / nrow(sampling_frame), nrow(sampling_frame))
  }

  sampling_frame <- sampling_frame
  set.seed(seed)
  assertthat::assert_that(length(ips) == nrow(sampling_frame))
  assertthat::assert_that("pointid" %in% colnames(sampling_frame))
  assertthat::assert_that(sample_size < nrow(sampling_frame))

  sampling_frame_df <- data.frame(
    st_coordinates(sampling_frame),
    st_drop_geometry(sampling_frame)
  )

  sampling_frame_df$pointid <- NULL
  assertthat::assert_that(all(sapply(sampling_frame_df, is.numeric)))
  sampling_frame_matrix <- as.matrix(sampling_frame_df)
  # scale the matrix so that all variables get same importance
  sampling_frame_matrix <- scale(sampling_frame_matrix)

  draw <- SamplingBigData::lpm2_kdtree(
    prob = ips,
    x = sampling_frame_matrix,
    inOrder = TRUE,
    ...
  )

  sampling_frame$in_sample <-
    ifelse(1:nrow(sampling_frame) %in% draw, TRUE, FALSE)
  sampling_frame$sample_order <- NA
  for (i in 1:length(draw)) {
    sampling_frame$sample_order[draw[i]] <- i
  }
  sample_df <- sampling_frame[sampling_frame$in_sample, ]
  return(sample_df)
}

allocatie <- function(steekproefkader,
                      min_samplesize = 30,
                      target_samplesize = 300,
                      allocatie_prop_minimum = 0.01,
                      allocatie_binnen_sbp = 0.5) {
  allocatie <- steekproefkader %>%
    st_drop_geometry() %>%
    count(openheid_klasse, is_sbp, name = "popsize") %>%
    mutate(allocatie_factor_sbp = ifelse(is_sbp,
                                         allocatie_binnen_sbp,
                                         1 - allocatie_binnen_sbp)) %>%
    group_by(is_sbp) %>%
    mutate(allocatie_factor_openheid = popsize / sum(popsize)) %>%
    ungroup() %>%
    mutate(allocatie = allocatie_factor_sbp * allocatie_factor_openheid) %>%
    filter(allocatie > allocatie_prop_minimum) %>%
    mutate(allocatie = allocatie / sum(allocatie),
           samplesize = min_samplesize +
             round(allocatie * (target_samplesize - n() * min_samplesize)))
  return(allocatie)
}
