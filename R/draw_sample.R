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
  sample_size = round(nrows(sampling_frame) / 20),
  ips,
  seed = 1234,
  ...
) {
  ips <- ips[[1]]
  sampling_frame <- sampling_frame[[1]]
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
