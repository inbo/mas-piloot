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
  sample_size_multiplication = 1,
  balance = c("X", "Y"),
  ips,
  seed = 1234,
  ...
) {

  sample_size_extra <- sample_size * sample_size_multiplication

  if (missing(ips)) {
    ips <- rep(sample_size_extra / nrow(sampling_frame), nrow(sampling_frame))
  }

  set.seed(seed)
  assertthat::assert_that(length(ips) == nrow(sampling_frame))
  assertthat::assert_that("pointid" %in% colnames(sampling_frame))
  assertthat::assert_that(sample_size_extra < nrow(sampling_frame))

  sampling_frame_df <- data.frame(
    st_coordinates(sampling_frame),
    st_drop_geometry(sampling_frame) %>%
      select(any_of(balance))
  )

  sampling_frame_df$pointid <- NULL
  sampling_frame_df$tar_group <- NULL
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
  sample_df <- sample_df %>%
    arrange(sample_order) %>%
    mutate(batch = ifelse(sample_order <= sample_size,
                          "eerste set" ,
                          "reserve set"))

  return(sample_df)
}

calc_target_samplesize <- function(gebied, telcirkel_radius = 300) {
  opp_telcirkel <- units::set_units(telcirkel_radius * telcirkel_radius * pi,
                                    m^2)
  target_size <- round(st_area(gebied) / opp_telcirkel, 0)

  return(units::drop_units(target_size))
}

allocatie <- function(steekproefkader,
                      min_samplesize = 0,
                      target_samplesize = 410,
                      popsize_minimum = 410,
                      allocatie_binnen_sbp = 0.5,
                      allocatie_leemstreek = 350/410,
                      ol_strata = c("OL", "HOL")) {
  allocatie <- steekproefkader %>%
    st_drop_geometry() %>%
    filter(openheid_klasse %in% ol_strata) %>%
    count(Naam, openheid_klasse, is_sbp, name = "popsize") %>%
    filter(popsize > popsize_minimum) %>%
    group_by(Naam) %>%
    mutate(allocatie_gebied = ifelse(Naam == "Oostelijke leemstreek",
                                     allocatie_leemstreek,
                                     1 - allocatie_leemstreek)) %>%
    group_by(Naam, is_sbp) %>%
    mutate(allocatie_sbp = ifelse(is_sbp, allocatie_binnen_sbp,
                                  1 - allocatie_binnen_sbp),
           allocatie_openheid = popsize / sum(popsize)) %>%
    ungroup() %>%
    mutate(
      allocatie = allocatie_gebied * allocatie_sbp * allocatie_openheid,
      samplesize = round(allocatie * target_samplesize)) %>%
    group_by(Naam) %>%
    mutate(
      targetsize = sum(samplesize),
      samplesize = pmax(samplesize, min_samplesize),
      excess = targetsize - sum(samplesize),
      samplesize = samplesize - round(allocatie * excess)) %>%
    ungroup()
  return(allocatie)
}
