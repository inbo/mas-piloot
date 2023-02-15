steekproef_uitdunnen <- function(
  steekproef_sf) {
  te_verwijderen <- steekproef_sf %>%
    filter(!is.na(nn_index)) %>%
    arrange(nn_distance, batch, sample_order) %>%
    group_by(nn_distance) %>%
    slice_tail() %>%
    st_drop_geometry()

  out <- steekproef_sf %>% anti_join(te_verwijderen)
  return(out)
}

nn_steekproef <- function(sample,
                          max_dist) {
  nn <- nngeo::st_nn(sample, sample,
                     maxdist = max_dist,
                     sparse = TRUE,
                     k = 2,
                     returnDist = TRUE)
  index <- vapply(nn$nn,
                  FUN = function(x) ifelse(length(x) > 1, x[2], NA),
                  FUN.VALUE = c(1))
  distance <- vapply(nn$dist,
                     FUN = function(x) ifelse(length(x) > 1, x[2], NA),
                     FUN.VALUE = c(1))
  nn_result <- sample %>%
    mutate(nn_index = index,
           nn_distance = distance)

  uitdunnen <- steekproef_uitdunnen(nn_result)

  return(uitdunnen)
}

output_finaal <- function(files, write_out) {
  if (write_out) {
    fs::dir_create("output")
    for (i in seq_along(files)) {
      name <- names(files)[i]
      object <- files[[i]]

      qs::qsave(object, file = paste0("output/", name))
    }
  }
  return(write_out)
}
