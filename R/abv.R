#' Read ABV data
#'
#' @return a sf object with UTM 1 km squares selected for ABV
#' @export
#'
#' @examples
abv_read_utm_squares <- function() {
  require(dplyr)
  abv_grid <- readr::read_csv("./data/abv/data-1626258505755.csv",
                              na = c("", "NA", "NULL"))
  utm1belgium <- read_utm_sf()
  abv_grid_sf <- utm1belgium %>%
    inner_join(abv_grid,
               by = c("TAG" = "hok")) %>%
    relocate(TAG, geometry, .after = recall) %>%
    rename(utm_hok = TAG) %>%
    select(-Shape_Area, -Shape_Area)
  return(abv_grid_sf)
}


#' Reconstructed sampling frame of the ABV monitoring scheme
#'
#' @param file relative path to the tsv file
#'
#'
#' @return a data.frame
#' @export
#'
#' @examples
abv_read_sampling_frame <- function(file = "data/abv/sampling_frame.tsv") {
  git2rdata::read_vc(file)
}

