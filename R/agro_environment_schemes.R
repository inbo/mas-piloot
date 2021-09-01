#' Read shapefile of parcels with agroenvironmental schemes
#'
#' @param year integer year
#' @param path path to where 'beheerovereenkomsten' shapefiles are located
#'
#' @return
#' @export
#'
#' @examples
read_aes <- function(
  year = 2020,
  path = "Z:/Projects/PRJ_NARA_2020/Target_3a/Agrecosysteem/"
) {
  shapefile <- fs::dir_ls(
    file.path(path),
    recurse = 0,
    regexp = paste0(".+_", year, "_.+\\.shp$"))

  shapefile <- shapefile[!grepl("Copy|COPY|copy", shapefile)]

  aes <- sf::read_sf(shapefile)
  return(aes)
}

