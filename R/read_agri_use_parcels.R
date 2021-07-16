#' Download agricultural use parcels (aup) data from the agiv download API
#'
#'
#' @param year Year of data collection
#' @param path path to the folder where data will be unzipped.
#' Default ./data/landbouwgebruikspercelen
#'
#' @details the output folder will contain multiple files, including GML file and
#' metadata
#' @export
#'
#' @examples
aup_download <- function(year = 2020,
                                      path = "./data/landbouwgebruikspercelen") {

  base_url <- "https://downloadagiv.blob.core.windows.net/landbouwgebruikspercelen/"

  zip <- paste0(year, "/Landbouwgebruikspercelen_LV_", year, "_GewVLA_Shapefile.zip")

  download_url <- paste0(base_url, zip)
  file <- tempfile(fileext = ".zip")
  httr::GET(download_url,
            httr::write_disk(file))
  utils::unzip(file, exdir = path)
  unlink(file)
  writeLines(text = "*\n!/.gitignore",
             con = file.path(path, ".gitignore"))
}


#' Read agricultural use data
#'
#'
#' @param path path to the vector data file
#'
#' @details beware: the full dataset in memory takes about 1.5 Gb. That is why
#' lazysf::lazysf() is used so the object returned is just a pointer to the data
#' on disk, but not the data in memory.
#'
#' @return a `lazy sf` object containing the parcel polygons and attributes
#' @export
#'
#' @examples
aup_read <- function(path) {
  assertthat::assert_that(!missing(path))
  sfobject <- lazysf::lazysf(path)
  return(sfobject)
}




#' Convert agri-use parcels to parquet
#'
#' @param input path to the shapefile
#' @param dsn data source name. A path and file name with .parquet extension
#'
#' @return
#' @export
#'
#' @examples
aup_sf_to_parquet <- function(
  input,
  dsn) {
  fs::dir_create(fs::path_dir(dsn), recurse = TRUE)
  fs::file_create(dsn)
  require(dplyr)
  sf::read_sf(input) %>%
    sfarrow::st_write_parquet(dsn = dsn)
}
