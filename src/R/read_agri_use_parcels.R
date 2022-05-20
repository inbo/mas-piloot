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

  zip1 <- ifelse(
    year >= 2018,
    paste0(year, "/Landbouwgebruikspercelen_LV_", year),
    paste0(year, "/Landbouwgebruikspercelen_ALV_", year)
    )

  zip2 <- c("_GewVLA_Shapefile.zip", "_GewVLA_Shape.zip",
            "_Shape.zip", "_Shapefile.zip")

  zip <- paste0(zip1, zip2)

  download_url <- paste0(base_url, zip)
  download_url  <- download_url[!purrr::map_lgl(download_url, httr::http_error)]
  file <- tempfile(fileext = ".zip")
  httr::GET(download_url,
            httr::write_disk(file))
  utils::unzip(file, exdir = path)
  unlink(file)
  writeLines(text = "*\n!/.gitignore",
             con = file.path(path, ".gitignore"))
}


#' Download 'landbouwstreken' from the agiv download API
#'
#'
#' @param path path to the folder where data will be unzipped.
#' Default ./data/landbouwstreken
#'
#' @details the output folder will contain multiple files, including GML file and
#' metadata
#' @export
#'
#' @examples
download_landbouwstreken <- function(path = "./data/landbouwstreken") {

  base_url <-
    "https://downloadagiv.blob.core.windows.net/landbouwstreken-belgie-toestand-1974-02-15/"

  zip <- "Landbouwstreken_Belgie_Shapefile.zip"

  download_url <- paste0(base_url, zip)
  download_url  <- download_url[!purrr::map_lgl(download_url, httr::http_error)]
  file <- tempfile(fileext = ".zip")
  httr::GET(download_url,
            httr::write_disk(file))
  utils::unzip(file, exdir = path, junkpaths = TRUE)
  unlink(file)
  writeLines(text = "*\n!/.gitignore",
             con = file.path(path, ".gitignore"))
}



#' Read agricultural use data
#'
#'
#' @param path path to the vector data file
#' @param query SQL query to pass in. Default NA.
#'
#' @details beware: the full dataset in memory takes about 1.5 Gb. That is why
#' lazysf::lazysf() is used so the object returned is just a pointer to the data
#' on disk, but not the data in memory.
#'
#' @return a `lazy sf` object containing the parcel polygons and attributes
#' @export
#'
#' @examples
aup_read <- function(path, query = NA) {
  assertthat::assert_that(!missing(path))
  sfobject <- lazysf::lazysf(path, query = query, quiet = TRUE)
  return(sfobject)
}




#' Convert agri-use parcels to parquet
#'
#' @param input path to the shapefile
#' @param dsn data source name. A path and file name with .parquet extension
#' @param query SQL statement
#'
#' @return
#' @export
#'
#' @examples
aup_sf_to_parquet <- function(
  input,
  dsn,
  query = NA) {
  fs::dir_create(fs::path_dir(dsn), recurse = TRUE)
  fs::file_create(dsn)
  temp <- sf::read_sf(input, query = query)
  sfarrow::st_write_parquet(obj = temp, dsn = dsn, chunk_size = 10000)
}
