#' Helper function to download a zip file from a URL and unzip it
#'
#' @param from url to download zip file from
#' @param to path to where the zip file needs to be extracted
#'
#' @return
#' @export
#'
#' @examples
download_unzip <- function(from, to) {
  file <- tempfile(fileext = ".zip")
  httr::GET(from,
            httr::write_disk(file))
  # complains if trailing '/'
  to <- sub("/$", "", x = to)
  utils::unzip(file, exdir = to)
  unlink(file)
}


#' Convert a big csv file to a grouped dataset consisting of parquet files
#'
#' @param csv path to the csv file
#' @param path directory path to write the parquet dataset to
#' @param partitioning A character vector of columns to use as partition keys
#'
#' @export
#'
#' @examples
csv_to_parquet <- function(csv,
                           schema = NULL,
                           partitioning = character(0),
                           path) {
  require(dplyr)
  arrow::open_dataset(
    sources = csv,
    format = "csv",
    schema = schema) %>%
    arrow::write_dataset(
      path = path,
      format = "parquet",
      partitioning = partitioning)
}


#' Get a raster layer from a Web Coverage Service
#'
#' @param wcs One of dtm, omz or mercator
#' @param bbox Numeric vector of length 4 with xmin, xmax, ymin and ymax
#' @param layername A layername (= CoverageId). If none is given the function
#' stop with a message listing available layernames
#' @param resolution the resolution in meters
#' @param crs the coordinate reference system. Default EPSG:31370 (Lambert 72).
#'
#' @return a terra::rast object
#' @export
#'
#' @examples
get_wcs_layer <- function(wcs = c("dtm", "omz", "mercator"),
                          bbox, #xmin, xmax, ymin, ymax
                          layername,
                          resolution,
                          crs = "EPSG:31370") {
  require(httr)
  # prelim check
  wcs <- match.arg(wcs)

  version <- ifelse(wcs == "mercator", "2.0.1", "1.0.0")

  # set url
  wcs <- switch(
    wcs,
    "omz" =   "https://inspire.informatievlaanderen.be/overdrachtdiensten/oi-omz/wcs",
    "dtm" = "https://inspire.informatievlaanderen.be/overdrachtdiensten/el-dtm/wcs",
    "mercator" = "https://www.mercator.vlaanderen.be/raadpleegdienstenmercatorpubliek/wcs"
  )


  if (missing(layername)) {
    url <- parse_url(wcs)
    url$query <- list(SERVICE = "WCS",
                      REQUEST = "GetCapabilities")
    request <- build_url(url)
    xml <- GET(url = request)
    xml <- xml2::read_xml(xml)
    xml <- xml2::xml_find_all(xml, xpath = "wcs:Contents/wcs:CoverageSummary/wcs:CoverageId")
    msg <- unlist(xml2::as_list(xml))
    msg <- paste("Please specify a layername from this list:",
                 paste(msg, collapse = "\n"),
                 collapse = "\n")
    options(warning.length = nchar(msg)[1])
    stop(msg)
  }

  assertthat::assert_that(is.character(layername))
  assertthat::assert_that(is.character(crs))
  assertthat::assert_that(
    is.vector(bbox, mode = "numeric"),
    length(bbox) == 4)

  names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

  assertthat::assert_that(is.numeric(resolution))

  # build url request
  url <- parse_url(wcs)

  if (version == "2.0.1") {
    url$query <- list(SERVICE = "WCS",
                      VERSION = version,
                      REQUEST = "GetCoverage",
                      COVERAGEID = layername,
                      CRS = crs,
                      SUBSET = paste0("X,http://www.opengis.net/def/crs/EPSG/0/31370(",
                                      bbox["xmin"],
                                      ",",
                                      bbox["xmax"],")"),
                      SUBSET = paste0("Y,http://www.opengis.net/def/crs/EPSG/0/31370(",
                                      bbox["ymin"],
                                      ",",
                                      bbox["ymax"],")"),
                      #SCALEFACTOR = 50,
                      FORMAT = "image/tiff",
                      RESPONSE_CRS = crs
    )
    request <- build_url(url)
  }

  if (version == "1.0.0") {
    result_width <- (bbox["xmax"] - bbox["xmin"]) / resolution
    result_height <- (bbox["ymax"] - bbox["ymin"]) / resolution

    url$query <- list(SERVICE = "WCS",
                      VERSION = version,
                      REQUEST = "GetCoverage",
                      COVERAGE = layername,
                      CRS = crs,
                      BBOX = paste(
                        bbox["xmin"],
                        bbox["ymin"],
                        bbox["xmax"],
                        bbox["ymax"],
                        sep = ","),
                      WIDTH = result_width,
                      HEIGHT = result_height,
                      FORMAT = "geoTIFF",
                      RESPONSE_CRS = crs
    )
    request <- build_url(url)
  }

  file <- tempfile(fileext = ".tif")
  GET(url = request,
      write_disk(file))

  raster <- terra::rast(file)
  return(raster)
}

#' read the UTM 1 km grid for Belgium
#'
#'
#' @return an sf polygon object
#' @export
#'
#' @examples
read_utm_sf <- function() {
  utm1belgium <- sf::read_sf(dsn = "S:/Belgie/Locatie/UTMroosters/utm1_bel.shp")
  return(utm1belgium)
}
