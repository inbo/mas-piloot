#' Get a layer from a web coverage service within a bounding box
#'
get_feature_wfs <- function(
  wfs,
  version = "2.0.0",
  layername,
  crs,
  bbox = NULL,
  filter = NULL,
  cql_filter = NULL,
  output_format = NULL,
  property_name = NULL,
  result_type = c("results", "hits")
  ) {
  require(httr)
  require(sf)
  result_type <- match.arg(result_type)
  url <- parse_url(wfs)
  if (grepl(pattern = "^2", x = version)) {
    url$query <- list(service = "wfs",
                      request = "GetFeature",
                      typeNames = layername,
                      srsName = crs,
                      bbox = bbox,
                      filter = filter,
                      cql_filter = cql_filter,
                      outputFormat = output_format,
                      propertyName = property_name,
                      resultType = result_type
    )
  }
  if (grepl(pattern = "^1", x = version)) {
    url$query <- list(service = "wfs",
                      request = "GetFeature",
                      typeName = layername,
                      srsName = crs,
                      bbox = bbox,
                      filter = filter,
                      cql_filter = cql_filter,
                      outputFormat = output_format,
                      propertyName = property_name,
                      resultType = NULL
    )
  }

  request <- build_url(url)

  if (result_type == "hits") {
    result <- GET(request)
    parsed <- xml2::as_list(content(result, "parsed", encoding = "UTF-8"))
    n_features <- attr(parsed$FeatureCollection, "numberMatched")
    return(n_features)
  } else {
    result <- read_sf(request)
    return(result)
  }
}





#' Get a layer from a web coverage service within a bounding box
#'
get_coverage_wcs <- function(wcs = c("dtm", "omz", "dsm"),
                          bbox, #xmin, xmax, ymin, ymax
                          layername,
                          resolution,
                          wcs_crs = "EPSG:4258",
                          output_crs = "EPSG:31370",
                          bbox_crs = "EPSG:31370",
                          version = c("1.0.0", "2.0.1")) {
  # prelim check
  version <- match.arg(version)
  wcs <- match.arg(wcs)
  wcs_crs <- match.arg(wcs_crs)
  bbox_crs <- match.arg(bbox_crs)

  # set url
  wcs <- switch(
    wcs,
    omz = "https://inspire.informatievlaanderen.be/overdrachtdiensten/oi-omz/wcs",
    dtm = "https://inspire.informatievlaanderen.be/overdrachtdiensten/el-dtm/wcs",
    dsm = "https://inspire.informatievlaanderen.be/overdrachtdiensten/el-dsm/wcs"
  )

  assertthat::assert_that(is.character(layername))
  assertthat::assert_that(is.character(output_crs))
  assertthat::assert_that(
    is.vector(bbox, mode = "numeric"),
    length(bbox) == 4)

  names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

  assertthat::assert_that(is.numeric(resolution))

  # build url request
  url <- parse_url(wcs)

  if (version == "2.0.1") {
    stop(paste0("code for version = ", version, "is not yet working"))
    url$query <- list(SERVICE = "WCS",
                      VERSION = version,
                      REQUEST = "GetCoverage",
                      COVERAGEID = layername,
                      CRS = wcs_crs,
                      SUBSET = paste0("x,http://www.opengis.net/def/crs/EPSG/0/31370(",
                                      bbox["xmin"],
                                      ",",
                                      bbox["xmax"],")"),
                      SUBSET = paste0("y,http://www.opengis.net/def/crs/EPSG/0/31370(",
                                      bbox["ymin"],
                                      ",",
                                      bbox["ymax"],")"),
                      #SCALEFACTOR = 50,
                      FORMAT = "image/tiff",
                      RESPONSE_CRS = output_crs
    )
    request <- build_url(url)
    # download een mht bestand met tif erin
    # geen idee hoe deze tif uit mht te halen
    file <- tempfile(fileext = ".mht")
    GET(url = request,
        write_disk(file))
  }

  if (version == "1.0.0") {
#    result_width <- (bbox["xmax"] - bbox["xmin"]) / resolution
#    result_height <- (bbox["ymax"] - bbox["ymin"]) / resolution

    matrix(bbox, ncol = 2, byrow = FALSE) %>%
      as.data.frame() %>%
      st_as_sf(coords = c("V1", "V2"), crs = bbox_crs) %>%
      st_transform(crs = wcs_crs) %>%
      st_coordinates() %>%
      as.vector() -> bbox

    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")

    url$query <- list(SERVICE = "WCS",
                      VERSION = version,
                      REQUEST = "GetCoverage",
                      COVERAGE = layername,
                      CRS = wcs_crs,
                      BBOX = paste(
                        bbox["xmin"],
                        bbox["ymin"],
                        bbox["xmax"],
                        bbox["ymax"],
                        sep = ","),
#                      WIDTH = result_width,
#                      HEIGHT = result_height,
                      RESX = resolution,
                      RESY = resolution,
                      FORMAT = "geoTIFF",
                      RESPONSE_CRS = wcs_crs
    )
    request <- build_url(url)
    file <- tempfile(fileext = ".tif")
    GET(url = request,
        write_disk(file))
  }

  raster <- terra::rast(file)
  template <- terra::project(raster, output_crs)
  res(template) <- resolution
  raster <- terra::project(raster, template)
  return(raster)
}


unpack_mht <- function(path) {
  #not yet working

  lines <- readLines(path)
  assertthat::assert_that(any(stringr::str_detect(lines, "image/tiff")))
  start <- which(stringr::str_detect(lines, "</GDALMetadata>")) + 1
  end <- length(lines) - 1
  tif <- lines[start:end]
  base64enc::base64decode(
    what = tif,
    output = file(stringr::str_replace(path, "mht", "tif"), "wb"))
}
