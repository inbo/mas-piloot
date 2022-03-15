#' Download land use maps from milieuinfo server
#'
#' @param year Character of length one. Either `'2019'` (default), `'2016'` or
#' `'2013'`
#' @param to folder to which files will be written
#'
#' @return
#' @export
#'
#' @examples
lum_download <- function(
  year = c("2019", "2016", "2013"),
  to = "data/landgebruik_vito"
) {
  year <- match.arg(year)

  url2019 <- "https://www.milieuinfo.be/dms/d/d/workspace/SpacesStore/95978609-c38b-4f51-8d61-5e893242feef/lu_landgebruik_vlaa_2019.zip"
  url2016 <- "https://www.milieuinfo.be/dms/d/d/workspace/SpacesStore/603cc702-ccba-4510-9159-6936aa29d6ac/lu_landgebruik_vlaa_2016_v2.zip"
  url2013 <- "https://www.milieuinfo.be/dms/d/d/workspace/SpacesStore/cda5c6d3-574b-4323-bb06-688d953738c0/lu_landgebruik_vlaa_2013_v2.zip"


  from <- switch(year,
                 "2019" = url2019,
                 "2016" = url2016,
                 "2013" = url2013)

  download_unzip(from = from, to = to)
  writeLines(text = "*\n!/.gitignore",
             con = file.path(to, ".gitignore"))
}


#testje <- sf::st_layers("Z:/Projects/PRJ_NARA_2020/Target_2/Extra_LGdatalagen.gdb")
#testje2 <- sf::st_layers("Z:/Projects/PRJ_NARA_2020/Basisbestanden/Basisdata.gdb")


#' read land use maps from inbo projects folder
#'
#' @param year one of "2013" or "2016"
#' @param layerid one of "finaal_natuurbeheer", "niveau1", "niveau2", "niveau3",
#' "niveau4"
#' @param root path to Projects/PRJ_NARA_2020/Basisbestanden/landgebruik_geotiff
#' @param add_levels Logical, default FALSE. If TRUE will map land use
#' categories to the numeric values.
#' This seems to seriously slow down subsequent operations.
#'
#' @return a terra::rast() object
#' @export
#'
#' @examples
lum_read_from_inbo <- function(
  year = c("2016", "2013"),
  layerid = c("finaal_natuurbeheer", "niveau1", "niveau2", "niveau3", "niveau4"),
  root = "data/landgebruik/inbo",
  add_levels = FALSE) {
  require(dplyr)
  require(readr)
  year <- match.arg(year)
  layerid <- match.arg(layerid)
  file <- paste0("LG", year, "_", layerid,".tif")
  tr <- terra::rast(file.path(root, file))
  # re-assign categories labels which got lost in translation
  # (ESRI -> geotiff -> terra::rast)
  if (add_levels) {
    legend <- read_csv2(
      "data/landgebruik/legende_landgebruik.csv",
      col_types = cols(
        bron = col_character(),
        bestand_id = col_character(),
        value = col_double(),
        label = col_character(),
        kleur = col_character()
      )) %>%
      filter(bestand_id == layerid) %>%
      select(ID = value, land_use = label) %>%
      as.data.frame()

    levels(tr) <- legend
  }

  return(tr)
}



#' read vito land use maps
#'
#' @param year one of "2019", "2016" or "2016"
#' @param root path to data/landgebruik/vito
#' @param add_levels Logical, default TRUE. If TRUE will map land use
#' categories to the numeric values.
#' @param add_colours LOgical, default TRUE. If TRUE will map colours to land
#' use
#'
#' @return a terra::rast() object
#' @export
#'
#' @examples
lum_read_from_vito <- function(
  year = c("2019", "2016", "2013"),
  root = "data/landgebruik/vito",
  add_levels = TRUE,
  add_colours = TRUE) {
  require(dplyr)
  require(readr)
  year <- match.arg(year)
  file <- paste0("lu_landgebruik_vlaa_", year, "_v2",".tif")
  tr <- terra::rast(file.path(root, file))
  # re-assign categories labels which got lost in translation
  # (ESRI -> geotiff -> terra::rast)
  if (add_levels) {
    oldwd <- setwd(root)
    legend <- read_csv2(
      file.path("..", "legende_landgebruik.csv"),
      col_types = cols(
        bron = col_character(),
        bestand_id = col_character(),
        value = col_double(),
        label = col_character(),
        kleur = col_character()
      )) %>%
      filter(bestand_id == "vito") %>%
      select(ID = value, land_use = label) %>%
      as.data.frame()
    setwd(oldwd)

    levels(tr) <- legend
  }
  if (add_colours) {
    oldwd <- setwd(root)
    legend <- read_csv2(
      file.path("..", "legende_landgebruik.csv"),
      col_types = cols(
        bron = col_character(),
        bestand_id = col_character(),
        value = col_double(),
        label = col_character(),
        kleur = col_character()
      )) %>%
      filter(bestand_id == "vito") %>%
      select(colour = kleur) %>%
      mutate(as_tibble(t(col2rgb(colour, alpha = TRUE)))) %>%
      select(-colour) %>%
      as.data.frame()
    setwd(oldwd)

    terra::coltab(tr) <- legend
  }

  return(tr)
}

