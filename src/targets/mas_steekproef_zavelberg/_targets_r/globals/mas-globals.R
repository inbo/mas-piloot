options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("dplyr",
                            "readr",
                            "tidyr",
                            "tibble",
                            "nngeo", 
                            "osmextract",
                            "terra",
                            "exactextractr",
                            "arrow",
                            "httr",
                            "sf",
                            "GVI"),
               format = "qs",
               memory = "transient",
               garbage_collection = TRUE,
               workspace_on_error = TRUE)

target_dir <- rprojroot::find_root_file(
    "src", "targets", "mas_steekproef_zavelberg",
    criterion = rprojroot::is_git_root)
mbag_dir <- rprojroot::find_root_file(
    criterion = rprojroot::is_git_root)

source(file.path(mbag_dir, "src", "R", "landuse_maps.R"))
source(file.path(mbag_dir, "src", "R", "geocomputations.R"))
source(file.path(mbag_dir, "src", "R", "draw_sample.R"))
source(file.path(mbag_dir, "src", "R", "steekproefkader.R"))
source(file.path(mbag_dir, "src", "R", "berekening_hulpvariabelen.R"))
source(file.path(mbag_dir, "src", "R", "steekproeftrekking_nabehandeling.R"))
source(file.path(mbag_dir, "src", "R", "wfs_wcs.R"))
