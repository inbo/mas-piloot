options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "sf", "nngeo", "readxl", "httr",
                            "osmextract", "terra", "exactextractr", "arrow"),
               format = "qs",
               memory = "transient",
               garbage_collection = TRUE,
               workspace_on_error = TRUE,
               workspaces = c("telcirkels_landgebruik_88468459")
               , debug = "telcirkels_landgebruik"
               )
target_dir <- rprojroot::find_root_file(
    "src", "mas", "targets",
    criterion = rprojroot::is_git_root)
mbag_dir <- rprojroot::find_root_file(
    criterion = rprojroot::is_git_root)
source(file.path(mbag_dir, "R", "wfs_wcs.R"))
source(file.path(mbag_dir, "R", "read_agri_use_parcels.R"))
source(file.path(mbag_dir, "R", "utils.R"))
source(file.path(mbag_dir, "R", "landuse_maps.R"))
source(file.path(mbag_dir, "R", "abv.R"))
source(file.path(mbag_dir, "R", "agro_environment_schemes.R"))
source(file.path(mbag_dir, "R", "geocomputations.R"))
source(file.path(mbag_dir, "R", "draw_sample.R"))
source(file.path(mbag_dir, "R", "steekproefkader.R"))

