list(
  tar_target(
    name = plus_sb,
    command = add_bo_to_frame(
      punten_df = punten_selectie_zichtbaarheid,
      path_bo = bo_file
      ),
    pattern = map(punten_selectie_zichtbaarheid)
  ),
  tar_target(
    name = lbg_statistics,
    command = calc_lbg(
      path = lbg_file,
      punten_sf = punten_selectie_zichtbaarheid
      ),
    pattern = map(punten_selectie_zichtbaarheid)
  ),
  tar_target(
    name = plus_openheid_landschap,
    command = add_openheid_landschap_to_frame(
      path = openheid_landschap_file,
      punten_sf = plus_sb,
      gebied = perimeters_data,
      cutlevels = c(1.25, 1.35, 1.51),
      class_labels = c("GL", "HGL", "HOL", "OL")
      ),
    pattern = map(perimeters_data, plus_sb)
  ),
  tar_target(
    name = sbp_akkervogels,
    command = read_sbp_akkervogels(
      path = sbp_akkervogels_file,
      gebied = perimeters_data,
      path_extra_soorten = sbp_overige_file,
      extra_soorten = c("hamster", "bruine kiekendief", "zomertortel", 
                        "grauwe kiekendief")
      ),
    pattern = map(perimeters_data)
  ),
  tar_target(
    name = steekproefkader_finaal,
    command = add_stratum_sbp(
      punten_sf = plus_openheid_landschap,
      sbp = sbp_akkervogels
      ),
    pattern = map(plus_openheid_landschap)
  )
)
