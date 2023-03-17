list(
  tarchetypes::tar_map(
    list(thin_dists = c(300, 600)),
    tar_target(
      steekproef_thinned,
      thin_sample(
        sample = steekproef,
        thin_dist = thin_dists
        ),
      pattern = map(steekproef)
    ),
    tar_target(
      steekproef_final,
      replace_by_existing(
        sample = steekproef_thinned,
        existing_points = existing_data,
        overlap_prop = 0.5,
        sbp_file = sbp_akkervogels
        ),
      pattern = map(steekproef_thinned)
    )
  )
)
