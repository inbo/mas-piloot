list(
  tar_target(
    steekproef_zichtbaarheid,
    filter_zichtbaarheid(sample = steekproef,
                         min_cvvi = 0.1),
    pattern = map(steekproef)
  ),
  tar_target(
    steekproef_nn,
    nn_steekproef(sample = steekproef_zichtbaarheid,
                  max_dist = 300),
    pattern = map(steekproef_zichtbaarheid)
  )
)
