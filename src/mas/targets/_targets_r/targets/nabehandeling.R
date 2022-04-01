list(
  tar_target(
    steekproef_nn,
    nn_steekproef(sample = steekproef,
                  max_dist = 300)
  ),
  tar_target(
    steekproef_zichtbaarheid,
    bereken_zichtbaarheid(steekproef_nn,
                          dist = 300,
                          obs_height = 1.7,
                          resolution = 1),
    pattern = map(steekproef_nn)
  )
)
