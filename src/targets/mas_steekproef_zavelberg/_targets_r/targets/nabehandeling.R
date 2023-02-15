list(
  tar_target(
    steekproef_zichtbaarheid,
    filter_zichtbaarheid(sample = steekproef,
                         min_cvvi = 0.1,
                         resolution = 5,
                         spacing = 10),
    pattern = map(steekproef)
  ),
  tar_target(
    steekproef_nn,
    nn_steekproef(sample = steekproef_zichtbaarheid,
                  max_dist = 300),
    pattern = map(steekproef_zichtbaarheid)
  ),
  tar_target(
    output_objecten,
    output_finaal(files = list(steekproefkader = steekproefkader_finaal, 
                               steekproef = steekproef_nn),
                  write_out = FALSE)
  )
)
