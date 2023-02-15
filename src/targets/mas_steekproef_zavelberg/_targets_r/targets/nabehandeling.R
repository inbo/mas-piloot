list(
  tar_target(
    steekproef_nn,
    nn_steekproef(sample = steekproef,
                  max_dist = 300),
    pattern = map(steekproef)
  ),
  tar_target(
    output_objecten,
    output_finaal(files = list(steekproefkader = steekproefkader_finaal, 
                               steekproef = steekproef_nn),
                  write_out = FALSE)
  )
)
