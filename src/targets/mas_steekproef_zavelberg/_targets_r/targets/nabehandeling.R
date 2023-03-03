list(
  tar_target(
    steekproef_thinned,
    thin_sample(
      sample = steekproef,
      thin_dist = 300
      ),
    pattern = map(steekproef)
  ),
  tar_target(
    output_objecten,
    output_finaal(
      files = list(steekproefkader = steekproefkader_finaal, 
                   steekproef_thin = steekproef_thinned),
      write_out = TRUE
      )
  )
)
