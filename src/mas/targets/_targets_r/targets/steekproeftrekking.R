list(
  tarchetypes::tar_group_by(
    steekproefkader_per_stratum,
    steekproefkader_finaal,
    is_sbp,
    openheid_klasse
  ),
  tar_target(
    allocatie_df,
    allocatie(steekproefkader = steekproefkader_finaal,
              min_samplesize = 30,
              target_samplesize = 300,
              allocatie_prop_minimum = 0.01,
              allocatie_binnen_sbp = 0.5)
  ),
  tarchetypes::tar_group_by(
    allocatie_per_stratum,
    allocatie_df,
    is_sbp,
    openheid_klasse
  ),
  tar_target(
    steekproef,
    draw_sample(sampling_frame = steekproefkader_per_stratum,
                sample_size = allocatie_per_stratum$samplesize * 2),
    pattern = map(steekproefkader_per_stratum, allocatie_per_stratum)
  )
)
