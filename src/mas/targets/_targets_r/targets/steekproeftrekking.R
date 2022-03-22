list(
  tar_target(
    allocatie_df,
    allocatie(steekproefkader = steekproefkader_finaal,
              min_samplesize = 0,
              target_samplesize = 410,
              allocatie_prop_minimum = 0.05,
              allocatie_binnen_sbp = 0.5,
              allocatie_leemstreek = 350/410,
              ol_strata = c("OL", "HOL"))
  ),
  tarchetypes::tar_group_by(
    allocatie_per_stratum,
    allocatie_df,
    Naam,
    is_sbp,
    openheid_klasse
  ),
  tarchetypes::tar_group_by(
    steekproefkader_per_stratum,
    steekproefkader_finaal %>%
      semi_join(allocatie_df
                %>%
                  select(Naam, is_sbp, openheid_klasse),
                by = c("Naam", "is_sbp", "openheid_klasse")),
    Naam,
    is_sbp,
    openheid_klasse
  ),
  tar_target(
    steekproef,
    draw_sample(sampling_frame = steekproefkader_per_stratum,
                sample_size = allocatie_per_stratum$samplesize,
                sample_size_multiplication = 2),
    pattern = map(steekproefkader_per_stratum, allocatie_per_stratum)
  ),
  tar_target(
    steekproef_nn,
    nn_steekproef(sample = steekproef,
                  max_dist =  300)
  )
)
