list(
  tar_target(
    name = target_size,
    command = calc_target_samplesize(gebied = selectie_openheid_klasses),
    pattern = map(selectie_openheid_klasses)
  ),
  tar_target(
    allocatie_df,
    allocatie(
      steekproefkader = steekproefkader_finaal,
      min_samplesize = 200,
      target_samplesize = target_size,
      popsize_minimum = 200,
      allocatie_binnen_sbp = 0.5,
      allocatie_leemstreek = 0,
      ol_strata = c("OL", "HOL")
      ),
    pattern = map(steekproefkader_finaal, target_size)
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
      semi_join(allocatie_df %>%
                  select(Naam, is_sbp, openheid_klasse),
                by = c("Naam", "is_sbp", "openheid_klasse")),
    Naam,
    is_sbp,
    openheid_klasse
  ),
  tar_target(
    steekproef,
    draw_sample(
      sampling_frame = steekproefkader_per_stratum,
      sample_size_multiplication = 1,
      balance = c("X", "Y", "area_prop_sb")
      ),
    pattern = map(steekproefkader_per_stratum, allocatie_per_stratum)
  )
)
