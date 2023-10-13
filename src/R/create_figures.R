roofvogels_f <- function() {
  roofvogels <- c("Blauwe Kiekendief", "Boomvalk", "Bruine Kiekendief",
                  "Buizerd",
                  "Grauwe Kiekendief", "Ransuil", "Slechtvalk", "Sperwer",
                  "Steenuil", "Steppekiekendief", "Torenvalk", "Velduil",
                  "Wespendief", "Zwarte Wouw")

  return(roofvogels)
}

plot_densiteit <- function(dsmodel, soort,
                           show_data = TRUE, year = 2022,
                           dsmodel2 = NULL,
                           obs_df,
                           design,
                           test = FALSE) {
  roofvogels <- roofvogels_f()

  # Calculate summary of dsmodel
  summary_dsmodel <- summary(dsmodel)

  summary_results_dsmodel <- bind_rows(
    summary_dsmodel$dht$individuals$D %>%
      mutate(variable = "density",
             type = "Aantal broedparen per 100 ha"),
    summary_dsmodel$dht$individuals$N %>%
      mutate(variable = "abundance",
             type = "Totaal aantal broedparen")
  ) %>%
    filter(Label != "Total")

  if (!is.null(dsmodel2)) {
    summary_results_dsmodel <- summary_results_dsmodel %>%
      mutate(Label = paste(Label, year[1], sep = " - "))

    summary_dsmodel2 <- summary(dsmodel2)

    summary_results_dsmodel2 <- bind_rows(
      summary_dsmodel2$dht$individuals$D %>%
        mutate(variable = "density",
               type = "Aantal broedparen per 100 ha"),
      summary_dsmodel2$dht$individuals$N %>%
        mutate(variable = "abundance",
               type = "Totaal aantal broedparen")
    ) %>%
      filter(Label != "Total") %>%
      mutate(Label = paste(Label, year[2], sep = " - "))
  }

  if ("jaar" %in% names(dsmodel$ddf$data)) {
    if (show_data) {
      plots_old <- design_2018_2023_mas %>%
        filter(!jaar %in% 2022:2023) %>%
        distinct(plotnaam) %>%
        pull()

      spec_presence <- obs_df %>%
        filter(plotnaam %in% plots_old)

      # Totale lijst van bezochte plots in 2018-2022
      bezoekenlijst <- design_2018_2023_mas_reduced  %>%
        select(-periode_in_jaar) %>%
        expand_grid(distinct(spec_presence, periode_in_jaar)) %>%
        arrange(plotnaam, jaar, periode_in_jaar)

      # Voeg afwezigheden toe door te mergen met alle bezoeken
      spec_df <- spec_presence %>%
        st_drop_geometry() %>%
        group_by(plotnaam, jaar, periode_in_jaar) %>%
        summarise(aantal = sum(aantal)) %>%
        full_join(bezoekenlijst, by = c("plotnaam", "jaar",
                                        "periode_in_jaar")) %>%
        replace(is.na(.), 0) %>%
        arrange(plotnaam, jaar, periode_in_jaar) %>%
        mutate(Region.Label = paste(regio, jaar, sep = " - "))

      # Oppervlakte telcirkels
      cirkelopp <- pi * 300^2

      average_df <- spec_df %>%
        # Bepaal voor elke plot het gemiddeld aantal individuen over
        # de telperiodes per jaar
        group_by(plotnaam, jaar, periode_in_jaar) %>%
        mutate(totaal = sum(aantal)) %>%
        group_by(plotnaam, jaar) %>%
        mutate(gemiddelde = mean(totaal)) %>%
        ungroup() %>%
        mutate(jaar = factor(jaar, ordered = TRUE)) %>%
        select(-c(periode_in_jaar, aantal, totaal)) %>%
        distinct() %>%

        # Deel cirkeloppervlakte om densiteit broedkoppels per plot te krijgen
        mutate(Estimate = gemiddelde / cirkelopp * 1e6)

      p <- ggplot() +
        stat_sum(data = average_df, aes(x = jaar, y = Estimate),
                 position = position_dodge(width = 0.5), alpha = 0.1,
                 colour = "firebrick")
    } else {
      p <- ggplot()
    }

    # Voeg resultaten dsmodel toe
    summary_results_dsmodel <- summary_results_dsmodel %>%
      separate(Label, into = c("regio", "jaar"), sep = " - ")

    summary_df <- summary_results_dsmodel %>%
      filter(variable == "density") %>%
      mutate(jaar = factor(jaar, ordered = TRUE)) %>%
      replace(. == 0, NA)

    # Plot
    p <- p +
      geom_point(data = summary_df, aes(x = jaar, y = Estimate), size = 3) +
      geom_errorbar(data = summary_df, aes(x = jaar, ymin = lcl, ymax = ucl),
                    width = 0.25) +
      facet_wrap(~regio) +
      theme(legend.position = "")

  } else {
    if (show_data) {
      spec_presence <- obs_df %>%
        filter(jaar %in% year)

      # Totale lijst van bezochte plots in bepaald jaar
      bezoekenlijst_year <- design %>%
        filter(jaar %in% year)

      # Voeg afwezigheden toe door te mergen met alle bezoeken
      spec_df <- spec_presence %>%
        st_drop_geometry() %>%
        group_by(plotnaam, jaar, periode_in_jaar) %>%
        summarise(aantal = sum(aantal), .groups = "drop") %>%
        full_join(bezoekenlijst_year, by = c("plotnaam",
                                             "jaar",
                                             "periode_in_jaar")) %>%
        replace(is.na(.), 0)

      # Oppervlakte telcirkels
      cirkelopp <- pi * 300^2



      if (test == TRUE) {
        veldleeuwerik_covars <- c("openheid", "jaar")
        detectiekans_df <- cbind(dsmodel$ddf$data,
                "detectiekans" = predict(dsmodel,
                                         se.fit = TRUE)$fitted,
                "standaardfout" = predict(dsmodel,
                                          se.fit = TRUE)$se) %>%
            mutate(jaar = 2022) %>%
            bind_rows(
              cbind(dsmodel2$ddf$data,
                    "detectiekans" = predict(dsmodel2,
                                             se.fit = TRUE)$fitted,
                    "standaardfout" = predict(dsmodel2,
                                              se.fit = TRUE)$se) %>%
                mutate(jaar = 2023)
            ) %>%
            select(all_of(veldleeuwerik_covars), "detectiekans") %>%
            distinct()

          set.seed(123)
          average_df <- spec_df %>%
            full_join(detectiekans_df, by = join_by(jaar, openheid)) %>%
            # Bepaal voor elke plot het gemiddeld aantal individuen over
            # de telperiodes per jaar
            group_by(plotnaam, jaar) %>%
            mutate(max = max(aantal),
                   gem = mean(aantal)) %>%
            ungroup() %>%
            mutate(gem = gem / detectiekans) %>%
            select(-c(periode_in_jaar, aantal)) %>%
            distinct() %>%
            group_by(regio, jaar, openheid, sbp) %>%
            mutate(bootstrap = mean_cl_boot(max)) %>%
            ungroup() %>%

            # Deel cirkeloppervlakte om densiteit broedkoppels per plot te krijgen
            mutate(Estimate = max / cirkelopp * 1e6,
                   gem_dens = gem  / cirkelopp * 1e6,
                   total_max_mean = bootstrap$y / cirkelopp * 1e6,
                   lcl_max_mean = bootstrap$ymin / cirkelopp * 1e6,
                   ucl_max_mean = bootstrap$ymax / cirkelopp * 1e6,
                   stratum = paste(openheid, sbp, sep = " - "),
                   jaar = as.character(jaar)) %>%
            select(-c(bootstrap)) %>%
            mutate(methode = "gemiddelde van maxima")
      } else {
        set.seed(123)
        average_df <- spec_df %>%
          # Bepaal voor elke plot het gemiddeld aantal individuen over
          # de telperiodes per jaar
          group_by(plotnaam, jaar) %>%
          mutate(max = max(aantal),
                 gem = mean(aantal)) %>%
          ungroup() %>%
          select(-c(periode_in_jaar, aantal)) %>%
          distinct() %>%
          group_by(regio, jaar, openheid, sbp) %>%
          mutate(bootstrap = mean_cl_boot(max)) %>%
          ungroup() %>%

          # Deel cirkeloppervlakte om densiteit broedkoppels per plot te krijgen
          mutate(Estimate = max / cirkelopp * 1e6,
                 gem_dens = gem  / cirkelopp * 1e6,
                 total_max_mean = bootstrap$y / cirkelopp * 1e6,
                 lcl_max_mean = bootstrap$ymin / cirkelopp * 1e6,
                 ucl_max_mean = bootstrap$ymax / cirkelopp * 1e6,
                 stratum = paste(openheid, sbp, sep = " - "),
                 jaar = as.character(jaar)) %>%
          select(-c(bootstrap)) %>%
          mutate(methode = "gemiddelde van maxima")
      }



      if (test == TRUE) {
        # Voeg resultaten dsmodel toe
        summary_results_dsmodel <- bind_rows(summary_results_dsmodel,
                                             summary_results_dsmodel2) %>%
          separate(Label, into = c("regio", "openheid", "sbp", "jaar"),
                   sep = " - ")

        summary_df <- summary_results_dsmodel %>%
          filter(variable == "density") %>%
          mutate(stratum = paste(openheid, sbp, sep = " - ")) %>%
          replace(. == 0, NA) %>%
          mutate(methode = "distance sampling") %>%
          full_join(average_df %>%
                      mutate(jaar = as.character(jaar)) %>%
                      select(jaar, regio, openheid, sbp, gem_dens),
                    by = join_by(regio, openheid, sbp, jaar)) %>%
          select(stratum, regio, jaar, "gemiddelde" = Estimate, lcl,
                 ucl, methode, "data" = gem_dens)

        average_df2 <- average_df %>%
          select(stratum, regio, jaar, "gemiddelde" = total_max_mean,
                 "lcl" = lcl_max_mean, "ucl" = ucl_max_mean, methode,
                 "data" = Estimate)

        p <- bind_rows(summary_df, average_df2) %>%
          ggplot() +
            stat_sum(aes(x = stratum, y = data, colour = methode),
                     position = position_dodge(width = 0.5), alpha = 0.1) +
            geom_point(aes(x = stratum, y = gemiddelde,
                           colour = methode), size = 2.5,
                       position = position_dodge(width = 0.5)) +
            geom_errorbar(aes(x = stratum, ymin = lcl,
                            ymax = ucl, colour = methode),
                          width = 0.25, position = position_dodge(width = 0.5)) +
            facet_grid(jaar~regio, scales = "free_x") +
            labs(colour = "Methode", x = "", y = "Aantal broedparen per 100 ha") +
            theme(legend.position = "top",
                  legend.background = element_rect(fill = "white",
                                                   color = "darkgrey"),
                  legend.margin = margin(6, 6, 6, 6))
        return(p)
      } else {
        p <- ggplot() +
          stat_sum(data = average_df, aes(x = stratum, y = Estimate,
                                          colour = jaar.f),
                   position = position_dodge(width = 0.5), alpha = 0.1)
      }

    } else {
      p <- ggplot()
    }

    if (is.null(dsmodel2)) {
      # Voeg resultaten dsmodel toe
      summary_results_dsmodel <- summary_results_dsmodel %>%
        separate(Label, into = c("regio", "openheid", "sbp"), sep = " - ")

      summary_df <- summary_results_dsmodel %>%
        filter(variable == "density") %>%
        mutate(stratum = paste(openheid, sbp, sep = " - ")) %>%
        replace(. == 0, NA)

      # Plot
      p <- p +
        geom_point(data = summary_df, aes(x = stratum, y = Estimate),
                   size = 3) +
        geom_errorbar(data = summary_df, aes(x = stratum, ymin = lcl,
                                             ymax = ucl), width = 0.25) +
        facet_wrap(~regio) +
        theme(legend.position = "")
    } else {
      # Voeg resultaten dsmodel toe
      summary_results_dsmodel <- bind_rows(summary_results_dsmodel,
                                           summary_results_dsmodel2) %>%
        separate(Label, into = c("regio", "openheid", "sbp", "jaar"),
                 sep = " - ")

      summary_df <- summary_results_dsmodel %>%
        filter(variable == "density") %>%
        mutate(stratum = paste(openheid, sbp, sep = " - ")) %>%
        replace(. == 0, NA)

      # Plot
      p <- p + geom_point(data = summary_df, aes(x = stratum, y = Estimate,
                          colour = jaar), size = 3,
                          position = position_dodge(width = 0.5)) +
        geom_errorbar(data = summary_df, aes(x = stratum, ymin = lcl,
                                             ymax = ucl, colour = jaar),
                      width = 0.25, position = position_dodge(width = 0.5)) +
        facet_wrap(~regio, scales = "free_x") +
        labs(colour = "Jaar") +
        theme(legend.position = "top",
              legend.background = element_rect(fill = "white",
                                               color = "darkgrey"),
              legend.margin = margin(6, 6, 6, 6))
    }

  }

  if (soort == "Haas" | spec %in% paste(roofvogels, "zonder_broedcode",
                                        sep = "_")) {
    p <- p + labs(x = "", y = "Aantal individuen per 100 ha",
                  title = gsub("_zonder_broedcode", "", soort))
  } else {
    p <- p + labs(x = "", y = "Aantal broedparen per 100 ha", title = soort)
  }

  if (show_data) {
    p <- p + scale_y_continuous(limits = c(0, NA),
                                trans = scales::pseudo_log_trans(),
                                breaks = c(0:5, seq(10, 50, 10),
                                           seq(75, 200, 25),
                                           seq(300, 1000, 100)))
  } else {
    p <- p + scale_y_continuous(limits = c(0, NA))
  }

  return(p)
}

get_effects <- function(df, ref = 0,
                        min_threshold = -0.2,
                        max_threshold = 0.25) {
  require(effectclass)

  # add effects
  df$effect_fine <- classification(
    lcl = df$lcl,
    ucl = df$ucl,
    threshold = c(min_threshold, max_threshold))

  df$effect_coarse <- coarse_classification(df$effect_fine)

  # order and rename effects
  out <- df %>%
    mutate(
      trend_fine = case_when(
        effect_fine == "++" ~ "sterk positief",
        effect_fine == "+" ~ "positief",
        effect_fine == "+~" ~ "gematigd positief",
        effect_fine == "~" ~ "stabiel",
        effect_fine == "-" ~ "negatief",
        effect_fine == "-~" ~ "gematigd negatief",
        effect_fine == "--" ~ "sterk negatief",
        TRUE ~ "onzeker",
      ),
      trend_fine = factor(trend_fine,
                          levels = c(
                            "sterk positief",
                            "positief",
                            "gematigd positief",
                            "stabiel",
                            "gematigd negatief",
                            "negatief",
                            "sterk negatief",
                            "onzeker"),
                          ordered = TRUE)
    ) %>%
    mutate(
      trend_coarse = case_when(
        effect_coarse == "+" ~ "positief",
        effect_coarse == "-" ~ "negatief",
        effect_coarse == "~" ~ "stabiel",
        TRUE ~ "onzeker",
      ),
      trend_coarse = factor(trend_coarse,
                            levels = c("positief",
                                       "stabiel",
                                       "negatief",
                                       "onzeker"),
                            ordered = TRUE)
    ) %>%
    mutate(
      trend_simple = case_when(
        lcl > ref ~ "positief",
        ucl < ref ~ "negatief",
        TRUE ~ "stabiel of onzeker"
      ),
      trend_simple = factor(trend_simple,
                            levels = c("positief",
                                       "negatief",
                                       "stabiel of onzeker"),
                            ordered = TRUE)
    ) %>%
    select(-c(effect_fine, effect_coarse))
}

create_draws_df <- function(model, seed = 123, ...) {
  require(tidybayes)

  df <- model %>%
    spread_draws(`b_area_prop_sb_cat.*`, regex = TRUE, seed = seed,
                 ndraws = 3000) %>%
    select(starts_with("b_area_prop_sb_cat")) %>%
    pivot_longer(cols = everything(), names_to = "area_prop_sb_cat_long") %>%
    mutate(area_prop_sb_cat = gsub("b_area_prop_sb_cat", "",
                                   area_prop_sb_cat_long),
           value_log = value,
           value = exp(value) - 1) %>%
    select(area_prop_sb_cat, value_log, value) %>%
    group_by(area_prop_sb_cat) %>%
    mutate(median_log = median(value_log),
           lcl_log = quantile(value_log, 0.05),
           ucl_log = quantile(value_log, 0.95)) %>%
    mutate(median = median(value),
           lcl = quantile(value, 0.05),
           ucl = quantile(value, 0.95)) %>%
    ungroup() %>%
    mutate(.point = "median",
           .width = 0.95 - 0.05)

  df_ordered <- df %>%
    mutate(area_prop_sb_cat = factor(area_prop_sb_cat,
                                     levels = c("nulbeleid",
                                                "laagbeleid",
                                                "middellaagbeleid",
                                                "middelhoogbeleid",
                                                "hoogbeleid"),
                                     ordered = TRUE))

  out <- get_effects(df_ordered, ...)
}

create_weighted_draws_df <- function(model, seed = 123, ...) {
  weight_df <- model$data %>%
    filter(area_prop_sb_cat != "nulbeleid") %>%
    count(area_prop_sb_cat) %>%
    mutate(
      group = case_when(
        grepl("laagbeleid", area_prop_sb_cat) ~ "laagbeleid",
        grepl("hoogbeleid", area_prop_sb_cat) ~ "hoogbeleid"
      )) %>%
    group_by(group) %>%
    mutate(sum = sum(n)) %>%
    ungroup() %>%
    mutate(weight = n / sum)

  df <- model %>%
    spread_draws(`b_area_prop_sb_cat.*`, regex = TRUE, seed = seed,
                 ndraws = 3000) %>%
    select(starts_with("b_area_prop_sb_cat")) %>%
    rename_with(~ gsub("b_area_prop_sb_cat", "", .x)) %>%
    rowwise() %>%
    mutate(laagbeleid2 = weighted.mean(c(laagbeleid, middellaagbeleid),
                                       pull(weight_df[weight_df$group == "laagbeleid", "weight"])),
           hoogbeleid2 = weighted.mean(c(middelhoogbeleid, hoogbeleid),
                                       pull(weight_df[weight_df$group == "hoogbeleid", "weight"]))) %>%
    select("laagbeleid" = laagbeleid2,
           "hoogbeleid" = hoogbeleid2) %>%
    pivot_longer(cols = everything(), names_to = "area_prop_sb_cat") %>%
    mutate(value_log = value,
           value = exp(value) - 1) %>%
    select(area_prop_sb_cat, value_log, value) %>%
    group_by(area_prop_sb_cat) %>%
    mutate(median_log = median(value_log),
           lcl_log = quantile(value_log, 0.05),
           ucl_log = quantile(value_log, 0.95)) %>%
    mutate(median = median(value),
           lcl = quantile(value, 0.05),
           ucl = quantile(value, 0.95)) %>%
    ungroup() %>%
    mutate(.point = "median",
           .width = 0.95 - 0.05)

  df_ordered <- df %>%
    mutate(area_prop_sb_cat = factor(area_prop_sb_cat,
                                     levels = c("nulbeleid",
                                                "laagbeleid",
                                                "hoogbeleid"),
                                     ordered = TRUE))

  out <- get_effects(df_ordered, ...)
}

index_labels <- function(x) {
  sprintf("%+.0f%%", 100 * (exp(x) - 1))
}

index_breaks <- function() {
  z <- 1 - c(
    5 / 6, 4 / 5, 3 / 4, 2 / 3, 1 / 2, 1 / 3, 1 / 5
  )
  z <- log(sort(z))
  c(z, 0, -z)
}

minor_breaks <- function() {
  breaks <- unique(abs(index_breaks()))
  out <- vector(length = length(breaks) - 1)
  i <- 1
  while (i < length(breaks)) {
    out[i] <- mean(c(breaks[i], breaks[i + 1]))
    i <- i + 1
  }
  return(c(-out, out))
}

index_breaks_rev <- function() {
  exp(index_breaks())
}

index_labels_rev <- function(digits = 2) {
  format(round(exp(index_breaks()), digits), nsmall = digits)
}
