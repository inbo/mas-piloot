roofvogels_f <- function() {
  roofvogels <- c("Blauwe Kiekendief", "Boomvalk", "Bruine Kiekendief",
                  "Buizerd",
                  "Grauwe Kiekendief", "Ransuil", "Slechtvalk", "Sperwer",
                  "Steenuil", "Steppekiekendief", "Torenvalk", "Velduil",
                  "Wespendief", "Zwarte Wouw")

  return(roofvogels)
}

summarise_dsmodels <- function(dsmodel, dsmodel2 = NULL, year) {
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

    summary_results_dsmodel <- summary_results_dsmodel %>%
      bind_rows(summary_results_dsmodel2)
  }

  return(summary_results_dsmodel)
}

plot_densiteit <- function(dsmodel, soort = NULL,
                           show_data = TRUE, year = 2022:2023,
                           dsmodel2 = NULL,
                           obs_df, design) {
  roofvogels <- roofvogels_f()

  # Calculate summary of dsmodel
  summary_results_dsmodel <- summarise_dsmodels(dsmodel, dsmodel2, year)

  if ("jaar" %in% names(dsmodel$ddf$data)) {
    if (show_data) {
      # Totale lijst van bezochte plots in 2018-2022
      plots_old <- design %>%
        filter(!jaar %in% 2022:2023) %>%
        distinct(plotnaam) %>%
        pull()

      bezoekenlijst <- design %>%
        filter(plotnaam %in% plots_old)

      spec_presence <- obs_df %>%
        filter(plotnaam %in% plots_old)

      # Voeg afwezigheden toe door te mergen met alle bezoeken
      spec_df <- spec_presence %>%
        group_by(plotnaam, jaar, periode_in_jaar) %>%
        summarise(aantal = sum(aantal), .groups = "drop") %>%
        full_join(bezoekenlijst, by = c("plotnaam",
                                        "jaar",
                                        "periode_in_jaar")) %>%
        replace(is.na(.), 0)

      # Oppervlakte telcirkels
      cirkelopp <- pi * 300^2

      detectiekans_df <- cbind(
        dsmodel$ddf$data,
        "detectiekans" = predict(dsmodel)$fitted) %>%
        select(regio, jaar, "detectiekans") %>%
        distinct()

      average_df <- spec_df %>%
        mutate(jaar = factor(jaar, ordered = TRUE)) %>%
        full_join(detectiekans_df, by = join_by(jaar, regio)) %>%
        # Bepaal voor elke plot het gemiddeld aantal individuen over
        # de telperiodes per jaar
        group_by(plotnaam, jaar) %>%
        mutate(gem_aantal = mean(aantal)) %>%
        ungroup() %>%
        mutate(gem_aantal_p = gem_aantal / detectiekans) %>%
        select(-c(periode_in_jaar, aantal)) %>%
        distinct() %>%

        # Deel cirkeloppervlakte om densiteit broedkoppels per plot te krijgen
        mutate(densiteit = gem_aantal_p / cirkelopp * 1e6)

      p <- ggplot() +
        stat_sum(data = average_df, aes(x = jaar, y = densiteit),
                 position = position_dodge(width = 0.5), alpha = 0.1,
                 colour = "firebrick") +
        labs(size = "Aantal telpunten")
    } else {
      p <- ggplot()
    }

    # Voeg resultaten dsmodel toe
    summary_df <- summary_results_dsmodel %>%
      separate(Label, into = c("regio", "jaar"), sep = " - ") %>%
      filter(variable == "density") %>%
      mutate(jaar = factor(jaar, ordered = TRUE)) %>%
      replace(. == 0, NA)

    # Plot
    p <- p +
      geom_point(data = summary_df, aes(x = jaar, y = Estimate), size = 3) +
      geom_errorbar(data = summary_df, aes(x = jaar, ymin = lcl, ymax = ucl),
                    width = 0.25) +
      facet_wrap(~regio) +
      theme(legend.position = "top",
            legend.background = element_rect(fill = "white",
                                             color = "darkgrey"),
            legend.margin = margin(6, 6, 6, 6))

  } else {
    if (show_data) {
      # Totale lijst van bezochte plots in bepaald jaar
      bezoekenlijst_year <- design %>%
        filter(jaar %in% year)

      spec_presence <- obs_df %>%
        filter(jaar %in% year)

      # Voeg afwezigheden toe door te mergen met alle bezoeken
      spec_df <- spec_presence %>%
        group_by(plotnaam, jaar, periode_in_jaar) %>%
        summarise(aantal = sum(aantal), .groups = "drop") %>%
        full_join(bezoekenlijst_year, by = c("plotnaam",
                                             "jaar",
                                             "periode_in_jaar")) %>%
        replace(is.na(.), 0)

      # Oppervlakte telcirkels
      cirkelopp <- pi * 300^2


      if (length(year) == 1) {
        detectiekans_df <- cbind(
          dsmodel$ddf$data,
          "detectiekans" = predict(dsmodel)$fitted) %>%
          select(regio, sbp, openheid, "detectiekans") %>%
          distinct() %>%
          mutate(jaar = year)
      } else {
        detectiekans_df <- cbind(
          dsmodel$ddf$data,
          "detectiekans" = predict(dsmodel)$fitted) %>%
          mutate(jaar = 2022) %>%
          bind_rows(
            cbind(dsmodel2$ddf$data,
                  "detectiekans" = predict(dsmodel2)$fitted) %>%
              mutate(jaar = 2023)
          ) %>%
          select(regio, sbp, openheid, jaar, "detectiekans") %>%
          distinct()
      }


      average_df <- spec_df %>%
        full_join(detectiekans_df,
                  by = join_by(regio, sbp, openheid, jaar)) %>%
        mutate(jaar = as.character(jaar)) %>%
        # Bepaal voor elke plot het gemiddeld aantal individuen over
        # de telperiodes per jaar
        group_by(plotnaam, jaar) %>%
        mutate(gem_aantal = mean(aantal)) %>%
        ungroup() %>%
        mutate(gem_aantal_p = gem_aantal / detectiekans) %>%
        select(-c(periode_in_jaar, aantal)) %>%
        distinct() %>%

        # Deel cirkeloppervlakte om densiteit broedkoppels per plot te krijgen
        mutate(densiteit = gem_aantal_p / cirkelopp * 1e6,
               stratum = paste(openheid, sbp, sep = " - "))

      p <- ggplot() +
        stat_sum(data = average_df, aes(x = stratum, y = densiteit,
                                        colour = jaar),
                 position = position_dodge(width = 0.5), alpha = 0.1) +
        labs(size = "Aantal telpunten")
    } else {
      p <- ggplot()
    }
    # Voeg resultaten dsmodel toe
    summary_df <- summary_results_dsmodel %>%
      separate(Label, into = c("regio", "openheid", "sbp", "jaar"),
               sep = " - ") %>%
      filter(variable == "density") %>%
      mutate(stratum = paste(openheid, sbp, sep = " - "),
             jaar = as.character(jaar)) %>%
      replace(. == 0, NA)

    # Plot
    if (length(year) == 1) {
      p <- p + geom_point(data = summary_df, aes(x = stratum, y = Estimate),
                          size = 3) +
        geom_errorbar(data = summary_df, aes(x = stratum, ymin = lcl,
                                             ymax = ucl), width = 0.25) +
        facet_wrap(~regio, scales = "free_x") +
        labs(colour = "Jaar") +
        theme(legend.position = "top",
              legend.background = element_rect(fill = "white",
                                               color = "darkgrey"),
              legend.margin = margin(6, 6, 6, 6))
    } else {
      p <- p + geom_point(data = summary_df, aes(x = stratum, y = Estimate,
                          colour = jaar, group = jaar), size = 3,
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

  if (soort == "Haas" | soort %in% paste(roofvogels, "zonder_broedcode",
                                        sep = "_")) {
    p <- p + labs(x = "", y = "Aantal individuen per 100 ha")
  } else if (is.null(soort)) {
    p <- p + labs(x = "", y = "Aantal broedparen per 100 ha")
  } else {
    p <- p + labs(x = "", y = "Aantal broedparen per 100 ha")
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

compare_from_df <- function(x, label1, label2) {
  mean1 <- x %>%
    filter(label == label1) %>%
    pull(estimate)
  se1 <- x %>%
    filter(label == label1) %>%
    pull(se)

  mean2 <- x %>%
    filter(label == label2) %>%
    pull(estimate)
  se2 <- x %>%
    filter(label == label2) %>%
    pull(se)

  return(cbind(label = label1, diff_lognormal(mean1, se1, mean2, se2)))
}

diff_lognormal <- function(mean1, se1, mean2, se2) {
  # Calculate log-transformed values for each group
  log_mean1 <- log(mean1)
  log_se1 <- sqrt(log(1 + (se1 / mean1)^2))

  log_mean2 <- log(mean2)
  log_se2 <- sqrt(log(1 + (se2 / mean2)^2))

  # Set confidence level (e.g., 95%)
  confidence_level <- 0.95
  z_score <- qnorm((1 + confidence_level) / 2)  # Two-tailed Z-score

  # Calculate the log-transformed confidence interval for the difference
  log_diff_mean <- log_mean1 - log_mean2
  log_diff_se <- sqrt(log_se1^2 + log_se2^2)

  log_CI_lower_diff <- log_diff_mean - z_score * log_diff_se
  log_CI_upper_diff <- log_diff_mean + z_score * log_diff_se

  return(tibble(log_diff_mean = log_diff_mean,
                log_diff_se = log_diff_se,
                lcl = log_CI_lower_diff,
                ucl = log_CI_upper_diff))
}

plot_densiteit_methods <- function(dsmodel, soort,
                                   show_data = TRUE, year = 2022:2023,
                                   dsmodel2 = NULL,
                                   obs_df,
                                   design) {
  roofvogels <- roofvogels_f()

  # Calculate summary of dsmodel
  summary_results_dsmodel <- summarise_dsmodels(dsmodel, dsmodel2, year)

  # Totale lijst van bezochte plots in bepaald jaar
  bezoekenlijst_year <- design %>%
    filter(jaar %in% year)

  spec_presence <- obs_df %>%
    filter(jaar %in% year)

  # Voeg afwezigheden toe door te mergen met alle bezoeken
  spec_df <- spec_presence %>%
    group_by(plotnaam, jaar, periode_in_jaar) %>%
    summarise(aantal = sum(aantal), .groups = "drop") %>%
    full_join(bezoekenlijst_year, by = c("plotnaam",
                                         "jaar",
                                         "periode_in_jaar")) %>%
    replace(is.na(.), 0)

  # Oppervlakte telcirkels
  cirkelopp <- pi * 300^2

  if (length(year) == 1) {
    detectiekans_df <- cbind(
      dsmodel$ddf$data,
      "detectiekans" = predict(dsmodel)$fitted) %>%
      select(regio, sbp, openheid, "detectiekans") %>%
      distinct() %>%
      mutate(jaar = year)
  } else {
    detectiekans_df <- cbind(
      dsmodel$ddf$data,
      "detectiekans" = predict(dsmodel)$fitted) %>%
      mutate(jaar = 2022) %>%
      bind_rows(
        cbind(dsmodel2$ddf$data,
              "detectiekans" = predict(dsmodel2)$fitted) %>%
          mutate(jaar = 2023)
      ) %>%
      select(regio, sbp, openheid, jaar, "detectiekans") %>%
      distinct()
  }

  set.seed(123)
  average_df <- spec_df %>%
    full_join(detectiekans_df,
              by = join_by(regio, sbp, openheid, jaar)) %>%
    mutate(jaar = as.character(jaar)) %>%
    # Bepaal voor elke plot het gemiddeld aantal individuen over
    # de telperiodes per jaar
    group_by(plotnaam, jaar) %>%
    mutate(max_aantal = max(aantal),
           gem_aantal = mean(aantal)) %>%
    ungroup() %>%
    mutate(gem_aantal_p = gem_aantal / detectiekans) %>%
    select(-c(periode_in_jaar, aantal)) %>%
    distinct() %>%
    group_by(regio, jaar, openheid, sbp) %>%
    mutate(bootstrap_max = mean_cl_boot(max_aantal)) %>%
    ungroup() %>%

    # Deel cirkeloppervlakte om densiteit broedkoppels per plot te krijgen
    mutate(max_densiteit = max_aantal / cirkelopp * 1e6,
           gem_densiteit = gem_aantal_p  / cirkelopp * 1e6,
           total_max_mean = bootstrap_max$y / cirkelopp * 1e6,
           lcl_max_mean = bootstrap_max$ymin / cirkelopp * 1e6,
           ucl_max_mean = bootstrap_max$ymax / cirkelopp * 1e6,
           stratum = paste(openheid, sbp, sep = " - ")) %>%
    select(-c(bootstrap_max)) %>%
    mutate(methode = "gemiddelde van maxima")


  # Voeg resultaten dsmodel toe
  summary_df <- summary_results_dsmodel  %>%
    separate(Label, into = c("regio", "openheid", "sbp", "jaar"),
             sep = " - ") %>%
    filter(variable == "density") %>%
    mutate(stratum = paste(openheid, sbp, sep = " - ")) %>%
    replace(. == 0, NA) %>%
    filter(!is.na(Estimate)) %>%
    mutate(methode = "distance sampling") %>%
    left_join(average_df %>%
                select(jaar, regio, openheid, sbp, gem_densiteit),
              by = join_by(regio, openheid, sbp, jaar)) %>%
    select(stratum, regio, jaar, "gemiddelde" = Estimate, lcl,
           ucl, methode, "data" = gem_densiteit) %>%
    mutate(full_stratum = paste(stratum, regio, jaar, sep = " - "))

  average_df2 <- average_df %>%
    select(stratum, regio, jaar, "gemiddelde" = total_max_mean,
           "lcl" = lcl_max_mean, "ucl" = ucl_max_mean, methode,
           "data" = max_densiteit) %>%
    mutate(full_stratum = paste(stratum, regio, jaar, sep = " - ")) %>%
    filter(full_stratum %in% unique(summary_df$full_stratum))

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
    labs(colour = "Methode", size = "Aantal telpunten") +
    theme(legend.position = "top",
          legend.background = element_rect(fill = "white",
                                           color = "darkgrey"),
          legend.margin = margin(6, 6, 6, 6))

  if (is.null(soort)) {
    p <- p + labs(x = "", y = "Aantal broedparen per 100 ha")
  } else if (soort == "Haas" | soort %in% paste(roofvogels, "zonder_broedcode",
                                         sep = "_")) {
    p <- p + labs(x = "", y = "Aantal individuen per 100 ha")
  } else {
    p <- p + labs(x = "", y = "Aantal broedparen per 100 ha")
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

create_draws_df <- function(model, ll = 0.025, ul = 0.975, seed = 123,
                            ndraws = 3000, ...) {
  require(tidybayes)

  df <- model %>%
    spread_draws(`b_area_prop_sb_cat.*`, regex = TRUE, seed = seed,
                 ndraws = ndraws) %>%
    select(starts_with("b_area_prop_sb_cat"), ".draw") %>%
    pivot_longer(cols = starts_with("b_area_prop_sb_cat"), names_to = "area_prop_sb_cat_long") %>%
    mutate(area_prop_sb_cat = gsub("b_area_prop_sb_cat", "",
                                   area_prop_sb_cat_long),
           value_log = value,
           value = exp(value) - 1) %>%
    select(area_prop_sb_cat, value_log, value, .draw) %>%
    group_by(area_prop_sb_cat) %>%
    mutate(median_log = median(value_log),
           lcl_log = quantile(value_log, ll),
           ucl_log = quantile(value_log, ul)) %>%
    mutate(median = median(value),
           lcl = quantile(value, ll),
           ucl = quantile(value, ul)) %>%
    ungroup() %>%
    mutate(.point = "median",
           .width = ul - ll)

  df_ordered <- df %>%
    mutate(area_prop_sb_cat = factor(area_prop_sb_cat,
                                     levels = c("nulbeleid",
                                                "laagbeleid",
                                                "middellaagbeleid",
                                                "middelhoogbeleid",
                                                "hoogbeleid"),
                                     ordered = TRUE))

  out <- get_effects(df_ordered, ...)
  return(out)
}

create_weighted_draws_df <- function(model, ll = 0.025, ul = 0.975,
                                     seed = 123, ...) {
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
           lcl_log = quantile(value_log, ll),
           ucl_log = quantile(value_log, ul)) %>%
    mutate(median = median(value),
           standaardfout = sd(value),
           lcl = quantile(value, ll),
           ucl = quantile(value, ul)) %>%
    ungroup() %>%
    mutate(.point = "median",
           .width = ul - ll)

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

plot_brms_fit <- function(model, x, species = "", axis_label = NULL, FUN = mean,
                          factor = 1, n = 100, show_data = FALSE,
                          interval = c(0.6, 0.95), scale_data = NULL) {
  require(tidybayes)
  require(modelr)

  if (is.null(axis_label)) {
    axis_label <- x
  }

  if ("plotnaam" %in% names(model$data)) {
    cat_vars <- model$data %>%
      select(-plotnaam) %>%
      select(!where(is.numeric)) %>%
      names()
  } else {
    cat_vars <- model$data %>%
      select(!where(is.numeric)) %>%
      names()
  }

  if (x %in% cat_vars) {
    plot_df <- model$data %>%
      select(-aantal) %>%
      group_by_at(cat_vars) %>%
      mutate(across(where(is.numeric), FUN)) %>%
      ungroup() %>%
      distinct() %>%
      add_epred_draws(model, ndraws = 3000)

    out <- plot_df %>%
      ggplot(aes(x = .data[[x]]))

    if (isTRUE(show_data)) {
      out <- out +
        geom_jitter(data = model$data,  aes(y = aantal), colour = "firebrick",
                    alpha = 0.1, width = 0.2, height = 0) +
        scale_y_continuous(limits = c(0, NA),
                           trans = scales::pseudo_log_trans())
    } else {
      out <- out +
        scale_y_continuous(limits = c(0, NA))
    }

    out <- out +
      stat_eye(aes(y = .epred), alpha = 1,
               .width = interval) +
      labs(x = axis_label,
           y = paste("aantal broedparen", species)) +
      theme(legend.position = "")

    if (length(cat_vars)  == 2) {
      facet_var <- setdiff(cat_vars, x)
      out <- out +
        facet_wrap(vars(!!sym(facet_var)))
    } else if (length(cat_vars) > 2) {
      facet_var <- setdiff(cat_vars, x)
      out <- out +
        facet_grid(vars(!!sym(facet_var[1])), vars(!!sym(facet_var[2])))
    }

  } else {
    if ("plotnaam" %in% names(model$data)) {
      means_df <- model$data %>%
        select_at(vars(-c("aantal", "plotnaam"), -x)) %>%
        group_by_at(cat_vars) %>%
        mutate(across(where(is.numeric), ~FUN(.x))) %>%
        ungroup() %>%
        distinct()

      range_df <- model$data %>%
        group_by_at(cat_vars) %>%
        summarize(min_var = min(!!sym(x)), max_var = max(!!sym(x)))

      # Create a sequence of 100 values for each group
      range_df <- range_df %>%
        rowwise() %>%
        mutate(value_range = list(seq(min_var, max_var, length.out = n))) %>%
        select(-c(max_var, min_var))

      # Expand the grid with combinations of categories and the corresponding range values
      plot_df <- full_join(means_df, range_df) %>%
        unnest(value_range) %>%
        `colnames<-`(c(names(model$data)[-match(c("aantal", "plotnaam", x),
                                                names(model$data))], x)) %>%
        add_epred_draws(model, ndraws = 3000, re_formula = NA)
    } else {
      means_df <- model$data %>%
        select_at(vars(-"aantal", -x)) %>%
        group_by_at(cat_vars) %>%
        mutate(across(where(is.numeric), ~FUN(.x))) %>%
        ungroup() %>%
        distinct()

      range_df <- model$data %>%
        group_by_at(cat_vars) %>%
        summarize(min_var = min(!!sym(x)), max_var = max(!!sym(x)))

      # Create a sequence of n values for each group
      range_df <- range_df %>%
        rowwise() %>%
        mutate(value_range = list(seq(min_var, max_var, length.out = n))) %>%
        select(-c(max_var, min_var))

      # Expand the grid with combinations of categories and the corresponding range values
      plot_df <- full_join(means_df, range_df) %>%
        unnest(value_range) %>%
        `colnames<-`(c(names(model$data)[-match(c("aantal", x),
                                                names(model$data))], x)) %>%
        add_epred_draws(model, ndraws = 3000)
    }

    if (!is.null(scale_data)) {
      out <- plot_df %>%
        ggplot(aes(x = ((.data[[x]] * sd(scale_data[[x]]))
                        + mean(scale_data[[x]])) * factor)) +
        coord_cartesian(ylim = c(0, NA))
    } else {
      out <- plot_df %>%
        ggplot(aes(x = .data[[x]]*factor)) +
        coord_cartesian(ylim = c(0, NA))
    }

    if (isTRUE(show_data)) {
      out <- out +
        geom_point(data = model$data, aes(y = aantal), colour = "firebrick",
                   alpha = 0.1) +
        scale_y_continuous(trans = scales::pseudo_log_trans())
    }

    out <- out +
      stat_lineribbon(aes(y = .epred), point_interval = "median_qi",
                      show.legend = FALSE, .width = interval, alpha = 0.5) +
      labs(x = axis_label,
           y = paste("aantal broedparen", species),
           fill = "credible\nlevel")

    if (length(cat_vars) == 1) {
      out <- out +
        facet_wrap(vars(!!sym(cat_vars)))
    } else if (length(cat_vars) >= 2) {
      out <- out +
        facet_grid(vars(!!sym(cat_vars[1])), vars(!!sym(cat_vars[2])))
    }
  }
  return(out)
}

