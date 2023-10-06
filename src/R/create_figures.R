plot_densiteit <- function(dsmodel, soort,
                           show_data = TRUE, year = 2022,
                           dsmodel2 = NULL) {
  roofvogels <- c("Blauwe Kiekendief", "Boomvalk", "Bruine Kiekendief",
                  "Buizerd",
                  "Grauwe Kiekendief", "Ransuil", "Slechtvalk", "Sperwer",
                  "Steenuil", "Steppekiekendief", "Torenvalk", "Velduil",
                  "Wespendief", "Zwarte Wouw")

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

      spec_presence <- df_obs_list[[gsub(" ", "", soort)]] %>%
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
      spec_presence <- df_obs_list[[gsub(" ", "", soort)]] %>%
        filter(jaar == year)

      # Totale lijst van bezochte plots in bepaald jaar
      bezoekenlijst_year <- bind_rows(design_2022_mas, design_2023_mas) %>%
        filter(jaar == year) %>%
        select(-periode_in_jaar) %>%
        expand_grid(distinct(spec_presence, periode_in_jaar)) %>%
        arrange(plotnaam, jaar, periode_in_jaar)

      # Voeg afwezigheden toe door te mergen met alle bezoeken
      spec_df <- spec_presence %>%
        st_drop_geometry() %>%
        group_by(plotnaam, jaar, periode_in_jaar) %>%
        summarise(aantal = sum(aantal)) %>%
        full_join(bezoekenlijst_year, by = c("plotnaam",
                                             "jaar",
                                             "periode_in_jaar")) %>%
        replace(is.na(.), 0) %>%
        arrange(plotnaam, jaar, periode_in_jaar) %>%
        mutate(Region.Label = paste(regio, jaar, sep = " - "))

      # Oppervlakte telcirkels
      cirkelopp <- pi * 300^2

      average_df <- spec_df %>%
        # Bepaal voor elke plot het gemiddeld aantal individuen over
        # de telperiodes per jaar
        group_by(plotnaam, periode_in_jaar) %>%
        mutate(totaal = sum(aantal)) %>%
        group_by(plotnaam) %>%
        mutate(gemiddelde = mean(totaal)) %>%
        ungroup() %>%
        select(-c(periode_in_jaar, aantal, totaal)) %>%
        distinct() %>%

        # Deel cirkeloppervlakte om densiteit broedkoppels per plot te krijgen
        mutate(Estimate = gemiddelde / cirkelopp * 1e6,
               stratum = paste(openheid, sbp, sep = " - "))

      p <- ggplot() +
        stat_sum(data = average_df, aes(x = stratum, y = Estimate),
                 position = position_dodge(width = 0.5), alpha = 0.1,
                 colour = "firebrick")
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
