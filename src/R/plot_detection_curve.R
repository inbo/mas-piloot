#'  Plot detection functions in ggplot
#'
#' Various functions used to specify key and adjustment functions for
#' detection functions from mrds package.
#' Final function to plot the detection function with ggplot
#'
#' \code{scalevalue} for either detection function it computes the scale with
#' the log link using the parameters and the covariate design matrix.
#' From mrds:::scalevalue
#'
#' \code{keyfct.hz} calculates the detection probability according to a
#' hazard rate model.
#' From mrds:::keyfct.hz
#'
#' \code{keyfct.hn} calculates the detection probability according to a
#' half normal model.
#' From mrds:::keyfct.hn
#'
#' \code{plot_detection_curve} will create a bar graph of distances with
#' superimposed detection curves and average fitting curve according to a
#' dsmodel. In a multicovariate model, a design matrix and labels can be passed
#' to create detection functions for each category
#'
#' scalevalue(key.scale, z)
#'
#' keyfct.hn(distance, key.scale)
#'
#' keyfct.hz(distance, key.scale, key.shape)
#'
#' plot_detection_curve(dist_model, design_mat = NULL, labels = NULL,
#'                      n_breaks, plot_average_fit = TRUE)
#'
#' @param distance  vector of distances
#' @param z design matrix for scale function
#' @param key.scale vector of scale values
#' @param key.shape vector of shape values
#' @param dist_model dsmodel
#' @param design_mat design matrix for plot function
#' @param labels vector of strings used for labeling the plotting categories
#' @param n_breaks number of breaks for the bar graph
#' @param plot_average_fit logical used to indicate whether the fitted average
#' detection curve should be plotted (default is TRUE)
#' @param show_data logical used to indicate whether the distance data should be
#' plotted (default is TRUE)
#'
#' @return
#' For \code{scalevalue}, vector of the scale parameters
#' For \code{keyfct.*}, vector of key function evaluations
#' For \code{plot_detection_curve}, ggplot object
#'
#' @examples
#' plot_detection_curve(dist_model)
#' plot_detection_curve(dist_model, n_breaks = 20, plot_average_fit = FALSE)
#' plot_detection_curve(dist_model, design_mat = plot_matrix,
#'                      labels = plot_labels, n_breaks = 15)
#' plot_detection_curve(dist_model, design_mat = plot_matrix,
#'                      labels = plot_labels, n_breaks = 20,
#'                      plot_average_fit = FALSE)


scalevalue <- function(key.scale, z) {
  return(exp(as.matrix(z) %*% key.scale))
}


keyfct.hz <- function(distance, key.scale, key_shape) {
  return(1 - exp(-(distance/key.scale)^(-key_shape)))
}


keyfct.hn <- function(distance, key.scale) {
  return(exp( -(( distance / (sqrt(2) * key.scale) )^2) ))
}


plot_detection_curve <- function(dist_model, design_mat = NULL, labels = NULL,
                                 n_breaks = NULL, plot_average_fit = TRUE,
                                 show_data = TRUE) {
  require(mrds)
  require(tidyverse)

  if (!dist_model$ddf$meta.data$point) {
    stop(paste0("Function only works for point transects!"), call. = FALSE)
  }

  # Vector of distances used to re-create the detection function (from 0 out to
  # truncation distance)
  trunc <- dist_model$ddf$meta.data$width
  distances <- seq(0, trunc, length.out = 100)

  #####
  # Calculate detection curves for different covariate combinations
  #####

  if (!dist_model$ddf$ds$aux$ddfobj$intercept.only & !is.null(design_mat)) {

    # Make sure the design matrix is a dataframe
    design_mat <- design_mat %>%
      as.data.frame()

    # Make sure the labels and design matrix are compatible
    if (nrow(design_mat) != length(labels)) {
      if (nrow(design_mat) < length(labels)) {
        stop(paste0("Too much lables specified for the given design matrix!"),
             call. = FALSE)
      } else {
        stop(paste0("Not enough lables specified for the given design matrix!"),
             call. = FALSE)
      }
    }

    key <- dist_model$ddf$ds$aux$ddfobj$type
    if (key == "hr") {
      # Shape parameter
      key_shape_hr <- scalevalue(dist_model$ddf$ds$aux$ddfobj$shape$parameters,
                                 matrix(1, nrow = 100, 1))

      # Scale parameter
      scaled_values <- scalevalue(dist_model$ddf$ds$aux$ddfobj$scale$parameters,
                                  design_mat) %>%
        as.data.frame() %>%
        mutate(cat = labels) %>%
        slice(rep(1:n(), each = 100)) %>%
        group_by(cat) %>%
        group_split()

      # Calculate detection probability values
      y_vals <- lapply(scaled_values, function(x) {
        keyfct.hz(distances, x[, 1], key_shape_hr)
      })

      # Combine in dataframe
      df_y_val <- y_vals %>%
        data.frame() %>%
        `colnames<-`(sort(labels)) %>%
        pivot_longer(col = everything(), names_to = "Legende",
                     values_to = "y_val") %>%
        arrange(Legende, desc(y_val)) %>%
        mutate(dist = rep(distances, length(labels)))

    } else if (key == "hn") {
      # Scale parameter
      scaled_values <- scalevalue(dist_model$ddf$ds$aux$ddfobj$scale$parameters,
                                  design_mat) %>%
        as.data.frame() %>%
        mutate(cat = labels) %>%
        slice(rep(1:n(), each = 100)) %>%
        group_by(cat) %>%
        group_split()

      # Calculate detection probability values
      y_vals <- lapply(scaled_values, function(x) {
        keyfct.hn(distances, x[, 1])
      })

      # Combine in dataframe
      df_y_val <- y_vals %>%
        data.frame() %>%
        `colnames<-`(sort(labels)) %>%
        pivot_longer(col = everything(), names_to = "Legende",
                     values_to = "y_val") %>%
        arrange(Legende, desc(y_val)) %>%
        mutate(dist = rep(distances, length(labels)))

    } else {
      stop(paste0("Function only applicable for Hazard-rate ",
                  "and Half-normal key functions!"), call. = FALSE)
    }
  }

  #####
  # Re-create the histogram
  #####

  # Detection probability for each fitted value & Nhat estimate
  selected <- rep(TRUE, nrow(dist_model$ddf$ds$aux$ddfobj$xmat))
  if (length(dist_model$ddf$fitted) == 1) {
    pdot <- rep(dist_model$ddf$fitted, sum(as.numeric(selected)))
  } else {
    pdot <- dist_model$ddf$fitted[selected]
    Nhat <- sum(1 / pdot)
  }

  # Create a dummy histogram
  # dist_data is the data.frame object of the distance data
  dist_data <- dist_model$ddf$data

  # Right-truncating

  if (is.null(n_breaks)) {
    if (dist_model$ddf$meta.data$binned) {
      n_breaks <- length(dist_model$ddf$ds$aux$breaks) - 1
    } else {
      n <- length(dist_model$ddf$ds$aux$ddfobj$xmat$distance)
      n_breaks <- round(sqrt(n), 0)
    }
  }

  breaks <- seq(0, trunc, trunc / n_breaks)
  dummy_hist <- hist(dist_data[dist_data$distance <= trunc,]$distance,
                     breaks = breaks, plot = FALSE)

  # Calculate expected counts for each distance (point transect only)
  nc <- length(breaks) - 1
  expected.counts <- -apply(matrix(c(breaks[2:(nc + 1)]^2, breaks[1:nc]^2),
    ncol = 2, nrow = nc), 1, diff) * (Nhat / breaks[nc + 1]^2)

  # Re-scale the counts
  dummy_hist$counts <- dummy_hist$counts / expected.counts

  #####
  # Calculate the fitted average detection probability
  #####

  ddfobj <- dist_model$ddf$ds$aux$ddfobj
  finebr <- seq(0, trunc, length.out = 101)
  xgrid <- NULL
  linevalues <- NULL
  newdat <- ddfobj$xmat
  for (i in 1:(length(finebr) - 1)) {
    x <- (finebr[i] + finebr[i + 1]) / 2
    xgrid <- c(xgrid, x)
    newdat$distance <- rep(x, nrow(newdat))

    detfct.values <- detfct(newdat$distance, ddfobj, select = selected,
                            width = trunc)
    linevalues <- c(linevalues, sum(detfct.values/pdot)/sum(1/pdot))
  }

  df_gemiddelde <- data.frame(dist = distances, lineval = linevalues)

  #####
  # Make final plot with ggplot()
  #####

  # Use the ggplot2 package to plot the histogram as a barplot and overlay the
  # detection functions
  hist_df <- data.frame(mids = dummy_hist$mids, counts = dummy_hist$counts)

  # Plot with covariates and design matrix
  if (!dist_model$ddf$ds$aux$ddfobj$intercept.only & !is.null(design_mat)) {
  # Plot with average fitting line
    if (plot_average_fit) {
      out <- ggplot(hist_df) +
        geom_bar(aes(x = mids, y = counts), stat = "identity",
                 width = trunc / n_breaks, fill = "white", color = "black") +
        geom_line(data = df_y_val, aes(x = dist, y = y_val, colour = Legende),
                  linewidth = 1) +
        geom_line(data = df_gemiddelde, aes(x = dist, y = lineval), linewidth = 1,
                  linetype = "dashed") +
        scale_y_continuous(limits = c(0, NA),
                           breaks = seq(0, 100, by = 0.25)) +
        labs(x = "Afstand (m)", y = "Detectiekans") +
        theme(legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_rect(fill = "white",
                                               color = "darkgrey"),
              legend.margin = margin(6, 6, 6, 6))
    # Plot without average fitting line
    } else {
      out <- ggplot(hist_df) +
        geom_bar(aes(x = mids, y = counts), stat = "identity",
                 width = trunc / n_breaks, fill = "white", color = "black") +
        geom_line(data = df_y_val, aes(x = dist, y = y_val, colour = Legende),
                  linewidth = 1) +
        scale_y_continuous(limits = c(0, NA),
                           breaks = seq(0, 100, by = 0.25)) +
        labs(x = "Afstand (m)", y = "Detectiekans") +
        theme(legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_rect(fill = "white",
                                               color = "darkgrey"),
              legend.margin = margin(6, 6, 6, 6))
    }

  # Plot without covariates/design matrix
  } else {
  # Plot average fit
    if (plot_average_fit) {
      out <- ggplot(hist_df) +
        geom_bar(aes(x = mids, y = counts), stat = "identity",
                 width = trunc / n_breaks, fill = "white", color = "black") +
        geom_line(data = df_gemiddelde, aes(x = dist, y = lineval), linewidth = 1,
                  linetype = "dashed") +
        scale_y_continuous(limits = c(0, NA),
                           breaks = seq(0, 100, by = 0.25)) +
        labs(x = "Afstand (m)", y = "Detectiekans") +
        theme(legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_rect(fill = "white", color = "darkgrey"),
              legend.margin = margin(6, 6, 6, 6))
    # Plot bar graph
    } else {
      out <- ggplot(hist_df) +
        geom_bar(aes(x = mids, y = counts), stat = "identity",
                 width = trunc / n_breaks, fill = "white", color = "black") +
        scale_y_continuous(limits = c(0, NA),
                           breaks = seq(0, 100, by = 0.25)) +
        labs(x = "Afstand (m)", y = "Detectiekans") +
        theme(legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_rect(fill = "white",
                                               color = "darkgrey"),
              legend.margin = margin(6, 6, 6, 6))
    }
  }

  if (show_data) {
    distance_data <- data.frame(dist = sort(unique(dist_model$ddf$data$distance)))
    out <- out +
      geom_point(data = distance_data, aes(x = dist,
                                           y = rep(0, nrow(distance_data))),
                 shape = "|", col = "brown", size = 3)
  }

  # Return plot
  return(out)
}
