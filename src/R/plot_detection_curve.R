# Scaling factor needed for correct plotting
# From mrds:::scalevalue
scalevalue <- function(key.scale, z) {
  return(exp(as.matrix(z) %*% key.scale))
}

# This function calculates the detection probability according to a hazard rate
# model from mrds:::keyfct.hz
keyfct.hz <- function(distance, key.scale, key_shape) {
  return(1 - exp(-(distance/key.scale)^(-key_shape)))
}

# This function calculates the detection probability according to a hazard rate
# model from mrds:::keyfct.hn
keyfct.hn <- function(distance, key.scale) {
  return(exp( -(( distance / (sqrt(2) * key.scale) )^2) ))
}

# from mrds:::detfct
bindata <- function(x, r, breaks) {
  return(hist(r[r >= x[1] & r <= x[2]], breaks = breaks,
              plot = FALSE)$counts)}
sumit <- function(x, n, wt) {
  return(sum(x / (wt * n)))}

plot_detection_curve <- function(dist_model, design_mat = NULL, labels = NULL,
                                 trunc = 300, n_breaks = 15,
                                 plot_average_fit = TRUE) {
  require(mrds)
  require(tidyverse)

  if (!dist_model$ddf$meta.data$point) {
    stop(paste0("Function only works for point transects!"), call. = FALSE)
  }

  # Vector of distances used to re-create the detection function (from 0 out to
  # truncation distance, default 300 m)
  distances <- seq(0, trunc, length.out = 100)


  # Calculate detection curves for different covariate combinations

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
        `colnames<-`(labels) %>%
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
        `colnames<-`(labels) %>%
        pivot_longer(col = everything(), names_to = "Legende",
                     values_to = "y_val") %>%
        arrange(Legende, desc(y_val)) %>%
        mutate(dist = rep(distances, length(labels)))

    } else {
      stop(paste0("Function only applicable for Hazard-rate ",
                  "and Half-normal key functions!"), call. = FALSE)
    }
  }


  # Re-create the histogram

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

  # Right-truncating default at 300 m
  # If you inspect the internal functions of the mrds package, you will find how
  # the number of histogram breaks is calculated. In our case, it is a variable
  # of the function, the default value is 15
  breaks <- seq(0, trunc, trunc / n_breaks)
  dummy_hist <- hist(dist_data[dist_data$distance <= trunc,]$distance,
                     breaks = breaks, plot = FALSE)

  # Calculate expected counts for each distance
  if (dist_model$ddf$meta.data$point) {
    nc <- length(breaks) - 1
    expected.counts <- -apply(matrix(c(breaks[2:(nc + 1)]^2, breaks[1:nc]^2),
      ncol = 2, nrow = nc), 1, diff) * (Nhat / breaks[nc + 1]^2)
  } else {
    expected.counts <- apply(t(as.matrix(c(0, trunc))), 1, bindata,
      r = (0:1000) * trunc / 1001, breaks = breaks)
    expected.counts <- apply(expected.counts, 1, sumit, n = 1001, wt = pdot)
  }

  # Re-scale the counts
  dummy_hist$counts <- dummy_hist$counts / expected.counts

  # Calculate the fitted average detection probability
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

  # Use the ggplot2 package to plot the histogram as a barplot and overlay the
  # detection functions
  hist_df <- data.frame(mids = dummy_hist$mids, counts = dummy_hist$counts)

  if (!dist_model$ddf$ds$aux$ddfobj$intercept.only & !is.null(design_mat)) {
    if (plot_average_fit) {
      out <- ggplot(hist_df) +
        geom_bar(aes(x = mids, y = counts), stat = "identity",
                 width = trunc / n_breaks, fill = "white", color = "black") +
        geom_line(data = df_y_val, aes(x = dist, y = y_val, colour = Legende),
                  size = 1) +
        geom_line(data = df_gemiddelde, aes(x = dist, y = lineval), size = 1,
                  linetype = "dashed") +
        scale_y_continuous(breaks = seq(0, 1, 0.2)) +
        labs(x = "Afstand (m)", y = "Detectiekans") +
        theme(legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_rect(fill = "white", color = "black"))
    } else {
      out <- ggplot(hist_df) +
        geom_bar(aes(x = mids, y = counts), stat = "identity",
                 width = trunc / n_breaks, fill = "white", color = "black") +
        geom_line(data = df_y_val, aes(x = dist, y = y_val, colour = Legende),
                  size = 1) +
        scale_y_continuous(breaks = seq(0, 1, 0.2)) +
        labs(x = "Afstand (m)", y = "Detectiekans") +
        theme(legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_rect(fill = "white", color = "black"))
    }
  } else {
    out <- ggplot(hist_df) +
      geom_bar(aes(x = mids, y = counts), stat = "identity",
               width = trunc / n_breaks, fill = "white", color = "black") +
      geom_line(data = df_gemiddelde, aes(x = dist, y = lineval), size = 1,
                linetype = "dashed") +
      scale_y_continuous(breaks = seq(0, 1, 0.2)) +
      labs(x = "Afstand (m)", y = "Detectiekans") +
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1),
            legend.background = element_rect(fill = "white", color = "black"))
  }

  # Return plot
  return(out)
}
