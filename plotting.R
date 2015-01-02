# All plotting functions

require("lattice")
require("Cairo")

histPlot <- function(catname, times.all, times.sub, 
                     n.bins, units, xaxs, bandwidth, padding = 0.1) {
  n.breaks <- as.numeric(n.bins) + 1
  breaks <- seq(xaxs$at[1], xaxs$at[nrow(xaxs)], length = n.breaks)
  bin.width <- diff(breaks[1:2])  # histogram bin width in seconds
  
  h <- hist(times.sub, plot = FALSE, breaks = breaks)
  h$counts <- h$counts * ping.interval / 60 * units * 24 * 3600 / bin.width
    # convert from pings per bin to hours per week
  
  # add some padding to times.all to avoid density going to zero at extremes:
  times.all.padded <- padding(times.all, fraction = padding)
  d <- density(times.all.padded, adjust = bandwidth)
  d$y <- d$y * length(times.all.padded) * ping.interval / 60 * 
         units * 24 * 3600
    # scale density to plot
  d.range <- d$x >= min(times.all) & d$x <= max(times.all)
  
  plotTitle <- paste(catname, " (hours per ", 
                     switch(as.character(units), "7" = "week", "1" = "day"), 
                     ")", sep = "")
  list(
    plot(h, main = plotTitle, 
         col = "aliceblue", border = "white", ylab = "", 
         ylim = c(0, 1.02 * max(max(h$counts), max(d$y[d.range]))), 
         xaxt = "n", xlab = ""), 
    axis(side = 1, at = xaxs$at, labels = xaxs$lab), 
    axis(4), 
    rug(times.sub, tick = 0.04, side = 1, col = addAlpha("lightcoral", 0.6)), 
    lines(d$x[d.range], d$y[d.range], lwd = 2, col = "lightcoral")
  )
}

matrixPlot <- function(dates, timesofday, 
                       xaxs = NULL, yaxs = NULL, add = FALSE) {
  if (!add) {
    list(
      plot(dates, timesofday, 
           pch = 16, cex = 0.75, col = addAlpha("#0080ff", 0.35), main = "", 
           xlab = "", ylab = "Time of day", xaxt = "n", yaxt = "n", 
           xlim = range(xaxs$at), xaxs = "i", 
           ylim = c(0,24), yaxs = "i", bty = "n"), 
      axis(side = 2, at = yaxs$at, labels = yaxs$lab), 
      axis(side = 4, at = yaxs$at, labels = yaxs$lab), 
      axis(side = 1, at = xaxs$at, labels = xaxs$lab), 
      abline(h = 24)
    )
  } else {
    list(
      points(dates, timesofday, 
             pch = 16, cex = 0.75, col = addAlpha("lightcoral", 0.35))
    )
  }
}

scatterPlot <- function(x, y, z = NULL, names, jitter, trellis, 
                        plotTitle, panels = 4) {
  if (jitter) {
    y <- jitter(y, factor = 2)
    x <- jitter(x, factor = 2)
  }
  if(!trellis) {
    formula <- y ~ x
    plotTitle <- paste(names[1], "vs.", tolower(names[2]), 
                       "(aggregated by day)")
    strip <- FALSE
  } else {
    formula <- y ~ x | equal.count(z, panels, overlap = 0.05)
    plotTitle <- paste(names[1], " vs. ", tolower(names[2]), 
                       " (conditioned on ", tolower(names[3]), 
                       ")", sep = "")
    stripLabels <- rep(paste(names[3], "(approximate quantiles)"), panels)
    strip <- strip.custom(factor.levels = stripLabels, 
                            strip.levels = TRUE, strip.names = FALSE)
  }
  xyplot(formula, pch = 16, cex = 1, alpha = 0.35, main = plotTitle, 
         xlab = paste(names[2], "(estimated hours)"), 
         ylab = paste(names[1], "(estimated hours)"), 
         strip = strip)
}

timeofdayPlot <- function(catname, timesofday, xaxs, bandwidth) {
  # n.bins option
  # informative y-axis such as efficiency
  h <- hist(timesofday, breaks = 12, plot = FALSE)
  
  timesofday.cyc <- c(timesofday - 24, timesofday, timesofday + 24)
  d <- density(timesofday.cyc, adjust = bandwidth)
  d$y <- d$y * 3
  d.range <- d$x >= 0 & d$x < 24
  
  list(
    plot(h, main = paste(catname, "(by time of day)"), 
      col = "aliceblue", border = "white", freq = FALSE, 
      xlim = c(0, 24), xaxt = "n", xlab = "Time of day", 
      ylim = c(0, 1.02 * max(max(h$density), max(d$y[d.range]))), 
        # keep ^^^ in mind when doing non-arbitrary y axis units
      yaxt = "n", ylab = ""), 
    rug(timesofday, tick = 0.04, side = 1, 
        col = addAlpha("lightcoral", 0.6)), 
    axis(side = 1, at = xaxs$at, labels = xaxs$lab), 
    lines(d$x[d.range], d$y[d.range], lwd = 2, col = "lightcoral")
  )
}

weekPlot <- function(dates, timesofday, wdays, chron, 
                     yaxs = NULL, add = FALSE) {
  if (chron) {
    wdays <- wdays + normalize(dates) * 0.5
  } else {
    wdays <- jitter(wdays, factor = 1)
  }
  
  if (!add) {
    list(
      plot(wdays, timesofday, pch = 16, cex = 0.75, 
           col = addAlpha("#0080ff", 0.35),
           yaxt = "n", xaxt = "n", main = "", 
           ylab = "Time of day", ylim = c(0, 24), 
           xlab = "", xlim = c(1, 7)), 
      axis(side = 1, at = 1:7, 
           labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
      axis(side = 2, at = yaxs$at, labels = yaxs$lab), 
      axis(side = 4, at = yaxs$at, labels = yaxs$lab)
    )
  } else {
    list(
      points(wdays, timesofday, pch = 16, cex = 0.75, 
             col = addAlpha("lightcoral", 0.35))
    )
  }
}
