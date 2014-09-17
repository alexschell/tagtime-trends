require("httr")
require("lattice")
source("utils.R")

shinyServer(
  function(input, output) {

    ttlog <- content(GET(logfile.url))
    ttlog <- unlist(strsplit(ttlog, "\n"))
    ttlog <- strsplit(ttlog, " ")
    dat <- data.frame(time = sapply(ttlog, function(x) as.numeric(x[1])), 
                      tags = sapply(ttlog, function(x) {
                        str <- x[x != ""]
                        paste(str[-(length(str) - 2:0)][-1], collapse = " ")
                      }))
    
    mat <- sapply(regexes, function(x) grepl(x, dat$tags, perl = perl))
    
    output$description <- renderText({
      descriptions[which(input$cat == catnames)]
    })
    
    output$daterangeUI <- renderUI({
      dateRangeInput("daterange", 
        label = "Time frame to display:", 
        start = as.Date(as.POSIXct(min(dat$time), origin = "1970-01-01")), 
        end = as.Date(as.POSIXct(max(dat$time), origin = "1970-01-01"))
      )
    })
    
    output$midnightUI <- renderUI({
      selectInput("midnight", 
                  label = "Custom midnight", 
                  choices = 0:6, 
                  selected = 0)
    })
    
    timeframe <- reactive({
      if (!is.null(input$daterange)) {
        starttime <- paste(input$daterange[1], "00:00:00")
        starttime <- as.numeric(as.POSIXct(starttime))
        endtime <- paste(as.Date(input$daterange[2] + 1), "00:00:00")
        endtime <- as.numeric(as.POSIXct(endtime))
        timeframe <- dat$time >= starttime & dat$time < endtime
      } else {
        timeframe <- rep(TRUE, nrow(dat))
      }
      timeframe
    })
    
    output$hist <- renderPlot({
      n.breaks <- as.numeric(input$n.bins_hist) + 1
      breaks <- seq(min(dat$time[timeframe()]), 
                    max(dat$time[timeframe()]), 
                    length = n.breaks)
      bin.width <- diff(breaks[1:2])  # histogram bin width in seconds
      n.cat <- which(input$cat == catnames)
      uptime <- dat$time[mat[,n.cat]]
      uptime.sub <- dat$time[timeframe()][mat[timeframe(), n.cat]]
      # better solution here? ^^^
      xlab.int <- seq(min(dat$time[timeframe()]), 
                      max(dat$time[timeframe()]), 
                      length = 8)
      xlab.str <- as.Date(as.POSIXct(xlab.int, origin = "1970-01-01"))
      h <- hist(uptime.sub, plot = FALSE, breaks = breaks)
      h$counts <- h$counts * ping.interval / 60 * 7 * 24 * 3600 / bin.width
        # convert from pings per bin to hours per week
      # d <- density(uptime, adjust = input$bandwidth_hist, cut = -1.5)
      # d$y <- d$y * length(uptime) * ping.interval / 60 * 7 * 24 * 3600
        # # scale density to plot
      # uptime padding hack to avoid density going to zero at extremes
      pad.fraction <- 0.1
      uptime.padded <- padding(uptime, pad.fraction)
      d <- density(uptime.padded, adjust = input$bandwidth_hist)
      d$y <- d$y * length(uptime.padded) * ping.interval / 60 * 7 * 24 * 3600
      # /hack
      par(las = 1)
      plot(h, main = paste(input$cat, "(hours per week)"), 
        col = "aliceblue", border = "white", ylab = "", 
        xaxt = "n", xlab = "")
      axis(side = 1, at = xlab.int, labels = xlab.str)
      axis(4)
      rug(uptime.sub, tick = 0.04, side = 1, col = mkColor("lightcoral", 0.6))
      # lines(d, lwd = 2, col = "lightcoral")
      # padding hack
      d.range <- d$x >= min(uptime) & d$x <= max(uptime)
      lines(d$x[d.range], d$y[d.range], lwd = 2, col = "lightcoral")
      # /hack
    })
    
    coords <- reactive({
      splitTimestamp(dat$time, midnight = input$midnight, tz = "EST5EDT")
    })
    
    hr.axis <- reactive({
      # only works properly for input$midnight in 0:6
      if (input$midnight == 0) {
        lab <- seq(0, 24, by = 6)
        loc <- seq(0, 24, by = 6)
      } else {
        mn <- as.numeric(input$midnight)
        x <- c(mn:24, 1:mn)
        x <- x[2:(length(x) - 1)]
        x <- x[x %% 6 == 0]
        lab <- c(mn, x, mn)
        loc <- lab - mn
        loc[length(loc)] <- 24
      }
      data.frame(loc = loc, lab = lab)
    })
    
    output$matrix <- renderPlot({
      n.cat <- which(input$cat == catnames)
      xlab.int <- seq(min(dat$time[timeframe()]), 
                      max(dat$time[timeframe()]), 
                      length = 8)
      xlab.str <- as.Date(as.POSIXct(xlab.int, origin = "1970-01-01"))
      par(las = 1)
      with(coords()[mat[,n.cat],], 
           plot(prev.midnight, time.of.day, 
           pch = "+", cex = 0.75, col = mkColor("#0080ff", 0.5), 
           main = paste(input$cat, "(by date and time of day)"), 
           xlab = "", ylab = "Time of day", xaxt = "n", yaxt = "n", 
           xlim = range(xlab.int), xaxs = "i", 
           ylim = c(0,24), yaxs = "i", bty = "n")
      )
      yaxis <- hr.axis()
      axis(side = 2, at = yaxis$loc, labels = yaxis$lab)
      axis(side = 4, at = yaxis$loc, labels = yaxis$lab)
      axis(side = 1, at = xlab.int, labels = xlab.str)
      abline(h = 24)
    })
    
    # Note antialiasing doesn't work in the deployed version
    output$xy <- renderPlot({
      n.cat <- which(input$cat == catnames)
      n.xcat <- which(input$xcat == catnames)
      y <- sapply(unique(coords()$prev.midnight[timeframe()]), 
                  function(x) {
                    index <- coords()$prev.midnight[timeframe()] == x & 
                      mat[timeframe(), n.cat]
                    nrow(coords()[timeframe(),][index,]) * ping.interval / 60
                  })
      x <- sapply(unique(coords()$prev.midnight[timeframe()]), 
                  function(x) {
                    index <- coords()$prev.midnight[timeframe()] == x & 
                      mat[timeframe(), n.xcat]
                    nrow(coords()[timeframe(),][index,]) * ping.interval / 60
                  })
      if (input$jitter) {
        y <- jitter(y, factor = 2)
        x <- jitter(x, factor = 2)
      }
      if(input$ccat == "(None)") {
        xyplot(y ~ x, pch = "+", cex = 2, alpha = 0.5, 
             main = paste(input$cat, "vs.", tolower(input$xcat), 
                          "(aggregated by day)"), 
             xlab = paste(input$xcat, "(estimated hours)"), 
             ylab = paste(input$cat, "(estimated hours)"))
      } else {
        n.ccat <- which(input$ccat == catnames)
        z <- sapply(unique(coords()$prev.midnight[timeframe()]), 
                    function(x) {
                      index <- coords()$prev.midnight[timeframe()] == x & 
                        mat[timeframe(), n.ccat]
                      nrow(coords()[timeframe(),][index,]) * ping.interval / 60
                    })
        Level = equal.count(z, 4, overlap = 0.1)
        xyplot(y ~ x | Level, pch = "+", cex = 2, alpha = 0.5, 
               main = paste(input$cat, "vs.", tolower(input$xcat), 
                            " - conditioned on", tolower(input$ccat)), 
               xlab = paste(input$xcat, "(estimated hours)"), 
               ylab = paste(input$cat, "(estimated hours)"))
      }
    })
    
    output$tod <- renderPlot({
      wdays <- switch(input$weekend,
                      "Weekdays only" = 1:5, 
                      "Weekends only" = 6:7, 
                      "All days" = 1:7)
      n.cat <- which(input$cat == catnames)
      x <- coords()$time.of.day[timeframe() & 
        mat[,n.cat] & coords()$wday %in% wdays]
      xcyc <- c(x - 24, x, x + 24)
      h <- hist(x, breaks = 12, plot = FALSE)
      plot(h, main = paste(input$cat, "(by time of day)"), 
        col = "aliceblue", border = "white", freq = FALSE, 
        xlim = c(0, 24), xaxt = "n", xlab = "Time of day", 
        yaxt = "n", ylab = "")
      rug(x, tick = 0.04, side = 1, col = mkColor("lightcoral", 0.6))
      yaxis <- hr.axis()
      axis(side = 1, at = yaxis$loc, labels = yaxis$lab)
      d <- density(xcyc, adjust = input$bandwidth_tod)
      d$y <- d$y * 3
      d.range <- d$x >= 0 & d$x < 24
      lines(d$x[d.range], d$y[d.range], lwd = 2, col = "lightcoral")
    })
    
    output$week <- renderPlot({
      x <- coords()
      if (input$ordered_week) {
        daterange <- range(x$prev.midnight)
        date.offset <- x$prev.midnight - min(x$prev.midnight)
        date.offset <- date.offset / diff(range(x$prev.midnight))
        date.offset <- date.offset * 0.5 - 0.25
        x$wday <- x$wday + date.offset
      } else {
        x$wday <- jitter(x$wday, factor = 1)
      }
      n.cat <- which(input$cat == catnames)
      par(las = 1)
      plot(x$wday[timeframe() & mat[,n.cat]], 
           x$time.of.day[timeframe() & mat[,n.cat]], pch = "+", cex = 0.75, 
           col = mkColor("#0080ff", 0.5),
           yaxt = "n", xaxt = "n", 
           main = paste(input$cat, "(by weekday and time of day)"), 
           ylab = "Time of day", xlab = "")
      axis(side = 1, at = 1:7, 
           labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
      yaxis <- hr.axis()
      axis(side = 2, at = yaxis$loc, labels = yaxis$lab)
      axis(side = 4, at = yaxis$loc, labels = yaxis$lab)
    })
  }
)
