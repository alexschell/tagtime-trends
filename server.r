require("httr")
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
      n.breaks <- as.numeric(input$n.bins) + 1
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
      d <- density(uptime, adjust = input$bandwidth, cut = -1.5)
      d$y <- d$y * length(uptime) * ping.interval / 60 * 7 * 24 * 3600
        # scale density to plot
      par(las = 1)
      plot(h, main = paste(input$cat, "(hours per week)"), 
        col = "aliceblue", border = "white", ylab = "", 
        xaxt = 'n', xlab = "")
      axis(side = 1, at = xlab.int, labels = xlab.str)
      axis(4)
      rug(uptime.sub, tick = 0.04, side = 1, col = "lightcoral")
      lines(d, lwd = 2, col = "lightcoral")
    })
    
    coords <- reactive({
      splitTimestamp(dat$time, midnight = input$midnight, tz = "EST5EDT")
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
           pch = "_", cex = 1, col = "lightcoral", 
           main = paste(input$cat, "(by date and time of day)"), 
           xlab = "", ylab = "Time of day", xaxt = "n", yaxt = "n", 
           xlim = range(xlab.int), xaxs = "i", 
           ylim = c(0,24), yaxs = "i", bty = "n")
      )
      # ylabels only work properly for custom midnights in 0:6
      if (input$midnight == 0) {
        ylabels <- seq(0, 24, by = 6)
        yat <- seq(0, 24, by = 6)
      } else {
        mn <- as.numeric(input$midnight)
        x <- c(mn:24, 1:mn)
        x <- x[2:(length(x) - 1)]
        x <- x[x %% 6 == 0]
        ylabels <- c(mn, x, mn)
        yat <- ylabels - mn
        yat[length(yat)] <- 24
      }
      axis(side = 2, at = yat, labels = ylabels)
      axis(side = 4, at = yat, labels = ylabels)
      axis(side = 1, at = xlab.int, labels = xlab.str)
      abline(h = 24)
    })
    
    # Note antialiasing doesn't work in the deployed version
    output$xy <- renderPlot({
      n.cat <- which(input$cat == catnames)
      n.xcat <- which(input$xcat == catnames)
      y <- sapply(unique(coords()$prev.midnight), 
                  function(x) {
                    index <- coords()$prev.midnight == x & 
                      mat[,n.cat] & timeframe()
                    nrow(coords()[index,]) * ping.interval / 60
                  })
      x <- sapply(unique(coords()$prev.midnight), 
                  function(x) {
                    index <- coords()$prev.midnight == x & 
                      mat[,n.xcat] & timeframe()
                    nrow(coords()[index,]) * ping.interval / 60
                  })
      if (input$jitter) {
        y <- jitter(y, factor = 3)
        x <- jitter(x, factor = 3)
      }
      plot(x, y, pch = 15, cex = 0.7, 
           main = paste(input$cat, "vs.", tolower(input$xcat), 
                        "(aggregated by day)"), 
           xlab = paste(input$xcat, "(estimated hours)"), 
           ylab = paste(input$cat, "(estimated hours)"))
    })
  }
)
