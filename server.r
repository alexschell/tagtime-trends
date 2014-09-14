require("httr")

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
    
    output$hist <- renderPlot({
      if (!is.null(input$daterange)) {
        starttime <- paste(input$daterange[1], "12:00:00")
        starttime <- as.numeric(as.POSIXct(starttime))
        endtime <- paste(as.Date(input$daterange[2] + 1), "12:00:00")
        endtime <- as.numeric(as.POSIXct(endtime))
        timeframe <- dat$time >= starttime & dat$time < endtime
      } else {
        timeframe <- rep(TRUE, nrow(dat))
      }
      n.breaks <- as.numeric(input$n.bins) + 1
      breaks <- seq(min(dat$time[timeframe]), 
                    max(dat$time[timeframe]), 
                    length = n.breaks)
      bin.width <- diff(breaks[1:2])  # histogram bin width in seconds
      n.cat <- which(input$cat == catnames)
      uptime <- dat$time[mat[,n.cat]]
      uptime.sub <- dat$time[timeframe][mat[timeframe, n.cat]]
      # better solution here?
      xlab.int <- seq(min(dat$time[timeframe]), 
                         max(dat$time[timeframe]), 
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
  }
)
