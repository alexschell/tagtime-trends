require("httr")

shinyServer(
  function(input, output) {

    ttlog <- content(GET(logfile.url))  # CHANGEME
    ttlog <- unlist(strsplit(ttlog, "\n"))
    ttlog <- strsplit(ttlog, " ")
    dat <- data.frame(time = sapply(ttlog, function(x) as.numeric(x[1])), 
                      tags = sapply(ttlog, function(x) {
                        str <- x[x != ""]
                        paste(str[-(length(str) - 2:0)][-1], collapse = " ")
                      }))
    
    mat <- sapply(tags, function(x) grepl(x, dat$tags, perl = perl))
    x.label.int <- seq(min(dat$time), max(dat$time), length = 8)
    x.label.str <- as.Date(as.POSIXct(x.label.int, origin = "1970-01-01"))
    
    output$description <- renderText({
      descriptions[which(input$tag == choices)]
    }) 
    
    output$hist <- renderPlot({
      n.breaks <- as.numeric(input$n.bins) + 1
      breaks <- seq(min(dat$time), max(dat$time), length = n.breaks)
      bin.width <- diff(breaks[1:2])  # histogram bin width in seconds
      n.tag <- which(input$tag == choices)
      uptime <- dat$time[mat[,n.tag]]
      h <- hist(uptime, plot = FALSE, breaks = breaks)
      h$counts <- h$counts * ping.interval / 60 * 7 * 24 * 3600 / bin.width
        # convert from pings per bin to hours per week
      d <- density(uptime, adjust = input$bandwidth, cut = -2)
      d$y <- d$y * sum(h$counts) * bin.width  # scale density to plot
      par(las = 1)
      plot(h, main = paste(input$tag, "(hours per week)"), 
        col = "aliceblue", border = "white", ylab = "", 
        xaxt = 'n', xlab = "")
      axis(side = 1, at = x.label.int, labels = x.label.str)
      axis(4)
      rug(uptime, tick = 0.04, side = 1, col = "lightcoral")
      lines(d, lwd = 2, col = "lightcoral")
    })
  }
)