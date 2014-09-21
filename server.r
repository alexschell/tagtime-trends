source("utils.R")
source("plotting.R")

shinyServer(
  function(input, output) {

    x <- parseLogfile(logfile.url, regexes, perl)
    timestamps <- x[[1]]
    matches <- x[[2]]
    
    output$description <- renderUI({
      helpText(descriptions[which(input$cat == catnames)])
    })
    
    output$legend <- renderUI({
      helpText(
        strong("Color coding:"), 
        div(input$cat, style = "color:#0080FF"), 
        div(input$cat2, style = "color:#F08080")) # lightcoral
    })
    
    output$daterangeUI <- renderUI({
      dateRangeInput("daterange", 
        label = "Time frame to display:", 
        start = as.Date(as.POSIXct(min(timestamps), origin = "1970-01-01")), 
        end = as.Date(as.POSIXct(max(timestamps), origin = "1970-01-01"))
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
        timeframe <- timestamps >= starttime & timestamps < endtime
      } else {
        timeframe <- rep(TRUE, length(timestamps))
      }
      timeframe
    })
    
    date.axis <- reactive({
      at <- seq(min(timestamps[timeframe()]), 
                 max(timestamps[timeframe()]), 
                 length = 8)
      lab <- as.Date(as.POSIXct(at, origin = "1970-01-01"))
      data.frame(at = at, lab = lab)
    })
    
    timeofday.axis <- reactive({
      # only works properly for input$midnight in 0:6
      mn <- as.numeric(input$midnight)
      if (mn == 0) {
        lab <- seq(0, 24, by = 6)
        at <- seq(0, 24, by = 6)
      } else {
        x <- c(mn:24, 1:mn)
        x <- x[2:(length(x) - 1)]
        x <- x[x %% 6 == 0]
        lab <- c(mn, x, mn)
        at <- lab - mn
        at[length(at)] <- 24
      }
      data.frame(at = at, lab = lab)
    })
    
    coords <- reactive({
      splitTimestamp(timestamps, midnight = as.numeric(input$midnight), tz = "EST5EDT")
    })
    
    output$hist <- renderPlot({
      n.cat <- which(input$cat == catnames)
      times.all <- timestamps[matches[,n.cat]]
      times.sub <- timestamps[timeframe()][matches[timeframe(), n.cat]]
      par(mar = c(par()$mar[1:3], 2.6))
      par(las = 1)
      histPlot(catname = input$cat, 
               times.all = times.all, 
               times.sub = times.sub, 
               n.bins = input$n.bins.hist, 
               units = switch(input$units.hist, 
                               "Hours per week" = 7, 
                               "Hours per day" = 1), 
               xaxs = date.axis(), 
               bandwidth = input$bandwidth.hist)
    })
    
    output$matrix <- renderPlot({
      n.cat <- which(input$cat == catnames)
      coords <- coords()[timeframe() & matches[,n.cat],]
      par(las = 1)
      matrixPlot(dates = coords$prev.midnight, 
                 timesofday = coords$timeofday, 
                 xaxs = date.axis(), 
                 yaxs = timeofday.axis())
      if (input$addcat) {
        n.cat2 <- which(input$cat2 == catnames)
        coords2 <- coords()[timeframe() & matches[,n.cat2],]
        matrixPlot(dates = coords2$prev.midnight, 
                   timesofday = coords2$timeofday, 
                   add = TRUE)
        title(paste(input$cat, "and", tolower(input$cat2), 
                    "(by date and time of day)"))
      } else {
        title(paste(input$cat, "(by date and time of day)"))
      }
    })
    
    # Note antialiasing doesn't work in the deployed version
    output$scatter <- renderPlot({
      n.cat <- which(input$cat == catnames)
      n.catx <- which(input$catx == catnames)
      
      # this is still very repetitive
      dates <- coords()$prev.midnight[timeframe()]
      y <- countPings(dates = dates,  
                      subset = matches[timeframe(), n.cat])
      x <- countPings(dates = dates, 
                      subset = matches[timeframe(), n.catx])
      y <- y * ping.interval / 60
      x <- x * ping.interval / 60
      
      if(!input$trellis) {
        scatterPlot(x, y, z = NULL, 
                    names = c(input$cat, input$catx), 
                    jitter = input$jitter, 
                    trellis = input$trellis)
      } else {
        n.catc <- which(input$catc == catnames)
        z <- countPings(dates = dates, 
                        subset = matches[timeframe(), n.catc])
        scatterPlot(x, y, z, 
                    names = c(input$cat, input$catx, input$catc), 
                    jitter = input$jitter, 
                    trellis = input$trellis)
      }
    })
    
    output$timeofday <- renderPlot({
      n.cat <- which(input$cat == catnames)
      wdays <- switch(input$weekend,
                "Weekdays only" = 1:5, 
                "Weekends only" = 6:7, 
                "All days" = 1:7)
      timesofday <- coords()$timeofday[timeframe() & matches[,n.cat] & 
                                       coords()$wday %in% wdays]
      timeofdayPlot(catname = input$cat, 
                  timesofday = timesofday, 
                  xaxs = timeofday.axis(), 
                  bandwidth = input$bandwidth.tod)
    })
    
    output$week <- renderPlot({
      n.cat <- which(input$cat == catnames)
      coords <- coords()[timeframe() & matches[,n.cat],]
      par(las = 1)
      weekPlot(dates = coords$prev.midnight, 
               timesofday = coords$timeofday, 
               wdays = coords$wday, 
               chron = input$ordered.week,
               yaxs = timeofday.axis())
      if (input$addcat) {
        n.cat2 <- which(input$cat2 == catnames)
        coords2 <- coords()[timeframe() & matches[,n.cat2],]
        weekPlot(dates = coords2$prev.midnight, 
                 timesofday = coords2$timeofday, 
                 wdays = coords2$wday, 
                 chron = input$ordered.week, 
                 add = TRUE)
        title(paste(input$cat, "and", tolower(input$cat2), 
                    "(by weekday and time of day)"))
      } else {
        title(paste(input$cat, "(by weekday and time of day)"))
      }
    })
  }
)
