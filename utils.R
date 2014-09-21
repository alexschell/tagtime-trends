# Miscellaneous functions

require("httr")

parseLogfile <- function(logfile.url, regexes, perl) {
  log <- content(GET(logfile.url))
  log <- unlist(strsplit(log, "\n"))
  log <- strsplit(log, " ")
  timestamps <- sapply(log, function(x) as.numeric(x[1]))
    # vector of unix timestamps corresponding to every ping
  tags <- sapply(log, 
                 function(x) {
                   nonempty <- x[x != ""]
                   paste(nonempty[-(length(nonempty) - 2:0)][-1], 
                         collapse = " ")
                 })
  matches <- sapply(regexes, function(x) grepl(x, tags, perl = perl))
    # boolean matrix with dimensions length(timestamps) x length(regexes)
    # e.g. matches[, 1] == grepl(regexes[1], tags)
  list(timestamps, matches)
}

splitTimestamp <- function(timestamp, tz = "", midnight = 0, hrs = TRUE) {
  if (!(midnight %in% 0:23)) {
    warning("your midnight argument is invalid. using default 00:00:00")
    midnight <- 0
  }
  if (midnight %in% 0:9) {
    midnight <- paste("0", midnight, ":00:00", sep = "")
  } else {
    midnight <- paste(midnight, ":00:00", sep = "")
  }
  day <- as.Date(substr(as.POSIXct(timestamp, origin = "1970-01-01", tz = tz), 1, 10))
  mn <- as.numeric(as.POSIXct(paste(day, midnight), tz = tz))
  mn <- ifelse(mn > timestamp, 
               as.numeric(as.POSIXct(paste(as.Date(day - 1), midnight), tz = tz)), 
               mn)
  timeofday <- timestamp - mn
  if (hrs) {
    timeofday <- timeofday / 3600
  }
  wday <- as.POSIXlt(timestamp, tz = tz, origin = "1970-01-01")$wday
  wday[wday == 0] <- 7
  data.frame(prev.midnight = mn, timeofday = timeofday, wday = wday)
}

addAlpha <- function(col, alpha = 1) {
  col = col2rgb(col) / 255
  rgb(col[1], col[2], col[3], alpha = alpha)
}

padding <- function(x, fraction) {
  span <- diff(range(x))
  x.left <- x[x < min(x) + fraction * span]
  x.right <- x[x > max(x) - fraction * span]
  count.left <- length(x.left)
  count.right <- length(x.right)
  pad.left <- seq(min(x) - fraction * span, min(x) - 1, length = count.left)
  pad.right <- seq(max(x) + 1, max(x) + fraction * span, length = count.right)
  c(pad.left, x, pad.right)
}

# can/do we want to/ rewrite this to work with normal timestamps
# given the problems with splitTimestamp()?
countPings <- function(dates, subset) {
  sapply(unique(dates), 
         function(x) {
           length(dates[dates == x & subset])
         })
}
