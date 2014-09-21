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
  day <- as.Date(as.POSIXlt(timestamp, origin = "1970-01-01", tz = tz))
  mn <- as.numeric(as.POSIXct(paste(day, midnight), tz = tz))
  mn <- ifelse(mn > timestamp, 
               as.numeric(as.POSIXct(paste(as.Date(day - 1), midnight), tz = tz)), 
               mn)
  timeofday <- timestamp - mn
  # or convert to POSIXlt and get time difference
  if (hrs) {
    timeofday <- timeofday / 3600
  }
  wday <- as.POSIXlt(mn, tz = tz, origin = "1970-01-01")$wday
  wday[wday == 0] <- 7
  data.frame(prev.midnight = mn, timeofday = timeofday, wday = wday)
}

addAlpha <- function(col, alpha = 1) {
  col = col2rgb(col) / 255
  rgb(col[1], col[2], col[3], alpha = alpha)
}

padding <- function(xs, fraction) {
  span <- diff(range(xs))
  xs.left <- xs[xs < min(xs) + fraction * span]
  xs.right <- xs[xs > max(xs) - fraction * span]
  count.left <- length(xs.left)
  count.right <- length(xs.right)
  pad.left <- seq(min(xs) - fraction * span, min(xs) - 1, length = count.left)
  pad.right <- seq(max(xs) + 1, max(xs) + fraction * span, length = count.right)
  c(pad.left, xs, pad.right)
}

# can/do we want to/ rewrite this to work with normal timestamps
# given the problems with splitTimestamp()?
countPings <- function(dates, subset) {
  sapply(unique(dates), 
         function(x) {
           length(dates[dates == x & subset])
         })
}

normalize <- function(xs) {
  span <- diff(range(xs))
  (xs - min(xs)) / span - 0.5
}
