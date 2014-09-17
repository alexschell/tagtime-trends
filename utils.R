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
  data.frame(prev.midnight = mn, time.of.day = timeofday, wday = wday)
}

mkColor <- function(col, alpha = 1) {
  col = col2rgb(col) / 255
  rgb(col[1], col[2], col[3], alpha = alpha)
}

padding <- function(x, fraction = 0.1) {
  span <- diff(range(x))
  x.left <- x[x < min(x) + fraction * span]
  x.right <- x[x > max(x) - fraction * span]
  count.left <- length(x.left)
  count.right <- length(x.right)
  pad.left <- seq(min(x) - fraction * span, min(x) - 1, length = count.left)
  pad.right <- seq(max(x) + 1, max(x) + fraction * span, length = count.right)
  c(pad.left, x, pad.right)
}