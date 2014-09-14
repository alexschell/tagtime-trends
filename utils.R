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
  day <- as.Date(substr(as.POSIXct(timestamp, origin = "1970-01-01"), 1, 10))
  mn <- as.numeric(as.POSIXct(paste(day, midnight)))
  mn <- ifelse(mn > timestamp, 
               as.numeric(as.POSIXct(paste(as.Date(day - 1), midnight))), 
               mn)
  timeofday <- timestamp - mn
  if (hrs) {
    timeofday <- timeofday / 3600
  }
  data.frame(prev.midnight = mn, time.of.day = timeofday)
}
