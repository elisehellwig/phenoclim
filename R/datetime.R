#functions that make it easier to use dates with the phenology data

dayToDate <- function(years, days, hours=NA, startTime='00:00:00',
                      timezone='America/Los_Angeles') {

    datestring <- as.Date(days-1, origin=paste0(years, '-01-01'))

    if (is.na(hours[1])) {
        dateTimeString <- paste0(datestring, ' ', startTime)

    } else {
        timestring <- paste0(sprintf("%02d", hours), ':00:00')
        dateTimeString <- paste(datestring, timestring)

    }


    posix <- as.POSIXct(dateTimeString, "%Y-%m-%d %H:%M:%OS",
                        tz=timezone)

    return(posix)

}
