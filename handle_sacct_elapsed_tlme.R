
# sacct writes elapsed time in the format hour:minute:seconds 
# or day-hours:minute:seconds
#
# this cannot be natively handled by R...
# this function converts the elapsed time into seconds for further use

GetTime_inSeconds <- function(sacct_time) {

  # does it contain a day?
  if (grepl("-", sacct_time, fixed = TRUE)) {
    entries <- unlist(strsplit(sacct_time, split = "-"))
    entries <- c(entries[1], unlist(strsplit(entries[2], split = ":")))
  } else { # only hours:minutes:seconds
    entries <- c(0, unlist(strsplit(sacct_time, split = ":")))
  }

  # return time elapsed in seconds
            as.numeric(entries[4]) +
    60.0 * (as.numeric(entries[3]) +
    60.0 * (as.numeric(entries[2]) +
    24.0 * (as.numeric(entries[1])
  )))
}