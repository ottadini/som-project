# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
# Comparison of smoothing windows on well-log data
#
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

library(signal)

DespikeLogs <- function(df, ignoreColnames) {  # df is the data frame of well logs
  # Runs median filter to remove signal spikes. 
  # Apply median filter to some columns. runmed cannot handle NAs:
  df[is.na(df)] <- 0  # replace NAs with zero in any column
  skip.indices <- which(colnames(df) %in% ignoreColnames)
  df[, -skip.indices] <- 
    sapply(
      df[, -skip.indices], 
      runmed, k=5)
  return(df)
}

SmoothLogs <- function(df, width=67, ignoreColnames){
  # Apply smoothing filter to some columns
  skip.indices <- which(colnames(df) %in% ignoreColnames)
  df[, -skip.indices] <- 
    sapply(
      df[, -skip.indices], 
      sgolayfilt, n=width)
  return(df)  
}

quickplot <- function(a, b, w){
  yylim <- c(0, 100)
  xxlim <- c(300, 400)
  
  plot(a, type="l", ylim=yylim, xlim=xxlim)
  par(new=TRUE)
  plot(b, type="l", ylim=yylim, col="red", xlim=xxlim)
  title(main=paste("Savitzky-Golay filter width=", w, collapse=""), 
        ylab=NULL)
}


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Set root data directory 
root_dir <- "~/Documents/DATA/Conductivity/SOM_project"
setwd(root_dir)

# A set of columns to ignore or exclude in some operations
kSkipColnames <- c("BORENAME", "Borehole", "DEPTH", "MEAS_TC", "MEAS_DEN", "MEAS_PORO", "STRAT")

# Process all logs in a directory at one go.
# Load all log data files into one big object.
# Get a list of all the files in specified directory path:
filenames <- list.files(path="merged_log_lab_data", full.names=TRUE)

# Load each file's data into a list of data frames:
logs <- lapply(filenames, read.csv, na.strings="NA", stringsAsFactors=FALSE)

# Add a 'names' attribute to the list of data frames:
borenames <- sub("_merged\\.csv", "", lapply(filenames, basename))
names(logs) <- borenames

# De-spike each files' logs using runmed:
logs.d <- lapply(logs, DespikeLogs, kSkipColnames)

# Apply smoothing, maybe Savitzky-Golay or running mean, with window of ~10 m.
# For well-logs with measurements at 6 inches (~0.15 metres), that means a 
# window of about 66:
logs.s37 <- lapply(logs.d, SmoothLogs, 37, kSkipColnames)
logs.s47 <- lapply(logs.d, SmoothLogs, 47, kSkipColnames)
logs.s57 <- lapply(logs.d, SmoothLogs, 57, kSkipColnames)
logs.s67 <- lapply(logs.d, SmoothLogs, 67, kSkipColnames)

quickplot(logs.d$B15005$GR, logs.s37$B15005$GR, 37)
quickplot(logs.d$B15005$GR, logs.s47$B15005$GR, 47)
quickplot(logs.d$B15005$GR, logs.s57$B15005$GR, 57)
quickplot(logs.d$B15005$GR, logs.s67$B15005$GR, 67)

