# ===== Preparation of data for use in SiroSOM software.
#
# Before anything else, de-spike using a running median filter (stats::runmed).
#
# Several data sets are required:
# 1.
# First method is a complete-cases analysis with cross-validation, so requires
# a complete-cases data set. 
# The main issue is selecting a balance between a large enough set of variables, 
# and a large enough set of boreholes containing those variables. 
# 2.
# A SOM imputation data set - the same as the first, except with a few extra 
# rows containing no TC data for which we can 'predict' a TC value. How to 
# select the extra rows? I don't know, but possibly look at the graphical 
# well logs from BR Thompson's thesis to decide, or Pechnig key wells. Should
# contain representatives from each of the main strat units I want to examine.
# 3.
# An alternative SOM data set, where each borehole will be treated individually, 
# to produce data-driven clusters that will be then be assigned TC values that
# relate to identified lithological clusters. This one needs lith and strat 
# columns
# 4.
# A set of any of the above that also contains temperature gradient (regardless
# of whether it has been affected by non-conductive transients). It will be 
# interesting to see whether and how the linear regression and SOM techniques 
# are affected.

library(sqldf)
library(signal)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# FUNCTIONS

kSkipColnames <- c("BORENAME", "DEPTH", "MEAS_TC", "MEAS_DEN", "MEAS_PORO", "STRAT")

CompleteCasesInclude <- function(d, desiredCols) {
    # Remove rows from logs with NAs in (only) the well-log responses (i.e. not the lab results).
    # This function allows specific columns to be targetted for filtering
    completeVec <- complete.cases(d[, desiredCols])
    return(d[completeVec, ])
}

CompleteCasesExclude <- function(d, undesiredColnames) {
    # The inverse of above 'CompleteCasesInclude' allows specific columns to be ignored
    # But we have to use column indexes, not text labels:
    # df[, -5] is OK, but df[, -"TC"] is not.
    skipVec <- which(colnames(d) %in% undesiredColnames)  # returns a vector of column indices
    completeVec <- complete.cases(d[, -skipVec])  # the minus sign chooses all columns but the skipVec ones
    return(d[completeVec, ])
}

DespikeLogs <- function(df) {  # df is the data frame of well logs
    # Get the complete.cases (i.e. remove rows with NAs) excluding certain columns
    logs.clean <- CompleteCasesExclude(df, kSkipColnames)
    # Apply median filter to some columns. runmed cannot handle NAs:
    logs.clean[is.na(logs.clean)] <- 0  # replace NAs with zero in any column
    skip.indices <- which(colnames(logs.clean) %in% kSkipColnames)
    logs.clean[, -skip.indices] <- 
        sapply(
            logs.clean[, -skip.indices], 
            runmed, k=5)
    return(logs.clean)
}
    
SmoothLogs <- function(df){
  # Apply smoothing filter to some columns
  skip.indices <- which(colnames(df) %in% kSkipColnames)
  df[, -skip.indices] <- 
    sapply(
      df[, -skip.indices], 
      sgolayfilt, n=67)  # window about 10 metres
  return(df)  
}

ScaleLogs <- function(df){
    # Scale and centre certain log response values
    skip.indices <- which(colnames(df) %in% kSkipColnames)
    df[, -skip.indices] <- 
        sapply(
            df[, -skip.indices], 
            scale, center=TRUE)  # I think I need center=TRUE
    return(df)
}

SubsetOnlyTC <- function(df) {
    # Create a df with only TC result rows
    tc.subset <- sqldf("
                        SELECT * 
                        FROM df l
                        WHERE l.MEAS_TC > 0
                        ORDER BY DEPTH
                        ")
    return(tc.subset)
}
SubsetTCNeighbours <- function(df, window=5) {   
    # Create a subset of the well-log data that includes
    # depths for those rows with TC data and their immediate neighbours
    # with 'window' as given in function argument.
    depths <- sqldf("
                SELECT l.DEPTH 
                FROM df l
                WHERE l.MEAS_TC > 0
                ORDER BY DEPTH
                ")
    
    # Secondly create a df with some neighbouring rows as well
    # Filter neighbouring log depths of TC values.
    # Need to use the fn$sqldf variant to allow for variables in the
    # SQL statement (i.e. so I can use $window)
    neighbours <- fn$sqldf("
                    SELECT DISTINCT l.*
                    FROM df l, depths d
                    WHERE l.DEPTH >= d.DEPTH - $window
                    AND l.DEPTH <= d.DEPTH + $window
                    ORDER BY DEPTH
                    ")
    
    # Write neighbours to file (CSV)
    filename <- "B15005.csv"
    write.csv(neighbours, file=paste("subsets", filename, sep="/"), row.names=FALSE)
    return(neighbours)
}



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Data processing

# Process all logs in a directory at one go.
# Set root data directory 
root_dir <- "~/Documents/DATA/Conductivity/SOM_project"
setwd(root_dir)

# Load log data into one big object
# Get a list of all the files in specified directory path:
filenames <- list.files(path="merged_log_lab_data", full.names=TRUE)

# Load each file's data into a list of data frames:
logs.list <- lapply(filenames, read.csv, na.strings="NA", stringsAsFactors=FALSE)

# Add a 'names' attribute to the list of data frames:
borenames <- sub("_merged\\.csv", "", lapply(filenames, basename))
names(logs.list) <- borenames

# De-spike each files' logs using runmed:
logs.despiked <- lapply(logs.list, DespikeLogs)
rm(logs.list)

# Apply smoothing, maybe Savitzky-Golay r running mean:
logs.smoothed <- lapply(logs.despiked, SmoothLogs)
rm(logs.despiked)

# Save a merged data set to filenow, before scaling/normalisation:
# Add a borename attribute / variable / column:
logs.named <- mapply(cbind, logs.smoothed, "Borehole"=borenames, SIMPLIFY=FALSE)
# Merge complete despiked and smoothed data set to one, and write to file:
df.all.data <- Reduce(function(x, y) merge(x, y, all=TRUE), logs.named)
rm(logs.named)
df.all.data$MEAS_DEN[df.all.data$MEAS_DEN==0] <- -999.25
df.all.data$MEAS_PORO[df.all.data$MEAS_PORO==0] <- -999.25
df.all.data$MEAS_TC[df.all.data$MEAS_TC==0] <- -999.25
#df.all.data[is.na(df.all.data)] <- -999.25
write.csv(df.all.data, file="subset_all_data.csv", na="-999.25", eol="\n", row.names=FALSE, quote=FALSE)
rm(df.all.data)

# Scale and centre for SOM (SOM uses Euclidean Distance measure):
logs.scaled <- lapply(logs.smoothed, ScaleLogs)
rm(logs.smoothed)

# Add a borename attribute / variable / column:
logs.named <- mapply(cbind, logs.scaled, "Borehole"=borenames, SIMPLIFY=FALSE)
rm(logs.scaled)

# Merge complete scaled data set to one, and write to file:
df.all.data <- Reduce(function(x, y) merge(x, y, all=TRUE), logs.named)
df.all.data$MEAS_DEN[df.all.data$MEAS_DEN==0] <- -999.25
df.all.data$MEAS_PORO[df.all.data$MEAS_PORO==0] <- -999.25
df.all.data$MEAS_TC[df.all.data$MEAS_TC==0] <- -999.25
write.csv(df.all.data, file="subset_all_data_scaled.csv", eol="\n", row.names=FALSE, quote=FALSE, na="-999.25")
rm(df.all.data)

# Create the TC only subsets, merge, and write to file:
logs.tconly <- lapply(logs.named, SubsetOnlyTC)
df.tconly <- Reduce(function(x, y) merge(x, y, all=TRUE), logs.tconly)
write.csv(df.tconly, file="subset_tc_only.csv", eol="\n", row.names=FALSE)
rm(logs.named)
rm(logs.tconly)


# Some examples using sqldf to read in data from CSV:
# Load specific portion of the ALL data set using read.csv.sql:
boisdale <- read.csv.sql("subset_all_data.csv", sql="select * from file where STRAT = 'Boisdale Formation'", comment.char="")
tc <- read.csv.sql("subset_all_data.csv", sql="select * from file where MEAS_TC > 0", comment.char="")

# Create test and training sets from data:
mydata <- df.tconly
inTrain <- sample(nrow(mydata), nrow(mydata)*(2/3))
training <- mydata[inTrain, ]  # two-thirds of data
testing <- mydata[-inTrain, ]  # remaining one-third







# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Data processing for single well-log files.

# Target data frames to hold compiled log data. 
# tc.only holds data rows with TC measurements,
# tc.neighbours holds tc data plus a few rows either side.
tc.only <- NULL  # can add rows using rbind to NULL.
tc.neighbours <- NULL

filename <- paste("merged_log_lab_data", "B15005_merged.csv", sep="/")
borename <- sub("_merged.csv", "", basename(filename))
logs <- read.csv(filename, na.strings="NA")

# Skip these columns in both complete.cases filter, and running median:
skip.colnames <- c("DEPTH", "MEAS_TC", "MEAS_DEN", "MEAS_PORO", "STRAT")

# logs.clean <- ImPartialCompleteCases(logs, c("DEPTH", "DT", "GR", "LN", "NEUT", "RES", "SN", "SP", "TEMP", "STRAT"))
logs.clean <- PartialCompleteCases(logs, skip.colnames)
logs.clean[is.na(logs.clean)] <- 0  # replace NAs with zero.

# De-spike the logs using a running median. Window only needs to be big enough
# to encompass any spikes, with a little extra for wiggle room. If typical 
# spike is 3 points, then a filter with k=5 is enough.
# Perform SMOOTHING / UPSCALING using a different procedure, maybe with a 
# Savitzky-Golay (polynomial fitting) filter, or running mean.

# Apply filter to some of the columns:
despike <- logs.clean
skip.indices <- which(colnames(despike) %in% skip.colnames)
despike[, -skip.indices] <- 
  sapply(
    despike[, -skip.indices], 
    runmed, k=5)

# Firstly create a df with only TC result rows
logs.tc.only <- sqldf("
    SELECT * 
    FROM logs l
    WHERE l.MEAS_TC IS NOT NULL
    ORDER BY DEPTH
    ")
# Append to tc.only data frame
tc.only <- merge(logs.tc.only, tc.only, all=TRUE)

# Select depths for only those rows with TC data
depths <- sqldf("
    SELECT l.DEPTH 
    FROM logs l
    WHERE l.MEAS_TC IS NOT NULL
    ORDER BY DEPTH
    ")

# Secondly create a df with some neighbouring rows as well
# Filter neighbouring log depths of TC values
neighbours <- sqldf("
    SELECT DISTINCT l.*
    FROM despike l, depths d
    WHERE l.DEPTH >= d.DEPTH - 5
    AND l.DEPTH <= d.DEPTH + 5
    ORDER BY DEPTH
    ")

# Write neighbours to file (CSV)
filename <- "B15005.csv"
write.csv(neighbours, file=paste("subsets", filename, sep="/"), row.names=FALSE)

# Create a new label variable containing the bore name
bore <- rep(bore.name, nrow(despike))  # create a borename attribute
new.logs <- cbind(bore, despike)

new.logs$borename <- rep(bore.name, nrow(despike))

# Add tc subset to compilation df
tc.only <- merge(tc.only, new.logs, all=T)

# Replace missing values in a vector:
var[is.na(var)] <- mean(var, na.rm = TRUE)

# Replace missing values in a data frame's vector:
df$var[is.na(df$var)] <- mean(df$var, na.rm = TRUE)

