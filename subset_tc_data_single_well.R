# ===== DATA PREPARATION STEP 2 =====
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

