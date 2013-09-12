# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Munging of well-log data set to hide borehole details for public consumption
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Get a list of all the files in specified directory path:
filenames <- list.files(path="merged_log_lab_data", full.names=TRUE)

# Load each file's data into a list of data frames:
logs <- lapply(filenames, read.csv, na.strings="NA", stringsAsFactors=FALSE)

# Add a 'names' attribute to the list of data frames:
borenames <- sub("_merged\\.csv", "", lapply(filenames, basename))
names(logs) <- borenames

ExcludeMissing <- function(df, ignore.names){
    skip <- which(colnames(df) %in% ignore.names)
    return(df[complete.cases(df[, -skip]), ])
}


RandomiseFactors <- function(df, label) {
    # Randomises factors in a data frame
    if(is.factor(df)) {
        levels(df) <- paste(label, sprintf("%03d", sample(1:length(levels(df)), length(levels(df)))), sep="_")
    }
    df
}

# Create a complete cases resultset, filtering NAs on certain columns only:
kSkipColnames <- c("BORENAME", "Borehole", "DEPTH", "MEAS_TC", "MEAS_DEN", "MEAS_PORO", "STRAT")
logs <- lapply(logs, ExcludeMissing, kSkipColnames)

# Add a borename column:
logs <- mapply(cbind, logs, "Borehole"=borenames, SIMPLIFY=FALSE)

# Merge complete cases data set to one, and write to file:
logs <- Reduce(function(x, y) merge(x, y, all=TRUE), logs)

# Round numeric variables to about 4 decimals places, to save on file size:
logs[, sapply(logs, is.numeric)] <-round(logs[, sapply(logs, is.numeric)], 4)

# Anonymise the borehole names for public viewing:
logs.levels <- list()
logs.levels$boreholes <- levels(logs$Borehole)
logs <- data.frame(lapply(logs, RandomiseFactors, "Site"))
logs.levels$anon <- levels(logs$Borehole)
write.csv(logs.levels, "Boreholelevels.csv")

# Munge the Strat variable
logs$STRAT <- as.factor(logs$STRAT)
logs.levels <- list()
logs.levels$Strat <- levels(logs$STRAT)
logs <- data.frame(lapply(logs, RandomiseFactors, "Unit"))
logs.levels$anon <- levels(logs$STRAT)
write.csv(logs.levels, "Stratlevels.csv")

# Save a merged data set to file now, before scaling/normalisation:
write.csv(logs, file="anonymous.csv", na="NA", eol="\n", row.names=FALSE, quote=FALSE)

# Reduce the data to a common set:
library(sqldf)
query <- 'select Strat, Borehole, GR, SP, SN, LN, NEUT, MEAS_TC from logs where GR > 0 AND SP > 0 AND SN > 0 AND LN > 0 AND NEUT > 0'
logs <- sqldf(query)
summary(logs)


write.csv(logs, file="anon-common.csv", na="NA", eol="\n", row.names=FALSE, quote=FALSE)

# Subset and scale
dfs <- logs
dfs[, c("GR", "SP", "SN", "LN", "NEUT")] <- scale(df[, c("GR", "SP", "SN", "LN", "NEUT")])
mysample <- dfs[sample(nrow(dfs), size=5000), ]
summary(mysample)
