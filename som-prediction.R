# ===== An attempt at prediction of TC using the kohonen package.
#
# 
library(sqldf)
library(kohonen)

ScaleLogs <- function(df, skipColNames){
    # Scale and centre certain log response values
    skip.indices <- which(colnames(df) %in% skipColNames)
    df[, -skip.indices] <- 
        sapply(
            df[, -skip.indices], 
            scale, center=TRUE)  # I think I need center=TRUE
    return(df)
}


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Data processing

# Set root data directory 
root_dir <- "~/Documents/DATA/Conductivity/SOM_project"
setwd(root_dir)

# Column labels to ignore. Can put labels in here that may not match anything:
kSkipColnames <- c("DEPTH", "Borehole", "STRAT", "TEMP", "RES", "DRHO", "RHOB", "LLD", "LLS", "LDEN", "SDEN", "LDE", "SDE", "CAL2", "GR2", "IND", "LAT2", "LN2", "NEU2", "SN2", "SP2", "TEM1", "TEM2", "LL3")

# Load TC data from CSV file, and subset:
fname <- "subset_all_data.csv"
tc <- read.csv.sql(fname, sql="select * from file where MEAS_TC > 0") #file.format=list(colClasses=classes))

# headset <- read.csv(fname, header=TRUE, nrows=10, na.strings="-999.25", comment.char="")
# classes <- lapply(headset, class)
# classes <- replace(classes, classes=="logical", "numeric")
# classes <- replace(classes, classes=="integer", "numeric")

# tc <- read.csv("subset_all_data.csv", na.strings="-999.25", comment.char="")
# tc <- sqldf("SELECT * 
#             FROM tc l
#             WHERE l.MEAS_TC > 0
#             ORDER BY DEPTH
#             ")


# Handle NAs (written as -999.25 in text file)
tc[tc==-999.25] <- NA

# Remove mostly empty columns/variables:
skips <- c("Borehole", "STRAT", "TEMP", "DEN", "DT", "CALI", "LAT", "RES", "DRHO", "RHOB", "LLD", "LLS", "LDEN", "SDEN", "LDE", "SDE", "CAL2", "GR2", "IND", "LAT2", "LN2", "NEU2", "SN2", "SP2", "TEM1", "TEM2", "LL3")
skipVec <- which(colnames(tc) %in% skips)  # returns a vector of column indices
tc <- tc[, -skipVec]

# Remove some rows with no NEUT value (following visual inspection)
tc <- tc[1:103, ]

# Replace missing values with medians:
tc$LN[is.na(tc$LN)] <- median(tc$LN, na.rm = TRUE)
tc$SN[is.na(tc$SN)] <- median(tc$SN, na.rm = TRUE)
tc$NEUT[is.na(tc$NEUT)] <- median(tc$NEUT, na.rm = TRUE)

# Write it out to a CSV file for future
write.csv(tc, file="som_set_02.csv", row.names=FALSE)

# Copy data
somdata <- tc[c("MEAS_TC", "SP", "LN", "SN", "GR", "NEUT")]
# Write this one out too
write.csv(somdata, file="somdata.csv", row.names=FALSE)

somdata.scaled <- scale(somdata)

# Create test and training sets from data:
inTrain <- sample(nrow(somdata), nrow(somdata)*(2/3))
training <- scale(somdata[inTrain, ])  # two-thirds of data
testing <- scale(somdata[-inTrain, ], 
                 center = attr(training, "scaled:center"),
                 scale = attr(training, "scaled:scale"))  # remaining one-third

# Unsupervised kohonen map ()
# som.tc <- som(data=scale(somdata), grid=somgrid(8, 5, "hexagonal"), toroidal=TRUE, rlen=1000)
# plot(som.tc)

# Supervised kohonen map, where the dependent variable is MEAS_TC.
# Trying to follow the examples in Wehrens and Buydens, 2007, 21(5), J Stat Soft.
# somdata[1] is the MEAS_TC variable
somX <- training[, -1]
somY <- training[, 1]
tc.xyf <- xyf(data=somX, Y=somY, xweight=0.5, grid=somgrid(6, 6, "hexagonal"), contin=TRUE)
# par(mfrow=c(1, 2))
# plot(tc.xyf, type="counts")
# plot(tc.xyf, type="quality")
# plot(tc.xyf)

# Prediction with test set:
tc.xyf.prediction <- predict(tc.xyf, newdata=testing[, -1])

# Have a gander
# Revert scaling of variables
meas.tc <- testing[, "MEAS_TC"] * attr(testing, 'scaled:scale') + attr(testing, 'scaled:center')
predicted.tc <- tc.xyf.prediction$prediction * attr(testing, 'scaled:scale') + attr(testing, 'scaled:center')

x <- seq(nrow(testing))
plot(x, meas.tc, type="l", col="black") #, ylim=c(-2, 2))
par(new=TRUE)
plot(x, predicted.tc, type="l", col="red") #, ylim=c(-2, 2))

x <- seq(nrow(testing))
plot(x, testing[, "MEAS_TC"], type="l", col="black", ylim=c(-2, 2))
par(new=TRUE)
plot(x, tc.xyf.prediction$prediction, type="l", col="red", ylim=c(-2, 2))

