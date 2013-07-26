# ===== Some exploration with the kohonn package.
# This script has some useful stuff, but is not using a useful data set.
# It will produce rubbish results. I've kept it for reference.

# for reference, see journal article in zotero:
# Wehrens and Buydens 2007. Self and super-organising maps in R: The kohonen package

require("kohonen")
require('stats')  # lots of functions for handling data, e.g. NAs and NaNs
require('zoo')  # also has advanced methods for handling NAs and NaNs, incl na.approx

# root data directory
root_dir <- "~/Documents/DATA/Conductivity/SOM_project"  # linux
root_dir <- "DATA/Conductivity/SOM_project"  # Windows
setwd(root_dir)

# Load log data
logs <- read.csv("merged_log_lab_data/B15005_merged.csv", na.strings="?")

# subset the data, removing TEMP, CONDUCTIVITY, POROSITY, DENSITY and their stdevs, 
# as they are too empty for analysis, and leaving in DEPTH for now??
logs.sub <- logs[, c('CALI','DEN','GR','LAT','LN','NEUT','SN','SP')]
# alternatively, one can specify the columns to drop in several ways:
# Set the column to NULL (from Section 2.7 of An Introduction to R)
logs.1 <- logs
logs.1$GR <- NULL  # OR logs.1[, "GR"] <- NULL
# Subset the column(s) out:
logs.2a <- logs[, !names(logs) %in% "GR"]
logs.2b <- logs[, !names(logs) %in% c("GR", "CONDUCTIVITY")]  # names don't have to be in data frame
logs.2c <- subset(logs, select=-GR)  # names must be in data frame or throws an error
logs.2d <- subset(logs, select=-c(GR, CONDUCTIVITY))


# check out correlations using pairs
pairs(logs.sub)
# result shows that only LN and SN seem highly correlated. No point performing PCA


# this includes the TC column... for prediction using the trained som
logs.pre <- logs[, c('DEPTH','CALI','DEN','GR','LAT','LN','NEUT','SN','SP','CONDUCTIVITY')]

# kohonen can't deal with missing / NaNs, so replace them by interpolation
# using zoo's na.approx
b <- data.frame(na.approx(logs.sub, na.rm=FALSE, rule=2))  # rule=2 means extend as well as interpolate

# look at result of interpolation
plot(logs$CALI, type='l', col=rgb(.1,.1,.1,.7))
par(new=TRUE)  # plot two things on top of each other
plot(b$CALI, type='l', col='red')

# plot the difference between the original and interpolated data
plot(logs$CALI-b$CALI, type='l', col='red')
plot(logs$SP-b$SP, type='l', col='red')


# normalise & centre each column's values (required as SOM algorithm is based on Euclidean distances)
logs.scld <- scale(b)

# load the laboratory TC data if it exists
conductivity <- read.csv("GoCad_files/B15005_tc.csv")
analyses <- conductivity[, c("from", "to", "TC", "DEN", "PORO")]

# load the stratigraphic data for the bore
strat <- read.csv("GoCad_files/B15005_strat_table.csv")
units <- strat[, c("from", "to", "strat")]

# merge the wireline, lab, and strat data sets using sql query (from SO question)
require(sqldf)
output <- sqldf("
      select d.*, a.TC MEAS_TC, a.DEN MEAS_DEN, a.PORO MEAS_PORO, u.strat 
      FROM logs d
      LEFT JOIN analyses a ON a.[from] <= d.DEPTH AND d.DEPTH < a.[to]
      LEFT JOIN units    u ON u.[from] <= d.DEPTH AND d.DEPTH < u.[to]
      ORDER BY DEPTH
      ")


# train the som
logs.som <- som(data=logs.scld, grid=somgrid(24, 20,'hexagonal'), toroidal=TRUE)

# default plot: codebook vectors in a 'segments' plot
plot(logs.som)  # equivalent to type="codes"

# plot distance to codebook vectors during training: a way to see
# how the data has adapted, and whether there were sufficient iterations
plot(logs.som, type='changes')

# plot the number of objects mapped to the individual units. 
# Empty units are depicted in gray.
par(mfrow=c(1, 2))
plot(logs.som, type='counts')
plot(logs.som, type='quality', main='Mapping quality')
# Mapping quality shows the mean distance of objects (mapped to a particular 
# unit) to the codebook vector of that unit. A good mapping should show small
# distances everywhere.

# plot  the sum of the distances to all immediate neighbours. This kind of visualization 
# is also known as a U-matrix plot. Units near a class boundary can be expected to have 
# higher average distances to their neighbours. Only available for the "som" and 
# "supersom" maps, for the moment.
plot(logs.som, type='dist.neighbours')
## use hierarchical clustering to cluster the codebook vectors
som.hc <- cutree(hclust(dist(logs.som$codes)), 10)
add.cluster.boundaries(logs.som, som.hc)

# plot shows where objects are mapped. It needs the "classif" argument, and a "labels" or "pchs" argument.
#plot(logs.som, type='mapping')

# properties of each unit can be calculated and shown in colour code. It can be used to 
# visualise the similarity of one particular object to all units in the map, to show the 
# mean similarity of all units and the objects mapped to them, etcetera. The parameter 
# property contains the numerical values.
plot(logs.som, type='property', 
     property=logs.som$codes[,8], 
     main = colnames(logs.som$codes)[8], 
     keepMargins=TRUE)

# plot the mean distance of objects mapped to a unit to the codebook vector of that unit. 
# The smaller the distances, the better the objects are represented by the codebook vectors.
plot(logs.som, type='quality')


# I can 'map' a new data matrix onto the trained som:
mapping <- map(logs.som, newdata=?)
# try some prediction
logs.prediction <- predict(logs.som, newdata=scale(logs.pre), trainY=logs.pre)
