# ===== #
suppressPackageStartupMessages(library(kohonen))
suppressPackageStartupMessages(library(ggplot2))
# Set root data directory 
root_dir <- "~/Documents/DATA/Conductivity/SOM_project"
setwd(root_dir)
# Load data, available at https://gist.github.com/ottadini/6068259
somdata <- read.csv("somdata.csv")


# Create SCALED test and training sets from data:
inTrain <- sample(nrow(somdata), nrow(somdata)*(2/3))
training <- scale(somdata[inTrain, ])
testing <- scale(somdata[-inTrain, ],
                 center = attr(training, "scaled:center"),
                 scale = attr(training, "scaled:scale"))


# Attempting to follow the examples in Wehrens and Buydens, 2007, 21(5), J Stat Soft.

# Supervised kohonen map, where the dependent variable is MEAS_TC (somdata[1]).
somX <- training
somY <- training[, 1]
somdata.xyf <- xyf(data=somX, Y=somY, contin=TRUE, rlen=500, 
                   xweight=0.2, grid=somgrid(5, 5, "hexagonal"))


# Figure 1. Training progress:
plot(somdata.xyf, type="changes")  # Seems to be converging.

# Figure 2. Codebook vectors
op <- par(mfrow=c(1, 2))
plot(somdata.xyf, type="codes")
par(op)

# Figure 3. All component planes:
cols <- ncol(somdata.xyf$codes$X)
op <- par(mfrow=c(ceiling(cols/2), 2))
for (i in 1:cols) {
    plot(somdata.xyf,
         type="property",
         property=somdata.xyf$codes$X[, i],
         main=colnames(somdata.xyf$codes$X)[i])
}
par(op)


# Figure 4. Property plot (prediction)
# Follow example from Wehrens & Buydens 2007 page 9:
tc.xyf <- predict(somdata.xyf, newdata=testing)$prediction  # Y codebook vectors
tc.predict <- as.numeric(tc.xyf)  # It's numeric anyway, but following blindly...
plot(somdata.xyf, 
     type="property", 
     property=tc.predict, 
     main="Prediction of TC")  # Not sure what to make of this.


# Figure 5. Crudely compare measurements to predicted values:
# Un-scale() the predicted tc:
SD <- attr(testing, 'scaled:scale')[["MEAS_TC"]]
Mean <- attr(testing, 'scaled:center')[["MEAS_TC"]]
tc.predict.descale <- sapply(tc.predict, function(x) x * SD + Mean)
# Pre-scaled (original) meas_tc data:
meas.tc.testing <- somdata[-inTrain, "MEAS_TC"]

# Basic plot of measured vs predictions:
x <- seq(nrow(testing))
op <- par(mfrow=c(1, 1))
plot(x, meas.tc.testing, type="l", col="black", ylim=c(0, 3))
par(new=TRUE)
plot(x, tc.predict.descale, type="l", col="red", ylim=c(0, 3))
title(paste(strwrap("Comparison of testing set to predicted value of thermal conductivity"), collapse="\n"))
par(op)

# Some statistical measures, but what will tell me how close the predictions
# come to the observations??
# var(meas.tc.testing)
# var(tc.predict.descale)
# cor(meas.tc.testing, tc.predict.descale, method="kendall")
# cor(meas.tc.testing, tc.predict.descale, method="spearman")
# cov(meas.tc.testing, tc.predict.descale)
# 
# RMS <- function(num) sqrt(sum(num^2)/length(num))
# 
# RMS(meas.tc.testing)
# RMS(tc.predict.descale)

# ===== #