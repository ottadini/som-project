# ===== #
library(kohonen)
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

# Supervised kohonen map, where the dependent variable is MEAS_TC.
# Attempting to follow the examples in Wehrens and Buydens, 2007, 21(5), J Stat Soft.
# somdata[1] is the MEAS_TC variable
somX <- training
somY <- training[, 1]
somdata.xyf <- xyf(data=training, Y=somY, 
                   xweight=0.2, grid=somgrid(5, 5, "hexagonal"), contin=TRUE)

# Prediction with test set:
somdata.xyf.prediction <- predict(somdata.xyf, newdata=testing)

# Follow example from Wehrens & Buydens 2007 page 9:
# Should the following be $unit.prediction, $unit.predictions, or $prediction ??
tc.xyf <- predict(somdata.xyf, newdata=testing)$prediction  # Y codebook vectors
tc.predict <- as.numeric(tc.xyf)
plot(somdata.xyf, type="property", property=tc.predict, main="Prediction of TC")


# Basic plot:
par(mfrow=c(1,1))
plot.new()
x <- seq(nrow(testing))
plot(x, testing[, "MEAS_TC"], type="l", col="black", ylim=c(-2, 2))
par(new=TRUE)
plot(x, somdata.xyf.prediction$prediction, type="l", col="red", ylim=c(-2, 2))

# Plot of codebook vectors
par(mfrow=c(1, 2))
plot(somdata.xyf, type="codes")

# Pre-scaled (original) meas_tc data:
meas.tc.testing <- somdata[-inTrain, "MEAS_TC"]

# Try to unscale the predicted tc:
descale <- attr(testing, 'scaled:scale')[["MEAS_TC"]]
decentre <- attr(testing, 'scaled:center')[["MEAS_TC"]]
predicted.tc <- lapply(somdata.xyf.prediction$prediction, function(x) x * descale + decentre)

# Basic plot:
plot.new()
plot(x, meas.tc.testing, type="l", col="black", ylim=c(0, 3))
par(new=TRUE)
plot(x, predicted.tc, type="l", col="red", ylim=c(0, 3))

var(meas.tc.testing)
var(as.numeric(predicted.tc))
cor(meas.tc.testing, as.numeric(predicted.tc), method="kendall")
cor(meas.tc.testing, as.numeric(predicted.tc), method="spearman")
cov(meas.tc.testing, as.numeric(predicted.tc))

RMS <- function(num) sqrt(sum(num^2)/length(num))

RMS(meas.tc.testing)
RMS(as.numeric(predicted.tc))

# ===== #