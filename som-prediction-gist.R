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
somdata.xyf <- xyf(data=training, Y=somY, contin=TRUE, rlen=500, 
                   xweight=0.2, grid=somgrid(5, 5, "hexagonal"))


# Follow example from Wehrens & Buydens 2007 page 9:
# Should the following be $unit.prediction, $unit.predictions, or $prediction ??
tc.xyf <- predict(somdata.xyf, newdata=testing)$prediction  # Y codebook vectors
tc.predict <- as.numeric(tc.xyf)
plot(somdata.xyf, type="property", property=tc.predict, main="Prediction of TC")
plot(somdata.xyf, type="changes")

# Basic plot:
par(mfrow=c(1,1))
x <- seq(nrow(testing))
plot(x, testing[, "MEAS_TC"], type="l", col="black", ylim=c(-2, 2))
par(new=TRUE)
plot(x, tc.predict, type="l", col="red", ylim=c(-2, 2))

# Plot of codebook vectors
par(mfrow=c(1, 2))
plot(somdata.xyf, type="codes")

# Pre-scaled (original) meas_tc data:
meas.tc.testing <- somdata[-inTrain, "MEAS_TC"]

# Un-scale() the predicted tc:
descale <- attr(testing, 'scaled:scale')[["MEAS_TC"]]
decentre <- attr(testing, 'scaled:center')[["MEAS_TC"]]
tc.predict.descale <- sapply(tc.predict, function(x) x * descale + decentre)

# Basic plot:
par(mfrow=c(1, 1))
plot(x, meas.tc.testing, type="l", col="black", ylim=c(0, 3))
par(new=TRUE)
plot(x, tc.predict.descale, type="l", col="red", ylim=c(0, 3))

var(meas.tc.testing)
var(tc.predict.descale)
cor(meas.tc.testing, tc.predict.descale, method="kendall")
cor(meas.tc.testing, tc.predict.descale, method="spearman")
cov(meas.tc.testing, tc.predict.descale)

RMS <- function(num) sqrt(sum(num^2)/length(num))

RMS(meas.tc.testing)
RMS(tc.predict.descale)

# ===== #