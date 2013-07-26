# =========================================================================== #
# this is how to merge all of the borehole data together into one data frame
# =========================================================================== #

# Bengworden South 6 log data
bw6 <- read.csv("~/Documents/DATA/Conductivity/SOM_project/KeyWells/bengworden_south_6.csv", na.strings="?")
bore <- rep("Bengworden South 6", nrow(bw6))  # create a borename attribute
bw6 <- cbind(bore, bw6)


# Sale-13 well log data
sale13 <- read.csv("~/Documents/DATA/Conductivity/SOM_project/KeyWells/sale_13_wilt.csv", na.strings="?")
bore <- rep("Sale 13", nrow(sale13))  # create a borename attribute
sale13 <- cbind(bore, sale13)

# Meerlieu 15001
gn9 <- read.csv("/home/harb/Documents/DATA/Conductivity/SOM_project/KeyWells/goon_nure_9_wilt.csv", na.strings="?")
bore <- rep("Goon Nure 9", nrow(gn9))  # create a borename attribute
gn9 <- cbind(bore, gn9)

df <- Reduce(function(x, y) merge(x, y, all=TRUE), list(bw6, sale13, gn9))
