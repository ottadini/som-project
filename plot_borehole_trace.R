require(rgl)

# get collars data from file
collars <- read.csv('/home/harb/Documents/DATA/Conductivity/SOM_project/collars.csv')

# alternative one:
segments <- collars[rep(1:nrow(collars), 1, each=2), ]
segments[c(F, T), 4] <- collars[4] - collars[5]
# end one

# alternative two:
# generate BOH coords from collars
boh <- transform(collars, elevation = elevation - total_depth)
# append BOH points to collars
segments <- rbind(collars, boh)
# reorder the rows for segments3d
n <- nrow(collars)
segments <- segments[kronecker(1:n, c(0, n), '+'), ]
# end two

open3d()

points3d(collars$easting, collars$northing, collars$elevation)
segments3d(segments$easting, segments$northing, segments$elevation)
