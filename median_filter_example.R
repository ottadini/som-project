# ===========================================================================
# Check result of smoothing (median filter) on well log curves
# ===========================================================================

library(ggplot2)

root_dir <- "~/Documents/DATA/Conductivity/SOM_project"
setwd(root_dir)

# Well log data
wellname <- "Denison 57"
logs <- read.csv("CSV_files_Gippsland/denison_57_wilt.csv", na.strings="nan")

# have a squizz
head(logs)
summary(logs)

# the typical depth interval between log measurements
# needed to determine the 'window' for smoothing in median filter
median(diff(logs$DEPTH))

# a simple plot
p <- ggplot(data=logs) 
p + geom_line(aes(y=CALI, x=DEPTH)) + scale_x_reverse() + coord_flip() + theme(aspect.ratio = 3)

# more impressive plots require some munging
require(reshape2)
melted <- melt(logs, id.vars='DEPTH')

# swap x,y axis variables as coord_flip() breaks everything
sp <- ggplot(melted, aes(x=value, y=DEPTH)) + 
    geom_path() + scale_y_reverse() +
    labs(title=paste(wellname, 'Wiltshire log data'))
sp + facet_grid(. ~ variable, scales='free_x')

# subset as required/desired
#logs <- logs[, c('DEPTH','CALI','DEN','GR')]  #,'LN','NEUT','SN','SP')]

# create a borename attribute
#label <- rep(wellname, nrow(logs))  
#logs <- cbind(label, logs)

# must remove empty rows for filtering & smoothing of logs
logs.filtered <- na.omit(logs)

# =====
# view before and after smoothing of well log curves
# ggplot version
# Set the limits in reverse order & the axis gets reversed (cf. xlim(20, 40))
# Or, to reverse the entire axis, use  + scale_x_reverse().
plt <- ggplot(data=logs.filtered, aes(y=GR, x=DEPTH)) + 
    coord_flip() + theme_gray()  + scale_x_reverse()
plt <- plt + geom_line(colour='#333333', size=1)

# check median filter over entire log
plt + geom_line(aes(y=runmed(logs.filtered$GR, 21), x=logs.filtered$DEPTH), colour='red', alpha=0.5) + coord_flip()
plt + geom_line(aes(y=runmed(logs.filtered$GR, 31), x=logs.filtered$DEPTH), colour='red', alpha=0.5) + coord_flip()
plt + geom_line(aes(y=runmed(logs.filtered$GR, 51), x=logs.filtered$DEPTH), colour='red', alpha=0.5) + coord_flip()

# check median filter over smaller intervals
plt <- plt + xlim(700, 500)

plt + labs(title='window = 11') +
    geom_line(aes(y=runmed(logs.filtered$GR, 11), 
                  x=logs.filtered$DEPTH), 
              colour='red', alpha=0.5)

plt + labs(title='window = 21') +
    geom_line(aes(y=runmed(logs.filtered$GR, 21), 
                  x=logs.filtered$DEPTH), 
              colour='red', alpha=0.5)

plt + labs(title='window = 31') +
    geom_line(aes(y=runmed(logs.filtered$GR, 31), 
                  x=logs.filtered$DEPTH), 
              colour='red', alpha=0.5)

plt + labs(title='window = 51') +
    geom_line(aes(y=runmed(logs.filtered$GR, 51), 
                  x=logs.filtered$DEPTH), 
              colour='red', alpha=0.5)


# Replot over TC data intervals
# Choose intervals from loaded conductivity data files
require(gridExtra)  # for arranging multiple plots on one canvas

# some theming issues to get rid of axis text and ticks
# add this after coord_flip() to target axes correctly
blank_axis <- theme(axis.text.y=element_blank(), 
                    axis.ticks.y=element_blank(),
                    legend.position="none")

# Load TC etc lab data
analyses <- read.csv("GoCad_files/DEN57_tc.csv", na.strings="?")
colnames(analyses) <- toupper(colnames(analyses))

from <- analyses$FROM[3] -50
to <- analyses$TO[3] + 50

plt <- plt + xlim(to, from) + blank_axis + xlab("")

plt2 <- ggplot(data=analyses) + labs(x="", y="Conductivity") +
        geom_point(aes(x=FROM, y=TC, colour='red', alpha=0.5, size=10)) +
        coord_flip() + theme_gray()  + scale_x_reverse() +
        xlim(to, from) + blank_axis


# how do I add in the strat labels as formation tops?
# Load strat data
strat <- read.csv(paste(root_dir, "GoCad_files/DEN53_strat_table.csv", sep="/"))
colnames(strat) <- toupper(colnames(strat))
units <- strat[, c("FROM", "TO", "STRAT")]

plt3 <- ggplot(data=strat) + labs(x="DEPTH", y="") + 
    geom_vline(aes(xintercept=FROM)) +
    geom_text(aes(x=FROM, y=100, label=STRAT), vjust=1.5) +
    coord_flip() + theme_gray()  + scale_x_reverse() +
    xlim(to, from)
    
# plot.margin units rotate clockwise from top: top, right, bottom, left.
plt4 <- grid.arrange(plt3 + theme(plot.margin=unit(c(1,0,1,1), "lines")), 
                     plt2 + theme(plot.margin=unit(c(1,0,1,0), "lines")), 
                     plt + theme(plot.margin=unit(c(1,1,1,0), "lines")), 
                     ncol=3)