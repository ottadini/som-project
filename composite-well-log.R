# ===========================================================================
# Plot simple 'composite' logs from well log data (Wiltshire logs)
# ===========================================================================
require(ggplot2)

# Well log data
rootdir <- "~/Documents/DATA/Conductivity/SOM_project"

logs <- read.csv(paste(rootdir, "CSV_files_Gippsland/bairnsdale_15005_wilt.csv", sep="/"), na.strings="?")
wellname <- 'Bairnsdale 15005'

# subset to make it manageable
#logs <- logs[, c('DEPTH','CALI','DEN','GR')]  #,'LN','NEUT','SN','SP')]

# a simple plot
#ggplot(data=logs, aes(y=GR, x=DEPTH)) + geom_line() + 
#    scale_x_reverse() + coord_flip() + theme(aspect.ratio = 3)

# more impressive plots require some munging
require(reshape2)
melted <- melt(logs, id.vars='DEPTH')

# histograms
h <- ggplot(data=melted) + geom_histogram(aes(y=..density.., x=log2(value)))
h + facet_wrap(~variable, scales='free')


# save last plot
ggsave(filename=paste(wellname, 'histograms.pdf'), 
       path=paste(rootdir, 'LAS_files_Gippsland/plots/', sep="/"))

# swap x,y axis variables as coord_flip() breaks everything
sp <- ggplot(melted, aes(x=value, y=DEPTH)) + 
    geom_path() + scale_y_reverse() +
    labs(title=paste(wellname, 'Wiltshire log data'))
sp + facet_grid(. ~ variable, scales='free_x')


# save last plot
ggsave(filename=paste(wellname, 'composite log.pdf'), 
       path=paste(rootdir, 'LAS_files_Gippsland/plots/', sep="/"), 
       width=11)
