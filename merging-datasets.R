###
# Combine data for a borehole, having different depth ranges.
# Use the highest resolution data (well-logs if they exist) as final resolution

# 18 well log files
logfiles <- read.table(text = "
bairnsdale_15005_wilt.csv
bengworden_south_6.csv
denison_53_wilt.csv
denison_57_wilt.csv
dutson_downs_1_wilt.csv
goon_nure_9_wilt.csv
holey_plains_185_wilt.csv
loy_yang_1675_wilt.csv
loy_yang_2390_wilt.csv
meerlieu_15001_wilt.csv
north_seaspray_1_wilt.csv
rosedale_301_wilt.csv
sale_13_wilt.csv
traralgon_286_wilt.csv
wellington_park_1_wilt.csv
woodside_south_1_wilt.csv
woranga_12_wilt.csv
woranga_17_wilt.csv
")

# the matching 18 tc files
tcfiles <- read.table(text = "
B15005_tc.csv
BWSTH6_tc.csv
DEN53_tc.csv
DEN57_tc.csv
DD1_tc.csv
GN9_tc.csv
HP185_tc.csv
LY1675_tc.csv
LY2390_tc.csv
M15001_tc.csv
NTHSEASPRY1_tc.csv
R301_tc.csv
S13_tc.csv
T286_tc.csv
WELLPK1_tc.csv
WOODSTH1_tc.csv
WOR12_tc.csv
WOR17_tc.csv
")

# the matching 18 strat files
stratfiles <- read.table(text = "
B15005_strat_table.csv
BWSTH6_geology.csv
DEN53_strat_table.csv
DEN57_strat_table.csv
DD1_strat_table.csv
GN9_strat_table.csv
HP185_strat_table.csv
LY1675_strat_table.csv
LY2390_strat_table.csv
M15001_strat_table.csv
NTHSEASPRY1_strat_table.csv
R301_strat_table.csv
S13_strat_table.csv
T286_strat_table.csv
WELLPK1_strat_table.csv
WOODSTH1_strat_table.csv
WOR12_strat_table.csv
WOR17_strat_table.csv
")

# function call to create new file names for the output (merged) files
mergenames.f <- function(x) gsub("tc", "merged", x)
mergedfiles <- apply(tcfiles, 1, mergenames.f)

files <- cbind(logfiles, tcfiles, stratfiles, mergedfiles)

# Set paths to data files
root_dir <- "/home/harb/Documents/DATA/Conductivity/SOM_project/"
setwd(root_dir)
logfilepath <- "CSV_files_Gippsland"
tcfilepath <- "GoCad_files"
stratfilepath <- "GoCad_files"
mergedfilepath <- "merged_log_lab_data"

# iterate over the files above. Apparently for loops are poor form in R
for (i in 1:nrow(files)) {

    bore <- files[i, ]  # row of 4 file names
    
    # Load well log data
    logs <- read.csv(paste(logfilepath, bore[1,1], sep="/"), na.strings="?")
    colnames(logs) <- toupper(colnames(logs))
    
    # Load laboratory data for borehole if it exists
    conductivity <- read.csv(paste(tcfilepath, bore[1,2], sep="/"))
    colnames(conductivity) <- toupper(colnames(conductivity))
    analyses <- conductivity[, c("FROM", "TO", "TC", "DEN", "PORO")]
    
    # Load stratigraphic data
    strat <- read.csv(paste(stratfilepath, bore[1,3], sep="/"))
    colnames(strat) <- toupper(colnames(strat))
    units <- strat[, c("FROM", "TO", "STRAT")]
    
    # Merge the data sets using sql query (from SO question)
    require(sqldf)
    output <- sqldf("
      select logs.*, a.TC MEAS_TC, a.DEN MEAS_DEN, a.PORO MEAS_PORO, u.STRAT 
      FROM logs 
      LEFT JOIN analyses a ON a.[FROM] <= logs.DEPTH AND logs.DEPTH <= a.[TO]
      LEFT JOIN units    u ON u.[FROM] <= logs.DEPTH AND logs.DEPTH <= u.[TO]
      ORDER BY DEPTH
      ")
    
    ## Write dataframe to csv
    write.csv(output, file=paste(mergedfilepath, bore[1,4], sep="/"), eol="\r\n", row.names=FALSE)
}
