# TODO: Add comment
# 
# Author: ecor
###############################################################################

### DA ANDARE AVANTI!!! 

library(RMAWGEN)
library(geographyTrentinoVis)
source('~/R-packages/geographyTrentinoVis/R/TrentinoPlotOn.R', chdir = TRUE)
data(trentino)
####


year_min <- 1961
year_max <- 1990

period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
station_prec <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
station_temp <- names(TEMPERATURE_MAX)[!(names(TEMPERATURE_MAX) %in% c("day","month","year"))]
prec_mesx <- PRECIPITATION[period,station_prec]
temp_mesx <- TEMPERATURE_MAX[period,station_temp]

names <- intersect(station_prec,station_temp)
isTemperature <- array(FALSE,length(names))
isPrecipitation <- array(FALSE,length(names))

names(isTemperature) <- names
names(isPrecipitation) <- names		
## removing nonworking stations (e.g. time series with NA)
accepted <- array(TRUE,length(names))
names(accepted) <- names
for (it in names) {
	cond_prec  <- (length(which(!is.na(prec_mesx[,it])))==length(prec_mesx[,it]))
	cond_temp  <- (length(which(!is.na(temp_mesx[,it])))==length(temp_mesx[,it]))
	isTemperature[it][cond_temp] <- TRUE
	isPrecipitation[it][cond_prec] <- TRUE
	
	accepted[it]  <- cond_prec | cond_temp
	
	####			(length(which(!is.na(temp_mesx[,it])))==length(temp_mesx[,it])) & cond 
}



station <- names[accepted]
isTemperature <- isTemperature[accepted]
isPrecipitation <- isPrecipitation[accepted]


CA <- STATION_NAMES %in% station

ISTEMP <- isTemperature[STATION_NAMES[CA]]
ISPREC <- isPrecipitation[STATION_NAMES[CA]]

### set lat lon data frame 

station_latlon_df <- as.data.frame(STATION_LATLON[,c(1:2)])
names(station_latlon_df) <- c("lon","lat")
station_latlon_df$id <- as.vector(STATION_NAMES)
station_latlon_df$obs <- "N"

condT <- station_latlon_df$id %in% station[isTemperature]
condP <- station_latlon_df$id %in% station[isPrecipitation]
condA <- condT & condP
condT <- condT & !condA
condP <- condP & !condA 


station_latlon_df$obs[condP] <- "P"
station_latlon_df$obs[condT] <- "T"
station_latlon_df$obs[condA] <- "A"


## PUT ON THE TRENTINO MAP 



map <- get_map(location = "trentino", maptype="satellite",zoom = 9)

type <- unique(station_latlon_df$obs)
fill <- type
fill[type=="N"] <- "black"
fill[type=="T"] <- "red"
fill[type=="P"] <- "blue"
fill[type=="A"] <- "green"

scale <- list(scale_colour_manual(breaks=type,values=fill),scale_fill_manual(labels=type,values=fill))  ##ll_discrete(values=fill) ##+scale_colour_manual(breaks=type,values=fill)
p <- plotOn(x=station_latlon_df,map=map,title="Weather Stations",scale.fill.gradient=FALSE,layer="obs",alpha=1,label="type",scale=scale)

