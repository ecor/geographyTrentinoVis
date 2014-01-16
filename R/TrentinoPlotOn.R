# TODO: Add comment
# 
# Author: ecor
###############################################################################


NULL
#'
#' Geographical representation of a raster map.
#' 
#' @param x a \code{Raster*} class object
#' @param map geograghical map on which \code{x} is plotted. It is an object returned by \code{\link{get_map}} or similars.
#' @param ... further arguments
#' 
#' 
#' @export 
#' 
#' @importFrom raster as.data.frame 
# @import ggmap
#' @note Useful link: \url{http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf}
#' @examples
#'  
#' library(geographyTrentinoVis)
#' data(soilwatercontent)
#' soilwatercontent_mask <- CreateMask(raster=soilwatercontent) ## see default in Usage
#' 

#' 
#' x <- plotOn(x=soilwatercontent_mask)
#' 
#' 
#' 
#' 
#' 




plotOn <- function(x,
		map=get_map(location = 'trentino', zoom = 9),
		latlon_crs=as.character("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),layer=1,
		legend="bottomright",
		title="Soil Water Content",
		label="swc",
		high="blue",
		low="white",
		alpha=c(0.1,0.7),
		range=NULL,...) 
{
	
	out <- NULL 
	
	y <- projectRaster(x,crs=latlon_crs)
	
	##out <- y 
	
	
	df <- as.data.frame(y,xy=TRUE)
	

	names(df)[names(df)=="x"] <- "lon"
	names(df)[names(df)=="y"] <- "lat"
	
	names_xy <- names(df)[names(df) %in% c("lat","lon")]
	
	
	
	 isNA <- is.na(df[,!(names(df) %in% names_xy)][,1])
	
	df <- df[!isNA,]	
	
	### set layer 
	
	names <- names(df)
	names(names) <- names 
	names <- names[!(names %in% names_xy)]
	layer <- names[layer]
	
	####
	
	df <- df[,c(names_xy,layer)]
	names(df) <- c(names_xy,label)
	
	
	####
	aes <- aes_string(x="lon",y="lat",colour=names(df)[3],fill=names(df)[3],...) ## ... further arguments for aes_string
	###aes <- aes(x=df$lon,y=df$lat,colour=df[,3],fill=df[,3],...)
	p <- ggmap(map,legend=legend)
	if (!is.null(range)) range <- range(df[,label])
	p <- ggmap(map,extent="normal")+geom_point(data=df,mapping=aes,alpha=0.4)+scale_fill_gradient(low=low,high=high,limits=range)+scale_color_gradient(low=low,high=high,limits=range)
	if ((!is.null(title)) | (!is.na(title))) p <- p+ggtitle(title)
	##		scale_alpha(range=range(alpha))
	
	## check alpha before XXX
	
	
	print(p)
	
	
	return(p)
	
	
}


