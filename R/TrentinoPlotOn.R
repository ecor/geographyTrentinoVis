# TODO: Add comment
# 
# Author: ecor
###############################################################################


NULL
#'
#' Geographical representation of a data frame or raster map.
#' 
#' @param x a \code{Raster*} class object
#' @param map geograghical map on which \code{x} is plotted. It is an object returned by \code{\link{get_map}} or similars.
#' @param latlon_crs string containing the utilized latitude longitude Coordinate Refarance System. See default in \code{Usage}.
#' @param layer brick layer utilized for geographical plotting. Default is 1. 
#' @param title string title of the graphic 
#' @param label string title (label) of the legend. It is used as the name of the scale if \code{scale.fill.gradient} is \code{TRUE}, otherwise it is ignored.
#' @param high colour for low end of gradient. See \code{\link{scale_fill_gradient}}.
#' @param low  colourf or high end of gradient. See \code{\link{scale_fill_gradient}}. 
#' @param alpha alpha coefficient. See \url{http://en.wikipedia.org/wiki/Alpha_compositing}. 
#' @param facet_wrap logical value. If \code{TRUE} it uses \code{\link{facet_wrap}} to print all plots. 
#' @param nrow,ncol number of rows and columns. See \code{\link{facet_wrap}}. 
#' @param scale.fill.gradient logical value. If it is \code{TRUE} (Default), it uses \code{\link{scale.fill.gradient}}
#' @param scale alternative parameter to \code{scale.fill.gradient}, it is a term potentially added for color scale (see \code{\link{scale_colour_hue}} o similars). It is used only if it is not \code{NULL} or \code{scale.fill.gradient} is \code{FALSE}
#' @param plot logical value. If \code{TRUE} (Default) the function also plots the map, otherwise returns its value without any preliminary plot.
#' @param ... further arguments
#' 
#' @return A \class{"ggplot"} object.
#' @export 
#' @seealso \code{\link{geom_point}},\code{\link{ggmap}},\code{\link{facet_wrap}}
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
#' x <- plotOn(x=soilwatercontent_mask,alpha=0.1)
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
		alpha=0.3,
		facet_wrap=FALSE,
		nrow=NULL,
		ncol=NULL,
		range=NULL,
		scale.fill.gradient=TRUE,scale=NULL,plot=TRUE,...) 
{
	
	out <- NULL 
	
	if (length(layer)>1) facet_wrap <- TRUE
	
	condRaster <- (class(x) %in% c("RasterBrick","RasterStack","RasterLayer"))
	conddf <- (is.data.frame(x))
	if (conddf) {
		
		conddflatlon <- (c("lat") %in% names(x)) & (c("lon") %in% names(x))
		
	}
	
	
	if (condRaster) {
		y <- projectRaster(x,crs=latlon_crs)
	
		df <- as.data.frame(y,xy=TRUE)
	
		names(df)[names(df)=="x"] <- "lon"
		names(df)[names(df)=="y"] <- "lat"
	
		names_xy <- names(df)[names(df) %in% c("lat","lon")]
	} else if (conddflatlon){ 
	
		df <- x 
		names_xy <- names(df)[names(df) %in% c("lat","lon")]
	} else { 
	
		stop("x has incorrect type!")
	}
	

	isNA <- is.na(as.data.frame(df[,!(names(df) %in% names_xy)])[,1])

	df <- df[!isNA,]	
	
	
	names <- names(df)
	names(names) <- names 
	names <- names[!(names %in% names_xy)]
	layer <- names[layer]
#	str(df)
#	print("xx")
	####
	

	df <- df[,c(names_xy,layer)]
	if (facet_wrap) {
		
		
		df <- melt(df,id.vars=names_xy)
		df <- df[,c(1,2,4,3)]
		names(df)[3] <- "label" ## DA LAVORARCI

		
	} else {
		
		
		names(df) <- c(names_xy,"label")
		
		
		
	}
	
	
	####
#	aes <- aes_string(x="lon",y="lat",colour=names(df)[3],fill=names(df)[3],...) ## ... further arguments for aes_string
	aes <- aes(x=lon,y=lat,colour=label,fill=label,...)
	###aes <- aes(x=df$lon,y=df$lat,colour=df[,3],fill=df[,3],...)
	p <- ggmap(map,legend=legend)
	if (!is.null(range)) range <- range(df[,label])
	p <- ggmap(map,extent="normal")+geom_point(data=df,mapping=aes,alpha=alpha,shape=15)
	if (scale.fill.gradient | is.null(scale)) {
		
		p<- p+scale_fill_gradient(name=label,low=low,high=high,limits=range)+scale_color_gradient(name=label,low=low,high=high,limits=range) ## scale_name=label
	} else if (!is.null(scale)) {
		
		for (it in scale) p <- p+it
		
		
	}
	if ((!is.null(title)) | (!is.na(title))) p <- p+ggtitle(title)
	if (facet_wrap) p <- p+facet_wrap(~ variable,nrow=nrow,ncol=ncol)
	##		scale_alpha(range=range(alpha))
	
	## check alpha before XXX
	
##	p <- p+geom_text(mapping=aes(x=lon,y=lat,label=id,color=obs),data=x,size = 3, vjust = 0, hjust = -0.5)
##	if (print.p)
	if (plot) print(p)
	
	
	return(p)
	
	
}


