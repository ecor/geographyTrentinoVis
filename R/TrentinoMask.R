NULL
#'
#' Raster Mask of the Trentino Region 
#' 
#' @param dsn see \code{\link{readOGR}}
#' @param layer see \code{\link{readOGR}}
#' @param url adress used for the download of data 
#' @param zip logical value. Default is \code{FALSE}. If \code{TRUE} the dowloaded file is unzipped after downloadig. See Details.
#' @param clean logical value. Default is \code{FALSE}. (experimental). 
#' @param option list of object, e.g. fields and related values corresponded to the selected polygons. 
#' @param raster \code{Raster*} or \code{NULL} object used for rasterization. Default is \code{NULL}, see \code{\link{rasterize}}. 
#' @param rasterRes Raster Resolution in x and y (meters or degrees). It is used to create a \code{Raster*} for rasterization in case the argument \code{raster} is \code{NULL}.
#' @param field see \code{rasterize}
#' @param filename filename where ... 
#' @param valuesFromRaster logical value. If it  is \code{TRUE}(Default), it fills the values within the mask with the ones of the raster map given by \code{raster}.
#' @param ... futher arguments for \code{\link{readOGR}}
#' 
#' 
#' @details In case of downloaded from an URL, the file is automatically unzipped if a zip archive wth \code{.zip} extenstion.
#' @export 
#' @examples 
#' library(stringr)
#' library(rgdal)
#' library(raster)
#' 
#' mask <- CreateMask() ## see default in Usage
#' 
#' 
#' 
#' 

#  http://www.provincia.tn.it/progetto_open_data/


# dataset: http://dati.trentino.it/dataset/limite-di-comune-amministrativo-valido-fino-al-2009-12-31
#  http://dati.trentino.it/storage/f/2013-05-09T141315/788_Comuni_amministrativi___DB_Prior_10k__12_12_2011_4uWD8r.zip
CreateMask <- function(dsn=NULL,layer,
				url="http://dati.trentino.it/storage/f/2013-05-09T141315/788_Comuni_amministrativi___DB_Prior_10k__12_12_2011_4uWD8r.zip",
				zip=FALSE,
				clean=FALSE,
				option=list(NOME_PROV="TRENTO"),
				rasterRes=500,
				raster=NULL,
				field="COM",
				filename="maskXYRES.grd",
				valuesFromRaster=TRUE,
				...)
{
			
			out <- NULL
			temporary_folder <- "./Temporary_GeograhyTrentinoVis"
			if (file.exists(temporary_folder) & clean) system(paste("rm -R ",temporary_folder,sep=" "))
			if (!file.exists(temporary_folder)) dir.create(temporary_folder)
			if (is.null(dsn)) {
				destfile <- str_split(url,pattern="/")[[1]]
				destfile <- paste(temporary_folder,destfile[length(destfile)],sep="/")
			    
				if (!file.exists(destfile)) download.file(url,destfile)
				
				if (str_detect(destfile,".zip") | zip) {
					
					exdir <- paste(destfile,"_dir",sep="")
					unzip(destfile,exdir=exdir)
					
					destfile <- list.files(exdir,full.names=FALSE)
				
					destfile <- str_split(destfile,pattern="[.]")[[1]]
					destfile <- destfile[-length(destfile)]
					if (length(destfile)>1) destfile <- paste(destfile,collapse=".")
					
					dsn <- exdir
					layer <- destfile

					
				}
				
		
				
			}
			
			out <- readOGR(dsn=dsn,layer=layer,...)
			
			names <- names(option)[names(option) %in% names(out)]
			
			for (it in names) {
				
				cond <- as.vector((out@data[,it])) %in% option[[it]]
			
				out <- out[cond,]
				
			}
			
			if (!is.null(rasterRes) | !is.null(raster)) {
				mask <- out
				
				if (is.null(raster)) { 
				
					bbox <- bbox(mask)
					if (length(rasterRes)==1) rasterRes <- rasterRes[c(1,1)]
					names(rasterRes) <- c("x","y")
					
					xmin <- bbox["x","min"]
					xmax <- bbox["x","max"]
					ymin <- bbox["y","min"]
					ymax <- bbox["y","max"]
					
					BD <- 2
					xmin <- xmin-BD*rasterRes["x"]
					ymin <- ymin-BD*rasterRes["y"]
					
					xmax <- xmax+BD*rasterRes["x"]
					ymax <- ymax+BD*rasterRes["y"]
					
					ncelx <- ceiling((xmax-xmin)/rasterRes["x"])
					ncely <- ceiling((ymax-ymin)/rasterRes["y"])
					
					xmax <- xmin+ncelx*rasterRes["x"]
					ymax <- ymin+ncely*rasterRes["y"]
					
					crs <- proj4string(mask) 
					raster <- raster(xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax,nrow=ncely,ncol=ncelx,crs=crs)
					
					raster[,] <- 1
##					xmax <- bbox(mask)
##					raster <- 
				} else {
					
					
					
					cond <- projection(raster)==proj4string(mask) ## check identical projection
					
					if (!cond) {
						
						warning <- paste("Check Different CRSs :",as.character(projection(raster)),"and",as.character(proj4string(mask)),sep=" ")
						warning(warning)
						
					} ##  TO BE CHECK 
					
					
				}
				
				res <- paste(res(raster),collapse="X")
				
				
				filename <- paste(temporary_folder,filename,sep="/")
				
				filename <- str_replace(filename,"XYRES",res)
				
			###	if (file.exist())
				out <- rasterize(x=mask,y=raster,field=field,fun=function(x,...){return(x[1])},filename=filename,overwrite=TRUE) 
				
				if (valuesFromRaster) {
					print(raster)
					names <- names(raster) 
					out[!is.na(out)] <- 1
					
					out <- raster*out
					
					names(out) <- names
					
				}
				
				
			}
			
			
			return(out)
	
}




