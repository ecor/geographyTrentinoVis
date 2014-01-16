# file geogr2.R
#
# This file contains a function ...
#
# author: Emanuele Cordano on 11-01-2014
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################


rm(list=ls())
library(geographyTrentinoVis)

data(soilwatercontent)

# alternatively:
#
#soilwatercontent_map <- raster(system.file("data/soilwatercontent.grd",package="geographyTrentinoVis"))
#
#
mask <- CreateMask(raster=soilwatercontent) ## see default in Usage



mask[!is.na(mask)] <- 1 

soilwatercontent_map <- mask(x=soilwatercontent,mask=mask)


trentinomap <- get_map(location = 'trentino', zoom = 9, maptype = 'hybrid')

##

soilwatercontent_df <- as.data.frame(soilwatercontent_map,xy=TRUE)

ggmap(trentinomap,geom="raster")
#
#
#setMethod('KML', signature(x='GeotopRasterBrick'), 
#		function (x, filename, crs=as.character("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),...) {  # "+init=epsg:4326")#} ,zip='', overwrite=FALSE, ...) {
#			
#			y <- projectRaster(brick(x),crs=crs)			
#			out <- KML(x=y,filename=filename,...) 
#			return(out)
#		}
#
#)

