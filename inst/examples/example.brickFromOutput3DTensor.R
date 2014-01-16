# file example.KML.GeotopRasterBrick.R 
#
# This file contains a script which plots the measurement sites on a GoogleMap support and uses the function 'brickFromOutputSoil3DTensor'
# WARNING: the simulation template contanis only few maps utilized in this script in order to save memory disk
# The data and te results contained in this script are for educational use only and may not be realistic.
#
#
# author: Emanuele Cordano on 03-07-2013
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
library(stringr)

library(geotopbricks)

wpath <- "/Volumes/My\ Passport/Trentino_500_dstr_GEOtop_2_008/Trentino_500_dstr_GEOtop_2_008"

x <- "SoilLiqContentTensorFile"
##x <- "SoilLiqWaterPressTensorFile"
when <- as.POSIXct("2005-08-09",tz="A")
b <- brickFromOutputSoil3DTensor(x,when=when,wpath=wpath,tz="A",use.read.raster.from.url=FALSE)

file <- "/Users/ecor/Dropbox/hydropica/sourcesR/geographyTrentinoVis/data/soilwatercontent"
writeRaster(b,filename=file)



