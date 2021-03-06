# file geogr1.R
#
# This file contains a function to create a mask of the Trentino Region.
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

mask <- CreateMask() ## see default in Usage
mask[!is.na(mask)] <- 1 
