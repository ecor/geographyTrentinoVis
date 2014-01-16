\name{plotOn}
\alias{plotOn}
\title{Geograhical representation of a raster map.}
\usage{
plotOn(x, map = get_map(location = "trentino", zoom = 9),
  latlon_crs = as.character("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
  layer = 1, legend = "bottomright", title = "Soil Water Content",
  label = "swc", high = "blue", low = "white", alpha = c(0.1, 0.7),
  range = NULL, ...)
}
\arguments{
  \item{x}{a \code{Raster*} class object}

  \item{map}{geograghical map on which \code{x} is plotted.
  It is an object returned by \code{\link{get_map}} or
  similars.}

  \item{...}{further arguments}
}
\description{
Geograhical representation of a raster map.
}
\note{
Useful link:
\url{http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf}
}
\examples{
library(geographyTrentinoVis)
data(soilwatercontent)
soilwatercontent_mask <- CreateMask(raster=soilwatercontent) ## see default in Usage


x <- plotOn(x=soilwatercontent_mask)
}

