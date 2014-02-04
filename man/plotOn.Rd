\name{plotOn}
\alias{plotOn}
\title{Geographical representation of a data frame or raster map.}
\usage{
plotOn(x, map = get_map(location = "trentino", zoom = 9),
  latlon_crs = as.character("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
  layer = 1, legend = "bottomright", title = "Soil Water Content",
  label = "swc", high = "blue", low = "white", alpha = 0.3,
  facet_wrap = FALSE, nrow = NULL, ncol = NULL, range = NULL, ...)
}
\arguments{
  \item{x}{a \code{Raster*} class object}

  \item{map}{geograghical map on which \code{x} is plotted.
  It is an object returned by \code{\link{get_map}} or
  similars.}

  \item{latlon_crs}{string containing the utilized latitude
  longitude Coordinate Refarance System. See default in
  \code{Usage}.}

  \item{layer}{brick layer utilized for geographical
  plotting. Default is 1.}

  \item{title}{string title of the graphic}

  \item{label}{string title (label) of the legend.}

  \item{high}{colour for low end of gradient. See
  \code{\link{scale_fill_gradient}}.}

  \item{low}{colourf or high end of gradient. See
  \code{\link{scale_fill_gradient}}.}

  \item{alpha}{alpha coefficient. See
  \url{http://en.wikipedia.org/wiki/Alpha_compositing}.}

  \item{facet_wrap}{logical value. If \code{TRUE} it uses
  \code{\link{facet_wrap}} to print all plots.}

  \item{nrow,ncol}{number of rows and columns. See
  \code{\link{facet_wrap}}.}

  \item{...}{further arguments}
}
\description{
Geographical representation of a data frame or raster map.
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
\seealso{
\code{\link{geom_point}},\code{\link{ggmap}},\code{\link{facet_wrap}}
}

