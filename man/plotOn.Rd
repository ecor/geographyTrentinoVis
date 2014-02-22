\name{plotOn}
\alias{plotOn}
\title{Geographical representation of a data frame or raster map.}
\usage{
plotOn(x, map = get_map(location = "trentino", zoom = 9),
  latlon_crs = as.character("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
  layer = 1, legend = "bottomright", title = "Soil Water Content",
  label = "swc", high = "blue", low = "white", alpha = 0.3,
  facet_wrap = FALSE, nrow = NULL, ncol = NULL, range = NULL,
  scale.fill.gradient = TRUE, scale = NULL, plot = TRUE, ...)
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

  \item{label}{string title (label) of the legend. It is
  used as the name of the scale if
  \code{scale.fill.gradient} is \code{TRUE}, otherwise it
  is ignored.}

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

  \item{scale.fill.gradient}{logical value. If it is
  \code{TRUE} (Default), it uses
  \code{\link{scale.fill.gradient}}}

  \item{scale}{alternative parameter to
  \code{scale.fill.gradient}, it is a term potentially
  added for color scale (see \code{\link{scale_colour_hue}}
  o similars). It is used only if it is not \code{NULL} or
  \code{scale.fill.gradient} is \code{FALSE}}

  \item{plot}{logical value. If \code{TRUE} (Default) the
  function also plots the map, otherwise returns its value
  without any preliminary plot.}

  \item{...}{further arguments}
}
\value{
A \class{"ggplot"} object.
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
x <- plotOn(x=soilwatercontent_mask,alpha=0.1)
}
\seealso{
\code{\link{geom_point}},\code{\link{ggmap}},\code{\link{facet_wrap}}
}

