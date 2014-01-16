\name{CreateMask}
\alias{CreateMask}
\title{DESCRIPTION TO DO}
\usage{
CreateMask(dsn = NULL, layer,
  url = "http://dati.trentino.it/storage/f/2013-05-09T141315/788_Comuni_amministrativi___DB_Prior_10k__12_12_2011_4uWD8r.zip",
  zip = FALSE, clean = FALSE, option = list(NOME_PROV = "TRENTO"),
  rasterRes = 500, raster = NULL, field = "COM",
  filename = "maskXYRES.grd", valuesFromRaster = TRUE, ...)
}
\arguments{
  \item{dsn}{see \code{\link{readOGR}}}

  \item{layer}{see \code{\link{readOGR}}}

  \item{url}{adress used for the download of data}

  \item{zip}{logical value. Default is \code{FALSE}. If
  \code{TRUE} the dowloaded file is unzipped after
  downloadig. See Details.}

  \item{clean}{logical value. Default is \code{FALSE}.
  (experimental).}

  \item{option}{list of object, e.g. fields and related
  values corresponded to the selected polygons.}

  \item{raster}{\code{Raster*} or \code{NULL} object used
  for rasterization. Default is \code{NULL}, see
  \code{\link{rasterize}}.}

  \item{rasterRes}{Raster Resolution in x and y (meters or
  degrees). It is used to create a \code{Raster*} for
  rasterization in case the argument \code{raster} is
  \code{NULL}.}

  \item{field}{see \code{rasterize}}

  \item{filename}{filename where ...}

  \item{valuesFromRaster}{logical value. If it is
  \code{TRUE}(Default), it fills the values within the mask
  with the ones of the raster map given by \code{raster}.}

  \item{...}{futher arguments for \code{\link{readOGR}}}
}
\description{
DESCRIPTION TO DO
}
\details{
In case of downloaded from an URL, the file is
automatically unzipped if a zip archive wth \code{.zip}
extenstion.
}
\examples{
library(stringr)
library(rgdal)
library(raster)

mask <- CreateMask() ## see default in Usage
}

