% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadkadaster.R
\name{loadkadaster}
\alias{loadkadaster}
\title{Retrieve kadaster information from PDOK WFS service}
\usage{
loadkadaster(sf, expand, layer)
}
\arguments{
\item{sf}{Spatial feature object}

\item{expand}{Extension in meters}

\item{layer}{Layer of kadastrale kaart, supported are "Perceel", "KadastraleGrens", "Nummeraanduidingreeks", "Bebouwing" and "OpenbareRuimteNaam"}
}
\description{
Retrieve kadater information for area of interest
}
\examples{
kadastrale_percelen <- loadkadaster(sf = aoi, expand = 20, layer = "Perceel")
}
