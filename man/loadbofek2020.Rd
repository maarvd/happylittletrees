% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadbofek2020.R
\name{loadbofek2020}
\alias{loadbofek2020}
\title{Retrieve BOFEK cluster from bodemdata WFS service}
\usage{
loadbofek2020(sf, expand)
}
\arguments{
\item{sf}{Spatial feature polygon}

\item{expand}{Extension in meters}
}
\description{
Retrieve BOFEK2020 cluster for polygon of interest
}
\examples{
bofek <- loadbofek2020(sf = parcel, expand = 20)
}
