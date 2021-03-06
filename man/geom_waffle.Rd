% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_waffle.R
\name{geom_waffle}
\alias{geom_waffle}
\title{Create Github style waffle plots.}
\usage{
geom_waffle(data, date_par, data_scale, scale_name, pal = "D", dir = -1)
}
\arguments{
\item{data}{The table containing the data}

\item{date_par}{The column containing the dates}

\item{data_scale}{The data column to be plotted}

\item{scale_name}{The description of the data.}

\item{pal}{The color palette. Default is "D" (viridis).}

\item{dir}{The color direction. Default is "-1".}
}
\value{
A GitHub style waffle plot of the data.
}
\description{
Based on code from: https://mvuorre.github.io/r/github-waffle-plot/
}
\examples{
geom_waffle(data, dates, data_scale, scale_name)
}

