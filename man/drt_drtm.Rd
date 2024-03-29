% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drtm.R
\name{drt_drtm}
\alias{drt_drtm}
\title{Demand responsive transport model class.}
\usage{
drt_drtm(
  model_name,
  aoi,
  pop,
  n_vir,
  m_seg = 100,
  calc_energy = e_walkDrive_pop
)
}
\arguments{
\item{model_name}{character, name of the drtm.}

\item{aoi}{sf, polygon of the Area of Interest (AOI).}

\item{pop}{sf, centroids of a hectaraster population dataset covering the full extent of the 'aoi' input (column name for population must be 'n').}

\item{n_vir}{numeric, number of the virtual stations to place.}

\item{m_seg}{numeric, resolution of the road segmentation in meters.}

\item{calc_energy}{function, energy calculation function.}
}
\value{
A demand responsive transport model of class 'drtm'.
}
\description{
Creates a demand responsive transport representation of in an Area of
Interest (AOI). The constructor takes a polygon (aoi), point population
values (pop) and the number of virtual stations to plan as input. Then it
retrieves the street network in the area of interest from OSM. Based on the
OSM data a routing graph for walking and driving is created.
}
\examples{
# Example data
aoi <-
  sf::st_read(system.file("example.gpkg", package = "drtplanr"), layer = "aoi")

pop <-
  sf::st_read(system.file("example.gpkg", package = "drtplanr"), layer = "pop")

# Create model
m <- drt_drtm(
  model_name = "Jegenstorf",
  aoi = aoi, pop = pop,
  n_vir = 10, m_seg = 100
)
m
}
