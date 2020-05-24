# drtplanr 0.0.0.9000

Experimental development version of the `drtplanr` package:
Tool for placing virtual stations in demand-responsive transport systems in
villages by defining and minimizing a global energy (`drtplanr`, name is
inspired by [stplanr](https://github.com/ropensci/stplanr)). The station
locations are randomly initialized in the street network and iteratively
optimized based on the reachable population in combination with walking and
driving times.

* Class `drtm` class: Demand-responsive transport model.
* Interface to drtm class: `drt_*()` functions.
* Package example: An assumed on-demand shuttle service for the community of
Jegenstorf in Bern, Switzerland. 
