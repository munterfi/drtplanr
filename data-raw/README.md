# Example data sets inlcuded in drtplanr

## How to

To recreate the package example data run the following command from the
data-raw directory root:
``` bash
./create.sh -k "<YOUR API KEY>"
```

**Note:** A HERE API key is required and the following R packages are required:
`data.table`, `sf` and `hereR`.

## Load the example is R

To read the example data from the package use:
``` r
aoi <-
  "example.gpkg" %>% system.file(package="drtplanr") %>%
  sf::st_read(layer = "aoi", quiet = TRUE)
  
pop <-
  "example.gpkg" %>% system.file(package="drtplanr") %>%
  sf::st_read(layer = "pop", quiet = TRUE)
```

## References

* [hereR](https://github.com/munterfi/hereR): R interface to the HERE REST APIs 
* [bfs](https://www.bfs.admin.ch/): Population and structural business statistics for Switzerland
