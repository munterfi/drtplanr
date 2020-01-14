# Demand responsive transport system planner (drtplanr)

Open-source tool for planning and optimizing demand responsive transport systems.

|![](docs/model_energy.png)|![](docs/station_map.png)|
|---|---|

## Getting started
First the necessary data sets have to be downloaded. Open a shell in the root directory
of the repository and run the following lines:

``` bash
# Get STATPOP data set
mkdir -p data/statpop && cd "$_"
curl https://www.bfs.admin.ch/bfsstatic/dam/assets/9947069/master -o statpop.zip
unzip -a statpop.zip
rm statpop.zip && cd -

# Get OSM cutout
mkdir -p data/osm && cd "$_"
curl https://download.geofabrik.de/europe/switzerland-latest-free.shp.zip -o osm.zip
unzip -a osm.zip
rm osm.zip && cd -
```

To be able to access the HERE APIs (routing, public transport connections and stations)
an API key for a HERE project is required. Go to [HERE Developer](https://developer.here.com/), log in or sign up and create an API key (250k request are free): Select a project, click ‘REST: Generate APP’, click ‘Create API Key’ and copy the key. Create a new json file from this template:
``` json
{
   "here":{
      "key":"<YOUR-API-KEY>"
   },
   "proxy":{
      "url":"",
      "usr":"",
      "pw":""
   }
}
```
Paste the key into the new file and save the file as `"config.json"` to the the root
directory of the repository.

Done! Now you can run the R scripts in the root repository.

``` bash
Rscript 01_prepare_data.R 
Rscript 02_model_setup.R  
Rscript 03_model.R  
Rscript 04_plot_result.R 
```

## Authors
* Merlin Unterfinger

## References
* [hereR](https://github.com/munterfinger/hereR): Routing in R using HERE APIs
* [geofabrik.de](https://download.geofabrik.de): OpenStreetMap data extracts
* [bfs](https://www.bfs.admin.ch/): Population data for Switzerland 2018 (STATPOP)

## Licence
* This repository is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.