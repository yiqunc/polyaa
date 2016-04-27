======== User Manual ========
This tool generates urbanization agglomeration indice at any given administrative district levels.


== PART 1: Dataset Preparation ==  

The minimum data requirement for this tool consists two parts:
(1) administrative district polygons 
(2) associated population data
In the "data" folder, you can see 'LKA_adm2.shp' and 'pop_2012.csv' files, which represent the district polygons and population (2012) for Sri Lanka respectively.
GADM (www.gadm.org) is generally a good source to obtain district polygons. For many countries like Australia, user can directly get excellent data services from the government, such as Australian Bureau of Statistics, we encourage user to get data from this official channels.
Additionally, user can provide "seed" files to facilitate the agglomeration algorithm:
(3) seed definition data
(4) seed population data
For the Sri Lanka dataset, the "admin_type.csv" file contains seed definition: for any record whose "Admin_Type" value equals to "Urban Council" or "Municipal Council" or "Pradeshiya Sabha" can be treated as a seed. "seedpop2012.csv" file contains the seed population for 2012. 
All datasets link to each other via administrative district name. Make sure they are well matched.


== PART 2: How It Works ==

Each agglomeration needs a seed. User can prepare the aforementioned two seed files for the algorithm or simply choose top N population districts as seeds. In either way, the selected seeds must meet a user defined seed minimum population criterion.

For each non-seed district, the algorithm first finds its "closest" seed using the following gravity equation:
gravity = seedpop / (dist^2)
where "seedpop" represents the population of a seed and "dist" stands for the Euclidian distance between the centroids of seed and non-seed districts.
A seed is more "attractive" to a non-seed if the gravity value is higher. In this way, the seed with the highest gravity value is assigned to that non-seed. 
But this doesn't indicate that this non-seed district can actually be merged to the seed. To make a merging decision, the algorithm takes into account two more factors:
(1) the non-seed district has a big enough population to meet a user defined minimum population criterion
(2) the travel time between the centroids of seed and non-seed districts meets a user defined maximum travel time criterion.

If the travel time between a seed and non-seed cannot be found in the cached route database (i.e. "output/countryname/routefinfo/db.csv"), the algorithm will call Mapquest/Google navigation API to get this value and then cache it. 
This will boost the processing speed dramatically in the next run.


== PART 3: How To Use It == 

The algorithm is written in R and tested in Mac OS X 10.8.5. User is assumed to know how to use R.

Before loading the script into R console, some changes need to be made in the source code:
(1) Set up your working directory
setwd("/Users/yiqunc/PolygonAA")
(2) Set up global variables:
the outputs for a country in a specific year will be stored at "gOutRootDirgCountryName/gDataYear/seedDirName" 
gOutRootDir <<- "./output/"
gCountryName <<- "Sirilanka"
gDataYear <<- "2012"

if travel distance / euclidean distance is bigger than gTERMax, the API results are treated as issused ones and need further correction.
gTERMax <<- 3.0

the total population of a country in a specific year
gCountryTotalPop <<- 20263723 # Sirilankn population  2012: 20263723 ;  for 2001 : 17179935

show debug infomation when processing
gShowDebugInfo = FALSE
(3) Define a projected srs (spatial reference system) for your case study area. User can use the "Proj4js format" string declared in http://spatialreference.org/ 
Finding a proper projected srs is important it affects the accuracy of euclidean distance calculation.
User can easily extend the srsDF data frame to include new srs for the target country.
If user cannot provide an valid projected srs, the script will use EPSG:3857 as the universal projected srs (http://trac.osgeo.org/proj/wiki/FAQ). While, this is NOT the best solution for any particular area.


(4) Setup default parameter values for generic function "AI.gen". Please check out the source code for parameter definitions. Here are some usage examples:

(a) create AI with single threshold combination: population density:150, minimum seed population 20000, travel time 60 minutes
AI.gen(thPopDen=150,thSeedMinPop=20000, thTvlTime=60)

(b) run AI in batch mode
AI.batch(vecThPopDen=c(150, 300),vecThSeedMinPop=c(50000, 100000),vecThTvlTime=c(60, 90))

(c) calculate sum AI for the batch mode outputs
AI.sum(vecThPopDen=c(150, 300),vecThSeedMinPop=c(50000, 100000))
