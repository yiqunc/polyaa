# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ========================================================================
# ========================================================================
# Copyright 2014-2016 University of Melbourne
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# ========================================================================
# ========================================================================
#
# Purpose: the algorithm is developed to measure the urbanization 
#          index of different countries in a specific year at any given 
#          administrative district levels. 
# Version: 3.1
# Last Updated: 28-April-2016
# Written by: Dr. Yiqun Chen    yiqun.c@unimelb.edu.au
#             Dr. Jennifer Day    jday@unimelb.edu.au
#
# Change Log:
# (v3.1) (1) compatibility fix for MapQuest/Google API changes.
#		 (2) use China mainland as a sample case.
#		 (3) user can provide a precooked inner centroids shape file instead of running InnerCentroids.gen() method
# (v3.0) (1)adding license stuff 
#        (2)add a new processing control varialbe: gLoopStartIndex, defualt as 1
#        In case of the process ends abnornally, e.g. experiencing network exceptions, or being terminated by ourselves since it's time to go home, 
#        and later we wanna continue the processing instead of starting for the every beginning again, we can set this variable with the 'index value' showing in the last output line:
#        [1088/14012]=====nonseedidx: 2340
#        [index value / total nonseed number]=====nonseedidx: nonseed idx
#        In this example, we need to set gLoopStartIndex to 1088.
#        ATTENTION: make sure to set gUseCachedRouteDBOnly to FALSE to avoid missing route info from real API calls if a 'complete' cached DB is not constructed yet.
#
#        (3)add a new processing control variable: gUseCachedRouteDBOnly, default as FALSE
#        This is a time-saving option. ALWAYS set it to FALSE  for the first run if any changes in seed or grid data sources.
#        If set to TRUE, it only queries the route info from cached DB and won't call APIs even it cannot find the route from DB. It works great for two scenarios:
#        (a) With the seed and grid datasource unchanged, if we start the first run with a loose set of configurations such as (150:50000:60) and the entire process ends normally,
#           then we will have a 'complete' cached route DB fully covers all routes for a harsh set of configurations such as (150:100000:60) or (300:50000:60) or (300:100000:60) or (150:50000:30).
#           so if we plan to run it again over these configurations, we can safely set gUseCachedRouteDBOnly to TRUE. 
#        (b) In same extreme cases, for example, when processing grids for Indea, the data set is too big to be handled properly in one run (it could be, but very likely to be terminated by network errors due to the long processing time). 
#           but we eventually can work out a 'complete' cached route DB part by part using gLoopStartIndex variable. Since the process is interrupted, we have to run it again to genereate AI values (ATTENTION: gLoopStartIndex must set to 1).
#           Because the cached DB is complete , we can set gUseCachedRouteDBOnly to FALSE to accelerate the process. 
#
# (v2.1) new auto-save feature, it periodically saves api route query data to local disk to aviod data loss when process is terminated abnormally. 
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# usage examples:
# (1) create AI with single threshold combination: population density:150, minimum seed population 20000, travel time 60 minutes
# AI.gen(thPopDen=150,thSeedMinPop=20000, thTvlTime=60)
#
# (2) run AI in batch mode
# AI.batch(vecThPopDen=c(150, 300),vecThSeedMinPop=c(50000, 100000),vecThTvlTime=c(60, 90))
#
# (3) calculate sum AI for the batch mode outputs
# AI.sum(vecThPopDen=c(150, 300),vecThSeedMinPop=c(50000, 100000))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# required packages
library(maptools)   # for geospatial services; also loads foreign and sp
library(rgdal)      # for map projection work; also loads sp
library(rgeos)
library(RCurl)
library(rjson)
library(RecordLinkage)

# set up the working directory
setwd("C:\\Users\\chen1\\SourceCodeRepos\\polyaa")

# the outputs for a country in a specific year will be stored at "gOutRootDirgCountryName/gDataYear/seedDirName" 
gOutRootDir <<- "./output/"
gCountryName <<- "China"
gDataYear <<- "2010"

#if travel distance / euclidean distance is bigger than gTERMax, the API results are treated as issused ones and need further correction.
gTERMax <<- 3.0

# the total population of a country in a specific year
gCountryTotalPop <<- 1332650308 # China population  2010: 1332650308; 2000: 1211070419  Jiangsu 2010:78500380 2000: 70205132

# global dataframe to store RouteInfo
gRouteInfoDF = NULL

# global auto routeinfo save threshold, e.g., every 200 new routes are obtained from API, save them locally 
gRouteInfoAutoSaveNum = 200

# only try to find route info from cached DB, won't call API again. 
# ATTENTION: always set gUseCachedRouteDBOnly to FALSE for the first run and don't turn it to TRUE until you read change log v2.6 carefully
gUseCachedRouteDBOnly = FALSE

# the start index of processing loop, default as 1
gLoopStartIndex = 1

# show debug infomation when processing
gShowDebugInfo = TRUE

# count how many times gg service has been requested
gGoogleRequestCounter <<- 1
gGoogleAPIKeys <<- c("AIzaSyDX523MBMKJvjlTUBAVqWQGatr0JVQEXVU","AIzaSyDB0GDOXAcrGMSxFkPhxfHHVqbAT_r9KXo","AIzaSyCC5bhar2y_032X4IJYPKbLVUEMAThuR24")
gGoogleAPIKeyCurIndex <<- -1
gGoogleAPIKeysRequestCounter <<- c(0,0,0)

# define URL templates
MapQuestKeyString = "&key=Fmjtd%7Cluua25uzl9%2Cbn%3Do5-962suy"
MapQuestDirectURLTemplate = "http://open.mapquestapi.com/directions/v1/route?outFormat=json&from=%.6f,%.6f&to=%.6f,%.6f&routeType=fastest&narrativeType=none&generalize=0&doReverseGeocode=false&unit=k"
GoogleDirectURLTemplate = "http://maps.googleapis.com/maps/api/directions/json?origin=%.6f,%.6f&destination=%.6f,%.6f&sensor=false"
GoogleGeoCodingURLTemplate = "http://maps.googleapis.com/maps/api/geocode/json?latlng=%.6f,%.6f&sensor=false"

# define SRS for case study area, use the "Proj4js format" string declared in http://spatialreference.org/ 
# Finding a proper projected srs is important for accurate projection. It affects the result of euclidean distance calculation.
#using EPSG:3857 for universal projected srs http://trac.osgeo.org/proj/wiki/FAQ. This is NOT the best solution for any particular area.
CONST_projected_proj4string_universal = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs" #"+proj=merc +datum=WGS84"

srsDF = data.frame(countryname=NULL, EPSG=NULL, proj4string=NULL)
# SLD99 / Sri Lanka Grid 1999: https://code.google.com/p/pyproj/source/browse/trunk/lib/pyproj/data/epsg?r=286
srsDF <<- rbind(srsDF,data.frame(countryname="SriLanka", EPSG="", proj4string="+proj=tmerc +lat_0=7.000471527777778 +lon_0=80.77171308333334 +k=0.9999238418 +x_0=500000 +y_0=500000 +a=6377276.345 +b=6356075.41314024 +towgs84=-0.293,766.95,87.713,0.195704,1.69507,3.47302,-0.039338 +units=m +no_defs") )
srsDF <<- rbind(srsDF,data.frame(countryname="Bangladesh", EPSG="EPSG:3106", proj4string="+proj=tmerc +lat_0=0 +lon_0=90 +k=0.9996 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +units=m +no_defs") )
srsDF <<- rbind(srsDF,data.frame(countryname="Nepal", EPSG="", proj4string="+proj=tmerc +lat_0=0 +lon_0=84 +k=0.9999 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +units=m +no_defs") )
srsDF <<- rbind(srsDF,data.frame(countryname="Bhutan", EPSG="EPSG:5266", proj4string="+proj=tmerc +lat_0=0 +lon_0=90 +k=1 +x_0=250000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") )

CONST_projected_proj4string = as.vector(srsDF[srsDF[,"countryname"] == gCountryName,"proj4string"])

if(length(CONST_projected_proj4string)==0 || CONST_projected_proj4string == ""){
  CONST_projected_proj4string = CONST_projected_proj4string_universal
}

#using EPSG:4326 for unprojected crs
CONST_unprojected_proj4string = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
#define epsg string, which correct the ".prj" file creation issue in 
CONST_unprojected_proj4string_epsg = "+init=epsg:4326"

debugPrint <- function(str){
  if(gShowDebugInfo){
    print(str)
  }
}

# load route info from file
RouteInfo.load <- function(filePath){
  if (file.exists(filePath)){
    con <- file(filePath, open="r") 
    gRouteInfoDF <<- read.table(con, header=TRUE, sep=",", blank.lines.skip = TRUE, fill = TRUE)
    close(con)
  } else {
    gRouteInfoDF <<- data.frame(startIdx=NULL, endIdx=NULL, eucDist=NULL, tvlDist=NULL, tvlTime=NULL, api=NULL, jsonPath=NULL)
  }
}

# append new route into gRouteInfoDF
RouteInfo.append <- function(startIdx=0, endIdx=0, eucDist=0, tvlDist=0, tvlTime=0, api="MQ", jsonPath="", routeInfoFilePath=""){
  gRouteInfoDF <<- rbind(gRouteInfoDF,data.frame(startIdx=startIdx, endIdx=endIdx, eucDist=eucDist, tvlDist=tvlDist, tvlTime=tvlTime, api=api, jsonPath=jsonPath))
  
  # triger auto save if increment meets the condition 
  if (!is.null(nrow(gRouteInfoDF))){
    if (nrow(gRouteInfoDF) %% gRouteInfoAutoSaveNum == 0){
      RouteInfo.save(routeInfoFilePath)
      debugPrint(sprintf("====================== auto-save at [%i] routes",nrow(gRouteInfoDF)))
    }
  }

}

# Google Navigation Service Wrapper
GG.query <- function(url="", mapUnicode=FALSE){
  
  # first, try to send request without key 
  gGoogleRequestCounter <<- gGoogleRequestCounter + 1
  output = fromJSON(getURL(url, .mapUnicode = mapUnicode))
  
  # if OVER_QUERY_LIMIT occurs, try to use a key that still has quota
  if(output$status == "OVER_QUERY_LIMIT"){
    # get a valid key index
    for(i in 1:length(gGoogleAPIKeysRequestCounter)){
      if(gGoogleAPIKeysRequestCounter[i]<2500){
        gGoogleAPIKeyCurIndex = i
        break
      }
    }
    # if valid key is available
    if(gGoogleAPIKeyCurIndex > 0){
      gGoogleRequestCounter <<- gGoogleRequestCounter + 1
      gGoogleAPIKeysRequestCounter[gGoogleAPIKeyCurIndex] <<- gGoogleAPIKeysRequestCounter[gGoogleAPIKeyCurIndex] + 1
      curKey = gGoogleAPIKeys[gGoogleAPIKeyCurIndex]
      keyedUrl = sprintf("%s&key=%s",url,curKey)
      keyedUrl = gsub("http", "https", keyedUrl)
      output = fromJSON(getURL(keyedUrl, .mapUnicode = mapUnicode))
    }else{
      #otherwise, still try the nokey request for any luck
      print(sprintf("=== ATTENTION: all GG keys quota are used."))
      gGoogleRequestCounter <<- gGoogleRequestCounter + 1
      output = fromJSON(getURL(url, .mapUnicode = mapUnicode))
    }
  }
  
  if(output$status != "OK"){
    print(sprintf("=== GG requests %i,  status: %s",gGoogleRequestCounter, output$status))
  }
  
  return (output)
  
}

# query route info from gRouteInfoDF based in start and end index
RouteInfo.query <- function(startIdx=0, endIdx=0){
  rlt = NULL
  if (!is.null(nrow(gRouteInfoDF))){
    if (nrow(gRouteInfoDF) > 0){
      sFilter = gRouteInfoDF[,"startIdx"] == startIdx
      eFilter = gRouteInfoDF[,"endIdx"] == endIdx
      rlt = gRouteInfoDF[sFilter&eFilter,]
    }
  }
  return(rlt)
}

# save route info into file
RouteInfo.save <- function(filePath){
  con <- file(filePath, open="w")
  write.table(gRouteInfoDF, file=filePath, row.names = FALSE, sep=",")
  close(con)
}

# generate centroids inside polygons
InnerCentroids.gen <- function(pols, trynum=10, accurate=TRUE) {
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # find inner centroid for polygons using negative buffer
  # inspired by http://r-sig-geo.2731867.n2.nabble.com/Better-label-placement-for-polygons-td7580550.html
  # Input:
  # (1) pols: SpatialPolygonsDataFrame
  # (2) trynum: how many times algorithm can try to neg-buffer. Once trynum is reached, the default gCentroid result will be used, it means an inner centroid is nor found.
  # (3) accurate: if false, in each neg-buffer loop, the centroid of new polygon will be tested to see if it is within the original polgyon; 
  #               if true, in each neg-buffer loop, the centroid of new polygon will be tested to see if it is within the new polgyon;
  # Output:
  # a list of centroids, each element is SpatialPoints
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  
  #plot(pols)
  centroidList = list()
  # For each polygon in pols, calculate the appropriate label point 
  for(i in seq_len(length(pols))) { 
    debugPrint(sprintf("=== calc inner centroid for polygon[%i]",i))
    p = pols[i,]
    
    # fill the centroidList with default centroid
    centroidList[[i]] = gCentroid(p)
    
    # continue if the centroid is already within polygon
    if (gWithin(centroidList[[i]], p)){
      #plot(centroidList[[i]], col="green", add=TRUE)
      next
    }
    
    # init buffer distance
    radius = sqrt(gArea(p)/pi)
    r = radius
    maxr = radius
    minr = 0
    counter = 0
    
    # try to find the inner centroid
    repeat {
      # exit condition test
      if (counter > trynum) break
      
      # shrink and create a new polygon
      p2 = gBuffer(p, width = -r)
      
      # if r is too big, the new polygon will be empty, try to decrease r
      if (gArea(p2) <= 0){
        maxr = r
        r = r - (maxr-minr)/2
        #debugPrint(sprintf("===keep going r:%.8f, step:%.8f",r,estep))
      } else {
        # if new polygon is not empty, let's test if its centroid is good enough to use
        centroid = gCentroid(p2)
        if (accurate == TRUE){
          flag = gWithin(centroid, p2)
        } else {
          flag = gWithin(centroid, p)
        }
        # if centroid is within polygon (new or original), we take it
        if (flag){
          centroidList[[i]] = centroid
          #debugPrint(centroid)
          #plot(centroidList[[i]], col="red", add=TRUE)
          break
        } else {
          # if not, it means the new polygon is not bufferred (or smooth) enough to hold its centroid inside, try to increase r
          minr = r
          r = r + (maxr-minr)/2
          #debugPrint(sprintf("not within===keep going r:%.8f, step:%.8f",r,estep))
        }
      }
      counter = counter + 1
    } 
  }
  
  # return the result
  return(centroidList)
}

# load centroids
InnerCentroids.load <- function(filePath, pols, trynum=10, accurate=TRUE) {
  centroidList = list()
  if (file.exists(filePath)){
    original_proj4string = attr(pols@proj4string,"projargs")
    con <- file(filePath, open="r")
    coordsDF = read.table(con, header=TRUE, sep=",", blank.lines.skip = TRUE, fill = TRUE)
    close(con)
    
    for (i in 1:nrow(coordsDF)){
      centroidList[[i]] = SpatialPoints(list(coordsDF[i,"x"], coordsDF[i,"y"]), proj4string=CRS(original_proj4string))
    }

    debugPrint("++++++++++++++++ cached centroids applied ++++++++++++++++++++")
  } else {
    centroidList = InnerCentroids.gen(pols,trynum,accurate)
    #save centroid
    InnerCentroids.save(filePath, centroidList)
  }
  
  #return centroids
  return(centroidList)
}

# save centroids locally
InnerCentroids.save <- function(filePath, ctrdList){
  coordsDF = data.frame(x=NULL, y=NULL)
  for (i in 1:length(ctrdList)){
    lat = ctrdList[[i]]@coords[2]
    lon = ctrdList[[i]]@coords[1]
    coordsDF <- rbind(coordsDF,  data.frame(x=lon, y=lat))
  }
  con <- file(filePath, open="w")
  write.table(coordsDF, file=filePath, row.names = FALSE, sep=",")
  close(con)
  debugPrint("++++++++++++ centroids are cached ++++++++++++")
}

# get the distance between two latlon location
GeoTool.getLatLonDistance <- function(sLat, sLon, eLat, eLon){
  
  s_lon = c(sLon)
  s_lat = c(sLat)
  s = SpatialPoints(list(s_lon, s_lat),proj4string=CRS(CONST_unprojected_proj4string))
  s_pj = spTransform(s,CRS(CONST_projected_proj4string))
  e_lon = c(eLon)
  e_lat = c(eLat)
  e = SpatialPoints(list(e_lon, e_lat),proj4string=CRS(CONST_unprojected_proj4string))
  e_pj = spTransform(e,CRS(CONST_projected_proj4string))
  # return projected distance in meters
  return(gDistance(s_pj, e_pj))
}

# gen_v2() method is handy if all required population data(e.g., pop and den) have already been cooked and wrapped into one shp file.
# This is a consice and compact version of gen() by skipping all data merging/matching steps
# A shp file that can be fed into this method must contain the following attributes (column names can be different): 
# (1)popYYYY (YYYY represents the census year in 4 digits, e.g. pop2010)
# (2)denYYYY (YYYY represents the census year in 4 digits, e.g. den2010)
# (3)lon: polygon centroid longitude (or x) 
# (4)lat: polygon centroid latitude (or y) 
# sample usage:
# (1) AI.gen_v2(thSeedMinPop=500000,thTvlTime=60,seedMergeDist=90,useSeedCandidates=TRUE,seedDirName="ByKeyCities_SeedMergeDist90")
# (2) AI.gen_v2(thPopDen=400,thSeedMinPop=1000000,thTvlTime=90,seedMergeDist=90,useSeedCandidates=FALSE,seedDirName="BySeedFilter_SeedMergeDist90")
AI.gen_v2 <- function(dsn="data\\CHN_adm", 
                   polyFileName="China_adm2", 
                   outDir=gOutRootDir,
                   thPopDen=150, 
                   thSeedMinPop=1000000,
                   thTvlTime=60, 
                   thSearchSeedRadius=200, 
                   thGeoCodingDistShift=3,
                   th_seed_minden = 400,
                   ttlPop=gCountryTotalPop, 
                   popDataColName="pop2010",
                   denDataColName="den2010",
                   centroidXColName = "lng",
                   centroidYColName = "lat",
                   areaDataColName="area",
                   districtDataColName="name", 
                   seedDirName="BySeedFilter",
                   seedCandidatesVec = c('dongguan','guangzhou','huizhou','zhaoqing','foshan','jiangmen','zhongshan','shenzhen','zhuhai','taizhou, jiangsu','yangzhou','zhenjiang','nanjing','nantong','changzhou','wuxi','suzhou','shanghai','huzhou','jiaxing','hangzhou','shaoxing','ningbo','zhoushan','qinhuangdao','beijing','beidaihe','haibin','tangshan','langfang','tianjin','huanghua','cangzhou','xinxiang','jiaozuo','kaifeng','zhengzhou','luoyang','xuchang','pingdingshan','mianyang','deyang','chengdu','leshan','chongqing','changchun','jilin city','tumen','yanji','longjing','qiqihar','harbin','mudanjiang','suifenhe','daqing','wuhan','qianjiang,hubei','xianning','jiujiang','jingdezhen','huanggang','nanchang','yingtan','changsha','fuzhou, jiangxi','huangshi','xiangtan','zhuzhou','xinyu','ezhou'),
                   seedCandidatesMatchColName = "adm1_name",
                   useSeedCandidates = FALSE,
                   countryName=gCountryName,
                   dataYear=gDataYear,
                   seedMergeDist=60
){
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # Input parameters:
  # (1) dsn: the root directory of data sets
  # (2) popFileName: population data (csv or txt) file name.
  # (3) polyFileName: administrative boundary shp file name.
  # (4) admtypeFileName: administrative type data file name, it can be ignored if the administrative type information is already in the "polyFileName" data
  # (5) outDir: the root directory of calculation outputs
  # (6) thPopDen: threshold of population density. A non-seed area is only considered as agglomeratable if its popden is bigger than this threshold. unit: the number of people in 1 square km
  # (7) thSeedMinPop: threshold of population. A seed area is only considered and used as an agglomeration core if its population is larger than this threshold. 
  # (8) thTvlTime: threshold of travel time between seed and non-seed districts. unit: minute
  # (9) thSearchSeedRadius: threshold of the Euclidian distance between a non-seed and its potential seeds. Those too distant seeds will be excluded from the gravity calculation.
  # (10) thGeoCodingDistShift: threshold of the Euclidian distance between the original point and reverse geocoding point. If the reverse geocoding point is too distant away from the original one, it will not be used.
  # (11) ttlPop: total population of the target country
  # (12) popMatchColName: a column name in "popFileName" data for matching (joining) with "polyFileName" data 
  # (13) polyMatchColName: a column name in "polyFileName" data for matching (joining) with "popFileName" data
  # (14) admtypeMatchColName: a column name in "admtypeFileName" data for matching (joining) with "polyFileName" data
  # (15) popDataColName: the name of a column in "popFileName" data containing the population data for AI calculation
  # (16) admtypeDataColName: the name of a column in "admtypeFileName" (or ""polyFileName) data containing the administrative type data for seed selection
  # (17) seedAsAdmTypeVec: a vector of administrative types. If not empty, seeds will be selected based these administrative types. Default is empty. It can be c("Urban Council","Municipal Council","Pradeshiya Sabha") or c("Pradeshiya Sabha")
  # (11) seedAsTopNHighPopArea: the number of top population areas to be selected as seeds. Default value is -1, which means all qualified seeds will be in use.
  # (19) seedDirName: for different seed selections, the result will be put into different output directories This is the directory name for a particular seed selection. It can be any valid and meaningful folder name, no whitespace in it.
  # (20) countryName: the name of the country. It is used as a part of the outputs path
  # (21) dataYear: the year of population data.  It is used as a part of the outputs path
  # (22) seedMergeDist: -1: all closest seeds pairs are mergeable; 0 (default): use the median value of all seeds Eculidian distance as a threshold and only those seed pairs whose distance is smaller than this threshold are mergeable; or other positive value (Unit: km)
  # (23) seedMergeTo: "smallestIdx": all mergeable seeds will be merged to the seed whose idx is the smallest; "highestPop"(default): all mergeable seeds will be merged to the seed whose population is the highest; "highestPopDen": all mergeable seeds will be merged to the seed whose population density is the highest
  # (24) seedPopFileName: real seed population file name. if both seedAsAdmTypeVec and seedPopFileName are provided, the thSeedMinPop will be further applied on real seed population in the data file. The previously found seed will be marked as non-seed if the real population is smaller than the threshold.
  # (25) thSeedNameSimilarity: threshold of similarity between divisonname (polyMatchColName) in shape file and the "seedName" in seedPopFileName. range from 0-1. 1: 100% similarity applied.
  # (26) useDivisionAsSeedIfNoNameMatch: in the case of no matches found between divisonname and "seedName", if FALSE, the previously found seed will be marked as non-seed; if TRUE, the seed remains.
  # Output:
  # (1) shp files containing agglomeration index information
  # (2) a calculated route data folder ("routeinfo") containing raw route request api results
  # (3) a calculated inner centroid data file ("innerCentroids.csv") 
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  
  algStartTime = Sys.time()
  
  # create a folder to hold route information 
  routeInfoDir = sprintf("%s%s/%s/",outDir,countryName,"routeinfo")
  dir.create(routeInfoDir, showWarnings=FALSE, recursive=TRUE)
  routeInfoFilePath = sprintf("%s%s",routeInfoDir,"db.csv")
  
  # load route info into gRouteInfoDF
  RouteInfo.load(routeInfoFilePath)
  
  # create a folder to AI results (shp files) for different seed choices
  outShpFileDir = sprintf("%s%s/%s/%s",outDir, countryName, dataYear, seedDirName)
  dir.create(outShpFileDir, showWarnings=FALSE, recursive=TRUE)
  
  # set TRUE to enable seeds agglomeration 
  isSeedAgglomeratable = TRUE
  
  # set the total popluation of the country
  ttlPopulation = ttlPop
  
  # define thresholds 
  # POP DENSITY: run an AI at each of these minimum levels: 150, 300, and 500 people per square kilometre
  # TRAVEL TIME: run an AI at each of these maximum levels: 30, 60, and 90 minutes
  # MINIMUM SEED POPULATION: run an AI at each of these minimum levels: 20,000, 50,000, 100,000, and 500,000.
  th_popden = thPopDen
  th_tvltime = thTvlTime * 60
  th_seed_minpop = thSeedMinPop # the minimum population to be considered as a seed
  
  # Eclidian distance to search seeds around a non-seed. Unit: meter. It is used first to reduce real distance computation
  th_dist = thSearchSeedRadius * 1000
  # the maximum Euclidian distance between an orginal point and its reverse geocoding point, if the distance bigger than this threshold, the reverse geocoding point will NOT be used
  th_geocoding_dist_shift = thGeoCodingDistShift * 1000 
  
  
  # load adm boundary polygons
  adm2 <- readOGR(dsn=dsn,layer=polyFileName,encoding="utf8", verbose=FALSE)
  
  # using orgSeqId filed to ensure that the merged data can match polygons
  adm2@data[,"orgSeqId"] = c(1:nrow(adm2@data))
  
  
  # add additional columns : isseed (0:not seed, 1:seed), aggidx (default 0), aggidx2 (default 0, when seed agglomeration applied) 
  adm2@data[,"isseed"] = 0
  adm2@data[,"aggidx"] = 0
  adm2@data[,"isseed2"] = 0 # is an agglomerated seed
  adm2@data[,"aggidx2"] = 0
  
  # load the centroids for use
  centroidList = list()
    
  for (i in 1:nrow(adm2@data)){
    centroidList[[i]] = SpatialPoints(list(adm2@data[i,centroidXColName], adm2@data[i,centroidYColName]), proj4string=adm2@proj4string)
  }
  
  centroidList_pj = list()
  for (i in 1:length(centroidList)){
    centroidList_pj[[i]] = spTransform(centroidList[[i]],CRS(CONST_projected_proj4string))
  }
  
  # find seeds by pop and den filters
  seedPopFilter = adm2@data[, popDataColName] > th_seed_minpop
  seedDenFilter = adm2@data[, denDataColName] > th_seed_minden
  
  seedFilter = seedPopFilter & seedDenFilter
  
  if(useSeedCandidates){
    seedFilter = seedFilter & tolower(adm2@data[, seedCandidatesMatchColName]) %in% tolower(seedCandidatesVec)
  }
  seedFilter = ifelse(is.na(seedFilter), FALSE, seedFilter)
  
  seedDF = adm2@data[seedFilter,]
  nonseedDF = adm2@data[!seedFilter,]
  
  flag_seed_exist = FALSE
  if (!is.null(nrow(seedDF))){
    if (nrow(seedDF) > 0) {
      flag_seed_exist = TRUE
    }
  }
  if (!flag_seed_exist){
    print("====== no valid seeds found, the selection criteria is too harsh =====")
    return()
  }
  
  flag_nonseed_exist = FALSE
  if (!is.null(nrow(nonseedDF))){
    if (nrow(nonseedDF) > 0) {
      flag_nonseed_exist = TRUE
    }
  }
  if (!flag_nonseed_exist){
    print("====== no valid nonseeds found, the selection criteria is too harsh =====")
    return()
  }
  
  
  adm2@data[seedFilter,"isseed"] = 1 # set isseed flag, and mark seed's aggidx as it rowname
  adm2@data[seedFilter,"isseed2"] = 0 # set agglomerated seed index will be assigned later
  adm2@data[seedFilter,"aggidx"] = adm2@data[seedFilter,"orgSeqId"] #attr(adm2@data[seedFilter,], "row.names")
  adm2@data[seedFilter,"aggidx2"] = adm2@data[seedFilter,"orgSeqId"] #attr(adm2@data[seedFilter,], "row.names")
  
  # agglomarate seed if required
  if (isSeedAgglomeratable == TRUE){
    
    aggSeedList = list()
    for(i in 1:length(adm2@data[seedFilter,"aggidx"])){
      aggSeedList[[i]] = c(adm2@data[seedFilter,"aggidx"][i])
    }
    
    testList  = aggSeedList
    
    # this flag controls the outer while-loop, making sure that all seeds are properly aggregated. 
    # the inner two nested for-loops start from the head of testList, testing distance between seeds from two seed groups:seeds-m and seeds-n 
    # each seed group contains one or more seeds. seeds in a same seed group will eventually aggregate as a single seed.
    # if, for example, a seed (seeds-n-1) in group seeds-n is found within seedMergeDist of any one of seed in group seeds-m, all seeds in group seeds-n will be merged to group seeds-m.
    # In this case, since the seed groups are updated (seeds-m gets expanded,and seeds-n get destroied), we need to restart the outer while-loop again and continue testing distance between the updated seed groups
    reloopflag = TRUE
    loopcounter = 0
    while(reloopflag){
      reloopflag = FALSE
      loopcounter = loopcounter + 1
      print(sprintf("loopcounter %i",loopcounter))
      # merge seed by "minimaldist" method: two closest seeds will be merged if their distance is within the seedMergeDist; 
      for(n in 1:length(testList)){
        #reloopflag = FALSE
        if(length(testList[[n]])==1 & testList[[n]][1]==-1){
          next
        }
        
        for(m in 1:length(testList)){
          #reloopflag = FALSE
          if (m == n){
            next
          }
          
          if (length(testList[[m]])==1 & testList[[m]][1]==-1){
            next
          }
          
          tmpMinDist = 999999999
          for(p in 1:length(testList[[n]])){  
            
            for(q in 1:length(testList[[m]])){
              
              dist = gDistance(centroidList_pj[[testList[[n]][p]]], centroidList_pj[[testList[[m]][q]]])
              
              if(dist<tmpMinDist){
                tmpMinDist = dist
              }
            }
          }
          
          #if mindist is smaller than threshold, merge m to n, and clear m          
          if(tmpMinDist < seedMergeDist*1000){
            testList[[n]] = c(testList[[n]], testList[[m]])
            testList[[m]] = c(-1)
            reloopflag = TRUE
            #break
          }
          
        }
        if (reloopflag){
          #break
        }
      }
    }
    
    aggSeedList = testList
    
    # agglomerate seed
    for (i in 1:length(aggSeedList)){
      
      # skip cleared list element
      if (length(aggSeedList[[i]])==1 &aggSeedList[[i]][1]==-1){
        next
      }
      
      # by default, merge to the smallest idx seed
      aggSeedIdx = min(aggSeedList[[i]])
      
      # merge to the highest population seed
      highestPopValue = -1
      for (tmpIdx in aggSeedList[[i]]){
        if (adm2@data[tmpIdx, popDataColName] > highestPopValue){
          aggSeedIdx = tmpIdx
          highestPopValue = adm2@data[tmpIdx, popDataColName]
        }
      }
      
      vec = aggSeedList[[i]]
      vec = vec[which(vec!=aggSeedIdx)]
      adm2@data[vec,"aggidx2"] = adm2@data[aggSeedIdx,"aggidx2"]
      
    }
  }
  
  # mark isseed2 column
  aggSeedIdxVec = adm2@data[,"aggidx2"]
  aggSeedIdxVec = aggSeedIdxVec[!duplicated(aggSeedIdxVec) & (aggSeedIdxVec > 0)]
  adm2@data[aggSeedIdxVec,"isseed2"] = 1
  aggSeedFilter = adm2@data[,"isseed2"] == 1
  
  #route information data frame to record 
  routeInfoDF = data.frame(nonSeedIdx=NULL,targetSeedIdx=NULL,eucDist=NULL,tvlDist=NULL,tvlTime=NULL)
  
  for(i in gLoopStartIndex:nrow(nonseedDF)){
    
    nonSeedIdx = nonseedDF[i,"orgSeqId"] #attr(nonseedDF[i,], "row.names")
    debugPrint(sprintf("[%i/%i]=====nonseedidx: %i", i, nrow(nonseedDF), nonSeedIdx))
    
    # if a nonseed area has low pop density than threshold, it is not agglomeratable, ignore it
    if (is.na(nonseedDF[i,denDataColName])) {
      debugPrint("-- ignored (NA)")
      next
    }
    
    if (nonseedDF[i,denDataColName] < th_popden) {
      debugPrint("-- ignored (popden too low)")
      next
    }
    
    s_lat = centroidList[[nonSeedIdx]]@coords[2]
    s_lon = centroidList[[nonSeedIdx]]@coords[1]
    
    targetSeedIdx = -1
    maxGravity = -1
    rtDist = -1
    rtTime = -1
    eucDist = -1
    # find the closest seed using gravity
    for(j in 1:nrow(seedDF)){
      seedIdx = seedDF[j,"orgSeqId"] #attr(seedDF[j,], "row.names")
      dist = gDistance(centroidList_pj[[nonSeedIdx]], centroidList_pj[[seedIdx]])
      if (dist >= th_dist) next
      
      # calc and find the max gravity
      gravity = seedDF[j,popDataColName] / (dist * dist)
      if (maxGravity < gravity){
        targetSeedIdx = seedIdx
        maxGravity = gravity
        eucDist = dist / 1000
      }  
    }
    
    # if cannot find a seed around it, igore it
    if (targetSeedIdx < 0){
      debugPrint("-- ignored (cannot find an adjacent seed )")
      next
    }
    
    e_lat = centroidList[[targetSeedIdx]]@coords[2]
    e_lon = centroidList[[targetSeedIdx]]@coords[1]
    
    # test if route infomation already exists in gRouteInfoDF, if yes, fetch travel distance and time directly, otherwise, call apis
    cachedRouteInfo = RouteInfo.query(nonSeedIdx, targetSeedIdx)
    flag_cached_found = FALSE
    if (!is.null(nrow(cachedRouteInfo))){
      if (nrow(cachedRouteInfo) > 0) {
        flag_cached_found = TRUE
      }
    }
    if (flag_cached_found){
      rtTime = cachedRouteInfo[1,"tvlTime"]
      rtDist = cachedRouteInfo[1,"tvlDist"]
      debugPrint("++++++++++++++ cached route info applied ++++++++++++++++")
    } else {
      
      # if gUseCachedRouteDBOnly is TURE and we cannot find route in cached DB, mark to be further corrected
      if (gUseCachedRouteDBOnly){
        debugPrint(sprintf("-- ignored (no route found in cached DB between [%.6f,%.6f] - [%.6f,%.6f] )",s_lat,s_lon,e_lat,e_lon))
        routeInfoDF<-rbind(routeInfoDF,data.frame(nonSeedIdx=nonSeedIdx, targetSeedIdx=targetSeedIdx, eucDist=eucDist, tvlDist=-1, tvlTime=-1))      
        next
      }
      
      GGDirectURL = sprintf(GoogleDirectURLTemplate,s_lat,s_lon,e_lat,e_lon)
      GGRawJson = GG.query(url=GGDirectURL, mapUnicode = FALSE)
      if (GGRawJson$status == "OK"){
        debugPrint("*********** route find in google api *************")
        rtDist = GGRawJson$route[[1]]$legs[[1]]$distance$value / 1000
        rtTime = GGRawJson$route[[1]]$legs[[1]]$duration$value
        
        # test if big TER exists, if does, mark to be further corrected 
        if (rtDist/eucDist > gTERMax) {   
          debugPrint(sprintf("-- ignored (too big TER found between [%.6f,%.6f] - [%.6f,%.6f] )",s_lat,s_lon,e_lat,e_lon))
          routeInfoDF<-rbind(routeInfoDF,data.frame(nonSeedIdx=nonSeedIdx, targetSeedIdx=targetSeedIdx, eucDist=eucDist, tvlDist=-1, tvlTime=-1))
          next
        }
        
        # save google route info to file
        #routeJsonFilePath = sprintf("%s%s_%i_%i.json",routeInfoDir,"GG",nonSeedIdx, targetSeedIdx)
        #write(ggjsonstr,routeJsonFilePath)
        RouteInfo.append(nonSeedIdx,targetSeedIdx,eucDist,rtDist,rtTime,"GG","", routeInfoFilePath)
        
      } else {
        # if route cannot be found, try to using reverse geocoding api to re-generate both start and end location for new route query
        # try to find new start location
        isNewLocationApplied = FALSE
        isNewRouteFound = FALSE
        GGGeoCodingURL = sprintf(GoogleGeoCodingURLTemplate,s_lat,s_lon)
        GGGeoCodingRawJson = GG.query(url=GGGeoCodingURL, mapUnicode = FALSE)
        if (GGGeoCodingRawJson$status == "OK"){
          new_s_lat = GGGeoCodingRawJson$results[[1]]$geometry$location$lat
          new_s_lon = GGGeoCodingRawJson$results[[1]]$geometry$location$lng
          # test new location is close enough to the original location
          tmpDist = GeoTool.getLatLonDistance( new_s_lat, new_s_lon, s_lat, s_lon)
          if (tmpDist < th_geocoding_dist_shift) {
            # new location is not far away from origial one, use it
            s_lat = new_s_lat
            s_lon = new_s_lon
            isNewLocationApplied = TRUE
          }
        }
        
        # try to find new end location
        GGGeoCodingURL = sprintf(GoogleGeoCodingURLTemplate,e_lat,e_lon)
        GGGeoCodingRawJson = GG.query(url=GGGeoCodingURL, mapUnicode = FALSE)
        if (GGGeoCodingRawJson$status == "OK"){
          new_e_lat = GGGeoCodingRawJson$results[[1]]$geometry$location$lat
          new_e_lon = GGGeoCodingRawJson$results[[1]]$geometry$location$lng
          # test new location is close enough to the original location
          tmpDist = GeoTool.getLatLonDistance( new_e_lat, new_e_lon, e_lat, e_lon)
          if (tmpDist < th_geocoding_dist_shift) {
            # new location is not far away from origial one, use it
            e_lat = new_e_lat
            e_lon = new_e_lon
            isNewLocationApplied = TRUE
          }
        }
        
        # if either location is update, give it a chance to query the route again 
        if (isNewLocationApplied == TRUE){   
          GGDirectURL = sprintf(GoogleDirectURLTemplate,s_lat,s_lon,e_lat,e_lon)
          GGRawJson = GG.query(url=GGDirectURL, mapUnicode = FALSE)
          if (GGRawJson$status == "OK"){
            debugPrint("*********** route find in google api === after reverse geocoding *************")
            rtDist = GGRawJson$route[[1]]$legs[[1]]$distance$value / 1000
            rtTime = GGRawJson$route[[1]]$legs[[1]]$duration$value
            
            
            # test if big TER exists, if does, mark to be further corrected 
            if (rtDist/eucDist > gTERMax) {   
              debugPrint(sprintf("-- ignored (too big TER found between [%.6f,%.6f] - [%.6f,%.6f] )",s_lat,s_lon,e_lat,e_lon))
              routeInfoDF<-rbind(routeInfoDF,data.frame(nonSeedIdx=nonSeedIdx, targetSeedIdx=targetSeedIdx, eucDist=eucDist, tvlDist=-1, tvlTime=-1))
              next
            }
            
            # save google route info to file
            #routeJsonFilePath = sprintf("%s%s_%i_%i.json",routeInfoDir,"GG",nonSeedIdx, targetSeedIdx)
            #write(ggjsonstr,routeJsonFilePath)
            RouteInfo.append(nonSeedIdx,targetSeedIdx,eucDist,rtDist,rtTime,"GG","", routeInfoFilePath)
            
            isNewRouteFound = TRUE
          }
        }
        
        # if still cannot find a route, leave it (whose tvlTime will be set to -1) for further travel time estimation processing
        if (isNewRouteFound == FALSE) {   
          debugPrint(sprintf("-- ignored (no route found between [%.6f,%.6f] - [%.6f,%.6f] )",s_lat,s_lon,e_lat,e_lon))
          routeInfoDF<-rbind(routeInfoDF,data.frame(nonSeedIdx=nonSeedIdx, targetSeedIdx=targetSeedIdx, eucDist=eucDist, tvlDist=rtDist, tvlTime=rtTime))
          next
        }
      }
        
    }
    
    # test if the real travel time to target seed is short enough  
    if (rtTime >= th_tvltime){
      debugPrint(sprintf("-- ignored (too long to travel tvlTime:%.0f, tvlDist:%.3f )",rtTime, rtDist))
      next
    }
    
    # save the route infomation if a nonseed can be agglomerated to a seed 
    routeInfoDF<-rbind(routeInfoDF,data.frame(nonSeedIdx=nonSeedIdx,targetSeedIdx=targetSeedIdx,eucDist=eucDist,tvlDist=rtDist,tvlTime=rtTime))
    
    # now the nonseed passes the agglomeratable test, agglomerate it to targetSeed by updatint its aggidx
    adm2@data[nonSeedIdx,"aggidx"] = adm2@data[targetSeedIdx,"aggidx"]
    adm2@data[nonSeedIdx,"aggidx2"] = adm2@data[targetSeedIdx,"aggidx2"]
    debugPrint(sprintf("========================== [%i] is agglomerated to [%i]", nonSeedIdx, targetSeedIdx))
    
  }
  
  # if no valid routes are found for the input parameters, exit process
  if(nrow(routeInfoDF)==0){
    print("====== no valid route found between seeds and nonseeds, the selection criteria is too harsh =====")
    return()
  }
  
  # if valid routes are found for the input parameters, try to correct the issue records if exist
  
  if(nrow(routeInfoDF)>0){
    
    # handle nonseeds whose agglomeratability cannot be determined yet by estimating travel time using simple equation
    # based on the known Euclidian distance and travel time between centroids in that country,  the estimated travel time is : eucDist * sumTvlTime / sumEucDist
    filter = routeInfoDF[,"tvlTime"] < 0
    restnonseedDF = routeInfoDF[filter,]
    
    flag_restnonseed_exist = FALSE
    if (!is.null(nrow(restnonseedDF))){
      if (nrow(restnonseedDF) > 0) {
        flag_restnonseed_exist = TRUE
      }
    }
    
    if (flag_restnonseed_exist){
      sumEucDist = sum(routeInfoDF[!filter,"eucDist"])
      sumTvlTime = sum(routeInfoDF[!filter,"tvlTime"])
      if (sumEucDist > 0 ){
        ratio = sumTvlTime / sumEucDist
        for (i in 1:length(restnonseedDF[[1]])){
          if (restnonseedDF[i,"eucDist"] * ratio >= th_tvltime){
            debugPrint("-- ignored (too long to travel ---- final decision) ")
            next
          } else {
            nonSeedIdx = restnonseedDF[i,"nonSeedIdx"]
            targetSeedIdx = restnonseedDF[i,"targetSeedIdx"]
            
            adm2@data[nonSeedIdx,"aggidx"] = adm2@data[targetSeedIdx,"aggidx"]
            adm2@data[nonSeedIdx,"aggidx2"] = adm2@data[targetSeedIdx,"aggidx2"]
            debugPrint(sprintf("========================== [%i] is agglomerated to [%i] ---- final decision", nonSeedIdx, targetSeedIdx))
          }
        }
      }
    }
    
  }
  else{
    # if no valid routes are found, the AI is directly computed based on seeds or merged seeds
    print("====== no valid route found between seeds and nonseeds, AI is directly computed based on seeds or merged seeds=====")
  }
  
  # calc AI based on seed
  rltDataFrame = data.frame(seedIdx=NULL,districtName=NULL,AI=NULL,sumPop=NULL, sumArea=NULL)
  df = adm2@data[seedFilter,]
  for (i in 1:nrow(df)){
    seedIdx = df[i, "orgSeqId"] #attr(df[i,], "row.names")
    districtName = df[i, districtDataColName]
    filter = adm2@data[, "aggidx"] == seedIdx
    sumPop = sum(adm2@data[filter, popDataColName])
    sumArea =  sum(adm2@data[filter, areaDataColName])
    AI = sumPop / ttlPopulation
    rltDataFrame<-rbind(rltDataFrame,data.frame(seedIdx=seedIdx, districtName=districtName, AI=AI, sumPop=sumPop, sumArea=sumArea))
  }
  #rltDataFrame$AI = format(rltDataFrame$AI, nsmall=4, digits=4, scientific = FALSE)
  write.table(rltDataFrame, file=sprintf("%s/AI_%i_%i_%i_orgseed.csv", outShpFileDir, thPopDen, thSeedMinPop, thTvlTime), row.names = FALSE, sep=",")
  
  
  # calc AI based on seed2 (agglomerated seed)
  rltDataFrame = data.frame(seedIdx=NULL,districtName=NULL,AI=NULL,sumPop=NULL, sumArea=NULL)
  df = adm2@data[aggSeedFilter,]
  for (i in 1:nrow(df)){
    seedIdx = df[i, "orgSeqId"]
    districtName = df[i, districtDataColName]
    filter = adm2@data[, "aggidx2"] == seedIdx
    sumPop = sum(adm2@data[filter, popDataColName])
    sumArea =  sum(adm2@data[filter, areaDataColName])
    AI = sumPop / ttlPopulation
    rltDataFrame<-rbind(rltDataFrame,data.frame(seedIdx=seedIdx, districtName=districtName, AI=AI, sumPop=sumPop, sumArea=sumArea))
  }
  
  #rltDataFrame$AI = format(rltDataFrame$AI, nsmall=4, digits=4, scientific = FALSE)
  write.table(rltDataFrame, file=sprintf("%s/AI_%i_%i_%i_aggseed.csv", outShpFileDir, thPopDen, thSeedMinPop, thTvlTime), row.names = FALSE, sep=",")
  
  #enforce data type as integer
  adm2@data[,"isseed"] = as.integer(adm2@data[,"isseed"])
  adm2@data[,"isseed2"] = as.integer(adm2@data[,"isseed2"])
  adm2@data[,"aggidx"] = as.integer(adm2@data[,"aggidx"])
  adm2@data[,"aggidx2"] = as.integer(adm2@data[,"aggidx2"])
  
  writeOGR(obj=adm2, dsn=outShpFileDir, layer=sprintf("AI_%i_%i_%i", thPopDen, thSeedMinPop, thTvlTime), driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
  
  #attach region infor to csv outputs
  AI.attachRegionInfo(dsn=dsn,outDir=gOutRootDir,
                      polyFileName=polyFileName, 
                      thPopDen=thPopDen, 
                      thSeedMinPop=thSeedMinPop,
                      thTvlTime=thTvlTime, 
                      seedDirName=seedDirName,
                      countryName=gCountryName,
                      dataYear=gDataYear)
  
  # save routeInfo
  RouteInfo.save(routeInfoFilePath)
  
  algEndTime = Sys.time()
  
  # print GG key usage:
  print("===========GG Key Usage Start===========")
  for(i in 1:length(gGoogleAPIKeysRequestCounter)){
    print(sprintf("keyreq:%i  key:%s", gGoogleAPIKeysRequestCounter[i],gGoogleAPIKeys[i]))
  }
  print(sprintf("ttlreq:%i", gGoogleRequestCounter))
  print("===========GG Key Usage END===========")
  
  print(sprintf("==========> all done (in %.2f seconds) <==========", as.numeric(algEndTime-algStartTime, units="secs")))
}

AI.attachRegionInfo <-function(dsn="data\\CHN_adm", 
                               outDir=gOutRootDir,
                               polyFileName="China_adm2", 
                               thPopDen=400, 
                               thSeedMinPop=1000000,
                               thTvlTime=90, 
                               seedDirName="ByKeyCities_SeedMergeDist90",
                               countryName=gCountryName,
                               dataYear=gDataYear
                               ){
  
  outShpFileDir = sprintf("%s%s/%s/%s",outDir, countryName, dataYear, seedDirName)
  
  adm2 <- readOGR(dsn=dsn,layer=polyFileName,encoding="utf8", verbose=FALSE)
  # using orgSeqId filed to ensure that the merged data can match polygons
  adm2@data[,"orgSeqId"] = c(1:nrow(adm2@data))
  
  keycities <- readOGR(dsn=sprintf("%s\\%s",dsn,"7-city-regions"),layer="China_adm2_SSHan_KeyCities",encoding="utf8", verbose=FALSE)
  
  fileTypes = c("aggseed","orgseed")
  
  for(k in 1:length(fileTypes)){
    dataTable = read.table(sprintf("%s/AI_%i_%i_%i_%s.csv", outShpFileDir, thPopDen, thSeedMinPop, thTvlTime, fileTypes[k]), header=TRUE, sep=",", blank.lines.skip = TRUE, fill = TRUE)
    #get id for each row in dataTable from original shp file
    filter = adm2@data[, "orgSeqId"] %in% dataTable[, "seedIdx"]
    
    dataTable[,"districId"] = adm2@data[filter, c("id")]
    dataTable[,"cityRegion"]= NULL
    #get region info from "China_adm2_SSHan_KeyCities.shp", linked by id
    
    for(i in 1:nrow(dataTable)){
      filter = keycities@data[,"id"] %in% dataTable[i,"districId"]
      if(nrow(keycities@data[filter,])>0){
        dataTable[i,"cityRegion"] = keycities@data[filter,"cityregion"]
      }
    }
    
    write.table(dataTable, file=sprintf("%s/AI_%i_%i_%i_%s_region.csv", outShpFileDir, thPopDen, thSeedMinPop, thTvlTime, fileTypes[k]), row.names = FALSE, sep=",")
    
  }
 
}

# generate Agglomeration Index based on given data sets
AI.gen <- function(dsn="./data/LKA_adm/", 
                    popFileName=sprintf("pop_%s.csv", gDataYear), 
                    polyFileName="LKA_adm2", 
                    admtypeFileName="admin_type.csv", 
                    outDir=gOutRootDir,
                    thPopDen=150, 
                    thSeedMinPop=50000, 
                    thTvlTime=60, 
                    thSearchSeedRadius=200, 
                    thGeoCodingDistShift=3, 
                    ttlPop=gCountryTotalPop, 
                    popMatchColName="divname", 
                    polyMatchColName="NAME_2", 
                    admtypeMatchColName="DSDivision", 
                    popDataColName="popvalue",
                    admtypeDataColName="Admin_Type", 
                    seedAsAdmTypeVec=c("Urban Council","Municipal Council"),
                    seedAsTopNHighPopArea = 40,
                    seedDirName="ByUcMc",
                    countryName=gCountryName,
                    dataYear=gDataYear,
                    seedMergeDist=10,
                    seedMergeTo="highestPop",
                    seedPopFileName=sprintf("seedpop%s.csv", gDataYear),
                    thSeedNameSimilarity=0.98,
                    useDivisionAsSeedIfNoNameMatch=FALSE
                    ){
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # Input parameters:
  # (1) dsn: the root directory of data sets
  # (2) popFileName: population data (csv or txt) file name.
  # (3) polyFileName: administrative boundary shp file name.
  # (4) admtypeFileName: administrative type data file name, it can be ignored if the administrative type information is already in the "polyFileName" data
  # (5) outDir: the root directory of calculation outputs
  # (6) thPopDen: threshold of population density. A non-seed area is only considered as agglomeratable if its popden is bigger than this threshold. unit: the number of people in 1 square km
  # (7) thSeedMinPop: threshold of population. A seed area is only considered and used as an agglomeration core if its population is larger than this threshold. 
  # (8) thTvlTime: threshold of travel time between seed and non-seed districts. unit: minute
  # (9) thSearchSeedRadius: threshold of the Euclidian distance between a non-seed and its potential seeds. Those too distant seeds will be excluded from the gravity calculation.
  # (10) thGeoCodingDistShift: threshold of the Euclidian distance between the original point and reverse geocoding point. If the reverse geocoding point is too distant away from the original one, it will not be used.
  # (11) ttlPop: total population of the target country
  # (12) popMatchColName: a column name in "popFileName" data for matching (joining) with "polyFileName" data 
  # (13) polyMatchColName: a column name in "polyFileName" data for matching (joining) with "popFileName" data
  # (14) admtypeMatchColName: a column name in "admtypeFileName" data for matching (joining) with "polyFileName" data
  # (15) popDataColName: the name of a column in "popFileName" data containing the population data for AI calculation
  # (16) admtypeDataColName: the name of a column in "admtypeFileName" (or ""polyFileName) data containing the administrative type data for seed selection
  # (17) seedAsAdmTypeVec: a vector of administrative types. If not empty, seeds will be selected based these administrative types. Default is empty. It can be c("Urban Council","Municipal Council","Pradeshiya Sabha") or c("Pradeshiya Sabha")
  # (11) seedAsTopNHighPopArea: the number of top population areas to be selected as seeds. Default value is -1, which means all qualified seeds will be in use.
  # (19) seedDirName: for different seed selections, the result will be put into different output directories This is the directory name for a particular seed selection. It can be any valid and meaningful folder name, no whitespace in it.
  # (20) countryName: the name of the country. It is used as a part of the outputs path
  # (21) dataYear: the year of population data.  It is used as a part of the outputs path
  # (22) seedMergeDist: -1: all closest seeds pairs are mergeable; 0 (default): use the median value of all seeds Eculidian distance as a threshold and only those seed pairs whose distance is smaller than this threshold are mergeable; or other positive value (Unit: km)
  # (23) seedMergeTo: "smallestIdx": all mergeable seeds will be merged to the seed whose idx is the smallest; "highestPop"(default): all mergeable seeds will be merged to the seed whose population is the highest; "highestPopDen": all mergeable seeds will be merged to the seed whose population density is the highest
  # (24) seedPopFileName: real seed population file name. if both seedAsAdmTypeVec and seedPopFileName are provided, the thSeedMinPop will be further applied on real seed population in the data file. The previously found seed will be marked as non-seed if the real population is smaller than the threshold.
  # (25) thSeedNameSimilarity: threshold of similarity between divisonname (polyMatchColName) in shape file and the "seedName" in seedPopFileName. range from 0-1. 1: 100% similarity applied.
  # (26) useDivisionAsSeedIfNoNameMatch: in the case of no matches found between divisonname and "seedName", if FALSE, the previously found seed will be marked as non-seed; if TRUE, the seed remains.
  # Output:
  # (1) shp files containing agglomeration index information
  # (2) a calculated route data folder ("routeinfo") containing raw route request api results
  # (3) a calculated inner centroid data file ("innerCentroids.csv") 
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #

algStartTime = Sys.time()
  
# create a folder to hold route information 
routeInfoDir = sprintf("%s%s/%s/",outDir,countryName,"routeinfo")
dir.create(routeInfoDir, showWarnings=FALSE, recursive=TRUE)
routeInfoFilePath = sprintf("%s%s",routeInfoDir,"db.csv")

# load route info into gRouteInfoDF
RouteInfo.load(routeInfoFilePath)

# create centroid file path
centroidFilePath = sprintf("%s%s/%s",outDir,countryName,"innerCentroids.csv")

# create a folder to AI results (shp files) for different seed choices
outShpFileDir = sprintf("%s%s/%s/%s",outDir, countryName, dataYear, seedDirName)
dir.create(outShpFileDir, showWarnings=FALSE, recursive=TRUE)

# set TRUE to enable seeds agglomeration 
isSeedAgglomeratable = TRUE

# set the total popluation of the country
ttlPopulation = ttlPop

# define thresholds 
# POP DENSITY: run an AI at each of these minimum levels: 150, 300, and 500 people per square kilometre
# TRAVEL TIME: run an AI at each of these maximum levels: 30, 60, and 90 minutes
# MINIMUM SEED POPULATION: run an AI at each of these minimum levels: 20,000, 50,000, 100,000, and 500,000.
th_popden = thPopDen
th_tvltime = thTvlTime * 60
th_seed_minpop = thSeedMinPop # the minimum population to be considered as a seed

# Eclidian distance to search seeds around a non-seed. Unit: meter. It is used first to reduce real distance computation
th_dist = thSearchSeedRadius * 1000
# the maximum Euclidian distance between an orginal point and its reverse geocoding point, if the distance bigger than this threshold, the reverse geocoding point will NOT be used
th_geocoding_dist_shift = thGeoCodingDistShift * 1000 

# load population data
popTable = read.table(sprintf("%s%s",dsn,popFileName), header=TRUE, sep=",", blank.lines.skip = TRUE, fill = TRUE)

# check if admtype file is provided, if yes, load it
if (nchar(admtypeFileName) > 0){
  con <- file(sprintf("%s%s",dsn,admtypeFileName), open="r")
  admintype = read.table(con, header=TRUE, sep=",", blank.lines.skip = TRUE, fill = TRUE)
  close(con)
}

# load adm boundary polygons
admRaw <- readOGR(dsn=dsn,layer=polyFileName,encoding="utf8", verbose=FALSE)

# check if the provided shp file is in projected or unporjected CRS
if (is.projected(admRaw)){
  adm2_pj = admRaw
  adm2 = spTransform(admRaw,CRS(CONST_unprojected_proj4string))
} else {
  adm2 = admRaw
  adm2_pj = spTransform(admRaw,CRS(CONST_projected_proj4string))
}

# choose part of data of interest, currently take all columns 
adm2_data = adm2@data

# calc area of each district, unit km^2
area = NULL
for (idx in 1:length(adm2_pj@polygons)){
  area = c(area, adm2_pj@polygons[[idx]]@area/1000000)
}

# using orgSeqId filed to ensure that the merged data can match polygons
adm2_data[,"orgSeqId"] = c(1:nrow(adm2_data))
joinedDF = merge(adm2_data, popTable, by.x = polyMatchColName, by.y = popMatchColName, all.x=TRUE, sort=FALSE)

# check if admtype file is provided, if yes, merge it with population data
if (nchar(admtypeFileName) > 0){
  joinedDF = merge(joinedDF, admintype, by.x = polyMatchColName, by.y = admtypeMatchColName, all.x=TRUE, sort=FALSE)
}

# reorder dataframe by orgSeqId so attribute data can match polygons 
orderedJoinedDF = joinedDF[order(joinedDF[,"orgSeqId"]), ]
joinedDF = orderedJoinedDF

# reset row.names as orgSeqId, VERY IMPORTANT
row.names(joinedDF) = as.vector(joinedDF[,"orgSeqId"])

# add area (km^2) and population desnsity (people per km^2) to dataframe
joinedDF[,"area"] = area
joinedDF[,"popden"] = joinedDF[,popDataColName] / joinedDF[,"area"]

# add additional columns : isseed (0:not seed, 1:seed), aggidx (default 0), aggidx2 (default 0, when seed agglomeration applied) 
joinedDF[,"isseed"] = 0
joinedDF[,"aggidx"] = 0
joinedDF[,"isseed2"] = 0 # is an agglomerated seed
joinedDF[,"aggidx2"] = 0

adm2@data = joinedDF

# load the centroids for use
centroidList = InnerCentroids.load(centroidFilePath,adm2)
centroidList_pj = list()
for (i in 1:length(centroidList)){
  centroidList_pj[[i]] = spTransform(centroidList[[i]],CRS(CONST_projected_proj4string))
}

# find seeds
# choose seed by admType or by top N population districts
if (length(seedAsAdmTypeVec) > 0){
  seedFilter = joinedDF[, admtypeDataColName] %in% seedAsAdmTypeVec 
  seedFilter = ifelse(is.na(seedFilter), FALSE, seedFilter)
  
  # if real seed population file is provided, double check the seed population threshold
  if (nchar(seedPopFileName) > 0){
    seedPopTable = read.table(sprintf("%s%s",dsn,seedPopFileName), header=TRUE, sep=",", na.strings=c("?","..."), blank.lines.skip = TRUE, fill = TRUE)
    seedPopTable[is.na(seedPopTable[,"seedPop"]),"seedPop"] = 0

    for(filterIdx in 1:length(seedFilter)){
      # ignore those already be filtered
      if(seedFilter[filterIdx]==FALSE) next

      # get current dsdivision name
      tmpSeedName = as.character(joinedDF[filterIdx, polyMatchColName])
      
      # match the "dsdivision name" with the "seedName" in seedpopfile
      matchVec = jarowinkler(tmpSeedName,as.character(seedPopTable[,"seedName"]))
      
      # find the best match: max(matchVec)
      filter= matchVec==max(matchVec)
      #print(sprintf("maxmatch between [%s] - [%s] (%.4f)", tmpSeedName, as.character(seedPopTable[filter,"seedName"]), max(matchVec)))
      
      # if the similarity between two names is high enough, do the further test, 
      if(max(matchVec) >= thSeedNameSimilarity){

        seedpop = as.integer(seedPopTable[filter,"seedPop"])
        if(seedpop[1] <= th_seed_minpop){
          #print("====ignored: real seed population is too small")
          # ignore this seed since the real population is smaller than threshold
          seedFilter[filterIdx] = FALSE
        } 
      } else {
        # if no matched name found, igonre/keep the seed based on useDivisionAsSeedIfNoNameMatch flag 
        #print("====ignored: no valid match found")
        if(useDivisionAsSeedIfNoNameMatch == FALSE) seedFilter[filterIdx] = FALSE
      }
    }
  } else {
    
    seedPopFilter = joinedDF[, popDataColName] > th_seed_minpop
    seedFilter = seedPopFilter & seedFilter
  }
  
} else {
  
  seedPopFilter = joinedDF[, popDataColName] > th_seed_minpop
  # choose seed by top N population districts
  topN = seedAsTopNHighPopArea
  
  if ((topN > nrow(joinedDF)) | (topN <= 0)) topN = nrow(joinedDF)
  
  tmpOrdered = as.vector(joinedDF[ order(-joinedDF[,popDataColName]), ][1:topN, polyMatchColName])
  seedFilter = seedPopFilter & joinedDF[, polyMatchColName] %in% tmpOrdered
  seedFilter = ifelse(is.na(seedFilter), FALSE, seedFilter)
}

seedDF = adm2@data[seedFilter,]
nonseedDF = adm2@data[!seedFilter,]

flag_seed_exist = FALSE
if (!is.null(nrow(seedDF))){
  if (nrow(seedDF) > 0) {
    flag_seed_exist = TRUE
  }
}
if (!flag_seed_exist){
  print("====== no valid seeds found, the selection criteria is too harsh =====")
  return()
}

flag_nonseed_exist = FALSE
if (!is.null(nrow(nonseedDF))){
  if (nrow(nonseedDF) > 0) {
    flag_nonseed_exist = TRUE
  }
}
if (!flag_nonseed_exist){
  print("====== no valid nonseeds found, the selection criteria is too harsh =====")
  return()
}


adm2@data[seedFilter,"isseed"] = 1 # set isseed flag, and mark seed's aggidx as it rowname
adm2@data[seedFilter,"isseed2"] = 0 # set agglomerated seed index will be assigned later
adm2@data[seedFilter,"aggidx"] = adm2@data[seedFilter,"orgSeqId"] #attr(adm2@data[seedFilter,], "row.names")
adm2@data[seedFilter,"aggidx2"] = adm2@data[seedFilter,"orgSeqId"] #attr(adm2@data[seedFilter,], "row.names")

# agglomarate seed if required
if (isSeedAgglomeratable == TRUE){
  
  aggSeedList = list()
  for(i in 1:length(adm2@data[seedFilter,"aggidx"])){
    aggSeedList[[i]] = c(adm2@data[seedFilter,"aggidx"][i])
  }
  
  testList  = aggSeedList
  
  # this flag controls the outer while-loop, making sure that all seeds are properly aggregated. 
  # the inner two nested for-loops start from the head of testList, testing distance between seeds from two seed groups:seeds-m and seeds-n 
  # each seed group contains one or more seeds. seeds in a same seed group will eventually aggregate as a single seed.
  # if, for example, a seed (seeds-n-1) in group seeds-n is found within seedMergeDist of any one of seed in group seeds-m, all seeds in group seeds-n will be merged to group seeds-m.
  # In this case, since the seed groups are updated (seeds-m gets expanded,and seeds-n get destroied), we need to restart the outer while-loop again and continue testing distance between the updated seed groups
  reloopflag = TRUE
  
  while(reloopflag){
  # merge seed by "minimaldist" method: two closest seeds will be merged if their distance is within the seedMergeDist; 
    for(n in 1:length(testList)){
      reloopflag = FALSE
      if(length(testList[[n]])==1 & testList[[n]][1]==-1){
        next
      }
      
      for(m in 1:length(testList)){
        reloopflag = FALSE
        if (m == n){
          next
        }
        
        if (length(testList[[m]])==1 & testList[[m]][1]==-1){
          next
        }
        
        tmpMinDist = 999999999
        for(p in 1:length(testList[[n]])){  
          
          for(q in 1:length(testList[[m]])){
            
            dist = gDistance(centroidList_pj[[testList[[n]][p]]], centroidList_pj[[testList[[m]][q]]])
            
            if(dist<tmpMinDist){
              tmpMinDist = dist
            }
          }
        }
        
        #if mindist is smaller than threshold, merge m to n, and clear m          
        if(tmpMinDist < seedMergeDist*1000){
          testList[[n]] = c(testList[[n]], testList[[m]])
          testList[[m]] = c(-1)
          reloopflag = TRUE
          break
        }
        
      }
      if (reloopflag){
        break
      }
    }
  }
    
  aggSeedList = testList
  
  # agglomerate seed
  for (i in 1:length(aggSeedList)){
    
    # skip cleared list element
    if (length(aggSeedList[[i]])==1 &aggSeedList[[i]][1]==-1){
      next
    }
    
    # by default, merge to the smallest idx seed
    aggSeedIdx = min(aggSeedList[[i]])
    
    # merge to the highest population seed
    highestPopValue = -1
    for (tmpIdx in aggSeedList[[i]]){
      if (adm2@data[tmpIdx, popDataColName] > highestPopValue){
        aggSeedIdx = tmpIdx
        highestPopValue = adm2@data[tmpIdx, popDataColName]
      }
    }
    
    vec = aggSeedList[[i]]
    vec = vec[which(vec!=aggSeedIdx)]
    adm2@data[vec,"aggidx2"] = adm2@data[aggSeedIdx,"aggidx2"]
    
  }
}

# mark isseed2 column
aggSeedIdxVec = adm2@data[,"aggidx2"]
aggSeedIdxVec = aggSeedIdxVec[!duplicated(aggSeedIdxVec) & (aggSeedIdxVec > 0)]
adm2@data[aggSeedIdxVec,"isseed2"] = 1
aggSeedFilter = adm2@data[,"isseed2"] == 1

#route information data frame to record 
routeInfoDF = data.frame(nonSeedIdx=NULL,targetSeedIdx=NULL,eucDist=NULL,tvlDist=NULL,tvlTime=NULL)

for(i in gLoopStartIndex:nrow(nonseedDF)){
  
  nonSeedIdx = nonseedDF[i,"orgSeqId"] #attr(nonseedDF[i,], "row.names")
  debugPrint(sprintf("[%i/%i]=====nonseedidx: %i", i, nrow(nonseedDF), nonSeedIdx))
  
  # if a nonseed area has low pop density than threshold, it is not agglomeratable, ignore it
  if (is.na(nonseedDF[i,"popden"])) {
    debugPrint("-- ignored (NA)")
    next
  }
  
  if (nonseedDF[i,"popden"] < th_popden) {
    debugPrint("-- ignored (popden too low)")
    next
  }
  
  s_lat = centroidList[[nonSeedIdx]]@coords[2]
  s_lon = centroidList[[nonSeedIdx]]@coords[1]
  
  targetSeedIdx = -1
  maxGravity = -1
  rtDist = -1
  rtTime = -1
  eucDist = -1
  # find the closest seed using gravity
  for(j in 1:nrow(seedDF)){
    seedIdx = seedDF[j,"orgSeqId"] #attr(seedDF[j,], "row.names")
    dist = gDistance(centroidList_pj[[nonSeedIdx]], centroidList_pj[[seedIdx]])
    if (dist >= th_dist) next

    # calc and find the max gravity
    gravity = seedDF[j,popDataColName] / (dist * dist)
    if (maxGravity < gravity){
      targetSeedIdx = seedIdx
      maxGravity = gravity
      eucDist = dist / 1000
    }  
  }
  
  # if cannot find a seed around it, igore it
  if (targetSeedIdx < 0){
    debugPrint("-- ignored (cannot find an adjacent seed )")
    next
  }
  
  e_lat = centroidList[[targetSeedIdx]]@coords[2]
  e_lon = centroidList[[targetSeedIdx]]@coords[1]
  
  # test if route infomation already exists in gRouteInfoDF, if yes, fetch travel distance and time directly, otherwise, call apis
  cachedRouteInfo = RouteInfo.query(nonSeedIdx, targetSeedIdx)
  flag_cached_found = FALSE
  if (!is.null(nrow(cachedRouteInfo))){
    if (nrow(cachedRouteInfo) > 0) {
      flag_cached_found = TRUE
    }
  }
  if (flag_cached_found){
    rtTime = cachedRouteInfo[1,"tvlTime"]
    rtDist = cachedRouteInfo[1,"tvlDist"]
    debugPrint("++++++++++++++ cached route info applied ++++++++++++++++")
  } else {
    
    # if gUseCachedRouteDBOnly is TURE and we cannot find route in cached DB, mark to be further corrected
    if (gUseCachedRouteDBOnly){
      debugPrint(sprintf("-- ignored (no route found in cached DB between [%.6f,%.6f] - [%.6f,%.6f] )",s_lat,s_lon,e_lat,e_lon))
      routeInfoDF<-rbind(routeInfoDF,data.frame(nonSeedIdx=nonSeedIdx, targetSeedIdx=targetSeedIdx, eucDist=eucDist, tvlDist=-1, tvlTime=-1))      
      next
    }
    
    MQDirectURL = sprintf(MapQuestDirectURLTemplate,s_lat,s_lon,e_lat,e_lon)
    #add keystring to mapquest request
    MQDirectURL = paste(MQDirectURL,MapQuestKeyString,sep="")
    GGDirectURL = sprintf(GoogleDirectURLTemplate,s_lat,s_lon,e_lat,e_lon)
    # MQRawJson is a list contains two attributes : route (list), info (list) . #attributes(MQRawJson)
    mqjsonstr = getURL(MQDirectURL)
    MQRawJson = fromJSON(mqjsonstr)
    
    # if cannot find a route with mapquest, try google
    if (MQRawJson$info$statuscode !=0 ){
      #
      ggjsonstr = getURL(GGDirectURL, .mapUnicode = FALSE)
      GGRawJson = fromJSON(ggjsonstr)
      if (GGRawJson$status == "OK"){
        debugPrint("*********** route find in google api *************")
        rtDist = GGRawJson$route[[1]]$legs[[1]]$distance$value / 1000
        rtTime = GGRawJson$route[[1]]$legs[[1]]$duration$value
        
        # test if big TER exists, if does, mark to be further corrected 
        if (rtDist/eucDist > gTERMax) {   
          debugPrint(sprintf("-- ignored (too big TER found between [%.6f,%.6f] - [%.6f,%.6f] )",s_lat,s_lon,e_lat,e_lon))
          routeInfoDF<-rbind(routeInfoDF,data.frame(nonSeedIdx=nonSeedIdx, targetSeedIdx=targetSeedIdx, eucDist=eucDist, tvlDist=-1, tvlTime=-1))
          next
        }
        
        # save google route info to file
        routeJsonFilePath = sprintf("%s%s_%i_%i.json",routeInfoDir,"GG",nonSeedIdx, targetSeedIdx)
        write(ggjsonstr,routeJsonFilePath)
        RouteInfo.append(nonSeedIdx,targetSeedIdx,eucDist,rtDist,rtTime,"GG",routeJsonFilePath, routeInfoFilePath)
        
      } else {
        # if route cannot be found, try to using reverse geocoding api to re-generate both start and end location for new route query
        # try to find new start location
        isNewLocationApplied = FALSE
        isNewRouteFound = FALSE
        GGGeoCodingURL = sprintf(GoogleGeoCodingURLTemplate,s_lat,s_lon)
        GGGeoCodingRawJson = fromJSON(getURL(GGGeoCodingURL, .mapUnicode = FALSE))
        if (GGGeoCodingRawJson$status == "OK"){
          new_s_lat = GGGeoCodingRawJson$results[[1]]$geometry$location$lat
          new_s_lon = GGGeoCodingRawJson$results[[1]]$geometry$location$lng
          # test new location is close enough to the original location
          tmpDist = GeoTool.getLatLonDistance( new_s_lat, new_s_lon, s_lat, s_lon)
          if (tmpDist < th_geocoding_dist_shift) {
            # new location is not far away from origial one, use it
            s_lat = new_s_lat
            s_lon = new_s_lon
            isNewLocationApplied = TRUE
          }
        }
        
        # try to find new end location
        GGGeoCodingURL = sprintf(GoogleGeoCodingURLTemplate,e_lat,e_lon)
        GGGeoCodingRawJson = fromJSON(getURL(GGGeoCodingURL, .mapUnicode = FALSE))
        if (GGGeoCodingRawJson$status == "OK"){
          new_e_lat = GGGeoCodingRawJson$results[[1]]$geometry$location$lat
          new_e_lon = GGGeoCodingRawJson$results[[1]]$geometry$location$lng
          # test new location is close enough to the original location
          tmpDist = GeoTool.getLatLonDistance( new_e_lat, new_e_lon, e_lat, e_lon)
          if (tmpDist < th_geocoding_dist_shift) {
            # new location is not far away from origial one, use it
            e_lat = new_e_lat
            e_lon = new_e_lon
            isNewLocationApplied = TRUE
          }
        }
        
        # if either location is update, give it a chance to query the route again 
        if (isNewLocationApplied == TRUE){   
          GGDirectURL = sprintf(GoogleDirectURLTemplate,s_lat,s_lon,e_lat,e_lon)
          ggjsonstr = getURL(GGDirectURL, .mapUnicode = FALSE)
          GGRawJson = fromJSON(ggjsonstr)
          if (GGRawJson$status == "OK"){
            debugPrint("*********** route find in google api === after reverse geocoding *************")
            rtDist = GGRawJson$route[[1]]$legs[[1]]$distance$value / 1000
            rtTime = GGRawJson$route[[1]]$legs[[1]]$duration$value
            
            
            # test if big TER exists, if does, mark to be further corrected 
            if (rtDist/eucDist > gTERMax) {   
              debugPrint(sprintf("-- ignored (too big TER found between [%.6f,%.6f] - [%.6f,%.6f] )",s_lat,s_lon,e_lat,e_lon))
              routeInfoDF<-rbind(routeInfoDF,data.frame(nonSeedIdx=nonSeedIdx, targetSeedIdx=targetSeedIdx, eucDist=eucDist, tvlDist=-1, tvlTime=-1))
              next
            }
            
            # save google route info to file
            routeJsonFilePath = sprintf("%s%s_%i_%i.json",routeInfoDir,"GG",nonSeedIdx, targetSeedIdx)
            write(ggjsonstr,routeJsonFilePath)
            RouteInfo.append(nonSeedIdx,targetSeedIdx,eucDist,rtDist,rtTime,"GG",routeJsonFilePath, routeInfoFilePath)
            
            isNewRouteFound = TRUE
          }
        }
        
        # if still cannot find a route, leave it (whose tvlTime will be set to -1) for further travel time estimation processing
        if (isNewRouteFound == FALSE) {   
          debugPrint(sprintf("-- ignored (no route found between [%.6f,%.6f] - [%.6f,%.6f] )",s_lat,s_lon,e_lat,e_lon))
          routeInfoDF<-rbind(routeInfoDF,data.frame(nonSeedIdx=nonSeedIdx, targetSeedIdx=targetSeedIdx, eucDist=eucDist, tvlDist=rtDist, tvlTime=rtTime))
          next
        }
      }
      
    } else {
      route = MQRawJson$route
      rtDist = route$distance #unit in km
      rtTime = route$time #unit in second
      
      # test if big TER exists, if does, mark to be further corrected 
      if (rtDist/eucDist > gTERMax) {   
        debugPrint(sprintf("-- ignored (too big TER found between [%.6f,%.6f] - [%.6f,%.6f] )",s_lat,s_lon,e_lat,e_lon))
        routeInfoDF<-rbind(routeInfoDF,data.frame(nonSeedIdx=nonSeedIdx, targetSeedIdx=targetSeedIdx, eucDist=eucDist, tvlDist=-1, tvlTime=-1))
        next
      }
      
      # save mapquest route info to file
      routeJsonFilePath = sprintf("%s%s_%i_%i.json",routeInfoDir,"MQ",nonSeedIdx, targetSeedIdx)
      write(mqjsonstr,routeJsonFilePath)
      RouteInfo.append(nonSeedIdx, targetSeedIdx, eucDist, rtDist,rtTime, "MQ", routeJsonFilePath, routeInfoFilePath)
    }
  }
  
  # test if the real travel time to target seed is short enough  
  if (rtTime >= th_tvltime){
    debugPrint(sprintf("-- ignored (too long to travel tvlTime:%.0f, tvlDist:%.3f )",rtTime, rtDist))
    next
  }
  
  # save the route infomation if a nonseed can be agglomerated to a seed 
  routeInfoDF<-rbind(routeInfoDF,data.frame(nonSeedIdx=nonSeedIdx,targetSeedIdx=targetSeedIdx,eucDist=eucDist,tvlDist=rtDist,tvlTime=rtTime))
  
  # now the nonseed passes the agglomeratable test, agglomerate it to targetSeed by updatint its aggidx
  adm2@data[nonSeedIdx,"aggidx"] = adm2@data[targetSeedIdx,"aggidx"]
  adm2@data[nonSeedIdx,"aggidx2"] = adm2@data[targetSeedIdx,"aggidx2"]
  debugPrint(sprintf("========================== [%i] is agglomerated to [%i]", nonSeedIdx, targetSeedIdx))
  
}

# if no valid routes are found for the input parameters, exit process
if(nrow(routeInfoDF)==0){
  print("====== no valid route found between seeds and nonseeds, the selection criteria is too harsh =====")
  return()
}

# if valid routes are found for the input parameters, try to correct the issue records if exist

if(nrow(routeInfoDF)>0){
  
  # handle nonseeds whose agglomeratability cannot be determined yet by estimating travel time using simple equation
  # based on the known Euclidian distance and travel time between centroids in that country,  the estimated travel time is : eucDist * sumTvlTime / sumEucDist
  filter = routeInfoDF[,"tvlTime"] < 0
  restnonseedDF = routeInfoDF[filter,]
  
  flag_restnonseed_exist = FALSE
  if (!is.null(nrow(restnonseedDF))){
    if (nrow(restnonseedDF) > 0) {
      flag_restnonseed_exist = TRUE
    }
  }
  
  if (flag_restnonseed_exist){
    sumEucDist = sum(routeInfoDF[!filter,"eucDist"])
    sumTvlTime = sum(routeInfoDF[!filter,"tvlTime"])
    if (sumEucDist > 0 ){
      ratio = sumTvlTime / sumEucDist
      for (i in 1:length(restnonseedDF[[1]])){
        if (restnonseedDF[i,"eucDist"] * ratio >= th_tvltime){
          debugPrint("-- ignored (too long to travel ---- final decision) ")
          next
        } else {
          nonSeedIdx = restnonseedDF[i,"nonSeedIdx"]
          targetSeedIdx = restnonseedDF[i,"targetSeedIdx"]
          
          adm2@data[nonSeedIdx,"aggidx"] = adm2@data[targetSeedIdx,"aggidx"]
          adm2@data[nonSeedIdx,"aggidx2"] = adm2@data[targetSeedIdx,"aggidx2"]
          debugPrint(sprintf("========================== [%i] is agglomerated to [%i] ---- final decision", nonSeedIdx, targetSeedIdx))
        }
      }
    }
  }
  
}
else{
  # if no valid routes are found, the AI is directly computed based on seeds or merged seeds
  print("====== no valid route found between seeds and nonseeds, AI is directly computed based on seeds or merged seeds=====")
}

# calc AI based on seed
rltDataFrame = data.frame(seedIdx=NULL,districtName=NULL,AI=NULL,sumPop=NULL, sumArea=NULL)
df = adm2@data[seedFilter,]
for (i in 1:nrow(df)){
  seedIdx = df[i, "orgSeqId"] #attr(df[i,], "row.names")
  districtName = df[i, polyMatchColName]
  filter = adm2@data[, "aggidx"] == seedIdx
  sumPop = sum(adm2@data[filter, popDataColName])
  sumArea =  sum(adm2@data[filter, "area"])
  AI = sumPop / ttlPopulation
  rltDataFrame<-rbind(rltDataFrame,data.frame(seedIdx=seedIdx, districtName=districtName, AI=AI, sumPop=sumPop, sumArea=sumArea))
}
#rltDataFrame$AI = format(rltDataFrame$AI, nsmall=4, digits=4, scientific = FALSE)
write.table(rltDataFrame, file=sprintf("%s/AI_%i_%i_%i_orgseed.csv", outShpFileDir, thPopDen, thSeedMinPop, thTvlTime), row.names = FALSE, sep=",")


# calc AI based on seed2 (agglomerated seed)
rltDataFrame = data.frame(seedIdx=NULL,districtName=NULL,AI=NULL,sumPop=NULL, sumArea=NULL)
df = adm2@data[aggSeedFilter,]
for (i in 1:nrow(df)){
  seedIdx = df[i, "orgSeqId"]
  districtName = df[i, polyMatchColName]
  filter = adm2@data[, "aggidx2"] == seedIdx
  sumPop = sum(adm2@data[filter, popDataColName])
  sumArea =  sum(adm2@data[filter, "area"])
  AI = sumPop / ttlPopulation
  rltDataFrame<-rbind(rltDataFrame,data.frame(seedIdx=seedIdx, districtName=districtName, AI=AI, sumPop=sumPop, sumArea=sumArea))
}

#rltDataFrame$AI = format(rltDataFrame$AI, nsmall=4, digits=4, scientific = FALSE)
write.table(rltDataFrame, file=sprintf("%s/AI_%i_%i_%i_aggseed.csv", outShpFileDir, thPopDen, thSeedMinPop, thTvlTime), row.names = FALSE, sep=",")

writeOGR(obj=adm2, dsn=outShpFileDir, layer=sprintf("AI_%i_%i_%i", thPopDen, thSeedMinPop, thTvlTime), driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)

# save routeInfo
RouteInfo.save(routeInfoFilePath)

algEndTime = Sys.time()

print(sprintf("==========> all done (in %.2f seconds) <==========", as.numeric(algEndTime-algStartTime, units="secs")))
}

AI.batch <- function(vecThPopDen=c(150, 300, 500), 
                     vecThSeedMinPop=c(20000, 50000, 100000),
                     vecThTvlTime=c(60, 90),
                     seedAsAdmTypeVec=c("Urban Council","Municipal Council"),
                     seedAsTopNHighPopArea = 40,
                     seedDirName="ByUcMc",
                     seedMergeDist=30
                     ){
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # This is a shortcut to run AI.gen over multiple threshold combinations. The input pareameters listed here are all keys to affect final outputs. 
  # The rest parameters required by AI.gen are left as default values. If we want to adjust only one of them for the AI.batch call, say changing "thSearchSeedRadius" from 200 to 250,
  # we need to change its default value in the AI.gen function definition, and reload the entire script then call AI.batch again. 
  # Input parameters:
  # (1) vecThPopDen: an integer vector of population density thresholds 
  # (2) vecThSeedMinPop: an integer vector of minimum seed population thresholds
  # (3) vecThTvlTime: an integer vector of travel time thresholds
  # (4) seedAsAdmTypeVec: the root dir of outputs
  # (5) seedAsTopNHighPopArea: the specific output dir name for different seed selections
  # (6) seedDirName: the country name of dataset
  # (7) seedMergeDist: the year of dataset
  # Output:
  # (1) outputs of AI.gen for each combination
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  sTime = Sys.time()
  
  for (ThPopDen in vecThPopDen){
    for (ThSeedMinPop in vecThSeedMinPop){
      for (ThTvlTime in vecThTvlTime){
        print(sprintf("============== BATCH RUN: (%i,%i,%i) ==============", ThPopDen, ThSeedMinPop, ThTvlTime))
        
        AI.gen(thPopDen=ThPopDen, 
                thSeedMinPop=ThSeedMinPop, 
                thTvlTime=ThTvlTime, 
                seedAsAdmTypeVec=seedAsAdmTypeVec,
                seedAsTopNHighPopArea=seedAsTopNHighPopArea,
                seedDirName=seedDirName,
                seedMergeDist=seedMergeDist)
      }
    }
  }
  
  eTime = Sys.time()
  print("==============================================")
  print(sprintf("=== BATCH DONE (in %.2f seconds) ===", as.numeric(eTime-sTime, units="secs")))
  print("==============================================")
}


AI.sum <- function(vecThPopDen=c(150, 300, 500), 
                    vecThSeedMinPop=c(20000, 50000, 100000),
                    vecThTvlTime=c(60, 90),
                    outDir=gOutRootDir,
                    seedDirName="ByUcMc",
                    countryName = gCountryName,
                    dataYear = gDataYear){
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # Input parameters:
  # (1) vecThPopDen: an integer vector of population density thresholds 
  # (2) vecThSeedMinPop: an integer vector of minimum seed population thresholds
  # (3) vecThTvlTime: an integer vector of travel time thresholds
  # (4) outDir: the root dir of outputs
  # (5) seedDirName: the specific output dir name for different seed selections
  # (6) countryName: the country name of dataset
  # (7) dataYear: the year of dataset
  # Output:
  # (1) create a table contains summed AI values of every combination (vecThPopDen X vecThSeedMinPop X vecThTvlTime)
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  
  rltDF <- data.frame(thpopden=NULL, thseedminpop=NULL, thtvltime=NULL, ttlAI=NULL)
  for (ThPopDen in vecThPopDen){
    for (ThSeedMinPop in vecThSeedMinPop){
      for (ThTvlTime in vecThTvlTime){
        filePath = sprintf("%s%s/%s/%s/AI_%i_%i_%i_aggseed.csv", outDir, countryName, dataYear, seedDirName, ThPopDen, ThSeedMinPop, ThTvlTime)
        if (file.exists(filePath)){
          tmpDF <- read.table(filePath, header=TRUE, sep=",", blank.lines.skip = TRUE, fill = TRUE)
          ttlAI = sum(tmpDF[,"AI"])
          ttlPop = sum(tmpDF[,"sumPop"])
          
          ttlArea = sum(tmpDF[,"sumArea"])
          rltDF <- rbind(rltDF, data.frame(thpopden=ThPopDen, thseedminpop=ThSeedMinPop, thtvltime=ThTvlTime, ttlAI=ttlAI, ttlPop=ttlPop, ttlArea=ttlArea))
        }
      }
    }
  }
  if (nrow(rltDF) == 0) {
    print("=====no valid data (*seed.csv) found in target folder")
  } else {
    rltDF$ttlAI = format(rltDF$ttlAI, nsmall=4, digits=4, scientific = FALSE)
    rltDF$thseedminpop = format(rltDF$thseedminpop, scientific = FALSE)
    write.table(rltDF, file=sprintf("%s%s/%s/%s/sumAI.csv", outDir, countryName, dataYear,  seedDirName), row.names = FALSE, sep=",", quote = FALSE)
    print("=====done")
  }
}
