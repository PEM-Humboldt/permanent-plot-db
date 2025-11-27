# Incorporating the Digital Elevation Model for Colombia
Marius Bottin

Julian Torres gave me the 2014 version (from IGAC) of the SRTM with a 1
arc-grade resolution.

``` r
DIR_DATA <- "/home/marius/Travail/Data/DEM/dem30mcolo/"
dir(DIR_DATA)
```

    [1] "dem30mcolo.tfw"         "dem30mcolo.tif"         "dem30mcolo.tif.aux.xml"
    [4] "dem30mcolo.tif.ovr"     "dem30mcolo.tif.vat.cpg" "dem30mcolo.tif.vat.dbf"

``` r
require(sf)
```

    Loading required package: sf

    Linking to GEOS 3.13.0, GDAL 3.9.3, PROJ 9.4.1; sf_use_s2() is TRUE

``` r
require(rpostgis)
```

    Loading required package: rpostgis

    Loading required package: RPostgreSQL

    Loading required package: DBI

     Welcome to rpostgis - Version: 1.6.0 (2024-12-05)
    - Support for `sp` and `raster` objects is deprecated.
    - These will be removed in a future release.
    - Please use `sf` and `terra` objects with rpostgis.

``` r
require(terra)
```

    Loading required package: terra

    terra 1.8.64

``` r
dem<-rast(paste(DIR_DATA,"dem30mcolo.tif",sep="/"))
#crs(dem)<-crs("epsg:4170")
```

## Connect to the database

``` r
pp_bst <- RPostgres::dbConnect(RPostgres::Postgres(), dbname = "pp_bst_col")
```

## Managing the reference systems

``` r
(sridDB<-RPostgres::dbGetQuery(pp_bst,"SELECT auth_name||':'||auth_srid srs FROM geometry_columns LEFT JOIN spatial_ref_sys USING(srid) WHERE f_table_schema='main' GROUP BY auth_name,auth_srid ORDER BY count(*) DESC LIMIT 1")$srs)
```

    [1] "EPSG:3116"

``` r
(srsDB<-RPostgres::dbGetQuery(pp_bst,"SELECT srid srs FROM geometry_columns LEFT JOIN spatial_ref_sys USING(srid) WHERE f_table_schema='main' GROUP BY srid ORDER BY count(*) DESC LIMIT 1")$srs)
```

    [1] 3116

``` r
#dem<-project(dem,crs(sridDB))
```

## Creating the shemas

``` r
schemas<-RPostgres::dbGetQuery(pp_bst,"SELECT schema_name FROM information_schema.schemata")$schema_name
if(!"tmp" %in% schemas)
{RPostgres::dbExecute(pp_bst,"CREATE schema tmp")}
if(!"spat" %in% schemas)
{RPostgres::dbExecute(pp_bst,"CREATE schema spat")}
```

## Manage the raster extension

``` r
RPostgres::dbExecute(pp_bst,"CREATE EXTENSION IF NOT EXISTS postgis_raster")
```

    [1] 0

## Importing the DEM

<!--NB: database need CREATE EXTENSION postgis_raster-->

**not applied**:

``` r
pgWriteRast(pp_bst,c("spat","dem"),raster=dem,overwrite=T)
```

## Bash

``` r
noData <- -32768L
file<-"dem30mcolo.tif"
fileReproject<-"dem30mcolo_3116.tif"
(commandReproject<-paste0("gdalwarp -t_srs \"",sridDB,"\" -srcnodata \"",noData,"\" ",  paste(DIR_DATA,file,sep="/")," ",paste(DIR_DATA,fileReproject,sep="/")))
```

    [1] "gdalwarp -t_srs \"EPSG:3116\" -srcnodata \"-32768\" /home/marius/Travail/Data/DEM/dem30mcolo//dem30mcolo.tif /home/marius/Travail/Data/DEM/dem30mcolo//dem30mcolo_3116.tif"

``` r
system(commandReproject)
(commandInsertDb <- paste0("raster2pgsql -s ",srsDB," -I -C -d -Y -t 100x100 -N \"",noData,"\" ", paste(DIR_DATA,fileReproject,sep="/")," spat.dem  | psql pp_bst_col"))
```

    [1] "raster2pgsql -s 3116 -I -C -d -Y -t 100x100 -N \"-32768\" /home/marius/Travail/Data/DEM/dem30mcolo//dem30mcolo_3116.tif spat.dem  | psql pp_bst_col"

``` r
system(commandInsertDb)
unlink(paste(DIR_DATA,fileReproject,sep="/"))
```

## test parcelas

``` sql
WITH a AS (
SELECT cd_event, event_id, (ST_SummaryStats(ST_clip(rast,1,ST_Buffer(pol_geom,20)))).* 
FROM main.event e
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN main.gp_event ge USING(cd_gp_event)
LEFT JOIN main.project p USING (cd_project)
LEFT JOIN spat.dem ON ST_Intersects(rast, pol_geom) 
WHERE project='Jabiru')
SELECT cd_event, event_id, * 
FROM a
LIMIT 10
--GROUP BY cd_event, event_id
```

| cd_event | event_id | cd_event | event_id | count | sum | mean | stddev | min | max |
|:---|:---|---:|:---|---:|---:|---:|---:|---:|---:|
| 2719 | Jabiru_census0_subplot1 | 2719 | Jabiru_census0_subplot1 | 4 | 1097 | 274.25 | 0.8291562 | 273 | 275 |
| 2720 | Jabiru_census0_subplot2 | 2720 | Jabiru_census0_subplot2 | 2 | 549 | 274.50 | 0.5000000 | 274 | 275 |
| 2721 | Jabiru_census0_subplot3 | 2721 | Jabiru_census0_subplot3 | 3 | 822 | 274.00 | 0.8164966 | 273 | 275 |
| 2722 | Jabiru_census0_subplot4 | 2722 | Jabiru_census0_subplot4 | 4 | 1099 | 274.75 | 1.4790199 | 273 | 277 |
| 2723 | Jabiru_census0_subplot5 | 2723 | Jabiru_census0_subplot5 | 2 | 550 | 275.00 | 2.0000000 | 273 | 277 |
| 2724 | Jabiru_census0_subplot6 | 2724 | Jabiru_census0_subplot6 | 2 | 550 | 275.00 | 2.0000000 | 273 | 277 |
| 2725 | Jabiru_census0_subplot7 | 2725 | Jabiru_census0_subplot7 | 4 | 1111 | 277.75 | 3.2691742 | 273 | 282 |
| 2726 | Jabiru_census0_subplot8 | 2726 | Jabiru_census0_subplot8 | 2 | 561 | 280.50 | 1.5000000 | 279 | 282 |
| 2727 | Jabiru_census0_subplot9 | 2727 | Jabiru_census0_subplot9 | 2 | 561 | 280.50 | 1.5000000 | 279 | 282 |
| 2728 | Jabiru_census0_subplot10 | 2728 | Jabiru_census0_subplot10 | 4 | 1130 | 282.50 | 2.2912878 | 279 | 285 |

Displaying records 1 - 10

``` sql
SELECT event_id, ST_Value(rast,ST_Centroid(pol_geom))
FROM main.event e
LEFT JOIN main.location USING (cd_loc)
LEFT JOIN main.gp_event USING (cd_gp_event)
LEFT JOIN main.project USING (cd_project)
LEFT JOIN spat.dem ON ST_Intersects(ST_Centroid(pol_geom),rast)
WHERE project='LaPaz'
LIMIT 10
```

| event_id                | st_value |
|:------------------------|---------:|
| LaPaz_census0_subplot1  |      163 |
| LaPaz_census0_subplot2  |      161 |
| LaPaz_census0_subplot3  |      161 |
| LaPaz_census0_subplot4  |      161 |
| LaPaz_census0_subplot5  |      161 |
| LaPaz_census0_subplot6  |      160 |
| LaPaz_census0_subplot7  |      160 |
| LaPaz_census0_subplot8  |      160 |
| LaPaz_census0_subplot9  |      159 |
| LaPaz_census0_subplot10 |      159 |

Displaying records 1 - 10

<!-- TODO:
* pass very negative values to NoData
* suppress the rast rows which only have NoData
-->

## Shutting down light…

``` r
RPostgres::dbDisconnect(pp_bst)
```
