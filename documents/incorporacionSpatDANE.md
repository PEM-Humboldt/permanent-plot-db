# Incorporating the geography from DANE for department and
municipalities
Marius Bottin

Natalia Medina gave me the 2023 geographic DANE version that I dowloaded
on my local computer

``` r
DIR_DATA <- "/home/marius/Travail/Data/DANE_geog_colombia/MGN 2023/"
dir(DIR_DATA)
```

     [1] "departamento.tsv"           "MGN_2023_COLOMBIA"         
     [3] "MGN_DPTO_POLITICO_2023.cpg" "MGN_DPTO_POLITICO_2023.dbf"
     [5] "MGN_DPTO_POLITICO_2023.prj" "MGN_DPTO_POLITICO_2023.qmd"
     [7] "MGN_DPTO_POLITICO_2023.shp" "MGN_DPTO_POLITICO_2023.shx"
     [9] "MGN_MPIO_POLITICO_2023.cpg" "MGN_MPIO_POLITICO_2023.dbf"
    [11] "MGN_MPIO_POLITICO_2023.prj" "MGN_MPIO_POLITICO_2023.qmd"
    [13] "MGN_MPIO_POLITICO_2023.shp" "MGN_MPIO_POLITICO_2023.shx"
    [15] "municipio.tsv"             

``` r
require(sf)
```

    Loading required package: sf

    Linking to GEOS 3.13.0, GDAL 3.9.3, PROJ 9.4.1; sf_use_s2() is TRUE

``` r
dpto<-st_read(DIR_DATA,"MGN_DPTO_POLITICO_2023")
```

    Reading layer `MGN_DPTO_POLITICO_2023' from data source 
      `/home/marius/Travail/Data/DANE_geog_colombia/MGN 2023' using driver `ESRI Shapefile'
    Simple feature collection with 33 features and 9 fields
    Geometry type: MULTIPOLYGON
    Dimension:     XY
    Bounding box:  xmin: -81.73562 ymin: -4.229406 xmax: -66.84722 ymax: 13.39473
    Geodetic CRS:  WGS 84

``` r
mpio<-st_read(DIR_DATA,"MGN_MPIO_POLITICO_2023")
```

    Reading layer `MGN_MPIO_POLITICO_2023' from data source 
      `/home/marius/Travail/Data/DANE_geog_colombia/MGN 2023' using driver `ESRI Shapefile'
    Simple feature collection with 1121 features and 13 fields
    Geometry type: MULTIPOLYGON
    Dimension:     XY
    Bounding box:  xmin: -81.73562 ymin: -4.229406 xmax: -66.84722 ymax: 13.39473
    Geodetic CRS:  WGS 84

``` r
st_crs(dpto)
```

    Coordinate Reference System:
      User input: WGS 84 
      wkt:
    GEOGCRS["WGS 84",
        DATUM["World Geodetic System 1984",
            ELLIPSOID["WGS 84",6378137,298.257223563,
                LENGTHUNIT["metre",1]]],
        PRIMEM["Greenwich",0,
            ANGLEUNIT["degree",0.0174532925199433]],
        CS[ellipsoidal,2],
            AXIS["latitude",north,
                ORDER[1],
                ANGLEUNIT["degree",0.0174532925199433]],
            AXIS["longitude",east,
                ORDER[2],
                ANGLEUNIT["degree",0.0174532925199433]],
        ID["EPSG",4326]]

``` r
st_crs(mpio)
```

    Coordinate Reference System:
      User input: WGS 84 
      wkt:
    GEOGCRS["WGS 84",
        DATUM["World Geodetic System 1984",
            ELLIPSOID["WGS 84",6378137,298.257223563,
                LENGTHUNIT["metre",1]]],
        PRIMEM["Greenwich",0,
            ANGLEUNIT["degree",0.0174532925199433]],
        CS[ellipsoidal,2],
            AXIS["latitude",north,
                ORDER[1],
                ANGLEUNIT["degree",0.0174532925199433]],
            AXIS["longitude",east,
                ORDER[2],
                ANGLEUNIT["degree",0.0174532925199433]],
        ID["EPSG",4326]]

``` r
head(dpto)
```

| dpto_ccdgo | dpto_cnmbr | dpto_ano_c | dpto_act_a | dpto_narea | dpto_nano | shape_Leng | shape_Area | suggestedS | geometry |
|:---|:---|---:|:---|---:|---:|---:|---:|:---|:---|
| 05 | ANTIOQUIA | 1886 | Constitucion Politica de 1886 | 62807.004 | 2023 | 21.492797 | 5.1351034 | Antioquia | MULTIPOLYGON (((-76.41355 8… |
| 08 | ATLÁNTICO | 1910 | Ley 21 de 1910 | 3314.458 | 2023 | 2.571601 | 0.2738234 | Atlántico | MULTIPOLYGON (((-74.84946 1… |
| 11 | BOGOTÁ, D.C. | 1538 | Constitucion Politica de 1886 | 1622.853 | 2023 | 3.765371 | 0.1322079 | Bogotá, D.C. | MULTIPOLYGON (((-74.07059 4… |
| 13 | BOLÍVAR | 1886 | Constitucion Politica de 1886 | 26720.329 | 2023 | 16.273430 | 2.1956689 | Bolívar | MULTIPOLYGON (((-76.17318 9… |
| 15 | BOYACÁ | 1886 | Constitucion Politica de 1886 | 23137.998 | 2023 | 15.906767 | 1.8883867 | Boyacá | MULTIPOLYGON (((-72.17368 7… |
| 17 | CALDAS | 1905 | 11 de Abril de 1905 | 7425.220 | 2023 | 6.664502 | 0.6054977 | Caldas | MULTIPOLYGON (((-74.67154 5… |

``` r
head(mpio)
```

| dpto_ccdgo | mpio_ccdgo | mpio_cdpmp | dpto_cnmbr | mpio_cnmbr | mpio_crslc | mpio_tipo | mpio_narea | mpio_nano | shape_Leng | shape_Area | suggestedS | suggestedC | geometry |
|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|:---|
| 05 | 001 | 05001 | ANTIOQUIA | MEDELLÍN | 1965 | MUNICIPIO | 374.83400 | 2023 | 1.0353800 | 0.0306076 | Antioquia | Medellín | MULTIPOLYGON (((-75.66974 6… |
| 05 | 002 | 05002 | ANTIOQUIA | ABEJORRAL | 1814 | MUNICIPIO | 507.14109 | 2023 | 1.1585036 | 0.0413839 | Antioquia | Abejorral | MULTIPOLYGON (((-75.46938 5… |
| 05 | 004 | 05004 | ANTIOQUIA | ABRIAQUÍ | 1912 | MUNICIPIO | 296.89405 | 2023 | 0.8121832 | 0.0242483 | Antioquia | Abriaquí | MULTIPOLYGON (((-76.08351 6… |
| 05 | 021 | 05021 | ANTIOQUIA | ALEJANDRÍA | Decreto departamental 304 de 1907 | MUNICIPIO | 128.93215 | 2023 | 0.7051995 | 0.0105345 | Antioquia | Alejandría | MULTIPOLYGON (((-75.0332 6…. |
| 05 | 030 | 05030 | ANTIOQUIA | AMAGÁ | 1912 | MUNICIPIO | 84.13248 | 2023 | 0.4455330 | 0.0068665 | Antioquia | Amagá | MULTIPOLYGON (((-75.67587 6… |
| 05 | 031 | 05031 | ANTIOQUIA | AMALFI | 1843 | MUNICIPIO | 1209.14546 | 2023 | 2.0587316 | 0.0989211 | Antioquia | Amalfi | MULTIPOLYGON (((-74.91836 7… |

``` r
table(mpio$mpio_tipo)
```


    ÁREA NO MUNICIPALIZADA                   ISLA              MUNICIPIO 
                        18                      1                   1102 

## Connect to the database

``` r
pp_bst <- RPostgres::dbConnect(RPostgres::Postgres(), dbname = "pp_bst_col")
```

## Managing the reference systems

``` r
(sridDB<-RPostgres::dbGetQuery(pp_bst,"SELECT srid FROM geometry_columns WHERE f_table_schema='main' GROUP BY srid ORDER BY count(*) DESC LIMIT 1")$srid)
```

    [1] 3116

``` r
dpto<-st_transform(dpto,sridDB)
mpio<-st_transform(mpio,sridDB)
```

## Creating the shemas

``` r
schemas<-RPostgres::dbGetQuery(pp_bst,"SELECT schema_name FROM information_schema.schemata")$schema_name
if(!"tmp" %in% schemas)
{RPostgres::dbExecute(pp_bst,"CREATE schema tmp")}
if(!"spat" %in% schemas)
{RPostgres::dbExecute(pp_bst,"CREATE schema spat")}
```

    [1] 0

## Creating the tables

``` r
all(toupper(mpio$suggestedC)==mpio$mpio_cnmbr)
```

    [1] TRUE

``` r
if(RPostgres::dbExistsTable(pp_bst,DBI::Id(schema="spat",table="mpio_dane_2023")))
{RPostgres::dbExecute(pp_bst,"DROP TABLE spat.mpio_dane_2023")}
if(RPostgres::dbExistsTable(pp_bst,DBI::Id(schema="spat",table="dpto_dane_2023")))
{RPostgres::dbExecute(pp_bst,"DROP TABLE spat.dpto_dane_2023")}
RPostgres::dbExecute(pp_bst,
"CREATE TABLE spat.dpto_dane_2023
(
  dpto_ccdgo char(2) PRIMARY KEY,
  dpto text UNIQUE
)")
```

    [1] 0

``` r
RPostgres::dbExecute(pp_bst,paste0(
  "SELECT AddGeometryColumn('spat','dpto_dane_2023','the_geom',",RPostgres::dbQuoteLiteral(pp_bst,sridDB),",'MULTIPOLYGON',2,true)"))
```

    [1] 0

``` r
RPostgres::dbExecute(pp_bst,
"CREATE TABLE spat.mpio_dane_2023
(
  mpio_cdpmp char(5) PRIMARY KEY,
  dpto_ccdgo char(2) REFERENCES spat.dpto_dane_2023(dpto_ccdgo),
  mpio text,
  UNIQUE(dpto_ccdgo,mpio)
)")
```

    [1] 0

``` r
RPostgres::dbExecute(pp_bst,paste0(
  "SELECT AddGeometryColumn('spat','mpio_dane_2023','the_geom',",RPostgres::dbQuoteLiteral(pp_bst,sridDB),",'MULTIPOLYGON',2,true)"))
```

    [1] 0

## Inserting the values in the table

``` r
dpto<-dpto[,c("dpto_ccdgo","suggestedS","geometry")]
colnames(dpto)<-c("dpto_ccdgo","dpto","geometry")
st_geometry(dpto)<-"the_geom"
st_write(dpto,pp_bst,DBI::Id(schema="spat", table="dpto_dane_2023"),append=T)
```

    Note: method with signature 'DBIObject#sf' chosen for function 'dbDataType',
     target signature 'PqConnection#sf'.
     "PqConnection#ANY" would also be valid

``` r
mpio<-mpio[,c("mpio_cdpmp","dpto_ccdgo","suggestedC","geometry")]
colnames(mpio)<-c("mpio_cdpmp","dpto_ccdgo","mpio","geometry")
st_geometry(mpio)<-"the_geom"
st_write(mpio,pp_bst,DBI::Id(schema="spat", table="mpio_dane_2023"),append=T)
```

## Shutting down light…

``` r
RPostgres::dbDisconnect(pp_bst)
```
