# Ejemplo de incorporación de datos a la base de datos: parcelas LaPaz,
Matitas, Plato
Marius Bottin

``` r
DIR_DATA <- "../../otherData/"
dir(DIR_DATA)
```

    [1] "Dryflor_Marius.txt"           "LaPaz.csv"                   
    [3] "Matitas.csv"                  "Plato.csv"                   
    [5] "TDF_taxonomicReference.RData"

``` r
encod<-stringi::stri_enc_detect(paste0(DIR_DATA,"/LaPaz.csv"))[[1]]$Encoding[1]
plots<-c("LaPaz","Matitas","Plato")
parcelas<-lapply(paste(DIR_DATA,paste(plots,"csv",sep="."),sep="/"),read.csv,dec=",",sep=";", fileEncoding=encod)
names(parcelas) <- plots
# corrección puntual: hay un "1" en las latitudes de LaPaz que no tiene sentido
parcelas$LaPaz$latitude_decimal[parcelas$LaPaz$latitude_decimal==1]<-NA
parcelas_tot <- data.frame(id=1:sum(sapply(parcelas,nrow)),plot=rep(names(parcelas),sapply(parcelas,nrow)),Reduce(rbind, parcelas))
```

## Connect to the database

``` r
pp_bst <- RPostgres::dbConnect(RPostgres::Postgres(), dbname = "pp_bst_col")
```

## Methodology and variables

``` r
def_unit <- RPostgres::dbReadTable(pp_bst, DBI::Id(schema = "main", table="def_unit"))
stopifnot("Number of individuals" %in% def_unit$unit)
def_var <- RPostgres::dbReadTable(pp_bst, DBI::Id(schema = "main", table="def_var"))
stopifnot("qt_int" %in% def_var$name_var)
def_method <- RPostgres::dbReadTable(pp_bst, DBI::Id(schema = "main", table="def_method"))
stopifnot("Permanent plot 1 (to be defined)" %in% def_method$method)
```

## Spatial data

### Sending individual coordinates

``` r
library(spData)
```

    To access larger datasets in this package, install the spDataLarge
    package with: `install.packages('spDataLarge',
    repos='https://nowosad.github.io/drat/', type='source')`

``` r
library(sf)
```

    Linking to GEOS 3.13.0, GDAL 3.9.3, PROJ 9.4.1; sf_use_s2() is TRUE

``` r
CRS<-3116
CRS_orig<-4326
worldData<-spData::world
colombia<-st_geometry(worldData[worldData$name_long=="Colombia",]) %>% st_set_crs(CRS_orig) %>% st_transform(CRS)
sf_parcelas<-st_as_sf(parcelas_tot[!is.na(parcelas_tot$longitude_decimal) & !is.na(parcelas_tot$latitude_decimal), ], coords=c("longitude_decimal","latitude_decimal")) %>% st_set_crs(CRS_orig) %>% st_transform(CRS)
st_write(obj = sf_parcelas, dsn = pp_bst, layer = DBI::Id(schema = "tmp", table = "parcelas"), delete_layer = T)
```

    Note: method with signature 'DBIObject#sf' chosen for function 'dbDataType',
     target signature 'PqConnection#sf'.
     "PqConnection#ANY" would also be valid

### Building the plot polygons

En este caso particular tenemos que construir los objetos espaciales que
se van a referenciar.

``` r
RPostgres::dbGetQuery(pp_bst,"
WITH a AS(
    SELECT plot,min(ST_X(geometry)) min_x,min(ST_Y(geometry)) min_y,max(ST_X(geometry)) max_x,max(ST_Y(geometry)) max_y
    FROM tmp.parcelas
    GROUP BY plot 
)
SELECT plot,
  ST_Distance(ST_SetSrid(ST_MakePoint(min_x,min_y),3116), ST_SetSrid(ST_MakePoint(min_x,max_y),3116)) distSN,
  ST_Distance(ST_SetSrid(ST_MakePoint(min_x,min_y),3116), ST_SetSrid(ST_MakePoint(max_x,min_y),3116)) distWE
FROM a
;")
```

| plot    |   distsn |   distwe |
|:--------|---------:|---------:|
| Plato   | 99.70003 | 99.70035 |
| LaPaz   | 99.44691 | 99.20926 |
| Matitas | 99.79035 | 99.70009 |

``` r
# CREATE A TABLE WITH TEMPORARY OBJECTS FOR THE PROJECTS POLYGONS
if(RPostgres::dbExistsTable(pp_bst,DBI::Id(schema="tmp",table="spatproject")))
{RPostgres::dbExecute(pp_bst,"DROP TABLE tmp.spatproject")}
```

    [1] 0

``` r
RPostgres::dbExecute(pp_bst,"
CREATE TABLE tmp.spatproject AS(
WITH a AS(
    SELECT plot,min(ST_X(geometry)) min_x,min(ST_Y(geometry)) min_y,max(ST_X(geometry)) max_x,max(ST_Y(geometry)) max_y
    FROM tmp.parcelas
    GROUP BY plot 
)
SELECT 
  plot,
  ST_SetSRID(ST_makePolygon(ST_MakeLine(ARRAY[st_makepoint(min_x,min_y),ST_MakePoint(min_x,max_y), ST_MakePoint(max_x,max_y), ST_MakePoint(max_x,min_y), ST_MakePoint(min_x,min_y)])),3116) geom
FROM a
)")
```

    [1] 3

### Building the subplot spatial objects

#### Defining the grid function in PL/PSQL

Creating the function to create grids as polygons (which we will use as
subplots)

<https://gis.stackexchange.com/questions/16374/creating-regular-polygon-grid-in-postgis>

``` r
RPostgres::dbExecute(pp_bst,"
CREATE OR REPLACE FUNCTION public.makegrid_2d (
  bound_polygon public.geometry,
  grid_step integer,
  metric_srid integer = 28408 --metric SRID (this particular is optimal for the Western Russia)
)
RETURNS public.geometry AS
$body$
DECLARE
  BoundM public.geometry; --Bound polygon transformed to the metric projection (with metric_srid SRID)
  Xmin DOUBLE PRECISION;
  Xmax DOUBLE PRECISION;
  Ymax DOUBLE PRECISION;
  X DOUBLE PRECISION;
  Y DOUBLE PRECISION;
  sectors public.geometry[];
  i INTEGER;
BEGIN
  BoundM := ST_Transform($1, $3); --From WGS84 (SRID 4326) to the metric projection, to operate with step in meters
  Xmin := ST_XMin(BoundM);
  Xmax := ST_XMax(BoundM);
  Ymax := ST_YMax(BoundM);

  Y := ST_YMin(BoundM); --current sector's corner coordinate
  i := -1;
  <<yloop>>
  LOOP
    IF (Y > Ymax) THEN  --Better if generating polygons exceeds the bound for one step. You always can crop the result. But if not you may get not quite correct data for outbound polygons (e.g. if you calculate frequency per sector)
        EXIT;
    END IF;

    X := Xmin;
    <<xloop>>
    LOOP
      IF (X > Xmax) THEN
          EXIT;
      END IF;

      i := i + 1;
      sectors[i] := ST_GeomFromText('POLYGON(('||X||' '||Y||', '||(X+$2)||' '||Y||', '||(X+$2)||' '||(Y+$2)||', '||X||' '||(Y+$2)||', '||X||' '||Y||'))', $3);

      X := X + $2;
    END LOOP xloop;
    Y := Y + $2;
  END LOOP yloop;

  RETURN ST_Transform(ST_Collect(sectors), ST_SRID($1));
END;
$body$
LANGUAGE 'plpgsql';
")
```

    [1] 0

#### Using the PL/SQL function to create the polygon grid

``` r
if(RPostgres::dbExistsTable(pp_bst, DBI::Id(schema="tmp",table="spatevent")))
   {RPostgres::dbRemoveTable(pp_bst,DBI::Id(schema="tmp",table="spatevent"))}
RPostgres::dbExecute(pp_bst,
"CREATE TABLE tmp.spatevent AS(
WITH a AS(
    SELECT plot,min(ST_X(geometry)) min_x,min(ST_Y(geometry)) min_y,max(ST_X(geometry)) max_x,max(ST_Y(geometry)) max_y
    FROM tmp.parcelas
    GROUP BY plot 
), b AS(
SELECT plot,ST_SetSRID(ST_makePolygon(ST_MakeLine(ARRAY[st_makepoint(min_x,min_y),ST_MakePoint(min_x,max_y), ST_MakePoint(max_x,max_y), ST_MakePoint(max_x,min_y), ST_MakePoint(min_x,min_y)])),3116) geom
FROM a
), c AS(
SELECT plot, (ST_dump(public.makegrid_2d(ST_Transform(geom,3116),10,3116))).*
FROM b
), d AS(
SELECT plot, path, ROUND(ST_X(ST_Centroid(geom))) x_round, ROUND(ST_Y(ST_CENTROID(geom))) y_round
FROM c
), e AS(
SELECT x_round, RANK() OVER (PARTITION BY plot ORDER BY x_round) rank_x
FROM d
GROUP BY plot,x_round
), f AS(
SELECT DISTINCT ON (y_round) y_round, RANK() OVER (PARTITION BY plot ORDER BY y_round) rank_y
FROM d
GROUP BY plot, y_round
)
SELECT d.plot, path, rank_x, rank_y,ST_Transform(geom,3116) geom
FROM d
LEFT JOIN e USING (x_round)
LEFT JOIN f USING (y_round)
LEFT JOIN c USING (plot,path)
ORDER BY plot, path
)")
```

    [1] 300

``` r
max_x<-10
max_y<-10
RPostgres::dbExecute(pp_bst,
paste0("DELETE FROM tmp.spatevent
WHERE rank_x > ",max_x," OR rank_y > ",max_y))
```

    [1] 0

##### subplot

``` r
orderedSubplot<-data.frame(
  rank_x = rep(1:max_x,max_y),
  rank_y = rep(1:max_y,each=max_x),
  subplot = NA
)
counter<-0
for(i in seq(1,max_x,by=2))
{
  for(j in 1:max_y)
  {
    counter<-counter+1
    orderedSubplot$subplot[orderedSubplot$rank_x==i & orderedSubplot$rank_y==j]<-counter
  }
  i<-i+1
  for(j in max_y:1)
  {
    counter<-counter+1
    orderedSubplot$subplot[orderedSubplot$rank_x==i & orderedSubplot$rank_y==j]<-counter
  }
}
RPostgres::dbWriteTable(conn = pp_bst, name = DBI::Id(schema="tmp",table="subplotorder"), value = orderedSubplot, overwrite=T)
RPostgres::dbExecute(pp_bst, "ALTER TABLE tmp.spatevent ADD COLUMN IF NOT EXISTS subplot int")
```

    [1] 0

``` r
RPostgres::dbExecute(pp_bst, "UPDATE tmp.spatevent se SET subplot=spo.subplot FROM tmp.subplotorder spo WHERE se.rank_x=spo.rank_x AND se.rank_y=spo.rank_y")
```

    [1] 300

``` r
comp<-RPostgres::dbGetQuery(pp_bst,"
SELECT p.subplot pt, se.subplot spat
FROM tmp.parcelas p
LEFT JOIN tmp.spatevent se ON ST_Intersects(p.geometry,se.geom)
WHERE p.plot='LaPaz'")
sum(comp$pt==comp$spat)/nrow(comp)
```

    [1] 0.9751553

``` r
comp<-RPostgres::dbGetQuery(pp_bst,"
SELECT p.subplot pt, se.subplot spat
FROM tmp.parcelas p
LEFT JOIN tmp.spatevent se ON ST_Intersects(p.geometry,se.geom)
WHERE p.plot='Matitas'")
sum(comp$pt==comp$spat,na.rm = T)/nrow(comp)
```

    [1] 0.9877255

``` r
comp<-RPostgres::dbGetQuery(pp_bst,"
SELECT p.subplot pt, se.subplot spat
FROM tmp.parcelas p
LEFT JOIN tmp.spatevent se ON ST_Intersects(p.geometry,se.geom)
WHERE p.plot='Plato'")
sum(comp$pt==comp$spat)/nrow(comp)
```

    [1] 0.971345

### Inserting locations

#### Projects

``` r
for(i in 1:4)
{
RPostgres::dbExecute(pp_bst, "DELETE FROM main.location WHERE cd_loc IN (
                     SELECT COALESCE(d.cd_loc,c.cd_loc,b.cd_loc,a.cd_loc)
                     FROM tmp.spatproject sp
                     LEFT JOIN main.location a ON sp.plot=a.location
                     LEFT JOIN main.location b ON b.parent_loc=a.cd_loc
                     LEFT JOIN main.location c ON c.parent_loc=b.cd_loc
                     LEFT JOIN main.location d ON d.parent_loc=c.cd_loc
                     WHERE (a.location IN (SELECT plot FROM tmp.spatproject)
                     OR b.location IN (SELECT plot FROM tmp.spatproject)
                     OR c.location IN (SELECT plot FROM tmp.spatproject)
                     OR d.location IN (SELECT plot FROM tmp.spatproject)
                     )
)")
}
RPostgres::dbExecute(pp_bst,
          "INSERT INTO main.location(location,cd_loc_type,cd_org_lev,pol_geom)
          SELECT plot, cd_loc_type, cd_org_lev, ST_Transform(geom, (SELECT srid FROM geometry_columns WHERE f_geometry_column = 'pol_geom' AND f_table_name = 'location'))
          FROM tmp.spatproject
          CROSS JOIN (SELECT cd_loc_type FROM main.def_location_type WHERE location_type='Plot polygon') a
          CROSS JOIN (SELECT cd_org_lev FROM main.def_organisation_level WHERE org_lev='project') b
          ")
```

    [1] 3

#### Events

``` r
RPostgres::dbExecute(pp_bst,
"INSERT INTO main.location(location, cd_loc_type, parent_loc, cd_org_lev, pol_geom)
SELECT plot || ': subplot ' || subplot, a.cd_loc_type,l.cd_loc parent_loc, b.cd_org_lev, ST_Transform(se.geom, (SELECT srid FROM geometry_columns WHERE f_geometry_column = 'pol_geom' AND f_table_name = 'location'))
FROM tmp.spatevent se
LEFT JOIN main.location l ON se.plot=l.location
CROSS JOIN (SELECT cd_loc_type FROM main.def_location_type WHERE location_type='Plot polygon') a
CROSS JOIN (SELECT cd_org_lev FROM main.def_organisation_level WHERE org_lev='event') b
ORDER BY l.cd_loc,subplot
")
```

    [1] 300

## Inserting projects

``` r
RPostgres::dbExecute(pp_bst,"DELETE FROM main.project WHERE project IN (SELECT plot FROM tmp.spatproject)")
```

    [1] 3

``` r
RPostgres::dbExecute(pp_bst,
          "INSERT INTO main.project(project, project_description,cd_proj_type,cd_method,cd_loc)
          SELECT plot,NULL,cd_proj_type,cd_method,cd_loc
          FROM tmp.spatproject sm
          CROSS JOIN (SELECT cd_proj_type FROM main.def_project_type WHERE proj_type='permanent plot') a
          CROSS JOIN (SELECT cd_method FROM main.def_method WHERE method='Permanent plot 1 (to be defined)') b
          LEFT JOIN main.location ON sm.plot=location
          ")
```

    [1] 3

## Inserting gp_events

``` r
parcelas_tot$measuring_date <- as.Date(parcelas_tot$measuring_date, format="%d/%m/%y")
RPostgres::dbWriteTable(conn = pp_bst, name = DBI::Id(schema="tmp",table="completeplots"), value = parcelas_tot, overwrite = T)
(tab_gp_event<-RPostgres::dbGetQuery(conn=pp_bst, 
"WITH a AS(
SELECT DISTINCT cd_project, cd_loc, 'arbo' cd_gp_biol, a.cd_method, measuring_date date_begin, measuring_date date_end
FROM tmp.completeplots cp
LEFT JOIN main.project p ON p.project=cp.plot
CROSS JOIN (SELECT cd_method, method FROM main.def_method WHERE method='pp1_census0') a
)
SELECT a.*, ROW_NUMBER() OVER (PARTITION BY cd_project ORDER BY date_begin) AS compaign_nb
FROM a
"))
```

| cd_project | cd_loc | cd_gp_biol | cd_method | date_begin | date_end   | compaign_nb |
|-----------:|-------:|:-----------|----------:|:-----------|:-----------|------------:|
|        334 |    634 | arbo       |         2 | 2015-11-08 | 2015-11-08 |           1 |
|        335 |    635 | arbo       |         2 | 2015-10-21 | 2015-10-21 |           1 |
|        336 |    636 | arbo       |         2 | 2015-11-07 | 2015-11-07 |           1 |

``` r
stopifnot(table(tab_gp_event$cd_project)==1)
RPostgres::dbExecute(pp_bst,
"INSERT INTO main.gp_event(cd_project, cd_loc, cd_gp_biol, cd_method, date_begin, date_end, campaign_nb)
WITH a AS(
SELECT DISTINCT cd_project, cd_loc, 'arbo' cd_gp_biol, a.cd_method, measuring_date date_begin, measuring_date date_end
FROM tmp.completeplots cp
LEFT JOIN main.project p ON p.project=cp.plot
CROSS JOIN (SELECT cd_method, method FROM main.def_method WHERE method='pp1_census0') a
)
SELECT a.*, ROW_NUMBER() OVER (PARTITION BY cd_project ORDER BY date_begin) AS compaign_nb
FROM a
")
```

    [1] 3

## Inserting events

``` r
RPostgres::dbExecute(pp_bst, 
"WITH a AS(
  SELECT p.project, cd_gp_event, date_begin date_gp_event
  FROM main.gp_event ge
  LEFT JOIN main.project p USING (cd_project)
  WHERE p.project IN (SELECT plot FROM tmp.completeplots)
)
INSERT INTO main.event(event_id, cd_gp_event, num_replicate, description_replicate, date_begin, date_end, cd_loc)
SELECT project||'_census0_subplot'||subplot, cd_gp_event, subplot, 'subplot '||subplot||'/100', date_gp_event, date_gp_event, cd_loc
FROM a
CROSS JOIN (SELECT generate_series(1,100) subplot) b 
LEFT JOIN main.location ON location=a.project||': subplot '||b.subplot
ORDER BY cd_gp_event, subplot
")
```

    [1] 300

``` r
events<-RPostgres::dbReadTable(pp_bst,DBI::Id(schema="main",table="event"))
m<-match(paste0(parcelas_tot$plot,"_census0_subplot",trimws(parcelas_tot$subplot)),events$event_id)
parcelas_tot$cd_event<-events$cd_event[m]
```

## Taxonomy

### Getting suggestions and validation from rdsTaxVal

``` r
library(rdsTaxVal)
taxo_parcelas<-new_taxo_oneTab(obj=parcelas_tot,currentFormat = "oneTable", taxonRanks_names = c(family="family",genus="genus",species="specificEpithet"), taxonRanks_epithetized = "specificEpithet",taxoCode = NA, comments = "comments",plot = "plot")
suggested_parcelas<-fullTaxonomicDiagnostic(taxo_parcelas,argsCheckFunction = list())
```

    cleaning space characters

    misplaced qualifiers for undetermined taxa

    unicity of genus in family

    checking for unicity of taxonomic information associated with taxonomic code

    Warning in checkUnicityCodetax(taxo = structure(list(id = 1:10422, plot =
    c("LaPaz", : The taxonomic codes are not defined in the object, apply a check
    on whether the taxonomic codes corresponds to taxonomic information does not
    make sense

    Comparing species information with gbif backbone

    Searching for 76 taxa in the GBIF Backbone
    ...

    done

    Analysing GBIF Backbone information

    64 taxa are found without any modification needed

    1 taxa are found with suggested orthographic changes

    11 taxa are suggested synonyms

    2 taxa are found with suggested higher rank changes

    0 taxa were not found

    Comparing genus information with gbif backbone

    Searching for 15 taxa in the GBIF Backbone
    ...

    done

    Analysing GBIF Backbone information

    15 taxa are found without any modification needed

    0 taxa are found with suggested orthographic changes

    0 taxa are suggested synonyms

    0 taxa are found with suggested higher rank changes

    0 taxa were not found

    Comparing family information with gbif backbone

    Searching for 4 taxa in the GBIF Backbone
    ...

    done

    Analysing GBIF Backbone information

    4 taxa are found without any modification needed

    0 taxa are found with suggested orthographic changes

    0 taxa are suggested synonyms

    0 taxa are found with suggested higher rank changes

    0 taxa were not found

``` r
corrected_parcelas<-correct(taxo_parcelas,suggested_parcelas)
```

``` r
suggestTAB<-suggested_parcelas$suggested
```

Comparing with the TDF reference (the idea is that has already been
reviewed in other validation run from other datasets)

``` r
load("../../otherData/TDF_taxonomicReference.RData")
notInTdfRef<-which(!getLowerTax(corrected_parcelas) %in% tdfTaxa$canonicalname)
correctNotInTdfRef<-intersect(suggestTAB$row,notInTdfRef)
```

Los casos que corresponden a correcciones automáticas,que no resultan en
taxones que ya se validaron en TDF y que no son simplemente “misplaced
qualifiers”:

``` r
kableExtra::kable(unique(suggestTAB[suggestTAB$row%in%correctNotInTdfRef & !suggestTAB$suggestDescription=="misplaced qualifiers for undetermined taxa",c("suggestDescription","family","genus","specificEpithet","suggest_family","suggest_genus","suggest_specificEpithet","suggest_verbatimTaxonRank")]))
```

|  | suggestDescription | family | genus | specificEpithet | suggest_family | suggest_genus | suggest_specificEpithet | suggest_verbatimTaxonRank |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| 93 | Comparing species information with gbif backbone (exactMatch_synonym) | Fabaceae | Piptadenia | flava | Fabaceae | Piptadenia | retusa | NA |
| 2292 | Comparing species information with gbif backbone (exactMatch_synonym) | Euphorbiaceae | Croton | rhamnifolius | Euphorbiaceae | Mallotus | rhamnifolius | NA |
| 2370 | Comparing species information with gbif backbone (exactMatch_synonym_changeHigherRanks) | Boraginaceae | Bourreria | cumanensis | Ehretiaceae | Bourreria | exsucca | NA |
| 4500 | Comparing species information with gbif backbone (fuzzyMatch_synonym) | Fabaceae | Lonchocarpus | sanctae-marthae | Fabaceae | Muellera | sanctae-marthae | NA |
| 8317 | cleaning space characters -\> Comparing species information with gbif backbone (exactMatch_synonym) | Rhamnaceae | Ziziphus | saeri | Rhamnaceae | Sarcomphalus | saeri | NA |

``` r
completeClassifparcelas<-extractCompleteTaxo(suggested_parcelas$analysedGbif,addLocalId = T,addAvailableAuthorship = T, getUnavailableAuthorship = T, getSynonyms = T)
finalTaxParcelas<-getLowerTax(corrected_parcelas)
casesToAdd<-extract(corrected_parcelas,"taxonRanks")[!is.na(finalTaxParcelas) &!finalTaxParcelas %in% completeClassifparcelas$canonicalname,]
stopifnot(nrow(casesToAdd)==0)
completeClassifparcelas$parent_localId <- completeClassifparcelas$localId[match(completeClassifparcelas$parent_gbifid , completeClassifparcelas$gbifid)]
```

### Sending taxonomy to the database

``` r
library(RPostgres)
pp_bst<-dbConnect(Postgres(),dbname="pp_bst_col")
dbExecute(pp_bst,"UPDATE main.taxo SET cd_accepted=cd_tax WHERE cd_accepted IS NULL")
```

    [1] 0

``` r
intoDB<-addClassifToDb(completeClassifparcelas, pp_bst)
corrected_parcelas$tax_localId <- completeClassifparcelas$localId [ match(finalTaxParcelas, completeClassifparcelas$canonicalname) ]
correctedParcelas<-addIdentifier(corrected_parcelas,"tax_localId",nameID = "Local taxon ID", typeID = "local", nameSource = "local R Session")
corrected_parcelas$cd_tax <- intoDB$final_cd_tax[match(corrected_parcelas$tax_localId, intoDB$localId)]
corrected_parcelas<-addIdentifier(corrected_parcelas,"cd_tax",nameID = "Database taxon ID", typeID="database", nameSource="pp_bst_col")
```

### Working on morpho-taxonomy

``` r
mTax<-getMorphoTaxo(corrected_parcelas)
w_mTax<-which(!is.na(mTax))
mTax<-mTax[w_mTax]
```

Morphotaxa need to be defined either for a project, a group of event or
an event, here we will use the projects, which correspond to the plots.

``` r
un_plots<-unique(corrected_parcelas$plot)
un_cd_projects<-unlist(sapply(paste0("SELECT cd_project FROM main.project WHERE project=",RPostgres::dbQuoteString(pp_bst,plots)),RPostgres::dbGetQuery, conn=pp_bst, USE.NAMES = F),use.names = F)
cd_projects<-un_cd_projects[match(corrected_parcelas$plot,un_plots)]
```

Morpho taxa must be defined with a type.

``` r
getMorphoType<-function(taxo)
{
  stopifnot(methods::is(taxo,"taxo_oneTab"))
  ATTR_mq <- attributes(taxo)$morphoQualifiers
  mqTab <- extract(taxo, "morphoQualifiers")
  nbQualifiers<-apply(mqTab, 1, function(x) sum(!is.na(x)))
  if(any(nbQualifiers>1))
  {stop("Some taxa have more than one qualifier (see extract(taxo, \"morphoQualifiers\")")}
  MT  <- rep(NA_character_, nrow(taxo) )
  if("sp_specif" %in% names(ATTR_mq))
  {
    colSp_specif<-ATTR_mq["sp_specif"]
    MT[grep("^spp?\\.? *$",taxo[,colSp_specif])]<-"Unspecified species"
    MT[grep("^[Ss]p ?\\.?[0-9]+",taxo[,colSp_specif])]<-"Numbered undetermined species"
    if(any(!is.na(taxo[,colSp_specif]) & is.na(MT))) {warning("Undertermined morpho-taxa types")}
  }
  if("cf_aff" %in% names(ATTR_mq))
  {
    colCf_aff<-ATTR_mq["cf_aff"]
    MT[!is.na(taxo[colCf_aff])]<-"Confere or affinity"
  }
  return(MT)
}
table(getMorphoType(corrected_parcelas))
```


    Numbered undetermined species           Unspecified species 
                              400                           185 

``` r
dbExecute(pp_bst,paste0("DELETE FROM main.morfo_taxo WHERE def_for_project IN (",paste(un_cd_projects,collapse=","),")"))
```

    [1] 0

``` r
morphoToSend<-unique(data.frame(cd_tax=corrected_parcelas$cd_tax,
           morphotaxon_type=getMorphoType(corrected_parcelas),
           name_morfo=getMorphoTaxo(corrected_parcelas),
           pseudo_rank=ifelse(grepl("species",getMorphoType(corrected_parcelas)),"SP",NA),
           verbatim_taxon_rank=corrected_parcelas$verbatimTaxonRank,
           def_for_project=cd_projects
           )[w_mTax,])
dbAppendTable(pp_bst,Id(schema="main",table="morfo_taxo"),morphoToSend)
```

    [1] 23

``` r
dbMT<-dbReadTable(pp_bst,Id(schema="main",table="morfo_taxo"))
dbToMatch<-as.matrix(dbMT[c("name_morfo","def_for_project")])
localMT<-as.matrix(data.frame(name_morfo=getMorphoTaxo(corrected_parcelas), def_for_project=cd_projects))
corrected_parcelas$cd_morfo<-dbMT$cd_morfo[match(split(localMT,row(localMT)),split(dbToMatch,row(dbToMatch)))]
#addIdentifier(corrected_parcelas,"cd_morfo",nameID = "Database Morphotaxon ID", typeID="Database",nameSource = "pp_bst_col")
```

### Determination

Determinations make sense at the individual case. Here are the
correspondance between rows and individuals:

``` r
matInd<-as.matrix(corrected_parcelas[c("plot","subplot","ind")])
ind_idx<-match(split(matInd,row(matInd)),split(matInd,row(matInd)))
tax_ind<-by(corrected_parcelas,ind_idx,function(tab)unique(tab[c("cd_tax","cd_morfo")]))
un_taxInd<-sapply(tax_ind,nrow)==1
sum(!un_taxInd)
```

    [1] 25

``` r
pb_ind<-as.numeric(names(un_taxInd)[!un_taxInd])
write.csv(x = corrected_parcelas[ind_idx %in% pb_ind,c("plot","subplot","ind","cd_tax","cd_morfo","genus","specificEpithet","verbatimTaxonRank")], file = "../../problemIndividualTaxo.csv")
```

There is a problem for various individuals which have more than one
taxon depending on the ramet.

``` r
majority<-function(mat)
{
  m<-match(split(mat,row(mat)),split(mat,row(mat)))
  return(as.numeric(names(sort(table(m),decreasing = T))[1]))
}
all(sapply(tax_ind,function(x)majority(as.matrix(x)))==1)
```

    [1] TRUE

## Inserting determination, register, individual and coordinates

``` r
ind_tab<-corrected_parcelas[unique(ind_idx),]
ind_tab$cd_project<-cd_projects[unique(ind_idx)]
ind_tab$cd_identif<-NA
ind_tab$cd_reg<-NA
ind_tab$cd_ind<-NA
dateIdentif<-as.Date("1/10/2025",format="%d/%m/%Y")
dbBegin(pp_bst)
for(i in 1:nrow(ind_tab))
{
  cd_identif <- RPostgres::dbGetQuery(pp_bst,paste0("INSERT INTO main.identification (cd_tax, cd_morfo, date_identif) VALUES(",
         dbQuoteLiteral(pp_bst,ind_tab$cd_tax[i]),",",
         dbQuoteLiteral(pp_bst,ind_tab$cd_morfo[i]),",",
         dbQuoteLiteral(pp_bst,dateIdentif),")
         RETURNING cd_identif")
  )$cd_identif
  cd_reg<-RPostgres::dbGetQuery(pp_bst,paste0("INSERT INTO main.register(cd_event, cd_identif, date_reg, qt_int)
         VALUES(",
         dbQuoteLiteral(pp_bst,ind_tab$cd_event[i]),",",
         dbQuoteLiteral(pp_bst,cd_identif),",",
         dbQuoteLiteral(pp_bst,ind_tab$measuring_date[i]),",",
         1,")
         RETURNING cd_reg")
         )$cd_reg
  if(!is.na(ind_tab$latitude_decimal[i])&!is.na(ind_tab$longitude_decimal[i]))
  {
    RPostgres::dbExecute(pp_bst,
    paste0("INSERT INTO main.register_location
           VALUES(",
           dbQuoteLiteral(pp_bst,cd_reg),",",
           "ST_Transform(ST_SetSRID(",
           "ST_MakePoint(",dbQuoteLiteral(pp_bst,ind_tab$longitude_decimal[i]),",",dbQuoteLiteral(pp_bst,ind_tab$latitude_decimal[i]),"),",dbQuoteLiteral(pp_bst,as.integer(CRS_orig)),"),",dbQuoteLiteral(pp_bst,as.integer(CRS)),"))"
           ))
  }
   cd_ind<-RPostgres::dbGetQuery(pp_bst,paste0("INSERT INTO main.individual(tag,uniq_in_project) VALUES(
         ", dbQuoteLiteral(pp_bst,ind_tab$ind[i]),",",# NOTE THIS WILL NOT ALWAYS WORK, WE MAY HAVE TO EXTRACT THE individual code from the tag
         dbQuoteLiteral(pp_bst,ind_tab$cd_project[i]),
         ") RETURNING cd_ind" 
         ))$cd_ind
  RPostgres::dbExecute(pp_bst,paste0("INSERT INTO main.reg_individual VALUES(",dbQuoteLiteral(pp_bst,cd_ind),",",dbQuoteLiteral(pp_bst,cd_reg),")"))
  ind_tab$cd_identif[i]<-cd_identif
  ind_tab$cd_reg[i]<-cd_reg
  ind_tab$cd_ind[i]<-cd_ind
}
dbCommit(pp_bst)
m<-match(ind_idx,unique(ind_idx))
corrected_parcelas$cd_identif<-ind_tab$cd_identif[m]
corrected_parcelas$cd_reg<-ind_tab$cd_reg[m]
corrected_parcelas$cd_ind<-ind_tab$cd_ind[m]
```

## Inserting subindividual

Inserting the ramet subpart:

``` r
subind_part <- RPostgres::dbReadTable(pp_bst,DBI::Id(schema="main",table="def_subindividual_part"))
if("ramet" %in% subind_part$part)
{
  cd_part <- subind_part$cd_part[subind_part$part=="ramet"]
} else {
  RPostgres::dbAppendTable(pp_bst, DBI::Id(schema="main",table="def_subindividual_part"),
                           data.frame(
                             part = "ramet",
                             cd_gp_biol ="arbo"
                           ))
  subind_part <- RPostgres::dbReadTable(pp_bst,DBI::Id(schema="main",table="def_subindividual_part"))
  cd_part <- subind_part$cd_part[subind_part$part=="ramet"]
}
```

Inserting the variables

``` r
vars<-RPostgres::dbGetQuery(pp_bst, "SELECT cd_var,name_var,org_lev FROM main.def_var dv LEFT JOIN main.def_organisation_level USING (cd_org_lev);")
if(any(vars$name_var=="dbh_cm" & vars$org_lev=="sub-individual characteristics"))
{
  cd_var_dbh_cm <- vars$cd_var[vars$name_var=="dbh_cm" & vars$org_lev=="sub-individual characteristics"]
} else{
  units<-RPostgres::dbGetQuery(pp_bst,"SELECT cd_unit, unit, measurement_type FROM main.def_unit LEFT JOIN main.def_measurement_type USING (cd_measurement_type)")
  if(any(units$unit=="centimeter" & units$measurement_type=="length"))
  {cd_unit_cm<-units$cd_unit[units$unit=="centimeter" & units$measurement_type=="length"]
  }else{
    cd_unit_cm<-dbGetQuery(pp_bst,paste0("INSERT INTO main.def_unit(cd_measurement_type,unit,unit_spa,abbv_unit,factor)
VALUES((SELECT cd_measurement_type FROM main.def_measurement_type WHERE measurement_type = 'length')",",",
RPostgres::dbQuoteLiteral(pp_bst,"centimeter"),",",
RPostgres::dbQuoteLiteral(pp_bst,"centímetro"),",",
RPostgres::dbQuoteLiteral(pp_bst,"cm"),",",
RPostgres::dbQuoteLiteral(pp_bst,0.001),
") RETURNING cd_unit"))$cd_unit
  }
  cd_var_dbh_cm<-dbGetQuery(pp_bst,paste0("INSERT INTO main.def_var(cd_unit,cd_org_lev,name_var,description,extra_var,extension_dwc,type_var,repeatable) VALUES(",
RPostgres::dbQuoteLiteral(pp_bst,cd_unit_cm),",
(SELECT cd_org_lev FROM main.def_organisation_level WHERE org_lev='sub-individual characteristics'),",
RPostgres::dbQuoteLiteral(pp_bst,"dbh_cm"),",",
RPostgres::dbQuoteLiteral(pp_bst,"Diameter at breast height"),",",
RPostgres::dbQuoteLiteral(pp_bst,T),",",
RPostgres::dbQuoteLiteral(pp_bst,'MeasurementOrFact'),",",
RPostgres::dbQuoteLiteral(pp_bst,"double precision"),",",
RPostgres::dbQuoteLiteral(pp_bst,F),")
RETURNING cd_var"
    ))$cd_var
}

if(any(vars$name_var=="height_m" & vars$org_lev=="sub-individual characteristics"))
{
  cd_var_height_m <- vars$cd_var[vars$name_var=="height_m" & vars$org_lev=="sub-individual characteristics"]
} else{
  units<-RPostgres::dbGetQuery(pp_bst,"SELECT cd_unit, unit, measurement_type FROM main.def_unit LEFT JOIN main.def_measurement_type USING (cd_measurement_type)")
  if(any(units$unit=="meter" & units$measurement_type=="length"))
  {cd_unit_m<-units$cd_unit[units$unit=="meter" & units$measurement_type=="length"]
  }else{
    cd_unit_m<-dbGetQuery(pp_bst,paste0("INSERT INTO main.def_unit(cd_measurement_type,unit,unit_spa,abbv_unit,factor)
VALUES((SELECT cd_measurement_type FROM main.def_measurement_type WHERE measurement_type = 'length')",",",
RPostgres::dbQuoteLiteral(pp_bst,"meter"),",",
RPostgres::dbQuoteLiteral(pp_bst,"metro"),",",
RPostgres::dbQuoteLiteral(pp_bst,"m"),",",
RPostgres::dbQuoteLiteral(pp_bst,1),
") RETURNING cd_unit"))$cd_unit
  }
  cd_var_height_m<-dbGetQuery(pp_bst,paste0("INSERT INTO main.def_var(cd_unit,cd_org_lev,name_var,description,extra_var,extension_dwc,type_var,repeatable) VALUES(",
RPostgres::dbQuoteLiteral(pp_bst,cd_unit_m),",
(SELECT cd_org_lev FROM main.def_organisation_level WHERE org_lev='sub-individual characteristics'),",
RPostgres::dbQuoteLiteral(pp_bst,"height_m"),",",
RPostgres::dbQuoteLiteral(pp_bst,"Height of a ramet"),",",
RPostgres::dbQuoteLiteral(pp_bst,T),",",
RPostgres::dbQuoteLiteral(pp_bst,'MeasurementOrFact'),",",
RPostgres::dbQuoteLiteral(pp_bst,"double precision"),",",
RPostgres::dbQuoteLiteral(pp_bst,F),")
RETURNING cd_var"
    ))$cd_var
}
```

``` r
corrected_parcelas$cd_project<-cd_projects
### TODO: manage that!
mat_ramet<-as.matrix(corrected_parcelas[c("plot","subplot","ind","ramet")])
corrected_parcelas<-corrected_parcelas[-which(!match(split(mat_ramet,row(mat_ramet)),split(mat_ramet,row(mat_ramet)))==1:nrow(mat_ramet)),]
####

mat_ramet<-as.matrix(corrected_parcelas[c("plot","subplot","ind","ramet")])
stopifnot(match(split(mat_ramet,row(mat_ramet)),split(mat_ramet,row(mat_ramet)))==1:nrow(mat_ramet))

corrected_parcelas$cd_subind<-NA
dbBegin(pp_bst)
for(i in 1:nrow(corrected_parcelas))
{
  cd_subind<-RPostgres::dbGetQuery(pp_bst,paste0(
    "INSERT INTO main.subindividual(cd_ind,cd_part,part_number,uniq_in_project,tag) VALUES(",
    RPostgres::dbQuoteLiteral(pp_bst,corrected_parcelas$cd_ind[i]),",",
    RPostgres::dbQuoteLiteral(pp_bst,cd_part),",",
    RPostgres::dbQuoteLiteral(pp_bst,corrected_parcelas$ramet[i]),",",
    RPostgres::dbQuoteLiteral(pp_bst,corrected_parcelas$cd_project[i]),",",
    RPostgres::dbQuoteLiteral(pp_bst,corrected_parcelas$individualID[i]),"
    ) RETURNING cd_subind"
  ))$cd_subind
  if(!is.na(corrected_parcelas$DBH_cm[i])){
  RPostgres::dbExecute(pp_bst,paste0(
    "INSERT INTO main.subindividual_characteristics(cd_reg,cd_subind,cd_var,subind_char_double)
    VALUES(",
    RPostgres::dbQuoteLiteral(pp_bst,corrected_parcelas$cd_reg[i]),",",
    RPostgres::dbQuoteLiteral(pp_bst,cd_subind),",",
    RPostgres::dbQuoteLiteral(pp_bst,cd_var_dbh_cm),",",
    RPostgres::dbQuoteLiteral(pp_bst,corrected_parcelas$DBH_cm[i]),
    ")"
  ))
  }
  if(!is.na(corrected_parcelas$height_m[i])){
  RPostgres::dbExecute(pp_bst,paste0(
    "INSERT INTO main.subindividual_characteristics(cd_reg,cd_subind,cd_var,subind_char_double)
    VALUES(",
    RPostgres::dbQuoteLiteral(pp_bst,corrected_parcelas$cd_reg[i]),",",
    RPostgres::dbQuoteLiteral(pp_bst,cd_subind),",",
    RPostgres::dbQuoteLiteral(pp_bst,cd_var_height_m),",",
    RPostgres::dbQuoteLiteral(pp_bst,corrected_parcelas$height_m[i]),
    ")"
  ))
  }
  corrected_parcelas$cd_subind[i] <- cd_subind
}
dbCommit(pp_bst)
```

``` r
save(corrected_parcelas,file="../../inserted_3parcelas_backup.RData")
```

## Shutting down light…

``` r
dbDisconnect(pp_bst)
```
