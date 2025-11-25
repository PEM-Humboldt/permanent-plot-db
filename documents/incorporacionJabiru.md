# Ejemplo de incorporación de datos a la base de datos: parcela Jabirú
Marius Bottin

``` r
DIR_DATA <- "../../otherData/"
dir(DIR_DATA)
```

     [1] "codigos_colecta_jabiru.csv"   "Dryflor_Marius.txt"          
     [3] "DwC_Jabiru.xlsx"              "DwC_LaPaz.xlsx"              
     [5] "DwC_Matitas.xlsx"             "DwC_Plato.xlsx"              
     [7] "Jabiru_str.xlsx"              "LaPaz.csv"                   
     [9] "Matitas.csv"                  "Plato.csv"                   
    [11] "TDF_taxonomicReference.RData"

``` r
library(openxlsx)
wb<-loadWorkbook(paste(DIR_DATA,"Jabiru_str.xlsx",sep="/"))
n_wb<-names(wb)
data_all<-lapply(n_wb,readWorkbook,xlsxFile=wb)
names(data_all)<-n_wb
data_all$codigos<-read.csv(paste(DIR_DATA,"codigos_colecta_jabiru.csv",sep="/"),sep=";")
data_all$Jabiru_str[data_all$Jabiru_str$Tag=="1021",]
```

|      | subplot | Tag  | ind   | ramet | family     | height_m | DBH_cm_ini | code     | plot   |
|:-----|--------:|:-----|:------|------:|:-----------|---------:|-----------:|:---------|:-------|
| 1111 |      37 | 1021 | 1021  |     1 | Meliaceae  |      6.8 |        4.3 | Triccari | Jabiru |
| 1112 |      37 | 1021 | 1021A |     2 | Annonaceae |      6.8 |        4.4 | Oxanespi | Jabiru |

``` r
data_all$Jabiru_str[data_all$Jabiru_str$ind=="1021A",]
```

|      | subplot | Tag  | ind   | ramet | family     | height_m | DBH_cm_ini | code     | plot   |
|:-----|--------:|:-----|:------|------:|:-----------|---------:|-----------:|:---------|:-------|
| 1112 |      37 | 1021 | 1021A |     2 | Annonaceae |      6.8 |        4.4 | Oxanespi | Jabiru |

``` r
data_all$Jabiru_str$ramet[data_all$Jabiru_str$ind=="1021A"]<-1
data_all$Jabiru_str$Tag[data_all$Jabiru_str$ind=="1021A"]<-"1021.1"
data_all$Jabiru_str$ind[data_all$Jabiru_str$ind=="1021A"]<-"1021.1"
data_all$Jabiru_str[data_all$Jabiru_str$Tag=="2610",]
```

|      | subplot | Tag  | ind   | ramet | family        | height_m | DBH_cm_ini | code     | plot   |
|:-----|--------:|:-----|:------|------:|:--------------|---------:|-----------:|:---------|:-------|
| 2809 |      96 | 2610 | 2610  |     1 | Malpighiaceae |       NA |        4.8 | Buncsp2  | Jabiru |
| 2810 |      96 | 2610 | 2610A |     2 | Annonaceae    |       NA |        3.7 | Oxanespi | Jabiru |

``` r
data_all$Jabiru_str[data_all$Jabiru_str$ind=="2610A",]
```

|      | subplot | Tag  | ind   | ramet | family     | height_m | DBH_cm_ini | code     | plot   |
|:-----|--------:|:-----|:------|------:|:-----------|---------:|-----------:|:---------|:-------|
| 2810 |      96 | 2610 | 2610A |     2 | Annonaceae |       NA |        3.7 | Oxanespi | Jabiru |

``` r
data_all$Jabiru_str$ramet[data_all$Jabiru_str$ind=="2610A"]<-1
data_all$Jabiru_str$Tag[data_all$Jabiru_str$ind=="2610A"]<-"2610.1"
data_all$Jabiru_str$ind[data_all$Jabiru_str$ind=="2610A"]<-"2610.1"
```

``` r
data_all$codigos$tag_suggested[!is.na(data_all$codigos$tag_suggested)] %in% data_all$Jabiru_str$Tag
```

    [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
data_all$codigos$tag[!is.na(data_all$codigos$tag)] %in% data_all$Jabiru_str$Tag
```

     [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    [46] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    [61] TRUE TRUE TRUE TRUE TRUE

``` r
data_all$Eventos$register_date<-as.Date(data_all$Eventos$register_date,origin = "1899-12-30")
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
```

``` r
spat_metadata<-st_as_sf(data_all$Eventos,coords=c("longitude_dec","latitude_dec"))
spat_metadata<-st_transform(st_set_crs(spat_metadata,CRS_orig),CRS)
st_write(obj=spat_metadata,dsn=pp_bst,layer=DBI::Id(schema="tmp", table="metadata"), delete_layer=T)
```

    Note: method with signature 'DBIObject#sf' chosen for function 'dbDataType',
     target signature 'PqConnection#sf'.
     "PqConnection#ANY" would also be valid

### Building the plot polygons

En este caso particular tenemos que construir los objetos espaciales que
se van a referenciar.

``` r
RPostgres::dbGetQuery(pp_bst,"

    SELECT plot,ST_X(geometry) min_x, ST_Y(geometry) min_y, ST_X(geometry)+100 max_x,ST_Y(geometry)+100 max_y
    FROM tmp.metadata
;")
```

| plot   |    min_x |   min_y |    max_x |   max_y |
|:-------|---------:|--------:|---------:|--------:|
| Jabiru | 916554.9 | 1051336 | 916654.9 | 1051436 |

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
    SELECT plot,ST_X(geometry) min_x, ST_Y(geometry) min_y, ST_X(geometry)+100 max_x,ST_Y(geometry)+100 max_y
    FROM tmp.metadata
)
SELECT 
  plot,
  ST_SetSRID(ST_makePolygon(ST_MakeLine(ARRAY[st_makepoint(min_x,min_y),ST_MakePoint(min_x,max_y), ST_MakePoint(max_x,max_y), ST_MakePoint(max_x,min_y), ST_MakePoint(min_x,min_y)])),3116) geom
FROM a
)")
```

    [1] 1

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
    SELECT plot,ST_X(geometry) min_x, ST_Y(geometry) min_y, ST_X(geometry)+100 max_x,ST_Y(geometry)+100 max_y
    FROM tmp.metadata
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

    [1] 121

``` r
max_x<-10
max_y<-10
RPostgres::dbExecute(pp_bst,
paste0("DELETE FROM tmp.spatevent
WHERE rank_x > ",max_x," OR rank_y > ",max_y))
```

    [1] 21

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

    [1] 100

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

    [1] 1

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

    [1] 100

``` r
RPostgres::dbExecute(pp_bst,"DELETE FROM main.project WHERE project IN (SELECT plot FROM tmp.spatproject)")
```

    [1] 1

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

    [1] 1

## Inserting gp_events

``` r
(tab_gp_event<-RPostgres::dbGetQuery(conn=pp_bst, 
"WITH a AS(
SELECT DISTINCT cd_project, cd_loc, 'arbo' cd_gp_biol, a.cd_method, register_date date_begin, register_date date_end
FROM tmp.metadata cp
LEFT JOIN main.project p ON p.project=cp.plot
CROSS JOIN (SELECT cd_method, method FROM main.def_method WHERE method='pp1_census0') a
)
SELECT a.*, ROW_NUMBER() OVER (PARTITION BY cd_project ORDER BY date_begin) AS compaign_nb
FROM a
"))
```

| cd_project | cd_loc | cd_gp_biol | cd_method | date_begin | date_end   | compaign_nb |
|-----------:|-------:|:-----------|----------:|:-----------|:-----------|------------:|
|        334 |    634 | arbo       |         2 | 2015-07-17 | 2015-07-17 |           1 |

``` r
stopifnot(table(tab_gp_event$cd_project)==1)
RPostgres::dbExecute(pp_bst,
"INSERT INTO main.gp_event(cd_project, cd_loc, cd_gp_biol, cd_method, date_begin, date_end, campaign_nb)
WITH a AS(
SELECT DISTINCT cd_project, cd_loc, 'arbo' cd_gp_biol, a.cd_method, register_date date_begin, register_date date_end
FROM tmp.metadata cp
LEFT JOIN main.project p ON p.project=cp.plot
CROSS JOIN (SELECT cd_method, method FROM main.def_method WHERE method='pp1_census0') a
)
SELECT a.*, ROW_NUMBER() OVER (PARTITION BY cd_project ORDER BY date_begin) AS compaign_nb
FROM a
")
```

    [1] 1

## Inserting events

``` r
RPostgres::dbExecute(pp_bst, 
"WITH a AS(
  SELECT p.project, cd_gp_event, date_begin date_gp_event
  FROM main.gp_event ge
  LEFT JOIN main.project p USING (cd_project)
  WHERE p.project IN (SELECT plot FROM tmp.metadata)
)
INSERT INTO main.event(event_id, cd_gp_event, num_replicate, description_replicate, date_begin, date_end, cd_loc)
SELECT project||'_census0_subplot'||subplot, cd_gp_event, subplot, 'subplot '||subplot||'/100', date_gp_event, date_gp_event, cd_loc
FROM a
CROSS JOIN (SELECT generate_series(1,100) subplot) b 
LEFT JOIN main.location ON location=a.project||': subplot '||b.subplot
ORDER BY cd_gp_event, subplot
")
```

    [1] 100

## Inserting people

``` r
if(RPostgres::dbExistsTable(pp_bst,DBI::Id(schema="tmp",table="people")))
{
  RPostgres::dbRemoveTable(pp_bst,DBI::Id(schema="tmp", table="people"))
}
RPostgres::dbWriteTable(pp_bst,DBI::Id(schema="tmp", table="people"), data_all$Miembros)
```

``` r
RPostgres::dbExecute(pp_bst,
"INSERT INTO main.people(verbatim_person, first_name1, first_name2, family_name1, family_name2, preferred_complete)
SELECT  DISTINCT name_publi verbatim_person, f1, f2, l1, l2, name_publi verbatim_person
FROM tmp.people tp 
LEFT JOIN main.people mp ON tp.name_publi=mp.preferred_complete
WHERE mp.cd_person IS NULL")
```

    [1] 12

``` r
RPostgres::dbExecute(pp_bst,
"INSERT INTO main.organization(org_name, org_abbrev)
SELECT  DISTINCT institution, abbv_inst
FROM tmp.people tp 
LEFT JOIN main.organization mo ON tp.abbv_inst=mo.org_abbrev
WHERE mo.cd_org IS NULL")
```

    [1] 3

``` r
RPostgres::dbExecute(pp_bst,"
WITH a AS (DELETE FROM main.people_role WHERE cd_project= (SELECT cd_project FROM main.project WHERE project='Jabiru'))
INSERT INTO main.people_role (cd_person, cd_org, role, date_apply, cd_project)
SELECT DISTINCT mp.cd_person, mo.cd_org, REPLACE(role_in_field,'Principle', 'Principal'), (SELECT MIN(date_begin) FROM main.gp_event LEFT JOIN main.project USING (cd_project) WHERE project='Jabiru') date_apply, (SELECT cd_project FROM main.project WHERE project='Jabiru') cd_project
FROM tmp.people tp
LEFT JOIN main.organization mo ON tp.abbv_inst=mo.org_abbrev
LEFT JOIN main.people mp ON tp.name_publi=mp.preferred_complete
")
```

    [1] 15

``` r
RPostgres::dbExecute(pp_bst,"ALTER TABLE tmp.people ADD COLUMN IF NOT EXISTS cd_tp serial, ADD COLUMN IF NOT EXISTS cd_people_role int, ADD COLUMN IF NOT EXISTS det_by boolean, ADD COLUMN IF NOT EXISTS rec_by boolean;")
```

    [1] 0

``` r
RPostgres::dbExecute(pp_bst,
"WITH a AS(
SELECT cd_tp, pr.cd_people_role, pr.role='Botanist' AS det_by, tp.sampling='stablishment' AS rec_by
FROM tmp.people tp
LEFT JOIN main.organization mo ON tp.abbv_inst=mo.org_abbrev
LEFT JOIN main.people mp ON tp.name_publi=mp.preferred_complete
LEFT JOIN main.people_role pr ON mp.cd_person=pr.cd_person AND mo.cd_org=pr.cd_org AND pr.role=REPLACE(role_in_field,'Principle', 'Principal')
)
UPDATE tmp.people tp
SET cd_people_role=a.cd_people_role, det_by=a.det_by, rec_by=a.rec_by
FROM a
WHERE tp.cd_tp=a.cd_tp")
```

    [1] 21

## Taxonomy

### Getting suggestions and validation from rdsTaxVal

``` r
library(rdsTaxVal)
head(data_all$Taxonomy)
```

| verbatimIdentification | Code | tag | recordNumber | leaf_phenology | life_form | taxonRank | kingdom | phylum | class | order | family | genus | specificEpithet | verbatimTaxonRank | scientificName | scientificNameAuthorship |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Ampelocera sp1 | Ampesp1 | 79 \| 184 \| 204 \| 847 \| 1873 | JAC-2154 \| JAC-2173 \| JAC-2185 \| JAC-2195 \| JAC-2213 | evergreen | tree | GENUS | Plantae | Tracheophyta | Magnoliopsida | Rosales | Cannabaceae | Ampelocera | NA | sp1 | Ampelocera | Klotzsch |
| Amyris pinnata | Amyrpinn | 293 | JAC-2164 | evergreen | tree | SPECIES | Plantae | Tracheophyta | Magnoliopsida | Sapindales | Rutaceae | Amyris | pinnata | NA | Amyris pinnata | Kunth |
| Anredera floribunda | Anreflor | 747 | JAC-2196 \| JAC-2193 | deciduous | liana | SPECIES | Plantae | Tracheophyta | Magnoliopsida | Caryophyllales | Basellaceae | Anredera | floribunda | NA | Anredera floribunda | (Moq.) Sperling |
| Asclepias sp | Asclsp | 1819 | JAC-2203 | evergreen | forb | GENUS | Plantae | Tracheophyta | Magnoliopsida | Gentianales | Apocynaceae | Asclepias | NA | sp | Asclepias | L. |
| Aspidosperma polyneuron | Aspipoly | 1244 | JAC-2200 | evergreen | tree | SPECIES | Plantae | Tracheophyta | Magnoliopsida | Gentianales | Apocynaceae | Aspidosperma | polyneuron | NA | Aspidosperma polyneuron | M√ºll.Arg. |
| Astronium graveolens | Astrgrav | 98 \| 270 \| 808 \| 1912 | JAC-2150 \| JAC-2168 \| JAC-2194 \| JAC-2217 | deciduous | tree | SPECIES | Plantae | Tracheophyta | Magnoliopsida | Sapindales | Anacardiaceae | Astronium | graveolens | NA | Astronium graveolens | Jacq. |

``` r
any(duplicated(data_all$Taxonomy$Code))
```

    [1] FALSE

``` r
any(is.na(data_all$Taxonomy$Code))
```

    [1] FALSE

``` r
taxo_toCheck<-rdsTaxVal::new_taxo_oneTab(data_all$Taxonomy,currentFormat="oneTable",taxonRanks_names = c(kingdom="kingdom",phylum="phylum",class="class",order="order",family="family",genus="genus",species="specificEpithet"),taxonRanks_epithetized = c("specificEpithet"),taxoCode = "Code",morphoQualifiers = c(sp_specif="verbatimTaxonRank"))
suggested<-fullTaxonomicDiagnostic(taxo_toCheck)
```

    cleaning space characters

    misplaced qualifiers for undetermined taxa

    unicity of genus in family

    unicity of family in order

    unicity of order in class

    unicity of class in phylum

    unicity of phylum in kingdom

    checking for unicity of taxonomic information associated with taxonomic code

    Comparing species information with gbif backbone

    Searching for 30 taxa in the GBIF Backbone
    ...

    done

    Analysing GBIF Backbone information

    30 taxa are found without any modification needed

    0 taxa are found with suggested orthographic changes

    0 taxa are suggested synonyms

    0 taxa are found with suggested higher rank changes

    0 taxa were not found

    Comparing genus information with gbif backbone

    Searching for 13 taxa in the GBIF Backbone
    ...

    done

    Analysing GBIF Backbone information

    13 taxa are found without any modification needed

    0 taxa are found with suggested orthographic changes

    0 taxa are suggested synonyms

    0 taxa are found with suggested higher rank changes

    0 taxa were not found

    Comparing family information with gbif backbone

    Searching for 1 taxa in the GBIF Backbone
    ...

    done

    Analysing GBIF Backbone information

    1 taxa are found without any modification needed

    0 taxa are found with suggested orthographic changes

    0 taxa are suggested synonyms

    0 taxa are found with suggested higher rank changes

    0 taxa were not found

    Comparing class information with gbif backbone

    Searching for 1 taxa in the GBIF Backbone
    ...

    done

    Analysing GBIF Backbone information

    1 taxa are found without any modification needed

    0 taxa are found with suggested orthographic changes

    0 taxa are suggested synonyms

    0 taxa are found with suggested higher rank changes

    0 taxa were not found

``` r
suggested$suggested
```

| id_suggest | row | suggestDescription | Code | kingdom | phylum | class | order | family | genus | specificEpithet | verbatimTaxonRank | gbifid |
|---:|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|

``` r
completeClassifparcelas<-extractCompleteTaxo(suggested$analysedGbif,addLocalId = T,addAvailableAuthorship = T, getUnavailableAuthorship = T, getSynonyms = F)
finalTaxParcelas<-getLowerTax(taxo_toCheck)
completeClassifparcelas$parent_localId <- completeClassifparcelas$localId[match(completeClassifparcelas$parent_gbifid , completeClassifparcelas$gbifid)]
completeClassifparcelas$acc_localId<-completeClassifparcelas$localId
```

### Sending taxonomy to the database

``` r
library(RPostgres)
intoDB<-addClassifToDb(completeClassifparcelas, pp_bst)
taxo_toCheck$tax_localId <-completeClassifparcelas$localId[match(finalTaxParcelas, completeClassifparcelas$canonicalname)]
taxo_toCheck<-addIdentifier(taxo_toCheck,"tax_localId",nameID = "Local taxon ID", typeID = "local", nameSource = "local R Session")
taxo_toCheck$cd_tax <- intoDB$final_cd_tax[match(taxo_toCheck$tax_localId, intoDB$localId)]
taxo_toCheck<-addIdentifier(taxo_toCheck,"cd_tax",nameID = "Database taxon ID", typeID="database", nameSource="pp_bst_col")
```

### Working on morpho-taxonomy

``` r
mTax<-getMorphoTaxo(taxo_toCheck)
w_mTax<-which(!is.na(mTax))
mTax<-mTax[w_mTax]
```

Morphotaxa need to be defined either for a project, a group of event or
an event, here we will use the projects, which correspond to the plots.

``` r
un_plots<-unique(spat_metadata$plot)
un_cd_projects<-unlist(sapply(paste0("SELECT cd_project FROM main.project WHERE project=",RPostgres::dbQuoteString(pp_bst,un_plots)),RPostgres::dbGetQuery, conn=pp_bst, USE.NAMES = F),use.names = F)
cd_projects<-un_cd_projects[match(spat_metadata$plot,un_plots)]
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
table(getMorphoType(taxo_toCheck))
```


    Numbered undetermined species           Unspecified species 
                               12                             5 

``` r
RPostgres::dbExecute(pp_bst,paste0("DELETE FROM main.morfo_taxo WHERE def_for_project IN (",paste(un_cd_projects,collapse=","),")"))
```

    [1] 0

``` r
morphoToSend<-unique(data.frame(cd_tax=taxo_toCheck$cd_tax,
           morphotaxon_type=getMorphoType(taxo_toCheck),
           name_morfo=getMorphoTaxo(taxo_toCheck),
           pseudo_rank=ifelse(grepl("species",getMorphoType(taxo_toCheck)),"SP",NA),
           verbatim_taxon_rank=taxo_toCheck$verbatimTaxonRank,
           def_for_project=cd_projects
           )[w_mTax,])
RPostgres::dbAppendTable(pp_bst,DBI::Id(schema="main",table="morfo_taxo"),morphoToSend)
```

    [1] 17

``` r
dbMT<-RPostgres::dbReadTable(pp_bst,DBI::Id(schema="main",table="morfo_taxo"))
dbToMatch<-as.matrix(dbMT[c("name_morfo","def_for_project")])
localMT<-as.matrix(data.frame(name_morfo=getMorphoTaxo(taxo_toCheck), def_for_project=cd_projects))
taxo_toCheck$cd_morfo<-dbMT$cd_morfo[match(split(localMT,row(localMT)),split(dbToMatch,row(dbToMatch)))]
#addIdentifier(corrected_parcelas,"cd_morfo",nameID = "Database Morphotaxon ID", typeID="Database",nameSource = "pp_bst_col")
```

<!-- HERE -->

### Determination

Determinations make sense at the individual case. Here are the
correspondance between rows and individuals:

``` r
data_all$codigos$tag_final<-ifelse(is.na(data_all$codigos$tag),data_all$codigos$tag_suggested,data_all$codigos$tag)
m<-match(data_all$codigos$tag_final,data_all$Jabiru_str$Tag)
#stopifnot(data_all$Jabiru_str$code[m]==data_all$codigos$code)
which(!data_all$Jabiru_str$code[m]==data_all$codigos$code)
```

    [1] 47

``` r
all_str<-data.frame(data_all$Jabiru_str,
  taxo_toCheck[match(data_all$Jabiru_str$code,taxo_toCheck$Code),c("cd_tax","cd_morfo")]
)
all_str$recordNumber<-NA
all_str$recordNumber[m]<-data_all$codigos$recordNumber
stopifnot(is.character(all_str$ind))
matInd<-as.matrix(all_str[c("plot","subplot","Tag")])
ind_idx<-match(split(matInd,row(matInd)),split(matInd,row(matInd)))
tax_ind<-by(all_str,ind_idx,function(tab)unique(tab[c("cd_tax","cd_morfo")]))
un_taxInd<-sapply(tax_ind,nrow)==1
sum(!un_taxInd)
```

    [1] 0

``` r
pb_ind<-as.numeric(names(un_taxInd)[!un_taxInd])
stopifnot(length(pb_ind)==0)
#write.csv(x = all_str[ind_idx %in% pb_ind,c("plot","subplot","Tag","ind","code","cd_tax","cd_morfo")], file = "../../Jabiru_problemIndividualTaxo.csv")
```

## Inserting determination, register, individual and coordinates

``` r
det_by<-RPostgres::dbGetQuery(pp_bst,"SELECT 'ARRAY['||STRING_AGG(DISTINCT cd_people_role::text,',')||']' det_by FROM tmp.people WHERE det_by")$det_by
rec_by<-RPostgres::dbGetQuery(pp_bst,"SELECT 'ARRAY['||STRING_AGG(DISTINCT cd_people_role::text,',')||']' rec_by FROM tmp.people WHERE rec_by")$rec_by

all_str$recordNumber[all_str$recordNumber==""]<-NA
matInd<-as.matrix(all_str[c("plot","subplot","Tag")])
ind_idx<-match(split(matInd,row(matInd)),split(matInd,row(matInd)))
ind_tab<-all_str[unique(ind_idx),]
ind_tab$measuring_date<-data_all$Eventos$register_date
ind_tab$cd_project<-cd_projects
ind_tab$cd_identif<-NA
ind_tab$cd_reg<-NA
ind_tab$cd_ind<-NA
all_cd_events<-RPostgres::dbGetQuery(pp_bst, "SELECT REGEXP_REPLACE(event_id,'^.+subplot([0-9]+)$','\\1','')::int subplot, cd_event FROM main.event WHERE event_id ~* 'Jabiru'")
ind_tab$cd_event<-all_cd_events$cd_event[match(ind_tab$subplot,all_cd_events$subplot)]
dateIdentif<-as.Date("01/02/2016",format="%d/%m/%Y")
stopifnot(sum(!is.na(ind_tab$recordNumber)) == sum(!is.na(all_str$recordNumber)))
dbBegin(pp_bst)
for(i in 1:nrow(ind_tab))
{
  cd_reg<-RPostgres::dbGetQuery(pp_bst,paste0("INSERT INTO main.register(cd_event, date_reg, qt_int,cds_recorded_by)
         VALUES(",
         dbQuoteLiteral(pp_bst,ind_tab$cd_event[i]),",",
         dbQuoteLiteral(pp_bst,ind_tab$measuring_date[i]),",",
         1,",",rec_by,")
         RETURNING cd_reg")
         )$cd_reg
  cd_identif <- RPostgres::dbGetQuery(pp_bst,paste0("INSERT INTO main.identification (cd_reg,cd_tax, cd_morfo, date_identif, catalog_id, identified_by) VALUES(",
         dbQuoteLiteral(pp_bst,cd_reg),",",
         dbQuoteLiteral(pp_bst,ind_tab$cd_tax[i]),",",
         dbQuoteLiteral(pp_bst,ind_tab$cd_morfo[i]),",",
         dbQuoteLiteral(pp_bst,dateIdentif),",",
         dbQuoteLiteral(pp_bst,ind_tab$recordNumber[i]),",",
         det_by,
         ")
         RETURNING cd_identif")
  )$cd_identif
#  if(!is.na(ind_tab$latitude_decimal[i])&!is.na(ind_tab$longitude_decimal[i]))
#  {
#    RPostgres::dbExecute(pp_bst,
#    paste0("INSERT INTO main.register_location
#           VALUES(",
#           dbQuoteLiteral(pp_bst,cd_reg),",",
#           "ST_Transform(ST_SetSRID(",
#           "ST_MakePoint(",dbQuoteLiteral(pp_bst,ind_tab$longitude_decimal[i]),",",dbQuoteLiteral(pp_bst,ind_tab$latitude_decimal[i]),"),",dbQuoteLiteral(pp_bst,as.integer(CRS_orig)),"),",dbQuoteLiteral(pp_bst,as.integer(CRS)),"))"
#           ))
#  }
   cd_ind<-RPostgres::dbGetQuery(pp_bst,paste0("INSERT INTO main.individual(tag,uniq_in_project) VALUES(
         ", dbQuoteLiteral(pp_bst,ind_tab$Tag[i]),",",# NOTE THIS WILL NOT ALWAYS WORK, WE MAY HAVE TO EXTRACT THE individual code from the tag
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
all_str$cd_identif<-ind_tab$cd_identif[m]
all_str$cd_reg<-ind_tab$cd_reg[m]
all_str$cd_ind<-ind_tab$cd_ind[m]
all_str$cd_project<-ind_tab$cd_project[m]
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
all_str$cd_project<-cd_projects

mat_ramet<-as.matrix(all_str[c("plot","subplot","ind","ramet")])
stopifnot(match(split(mat_ramet,row(mat_ramet)),split(mat_ramet,row(mat_ramet)))==1:nrow(mat_ramet))

all_str$cd_subind<-NA
dbBegin(pp_bst)
for(i in 1:nrow(all_str))
{
  cd_subind<-RPostgres::dbGetQuery(pp_bst,paste0(
    "INSERT INTO main.subindividual(cd_ind,cd_part,part_number,uniq_in_project,tag) VALUES(",
    RPostgres::dbQuoteLiteral(pp_bst,all_str$cd_ind[i]),",",
    RPostgres::dbQuoteLiteral(pp_bst,cd_part),",",
    RPostgres::dbQuoteLiteral(pp_bst,all_str$ramet[i]),",",
    RPostgres::dbQuoteLiteral(pp_bst,all_str$cd_project[i]),",",
    RPostgres::dbQuoteLiteral(pp_bst,all_str$ind[i]),"
    ) RETURNING cd_subind"
  ))$cd_subind
  if(!is.na(all_str$DBH_cm[i])){
  RPostgres::dbExecute(pp_bst,paste0(
    "INSERT INTO main.subindividual_characteristics(cd_reg,cd_subind,cd_var,subind_char_double)
    VALUES(",
    RPostgres::dbQuoteLiteral(pp_bst,all_str$cd_reg[i]),",",
    RPostgres::dbQuoteLiteral(pp_bst,cd_subind),",",
    RPostgres::dbQuoteLiteral(pp_bst,cd_var_dbh_cm),",",
    RPostgres::dbQuoteLiteral(pp_bst,all_str$DBH_cm_ini[i]),
    ")"
  ))
  }
  if(!is.na(all_str$height_m[i])){
  RPostgres::dbExecute(pp_bst,paste0(
    "INSERT INTO main.subindividual_characteristics(cd_reg,cd_subind,cd_var,subind_char_double)
    VALUES(",
    RPostgres::dbQuoteLiteral(pp_bst,all_str$cd_reg[i]),",",
    RPostgres::dbQuoteLiteral(pp_bst,cd_subind),",",
    RPostgres::dbQuoteLiteral(pp_bst,cd_var_height_m),",",
    RPostgres::dbQuoteLiteral(pp_bst,all_str$height_m[i]),
    ")"
  ))
  }
  all_str$cd_subind[i] <- cd_subind
}
dbCommit(pp_bst)
```

## Creating the occurrenceID

``` sql
WITH a AS(
SELECT r.cd_reg,
  'ObservacionHumana' AS basis_of_record
  /*CASE
    WHEN COALESCE(voucher, catalog_id) IS NULL THEN 'ObservacionHumana'
    ELSE 'PreservedSpecimen'
    END basis_of_record*/
FROM main.register r
LEFT JOIN main.identification i USING (cd_reg)
)
SELECT cd_reg, project, 'IAvH:'||UPPER(basis_of_record)||':'||UPPER(project)||'_CENSUS0:'||LPAD( (ROW_NUMBER() OVER (PARTITION BY cd_project ORDER BY cd_project,cd_reg))::text,4,'0') occurrence_id
FROM main.register r
LEFT JOIN main.event e USING (cd_event)
LEFT JOIN main.gp_event ge USING (cd_gp_event)
LEFT JOIN main.project p USING (cd_project)
LEFT JOIN a USING(cd_reg)
WHERE project IN ('Jabiru')
```

| cd_reg | project | occurrence_id                              |
|:-------|:--------|:-------------------------------------------|
| 6366   | Jabiru  | IAvH:OBSERVACIONHUMANA:JABIRU_CENSUS0:0001 |
| 6367   | Jabiru  | IAvH:OBSERVACIONHUMANA:JABIRU_CENSUS0:0002 |
| 6368   | Jabiru  | IAvH:OBSERVACIONHUMANA:JABIRU_CENSUS0:0003 |
| 6369   | Jabiru  | IAvH:OBSERVACIONHUMANA:JABIRU_CENSUS0:0004 |
| 6370   | Jabiru  | IAvH:OBSERVACIONHUMANA:JABIRU_CENSUS0:0005 |
| 6371   | Jabiru  | IAvH:OBSERVACIONHUMANA:JABIRU_CENSUS0:0006 |
| 6372   | Jabiru  | IAvH:OBSERVACIONHUMANA:JABIRU_CENSUS0:0007 |
| 6373   | Jabiru  | IAvH:OBSERVACIONHUMANA:JABIRU_CENSUS0:0008 |
| 6374   | Jabiru  | IAvH:OBSERVACIONHUMANA:JABIRU_CENSUS0:0009 |
| 6375   | Jabiru  | IAvH:OBSERVACIONHUMANA:JABIRU_CENSUS0:0010 |

Displaying records 1 - 10

``` r
reg_occurrenceId<-dbGetQuery(pp_bst,"
WITH a AS(
SELECT cd_reg, project, 'IAvH:'||'OBSERVACIONHUMANA'||':'||UPPER(project)||'_CENSUS0:'||LPAD( (ROW_NUMBER() OVER (PARTITION BY cd_project ORDER BY cd_project,cd_reg))::text,4,'0') occurrence_id
FROM main.register r
LEFT JOIN main.event e USING (cd_event)
LEFT JOIN main.gp_event ge USING (cd_gp_event)
LEFT JOIN main.project p USING (cd_project)
WHERE project IN ('Jabiru')
)UPDATE main.register AS r
SET occurrence_id=a.occurrence_id
FROM a
WHERE a.cd_reg=r.cd_reg AND r.occurrence_id IS NULL
RETURNING r.cd_reg, r.occurrence_id")
stopifnot(all(is.na(all_str$occurrenceID)))
all_str$occurrenceID<-reg_occurrenceId$occurrence_id[match(all_str$cd_reg,reg_occurrenceId$cd_reg)]
```

``` r
save(all_str,file="../../inserted_Jabiru.RData")
```

## Shutting down light…

``` r
dbDisconnect(pp_bst)
```
