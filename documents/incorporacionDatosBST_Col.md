# Ejemplo de incorporación de datos a la base de datos: Red BST-Col
Marius Bottin

El equipo de investigación del Centro de Estudios Socio-ecológicos y
Cambio Global utiliza un formato interno de R (formato RDS) para
almacenar los datos de parcelas permanentes según un esquema de bases de
datos relacionales. Si bien el lenguaje R no permite directamente
imponer reglas de relaciones entre tablas, la lógica interna es la misma
que en una base de datos en SQL, con llaves primarias y foráneas.

En este documento, vamos a mostrar como se pueden incorporar los datos
de la red BST-Col en la base de datos SQL. Haciendo este ejercicio de
incorporación, van a aparecer preguntas y dudas sobre los datos.

## Crear y incializar la base de datos

``` r
require(RPostgres)
```

    Loading required package: RPostgres

``` r
dbname <- "pp_bst_col"
source("../db_creation/reinitializeDB.R")
```

    Warning in eval(ei, envir): database pp_bst_col will be reinitialized, if this
    is not what you want, please do something, fast!

## Leer el archivo

En la carpeta compartida, hay varios archivos RDS:

``` r
dirRDS<-"../../data_google/4.rdsProyectos/"
dir(dirRDS)
```

    [1] "dataCerrejon.rds"      "dataDry4.rds"          "dataIFN-Orinoquia.rds"
    [4] "dataQuimbo.rds"        "dataRedBSTCol.rds"     "dataTDF.rds"          
    [7] "gadm36_COL_1_sp.rds"  

Vamos a enforcarnos acá en los datos de la red de Bosques Secos
Tropicales de Colombia (BST-Col).

``` r
rdsBST<-readRDS("../../data_google/4.rdsProyectos/dataRedBSTCol.rds")
```

## Manejar proyectos y localizaciones de proyectos

En la base de datos biologicas, para razones explicadas
[acá](%22./dataArchitecture.md%22), cada una de las parcelas permanentes
debe estar entrada como un proyecto.

``` r
names(rdsBST)
```

    [1] "metadata" "taxonomy" "censuses" "dates"    "members" 

``` r
metadata_tot<-Reduce(rbind,rdsBST$metadata)
head(metadata_tot,n = 5)
```

|  | registerDate | latitude_dec | longitude_dec | altitude_m | area_ha | areaType | plotType | shapeType | length_m | width_m | subplotArea_ha | region | ecosystem | locationName | locationType | state | country | province | terrainType | substrateGeology | generalSlope_deg | forestComposition | forestStatus | forestAge_yrs | nearestAnthropogenicEdge_m | fragmentSize_ha | comments |
|:---|:---|---:|---:|---:|---:|:---|:---|:---|---:|---:|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|:---|
| AltoSanJorgeInicial | 2020-01-10 | 11.16783 | -73.43339 | 119 | 0.1 | Projected horizontal surface | Permanent | rectangle | 50 | 20 | 0.01 | Caribbean | tropical dry forests | Hamlet Alto de San Jorge |  | Guajira | Colombia | Dibulla | Lowland |  |  | Mixed forest | Early |  | NA | NA | All data and sources associated are available for consulting and potential uses in analysis and publications. But, as normativity of Red BST-Col, in all cases Principle Researchers should contacted for correct use-credits of the information |
| AltoSanJorgeT1P1 | 2020-01-08 | 11.17416 | -73.43469 | 152 | 0.1 | Projected horizontal surface | Permanent | rectangle | 50 | 20 | 0.01 | Caribbean | tropical dry forests | Private land |  | La Guajira | Colombia | Alto San Jorge | Lowland |  |  | Mixed forest | Secondary forests |  | NA | NA | All data and sources associated are available for consulting and potential uses in analysis and publications. But, as normativity of Red BST-Col, in all cases Principle Researchers should contacted for correct use-credits of the information |
| AltoSanJorgeT1P2 | 2020-01-08 | 11.16858 | -73.43082 | 106 | 0.1 | Projected horizontal surface | Permanent | rectangle | 50 | 20 | 0.01 | Caribbean | tropical dry forests | Private land |  | La Guajira | Colombia | Alto San Jorge | Lowland |  |  | Mixed forest | Secondary forests |  | NA | NA | All data and sources associated are available for consulting and potential uses in analysis and publications. But, as normativity of Red BST-Col, in all cases Principle Researchers should contacted for correct use-credits of the information |
| AltoSanJorgeT1P3 | 2020-01-08 | 11.16772 | -73.44295 | 153 | 0.1 | Projected horizontal surface | Permanent | rectangle | 50 | 20 | 0.01 | Caribbean | tropical dry forests | Private land |  | La Guajira | Colombia | Alto San Jorge | Lowland |  |  | Mixed forest | Secondary forests |  | NA | NA | All data and sources associated are available for consulting and potential uses in analysis and publications. But, as normativity of Red BST-Col, in all cases Principle Researchers should contacted for correct use-credits of the information |
| ArmeroInicial | 2020-09-01 | 5.05000 | -74.81700 | 240 | 0.1 | Projected horizontal surface | Permanent | rectangle | 50 | 20 | 0.01 | Magdalena river valley | tropical dry forests | Private Natural Reserve Jabiru |  | Tolima | Colombia | Armero | Lowland |  | 10-20 degree slope (moderately sloping) | Mixed forest | Early | 5.0-10.0 | NA | NA | All data and sources associated are available for consulting and potential uses in analysis and publications. But, as normativity of Red BST-Col, in all cases Principle Researchers should contacted for correct use-credits of the information |

Antes de poder incorporar los proyectos, en caso de que esos tengan una
metodología particular, se debe incorporar la metodología y las
variables que permiten incorporar un codigo de metodología.

``` r
dbpp<-dbConnect(Postgres(),dbname=dbname)
```

``` sql
INSERT INTO main.def_unit(cd_measurement_type, unit, unit_spa, abbv_unit, factor)
VALUES(
(SELECT cd_measurement_type FROM main.def_measurement_type WHERE measurement_type='number'), 'Number of individuals', 'Número de individuos', 'ind.', 1);
```

``` sql
INSERT INTO main.def_var(
 cd_unit,
 cd_org_lev,
 cd_var_gp,
 extra_var,
 repeatable,
 var_comment,
 type_var,
 name_dwc,
 extension_dwc,
 name_var,
 description
)
VALUES
  ((SELECT cd_unit FROM main.def_unit WHERE unit='Number of individuals'),
  (SELECT cd_org_lev FROM main.def_organisation_level WHERE org_lev='register'),
  NULL,
  false,
  false,
  'Note that even though this variable can contain true/false value (when there is never more than one individual by register), they are systematized as 0/1',
  'integer',
  'individualCount',
  'occurrence',
  'qt_int',
  'Number of indiduals of a species in a register'
  );
```

``` sql
INSERT INTO main.def_method(
 method,
 method_spa,
 cd_var_ind_qt,
 description_spa,
 description,
 required_var,
 cd_org_lev
)
VALUES(
 'Permanent plot 1 (to be defined)',
 'Parcelas permanentes 1 (por definir)',
 (SELECT cd_var FROM main.def_var WHERE name_var='qt_int' AND cd_org_lev = (SELECT cd_org_lev FROM main.def_organisation_level WHERE org_lev='register')),
 'descripción metodo por definir',
 'todo: describe the method',
 NULL,
 (SELECT cd_org_lev FROM main.def_organisation_level WHERE org_lev='register')
);
```

Con las metodologías definidas (no realmente, más bien con los campos
obligatorios llenos), podemos definir las localidades que corresponden a
las parcelas.

``` r
require(sf)
```

    Loading required package: sf

    Linking to GEOS 3.13.0, GDAL 3.9.3, PROJ 9.4.1; sf_use_s2() is TRUE

    WARNING: different compile-time and runtime versions for GEOS found:

    Linked against: 3.13.0-CAPI-1.19.0 compiled against: 3.12.1-CAPI-1.18.1

    It is probably a good idea to reinstall sf (and maybe lwgeom too)

``` r
dbLocCol<-dbConnect(Postgres(),dbname='dev_geogref')
colombia<-st_read(dbLocCol,"dissolved_colombia")
dbDisconnect(dbLocCol)
plot(st_geometry(colombia), border=NA, col="red")
```

![](./Fig/Incor_BSTunnamed-chunk-9-1.png)

``` r
spatPoints<-st_as_sf(metadata_tot,coords=c("longitude_dec","latitude_dec")) %>%
  st_set_crs(4326) %>% st_transform(st_crs(colombia))
plot(st_geometry(spatPoints),reset=F)
plot(st_geometry(colombia),add=T,border=F,col='turquoise')
plot(st_geometry(spatPoints),add=T,pch='+')
```

![](./Fig/Incor_BSTmapParcelas-1.png)

``` r
schemas<-dbGetQuery(dbpp, "SELECT schema_name FROM information_schema.schemata")$schema_name
if(!"tmp" %in% schemas){dbExecute(dbpp, "CREATE SCHEMA tmp")}
```

    [1] 0

``` r
spatPoints$namepp<-rownames(spatPoints)
st_write(obj=spatPoints, dsn=dbpp, layer=Id(schema="tmp",table="spatial_metadata"),delete_layer=T)
```

    Note: method with signature 'DBIObject#sf' chosen for function 'dbDataType',
     target signature 'PqConnection#sf'.
     "PqConnection#ANY" would also be valid

``` r
rdsBST$dates[[1]]
allDates<-data.frame(
  pp=rep(names(rdsBST$dates),sapply(rdsBST$dates,nrow)),
  Reduce(rbind,rdsBST$dates)
)
head(allDates)
rdsBST$members[[1]]
allMembers<-data.frame(
  pp=rep(names(rdsBST$members),sapply(rdsBST$members,nrow)),
  Reduce(rbind,rdsBST$members)
)
head(allMembers)
```

# Tags

``` r
names(rdsBST)
rdsBST$censuses$census0$AltoSanJorgeInicial
rdsBST$taxonomy$AltoSanJorgeInicial
```
