# Exploración de ejemplos de datos en formato RDS
Marius Bottin

El equipo de investigación del Centro de Estudios Socio-ecológicos y
Cambio Global utiliza un formato interno de R (formato RDS) para
almacenar los datos de parcelas permanentes según un esquema de bases de
datos relacionales. Si bien el lenguaje R no permite directamente
imponer reglas de relaciones entre tablas, la lógica interna es la misma
que en una base de datos en SQL, con llaves primarias y foráneas.

En este documento, vamos a explorar algunos ejemplos de esos archivos
para entender sus estructuras y analizar sus contenidos.

## Reading the files:

En la carpeta compartida, hay varios archivos RDS:

``` r
dirRDS<-"../../data_google/4.rdsProyectos/"
dir(dirRDS)
```

    [1] "dataCerrejon.rds"      "dataDry4.rds"          "dataIFN-Orinoquia.rds"
    [4] "dataQuimbo.rds"        "dataRedBSTCol.rds"     "dataTDF.rds"          
    [7] "gadm36_COL_1_sp.rds"  

Cargamos los datos de los archivos gracias a las funciones `lapply` y
`readRDS`

``` r
allRDSData<-lapply(paste(dirRDS,dir(dirRDS,pattern="rds$"),sep="/"),readRDS)
names(allRDSData)<-gsub("\\.rds$","",dir(dirRDS,pattern="rds$"))
```

Cuando miramos los datos, podemos ver que no todos los archivos
corresponden a datos clasicos de parcelas permanentes en formato RDS
(únicamente los archivos que contienen elementos como “taxonomy” y
“censuses”):

``` r
(n_ard<-lapply(allRDSData,names))
```

    Loading required package: sp

    $dataCerrejon
    [1] "metadata"    "taxonomy"    "censuses"    "dates"       "members"    
    [6] "spatialData"

    $dataDry4
    [1] "productivity"     "productivity.nas" "productivity.imp"

    $`dataIFN-Orinoquia`
     [1] "latitud_dec"            "longitud_dec"           "tamano_individuo"      
     [4] "condicion"              "azimut_grados"          "distancia_m"           
     [7] "tallo_unico_multiple"   "no_de_fuste"            "equipo1"               
    [10] "dap_cm"                 "pom_m"                  "equipo2"               
    [13] "alturatotal_m"          "alturafustal_m"         "numero_colector"       
    [16] "familia"                "genero"                 "epiteto"               
    [19] "niveldeidentidad"       "epitetointraespecifico" "autor"                 

    $dataQuimbo
    [1] "taxonomy"     "censuses"     "traits"       "analyzedData" "dates"       
    [6] "FaseII"      

    $dataRedBSTCol
    [1] "metadata" "taxonomy" "censuses" "dates"    "members" 

    $dataTDF
    [1] "dates"         "taxonomy"      "structure"     "dynamics"     
    [5] "traits"        "spatial"       "analyzedData"  "members"      
    [9] "plotsMetadata"

    $gadm36_COL_1_sp
     [1] "GID_0"     "NAME_0"    "GID_1"     "NAME_1"    "VARNAME_1" "NL_NAME_1"
     [7] "TYPE_1"    "ENGTYPE_1" "CC_1"      "HASC_1"   

``` r
names(allRDSData)[sapply(n_ard,function(x)"censuses" %in% x | "taxonomy" %in% x)]
```

    [1] "dataCerrejon"  "dataQuimbo"    "dataRedBSTCol" "dataTDF"      

Cuantos eventos de muestreos (censuses) contienen los juegos de datos:

``` r
lapply(allRDSData,function(x){
  if("censuses" %in% names(x))
  {return(names(x$censuses))}else{return(NA)}
  })
```

    $dataCerrejon
    [1] "census0"

    $dataDry4
    [1] NA

    $`dataIFN-Orinoquia`
    [1] NA

    $dataQuimbo
    [1] "growth"   "survival"

    $dataRedBSTCol
    [1] "census0"

    $dataTDF
    [1] NA

    $gadm36_COL_1_sp
    [1] NA

Parece que “dataQuimbo” tiene varios eventos de muestreo, de otro lado
“dataRedBSTCol” y “dataCerrejon” solo contienen un “census0”. Sin
embargo, esperabamos encontrar censuses en la forma de census0, census1,
census2 etc.

Para entender mejor la estructura de esos archivos de datos,
exploraremos en más detalles los juegos de datos:

## Quimbo

``` r
quimbo <- allRDSData$dataQuimbo
names(quimbo)
```

    [1] "taxonomy"     "censuses"     "traits"       "analyzedData" "dates"       
    [6] "FaseII"      

### Descripción de cada elemento

#### Taxonomy

La tabla taxonomy contiene:

- 1 codigo para cada especie
- los niveles familia, genero y especie de cada taxon
- el nombre común de las especies

``` r
head(quimbo$taxonomy)
```

| code     | family        | genus      | species    | common_name |
|:---------|:--------------|:-----------|:-----------|:------------|
| Acacfarn | Leguminosae   | Acacia     | farnesiana | Pela        |
| Albiguac | Leguminosae   | Albizia    | guachapele | Igua        |
| Albiniop | Leguminosae   | Albizia    | niopoides  | Bayo        |
| Albisama | Leguminosae   | Albizia    | saman      | Saman       |
| Amyrpinn | Rutaceae      | Amyris     | pinnata    | Bilanda     |
| Anacexce | Anacardiaceae | Anacardium | excelsum   | Caracoli    |

Los codigos de taxón son siempre unicos, y no son faltantes

``` r
sum(duplicated(quimbo$taxonomy$code))
```

    [1] 0

``` r
sum(is.na(quimbo$taxonomy$code))
```

    [1] 0

Todos los elementos parecen ser obligatorios y no faltan valores:

``` r
apply(quimbo$taxonomy,2,function(x)sum(is.na(x)|x==""))
```

           code      family       genus     species common_name 
              0           0           0           0           0 

Quimbo contiene 43 especies.

#### dates

Descripción de los eventos de muestreos de quimbo

``` r
quimbo$dates[order(quimbo$dates$measuringDate),]
```

|     | nucleus                  | nucleusCode | measuringType | measuringDate |
|:----|:-------------------------|:------------|:--------------|:--------------|
| 4   | intensive                | int         | survival0     | 2016-07-19    |
| 13  | intemediate_with_removal | iwr         | survival0     | 2016-07-19    |
| 22  | intemediate_no_removal   | inr         | survival0     | 2016-07-19    |
| 14  | intemediate_with_removal | iwr         | survival1     | 2016-11-29    |
| 23  | intemediate_no_removal   | inr         | survival1     | 2016-11-29    |
| 5   | intensive                | int         | survival1     | 2016-12-13    |
| 10  | intemediate_with_removal | iwr         | growth0       | 2017-01-17    |
| 19  | intemediate_no_removal   | inr         | growth0       | 2017-01-17    |
| 1   | intensive                | int         | growth0       | 2017-01-25    |
| 6   | intensive                | int         | survival2     | 2017-03-24    |
| 15  | intemediate_with_removal | iwr         | survival2     | 2017-03-24    |
| 24  | intemediate_no_removal   | inr         | survival2     | 2017-03-24    |
| 20  | intemediate_no_removal   | inr         | growth1       | 2017-06-28    |
| 25  | intemediate_no_removal   | inr         | survival3     | 2017-06-28    |
| 11  | intemediate_with_removal | iwr         | growth1       | 2017-06-30    |
| 7   | intensive                | int         | survival3     | 2017-07-27    |
| 16  | intemediate_with_removal | iwr         | survival3     | 2017-07-27    |
| 2   | intensive                | int         | growth1       | 2017-08-27    |
| 8   | intensive                | int         | survival4     | 2017-09-30    |
| 17  | intemediate_with_removal | iwr         | survival4     | 2017-09-30    |
| 26  | intemediate_no_removal   | inr         | survival4     | 2017-09-30    |
| 3   | intensive                | int         | growth2       | 2017-12-15    |
| 9   | intensive                | int         | survival5     | 2017-12-15    |
| 21  | intemediate_no_removal   | inr         | growth2       | 2017-12-15    |
| 27  | intemediate_no_removal   | inr         | survival5     | 2017-12-15    |
| 12  | intemediate_with_removal | iwr         | growth2       | 2017-12-16    |
| 18  | intemediate_with_removal | iwr         | survival5     | 2017-12-16    |
