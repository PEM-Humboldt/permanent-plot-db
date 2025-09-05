# Ejemplo de incorporación de datos a la base de datos: Red BST-Col
Marius Bottin

``` r
DIR_DATA <- "../../otherData/"
dir(DIR_DATA)
```

    [1] "Dryflor_Marius.txt"           "LaPaz.csv"                   
    [3] "Matitas.csv"                  "Plato.csv"                   
    [5] "TDF_taxonomicReference.RData"

``` r
plots<-c("LaPaz","Matitas","Plato")
parcelas<-lapply(paste(DIR_DATA,paste(plots,"csv",sep="."),sep="/"),read.csv,dec=",",sep=";")
names(parcelas)<-plots
```

## Taxonomy

``` r
library(rdsTaxVal)
taxo_parcelas<-new_taxo_oneTab(obj=parcelas,currentFormat = "listPlot", taxonRanks_names = c(family="family",genus="genus",species="specificEpithet"), taxonRanks_epithetized = "specificEpithet",taxoCode = NA, comments = "comments",plot = "plot")
suggested_parcelas<-fullTaxonomicDiagnostic(taxo_parcelas,argsCheckFunction = list())
```

    cleaning space characters

    misplaced qualifiers for undetermined taxa

    unicity of genus in family

    checking for unicity of taxonomic information associated with taxonomic code

    Warning in checkUnicityCodetax(taxo = structure(list(plot = c("LaPaz", "LaPaz",
    : The taxonomic codes are not defined in the object, apply a check on whether
    the taxonomic codes corresponds to taxonomic information does not make sense

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

Comparing with the TDF reference:

``` r
load("../../otherData/TDF_taxonomicReference.RData")
notInTdfRef<-which(!getLowerTax(corrected_parcelas) %in% tdfTaxa$canonicalname)
correctNotInTdfRef<-intersect(suggestTAB$row,notInTdfRef)
```

Los casos que corresponden a correcciones automaticas,que no resultan en
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
completeClassifparcelas<-extractCompleteTaxo(suggested_parcelas$analysedGbif)
finalTaxParcelas<-getLowerTax(corrected_parcelas)
casesToAdd<-extract(corrected_parcelas,"taxonRanks")[!is.na(finalTaxParcelas) &!finalTaxParcelas %in% completeClassifparcelas$canonicalname,]
stopifnot(nrow(casesToAdd)==0)
```
