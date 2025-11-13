# Export Darwin Core Archive for the 3 plots
Marius Bottin

## Connection to the database

``` r
pp_bst <- RPostgres::dbConnect(RPostgres::Postgres(), dbname = "pp_bst_col")
```

## La Paz

### Event

``` sql
WITH a AS(
SELECT DISTINCT ON (cd_event)cd_event, mpio, dpto, ST_Area(ST_intersection(l.pol_geom,m.the_geom))
FROM main.event e 
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN main.gp_event ge USING (cd_gp_event)
LEFT JOIN main.project p USING (cd_project)
LEFT JOIN spat.mpio_dane_2023 m  ON ST_Intersects(l.pol_geom,m.the_geom)
LEFT JOIN spat.dpto_dane_2023 d USING(dpto_ccdgo)
WHERE project='LaPaz'
ORDER BY cd_event,ST_Area(ST_intersection(l.pol_geom,m.the_geom)) DESC
), b AS(
  SELECT cd_event,
    ROUND(ST_YMAX(pol_geom) - ST_YMIN(pol_geom)) height, 
    ROUND(ST_XMAX(pol_geom) - ST_XMIN(pol_geom)) width
  FROM main.event e
  LEFT JOIN main.location USING (cd_loc)
),c AS(
  SELECT cd_project,
    ROUND(ST_YMAX(pol_geom) - ST_YMIN(pol_geom)) height, 
    ROUND(ST_XMAX(pol_geom) - ST_XMIN(pol_geom)) width
  FROM main.project p
  LEFT JOIN main.location USING (cd_loc)
),d AS(
SELECT DISTINCT ON (cd_project)cd_project, mpio, dpto, ST_Area(ST_intersection(l.pol_geom,m.the_geom))
FROM main.project p
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN spat.mpio_dane_2023 m  ON ST_Intersects(l.pol_geom,m.the_geom)
LEFT JOIN spat.dpto_dane_2023 d USING(dpto_ccdgo)
WHERE project='LaPaz'
ORDER BY cd_project,ST_Area(ST_intersection(l.pol_geom,m.the_geom)) DESC
)
--parentEvents
SELECT project||'_census'|| (campaign_nb -1)::text AS "eventID",
  NULL AS "parentEventID",
  'Parcela permanente' "eventType",
  'Parcela permanente'||ROUND(c.height/10)*10||'m. x '||ROUND(c.width/10)*10||'m.' AS "samplingProtocol",--modified for precision
  (ROUND(ST_Area(l.pol_geom)/1000)*1000)::int AS sampleSizeValue, 'm2' AS "sampleSizeUnit",--modified for precision
  'census'|| (campaign_nb -1)::text  AS "fieldNumber",
  CASE 
    WHEN ge.date_begin=ge.date_end OR ge.date_end IS NULL THEN TO_CHAR(ge.date_begin,'YYYY-MM-DD')
    WHEN ge.date_end IS NOT NULL AND ge.date_end<>ge.date_begin THEN TO_CHAR(ge.date_begin,'YYYY-MM-DD')||'/'||TO_CHAR(ge.date_end,'YYYY-MM-DD')
  END "dateEvent",
  'Bosque seco tropical' AS "habitat",
  'Event' AS "type",
  'Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH)' AS "institutionCode",
  CASE
    WHEN d.mpio IS NOT NULL THEN 'América del Sur'
    ELSE NULL
  END AS continent,
  CASE
    WHEN d.mpio IS NOT NULL THEN 'Colombia'
    ELSE NULL
  END AS country,
    CASE
    WHEN d.mpio IS NOT NULL THEN 'CO'
    ELSE NULL
  END AS "countryCode",
  dpto AS "stateProvince",
  mpio AS county,
  ST_X(ST_Centroid(l.pol_geom)) AS "decimalLatitude",
  ST_Y(ST_Centroid(l.pol_geom)) AS "decimalLongitude",
  srs.auth_name||':'||srs.auth_srid AS "geodeticDatum",
  ST_AsText(l.pol_geom, 2 ) AS "footprintWKT",
  srs.auth_name||':'||srs.auth_srid AS "footprintSRS"
FROM main.gp_event ge
LEFT JOIN main.project p USING (cd_project,cd_loc)
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN c USING (cd_project)
LEFT JOIN d USING (cd_project)
LEFT JOIN spatial_ref_sys srs ON ST_SRID(pol_geom)=srs.srid
WHERE project='LaPaz'


UNION ALL

--events
(SELECT event_id "eventID",
  project||'_census'|| (campaign_nb -1)::text  AS "parentEventID",
  'Parcela permanente' "eventType", 'Subparcela '||b.height||'m. x '||b.width||'m.' AS "samplingProtocol",
  ST_area(l.pol_geom)::int AS "sampleSizeValue", 'm2' AS "sampleSizeUnit", description_replicate AS "fieldNumber",
  CASE 
    WHEN e.date_begin=e.date_end OR e.date_end IS NULL THEN TO_CHAR(e.date_begin,'YYYY-MM-DD')
    WHEN e.date_end IS NOT NULL AND e.date_end<>e.date_begin THEN TO_CHAR(e.date_begin,'YYYY-MM-DD')||'/'||TO_CHAR(e.date_end,'YYYY-MM-DD')
  END "dateEvent",
  'Bosque seco tropical' AS "habitat",
  'Event' AS "type",
  'Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH)' AS "institutionCode",
  CASE
    WHEN a.mpio IS NOT NULL THEN 'América del Sur'
    ELSE NULL
  END AS continent,
  CASE
    WHEN a.mpio IS NOT NULL THEN 'Colombia'
    ELSE NULL
  END AS country,
    CASE
    WHEN a.mpio IS NOT NULL THEN 'CO'
    ELSE NULL
  END AS "countryCode",
  dpto AS "stateProvince",
  mpio AS county,
  ST_X(ST_Centroid(l.pol_geom)) AS "decimalLatitude",
  ST_Y(ST_Centroid(l.pol_geom)) AS "decimalLongitude",
  srs.auth_name||':'||srs.auth_srid AS "geodeticDatum",
  ST_AsText(l.pol_geom, 2 ) AS "footprintWKT",
  srs.auth_name||':'||srs.auth_srid AS "footprintSRS"
FROM main.event e
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN main.gp_event USING (cd_gp_event)
LEFT JOIN main.project USING (cd_project)
LEFT JOIN a USING(cd_event)
LEFT JOIN b USING(cd_event)
LEFT JOIN spatial_ref_sys srs ON ST_SRID(pol_geom)=srs.srid
WHERE project='LaPaz'
ORDER BY num_replicate)
```

### Register

``` sql
WITH last_identif AS(
SELECT DISTINCT ON (cd_reg) i.*
FROM main.register
LEFT JOIN main.identification i USING (cd_reg)
ORDER BY cd_reg, date_identif DESC
), recorded_by AS(
SELECT cd_reg, STRING_AGG(name,'|') recorded_by
FROM (SELECT cd_reg, UNNEST(cds_recorded_by) cd_people_role FROM main.register) crb
LEFT JOIN main.people_role USING (cd_people_role)
LEFT JOIN (SELECT cd_person, 
              CASE 
                WHEN preferred_complete IS NOT NULL THEN preferred_complete
                WHEN preferred_first IS NOT NULL AND preferred_family IS NOT NULL THEN preferred_first||' '||preferred_family
                WHEN (first_name1 IS NOT NULL AND family_name1 IS NOT NULL) THEN concat(first_name1,' '||first_name2,' ',family_name1,' '||family_name2)
                ELSE verbatim_person
              END name
            FROM main.people) p USING (cd_person)
GROUP BY cd_reg
), identified_by AS(
  SELECT cd_identif, STRING_AGG(name,'|') identified_by
FROM (SELECT cd_identif, UNNEST(identified_by) cd_people_role FROM main.identification) crb
LEFT JOIN main.people_role USING (cd_people_role)
LEFT JOIN (SELECT cd_person, 
              CASE 
                WHEN preferred_complete IS NOT NULL THEN preferred_complete
                WHEN preferred_first IS NOT NULL AND preferred_family IS NOT NULL THEN preferred_first||' '||preferred_family
                WHEN (first_name1 IS NOT NULL AND family_name1 IS NOT NULL) THEN concat(first_name1,' '||first_name2,' ',family_name1,' '||family_name2)
                ELSE verbatim_person
              END name
            FROM main.people) p USING (cd_person)
GROUP BY cd_identif
)
SELECT occurrence_id AS "occurrenceID",
  CASE
    WHEN COALESCE(voucher,catalog_id) IS NULL THEN 'HumanObservation'
    ELSE 'PreservedSpecimen'
  END AS "basisOfRecord",
  'Event' AS "type",
  CASE
    WHEN COALESCE(voucher,catalog_id) IS NULL THEN 'Observación sistemática'
    ELSE 'Colecta botánica'
  END AS "samplingProtocol",
  'Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH)' AS "institutionCode",
  catalog_id AS "catalogNumber",
  recorded_by.recorded_by AS "recordedBy",
  COALESCE(organism_id, ind.tag) AS "organismID",
  qt_int AS "individualCount",
  r.remarks AS "occurrenceRemarks",
  event_id AS "eventID",
  CASE
    WHEN m.mpio IS NOT NULL THEN 'América del Sur'
    ELSE NULL
  END AS continent,
  CASE
    WHEN m.mpio IS NOT NULL THEN 'Colombia'
    ELSE NULL
  END AS country,
    CASE
    WHEN m.mpio IS NOT NULL THEN 'CO'
    ELSE NULL
  END AS "countryCode",
  dpto AS "stateProvince",
  mpio AS county,
  ST_X(pt_geom) AS "decimalLatitude",
  ST_Y(pt_geom) AS "decimalLongitude",
  srs.auth_name||':'||srs.auth_srid AS "geodeticDatum",
  identified_by.identified_by AS "identifiedBy",
  TO_CHAR(date_identif,'YYYY-MM-DD') AS "dateIdentified",
  identification_qualifier AS "identificationQualifier",
  t.name_tax AS "ScientificName",
  t.authorship AS "ScientificNameAuthorship",
  tax_rank_spa AS "taxonRank",
  verbatim_taxon_rank AS "verbatimTaxonRank",
  CASE
    WHEN t.gbifid IS NOT NULL THEN 'gbif.org/species/'||t.gbifid
  END AS "taxonID",
  tk.name_tax AS kingdom,
  tp.name_tax AS phylum,
  tc.name_tax AS "class",
  tor.name_tax AS "order",
  tfam.name_tax AS family,
  tgn.name_tax AS genus,
  REGEXP_REPLACE(tsp.name_tax,'^[^ ]* ','') AS "specificEpithet"
  
FROM main.register r
LEFT JOIN main.register_location l USING (cd_reg)
LEFT JOIN main.reg_individual USING (cd_reg)
LEFT JOIN main.individual ind USING (cd_ind)
LEFT JOIN last_identif USING (cd_reg)
LEFT JOIN recorded_by USING (cd_reg)
LEFT JOIN identified_by USING (cd_identif)
LEFT JOIN spat.mpio_dane_2023 m  ON ST_Intersects(l.pt_geom,m.the_geom)
LEFT JOIN spat.dpto_dane_2023 d USING(dpto_ccdgo)
LEFT JOIN main.taxo t USING (cd_tax)
LEFT JOIN main.def_tax_rank USING (cd_rank)
LEFT JOIN main.morfo_taxo mt USING (cd_tax,cd_morfo)
LEFT JOIN main.taxo tk ON find_higher_id(t.cd_tax,'KG')=tk.cd_tax
LEFT JOIN main.taxo tp ON find_higher_id(t.cd_tax,'PHY')=tp.cd_tax
LEFT JOIN main.taxo tc ON find_higher_id(t.cd_tax,'CL')=tc.cd_tax
LEFT JOIN main.taxo tor ON find_higher_id(t.cd_tax,'OR')=tor.cd_tax
LEFT JOIN main.taxo tfam ON find_higher_id(t.cd_tax,'FAM')=tfam.cd_tax
LEFT JOIN main.taxo tgn ON find_higher_id(t.cd_tax,'GN')=tgn.cd_tax
LEFT JOIN main.taxo tsp ON find_higher_id(t.cd_tax,'SP')=tsp.cd_tax
LEFT JOIN main.event e USING (cd_event)
LEFT JOIN main.gp_event USING (cd_gp_event)
LEFT JOIN main.project USING (cd_project)
LEFT JOIN spatial_ref_sys srs ON ST_SRID(pt_geom)=srs.srid
WHERE project='LaPaz'
ORDER BY num_replicate, ind.tag
```

### Measurements or facts

``` sql
SELECT 
  occurrence_id "occurrenceID",
  'DBH ramet ' || part_number "measurementType (DBH)",
  s1.subind_char_double "measurementValue (DBH)",
  u1.abbv_unit "measurementUnit (DBH)",
  'Altura ramet ' || part_number "measurementType (Altura)",
  s2.subind_char_double "measurementValue (Altura)",
  u2.abbv_unit "measurementUnit (Altura)",
  'tag ramet '|| part_number "measurementType (tag)",
  si.tag "measurementValue (tag)"
FROM main.subindividual_characteristics s1
FULL JOIN main.subindividual_characteristics s2 USING (cd_reg,cd_subind)
LEFT JOIN main.def_var v1 ON s1.cd_var=v1.cd_var
LEFT JOIN main.def_var v2 ON s2.cd_var=v2.cd_var
LEFT JOIN main.def_unit u1 ON v1.cd_unit=u1.cd_unit
LEFT JOIN main.def_unit u2 ON v2.cd_unit=u2.cd_unit
LEFT JOIN main.subindividual si USING (cd_subind)
LEFT JOIN main.def_subindividual_part USING (cd_part)
LEFT JOIN main.individual USING (cd_ind)
LEFT JOIN main.register USING(cd_reg)
LEFT JOIN main.event USING (cd_event)
LEFT JOIN main.gp_event USING (cd_gp_event)
LEFT JOIN main.project USING (cd_project)
WHERE v1.name_var='dbh_cm' AND v2.name_var='height_m' AND project='LaPaz'
ORDER BY num_replicate, cd_ind, part_number
```

### Export

``` r
library(rdsTaxVal)
saveInExcel("../../otherData/DwC_LaPaz.xlsx",lVar=c("event","register","MeasurementsOrFacts"))
```

    Writing sheets: event register MeasurementsOrFacts
    into file:/home/marius/Travail/traitementDonnees/2024_parcelas_permanentes/otherData/DwC_LaPaz.xlsx

## La Paz

### Event

``` sql
WITH a AS(
SELECT DISTINCT ON (cd_event)cd_event, mpio, dpto, ST_Area(ST_intersection(l.pol_geom,m.the_geom))
FROM main.event e 
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN main.gp_event ge USING (cd_gp_event)
LEFT JOIN main.project p USING (cd_project)
LEFT JOIN spat.mpio_dane_2023 m  ON ST_Intersects(l.pol_geom,m.the_geom)
LEFT JOIN spat.dpto_dane_2023 d USING(dpto_ccdgo)
WHERE project='Matitas'
ORDER BY cd_event,ST_Area(ST_intersection(l.pol_geom,m.the_geom)) DESC
), b AS(
  SELECT cd_event,
    ROUND(ST_YMAX(pol_geom) - ST_YMIN(pol_geom)) height, 
    ROUND(ST_XMAX(pol_geom) - ST_XMIN(pol_geom)) width
  FROM main.event e
  LEFT JOIN main.location USING (cd_loc)
),c AS(
  SELECT cd_project,
    ROUND(ST_YMAX(pol_geom) - ST_YMIN(pol_geom)) height, 
    ROUND(ST_XMAX(pol_geom) - ST_XMIN(pol_geom)) width
  FROM main.project p
  LEFT JOIN main.location USING (cd_loc)
),d AS(
SELECT DISTINCT ON (cd_project)cd_project, mpio, dpto, ST_Area(ST_intersection(l.pol_geom,m.the_geom))
FROM main.project p
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN spat.mpio_dane_2023 m  ON ST_Intersects(l.pol_geom,m.the_geom)
LEFT JOIN spat.dpto_dane_2023 d USING(dpto_ccdgo)
WHERE project='Matitas'
ORDER BY cd_project,ST_Area(ST_intersection(l.pol_geom,m.the_geom)) DESC
)
--parentEvents
SELECT project||'_census'|| (campaign_nb -1)::text AS "eventID",
  NULL AS "parentEventID",
  'Parcela permanente' "eventType",
  'Parcela permanente'||ROUND(c.height/10)*10||'m. x '||ROUND(c.width/10)*10||'m.' AS "samplingProtocol",--modified for precision
  (ROUND(ST_Area(l.pol_geom)/1000)*1000)::int AS sampleSizeValue, 'm2' AS "sampleSizeUnit",--modified for precision
  'census'|| (campaign_nb -1)::text  AS "fieldNumber",
  CASE 
    WHEN ge.date_begin=ge.date_end OR ge.date_end IS NULL THEN TO_CHAR(ge.date_begin,'YYYY-MM-DD')
    WHEN ge.date_end IS NOT NULL AND ge.date_end<>ge.date_begin THEN TO_CHAR(ge.date_begin,'YYYY-MM-DD')||'/'||TO_CHAR(ge.date_end,'YYYY-MM-DD')
  END "dateEvent",
  'Bosque seco tropical' AS "habitat",
  'Event' AS "type",
  'Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH)' AS "institutionCode",
  CASE
    WHEN d.mpio IS NOT NULL THEN 'América del Sur'
    ELSE NULL
  END AS continent,
  CASE
    WHEN d.mpio IS NOT NULL THEN 'Colombia'
    ELSE NULL
  END AS country,
    CASE
    WHEN d.mpio IS NOT NULL THEN 'CO'
    ELSE NULL
  END AS "countryCode",
  dpto AS "stateProvince",
  mpio AS county,
  ST_X(ST_Centroid(l.pol_geom)) AS "decimalLatitude",
  ST_Y(ST_Centroid(l.pol_geom)) AS "decimalLongitude",
  srs.auth_name||':'||srs.auth_srid AS "geodeticDatum",
  ST_AsText(l.pol_geom, 2 ) AS "footprintWKT",
  srs.auth_name||':'||srs.auth_srid AS "footprintSRS"
FROM main.gp_event ge
LEFT JOIN main.project p USING (cd_project,cd_loc)
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN c USING (cd_project)
LEFT JOIN d USING (cd_project)
LEFT JOIN spatial_ref_sys srs ON ST_SRID(pol_geom)=srs.srid
WHERE project='Matitas'


UNION ALL

--events
(SELECT event_id "eventID",
  project||'_census'|| (campaign_nb -1)::text  AS "parentEventID",
  'Parcela permanente' "eventType", 'Subparcela '||b.height||'m. x '||b.width||'m.' AS "samplingProtocol",
  ST_area(l.pol_geom)::int AS "sampleSizeValue", 'm2' AS "sampleSizeUnit", description_replicate AS "fieldNumber",
  CASE 
    WHEN e.date_begin=e.date_end OR e.date_end IS NULL THEN TO_CHAR(e.date_begin,'YYYY-MM-DD')
    WHEN e.date_end IS NOT NULL AND e.date_end<>e.date_begin THEN TO_CHAR(e.date_begin,'YYYY-MM-DD')||'/'||TO_CHAR(e.date_end,'YYYY-MM-DD')
  END "dateEvent",
  'Bosque seco tropical' AS "habitat",
  'Event' AS "type",
  'Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH)' AS "institutionCode",
  CASE
    WHEN a.mpio IS NOT NULL THEN 'América del Sur'
    ELSE NULL
  END AS continent,
  CASE
    WHEN a.mpio IS NOT NULL THEN 'Colombia'
    ELSE NULL
  END AS country,
    CASE
    WHEN a.mpio IS NOT NULL THEN 'CO'
    ELSE NULL
  END AS "countryCode",
  dpto AS "stateProvince",
  mpio AS county,
  ST_X(ST_Centroid(l.pol_geom)) AS "decimalLatitude",
  ST_Y(ST_Centroid(l.pol_geom)) AS "decimalLongitude",
  srs.auth_name||':'||srs.auth_srid AS "geodeticDatum",
  ST_AsText(l.pol_geom, 2 ) AS "footprintWKT",
  srs.auth_name||':'||srs.auth_srid AS "footprintSRS"
FROM main.event e
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN main.gp_event USING (cd_gp_event)
LEFT JOIN main.project USING (cd_project)
LEFT JOIN a USING(cd_event)
LEFT JOIN b USING(cd_event)
LEFT JOIN spatial_ref_sys srs ON ST_SRID(pol_geom)=srs.srid
WHERE project='Matitas'
ORDER BY num_replicate)
```

### Register

``` sql
WITH last_identif AS(
SELECT DISTINCT ON (cd_reg) i.*
FROM main.register
LEFT JOIN main.identification i USING (cd_reg)
ORDER BY cd_reg, date_identif DESC
), recorded_by AS(
SELECT cd_reg, STRING_AGG(name,'|') recorded_by
FROM (SELECT cd_reg, UNNEST(cds_recorded_by) cd_people_role FROM main.register) crb
LEFT JOIN main.people_role USING (cd_people_role)
LEFT JOIN (SELECT cd_person, 
              CASE 
                WHEN preferred_complete IS NOT NULL THEN preferred_complete
                WHEN preferred_first IS NOT NULL AND preferred_family IS NOT NULL THEN preferred_first||' '||preferred_family
                WHEN (first_name1 IS NOT NULL AND family_name1 IS NOT NULL) THEN concat(first_name1,' '||first_name2,' ',family_name1,' '||family_name2)
                ELSE verbatim_person
              END name
            FROM main.people) p USING (cd_person)
GROUP BY cd_reg
), identified_by AS(
  SELECT cd_identif, STRING_AGG(name,'|') identified_by
FROM (SELECT cd_identif, UNNEST(identified_by) cd_people_role FROM main.identification) crb
LEFT JOIN main.people_role USING (cd_people_role)
LEFT JOIN (SELECT cd_person, 
              CASE 
                WHEN preferred_complete IS NOT NULL THEN preferred_complete
                WHEN preferred_first IS NOT NULL AND preferred_family IS NOT NULL THEN preferred_first||' '||preferred_family
                WHEN (first_name1 IS NOT NULL AND family_name1 IS NOT NULL) THEN concat(first_name1,' '||first_name2,' ',family_name1,' '||family_name2)
                ELSE verbatim_person
              END name
            FROM main.people) p USING (cd_person)
GROUP BY cd_identif
)
SELECT occurrence_id AS "occurrenceID",
  CASE
    WHEN COALESCE(voucher,catalog_id) IS NULL THEN 'HumanObservation'
    ELSE 'PreservedSpecimen'
  END AS "basisOfRecord",
  'Event' AS "type",
  CASE
    WHEN COALESCE(voucher,catalog_id) IS NULL THEN 'Observación sistemática'
    ELSE 'Colecta botánica'
  END AS "samplingProtocol",
  'Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH)' AS "institutionCode",
  catalog_id AS "catalogNumber",
  recorded_by.recorded_by AS "recordedBy",
  COALESCE(organism_id, ind.tag) AS "organismID",
  qt_int AS "individualCount",
  r.remarks AS "occurrenceRemarks",
  event_id AS "eventID",
  CASE
    WHEN m.mpio IS NOT NULL THEN 'América del Sur'
    ELSE NULL
  END AS continent,
  CASE
    WHEN m.mpio IS NOT NULL THEN 'Colombia'
    ELSE NULL
  END AS country,
    CASE
    WHEN m.mpio IS NOT NULL THEN 'CO'
    ELSE NULL
  END AS "countryCode",
  dpto AS "stateProvince",
  mpio AS county,
  ST_X(pt_geom) AS "decimalLatitude",
  ST_Y(pt_geom) AS "decimalLongitude",
  srs.auth_name||':'||srs.auth_srid AS "geodeticDatum",
  identified_by.identified_by AS "identifiedBy",
  TO_CHAR(date_identif,'YYYY-MM-DD') AS "dateIdentified",
  identification_qualifier AS "identificationQualifier",
  t.name_tax AS "ScientificName",
  t.authorship AS "ScientificNameAuthorship",
  tax_rank_spa AS "taxonRank",
  verbatim_taxon_rank AS "verbatimTaxonRank",
  CASE
    WHEN t.gbifid IS NOT NULL THEN 'gbif.org/species/'||t.gbifid
  END AS "taxonID",
  tk.name_tax AS kingdom,
  tp.name_tax AS phylum,
  tc.name_tax AS "class",
  tor.name_tax AS "order",
  tfam.name_tax AS family,
  tgn.name_tax AS genus,
  REGEXP_REPLACE(tsp.name_tax,'^[^ ]* ','') AS "specificEpithet"
  
FROM main.register r
LEFT JOIN main.register_location l USING (cd_reg)
LEFT JOIN main.reg_individual USING (cd_reg)
LEFT JOIN main.individual ind USING (cd_ind)
LEFT JOIN last_identif USING (cd_reg)
LEFT JOIN recorded_by USING (cd_reg)
LEFT JOIN identified_by USING (cd_identif)
LEFT JOIN spat.mpio_dane_2023 m  ON ST_Intersects(l.pt_geom,m.the_geom)
LEFT JOIN spat.dpto_dane_2023 d USING(dpto_ccdgo)
LEFT JOIN main.taxo t USING (cd_tax)
LEFT JOIN main.def_tax_rank USING (cd_rank)
LEFT JOIN main.morfo_taxo mt USING (cd_tax,cd_morfo)
LEFT JOIN main.taxo tk ON find_higher_id(t.cd_tax,'KG')=tk.cd_tax
LEFT JOIN main.taxo tp ON find_higher_id(t.cd_tax,'PHY')=tp.cd_tax
LEFT JOIN main.taxo tc ON find_higher_id(t.cd_tax,'CL')=tc.cd_tax
LEFT JOIN main.taxo tor ON find_higher_id(t.cd_tax,'OR')=tor.cd_tax
LEFT JOIN main.taxo tfam ON find_higher_id(t.cd_tax,'FAM')=tfam.cd_tax
LEFT JOIN main.taxo tgn ON find_higher_id(t.cd_tax,'GN')=tgn.cd_tax
LEFT JOIN main.taxo tsp ON find_higher_id(t.cd_tax,'SP')=tsp.cd_tax
LEFT JOIN main.event e USING (cd_event)
LEFT JOIN main.gp_event USING (cd_gp_event)
LEFT JOIN main.project USING (cd_project)
LEFT JOIN spatial_ref_sys srs ON ST_SRID(pt_geom)=srs.srid
WHERE project='Matitas'
ORDER BY num_replicate, ind.tag
```

### Measurements or facts

``` sql
SELECT 
  occurrence_id "occurrenceID",
  'DBH ramet ' || part_number "measurementType (DBH)",
  s1.subind_char_double "measurementValue (DBH)",
  u1.abbv_unit "measurementUnit (DBH)",
  'Altura ramet ' || part_number "measurementType (Altura)",
  s2.subind_char_double "measurementValue (Altura)",
  u2.abbv_unit "measurementUnit (Altura)",
  'tag ramet '|| part_number "measurementType (tag)",
  si.tag "measurementValue (tag)"
FROM main.subindividual_characteristics s1
FULL JOIN main.subindividual_characteristics s2 USING (cd_reg,cd_subind)
LEFT JOIN main.def_var v1 ON s1.cd_var=v1.cd_var
LEFT JOIN main.def_var v2 ON s2.cd_var=v2.cd_var
LEFT JOIN main.def_unit u1 ON v1.cd_unit=u1.cd_unit
LEFT JOIN main.def_unit u2 ON v2.cd_unit=u2.cd_unit
LEFT JOIN main.subindividual si USING (cd_subind)
LEFT JOIN main.def_subindividual_part USING (cd_part)
LEFT JOIN main.individual USING (cd_ind)
LEFT JOIN main.register USING(cd_reg)
LEFT JOIN main.event USING (cd_event)
LEFT JOIN main.gp_event USING (cd_gp_event)
LEFT JOIN main.project USING (cd_project)
WHERE v1.name_var='dbh_cm' AND v2.name_var='height_m' AND project='Matitas'
ORDER BY num_replicate, cd_ind, part_number
```

### Export

``` r
library(rdsTaxVal)
saveInExcel("../../otherData/DwC_Matitas.xlsx",lVar=c("event","register","MeasurementsOrFacts"))
```

    Writing sheets: event register MeasurementsOrFacts
    into file:/home/marius/Travail/traitementDonnees/2024_parcelas_permanentes/otherData/DwC_Matitas.xlsx

## La Paz

### Event

``` sql
WITH a AS(
SELECT DISTINCT ON (cd_event)cd_event, mpio, dpto, ST_Area(ST_intersection(l.pol_geom,m.the_geom))
FROM main.event e 
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN main.gp_event ge USING (cd_gp_event)
LEFT JOIN main.project p USING (cd_project)
LEFT JOIN spat.mpio_dane_2023 m  ON ST_Intersects(l.pol_geom,m.the_geom)
LEFT JOIN spat.dpto_dane_2023 d USING(dpto_ccdgo)
WHERE project='Plato'
ORDER BY cd_event,ST_Area(ST_intersection(l.pol_geom,m.the_geom)) DESC
), b AS(
  SELECT cd_event,
    ROUND(ST_YMAX(pol_geom) - ST_YMIN(pol_geom)) height, 
    ROUND(ST_XMAX(pol_geom) - ST_XMIN(pol_geom)) width
  FROM main.event e
  LEFT JOIN main.location USING (cd_loc)
),c AS(
  SELECT cd_project,
    ROUND(ST_YMAX(pol_geom) - ST_YMIN(pol_geom)) height, 
    ROUND(ST_XMAX(pol_geom) - ST_XMIN(pol_geom)) width
  FROM main.project p
  LEFT JOIN main.location USING (cd_loc)
),d AS(
SELECT DISTINCT ON (cd_project)cd_project, mpio, dpto, ST_Area(ST_intersection(l.pol_geom,m.the_geom))
FROM main.project p
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN spat.mpio_dane_2023 m  ON ST_Intersects(l.pol_geom,m.the_geom)
LEFT JOIN spat.dpto_dane_2023 d USING(dpto_ccdgo)
WHERE project='Plato'
ORDER BY cd_project,ST_Area(ST_intersection(l.pol_geom,m.the_geom)) DESC
)
--parentEvents
SELECT project||'_census'|| (campaign_nb -1)::text AS "eventID",
  NULL AS "parentEventID",
  'Parcela permanente' "eventType",
  'Parcela permanente'||ROUND(c.height/10)*10||'m. x '||ROUND(c.width/10)*10||'m.' AS "samplingProtocol",--modified for precision
  (ROUND(ST_Area(l.pol_geom)/1000)*1000)::int AS sampleSizeValue, 'm2' AS "sampleSizeUnit",--modified for precision
  'census'|| (campaign_nb -1)::text  AS "fieldNumber",
  CASE 
    WHEN ge.date_begin=ge.date_end OR ge.date_end IS NULL THEN TO_CHAR(ge.date_begin,'YYYY-MM-DD')
    WHEN ge.date_end IS NOT NULL AND ge.date_end<>ge.date_begin THEN TO_CHAR(ge.date_begin,'YYYY-MM-DD')||'/'||TO_CHAR(ge.date_end,'YYYY-MM-DD')
  END "dateEvent",
  'Bosque seco tropical' AS "habitat",
  'Event' AS "type",
  'Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH)' AS "institutionCode",
  CASE
    WHEN d.mpio IS NOT NULL THEN 'América del Sur'
    ELSE NULL
  END AS continent,
  CASE
    WHEN d.mpio IS NOT NULL THEN 'Colombia'
    ELSE NULL
  END AS country,
    CASE
    WHEN d.mpio IS NOT NULL THEN 'CO'
    ELSE NULL
  END AS "countryCode",
  dpto AS "stateProvince",
  mpio AS county,
  ST_X(ST_Centroid(l.pol_geom)) AS "decimalLatitude",
  ST_Y(ST_Centroid(l.pol_geom)) AS "decimalLongitude",
  srs.auth_name||':'||srs.auth_srid AS "geodeticDatum",
  ST_AsText(l.pol_geom, 2 ) AS "footprintWKT",
  srs.auth_name||':'||srs.auth_srid AS "footprintSRS"
FROM main.gp_event ge
LEFT JOIN main.project p USING (cd_project,cd_loc)
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN c USING (cd_project)
LEFT JOIN d USING (cd_project)
LEFT JOIN spatial_ref_sys srs ON ST_SRID(pol_geom)=srs.srid
WHERE project='Plato'


UNION ALL

--events
(SELECT event_id "eventID",
  project||'_census'|| (campaign_nb -1)::text  AS "parentEventID",
  'Parcela permanente' "eventType", 'Subparcela '||b.height||'m. x '||b.width||'m.' AS "samplingProtocol",
  ST_area(l.pol_geom)::int AS "sampleSizeValue", 'm2' AS "sampleSizeUnit", description_replicate AS "fieldNumber",
  CASE 
    WHEN e.date_begin=e.date_end OR e.date_end IS NULL THEN TO_CHAR(e.date_begin,'YYYY-MM-DD')
    WHEN e.date_end IS NOT NULL AND e.date_end<>e.date_begin THEN TO_CHAR(e.date_begin,'YYYY-MM-DD')||'/'||TO_CHAR(e.date_end,'YYYY-MM-DD')
  END "dateEvent",
  'Bosque seco tropical' AS "habitat",
  'Event' AS "type",
  'Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH)' AS "institutionCode",
  CASE
    WHEN a.mpio IS NOT NULL THEN 'América del Sur'
    ELSE NULL
  END AS continent,
  CASE
    WHEN a.mpio IS NOT NULL THEN 'Colombia'
    ELSE NULL
  END AS country,
    CASE
    WHEN a.mpio IS NOT NULL THEN 'CO'
    ELSE NULL
  END AS "countryCode",
  dpto AS "stateProvince",
  mpio AS county,
  ST_X(ST_Centroid(l.pol_geom)) AS "decimalLatitude",
  ST_Y(ST_Centroid(l.pol_geom)) AS "decimalLongitude",
  srs.auth_name||':'||srs.auth_srid AS "geodeticDatum",
  ST_AsText(l.pol_geom, 2 ) AS "footprintWKT",
  srs.auth_name||':'||srs.auth_srid AS "footprintSRS"
FROM main.event e
LEFT JOIN main.location l USING (cd_loc)
LEFT JOIN main.gp_event USING (cd_gp_event)
LEFT JOIN main.project USING (cd_project)
LEFT JOIN a USING(cd_event)
LEFT JOIN b USING(cd_event)
LEFT JOIN spatial_ref_sys srs ON ST_SRID(pol_geom)=srs.srid
WHERE project='Plato'
ORDER BY num_replicate)
```

### Register

``` sql
WITH last_identif AS(
SELECT DISTINCT ON (cd_reg) i.*
FROM main.register
LEFT JOIN main.identification i USING (cd_reg)
ORDER BY cd_reg, date_identif DESC
), recorded_by AS(
SELECT cd_reg, STRING_AGG(name,'|') recorded_by
FROM (SELECT cd_reg, UNNEST(cds_recorded_by) cd_people_role FROM main.register) crb
LEFT JOIN main.people_role USING (cd_people_role)
LEFT JOIN (SELECT cd_person, 
              CASE 
                WHEN preferred_complete IS NOT NULL THEN preferred_complete
                WHEN preferred_first IS NOT NULL AND preferred_family IS NOT NULL THEN preferred_first||' '||preferred_family
                WHEN (first_name1 IS NOT NULL AND family_name1 IS NOT NULL) THEN concat(first_name1,' '||first_name2,' ',family_name1,' '||family_name2)
                ELSE verbatim_person
              END name
            FROM main.people) p USING (cd_person)
GROUP BY cd_reg
), identified_by AS(
  SELECT cd_identif, STRING_AGG(name,'|') identified_by
FROM (SELECT cd_identif, UNNEST(identified_by) cd_people_role FROM main.identification) crb
LEFT JOIN main.people_role USING (cd_people_role)
LEFT JOIN (SELECT cd_person, 
              CASE 
                WHEN preferred_complete IS NOT NULL THEN preferred_complete
                WHEN preferred_first IS NOT NULL AND preferred_family IS NOT NULL THEN preferred_first||' '||preferred_family
                WHEN (first_name1 IS NOT NULL AND family_name1 IS NOT NULL) THEN concat(first_name1,' '||first_name2,' ',family_name1,' '||family_name2)
                ELSE verbatim_person
              END name
            FROM main.people) p USING (cd_person)
GROUP BY cd_identif
)
SELECT occurrence_id AS "occurrenceID",
  CASE
    WHEN COALESCE(voucher,catalog_id) IS NULL THEN 'HumanObservation'
    ELSE 'PreservedSpecimen'
  END AS "basisOfRecord",
  'Event' AS "type",
  CASE
    WHEN COALESCE(voucher,catalog_id) IS NULL THEN 'Observación sistemática'
    ELSE 'Colecta botánica'
  END AS "samplingProtocol",
  'Instituto de Investigación de Recursos Biológicos Alexander von Humboldt (IAvH)' AS "institutionCode",
  catalog_id AS "catalogNumber",
  recorded_by.recorded_by AS "recordedBy",
  COALESCE(organism_id, ind.tag) AS "organismID",
  qt_int AS "individualCount",
  r.remarks AS "occurrenceRemarks",
  event_id AS "eventID",
  CASE
    WHEN m.mpio IS NOT NULL THEN 'América del Sur'
    ELSE NULL
  END AS continent,
  CASE
    WHEN m.mpio IS NOT NULL THEN 'Colombia'
    ELSE NULL
  END AS country,
    CASE
    WHEN m.mpio IS NOT NULL THEN 'CO'
    ELSE NULL
  END AS "countryCode",
  dpto AS "stateProvince",
  mpio AS county,
  ST_X(pt_geom) AS "decimalLatitude",
  ST_Y(pt_geom) AS "decimalLongitude",
  srs.auth_name||':'||srs.auth_srid AS "geodeticDatum",
  identified_by.identified_by AS "identifiedBy",
  TO_CHAR(date_identif,'YYYY-MM-DD') AS "dateIdentified",
  identification_qualifier AS "identificationQualifier",
  t.name_tax AS "ScientificName",
  t.authorship AS "ScientificNameAuthorship",
  tax_rank_spa AS "taxonRank",
  verbatim_taxon_rank AS "verbatimTaxonRank",
  CASE
    WHEN t.gbifid IS NOT NULL THEN 'gbif.org/species/'||t.gbifid
  END AS "taxonID",
  tk.name_tax AS kingdom,
  tp.name_tax AS phylum,
  tc.name_tax AS "class",
  tor.name_tax AS "order",
  tfam.name_tax AS family,
  tgn.name_tax AS genus,
  REGEXP_REPLACE(tsp.name_tax,'^[^ ]* ','') AS "specificEpithet"
  
FROM main.register r
LEFT JOIN main.register_location l USING (cd_reg)
LEFT JOIN main.reg_individual USING (cd_reg)
LEFT JOIN main.individual ind USING (cd_ind)
LEFT JOIN last_identif USING (cd_reg)
LEFT JOIN recorded_by USING (cd_reg)
LEFT JOIN identified_by USING (cd_identif)
LEFT JOIN spat.mpio_dane_2023 m  ON ST_Intersects(l.pt_geom,m.the_geom)
LEFT JOIN spat.dpto_dane_2023 d USING(dpto_ccdgo)
LEFT JOIN main.taxo t USING (cd_tax)
LEFT JOIN main.def_tax_rank USING (cd_rank)
LEFT JOIN main.morfo_taxo mt USING (cd_tax,cd_morfo)
LEFT JOIN main.taxo tk ON find_higher_id(t.cd_tax,'KG')=tk.cd_tax
LEFT JOIN main.taxo tp ON find_higher_id(t.cd_tax,'PHY')=tp.cd_tax
LEFT JOIN main.taxo tc ON find_higher_id(t.cd_tax,'CL')=tc.cd_tax
LEFT JOIN main.taxo tor ON find_higher_id(t.cd_tax,'OR')=tor.cd_tax
LEFT JOIN main.taxo tfam ON find_higher_id(t.cd_tax,'FAM')=tfam.cd_tax
LEFT JOIN main.taxo tgn ON find_higher_id(t.cd_tax,'GN')=tgn.cd_tax
LEFT JOIN main.taxo tsp ON find_higher_id(t.cd_tax,'SP')=tsp.cd_tax
LEFT JOIN main.event e USING (cd_event)
LEFT JOIN main.gp_event USING (cd_gp_event)
LEFT JOIN main.project USING (cd_project)
LEFT JOIN spatial_ref_sys srs ON ST_SRID(pt_geom)=srs.srid
WHERE project='Plato'
ORDER BY num_replicate, ind.tag
```

### Measurements or facts

``` sql
SELECT 
  occurrence_id "occurrenceID",
  'DBH ramet ' || part_number "measurementType (DBH)",
  s1.subind_char_double "measurementValue (DBH)",
  u1.abbv_unit "measurementUnit (DBH)",
  'Altura ramet ' || part_number "measurementType (Altura)",
  s2.subind_char_double "measurementValue (Altura)",
  u2.abbv_unit "measurementUnit (Altura)",
  'tag ramet '|| part_number "measurementType (tag)",
  si.tag "measurementValue (tag)"
FROM main.subindividual_characteristics s1
FULL JOIN main.subindividual_characteristics s2 USING (cd_reg,cd_subind)
LEFT JOIN main.def_var v1 ON s1.cd_var=v1.cd_var
LEFT JOIN main.def_var v2 ON s2.cd_var=v2.cd_var
LEFT JOIN main.def_unit u1 ON v1.cd_unit=u1.cd_unit
LEFT JOIN main.def_unit u2 ON v2.cd_unit=u2.cd_unit
LEFT JOIN main.subindividual si USING (cd_subind)
LEFT JOIN main.def_subindividual_part USING (cd_part)
LEFT JOIN main.individual USING (cd_ind)
LEFT JOIN main.register USING(cd_reg)
LEFT JOIN main.event USING (cd_event)
LEFT JOIN main.gp_event USING (cd_gp_event)
LEFT JOIN main.project USING (cd_project)
WHERE v1.name_var='dbh_cm' AND v2.name_var='height_m' AND project='Plato'
ORDER BY num_replicate, cd_ind, part_number
```

### Export

``` r
library(rdsTaxVal)
saveInExcel("../../otherData/DwC_Plato.xlsx",lVar=c("event","register","MeasurementsOrFacts"))
```

    Writing sheets: event register MeasurementsOrFacts
    into file:/home/marius/Travail/traitementDonnees/2024_parcelas_permanentes/otherData/DwC_Plato.xlsx
