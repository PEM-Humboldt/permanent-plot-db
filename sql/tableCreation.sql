BEGIN;
CREATE SCHEMA main;
CREATE SCHEMA external;

CREATE TABLE main.def_organisation_level
(
	cd_org_lev serial PRIMARY KEY,
	org_lev text UNIQUE NOT NULL,
	pkey_field text UNIQUE NOT NULL,
	main_table text NOT NULL,
	other_tables text[]
);
INSERT INTO main.def_organisation_level(org_lev,pkey_field,main_table)
VALUES
	('sub-individual','cd_subind','sub_individual'),
	('sub-individual characteristics','cd_subind_char','subindividual_characteristics'),
	('individual','cd_ind','individual'),
	('individual characteristics','cd_ind_char','individual_characteristics'),
	('register','cd_reg','register'),
	('event','cd_event','event'),
	('event group','cd_gp_event','gp_event'),
	('project','cd_project','project'),
	('methodology','cd_method','def_method'),
	('identification','cd_ident','identification'),
	('taxon','cd_tax','taxo'),
	('morfo-taxon','cd_morfo','morfo_taxo'),
	('person','cd_person','people'),
	('organization','cd_org','organization'),
	('role','cd_people_role','people_role')
	
	;
CREATE TABLE main.def_location_type(
	cd_loc_type integer PRIMARY KEY NOT NULL,
	location_type text UNIQUE NOT NULL,
	geom_type text NOT NULL,
	comment text
	);
INSERT INTO main.def_location_type
VALUES
	(1,'Site point','POINT','Should represent the whole location, prefer the centroid than the first or last sampling location'),
	(2,'Plot polygon','POLYGON','Polygon containing the whole sampling site'),
	(3,'Transect','MULTILINESTRING','Follow the sampling path of the transect, the order of the geometry is important')
	(4,'Plot group','POLYGON','Convex Hull of the plot coordinates in a group of plots')
	;

-- it should be called sites and have the possibility to get real coordinates or spatial data
CREATE TABLE main.location
(
    cd_loc serial PRIMARY KEY,
    location varchar(50) UNIQUE NOT NULL,
    other_location_name text[],
    cd_loc_type integer REFERENCES main.def_location_type(cd_loc_type),
    parent_loc integer REFERENCES main.location(cd_loc),
    cd_org_lev integer REFERENCES main.def_organisation_level(cd_org_lev)
)
;
SELECT AddGeometryColumn('main', 'location', 'pt_geom', 3116, 'POINT', 2);
SELECT AddGeometryColumn('main', 'location', 'pol_geom', 3116, 'MULTIPOLYGON', 2);
SELECT AddGeometryColumn('main', 'location', 'li_geom', 3116, 'MULTILINESTRING', 2);
-- CREATE INDEX main_location_pt_geom_spat_idx ON main.location USING GIST(pt_geom);
-- CREATE INDEX main_location_pol_geom_spat_idx ON main.location USING GIST(pol_geom);
-- CREATE INDEX main_location_li_geom_spat_idx ON main.location USING GIST(li_geom);

--CREATE INDEX punto_referencia_cd_plat_key ON main.punto_referencia(cd_plat);


-- instead of the
CREATE TABLE main.def_gp_biol
(
    cd_gp_biol char(4) PRIMARY KEY,
    biol_gp varchar(50) UNIQUE NOT NULL,
    super_gp varchar(50),
    biol_gp_spa varchar(50),
    super_gp_spa varchar(50),
    aquatic boolean

);
INSERT INTO main.def_gp_biol
VALUES
    ('herp', 'Herpetofauna',NULL,'Herpetos',NULL,false),
    ('atro', 'Atropellamientos', NULL, 'Atropellamientos', NULL,false),
    ('aves', 'Birds', NULL, 'Aves',NULL,false),
    ('arbo', 'Trees', 'Botany', 'Arbórea', 'Botánica',false),
    ('epva', 'Vascular epiphytes', 'Botany', 'Epifitas vasculares','Botánica',false),
    ('epnv', 'Non-vascular epiphytes', 'Botany', 'Epifitas no vasculares', 'Botánica',false),
    ('cole', 'Collembola', NULL, 'Colémbolos', NULL,false),
    ('esca', 'Beetles', NULL, 'Escarabajos', NULL,false),
    ('horm','Ants', NULL, 'Hormigas', NULL,false),
    ('mami','Mammals', NULL, 'Mamiferos', NULL,false),
    ('mari','Butterflies', NULL, 'Mariposas', NULL,false),
    ('pece', 'Fishes', NULL, 'Peces', NULL,true),
    ('fipl','Phytoplankton', 'Hydrobiology', 'Fitoplancton', 'Hidrobiología',true),
    ('zopl', 'Zooplankton' , 'Hydrobiology', 'Zooplancton', 'Hidrobiología',true),
    ('peri', 'Periphyton' , 'Hydrobiology', 'Perifiton', 'Hidrobiología',true),
    ('mafi', 'Macrophytes' , 'Hydrobiology', 'Macrofitas', 'Hidrobiología',true),
    ('minv', 'Macroinvertebrates' , 'Hydrobiology', 'Macroinvertebrados', 'Hidrobiología',true),
    ('catr', 'Camera traps', NULL, 'Cameras trampa', NULL, false),
    ('arch', 'Archaea', 'Microorganisms', 'Arqueas', 'Microorganismos', false),
    ('bact', 'Bacteria', 'Microorganisms', 'Bacteria', 'Microorganismos', false),
    ('paso', 'Soundscape',NULL, 'Paísajes sonores', NULL, false)

;

CREATE TABLE main.def_measurement_type -- a measurement type is something like distance, area, time, density, number of individuals, density, concentration, volume. This allows us to define what is virtually possible to translate from one variable to another.
(
    cd_measurement_type smallserial PRIMARY KEY,
    measurement_type text
);

INSERT INTO main.def_measurement_type(measurement_type)
VALUES
    ('area'),
    ('time'),
    ('distance'),
    ('density'),
    ('mass'),
    ('number'),
    ('presence/absence'),
    ('longitude'),
    ('volume'),
    ('number concentration (density)'),
    ('mass concentration'),
    ('volume concentration'),
    ('percentage')
    ;

--('area', 'time', 'number of traps', 'distancia', 'number of individuals', 'density', 'concentration'))

CREATE TABLE main.def_unit
(
    cd_unit smallserial PRIMARY KEY,
    cd_measurement_type smallint REFERENCES main.def_measurement_type(cd_measurement_type) NOT NULL,
    unit text NOT NULL UNIQUE,
    unit_spa text,
    abbv_unit text,
    factor double precision NOT NULL,
    UNIQUE (cd_measurement_type,unit)
);
CREATE TABLE main.def_var_gp
(
	cd_var_gp serial PRIMARY KEY,
	var_gp text UNIQUE NOT NULL,
	description text
);

CREATE TABLE main.def_var
(
	cd_var serial PRIMARY KEY,
	cd_unit int REFERENCES main.def_unit(cd_unit),
	cd_org_lev int NOT NULL REFERENCES main.def_organisation_level(cd_org_lev),
	cd_var_gp int REFERENCES main.def_var_gp(cd_var_gp),
	name_var text NOT NULL,
	description text,
	var_comment text,
	extra_var boolean default true, -- is the var in the extra tables, which means not indexable?
	name_dwc text,
	extension_dwc text,
	type_var varchar(25) NOT NULL,
	repeatable boolean default false,
	CHECK (type_var IN ('categories','text','integer','double precision','boolean') )
);
CREATE TABLE main.controlled_vocab
(
	cd_categ serial PRIMARY KEY,
	cd_var int REFERENCES main.def_var(cd_var),
	categ text NOT NULL,
	description text
);

CREATE TABLE main.def_method
(
    cd_method smallserial PRIMARY KEY,
    method text UNIQUE NOT NULL,
    method_spa text,
    cd_var_ind_qt smallint REFERENCES main.def_var(cd_var),
    description_spa text,
    description text,
    required_var int[],
    cd_org_lev int REFERENCES main.def_organisation_level(cd_org_lev)
);

CREATE TABLE main.organization_type
(
	cd_org_type smallserial PRIMARY KEY,
	org_type text UNIQUE NOT NULL,
	org_type_spa text,
	comments text
);


INSERT INTO main.organization_type(org_type,org_type_spa)
VALUES
	('public institution','institución publica'),
	('private institution','institución privada'),
	('non-governemental organization','organización no gubernamental'),
	('fundation','fundación'),
	('social organization','organización social'),
	('professional union','gremio'),
	('communication media','medio de comunicación'),
	('non-profit organization','organización sin Ánimo de lucro'),
	('indigeneous reservation','resguardo indigena'),
	('Person','persona independiente'),
	('Association','asociación'),
	('network','red de actores')
;

CREATE TABLE main.organization
(
    cd_org smallserial PRIMARY KEY,
    org_name text,
    cd_org_type smallint REFERENCES main.organization_type
);

CREATE TABLE main.def_org_rel
(
	cd_org_rel smallserial PRIMARY KEY,
	org_relation_type text,
	description text
);
INSERT INTO main.def_org_rel(org_relation_type, description)
VALUES
	('Institutional part of','Organization 1 is an institutional part of organization 2'),
	('Part of','Organization 1 is part of organization 2, but there is no institutional dependency')
;
CREATE TABLE main.organization_relationship
(
	cd_org1 smallint REFERENCES main.organization(cd_org),
	cd_org2 smallint REFERENCES  main.organization(cd_org),
	cd_rel_org smallint REFERENCES main.def_org_rel(cd_org_rel)/*,
	here we should add a reference to a project to fill only when the relationship between institutions is only for a particular project*/
);

CREATE TABLE main.people
(
    cd_person smallserial PRIMARY KEY,
    verbatim_person text UNIQUE NOT NULL,
    first_name1 text,
    first_name2 text,
    family_name1 text,
    family_name2 text,
    gender char(1),
    birth date,
    declared_identity text[]
);

CREATE TABLE main.people_role
(
	cd_people_role serial PRIMARY KEY,
	cd_person smallint REFERENCES main.people(cd_person),
	cd_org smallint REFERENCES main.organization(cd_org) NOT NULL,
	role text NOT NULL,
	date_apply date,
	date_begin date,
	date_end date/*,
	here we should add a reference to a project to fill only when the role for a particular project*/
);

CREATE TABLE main.role_directory
(
	cd_people_role int PRIMARY KEY REFERENCES main.people_role(cd_people_role),
	email text,
	phone_number text,
	adress text,
	city text,
	country text
);

CREATE TABLE main.def_project_type
(
	cd_proj_type smallserial PRIMARY KEY,
	proj_type text NOT NULL,
	proj_type_description text NOT NULL
);

INSERT INTO main.def_project_type(proj_type, proj_type_description)
VALUES
	('institutional project','Project with a reference ID corresponding to an institutional project. This kind of project usually contains various projects here'),
	('collection field campaign','Project organized around a field campaign, usually included in a larger project with an institutional ID'),
	('permanent plot','Project mostly useful to be able to filter a permanent plot, note that this kind of project should include various gp_event corresponding to various campaign on the same site'),
	('data project','Meta-project with the objective of integrating data from various projects'),
	('permanent plot set','Set of permanent plots which share characteristics, geographic or otherwise')
	;


CREATE TABLE main.project
(
	cd_project serial PRIMARY KEY,
	project text UNIQUE,
	project_description text,
	cd_proj_type smallint REFERENCES main.def_project_type,
	cd_method int REFERENCES main.def_method,
	cd_loc int REFERENCES main.location(cd_loc)
);
ALTER TABLE main.organization_relationship ADD COLUMN cd_project int REFERENCES main.project(cd_project);
ALTER TABLE main.people_role ADD COLUMN cd_project int REFERENCES main.project(cd_project);

CREATE TABLE main.proj_rel_type  -- On the long term we will be able to document project types and the different relationships between projects such as funding relationships, differenciating data projects which only exists in terms of data management from metaprojects having an institutional structures etc. That is why we do not only use a simple foreign key to a parent project.
(
	cd_proj_rel_type smallserial PRIMARY KEY,
	proj_rel_type text UNIQUE NOT NULL,
	description text
);
INSERT INTO main.proj_rel_type(proj_rel_type,description)
VALUES
	('part_of','Proj1 is part of proj2');
CREATE TABLE main.project_relationship
(
	cd_proj1 int REFERENCES main.project (cd_project),
	cd_proj2 int REFERENCES main.project (cd_project),
	cd_proj_rel_type smallint REFERENCES main.proj_rel_type
);


CREATE TABLE main.gp_event
(
    cd_gp_event serial PRIMARY KEY,
    cd_project int REFERENCES main.project (cd_project),
    cd_loc integer REFERENCES main.location(cd_loc) ON DELETE SET NULL ON UPDATE CASCADE,
    cd_gp_biol char(4) REFERENCES main.def_gp_biol(cd_gp_biol) ON DELETE CASCADE NOT NULL,
    cd_method int REFERENCES main.def_method(cd_method),
    --cd_protocol int REFERENCES main.def_protocol(cd_protocol) ON DELETE SET NULL ON UPDATE CASCADE ,
    campaign_nb int NOT NULL,
    subpart varchar(10),
    UNIQUE(cd_loc,cd_gp_biol,cd_method,campaign_nb,subpart)
);
-- CREATE INDEX gp_event_cd_punto_ref_fkey ON main.gp_event(cd_pt_ref);
-- CREATE INDEX gp_event_cd_gp_biol_fkey ON main.gp_event(cd_gp_biol);
-- CREATE INDEX gp_event_cd_protocol_fkey ON main.gp_event(cd_protocol);



CREATE TABLE main.event
(
    cd_event serial PRIMARY KEY,
    event_id text UNIQUE,
    cd_gp_event int REFERENCES main.gp_event(cd_gp_event) ON DELETE CASCADE NOT NULL,
    num_replicate int NOT NULL, --numero de la trampa o del evento (dentro del gp_event)
    description_replicate text, --part of the eventID that describes the replicate
    date_time_begin timestamp,
    date_time_end timestamp,
    locality_verb text,
    event_remarks text,
    cds_creator integer[] ,-- note: we can't use foreign keys because more than  one person might have created the event, otherwise we need to make more tables.
    created date,
    cd_loc int REFERENCES main.location(cd_loc),
    UNIQUE (cd_gp_event, num_replicate),
    CHECK (date_time_begin<date_time_end OR date_time_begin IS NULL OR date_time_end IS NULL)
)
;
-- CREATE INDEX event_cd_gp_event_fkey ON main.event(cd_gp_event);

CREATE TABLE main.def_tax_rank
(
    cd_rank varchar(6) PRIMARY KEY,
    tax_rank text UNIQUE NOT NULL,
    tax_rank_spa text UNIQUE NOT NULL,
    rank_level int UNIQUE NOT NULL,
    marker varchar(7) UNIQUE,
    placement_marker varchar(40),
    CHECK (placement_marker IN ('before epithet', 'absent'))
);


INSERT INTO main.def_tax_rank
VALUES
    ('FORM', 'form', 'forma', 1,'f.','before epithet'),
    ('SUBVAR', 'subvariety','subvariedad', 2,'subvar.', 'before epithet'),
    ('VAR', 'variety','variedad', 3,'var.', 'before epithet'),
    ('SUBSP', 'subspecies', 'subespecie', 9, 'subsp.', 'before epithet'),
    ('SP', 'species', 'especie', 10, 'sp.', 'absent'),
    ('SPSP', 'superspecies', 'superespecie', 11, NULL, 'absent'),
    ('SGN', 'subgenus', 'subgénero', 19, 'subgen.', 'before epithet'),
    ('GN', 'genus', 'género', 20,'gen.', 'absent'),
    ('TR', 'tribe', 'tribu', 21, NULL, 'absent'),
    ('SFAM', 'subfamily', 'subfamilia', 99,'subfam.', 'absent'),
    ('FAM', 'family', 'familia', 100,'fam.', 'absent'),
    ('SPFAM', 'superfamily', 'superfamilia', 101,NULL, 'absent'),
    ('SOR', 'suborder', 'suborden', 199,NULL, 'absent'),
    ('OR', 'order', 'orden', 200,'ord.', 'absent'),
    ('LEG', 'legion', 'legión', 201,NULL, 'absent'),
    ('SCL', 'subclass', 'subclase', 299,NULL, 'absent'),
    ('CL', 'class', 'clase', 300, 'cl.', 'absent'),
    ('SPCL', 'superclass', 'superclase', 301,NULL, 'absent'),
    ('SPHY', 'subphylum', 'subfilo', 399,NULL, 'absent'),
    ('PHY', 'phylum', 'filo', 400,'phyl.', 'absent'),
    ('SPPHY', 'superphylum', 'superfilo', 401,NULL, 'absent'),
    ('SKG', 'subkingdom', 'subreino', 499,NULL, 'absent'),
    ('KG', 'kingdom', 'reino', 500,NULL, 'absent'),
    ('SPKG', 'superkingdom', 'superreino', 501,NULL, 'absent'),
    ('SDOM', 'subdomain', 'subdominio', 999,NULL, 'absent'),
    ('DOM', 'domain', 'dominio', 1000,NULL, 'absent');

CREATE TABLE main.taxo
(
    cd_tax serial PRIMARY KEY,
    name_tax text UNIQUE,
    authorship text,
    cd_rank varchar(6) REFERENCES main.def_tax_rank(cd_rank) ON DELETE SET NULL ON UPDATE CASCADE,
    cd_parent int REFERENCES main.taxo(cd_tax) ON UPDATE CASCADE,
    UNIQUE(name_tax, authorship),
    CHECK(cd_rank IN ('KG', 'DOM') OR cd_parent IS NOT NULL),
    CHECK(
        CASE
            WHEN cd_rank IN ('SGN', 'GN', 'TR', 'SFAM', 'FAM', 'SPFAM', 'SOR', 'OR', 'LEG', 'SCL', 'CL', 'SPCL', 'SPHY', 'PHY', 'SPPHY', 'SKG', 'KG', 'SPKG', 'SDOM', 'DOM') THEN name_tax ~ '^[A-Z][a-z-]+$' OR name_tax ~ '^[A-Z][a-z-]+ (ordo )?incertae sedis$'
            WHEN cd_rank='SP' THEN name_tax ~ '^[A-Z][a-z-]+ [a-z-]+$'
            WHEN cd_rank IN ('FORM','SUBVAR','VAR','SUBSP') THEN name_tax ~ '^[A-Z][a-z-]+ [a-z-]+ [a-z-]+$'
        END
        )
);
-- CREATE INDEX taxo_cd_parent_key ON main.taxo(cd_parent);
-- CREATE INDEX taxo_cd_rank_key ON main.taxo(cd_rank);

CREATE TABLE main.morfo_taxo
(
    cd_morfo serial PRIMARY KEY,
    cd_tax int REFERENCES main.taxo(cd_tax) ON DELETE CASCADE ON UPDATE CASCADE,
    name_morfo text NOT NULL,
    verbatim_taxon text,
    description text,
    --min_level int REFERENCES main.def_nivel_taxo(cd_niv_taxo),
    --max_level int REFERENCES main.def_nivel_taxo(cd_niv_taxo),
    pseudo_rank varchar(6) REFERENCES main.def_tax_rank(cd_rank) ON DELETE SET NULL ON UPDATE CASCADE,
    cds_tax_possibilities integer[]
);
-- CREATE INDEX morfo_taxo_cd_gp_biol_key ON main.morfo_taxo(cd_gp_biol);
-- CREATE INDEX morfo_taxo_cd_tax_key ON main.morfo_taxo(cd_tax);
-- CREATE INDEX morfo_taxo_pseudo_rank_key ON main.morfo_taxo(pseudo_rank);

CREATE TABLE main.register
(
    cd_reg serial PRIMARY KEY,
    cd_event int REFERENCES main.event(cd_event) ON DELETE CASCADE NOT NULL,
    cds_recorded_by int[],
    date_time timestamp,
    locality_verb text,
    qt_int integer,
    qt_double double precision,
    remarks text,
    occurrence_id text UNIQUE,
    cd_loc int REFERENCES main.location(cd_loc),
    UNIQUE (cd_event,occurrence_id)
);
-- CREATE INDEX registros_cd_event_idx ON main.registros USING GIST(the_geom);
-- CREATE INDEX registros_cd_event_fkey ON main.registros(cd_event);
-- CREATE INDEX registros_cd_tax_fkey ON main.registros(cd_tax);
-- CREATE INDEX registros_cd_morfo_fkey ON main.registros(cd_morfo);

CREATE TABLE main.identification -- note that in the case of identification reporting to individuals that are present in various registers, we have the problem of needing to go through the registers to apply these identification to the individuals. It will be complicated, but it actually represents better the reality of the data. It is particularly messy in the cases where there are various identifications through time: we will need to resolve the multiple registers + multiple identification mess before knowing the identity of the individual, let's add that sometimes there is a parent identification, and you got a very difficult problem to manage.
(
	cd_identif serial PRIMARY KEY,
	cd_reg int REFERENCES main.register(cd_reg),
	cd_tax int REFERENCES main.taxo(cd_tax),
	cd_morfo int REFERENCES main.morfo_taxo (cd_morfo),
	parent_identif int REFERENCES main.identification(cd_identif), --I am not completely convinced here but having a parent identification allows to manage the fact that sometimes the identification based on a register applies to all the registers that have been noted as the same species
	date_identif date,
	identified_by int[],
	remarks_identif text,
	catalog text,
	voucher text
);

CREATE TABLE main.individual
(
	cd_ind serial PRIMARY KEY,
    organism_id text,
	uniq_in_project int REFERENCES main.project(cd_project),
	uniq_in_gp_event int REFERENCES main.gp_event(cd_gp_event),
	uniq_in_event int REFERENCES main.event(cd_event),
	uniq_in_register int REFERENCES main.register(cd_reg),
	tag text,
	UNIQUE(uniq_in_project,uniq_in_gp_event,uniq_in_event,uniq_in_register,tag)
);


CREATE TABLE main.def_subindividual_part
(
    cd_part smallserial PRIMARY KEY,
    part text,
    cd_gp_biol char(4) REFERENCES main.def_gp_biol(cd_gp_biol),
    UNIQUE(part, cd_gp_biol)
);

CREATE TABLE main.subindividual
(
	cd_subind serial PRIMARY KEY,
	cd_ind int REFERENCES main.individual (cd_ind),
	cd_part smallint REFERENCES main.def_subindividual_part(cd_part),
	part_number int,
	parent_cd_subind int REFERENCES main.subindividual(cd_subind),
	uniq_in_project int REFERENCES main.project(cd_project),
	uniq_in_gp_event int REFERENCES main.gp_event(cd_gp_event),
	uniq_in_event int REFERENCES main.event(cd_event),
	uniq_in_register int REFERENCES main.register(cd_reg),
	tag text,
	UNIQUE(cd_ind,cd_part,part_number)
);

CREATE TABLE main.individual_characteristics
(
    cd_reg int REFERENCES main.register(cd_reg),
    cd_ind int REFERENCES main.individual(cd_ind),
    cd_var smallint REFERENCES main.def_var,
    ind_char_int integer,
    ind_char_double double precision,
    ind_char_text text,
    ind_char_categ integer REFERENCES main.controlled_vocab(cd_categ),
    ind_char_bool boolean,
    CHECK( ((ind_char_categ IS NOT NULL)::integer + (ind_char_int IS NOT NULL)::integer + (ind_char_int IS NOT NULL)::integer + (ind_char_categ IS NOT NULL)::integer + ind_char_bool::integer)=1)
    --UNIQUE (cd_reg,cd_var_ind_char,COALESCE(cd_categ::double,ind_char_int::double,ind_char_double))
    --might be useful to create more specific constraints depending on whether it is an integer, double or categorial variable.
);


CREATE TABLE main.subindividual_characteristics
(
    cd_reg int REFERENCES main.register(cd_reg),
    cd_subind int REFERENCES main.subindividual(cd_subind),
    cd_var smallint REFERENCES main.def_var,
    ind_char_int integer,
    ind_char_double double precision,
    ind_char_text text,
    ind_char_categ integer REFERENCES main.controlled_vocab(cd_categ),
    ind_char_bool boolean,
    CHECK( ((ind_char_categ IS NOT NULL)::integer + (ind_char_int IS NOT NULL)::integer + (ind_char_int IS NOT NULL)::integer + (ind_char_categ IS NOT NULL)::integer + ind_char_bool::integer)=1)
    --UNIQUE (cd_reg,cd_var_ind_char,COALESCE(cd_categ::double,ind_char_int::double,ind_char_double))
    --might be useful to create more specific constraints depending on whether it is an integer, double or categorial variable.
);


-- Extra tables
CREATE TABLE main.project_characteristics
(
	cd_project int REFERENCES main.project(cd_project),
	cd_var smallint REFERENCES main.def_var,
	project_char_int integer,
	project_char_double double precision,
	project_char_text text,
	project_char_categ int REFERENCES main.controlled_vocab(cd_categ),
	project_char_bool boolean
);

CREATE TABLE main.gp_event_characteristics
(
	cd_gp_event int REFERENCES main.gp_event(cd_gp_event),
	cd_var smallint REFERENCES main.def_var,
	gp_event_char_int int,
	gp_event_char_double double precision,
	gp_event_char_text text,
	gp_event_char_categ int REFERENCES main.controlled_vocab(cd_categ),
	gp_event_char_bool boolean
);

CREATE TABLE main.event_characteristics
(
	cd_event int REFERENCES main.event(cd_event),
	cd_var smallint REFERENCES main.def_var,
	event_char_int int,
	event_char_double double precision,
	event_char_text text,
	event_char_categ int REFERENCES main.controlled_vocab(cd_categ),
	event_char_bool boolean
);

CREATE TABLE main.reg_characteristics
(
	cd_reg int REFERENCES main.register(cd_reg),
	cd_var smallint REFERENCES main.def_var,
	reg_char_int int,
	reg_char_double double precision,
	reg_char_text text,
	reg_char_categ int REFERENCES main.controlled_vocab(cd_categ),
	reg_char_bool boolean
);


CREATE TABLE main.taxon_characteristics
(
	cd_tax int REFERENCES main.taxo(cd_tax),
	cd_var smallint REFERENCES main.def_var,
	tax_char_int int,
	tax_char_double double precision,
	tax_char_text text,
	tax_char_categ int REFERENCES main.controlled_vocab(cd_categ),
	tax_char_bool boolean
);

CREATE TABLE main.organization_characteristics
(
	cd_org int REFERENCES main.organization(cd_org),
	cd_var smallint REFERENCES main.def_var,
	org_char_int int,
	org_char_double double precision,
	org_char_text text,
	org_char_categ int REFERENCES main.controlled_vocab(cd_categ),
	org_char_bool boolean
);

COMMIT;
