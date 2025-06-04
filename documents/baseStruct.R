# This file is to create diagrams of the database
require(RPostgres)
require(dm)
require(DiagrammeRsvg)
require(rsvg)
require(png)

require(RPostgres)
biol<-dbConnect(Postgres(),"iavh_biol")

tables_biol<- data.frame(
  table_name = dbGetQuery(biol,"SELECT table_name FROM information_schema.tables WHERE table_schema='main'")$table_name
)

dm_object <- dm_from_con(biol,schema=c("main"), table_names = tables_biol$table_name, learn_keys = T)
A<-dm_object %>%
  dm_draw(view_type = "all")
#A
t_file<-tempfile(fileext = ".png")
DiagrammeRsvg::export_svg(A) %>% charToRaw %>% rsvg_png(file = t_file)
plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '',xlim=c(0,1),ylim=c(0,1))
png <- readPNG(t_file)
rasterImage(png,0,0,1,1)

tables_biol$def_accesory = rep(F,nrow(tables_biol))
tables_biol$def_accesory [tables_biol$table_name %in% c("controlled_vocab","def_gp_biol","def_location_type","def_measurement_type","def_method","def_org_rel","def_organisation_level","def_subindividual_part","def_tax_rank","def_unit","def_var","def_var_gp","organization_type","proj_rel_type","project_relationship")] <- T


A<-dm_object %>%
  dm_set_colors("red"=all_of(tables_biol$table_name[tables_biol$def_accesory]))%>%
  dm_draw(view_type = "all")
A

dbDisconnect(biol)
