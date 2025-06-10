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


dbDisconnect(biol)
