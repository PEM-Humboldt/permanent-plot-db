# This script objective is to create an empty database with the architecture defined in sql/table_creation.sql

require(RPostgres)
admin_db_param <- list(drv=Postgres(),dbname="postgres") 
dbname <- "iavh_biol" 
dbParam<-list(drv = Postgres(), dbname=dbname)
# Admin connection: drop and create again the database
dbAdmin<-do.call(dbConnect,args=admin_db_param)
dbExecute(dbAdmin, paste0("DROP DATABASE ",dbQuoteIdentifier(dbAdmin,dbname)))
dbExecute(dbAdmin, paste0("CREATE DATABASE ",dbQuoteIdentifier(dbAdmin,dbname)))
dbDisconnect(dbAdmin)
# Prepare database: postgis extension
db<-do.call(dbConnect,dbParam)
dbExecute(db,"CREATE EXTENSION postgis")
# Execute table creation script
system(paste0("psql ",dbname," -f ../sql/tableCreation.sql"))
dbDisconnect(db)
