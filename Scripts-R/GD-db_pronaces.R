library(RSQLite)
library(stringr)

db_conect <- dbConnect(drv = SQLite(), "../../Database/pronaces.db")

tablas <- dbListTables(db_conect)

consulta <- str_replace("SELECT name FROM files", "files", tablas[grepl("files", tablas)])

dbGetQuery(db_conect, consulta)

consulta <- str_replace("SELECT name FROM files", "files", tablas[grepl("folders", tablas)])

dbGetQuery(db_conect, consulta)


dbGetQuery(db_conect, "SELECT name FROM drive_files where mimeType like '%pdf%'")


arch_info <- dbListFields(db_conect, "drive_files")
folder_info <- dbListFields(db_conect, "drive_folders")

consulta <- str_replace("SELECT _parent FROM files inner join folder", "files", tablas[grepl("folders", tablas)])
dbGetQuery(db_conect, consulta)

campos <- "f.name as name, f._parent AS dentro, t.name AS n_dentro"
origen <- "drive_folders f LEFT OUTER JOIN drive_folders t ON f._parent = t.id"
condicion <- "t.name LIKE '%Libro Semillas para%'"
consulta <- paste("SELECT", campos, "FROM", origen, "WHERE", condicion)
resultados <- dbGetQuery(db_conect, consulta)
resultados


