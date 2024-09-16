# for ShapeFile (shp) manipulation
library(sf)
# for .dbf file manipulation
library(foreign)

icc <- c("AL","AD","AT","BY","BE","BA","BG","HR","CY","CZ","DK","EE","FO","FI","FR","DE","GI","GR","HU","IS","IE","IM","IT","RS","LV","LI","LT","LU","MK","MT","MD","MC","ME","NL","NO","PL","PT","RO","RU","SM","RS","SK","SI","ES","SE","CH","UA","GB","VA","RS","GE","ND")
countries <- c("Albania","Andorra","Austria","Belarus","Belgium","Bosnia  and  Herzegovina","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark","Estonia","Faroe Islands","Finland","France","Germany","Gibraltar","Greece","Hungary","Iceland","Ireland","Isle  of  Man","Italy","Kosovo","Latvia","Liechtenstein","Lithuania","Luxembourg","Macedonia","Malta","Moldova","Monaco","Montenegro","Netherlands","Norway","Poland","Portugal","Romania","Russia","San  Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Ukraine","United  Kingdom","Vatican  city","Yugoslavia", "Georgia","Not Defined")
dictionary <- data.frame()
dictionary <- rbind(dictionary, countries)
colnames(dictionary) <- icc

node_fields <- "RStationID,NAMA1,ICC"
path <- "./RAILS/FullEurope"

shp_path <- paste(path, "RailrdC.shp", sep = "/")
# dbf_path <- paste(path, "RailrdC.dbf", sep = "/")

epsg <- 4258

sql_query <- paste("SELECT", node_fields, "FROM RailrdC WHERE F_CODE = 'AQ125'")
shp <- st_read(shp_path, query = sql_query)
# dbf <- read.dbf(dbf_path)

df <- data.frame(matrix(ncol = 6, nrow = length(shp$ICC)))
names(df) <- c("NodeID", "NodeLabel", "ICC", "Lon", "Lat", "Country")

df$Lat <- st_coordinates(st_transform(shp, epsg)$geometry)[, 1]
df$Lon <- st_coordinates(st_transform(shp, epsg)$geometry)[, 2]
df$ICC <- shp$ICC
df$NodeLabel <- shp$NAMA1
df$Country <- c(dictionary[df$ICC])
df <- df[!duplicated(df), ]
range <- length(df$Lat)
df$NodeID <- paste(df$ICC, seq(range), sep = ".")
 
library(data.table)
fwrite(df, paste(path, "Nodes.csv", sep = "/"))