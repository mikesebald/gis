library(rgdal)
library(cleangeo)
library(broom)
library(dplyr)
library(data.table)
library(ggmap)
library(rgeos)
library(geojsonio)

# The input file geodatabase
fgdb = "C:/DPD_Shapefiles/repaired/DE_DPDPLZ_20161028.gdb"
ogrListLayers(fgdb)
fc = readOGR(dsn = fgdb,
             layer = "DE_DPDPLZ_20161028_Repaired")

data.dt <- as.data.table(fc@data)

report <- as.data.table(clgeo_CollectionReport(fc))
isvalid <- report$valid
summary <- clgeo_SummaryReport(report)
warnings <- report[valid == FALSE]
susp_feat <- clgeo_SuspiciousFeatures(report)

susp_dpdplzs <- as.character(data.dt[!isvalid, DPDPLZ])

y <- lapply(fc@polygons[susp_feat], tidy)
z <- rbindlist(y)
  
setkey(z, long, lat, id)
doubles <- z[duplicated(z[,.(long, lat, id)])]
duplicates <- subset(z, long %in% doubles[, long] &
                       lat %in% doubles[, lat] &
                       id %in% doubles[, id])

length(susp_feat)
writeOGR(fc[susp_feat, ], ".", "suspects_repaired", driver="ESRI Shapefile")

non_susp_feat <- setdiff((1:length(fc)), susp_feat)
length(non_susp_feat)
writeOGR(fc[non_susp_feat, ], ".", "non_suspects_repaired", driver="ESRI Shapefile")

goodones <- spTransform(fc[non_susp_feat, ], CRS("+proj=longlat +datum=WGS84"))
geojson_write(goodones, file = "goodones")

badones <- spTransform(fc[susp_feat, ], CRS("+proj=longlat +datum=WGS84"))
geojson_write(badones, file = "badones")

dpdplz <- data.dt[as.numeric(duplicates$id), DPDPLZ]
duplicates <- cbind(duplicates, dpdplz)
setkey(duplicates, dpdplz)

write.csv(duplicates, "duplicates.csv")
