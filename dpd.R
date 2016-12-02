library(rgdal)
library(cleangeo)
library(broom)
library(dplyr)
library(data.table)
library(ggmap)
library(rgeos)
library(geojsonio)
library(microbenchmark)

# The input file geodatabase
fgdb = "C:/DPD_Shapefiles/repaired/DE_DPDPLZ_20161028.gdb"
ogrListLayers(fgdb)
fc = readOGR(dsn = fgdb,
             layer = "DE_DPDPLZ_20161028_Repaired")

# Analyze the shapefile to identify suspicious polygons
report <- as.data.table(clgeo_CollectionReport(fc))
isvalid <- report$valid
summary <- clgeo_SummaryReport(report)
warnings <- report[valid == FALSE]
susp_feat <- clgeo_SuspiciousFeatures(report)

# "Fix" the suspicious polygons by just eliminating redundant vertices. 
# Combine all polygons together to write it into one geoJson file.
tdir <- getwd()
setwd("output/")

# Option 1 - not working well
# x <- fc[susp_feat[1], ]
# p <- x@polygons[[1]]@Polygons[[1]]@coords
# f <- rbind(p[!duplicated(p), ], p[nrow(p), ])
# x@polygons[[1]]@Polygons[[1]]@coords <- f
# fixed <- spTransform(x, CRS("+proj=longlat +datum=WGS84"))
# 
# for (i in 2:length(susp_feat)) {
#   message(paste0("Processing shape ", i))
#   x <- fc[susp_feat[i], ]
#   p <- x@polygons[[1]]@Polygons[[1]]@coords
#   f <- rbind(p[!duplicated(p), ], p[nrow(p), ])
#   x@polygons[[1]]@Polygons[[1]]@coords <- f
#   fixed <- rbind(fixed, spTransform(x, CRS("+proj=longlat +datum=WGS84")), 
#                  makeUniqueIDs = TRUE)
# }
# 
# geojson_write(fixed, file = "fixed")

# Option 2
for (i in 1:length(susp_feat)) {
  message(paste0("Processing shape ", i))
  x <- fc[susp_feat[i], ]
  p <- x@polygons[[1]]@Polygons[[1]]@coords
  f <- rbind(p[!duplicated(p), ], p[nrow(p), ])
  x@polygons[[1]]@Polygons[[1]]@coords <- f
  fixed <- spTransform(x, CRS("+proj=longlat +datum=WGS84"))
  geojson_write(fixed, file = paste0("fixed_", susp_feat[i]))
}

# continue here


# Write the suspects unchanged into a shapefile
writeOGR(fc[susp_feat, ], ".", "suspects", driver="ESRI Shapefile")

# Write the non-suspects into a shapefile and into geoJson
non_susp_feat <- setdiff((1:length(fc)), susp_feat)
writeOGR(fc[non_susp_feat, ], ".", "non_suspects", driver="ESRI Shapefile")
goodones <- spTransform(fc[non_susp_feat, ], CRS("+proj=longlat +datum=WGS84"))
geojson_write(goodones, file = "goodones")

# badones <- spTransform(fc[susp_feat, ], CRS("+proj=longlat +datum=WGS84"))
# geojson_write(badones, file = "badones")

# Extract the duplicate DPD-PLZs for reporting purposes
data.dt <- as.data.table(fc@data)
y <- lapply(fc@polygons[susp_feat], tidy)
z <- rbindlist(y)

setkey(z, long, lat, id)
doubles <- z[duplicated(z[,.(long, lat, id)])]
duplicates <- subset(z, long %in% doubles[, long] &
                       lat %in% doubles[, lat] &
                       id %in% doubles[, id])

dpdplz <- data.dt[as.numeric(duplicates$id), DPDPLZ]
duplicates <- cbind(duplicates, dpdplz)
setkey(duplicates, dpdplz)

write.csv(duplicates, "duplicates.csv")

setwd(tdir)
