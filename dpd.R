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

# List all feature classes in a file geodatabase
# subset(ogrDrivers(), grepl("GDB", name))
fc_list = ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc = readOGR(dsn=fgdb,layer="DE_DPDPLZ_20161028_Repaired")

data.dt <- as.data.table(fc@data)

# Determine the FC extent, projection, and attribute information
# summary(fc)

report <- as.data.table(clgeo_CollectionReport(fc))
summary <- clgeo_SummaryReport(report)

warnings <- report[valid == FALSE]

susp_feat <- clgeo_SuspiciousFeatures(report)

x <- lapply(fc@polygons[susp_feat], function(x) {x})
y <- lapply(x, tidy)
z <- rbindlist(y)

setkey(z, long, lat, id)
doubles <- z[duplicated(z[,.(long, lat, id)])]
duplicates <- subset(z, long %in% doubles[, long] &
                       lat %in% doubles[, lat] &
                       id %in% doubles[, id])

# plot(fc[susp_feat, ])

t <- as.data.table(table(doubles$id))
setorder(t, -N)
# View(t)
# 
# plot(fc[12235, ])
# plot(fc[165, ])
# plot(fc[260, ])
# 
# p <- fc[260, ]
# fixed  <- gSimplify(p, tol = 0.00001)
# fixed <- gBuffer(fixed, byid = TRUE, width = 0)
# plot(fixed)
# clgeo_CollectionReport(fixed)

# writeOGR(fc[susp_feat, ], ".", "suspects_new", driver="ESRI Shapefile")

nrow(fc[susp_feat, ])

# getwd()

x <- c(1:length(fc))
y <- x %in% susp_feat
non_susp_feat <- x[!y]

# writeOGR(fc[non_susp_feat, ], ".", "non_suspects_new", driver="ESRI Shapefile")


goodones <- spTransform(fc[non_susp_feat, ], CRS("+proj=longlat +datum=WGS84"))
geojson_write(goodones, file = "c:/temp/goodones")

badones <- spTransform(fc[susp_feat, ], CRS("+proj=longlat +datum=WGS84"))
geojson_write(badones, file = "c:/temp/badones")


write.csv(duplicates, "c:/temp/duplicates.csv")



# 
# 
# shape <- spTransform(fc[susp_feat, ], CRS("+proj=longlat +datum=WGS84"))
# mapImage <- get_map(location = c(lon = 9.5, lat = 51),
#                     color = "color",
#                     source = "google",
#                     maptype = "roadmap",
#                     zoom = 6)
# 
# ggmap(mapImage) +
#   geom_polygon(
#     aes(x = long,
#         y = lat,
#         group = group),
#     data = shape,
#     color = "red",
#     fill = "yellow",
#     alpha = 0.5
#   ) +
#   labs(x = "Longitude",
#        y = "Latitude")
# 
