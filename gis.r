library(sp)
library(rgdal)
library(geojsonio)
library(broom)
library(data.table)
library(dplyr)


file_name <- "../gis_data/bw/AX_KommunalesGebiet.shp"
# shape_layers <- ogrListLayers(file_name)
# ogrInfo(file_name, "AX_KommunalesGebiet")
shape_kommunal <- readOGR(file_name, layer = "AX_KommunalesGebiet", 
                          use_iconv = TRUE, 
                          encoding = "UTF-8")
data_kommunal_dt <- as.data.table(shape_kommunal@data)
head(data_kommunal_dt)

# will do more stuff tomorrow


# #############################################################################


file_name <- "c:/temp/inspire open data/opmplc_essh_gb/data/HP/HP_Building.shp"
shape_layers <- ogrListLayers(file_name)
# ogrInfo(file_name, "AX_KommunalesGebiet")
shape <- readOGR(file_name, layer = "HP_Building")
dt <- as.data.table(shape)
head(dt)

names(shape)

shape@bbox


# #############################################################################


file_name <- "c:/temp/inspire open data/opmplc_essh_gb/data/HP/HP_NamedPlace.shp"
shape_layers <- ogrListLayers(file_name)
# ogrInfo(file_name, "AX_KommunalesGebiet")
shape <- readOGR(file_name, layer = "HP_NamedPlace")
dt <- as.data.table(shape)
head(dt)

slotNames(shape)
names(shape)

shape@bbox

# #############################################################################


# layers1 <- ogrListLayers("../gis_data/dpd_complete/DPDPLZ_SHAPES/DE_DPDPLZ_20161028.shp")
shape1 <- readOGR("c:/DPD_Shapefiles/DPDPLZ_SHAPES/DE_DPDPLZ_20161028.shp",
                  layer = "DE_DPDPLZ_20161028")

polys.df <- tidy(shape1)

fromMongo <- read.csv("c:/temp/dpd/dpdplz.txt")

polys.dt <- as.data.table(polys.df)
mongo.dt <- as.data.table(fromMongo)




shape <- spTransform(shape1, CRS("+proj=longlat +datum=WGS84"))

geojson_write(shape, file = "c:/temp/DE_DPDPLZ_20161028_2")


# #############################################################################

area <- readShapePoly("../gis_data/dpd/DPDPLZ_20161024.shp")

colors <- brewer.pal(9, "BuGn")

mapImage <- get_map(location = c(lon = 9.5, lat = 51),
                    color = "color",
                    source = "google",
                    maptype = "roadmap",
                    zoom = 6)

area.points <- fortify(area)


ggmap(mapImage) +
  geom_polygon(
    aes(x = long,
        y = lat,
        group = group),
    data = area.points[c(1:1000),],
    color = colors[9],
    fill = colors[6],
    alpha = 0.5
  ) +
  labs(x = "Longitude",
       y = "Latitude")


# #############################################################################


shapes <- readOGR("../gis_data/dpd/DPDPLZ_20161024.shp",
                  layer = "DPDPLZ_20161024",
                  verbose = FALSE)

leaflet(shapes) %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5
  )

# #############################################################################

topoData <- readLines("e:/ubuntu_transfer/dpd_3.json") %>% paste(collapse = "\n")

leaflet() %>% setView(lng = 7, lat = 51, zoom = 6) %>%
  addTiles() %>%
  addTopoJSON(topoData, weight = 1, color = "#444444", fill = FALSE)


# #############################################################################

ogrDrivers()

# #############################################################################

layers <- ogrListLayers("e:/ubuntu_transfer/dpd_3.json")

shape <- readOGR("e:/ubuntu_transfer/dpd_3.json", 
                 layer = "OGRGeoJSON")

plot(shape)
