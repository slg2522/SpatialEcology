# Load packages.
library(rgdal)
library(raster)
library(rgeos)
library(dismo)
library(sf)
library(geojsonsf)
library(geojsonR)

region2 <- '{
    "type": "Feature",
    "geometry": {
      "type": "Polygon", 
      "coordinates": [
      [
        [
          -170.683964,
          56.164425
        ],
        [
          -164.12974,
          59.855408
        ],
        [
          -164.381355,
          60.22228
        ],
        [
          -164.918427,
          60.359074
        ],
        [
          -165.378576,
          60.575201
        ],
        [
          -166.542038,
          60.405894
        ],
        [
          -167.623684,
          60.249236
        ],
        [
          -168.58986,
          59.929488
        ],
        [
          -174.318629,
          58.275844
        ],
        [
          -174.013049,
          57.949867
        ],
        [
          -173.507136,
          57.776624
        ],
        [
          -174.054426,
          57.751585
        ],
        [
          -173.904469,
          57.393853
        ],
        [
          -173.6281,
          57.104479
        ],
        [
          -173.266998,
          56.681655
        ],
        [
          -172.526157,
          56.471937
        ],
        [
          -172.275877,
          56.6892
        ],
        [
          -171.428619,
          56.44931
        ],
        [
          -170.683964,
          56.164425
        ]
      ]
      ]
      }
    }'


sf <- geojson_sf(region2)

head(sf)
plot(sf)

st_write(sf, "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Regions/region2/region2Location.shp")

shp <- shapefile("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Regions/region2/region2Location.shp")
plot(shp)
head(shp)


library(terra)
v <- vect(shp)
r <- terra::rast(v, resolution = 1, vals=1)
r <- terra::mask(r, v)
g <- as.polygons(r, dissolve=FALSE)
plot(g)
plot(shp, add=TRUE)
region2Crop <- crop(g, v)
plot(region2Crop)

s <- sf::st_as_sf(region2Crop)

st_write(s, "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Regions/region2/region2Grid.shp")

test <- shapefile("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Regions/region2/region2Grid.shp")

plot(test)

