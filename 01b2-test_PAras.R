## Testing ground.
# Shown here: I can avoid st_buffer (to resolve problematic polys)...
# ...st_union() or st_combine() to squish polys together before rasterizing.
# In fact, something in st_combine() seemed to be dropping out overlapping areas.
# Asked on StackExchange: https://gis.stackexchange.com/questions/319464/why-does-fasterize-gives-different-output-for-multipolygons-with-and-without-int

# PA.IIV.terr.1km <- st_read(dsn = paste0(data.dir,"/WDPA_Apr2019-shapefile/PA.IIV.terr.1km.shp"))
# PA.IV.terr.1km <- st_read(dsn = paste0(data.dir,"/WDPA_Apr2019-shapefile/PA.IV.terr.1km.shp"))
# PA.IVI.terr.1km <- st_read(dsn = paste0(data.dir,"/WDPA_Apr2019-shapefile/PA.IVI.terr.1km.shp"))


# Pull out Yellowstone Grand Teton
PA.IIV.terr.1km$NAME <- as.character(PA.IIV.terr.1km$NAME)
YNPGT <- PA.IIV.terr.1km %>%
  dplyr::filter(NAME == "Grand Teton" | NAME == "Grand Teton Proposed or Recom")
YNPGT$DESIG_ENG <- droplevels(YNPGT$DESIG_ENG)
class(YNPGT)
YNPGT <- st_transform(YNPGT, crs) # Set to CRS of reference raster
# st_write(YNPGT, "WDPA_Apr2019-shapefile/YNPGT.shp")
bbox <- st_bbox(YNPGT) # Keep bounding box for plotting
# # But if I want it smaller
# df <- data.frame(lon=c(-9210000,-9210000,-9180000,-9180000),
#                  lat=c(5400000,5460000,5460000,5400000))
# # Need to close it: 1st & last pts must be identical
# df <- rbind(df, df[1,])
# tinybox <- st_sf(st_sfc(st_polygon(list(as.matrix(df)))), crs = crs)
# bbox <- st_bbox(tinybox)

# Alt: for redwood
PA.IVI.terr.1km$NAME <- as.character(PA.IVI.terr.1km$NAME)
redwood <- PA.IVI.terr.1km %>%
  dplyr::filter(substr(NAME, 1, 7) == "Redwood")
redwoodish <- PA.IVI.terr.1km %>%
  dplyr::filter(NAME == "Lake Earl" | NAME == "Redwood")
redwoodNP <- redwood %>%
  dplyr::filter(NAME == "Redwood")

redwood <- st_transform(redwood, crs)
redbox <- st_bbox(redwood)
# -10511152   4729262 -10425092   5214084 

redwoodish <- st_transform(redwoodish, crs)

redwoodNP <- st_transform(redwoodNP, crs)
rednpbox <- st_bbox(redwoodNP)
# -10463223   5130348 -10425092   5214084 


# Read in shapefile.
# YNPGT <- st_read("WDPA_Apr2019-shapefile/YNPGT.shp")
bbox <- st_bbox(YNPGT) # Keep bounding box for plotting b/c global extent
# Rasterize. "Sum" so any spot with poly is retained (e.g., not just 'first' poly).
YNPGT.r <- YNPGT %>% fasterize(raster = ref.ras, fun = "sum")

# Combine that to flatten all polygons into one -- resolves internal boundaries
combo <- st_combine(YNPGT)
# Rasterize 
combo <- st_sf(combo) # combine outputs sfc; need sf for fasterize.
combo.r <- combo %>% fasterize(raster = ref.ras, fun = "sum")

# Union that to flatten all polygons into one -- drops internal boundaries.
union <- st_union(YNPGT)
# Rasterize 
union <- st_sf(union) # combine outputs sfc; need sf for fasterize.
union.r <- union %>% fasterize(raster = ref.ras, fun = "sum")


par(mfrow=c(2,3))
plot(st_geometry(YNPGT),
     xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]),
     col = c("green", "pink"))
title("YNP-GT orig")
plot(st_geometry(combo),
     xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]),
     col = c("green", "pink"))
title("YNP-GT  st_combine")
plot(st_geometry(union),
     xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]),
     col = c("green", "pink"))
title("YNP-GT  st_union")
plot(YNPGT.r,
     xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]),
     col = c("green", "pink"))
title("YNP-GT raster from orig")
plot(combo.r,
     xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]),
     col = c("green", "pink"))
title("YNP-GT raster from st_combine")
plot(union.r,
     xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]),
     col = c("green", "pink"))
title("YNP-GT raster from st_union")
