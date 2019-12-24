#####################
#### RASTERIZING ####
#####################


#### OVERLAPS? ######################################################
## Before fasterizing, how big of an issue will overlaps be?
# I can specify the order of rasterization.
# So can I put biggest PA on top, so that they'll eat up small inclusions?
# For example, any wilderness or wild/scenic river within an NP will be the NP.
# I could set IDs based on PA size and then select max in fasterize...

## Look at Greater Yellowstone zone
GYE <- PA.IVInoass.1km %>%
  filter(., grepl("Yellowstone|Teton|Absaroka|Washakie", NAME)) %>%
  filter(GIS_AREA > 30) # nix small sites elsewhere 
plot(st_geometry(GYE))
(gye.bbox <- st_bbox(GYE))
# Make bigge bbox
df <- data.frame(lon=c(-112, -112, -109, -109),
                 lat=c(43.5, 45.5, 43.5, 45.5))
# Need to close it: 1st & last pts must be identical
df <- rbind(df, df[1,])
gye.bbox <- st_bbox(st_sf(st_sfc(st_polygon(list(as.matrix(df)))), crs = st_crs(redwood)))



## Look at Redwoods zone (exclude spot in MN)
redwood <- PA.IVInoass.1km %>%
  filter(., grepl("Redwood", NAME)) %>%
  filter(!NAME == "WRP_Redwood, MN (27127)") # 20 obs
bbox <- st_bbox(redwood)
# -124.20333   37.00931 -122.03447   41.84049
# Make smaller bbox
df <- data.frame(lon=c(-124, -124, -122.5, -122.5),
                 # lat=c(39, 41, 41, 39))
                 lat=c(37, 41.5, 37, 41.5))
# Need to close it: 1st & last pts must be identical
df <- rbind(df, df[1,])
bbox <- st_bbox(st_sf(st_sfc(st_polygon(list(as.matrix(df)))), crs = st_crs(redwood)))

plot(st_geometry(redwood),
     xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]),
     col = sf.colors(nrow(redwood)))





## Try fasterizing, retaining ID, default fun (last); see how many dropped
# redwoods.r <- fasterize(redwood, ecoreg.r, field = "ID")
# length(unique(getValues(redwoods.r))) # 8  unique IDs
# # NA 57281 52178 55146 37805 50794 46733 42538
# # ^ 8 IDs relative to 19 observations means some got dropped (overlap)

# redwoods.r <- fasterize(redwood, ecoreg.r, field = "ID", fun = "max")
# length(unique(getValues(redwoods.r))) # 8  unique IDs
# # NA 57281 52178 55146 37805 50794 46733 42538
# # ^ Those unique IDs match the ones with last; i.e., order in which polygons drawn?
# # Prairie Creek Redwoods SP (49094) is contained by NP (57281).
# # So former gets dropped. Prove with summation approach below.

# redwoods.r <- fasterize(redwood, ecoreg.r, field = "ID", fun = "sum")
# length(unique(getValues(redwoods.r))) # 11  unique IDs
# # NA 105242  57281  52178 102179 106375  55146  37805  50794  46733  42538
# # ^ These are just dded together if overlap.
# 49094 + 57281 # 106375, which is Prairie Creek + Redwoods





#### TEMPLATE ############################################################

## Create higher res raster for PA conversion, based on ecoreg
# Load ecoregion raster template
ecoreg.r <- raster(paste0(data.dir,"ecoregions/ecoregion raster.tif"))
plot(ecoreg.r)


# Will want to retain  pixels with at least 75% PA
extent(ecoreg.r)
crs(ecoreg.r)
res(ecoreg.r) # 0.04166667. Want pixels 1/4 size of these pixels.

ref.ras <- raster(extent(ecoreg.r),
                  res = res(ecoreg.r)/2, # each side new pixel is half original
                  crs=crs(ecoreg.r),
                  vals =1)

crs <- paste0(crs(ref.ras))
rem(ecoreg.r)





#### PREP POLYS ############################################################

# Fasterize needs sf. (use st_sf() if layer is sfc_MULTIPOLYGON)
class(PA.IVI.1km) 
class(PA.IVInoass.1km) 

# Set crs to match raster template
PA.IVI.1km <- st_transform(PA.IVI.1km, crs); st_crs(PA.IVI.1km)
PA.IVInoass.1km <- st_transform(PA.IVInoass.1km, crs); st_crs(PA.IVInoass.1km)





#### RASTERIZE ############################################################

## Rasterize with fasterize. Specify ID as field to retain. 
# Background default is NA.
PA.IVI.r <- fasterize(PA.IVI.1km,
                      raster = ref.ras, field = "ID", fun = "max")
PA.IVInoass.r <- fasterize(PA.IVInoass.1km,
                           raster = ref.ras, field = "ID", fun = "max")


plot(PA.IVI.r, xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]))
plot(PA.IVInoass.r, xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]))


# res should be the only parameter different: template coarser.
extent(PA.IVI.r) ; extent(PA.IVInoass.r) ; extent(ref.ras) #; extent(ecoreg.r)
crs(PA.IVI.r) ; crs(PA.IVInoass.r); crs(ref.ras) #; crs(ecoreg.r)
res(PA.IVI.r) ; res(PA.IVInoass.r); res(ref.ras) #; res(ecoreg.r)




#### AGGREGATE ##########################################
## Aggregate & align with template raster. Keep only 75% PA pixels.
# Want to maintain IDs of PAs, so create second PA template w/ any PA = 1
# Then will aggregate by mode with IDs and clip output based on that PA template.

## Create PA=1 templates
temp.PA.IVI.r <- PA.IVI.r
temp.PA.IVInoass.r <- PA.IVInoass.r
temp.PA.IVI.r[! is.na(temp.PA.IVI.r)] <- 1
temp.PA.IVInoass.r[! is.na(temp.PA.IVInoass.r)] <- 1

## Aggregate these by factor of 2 and sum. Keep pixels with 3 or 4 (GTE 75% PA)
start <- Sys.time()
temp.PA.IVI.r.agg <- aggregate(temp.PA.IVI.r, fact = 2, fun = "sum")
temp.PA.IVInoass.r.agg <- aggregate(temp.PA.IVInoass.r, fact = 2, fun = "sum")
print(paste0("Done aggregating"))
print(Sys.time() - start)

## Retain only pixels (0.04166667x0.04166667) that have at least 75% area in PA.
# Set all values < 3 to NA (or zero for math); retain all GTE 3 as 1
temp.PA.IVI.r.agg[temp.PA.IVI.r.agg < 3] <- 0
temp.PA.IVI.r.agg[temp.PA.IVI.r.agg >= 3] <- 1
temp.PA.IVInoass.r.agg[temp.PA.IVInoass.r.agg < 3] <- 0
temp.PA.IVInoass.r.agg[temp.PA.IVInoass.r.agg >= 3] <- 1



## Aggregate ID-ed rasters by factor of 2 and keep majority PA.
# (0.02083333x0.02083333 --> 0.04166667x0.04166667).
# N.b., don't set na.rm=FALSE b/c ANY NA returns NA, even if not mode.
start <- Sys.time()
PA.IVI.r.agg <- aggregate(PA.IVI.r, fact = 2, fun = modal)
PA.IVInoass.r.agg <- aggregate(PA.IVInoass.r, fact = 2, fun = modal)
print(paste0("Done aggregating"))
print(Sys.time() - start) 

plot(PA.IVI.r.agg, xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]))
plot(PA.IVInoass.r.agg, xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]))



## Overlays rasters and retain the ID's PAs that coincide w/ an "in" pixel.
# "In" pixels are 1, not NA.
PA.IVI <- overlay(PA.IVI.r.agg, temp.PA.IVI.r.agg,
                  fun=function(r1, r1){return(r1*r2)})
# Alt: just use PA=1 raster as a mask
PA.IVI <- mask(PA.IVI.r.agg, temp.PA.IVI.r.agg)
PA.IVInoass <- mask(PA.IVInoass.r.agg, temp.PA.IVInoass.r.agg)

plot(PA.IVI, xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]))
plot(PA.IVInoass, xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]))




#### CHECK ####################################################
par(mfrow=c(1,2))
plot(PA.IVI)
plot(PA.IVInoass)
plot(PA.IVI, xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]))
plot(PA.IVInoass, xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]))

## Compare pre and post aggregation and mask
par(mfrow=c(2,3))
plot(st_geometry(GYE),
     xlim=c(gye.bbox[1], gye.bbox[3]),
     ylim=c(gye.bbox[2], gye.bbox[4]),
     col = sf.colors(nrow(GYE)))
title("orig shps (select)")
plot(temp.PA.IVI.r,
     xlim=c(gye.bbox[1], gye.bbox[3]),
     ylim=c(gye.bbox[2], gye.bbox[4]),
     col = "green")
title("pre-agg 75% PA template")
plot(temp.PA.IVI.r.agg,
     xlim=c(gye.bbox[1], gye.bbox[3]),
     ylim=c(gye.bbox[2], gye.bbox[4]),
     col = "green")
title("agg'ed 75% PA template")
plot(PA.IVI.r,
     xlim=c(gye.bbox[1], gye.bbox[3]),
     ylim=c(gye.bbox[2], gye.bbox[4]))
title("pre-agg ID'ed")
plot(PA.IVI.r.agg,
     xlim=c(gye.bbox[1], gye.bbox[3]),
     ylim=c(gye.bbox[2], gye.bbox[4]))
title("agg'ed ID'ed")
plot(PA.IVI,
     xlim=c(gye.bbox[1], gye.bbox[3]),
     ylim=c(gye.bbox[2], gye.bbox[4]))
title("agg'ed ID'ed masked to 75% PA")
par(mfrow=c(1,1))


## Check to ensure different areas have different values
plot(PA.IVI,
     xlim=c(gye.bbox[1], gye.bbox[3]),
     ylim=c(gye.bbox[2], gye.bbox[4]),
     col=rainbow(20))
# zoom(PA.IVI,
#      xlim=c(gye.bbox[1], gye.bbox[3]),
#      ylim=c(gye.bbox[2], gye.bbox[4]))
click(PA.IVI,
     xlim=c(gye.bbox[1], gye.bbox[3]),
     ylim=c(gye.bbox[2], gye.bbox[4]))




#### SAVE #############################################################3
# Write
writeRaster(PA.IVI, paste0(out.dir,"PA.IVI.r.GTE75perc_",currentDate,".tif"))
writeRaster(PA.IVInoass, paste0(out.dir,"PA.IVInoass.r.GTE75perc_",currentDate,".tif"))



