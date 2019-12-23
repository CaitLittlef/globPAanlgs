#####################
#### RASTERIZING ####
#####################


#### OVERLAPS? ######################################################
## Before fasterizing, how big of an issue will overlaps be?
# I can specify the order of rasterization.
# So can I put biggest PA on top, so that they'll eat up small inclusions?
# For example, any wilderness or wild/scenic river within an NP will be the NP.
# I could set IDs based on PA size and then select max in fasterize...


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
# Set all values < 3 to NA; retain all GTE 3 as 1
temp.PA.IVI.r.agg[temp.PA.IVI.r.agg < 3] <- NA
temp.PA.IVI.r.agg[temp.PA.IVI.r.agg >= 3] <- 1
temp.PA.IVInoass.r.agg[temp.PA.IVInoass.r.agg < 3] <- NA
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
zoom(PA.IVI.r.agg, xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]))
plot(PA.IVInoass.r.agg, xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]))



## Raster math and only retain the ID's PAs that coincide w/ an "in" pixel.
# Save for comparisons
PA.IIV.r.agg.orig <- PA.IIV.r.agg
PA.IV.r.agg.orig <- PA.IV.r.agg
PA.IVI.r.agg.orig <- PA.IVI.r.agg



## DO NOT RESAMPLE TO TEMPLATE.R ELSE COARSEN ##

## Retain only pixels that don't have template val 17 (WATER).
# Could overlay, e.g....
# park.85.2050.r <- overlay(park.MHHW.r, slr85.r, low85.r,
#                           fun=function(r1, r2, r3){return(r1*r2*r3)})
# But creating mask with clim = 17 <- NA leaves less to process.
mask <- template.r
mask[mask == 17] <- NA # 17 is water in template.
PA.IIV.r.fin <- mask(PA.IIV.r.agg, mask)
PA.IV.r.fin <- mask(PA.IV.r.agg, mask)
PA.IVI.r.fin <- mask(PA.IVI.r.agg, mask)

# Alt: Set non-zero values to 1, all non-template = 0. 
mask01 <- template.r
mask01[mask01 == 17] <- 0
mask01[! mask01 == 0] <- 1
# par(mfrow=c(1,1))
# plot(mask01)
# par(mfrow=c(1,1)) ; plot(mask01)
PA.IIV.r.overlay <- overlay(PA.IIV.r.agg, mask01,
                        fun=function(r1, r2){return(r1*r2)})
PA.IV.r.overlay <- overlay(PA.IV.r.agg, mask01,
                       fun=function(r1, r2){return(r1*r2)})
PA.IVI.r.overlay <- overlay(PA.IVI.r.agg, mask01,
                        fun=function(r1, r2){return(r1*r2)})


#########################################################
## Admire. Yellowstone - Grand Teton
par(mfrow=c(1,3))
plot(PA.IIV.r.fin, xlim=c(-9220000,-9040000), ylim=c(5350000,5590000))
plot(PA.IV.r.fin, xlim=c(-9220000,-9040000), ylim=c(5350000,5590000))
plot(PA.IVI.r.fin, xlim=c(-9220000,-9040000), ylim=c(5350000,5590000))
par(mfrow=c(1,1))

## Admire. Redwoods
par(mfrow=c(1,4))
plot(PA.IIV.r.fin, xlim=c(-10463223,-10425092), ylim=c(5130348,5214084))
title("cat I-IV")
plot(PA.IV.r.fin, xlim=c(-10463223,-10425092), ylim=c(5130348,5214084))
title("cat I-V")
plot(PA.IVI.r.fin, xlim=c(-10463223,-10425092), ylim=c(5130348,5214084))
title("cat I-VI")
plot(st_geometry(redwood), xlim=c(-10463223,-10425092), ylim=c(5130348,5214084), col = "green")
title("orig shp")
par(mfrow=c(1,1))

## Compare pre and post aggregation and mask
par(mfrow=c(2,3))
plot(st_geometry(redwoodish), xlim=c(-10463223,-10425092), ylim=c(5130348,5214084), col = "green")
title("orig shp")
plot(PA.IV.r, xlim=c(-10463223,-10425092), ylim=c(5130348,5214084))
title("pre-agg")
plot(PA.IV.r.agg.orig, xlim=c(-10463223,-10425092), ylim=c(5130348,5214084))
title("agg'ed")
plot(PA.IV.r.agg, xlim=c(-10463223,-10425092), ylim=c(5130348,5214084))
title("agg'ed; keep 75%")
plot(PA.IV.r.fin, xlim=c(-10463223,-10425092), ylim=c(5130348,5214084))
title("agg'ed; 75%; masked")
plot(PA.IV.r.overlay, xlim=c(-10463223,-10425092), ylim=c(5130348,5214084))
title("agg'ed; 75%; overlay")
par(mfrow=c(1,1))


# Write
writeRaster(PA.IIV.r.fin, paste0(out.dir,"PA.IIV.r.GTE75perc_",currentDate,".tif"))
writeRaster(PA.IV.r.fin, paste0(out.dir,"PA.IV.r.GTE75perc_",currentDate,".tif"))
writeRaster(PA.IVI.r.fin, paste0(out.dir,"PA.IVI.r.GTE75perc_",currentDate,".tif"))





