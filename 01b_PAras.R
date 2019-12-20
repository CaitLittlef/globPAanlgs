#### OVERLAPS? ######################################################
## Before fasterizing, how big of an issue will overlaps be?
# Want terrestrial PA ID to trump, for example, wild & scenic rivers.
# How extensive is overlap problem?

## Look at Redwoods zone (exclude spot in MN)
redwood <- PA.IVInoass.1km %>%
  filter(., grepl("Redwood", NAME)) %>%
  filter(!NAME == "WRP_Redwood, MN (27127)") # 20 obs
bbox <- st_bbox(redwood)
# -124.20333   37.00931 -122.03447   41.84049
# Make smaller bbox
df <- data.frame(lon=c(-124, -124, -123, -123),
                 # lat=c(39, 41, 41, 39))
                 lat=c(40, 40.5, 40.5, 40))
# Need to close it: 1st & last pts must be identical
df <- rbind(df, df[1,])
bbox <- st_bbox(st_sf(st_sfc(st_polygon(list(as.matrix(df)))), crs = st_crs(redwood)))

plot(st_geometry(redwood),
     xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]),
     col = sf.colors(nrow(redwood)))



## I can specify the order of rasterization.
# So can I put biggest PA on top, so that they'll eat up small inclusions.
# For example, any wilderness or wild/scenic river within an NP will be the NP. 

## Try fasterizing, retaining ID, default fun (last); see how many dropped
redwoods.r <- fasterize(redwood, ecoreg.r, field = "ID")
length(unique(getValues(redwoods.r))) # 12  unique IDs, which means some dropped.
# NA, 42670 40691 45847 41172 40952 40951 42617 51015 44402 51719 44411

redwoods.r <- fasterize(redwood, ecoreg.r, field = "ID", fun = "sum")
length(unique(getValues(redwoods.r))) # 13  unique IDs, which means some dropped.
# NA, 83361 40691 45847 41172 81863 81643 81642 42617 51015 44402 51719 44411
# 83361, 81863, 81643, 81642 new; 42670, 40952, 40951 dropped
# ^ Those new ones are just added together IDs. So sum won't work.



################################################################
## Load data
PA.IIV <- st_read(dsn = paste0(data.dir,"WDPA_Apr2019-shapefile/PA.IIV.terr.1km.shp"))
PA.IV <- st_read(dsn = paste0(data.dir,"WDPA_Apr2019-shapefile/PA.IV.terr.1km.shp"))
PA.IVI <- st_read(dsn = paste0(data.dir,"WDPA_Apr2019-shapefile/PA.IVI.terr.1km.shp"))


## Load raster template 
clim.r <- raster("def.1961.1990.climo.tif")
template.r <- raster(paste0(data.dir,"landcover.2007.4km.ea.tif"))
# extent(clim.r)
# crs(clim.r)
# res(clim.r)
extent(template.r)
crs(template.r)
res(template.r)

## Create higher res raster for PA conversion.
# (Will want to retain 4km pixels with at least 75% PA)
ref.ras <- raster(xmn = -16940774, xmx = 16939226 ,
                  ymn = -8479671, ymx = 8396329,
                  resolution = 2000, # 1/4 size of template raster
                  crs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", 
                  vals = 1)

crs <- paste0(crs(ref.ras))

################################################################
## Prep polygons
# Fasterize needs sf. (use st_sf() if layer is sfc_MULTIPOLYGON)
class(PA.IIV)
class(PA.IV) 
class(PA.IVI) 
# Set crs to match raster template
st_crs(PA.IIV)
st_crs(PA.IV) 
st_crs(PA.IVI) 
PA.IIV.t <- st_transform(PA.IIV, crs) ; st_crs(PA.IIV)
PA.IV.t <- st_transform(PA.IV, crs) ; st_crs(PA.IV) 
PA.IVI.t <- st_transform(PA.IVI, crs) ; st_crs(PA.IVI) 


############################################################
## Rasterize with fasterize. Field default is setting all polys to 1.
# Define function else seems only 1st of overlap (even if not complete) gets kept.
# "any" keeps any spot with a polygon; "sum" adds.
# Background default is NA.
PA.IIV.r <- fasterize(PA.IIV.t, raster = ref.ras, fun = "any")
PA.IV.r <- fasterize(PA.IV.t, raster = ref.ras, fun = "any")
PA.IVI.r <- fasterize(PA.IVI.t, raster = ref.ras, fun = "any")

par(mfrow=c(1,3)) # Look around Yellowstone-Grand Teton
plot(PA.IIV.r, xlim=c(-9220000,-9040000), ylim=c(5350000,5590000))
plot(PA.IV.r, xlim=c(-9220000,-9040000), ylim=c(5350000,5590000))
plot(PA.IVI.r, xlim=c(-9220000,-9040000), ylim=c(5350000,5590000))
par(mfrow=c(1,1))

# res should be the only parameter different: template coarser.
extent(PA.IIV.r) ; extent(PA.IV.r) ; extent(PA.IVI.r) ; extent(template.r)
crs(PA.IIV.r) ; crs(PA.IV.r); crs(PA.IVI.r) ; crs(template.r)
res(PA.IIV.r) ; res(PA.IV.r); res(PA.IVI.r); res(template.r)


#####################################################
## Aggregate & align with climate raster. Keep only 75% PA pixels
# Make sure all PAs are only 1
PA.IIV.r[! is.na(PA.IIV.r)] <- 1
PA.IV.r[! is.na(PA.IV.r)] <- 1
PA.IVI.r[! is.na(PA.IVI.r)] <- 1

## Aggregate by factor of 2 (2000x2000 --> 4000x4000) and take sum.
# Sum is proxy for how much of clim pixel has PA. Want at least 75%.
# All PA areas have val of 1, so sum for keeping should be 3 or 4.
start <- Sys.time()
PA.IIV.r.agg <- aggregate(PA.IIV.r, fact = 2, fun = "sum")
PA.IV.r.agg <- aggregate(PA.IV.r, fact = 2, fun = "sum")
PA.IVI.r.agg <- aggregate(PA.IVI.r, fact = 2, fun = "sum")
print(paste0("Done aggregating"))
print(Sys.time() - start) 

# Save for comparisons
PA.IIV.r.agg.orig <- PA.IIV.r.agg
PA.IV.r.agg.orig <- PA.IV.r.agg
PA.IVI.r.agg.orig <- PA.IVI.r.agg

## Retain only pixels (4000x4000) that have at least 75% area in PA.
# Set all values < 3 to NA; retain all GTE 3 as 1
PA.IIV.r.agg[PA.IIV.r.agg < 3] <- NA
PA.IIV.r.agg[PA.IIV.r.agg >= 3] <- 1
PA.IV.r.agg[PA.IV.r.agg < 3] <- NA
PA.IV.r.agg[PA.IV.r.agg >= 3] <- 1
PA.IVI.r.agg[PA.IVI.r.agg < 3] <- NA
PA.IVI.r.agg[PA.IVI.r.agg >= 3] <- 1

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





