currentDate <- Sys.Date()
wd <- setwd("D:/Shared/BackedUp/Caitlin/GlobalCnxns") 

## Slim-down PA database, per criteria:
# 1) Cats IVI
# 2) Only pixels w/ 75% more in PA
# 3) Assign unique ID to pixels for each PA (crosswalk to PA name)
# 4) Don't exclude water, but prioritize terrestrial PAs for ID if overlap
# 5) Matches res/projection of Solomon's ecoreg template

## Also removing:
# 1) Proposed PAs
# 2) PAs < 1km to speed processing
# 3) Anything not in Solomon's ecoreg template (will nix marine)







#### LOAD ######################################################

## Load spatial data
# Global database of PAs
PA.all <- st_read(dsn = paste0(data.dir,"/WDPA_Dec2019-shapefile/WDPA_Dec2019-shapefile-polygons.shp"))
PA.slim <- PA.all %>%
  dplyr::select(NAME, DESIG_ENG, IUCN_CAT, MARINE, STATUS, REP_M_AREA, REP_AREA, GIS_M_AREA, GIS_AREA, ISO3) 
rm(PA.all)

# ecoregion template
ecoreg.r <- raster(paste0(data.dir,"ecoregions/ecoregion raster.tif"))
plot(ecoreg.r)







#### CRITERIA ######################################################

## Retain PAs per criteria above.
# Dinerstein excludes proposed and status = not reported.
# But Dinerstein retains no assigned b/c lots community reserves.
# Create two versions: 1 w/ and 1 w/o not assigned



## Keep proposed
PA.slim <- PA.slim %>%
  filter(! STATUS == "Proposed")



## Create 2 versions
# Make look-up
keepsIVI = c("Ia", "Ib", "II", "III", "IV", "V", "VI")
keepsIVInoass = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Assigned")

# Retain per look-ups
PA.IVI <- PA.slim[PA.slim$IUCN_CAT %in% keepsIVI, ] #154291
sum(PA.IVI$GIS_AREA) #35354003
PA.IVInoass <- PA.slim[PA.slim$IUCN_CAT %in% keepsIVInoass, ] #162131
sum(PA.IVInoass$GIS_AREA) #36129306
sum(PA.IVInoass$GIS_AREA) - sum(PA.IVI$GIS_AREA) # gives 775302 more sqkm



## Remove anything LTE 1 km2.
# Use calc'ed by IUCN b/c reported often zero.
temp <- data.frame(PA.IVI$GIS_AREA, PA.IVI$REP_AREA) %>% View()
rm(temp)

PA.IVI.1km <- PA.IVI %>% filter(GIS_AREA >= 1) #61197
PA.IVInoass.1km <- PA.IVInoass %>% filter(GIS_AREA >= 1) #62686

rm(PA.IVI, PA.IVInoass)






#### IDs ######################################################

## Assign unique ID's for names (even though some may get lost if overlap)

## Drop unused levels
PA.IVI.1km$STATUS <- droplevels(PA.IVI.1km$STATUS)
PA.IVI.1km$DESIG_ENG <- droplevels(PA.IVI.1km$DESIG_ENG)
PA.IVI.1km$NAME <- droplevels(PA.IVI.1km$NAME)

PA.IVInoass.1km$STATUS <- droplevels(PA.IVInoass.1km$STATUS)
PA.IVInoass.1km$DESIG_ENG <- droplevels(PA.IVInoass.1km$DESIG_ENG)
PA.IVInoass.1km$NAME <- droplevels(PA.IVInoass.1km$NAME)



## Create ID variable & look-up tables.
# Use row number instead of PA name b/c some have same name
PA.IVI.1km <- PA.IVI.1km %>%
  mutate(ID = row_number()) %>%
  dplyr::select(ID, everything())
PA.IVInoass.1km <- PA.IVInoass.1km %>%
  mutate(ID = row_number()) %>%
  dplyr::select(ID, everything())

lu.PA.IVI.1km <- data.frame(PA.IVI.1km$ID, PA.IVI.1km$NAME)
lu.PA.IVInoass.1km <- data.frame(PA.IVInoass.1km$ID, PA.IVInoass.1km$NAME)

write.csv(lu.PA.IVI.1km,
          paste0(out.dir,"lu.PA.IVI.1km.csv"),
          row.names = FALSE)
write.csv(lu.PA.IVInoass.1km,
          paste0(out.dir,"lu.PA.IVInoass.1km.csv"),
          row.names = FALSE)




