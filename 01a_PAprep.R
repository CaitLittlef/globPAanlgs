currentDate <- Sys.Date()


## Slim-down PA database, per criteria:
# 1) Cats IVI
# 2) Only pixels w/ 75% more in PA
# 3) Assign unique ID to pixels for each PA (crosswalk to PA name)
# 4) Don't exclude water, but prioritize terrestrial PAs for ID if overlap
# 5) Matches res/projection of Solomon's ecoreg template

## Also removing:
# 1) Proposed PAs
# 2) PAs < 1km to speed processing
# 3) parks that are 100% marine.
# 3) Anything not in Solomon's ecoreg template

## Ref: https://www.protectedplanet.net/c/wdpa-lookup-tables
## Ref (more details on cats I-VI): https://portals.iucn.org/library/sites/library/files/documents/PAG-021.pdf


#### LOAD ######################################################

## Load spatial data
# Global database of PAs
PA.all <- st_read(dsn = paste0(data.dir,"/WDPA_Dec2019-shapefile/WDPA_Dec2019-shapefile-polygons.shp"))
PA.slim <- PA.all %>%
  dplyr::select(NAME, DESIG_ENG, IUCN_CAT, MARINE, STATUS, REP_M_AREA, REP_AREA, GIS_M_AREA, GIS_AREA, ISO3) 
rm(PA.all)




#### CRITERIA ######################################################

## Retain PAs per criteria above.
# Dinerstein excludes proposed and status = not reported.
# But Dinerstein retains no assigned b/c lots community reserves.
# Create two versions: 1 w/ and 1 w/o not assigned


## Nix proposed and 100% marine
PA.slim <- PA.slim %>%
  filter(! STATUS == "Proposed") %>%
  filter(! MARINE == 2)


## Create 2 versions
# Make look-up
keepsIVI = c("Ia", "Ib", "II", "III", "IV", "V", "VI")
keepsIVInoass = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Assigned")

# Retain per look-ups
PA.IVI <- PA.slim[PA.slim$IUCN_CAT %in% keepsIVI, ] 
PA.IVInoass <- PA.slim[PA.slim$IUCN_CAT %in% keepsIVInoass, ] 
sum(PA.IVInoass$GIS_AREA) - sum(PA.IVI$GIS_AREA) # gives 160359 more sqkm


## Remove anything LTE 1 km2.
# Use calc'ed by IUCN b/c reported is often zero.
# temp <- data.frame(PA.IVI$GIS_AREA, PA.IVI$REP_AREA) %>% View()
# rm(temp)

PA.IVI.1km <- PA.IVI %>% filter(GIS_AREA >= 1) 
PA.IVInoass.1km <- PA.IVInoass %>% filter(GIS_AREA >= 1) 

# rm(PA.IVI, PA.IVInoass)






#### IDs ######################################################

## Assign unique ID's for names (even though some may get lost if overlap)
# Assign ID based on size so that when fasterizing, I can take max ID.
# This should take care of any small PAs (e.g., wilderness, wild/scenic rivers)...
# ...that are contained wholly within a larger PA (whose ID I want)

PA.IVI.1km <- PA.IVI.1km %>%
  arrange(GIS_AREA) %>%
  mutate(ID = row_number()) %>%
  dplyr::select(ID, everything())
PA.IVInoass.1km <- PA.IVInoass.1km %>%
  arrange(GIS_AREA) %>%
  mutate(ID = row_number()) %>%
  dplyr::select(ID, everything())

## Save
st_write(PA.IVI.1km, paste0(out.dir,"PA.IVI.1km_",currentDate,".shp"))
st_write(PA.IVInoass.1km, paste0(out.dir,"PA.IVInoass.1km_",currentDate,".shp"))


## Create look-up for IDs
lu.PA.IVI.1km <- PA.IVI.1km %>% st_drop_geometry()
lu.PA.IVInoass.1km <- PA.IVInoass.1km %>% st_drop_geometry()

write.csv(lu.PA.IVI.1km,
          paste0(out.dir,"lu.PA.IVI.1km.csv"),
          row.names = FALSE)
write.csv(lu.PA.IVInoass.1km,
          paste0(out.dir,"lu.PA.IVInoass.1km.csv"),
          row.names = FALSE) 




