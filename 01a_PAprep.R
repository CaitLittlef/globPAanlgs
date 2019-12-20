currentDate <- Sys.Date()
wd <- setwd("D:/Shared/BackedUp/Caitlin/GlobalCnxns") 

## Slim-down PA database, per criteria:
# 1) Cats I-IV, I-V, IVI
# 2) Only terrestrial
# 3) Only pixels w/ 75% more in PA (requires rasterizing)
# 4) Matches res/projection of Sean's template

# Also removing proposed PAs, those <1km, anything with clim = 0.

##########################################################
## Load spatial data
# Global database of PAs
PA.all <- st_read(dsn = paste0(data.dir,"/WDPA_Apr2019-shapefile/WDPA_Apr2019-shapefile-polygons.shp"))
PA.slim <- PA.all %>%
  dplyr::select(NAME, DESIG_ENG, IUCN_CAT, MARINE, STATUS, REP_M_AREA, REP_AREA, GIS_M_AREA, GIS_AREA) 


##########################################################
## Keep different cats
keepsIIV = c("Ia", "Ib", "II", "III", "IV")
keepsIV = c("Ia", "Ib", "II", "III", "IV", "V")
keepsIVI = c("Ia", "Ib", "II", "III", "IV", "V", "VI")
PA.IIV <- PA.slim[PA.slim$IUCN_CAT %in% keepsIIV, ]
PA.IV <- PA.slim[PA.slim$IUCN_CAT %in% keepsIV, ]
PA.IVI <- PA.slim[PA.slim$IUCN_CAT %in% keepsIVI, ]

## Remove 100% marine and any proposed
PA.IIV.terr <- PA.IIV %>%
  filter(! MARINE == 2) %>%
  filter(! STATUS == "Proposed")
PA.IV.terr <- PA.IV %>%
  filter(! MARINE == 2) %>%
  filter(! STATUS == "Proposed")
PA.IVI.terr <- PA.IVI %>%
  filter(! MARINE == 2) %>%
  filter(! STATUS == "Proposed")


## Remove anything LTE 1 km2 (reported; not calc'ed by UNEP)
PA.IIV.terr.1km <- PA.IIV.terr %>%
  filter(REP_AREA >= 1) # 37180 remain
PA.IV.terr.1km <- PA.IV.terr %>%
  filter(REP_AREA >= 1) # 54615 remain
PA.IVI.terr.1km <- PA.IVI.terr %>%
  filter(REP_AREA >= 1) # 58146 remain


## Drop unused levels
PA.IIV.terr.1km$MARINE <- droplevels(PA.IIV.terr.1km$MARINE)
PA.IIV.terr.1km$STATUS <- droplevels(PA.IIV.terr.1km$STATUS)
PA.IIV.terr.1km$DESIG_ENG <- droplevels(PA.IIV.terr.1km$DESIG_ENG)
PA.IIV.terr.1km$NAME <- droplevels(PA.IIV.terr.1km$NAME)

PA.IV.terr.1km$MARINE <- droplevels(PA.IV.terr.1km$MARINE)
PA.IV.terr.1km$STATUS <- droplevels(PA.IV.terr.1km$STATUS)
PA.IV.terr.1km$DESIG_ENG <- droplevels(PA.IV.terr.1km$DESIG_ENG)
PA.IV.terr.1km$NAME <- droplevels(PA.IV.terr.1km$NAME)

PA.IVI.terr.1km$MARINE <- droplevels(PA.IVI.terr.1km$MARINE)
PA.IVI.terr.1km$STATUS <- droplevels(PA.IVI.terr.1km$STATUS)
PA.IVI.terr.1km$DESIG_ENG <- droplevels(PA.IVI.terr.1km$DESIG_ENG)
PA.IVI.terr.1km$NAME <- droplevels(PA.IVI.terr.1km$NAME)


## Retain only terrestrial based on desig and spot check at https://protectedplanet.net.
# Create look-up table for designations to be dropped (with fuller list (PA.IVI.terr.1km))
# IIV.levels <- as.data.frame(cbind(row_number(levels(PA.IIV.terr.1km$DESIG_ENG)),
#                                   levels(PA.IIV.terr.1km$DESIG_ENG)))
# IV.levels <- as.data.frame(cbind(row_number(levels(PA.IV.terr.1km$DESIG_ENG)),
#                                  levels(PA.IV.terr.1km$DESIG_ENG)))
# IVI.levels <- as.data.frame(cbind(row_number(levels(PA.IVI.terr.1km$DESIG_ENG)),
#                                   levels(PA.IVI.terr.1km$DESIG_ENG)))
# # Combine all into one table: has desig names and corresponding level nums.
# desigs <- list(IIV.levels, IV.levels, IVI.levels) %>%
#   reduce(full_join, by = "V2") %>%
#   dplyr::select(V2, V1.x, V1.y, V1)
# 
# desigs <- desigs %>% modify_at(c(2, 3, 4), as.character) %>%
#   modify_at(c(2,3,4), as.integer)
# colnames(desigs) <- c("names", "IIV.nums", "IV.nums", "IVI.nums")
# # write.csv(desigs, "desigs.to.drop.csv")

# Then I tested with...
# PA.IIV.terr.1km %>%
#   filter(DESIG_ENG == levels(DESIG_ENG)[492])
# ... and filled-in csv.

## Load look-up table (created above) for removing unwanted designations.
# N.b., each layer (I-IV, I-V, I-VI) has different factor level asso with desigs.
desigs <- read.csv("desigs.to.drop_COMPLETE.csv")
desigs$drop <- as.character(desigs$drop)
desigs$drop[desigs$drop == ""] <- "N"
desigs$drop[desigs$drop == "x"] <- "Y"
desigs %>% count(drop)

# Create mini filter for each, with asso levels of desigs to be dropped
drops.IIV <- desigs %>% filter(drop == "Y") %>% dplyr::select(IIV.nums)
drops.IV <- desigs %>% filter(drop == "Y") %>% dplyr::select(IV.nums)
drops.IVI <- desigs %>% filter(drop == "Y") %>% dplyr::select(IVI.nums)

# Filter based on desigs to be dropped
PA.IIV.terr.1km <- PA.IIV.terr.1km %>%
  filter(! as.integer(PA.IIV.terr.1km$DESIG_ENG) %in% drops.IIV$IIV.nums) #36480
PA.IV.terr.1km <- PA.IV.terr.1km %>%
  filter(! as.integer(PA.IV.terr.1km$DESIG_ENG) %in% drops.IV$IV.nums) #53755
PA.IVI.terr.1km <- PA.IVI.terr.1km %>%
  filter(! as.integer(PA.IVI.terr.1km$DESIG_ENG) %in% drops.IVI$IVI.nums) #57167


## Save JIC
st_write(PA.IIV.terr.1km, "WDPA_Apr2019-shapefile/PA.IIV.terr.1km.shp")
st_write(PA.IV.terr.1km, "WDPA_Apr2019-shapefile/PA.IV.terr.1km.shp")
st_write(PA.IVI.terr.1km, "WDPA_Apr2019-shapefile/PA.IVI.terr.1km.shp")

