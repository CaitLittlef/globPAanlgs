


## Make sure no non-terrestrial are still in there.
# Look at designations and spot check at https://protectedplanet.net.
# levels(PA.IIV.terr.1km$DESIG_ENG)
# levels(PA.IV.terr.1km$DESIG_ENG)
# levels(PA.IVI.terr.1km$DESIG_ENG)

# Already did so with PA.IIV.terr.1km. Create look-up for dropping.
# This is what I HAD done:
# PA.IIV.terr.1km %>%
#   filter(DESIG_ENG == levels(DESIG_ENG)[492])
# Add to-be-dropped levels to drop list
drop.levels <- c(10, 11, 12, 23, 44, 49, 99, 123, 140, 184, 185, 186, 187, 188, 189, 190,
                 191, 192, 193, 194, 195, 216, 340, 341, 342, 343, 344, 363, 447, 448, 477,
                 478, 488, 491, 492, 493, 494)
# # drop.levels are numbers so read factor (DESIG_ENG) as such
# PA.IIV.terr.1km <- PA.IIV.terr.1km %>%
#   filter(! as.integer(PA.IIV.terr.1km$DESIG_ENG) %in% drop.levels) # 36425

# Create look-up table for designations to be dropped (with fuller list (PA.IVI.terr.1km))
IIV.levels <- as.data.frame(cbind(row_number(levels(PA.IIV.terr.1km$DESIG_ENG)),
                                  levels(PA.IIV.terr.1km$DESIG_ENG)))
IV.levels <- as.data.frame(cbind(row_number(levels(PA.IV.terr.1km$DESIG_ENG)),
                                 levels(PA.IV.terr.1km$DESIG_ENG)))
IVI.levels <- as.data.frame(cbind(row_number(levels(PA.IVI.terr.1km$DESIG_ENG)),
                                  levels(PA.IVI.terr.1km$DESIG_ENG)))
# Combine all into one table: has desig names and corresponding level nums.
desigs <- list(IIV.levels, IV.levels, IVI.levels) %>%
  reduce(full_join, by = "V2") %>%
  dplyr::select(V2, V1.x, V1.y, V1)

desigs <- desigs %>% modify_at(c(2, 3, 4), as.character) %>%
  modify_at(c(2,3,4), as.integer)
colnames(desigs) <- c("names", "IIV.nums", "IV.nums", "IVI.nums")
# write.csv(desigs, "desigs.to.drop.csv")

# ^ Checked this and loading complete one in 01a.


# [10] "Aquatic Preserve"                                                                                   
# [11] "Aquatic reserve"                                                                                    
# [12] "Aquatic Reserve" 
# [23] "Beach"
# [44] "Central Mangrove Wetland" 
# [49] "Coastal Preserve"                                                                                   
# [50] "Coastal Reserve"                                                                                    
# [51] "Coasts and Areas for Nature Protection"  
# [99] "Fish Sanctuary"  
# [123] "Grey Seal Protection Area"   
# [132] "Hemispheric Shorebird Reserve"
# [140] "Heritage River"   
# [184] "Marine and Coastal Protected Area"                                                                  
# [185] "Marine Biological Preserve"                                                                         
# [186] "Marine Life Conservation District"                                                                  
# [187] "Marine Mammal Sanctuary"                                                                            
# [188] "Marine National Park"                                                                               
# [189] "Marine Nature Reserve"                                                                              
# [190] "Marine Park"                                                                                        
# [191] "Marine Park / Marine Reserve / Wildlife Reserve"                                                    
# [192] "Marine Preserve"                                                                                    
# [193] "Marine Reserve"                                                                                     
# [194] "Marine Reserve / Marine Conservation Area / Wildlife Reserve"                                       
# [195] "Marine Sanctuary"  
# [216] "National Estuarine Research Reserve"  
# [249] "National River"  # large enough poly around river -- keeping
# [340] "Proposed aquatic reserve"                                                                           
# [341] "Proposed biodiversity reserve"                                                                      
# [342] "Proposed ecological reserve"                                                                        
# [343] "Proposed National Parks Act park or park addition"                                                  
# [344] "Proposed Wilderness"                             
# [363] "Protected Water Surface"    
# [439] "State Beach" 
# [447] "State Marine Conservation Area"                                                                     
# [448] "State Marine Reserve"     
# [477] "Turtle Nesting Beach"
# [478] "Underwater Park"   
# [487] "Water and Forest Reserve"                                                                           
# [488] "Water fowl gathering area"     
# [491] "Wetland of International Importance"                                                                
# [492] "Wetland Protected Area"                                                                             
# [493] "Wetland Protected Area - Tidal Flat"                                                                
# [494] "Wetland Reserve"






