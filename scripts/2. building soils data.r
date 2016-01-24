#compiling soil profile data to determine soil C and N storage, texture and pH.

soil<- read.csv('NACP_TERRA-PNW_1292/data/NACP_TERRA_PNW_soil.csv')
#DATA CLEAN UP#
#First some data clean up to deal with unnecessary units row and converting character vectors to numeric. 
soil<- soil[-1,] #drop line of units in data frame.
#convert characters vectors that should be numeric vectors to numeric vectors. 
cols<- c(15,17:25)
soil[,cols] = apply(soil[,cols], 2, function(x) as.numeric(as.character(x)))
#get rid of missing values coded as -9999, replace with NA. 
soil[,cols] = apply(soil[,cols], 2, function(x) ifelse(x==-9999,NA,x))

#only inclduing sites with data toa tleast 20cm depth. This includes 175 of 211 profiles. 

#so, toss out anything where top of a soil layer is 20cm or above (this excludes 0 sites). 
soil<-soil[soil$UPPER_DEPTH_OF_SOIL_LAYER < 20,]
#remove any plots that are not at least 20cm deep- this is 36 sites.
to.remove <- aggregate(LOWER_DEPTH_OF_SOIL_LAYER~PLOT_ID, data=soil, FUN='max')
to.remove <- subset(to.remove,to.remove$LOWER_DEPTH_OF_SOIL_LAYER<20)
soil<- soil[!(soil$PLOT_ID %in% to.remove$PLOT_ID),]

#calculate percent C and N from bulk density and depth.
#BD= kg / m2 = (g soil / cm3) * (depth in cm) * (10,000 cm2 / m2)
#C =  g / m2 = frac.C * (g soil / cm3) * (depth in cm) * (10,000 cm2 / m2)
#also note typo for carbon. 'LAYEL' instead of 'LAYER'. 
soil$f.C <- (soil$CARBON_CONTENT_OF_SOIL_LAYEL  ) / (soil$BULK_DENSITY_OF_SOIL_LAYER*1000)
soil$f.N <- (soil$NITROGEN_CONTENT_OF_SOIL_LAYER) / (soil$BULK_DENSITY_OF_SOIL_LAYER*1000)

#calculate layer depth
soil$depth<- (soil$LOWER_DEPTH_OF_SOIL_LAYER - soil$UPPER_DEPTH_OF_SOIL_LAYER)
#recalculate bulk density as a volumetric quantity: g soil / cm3
# kg soil / m2 = g soil / cm3 * 1/(1000 g / kg) * depth (cm) * (10,000 cm2/ m2)
soil$bd.vol <- soil$BULK_DENSITY_OF_SOIL_LAYER / (soil$depth*10)

#reset max soil depth to 20cm for lowest soil layer- we calculating storage to 20cm
soil$LOWER_DEPTH_OF_SOIL_LAYER <- ifelse(soil$LOWER_DEPTH_OF_SOIL_LAYER>20, 20, soil$LOWER_DEPTH_OF_SOIL_LAYER)

#recalculate soil$depth with the new 20cm max depth condition. 
soil$depth<- (soil$LOWER_DEPTH_OF_SOIL_LAYER - soil$UPPER_DEPTH_OF_SOIL_LAYER)


#now calculate soil C and N  storage by layer- g C or N / m2, as f.C * bulk (g / cm3) * depth (cm) * (10,000cm / m2)
soil$layer.C <- soil$bd.vol * soil$f.C * soil$depth * 10000
soil$layer.N <- soil$bd.vol * soil$f.N * soil$depth * 10000

#get bulk density and profile weighted texture and pH, everything matched to the correct plot ID. 
#calculate mass of soil in a given layer, and then mass of soil in the total profile
#this enables depth/bulk density weighting of pH and texture. 
soil$layer.mass.cm2   <- soil$bd * soil$depth
ag <- aggregate(layer.mass.cm2 ~ PLOT_ID,data=soil,FUN='sum')
soil$total.mass.cm2 <- merge(soil,ag,all.x=T,by='PLOT_ID')[,"layer.mass.cm2.y"]


#calculate total profile soil mass weighted values of pH and texture by layer. 
soil$pH.layer   <- soil$PH_OF_SOIL_LAYER                      * soil$layer.mass.cm2 * (1/soil$total.mass.cm2)
soil$sand.layer <- soil$VOLUME_FRACTION_OF_SAND_IN_SOIL_LAYER * soil$layer.mass.cm2 * (1/soil$total.mass.cm2)
soil$silt.layer <- soil$VOLUME_FRACTION_OF_SILT_IN_SOIL_LAYER * soil$layer.mass.cm2 * (1/soil$total.mass.cm2)
soil$clay.layer <- soil$VOLUME_FRACTION_OF_CLAY_IN_SOIL_LAYER * soil$layer.mass.cm2 * (1/soil$total.mass.cm2)


#aggregate across layers to get profile totals
total<- aggregate(layer.C~PLOT_ID,data=soil,FUN='sum', na.action=na.pass)
colnames(total)[2] <- "C.storage"
total$N.storage <- aggregate(layer.N    ~PLOT_ID,data=soil,FUN='sum', na.action=na.pass)[,2]
total$pH        <- aggregate(pH.layer   ~PLOT_ID,data=soil,FUN='sum', na.action=na.pass)[,2]
total$sand      <- aggregate(sand.layer ~PLOT_ID,data=soil,FUN='sum', na.action=na.pass)[,2]
total$silt      <- aggregate(silt.layer ~PLOT_ID,data=soil,FUN='sum', na.action=na.pass)[,2]
total$clay      <- aggregate(clay.layer ~PLOT_ID,data=soil,FUN='sum', na.action=na.pass)[,2]


#save soil output
write.csv(total,'data.outputs/belowground.csv')

#pair this soil data with the foliar data and save.
foliar<- read.csv('data.outputs/aboveground.csv')
paired<- merge(foliar,total,by="PLOT_ID",all.x=T)
write.csv(paired,'data.outputs/all_data_paired.csv')


