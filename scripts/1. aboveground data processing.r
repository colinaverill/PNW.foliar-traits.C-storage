#This script aggregates the above ground composition and growth data and foliar chemistry data.
#it generates a basal area weighted foliar CN value. 
#This value uses plot specific foliar CN data for each species when availabile
#if plot specific foliar CN is unavailable, it uses the data set mean for a given species. 


#DATA CLEAN UP#
#First some data clean up to deal with unnecessary units row and converting character vectors to numeric. 
leaf.trait<- read.csv('NACP_TERRA-PNW_1292/data/NACP_TERRA_PNW_leaf_trait.csv')
leaf.trait<- leaf.trait[-1,] #drop line of units in data frame.
#convert characters vectors that should be numeric vectors to numeric vectors. 
cols<- c(8:11,17:27)
leaf.trait[,cols] = apply(leaf.trait[,cols], 2, function(x) as.numeric(as.character(x)))
#get rid of missing values coded as -9999, replace with NA. 
cols<- c(10:11,17:27)
leaf.trait[,cols] = apply(leaf.trait[,cols], 2, function(x) ifelse(x==-9999,NA,x))

#cleanup forest product. 
forest<- read.csv('NACP_TERRA-PNW_1292/data/NACP_TERRA_PNW_forest_biomass_productivity.csv')
forest<- forest[-1,]
cols<- c(10:14,18,20,22,24:32)
forest[,cols] = apply(forest[,cols], 2, function(x) as.numeric(as.character(x)))
forest[,cols] = apply(forest[,cols], 2, function(x) ifelse(x==-9999,NA,x))

#AGGREGATE INFORMATION#
###aggregate all leaf trait means of interest by plot and species.###
test<- aggregate(LEAF_CARBON~PLOT_ID*GENUS*SPECIES,data=leaf.trait,FUN="mean",na.action=na.pass)
test<- test[,-4]
#grab rest of the means. 
cols<- c(17:27)
test2<- apply(leaf.trait[,cols],2, function(x) aggregate(x ~ PLOT_ID*GENUS*SPECIES,data=leaf.trait,FUN="mean",na.action=na.pass)[,4])
leaf.mean <- cbind(test,test2)

#species abbreviation in 'forest' dataset is the first 3 letters of the genus and the first 3 letters of the species.
leaf.mean$genus.code<- toupper(substr(leaf.mean$GENUS,1,3))
leaf.mean$species.code<- toupper(substr(leaf.mean$SPECIES,1,3))
leaf.mean$SPP.ABBREV <- paste(leaf.mean$genus.code,leaf.mean$species.code,sep='')

#Now attach plot specific species mean chemistry to each forest site observation, for the first 4 dominant species.
all<- merge(forest,leaf.mean[,c("PLOT_ID","SPP.ABBREV","LEAF_CN")], by.x=c("PLOT_ID", "SPP_O1_ABBREV"),by.y=c("PLOT_ID","SPP.ABBREV"),all.x=T)
names(all)[names(all)=='LEAF_CN']<-"LEAF_CN.1"
all<- merge(all,leaf.mean[,c("PLOT_ID","SPP.ABBREV","LEAF_CN")], by.x=c("PLOT_ID", "SPP_O2_ABBREV"),by.y=c("PLOT_ID","SPP.ABBREV"),all.x=T)
names(all)[names(all)=='LEAF_CN']<-"LEAF_CN.2"
all<- merge(all,leaf.mean[,c("PLOT_ID","SPP.ABBREV","LEAF_CN")], by.x=c("PLOT_ID", "SPP_O3_ABBREV"),by.y=c("PLOT_ID","SPP.ABBREV"),all.x=T)
names(all)[names(all)=='LEAF_CN']<-"LEAF_CN.3"
all<- merge(all,leaf.mean[,c("PLOT_ID","SPP.ABBREV","LEAF_CN")], by.x=c("PLOT_ID", "SPP_O4_ABBREV"),by.y=c("PLOT_ID","SPP.ABBREV"),all.x=T)
names(all)[names(all)=='LEAF_CN']<-"LEAF_CN.4"

#many observations do not have site level chemistry for a species. In this case you need to get data set level chemistry for each species. 
#this is working for one aggregation. Above trick for whole dataframe not working, but whatever, move along. 
leaf.grand.mean<- aggregate(LEAF_CN~GENUS*SPECIES,data=leaf.trait,FUN="mean",na.action=na.pass)
leaf.grand.mean$genus.code<- toupper(substr(leaf.grand.mean$GENUS,1,3))
leaf.grand.mean$species.code<- toupper(substr(leaf.grand.mean$SPECIES,1,3))
leaf.grand.mean$SPP.ABBREV <- paste(leaf.grand.mean$genus.code,leaf.grand.mean$species.code,sep='')


#anytime you have a NA value for a given leaf CN, pop in the species mean for the data set (if there is one)
all$LEAF_CN.1[is.na(all$LEAF_CN.1)] <- leaf.grand.mean$LEAF_CN[match(all$SPP_O1_ABBREV[is.na(all$LEAF_CN.1)],leaf.grand.mean$SPP.ABBREV)]
all$LEAF_CN.2[is.na(all$LEAF_CN.2)] <- leaf.grand.mean$LEAF_CN[match(all$SPP_O2_ABBREV[is.na(all$LEAF_CN.2)],leaf.grand.mean$SPP.ABBREV)]
all$LEAF_CN.3[is.na(all$LEAF_CN.3)] <- leaf.grand.mean$LEAF_CN[match(all$SPP_O3_ABBREV[is.na(all$LEAF_CN.3)],leaf.grand.mean$SPP.ABBREV)]
all$LEAF_CN.4[is.na(all$LEAF_CN.4)] <- leaf.grand.mean$LEAF_CN[match(all$SPP_O4_ABBREV[is.na(all$LEAF_CN.4)],leaf.grand.mean$SPP.ABBREV)]

#remove sites that don't have basal area reported for the most dominant species. 
all<-all[!is.na(all$SPP_O1_BASAL_AREA_FRACTION),]

#Great. Now remove sites that still dont have leaf chemistry for the dominant species. 
all  <- all[!is.na(all$LEAF_CN.1),] #if the most abundant spp doesnt have chemistry, kill it.

#grab list of all sites that have reported basal area for a species, but no leaf chemistry. 
to.remove1 <- all[all$SPP_O2_BASAL_AREA_FRACTION > 0 & is.na(all$LEAF_CN.2),]
to.remove2 <- all[all$SPP_O3_BASAL_AREA_FRACTION > 0 & is.na(all$LEAF_CN.3),]
to.remove3 <- all[all$SPP_O4_BASAL_AREA_FRACTION > 0 & is.na(all$LEAF_CN.4),]
to.remove  <- rbind(to.remove1,to.remove2,to.remove3)

#remove these sites from the 'all' dataframe. 172 sites with complete basal area and foliar chemistry remain.
'%!in%' <- function(x,y)!('%in%'(x,y))
all<- all[all$PLOT_ID %!in% to.remove$PLOT_ID,]

#generate basal area weighted foliar CN. 
#this is the expression, but its reporting a bunch of NAs. NAs in irrelevant basal areas are causing problems.
#change foliar CN values recorded as NA to 0
all$LEAF_CN.2[is.na(all$LEAF_CN.2)] <- 0
all$LEAF_CN.3[is.na(all$LEAF_CN.3)] <- 0
all$LEAF_CN.4[is.na(all$LEAF_CN.4)] <- 0
all$basal.weighted.CN<- (all$SPP_O1_BASAL_AREA_FRACTION * all$LEAF_CN.1 + all$SPP_O2_BASAL_AREA_FRACTION * all$LEAF_CN.2 + all$SPP_O3_BASAL_AREA_FRACTION * all$LEAF_CN.3 + all$SPP_O4_BASAL_AREA_FRACTION * all$LEAF_CN.4) / (all$SPP_O1_BASAL_AREA_FRACTION + all$SPP_O2_BASAL_AREA_FRACTION + all$SPP_O3_BASAL_AREA_FRACTION + all$SPP_O4_BASAL_AREA_FRACTION)

output <- all[,c("PLOT_ID","LATITUDE","LONGITUDE","MAT_C","MAP","ELEVATION","AG_PROD_TREE_TOTAL_AS_CARBON","basal.weighted.CN")]
write.csv(output,"data.outputs/aboveground.csv")
