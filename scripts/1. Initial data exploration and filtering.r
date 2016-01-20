#initial script: what is in here and how will we aggregate it. 
soil<- read.csv('NACP_TERRA-PNW_1292/data/NACP_TERRA_PNW_soil.csv')


#step 1: aggregate leaf traits by species and site

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

#now need to get bassal area weighted or growth weighted leaf chemistry for the plot
#there are 266 unique plot IDs, only 206 unique plot IDs in the leaf.mean. 
#move forward with the 206 that have data in leaf.mean
#you have a dummy stack overflow example that has this working. Need to figure out why its not extending. 
forest$leaf.C = f

forest.test <-    apply(forest, 1, function(x) 
                    leaf.mean[leaf.mean$PLOT_ID==forest['PLOT_ID']&leaf.mean$SPP.ABBREV==x['SPP_O1_ABBREV'],'weight']*as.numeric(x['SPP_O1_BASAL_AREA_FRACTION']))# + 
                    #d2[d2$site==x[1]&d2$species==x[4],'weight']*as.numeric(x[5]))
