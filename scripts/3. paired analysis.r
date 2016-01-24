#analyzing paired foliar chemistry and soil attributes. 
d<- read.csv('data.outputs/all_data_paired.csv')
soil.cn <- d$C.storage / d$ N.storage
d[,1:17]<- as.numeric(d[,1:17],na.action=na.pass)
colnames(d)[(names(d) == "AG_PROD_TREE_TOTAL_AS_CARBON")] <- "productivity"

require(car)

#are carbon storage and input CN ratio related? NOPE. 
m1<- lm(log(C.storage)~basal.weighted.CN * productivity + MAT_C + MAP + clay,data=d)
plot(residuals(m1)~fitted(m1))
qqnorm(residuals(m1))
summary(m1)
vif(m1)

plot(basal.weighted.CN ~ pH,data=d)


summary(lm(soil.cn~basal.weighted.CN,data=d))$r.squared

#Is the CN of carbon storage and the CN of inputs positively correlated? YES.
#can't include MAP and pH in same model. 80% correlated, R2=0.81
m1<- lm(log(soil.cn ~ + basal.weighted.CN + MAT_C + pH,data=d)
plot(residuals(m1)~fitted(m1))
qqnorm(residuals(m1))
summary(m1)
vif(m1)

#conclusion: I still need to add a AM / EM predictor into this and see if there is enough mileage in the spread to separate input C:N and mycorrhizal type
#however, based on this analysis, high C:N inputs is correlated with high soil C:N. 

#make a plot of soil C storage vs. input C:N
png(filename='figures/input_CN_vs_storage.png',width=5,height=5,units='in',res=300)

plot(log(C.storage)~basal.weighted.CN,data=d,pch=16,cex=0.7,ylab='soil C storage / m2',xlab='C:N of inputs')
abline(lm(log(C.storage)~basal.weighted.CN,data=d),lty=2,lwd=2)
mtext('not signficant bro (n=102)')

dev.off()

#CN of inputs vs. CN of storage. 
plot(soil.cn~basal.weighted.CN,data=d,pch=16,cex = 0.7)
abline(lm(soil.cn~basal.weighted.CN,data=d),lwd=2,lty=2)
plot(C.storage ~ )