dat = read.csv("StormData.csv")
names(dat)
#preprocess PROPDMG
dat$PropExp[dat$PROPDMGEXP=="" | dat$PROPDMGEXP=="?" |dat$PROPDMGEXP=="-"|dat$PROPDMGEXP=="+"|dat$PROPDMGEXP=="0" ]=10^0
dat$PropExp[dat$PROPDMGEXP=="1" ]=10^1
dat$PropExp[dat$PROPDMGEXP=="2" |dat$PROPDMGEXP=="h"|dat$PROPDMGEXP=="H"]=10^2
dat$PropExp[dat$PROPDMGEXP=="3"|dat$PROPDMGEXP=="K" ]=10^3
dat$PropExp[dat$PROPDMGEXP=="4" ]=10^4
dat$PropExp[dat$PROPDMGEXP=="5" ]=10^5
dat$PropExp[dat$PROPDMGEXP=="6"|dat$PROPDMGEXP=="m"|dat$PROPDMGEXP=="M" ]=10^6
dat$PropExp[dat$PROPDMGEXP=="7" ]=10^7
dat$PropExp[dat$PROPDMGEXP=="8" ]=10^8
dat$PropExp[dat$PROPDMGEXP=="B" ]=10^9

#preprocess CROPDMGEXP
dat$CropExp[dat$CROPDMGEXP==""|dat$CROPDMGEXP=="?"|dat$CROPDMGEXP=="0" ]=10^0
dat$CropExp[dat$CROPDMGEXP=="2" ]=10^2
dat$CropExp[dat$CROPDMGEXP=="k"|dat$CROPDMGEXP=="K" ]=10^3
dat$CropExp[dat$CROPDMGEXP=="m"|dat$CROPDMGEXP=="M" ]=10^6
dat$CropExp[dat$CROPDMGEXP=="B" ]=10^9


dat$PropDmgVal = dat$PROPDMG * dat$PropExp
dat$CropDmgVal = dat$CROPDMG * dat$CropExp


# Polulation Health
fatRes = aggregate(dat$FATALITIES, by=list(dat$EVTYPE),sum,na.rm=T)
names(fatRes) = c("Event Type", "Number")
fatRes = fatRes[order(fatRes$Number,decreasing=T),]

injRes = aggregate(dat$INJURIES, by=list(dat$EVTYPE),sum,na.rm=T)
names(injRes) = c("Event Type", "Number")
injRes = injRes[order(injRes$Number,decreasing=T),]

par(mfrow=c(2,1))
barplot(fatRes$Number[1:5],sub="Fatalities",names.arg=fatRes$"Event Type"[1:5])
barplot(injRes$Number[1:5],sub="Injuries",names.arg=injRes$"Event Type"[1:5])

# Economic Consequence
propRes = aggregate(dat$PropDmgVal, by=list(dat$EVTYPE),sum,na.rm=T)
names(propRes) = c("Event Type", "Value")
propRes = propRes[order(propRes$Value,decreasing=T),]

cropRes = aggregate(dat$CropDmgVal, by=list(dat$EVTYPE),sum,na.rm=T)
names(cropRes) = c("Event Type", "Value")
cropRes = cropRes[order(cropRes$Value,decreasing=T),]

par(mfrow=c(2,1))
barplot(propRes$Value[1:5],sub="Property Damage",names.arg=propRes$"Event Type"[1:5])
barplot(cropRes$Value[1:5],sub="Crop Damage",names.arg=cropRes$"Event Type"[1:5])
