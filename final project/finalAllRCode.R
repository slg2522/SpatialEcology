#########################Krill Thinning########################################

#load packages
library(raster)
library(dismo)
library(ggplot2)
library(usmap)

#load data
krill <- read.csv("C:/Users/hongs/OneDrive - University of Maryland/Desktop/anaconda files/krillMapbox/timeKrill.csv")
#subset by each year
k1993 <- subset(krill, krill$LABEL == "1993")
k1994 <- subset(krill, krill$LABEL == "1994")
k1995 <- subset(krill, krill$LABEL == "1995")
k1996 <- subset(krill, krill$LABEL == "1996")
k1997 <- subset(krill, krill$LABEL == "1997")
k1998 <- subset(krill, krill$LABEL == "1998")
k1999 <- subset(krill, krill$LABEL == "1999")
k2000 <- subset(krill, krill$LABEL == "2000")
k2001 <- subset(krill, krill$LABEL == "2001")
k2002 <- subset(krill, krill$LABEL == "2002")
k2003 <- subset(krill, krill$LABEL == "2003")
k2004 <- subset(krill, krill$LABEL == "2004")
k2005 <- subset(krill, krill$LABEL == "2005")
k2006 <- subset(krill, krill$LABEL == "2006")
k2007 <- subset(krill, krill$LABEL == "2007")
k2008 <- subset(krill, krill$LABEL == "2008")
k2009 <- subset(krill, krill$LABEL == "2009")
k2010 <- subset(krill, krill$LABEL == "2010")
k2011 <- subset(krill, krill$LABEL == "2011")
k2012 <- subset(krill, krill$LABEL == "2012")
k2013 <- subset(krill, krill$LABEL == "2013")
k2014 <- subset(krill, krill$LABEL == "2014")
k2015 <- subset(krill, krill$LABEL == "2015")
k2016 <- subset(krill, krill$LABEL == "2016")
k2017 <- subset(krill, krill$LABEL == "2017")
k2018 <- subset(krill, krill$LABEL == "2018")

#remove the year column
k1993 <- subset(k1993, select = -c(LABEL))
k1994 <- subset(k1994, select = -c(LABEL))
k1995 <- subset(k1995, select = -c(LABEL))
k1996 <- subset(k1996, select = -c(LABEL))
k1997 <- subset(k1997, select = -c(LABEL))
k1998 <- subset(k1998, select = -c(LABEL))
k1999 <- subset(k1999, select = -c(LABEL))
k2000 <- subset(k2000, select = -c(LABEL))
k2001 <- subset(k2001, select = -c(LABEL))
k2002 <- subset(k2002, select = -c(LABEL))
k2003 <- subset(k2003, select = -c(LABEL))
k2004 <- subset(k2004, select = -c(LABEL))
k2005 <- subset(k2005, select = -c(LABEL))
k2006 <- subset(k2006, select = -c(LABEL))
k2007 <- subset(k2007, select = -c(LABEL))
k2008 <- subset(k2008, select = -c(LABEL))
k2009 <- subset(k2009, select = -c(LABEL))
k2010 <- subset(k2010, select = -c(LABEL))
k2011 <- subset(k2011, select = -c(LABEL))
k2012 <- subset(k2012, select = -c(LABEL))
k2013 <- subset(k2013, select = -c(LABEL))
k2014 <- subset(k2014, select = -c(LABEL))
k2015 <- subset(k2015, select = -c(LABEL))
k2016 <- subset(k2016, select = -c(LABEL))
k2017 <- subset(k2017, select = -c(LABEL))
k2018 <- subset(k2018, select = -c(LABEL))

#set the resolution
res = 5

#sampling for 1993
r1993 = raster(extent(range(k1993[,1]), range(k1993[,2])) + res)
res(r1993) = res
plot(k1993)
samp1993 = gridSample(k1993, r1993, n=1)
plot(k1993, cex=1)
points(samp1993, pch='x', col='red')

#sampling for 1994
r1994 = raster(extent(range(k1994[,1]), range(k1994[,2])) + res)
res(r1994) = res
plot(k1994)
samp1994 = gridSample(k1994, r1994, n=1)
plot(k1994, cex=1)
points(samp1994, pch='x', col='red')

#sampling for 1995
r1995 = raster(extent(range(k1995[,1]), range(k1995[,2])) + res)
res(r1995) = res
plot(k1995)
samp1995 = gridSample(k1995, r1995, n=1)
plot(k1995, cex=1)
points(samp1995, pch='x', col='red')

#sampling for 1996
r1996 = raster(extent(range(k1996[,1]), range(k1996[,2])) + res)
res(r1996) = res
plot(k1996)
samp1996 = gridSample(k1996, r1996, n=1)
plot(k1996, cex=1)
points(samp1996, pch='x', col='red')

#sampling for 1997
r1997 = raster(extent(range(k1997[,1]), range(k1997[,2])) + res)
res(r1997) = res
plot(k1997)
samp1997 = gridSample(k1997, r1997, n=1)
plot(k1997, cex=1)
points(samp1997, pch='x', col='red')

#sampling for 1998
r1998 = raster(extent(range(k1998[,1]), range(k1998[,2])) + res)
res(r1998) = res
plot(k1998)
samp1998 = gridSample(k1998, r1998, n=1)
plot(k1998, cex=1)
points(samp1998, pch='x', col='red')

#sampling for 1999
r1999 = raster(extent(range(k1999[,1]), range(k1999[,2])) + res)
res(r1999) = res
plot(k1999)
samp1999 = gridSample(k1999, r1999, n=1)
plot(k1999, cex=1)
points(samp1999, pch='x', col='red')

#sampling for 2000
r2000 = raster(extent(range(k2000[,1]), range(k2000[,2])) + res)
res(r2000) = res
plot(k2000)
samp2000 = gridSample(k2000, r2000, n=1)
plot(k2000, cex=1)
points(samp2000, pch='x', col='red')

#sampling for 2001
r2001 = raster(extent(range(k2001[,1]), range(k2001[,2])) + res)
res(r2001) = res
plot(k2001)
samp2001 = gridSample(k2001, r2001, n=1)
plot(k2001, cex=1)
points(samp2001, pch='x', col='red')

#sampling for 2002
r2002 = raster(extent(range(k2002[,1]), range(k2002[,2])) + res)
res(r2002) = res
plot(k2002)
samp2002 = gridSample(k2002, r2002, n=1)
plot(k2002, cex=1)
points(samp2002, pch='x', col='red')

#sampling for 2003
r2003 = raster(extent(range(k2003[,1]), range(k2003[,2])) + res)
res(r2003) = res
plot(k2003)
samp2003 = gridSample(k2003, r2003, n=1)
plot(k2003, cex=1)
points(samp2003, pch='x', col='red')

#sampling for 2004
r2004 = raster(extent(range(k2004[,1]), range(k2004[,2])) + res)
res(r2004) = res
plot(k2004)
samp2004 = gridSample(k2004, r2004, n=1)
plot(k2004, cex=1)
points(samp2004, pch='x', col='red')

#sampling for 2005
r2005 = raster(extent(range(k2005[,1]), range(k2005[,2])) + res)
res(r2005) = res
plot(k2005)
samp2005 = gridSample(k2005, r2005, n=1)
plot(k2005, cex=1)
points(samp2005, pch='x', col='red')

#sampling for 2006
r2006 = raster(extent(range(k2006[,1]), range(k2006[,2])) + res)
res(r2006) = res
plot(k2006)
samp2006 = gridSample(k2006, r2006, n=1)
plot(k2006, cex=1)
points(samp2006, pch='x', col='red')

#sampling for 2007
r2007 = raster(extent(range(k2007[,1]), range(k2007[,2])) + res)
res(r2007) = res
plot(k2007)
samp2007 = gridSample(k2007, r2007, n=1)
plot(k2007, cex=1)
points(samp2007, pch='x', col='red')

#sampling for 2008
r2008 = raster(extent(range(k2008[,1]), range(k2008[,2])) + res)
res(r2008) = res
plot(k2008)
samp2008 = gridSample(k2008, r2008, n=1)
plot(k2008, cex=1)
points(samp2008, pch='x', col='red')

#sampling for 2009
r2009 = raster(extent(range(k2009[,1]), range(k2009[,2])) + res)
res(r2009) = res
plot(k2009)
samp2009 = gridSample(k2009, r2009, n=1)
plot(k2009, cex=1)
points(samp2009, pch='x', col='red')

#sampling for 2010
r2010 = raster(extent(range(k2010[,1]), range(k2010[,2])) + res)
res(r2010) = res
plot(k2010)
samp2010 = gridSample(k2010, r2010, n=1)
plot(k2010, cex=1)
points(samp2010, pch='x', col='red')

#sampling for 2011
r2011 = raster(extent(range(k2011[,1]), range(k2011[,2])) + res)
res(r2011) = res
plot(k2011)
samp2011 = gridSample(k2011, r2011, n=1)
plot(k2011, cex=1)
points(samp2011, pch='x', col='red')

#sampling for 2012
r2012 = raster(extent(range(k2012[,1]), range(k2012[,2])) + res)
res(r2012) = res
plot(k2012)
samp2012 = gridSample(k2012, r2012, n=1)
plot(k2012, cex=1)
points(samp2012, pch='x', col='red')

#sampling for 2013
r2013 = raster(extent(range(k2013[,1]), range(k2013[,2])) + res)
res(r2013) = res
plot(k2013)
samp2013 = gridSample(k2013, r2013, n=1)
plot(k2013, cex=1)
points(samp2013, pch='x', col='red')

#sampling for 2014
r2014 = raster(extent(range(k2014[,1]), range(k2014[,2])) + res)
res(r2014) = res
plot(k2014)
samp2014 = gridSample(k2014, r2014, n=1)
plot(k2014, cex=1)
points(samp2014, pch='x', col='red')

#sampling for 2015
r2015 = raster(extent(range(k2015[,1]), range(k2015[,2])) + res)
res(r2015) = res
plot(k2015)
samp2015 = gridSample(k2015, r2015, n=1)
plot(k2015, cex=1)
points(samp2015, pch='x', col='red')

#sampling for 2016
r2016 = raster(extent(range(k2016[,1]), range(k2016[,2])) + res)
res(r2016) = res
plot(k2016)
samp2016 = gridSample(k2016, r2016, n=1)
plot(k2016, cex=1)
points(samp2016, pch='x', col='red')

#sampling for 2017
r2017 = raster(extent(range(k2017[,1]), range(k2017[,2])) + res)
res(r2017) = res
plot(k2017)
samp2017 = gridSample(k2017, r2017, n=1)
plot(k2017, cex=1)
points(samp2017, pch='x', col='red')

#sampling for 2018
r2018 = raster(extent(range(k2018[,1]), range(k2018[,2])) + res)
res(r2018) = res
plot(k2018)
samp2018 = gridSample(k2018, r2018, n=1)
plot(k2018, cex=1)
points(samp2018, pch='x', col='red')

###############################################################################

#attempt adding years
#1995-2000 grouping
k9500 = rbind(k1995, k1996, k1997, k1998, k1999, k2000)
plot(k9500, cex=1)
r9500 = raster(extent(range(k9500[,1]), range(k9500[,2])) + res)
res(r9500) = res
plot(k9500)
samp9500 = gridSample(k9500, r9500, n=1)
plot(k9500, cex=1)
points(samp9500, pch='x', col='red')

#2001-2006 grouping
k0106 = rbind(k2001, k2002, k2003, k2004, k2005, k2006)
plot(k0106, cex=1)
r0106 = raster(extent(range(k0106[,1]), range(k0106[,2])) + res)
res(r0106) = res
plot(k0106)
samp0106 = gridSample(k0106, r0106, n=1)
plot(k0106, cex=1)
points(samp0106, pch='x', col='red')

#2007-2012 grouping
k0712 = rbind(k2007, k2008, k2009, k2010, k2011, k2012)
plot(k0712, cex=1)
r0712 = raster(extent(range(k0712[,1]), range(k0712[,2])) + res)
res(r0712) = res
plot(k0712)
samp0712 = gridSample(k0712, r0712, n=1)
plot(k0712, cex=1)
points(samp0712, pch='x', col='red')

#2013-2018 grouping
k1318 = rbind(k2013, k2014, k2015, k2016, k2017, k2018)
plot(k1318, cex=1)
r1318 = raster(extent(range(k1318[,1]), range(k1318[,2])) + res)
res(r1318) = res
plot(k1318)
samp1318 = gridSample(k1318, r1318, n=1)
plot(k1318, cex=1)
points(samp1318, pch='x', col='red')

#############################################################################

k9500$SPEC <- c("krill")

#try spatial thinning
#from RSpatial
library(spThin)
thin9500 <- thin(
  k9500,
  lat.col = "LAT",
  long.col = "LON",
  spec.col = "SPEC",
  thin.par=50,
  reps=1,
  locs.thinned.list.return = TRUE,
  write.files = FALSE,
  max.files = 5,
  out.dir,
  out.base = "thinned_data",
  write.log.file = FALSE,
  log.file = "spatial_thin_log.txt",
  verbose = TRUE
)

thinned9500 <- data.frame(thin9500[[1]])

#thinned data as csv
write.csv(thinned9500, "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/thinned9500LongLat.csv", row.names=TRUE)

alaska <- plot_usmap(regions=c("state"), include=c("AK"), linewidth=1, labels = FALSE)

#transform data
transk9500 <- usmap_transform(k9500, input_names = c("LON", "LAT"), output_names = c("x", "y") )
trans9500 <- usmap_transform(thinned9500, input_names = c("Longitude", "Latitude"), output_names = c("x", "y") )


#plot with transformed data
alaska + geom_point(data=transk9500, aes(x=x, y=y, colour="raw"), size=1.5, alpha=I(0.5)) +
  geom_point(data=trans9500, aes(x=x, y=y, colour="thin"), size=1.5, alpha=I(0.5))

#plot side-by-side
par(mfrow=c(1,2))
raw9500 <- alaska + geom_point(data=transk9500, aes(x=x, y=y, colour="raw"), size=1.5, alpha=I(0.5)) + labs(title = "1995-2000 Raw")
thin9500 <- alaska + geom_point(data=trans9500, aes(x=x, y=y, colour="thin"), size=1.5, alpha=I(0.5)) + labs(title = "1995-2000 Thinned")
par(mfrow=c(1,2))
raw9500
thin9500

#save raw and thinned maps
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/raw1995-2000.png",
    width=780, height=480)
raw9500
dev.off()

png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/thin1995-2000.png",
    width=780, height=480)
thin9500
dev.off()

###############################################################

k0106$SPEC <- c("krill")

#try spatial thinning
#from RSpatial
thin0106 <- thin(
  k0106,
  lat.col = "LAT",
  long.col = "LON",
  spec.col = "SPEC",
  thin.par=50,
  reps=1,
  locs.thinned.list.return = TRUE,
  write.files = FALSE,
  max.files = 5,
  out.dir,
  out.base = "thinned_data",
  write.log.file = TRUE,
  log.file = "spatial_thin_log.txt",
  verbose = TRUE
)

thinned0106 <- data.frame(thin0106[[1]])

#thinned data as csv
write.csv(thinned0106, "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/thinned0106LongLat.csv", row.names=TRUE)


alaska <- plot_usmap(regions=c("state"), include=c("AK"), linewidth=1, labels = FALSE)

#transform data
transk0106 <- usmap_transform(k0106, input_names = c("LON", "LAT"), output_names = c("x", "y") )
trans0106 <- usmap_transform(thinned0106, input_names = c("Longitude", "Latitude"), output_names = c("x", "y") )


#plot with transformed data
alaska + geom_point(data=transk0106, aes(x=x, y=y, colour="raw"), size=1.5, alpha=I(0.5)) +
  geom_point(data=trans0106, aes(x=x, y=y, colour="thin"), size=1.5, alpha=I(0.5))

#plot side-by-side
par(mfrow=c(1,2))
raw0106 <- alaska + geom_point(data=transk0106, aes(x=x, y=y, colour="raw"), size=1.5, alpha=I(0.5)) + labs(title = "Raw")
thin0106 <- alaska + geom_point(data=trans0106, aes(x=x, y=y, colour="thin"), size=1.5, alpha=I(0.5)) + labs(title = "Thinned")
par(mfrow=c(1,2))
raw0106
thin0106

#save raw and thinned maps
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/raw2001-2006.png",
    width=780, height=480)
raw0106
dev.off()

png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/thin2001-2006.png",
    width=780, height=480)
thin0106
dev.off()

##################################################################################

k0712$SPEC <- c("krill")

#try spatial thinning
#from RSpatial
thin0712 <- thin(
  k0712,
  lat.col = "LAT",
  long.col = "LON",
  spec.col = "SPEC",
  thin.par=50,
  reps=1,
  locs.thinned.list.return = TRUE,
  write.files = FALSE,
  max.files = 5,
  out.dir,
  out.base = "thinned_data",
  write.log.file = TRUE,
  log.file = "spatial_thin_log.txt",
  verbose = TRUE
)

thinned0712 <- data.frame(thin0712[[1]])

#thinned data as csv
write.csv(thinned0712, "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/thinned0712LongLat.csv", row.names=TRUE)


alaska <- plot_usmap(regions=c("state"), include=c("AK"), linewidth=1, labels = FALSE)

#transform data
transk0712 <- usmap_transform(k0712, input_names = c("LON", "LAT"), output_names = c("x", "y") )
trans0712 <- usmap_transform(thinned0712, input_names = c("Longitude", "Latitude"), output_names = c("x", "y") )


#plot with transformed data
alaska + geom_point(data=transk0712, aes(x=x, y=y, colour="raw"), size=1.5, alpha=I(0.5)) +
  geom_point(data=trans0712, aes(x=x, y=y, colour="thin"), size=1.5, alpha=I(0.5))

#plot side-by-side
par(mfrow=c(1,2))
raw0712 <- alaska + geom_point(data=transk0712, aes(x=x, y=y, colour="raw"), size=1.5, alpha=I(0.5)) + labs(title = "Raw")
thin0712 <- alaska + geom_point(data=trans0712, aes(x=x, y=y, colour="thin"), size=1.5, alpha=I(0.5)) + labs(title = "Thinned")
par(mfrow=c(1,2))
raw0712
thin0712

#save raw and thinned maps
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/raw2007-2012.png",
    width=780, height=480)
raw0712
dev.off()

png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/thin2007-2012.png",
    width=780, height=480)
thin0712
dev.off()

################################################################################

k1318$SPEC <- c("krill")

#try spatial thinning
#from RSpatial
thin1318 <- thin(
  k1318,
  lat.col = "LAT",
  long.col = "LON",
  spec.col = "SPEC",
  thin.par=50,
  reps=1,
  locs.thinned.list.return = TRUE,
  write.files = FALSE,
  max.files = 5,
  out.dir,
  out.base = "thinned_data",
  write.log.file = TRUE,
  log.file = "spatial_thin_log.txt",
  verbose = TRUE
)

thinned1318 <- data.frame(thin1318[[1]])

#thinned data as csv
write.csv(thinned1318, "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/thinned1318LongLat.csv", row.names=TRUE)

alaska <- plot_usmap(regions=c("state"), include=c("AK"), linewidth=1, labels = FALSE)

#transform data
transk1318 <- usmap_transform(k1318, input_names = c("LON", "LAT"), output_names = c("x", "y") )
trans1318 <- usmap_transform(thinned1318, input_names = c("Longitude", "Latitude"), output_names = c("x", "y") )


#plot with transformed data
alaska + geom_point(data=transk1318, aes(x=x, y=y, colour="raw"), size=1.5, alpha=I(0.5)) +
  geom_point(data=trans1318, aes(x=x, y=y, colour="thin"), size=1.5, alpha=I(0.5))

#plot side-by-side
par(mfrow=c(1,2))
raw1318 <- alaska + geom_point(data=transk1318, aes(x=x, y=y, colour="raw"), size=1.5, alpha=I(0.5)) + labs(title = "Raw")
thin1318 <- alaska + geom_point(data=trans1318, aes(x=x, y=y, colour="thin"), size=1.5, alpha=I(0.5)) + labs(title = "Thinned")
par(mfrow=c(1,2))
raw1318
thin1318

#save raw and thinned maps
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/raw2013-2018.png",
    width=780, height=480)
raw1318
dev.off()

png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/thin2013-2018.png",
    width=780, height=480)
thin1318
dev.off()

###############################################################################

#test for normality
par(mfrow=c(1,1))

#x coordinate (long)
#thinned
qqnorm(trans9500$x, pch = 1, frame = FALSE)
qqline(trans9500$x, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQthinX1995-2000.png",
    width=780, height=480)
qqnorm(trans9500$x, pch = 1, frame = FALSE)
qqline(trans9500$x, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans9500$x)

#original
qqnorm(transk9500$x, pch = 1, frame = FALSE)
qqline(transk9500$x, col = "steelblue", lwd = 2)

png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQrawX1995-2000.png",
    width=780, height=480)
qqnorm(transk9500$x, pch = 1, frame = FALSE)
qqline(transk9500$x, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(transk9500$x)

#y coordinate (lat)
#thinned
qqnorm(trans9500$y, pch = 1, frame = FALSE)
qqline(trans9500$y, col = "steelblue", lwd = 2)

png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQthinY1995-2000.png",
    width=780, height=480)
qqnorm(trans9500$y, pch = 1, frame = FALSE)
qqline(trans9500$y, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans9500$y)

#original
qqnorm(transk9500$y, pch = 1, frame = FALSE)
qqline(transk9500$y, col = "steelblue", lwd = 2)

png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQrawY1995-2000.png",
    width=780, height=480)
qqnorm(transk9500$y, pch = 1, frame = FALSE)
qqline(transk9500$y, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(transk9500$y)

#non-transformed
boxplot(transk9500$x)$out
boxplot(transk9500$y)$out

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxRawX1995-2000.png",
    width=780, height=480)
boxplot(transk9500$x)$out
dev.off()

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxRawY1995-2000.png",
    width=780, height=480)
boxplot(transk9500$y)$out
dev.off()

#visualize outliers thinned
boxplot(trans9500$x)$out
boxplot(trans9500$y)$out

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxThinX1995-2000.png",
    width=780, height=480)
boxplot(trans9500$x)$out
dev.off()

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxThinY1995-2000.png",
    width=780, height=480)
boxplot(trans9500$y)$out
dev.off()

##############################################################################

#x coordinate (long)
#thinned
qqnorm(trans0106$x, pch = 1, frame = FALSE)
qqline(trans0106$x, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQthinX2001-2006.png",
    width=780, height=480)
qqnorm(trans0106$x, pch = 1, frame = FALSE)
qqline(trans0106$x, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans0106$x)

#original
qqnorm(transk0106$x, pch = 1, frame = FALSE)
qqline(transk0106$x, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQrawX2001-2006.png",
    width=780, height=480)
qqnorm(transk0106$x, pch = 1, frame = FALSE)
qqline(transk0106$x, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(transk0106$x)

#y coordinate (lat)
#thinned
qqnorm(trans0106$y, pch = 1, frame = FALSE)
qqline(trans0106$y, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQthinY2001-2006.png",
    width=780, height=480)
qqnorm(trans0106$y, pch = 1, frame = FALSE)
qqline(trans0106$y, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans0106$y)

#original
qqnorm(transk0106$y, pch = 1, frame = FALSE)
qqline(transk0106$y, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQrawY2001-2006.png",
    width=780, height=480)
qqnorm(transk0106$y, pch = 1, frame = FALSE)
qqline(transk0106$y, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(transk0106$y)

#visualize outliers
boxplot(transk0106$x)$out
boxplot(transk0106$y)$out

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxRawX2001-2006.png",
    width=780, height=480)
boxplot(transk0106$x)$out
dev.off()

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxRawY2001-2006.png",
    width=780, height=480)
boxplot(transk0106$y)$out
dev.off()

#visualize outliers
boxplot(trans0106$x)$out
boxplot(trans0106$y)$out

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxThinX2001-2006.png",
    width=780, height=480)
boxplot(trans0106$x)$out
dev.off()

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxThinY2001-2006.png",
    width=780, height=480)
boxplot(trans0106$y)$out
dev.off()

##############################################################################

#x coordinate (long)
#thinned
qqnorm(trans0712$x, pch = 1, frame = FALSE)
qqline(trans0712$x, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQthinX2007-2012.png",
    width=780, height=480)
qqnorm(trans0712$x, pch = 1, frame = FALSE)
qqline(trans0712$x, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans0712$x)

#original
qqnorm(transk0712$x, pch = 1, frame = FALSE)
qqline(transk0712$x, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQRawX2007-2012.png",
    width=780, height=480)
qqnorm(transk0712$x, pch = 1, frame = FALSE)
qqline(transk0712$x, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(transk0712$x)

#y coordinate (lat)
#thinned
qqnorm(trans0712$y, pch = 1, frame = FALSE)
qqline(trans0712$y, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQThinY2007-2012.png",
    width=780, height=480)
qqnorm(trans0712$y, pch = 1, frame = FALSE)
qqline(trans0712$y, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans0712$y)

#original
qqnorm(transk0712$y, pch = 1, frame = FALSE)
qqline(transk0712$y, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQRawY2007-2012.png",
    width=780, height=480)
qqnorm(transk0712$y, pch = 1, frame = FALSE)
qqline(transk0712$y, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(transk0712$y)

#visualize outliers
boxplot(trans0712$x)$out
boxplot(trans0712$y)$out

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxThinX2007-2012.png",
    width=780, height=480)
boxplot(trans0712$x)$out
dev.off()

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxThinY2007-2012.png",
    width=780, height=480)
boxplot(trans0712$y)$out
dev.off()

#visualize outliers
boxplot(transk0712$x)$out
boxplot(transk0712$y)$out

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxRawX2007-2012.png",
    width=780, height=480)
boxplot(transk0712$x)$out
dev.off()

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxRawY2007-2012.png",
    width=780, height=480)
boxplot(transk0712$y)$out
dev.off()

##############################################################################

#x coordinate (long)
#thinned
qqnorm(trans1318$x, pch = 1, frame = FALSE)
qqline(trans1318$x, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQthinX2013-2018.png",
    width=780, height=480)
qqnorm(trans1318$x, pch = 1, frame = FALSE)
qqline(trans1318$x, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans1318$x)

#original
qqnorm(transk1318$x, pch = 1, frame = FALSE)
qqline(transk1318$x, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQRawX2013-2018.png",
    width=780, height=480)
qqnorm(transk1318$x, pch = 1, frame = FALSE)
qqline(transk1318$x, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(transk1318$x)

#y coordinate (lat)
#thinned
qqnorm(trans1318$y, pch = 1, frame = FALSE)
qqline(trans1318$y, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQthinY2013-2018.png",
    width=780, height=480)
qqnorm(trans1318$y, pch = 1, frame = FALSE)
qqline(trans1318$y, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans1318$y)

#original
qqnorm(transk1318$y, pch = 1, frame = FALSE)
qqline(transk1318$y, col = "steelblue", lwd = 2)

#save QQ plot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/QQRawY2013-2018.png",
    width=780, height=480)
qqnorm(transk1318$y, pch = 1, frame = FALSE)
qqline(transk1318$y, col = "steelblue", lwd = 2)
dev.off()

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(transk1318$y)

#visualize outliers
boxplot(trans1318$x)$out
boxplot(trans1318$y)$out

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxThinX2013-2018.png",
    width=780, height=480)
boxplot(trans1318$x)$out
dev.off()

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxThinY2013-2018.png",
    width=780, height=480)
boxplot(trans1318$y)$out
dev.off()

#visualize outliers
boxplot(transk1318$x)$out
boxplot(transk1318$y)$out

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxRawX2013-2018.png",
    width=780, height=480)
boxplot(transk1318$x)$out
dev.off()

#save boxplot
png(file="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/samplingBias/boxRawY2013-2018.png",
    width=780, height=480)
boxplot(transk1318$y)$out
dev.off()

##############################MaxENT###########################################

# load the required libraries
library(raster)
library(sp)
library(rgdal)
library(dismo)
library(sp)
library(sm)
library(colorRamps)
library(ENMeval)
library(terra)

################################################################################
# CHUNK 1: Read in & prep data
################################################################################

# set working directory to data folder
#setwd("pathToDirHere")
wd <- ("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt")
setwd(wd)

#load the rasters and create the necessary raster stacks prior to averaging

#chlorophyllA
#load in all files
ca <- list.files(path="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt/data/ChlorophyllA", pattern = "TIFF", full.names = TRUE)
#stack the files
stackCA <- raster::stack(ca)
#write a combined raster file
writeRaster(stackCA, "chlorophyllCombined.TIF")
#take the average
ca_avg <- mean(stackCA)
#plot the average
plot(ca_avg)


#sea surface temperature
#load in all files
sst <- list.files(path="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt/data/SeaSurfaceTemperature", pattern = "TIFF", full.names = TRUE)
#stack the files
stackSST <- raster::stack(sst)
#write a combined raster file
writeRaster(stackSST, "sstCombined.TIF")
#take the average
sst_avg <- mean(stackSST)
#plot the average
plot(sst_avg)
#terra does not take non-integer values, to convert, must take factors of 5 and 2
#change to 0.5 resolution
sst5 <- aggregate(sst_avg, 2)
plot(sst5)
#change to 0.1 resolution
sst1 <- disaggregate(sst5, 5)
plot(sst1)


#bathymetry
#load in all files
bathymetry <- list.files(path="C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt/data/Bathymetry", pattern = "TIFF", full.names = TRUE)
#stack the files
stackBath <- raster::stack(bathymetry)
#write a combined raster file
writeRaster(stackBath, "bathymetryCombined.TIF")
#take the average
bath_avg <- mean(stackBath)
#plot the average
plot(bath_avg)

# thinned krill occurrence data for the Bering Sea
krill9500 <- read.csv("thin9500LonLat.csv")
krill0106 <- read.csv("thin0106LonLat.csv")
krill0712 <- read.csv("thin0712LonLat.csv")
krill1318 <- read.csv("thin1318LonLat.csv")

#check the data with headers
head(krill9500)
head(krill0106)
head(krill0712)
head(krill1318)

#combine all of the thinned krill data
krillGeoXY <- data.frame(species=c(krill9500$species, krill0106$species, krill0712$species, krill1318$species),
                         lon=c(krill9500$lon, krill0106$lon, krill0712$lon, krill1318$lon),
                         lat=c(krill9500$lat, krill0106$lat, krill0712$lat, krill1318$lat))

#crop rasters:
e <- extent(-180, -150, 50, 70)
#chlorophyllA
cropCA <- crop(ca_avg, e)
plot(cropCA)
#SST
cropSST <- crop(sst1, e)
plot(cropSST)
#Bathymetry
cropBATH <- crop(bath_avg, e)
plot(cropBATH)

#stack all of the environmental covariates
envCovariates <- stack(cropCA, cropSST, cropBATH)

# read in climate & elev data
neClim <- stack(envCovariates)
krillSp <- krillGeoXY # make a new object for use later
# convert to sp object
coordinates(krillSp) <- c("lon", "lat")
# assign projection
projection(krillSp) <- projection(neClim)

# let's look at the data
plot(neClim$layer.1, col=rgb.tables(1000))
points(krillSp, pch=20, cex=0.7, col="black")

# make a mask raster to use below
mask <- neClim[[1]]>-1000


# Sampling bias ----------------------------------------------------------------
# To deal with spatial sampling bias, we will create
# a raster that reflects the sampling density of krill
# in the study region using kernel density estimation (KDE)

# use the x-y coords to get the cell numbers
bias <- cellFromXY(mask, krillSp) # cells with records
cells <- unique(sort(bias))
# xy-locations of all samples
kernelXY <- xyFromCell(mask, cells)
samps <- as.numeric(table(bias)) 
# number of samples in each grid cell
head(samps)
max(samps) #one grid cell has 255 records in it

# code to make KDE (kernel density estimate) raster
# Our goal is to extrapolate sampling density across the study region
# and to make a raster that matches the env predictors exactly. To do that,
# we need to set some parameters to produce a grid that is close in size
# and resolution to the env rasters. The 'sm.density' function will perform
# 2D density estimation.
KDEsur <- sm.density(kernelXY, 
                     weights=samps, 
                     display="none", # do not plot 
                     ngrid=300, # number of grid cells along each dimension,
                     # there are 300 columns
                     ylim=c(50,70), # approximate latitude range 
                     xlim=c(-180,-150), # approximate longitude range
                     nbins=0) # see help, can ignore

# let's look at the structure
str(KDEsur)
head(KDEsur$eval.points) # point locations where denisty was estimated
KDEsur$estimate[1:5, 1:5] # the density estimates

# now we need to turn this output into a raster
# expand grid will make all possible combinations of two vectors
# in this case we want to get all possible combinates of the x-y coordinates
# which will produce a full grid
pointGrid <- expand.grid(x=KDEsur$eval.points[,1], y=KDEsur$eval.points[,2])
KDErast <- SpatialPoints(pointGrid) # convert to sp object
# And now a step closer to a grid
KDErast <- SpatialPixelsDataFrame(KDErast, # points
                                  data.frame(kde = array(KDEsur$estimate, # data
                                                         length(KDEsur$estimate))))
# convert to a raster and plot
KDErast <- raster(KDErast)
plot(KDErast)

# now crop, etc so that it matches the climate data
KDErast <- resample(KDErast, mask)
KDErast <- KDErast*mask
plot(KDErast)
stack(neClim, KDErast) # no errors!

# convert to points for use as background locations 
KDEpts <- rasterToPoints(KDErast)
bg <- randomPoints(KDErast, 500, prob=T) # alternate method
################################################################################


################################################################################
# CHUNK 2: Run models
################################################################################
# extract data for krill spp
krillSpp <- c("krill")
a <- 1 #only working with krill as a taxonomic group
# loop to run models for each species if there were multiple species of krill
#for(a in 1:length(krillSpp)){
krillGeoXY <- krillGeoXY[krillGeoXY$species==krillSpp[a],-1]
#check the dimensions
dim(krillGeoXY)
#plot
plot(neClim[[1]])
points(krillGeoXY$lon, krillGeoXY$lat, pch=20, col="black")

########## make sets of training/test (t/t) data
ttSplit = 0.25 # ttSplit = test/train split percentage
# function to partition data into t/t
fold <- function(ttSplit){ 
  k = round(1/ttSplit, 0)
  fold <- kfold(krillGeoXY, k=k)
}

# make sets of t/t data
sets = 5 # number of t/t sets to make. We used 50 in the paper.
folds <- replicate(sets, fold(ttSplit)) # replicate five different random folds
head(folds)

# now loop through to build lists of t/t data
krillTrain <- list()
krillTest <- list()
for(h in 1:sets){
  krillTrain[[h]] <- krillGeoXY[folds[,h]!=1,]
  krillTest[[h]] <- krillGeoXY[folds[,h]==1,]
}
str(krillTrain)

plot(neClim[[1]])
points(krillTrain[[1]], pch=20)
points(krillTrain[[2]], pch=20, col="red")



# now we will work through four different maxent models:
# (1) linear features only
# (2) Default features (set by number of occurrence records)
# (3) linear features only, bias corrected background
# (4) default features, bias corrected background



# MODEL 1 -----------------------------------------------------------------
#### MAXENT - LINEAR FEATURES
krillmaxMods_LF <- list()
# loop through each fold (5 total), fit a model, and place fitted model
# in a list
for(f in 1:sets){
  print(f)
  #### MAXENT - LINEAR FEATURES
  krillmaxMods_LF[[f]] <- maxent(neClim, krillTrain[[f]], 
                                 args=c(c("-h", # turn off hinge features
                                          "-q", # turn off quadratic features
                                          "-p", # turn off product features
                                          "-P", # return response curves
                                          "nothreshold"), # turn off threshold
                                        c("-m", 10000))) # max iterations = 10K
}
# predict to geography
krillmaxStack_LF <- predict(krillmaxMods_LF[[1]], neClim) #cloglog
krillmaxStackRAW_LF <- predict(krillmaxMods_LF[[1]], neClim, args='outputformat=raw')

# loop through the rest of the models and predict
for(j in 2:sets){
  print(j)
  mod_LF <- predict(krillmaxMods_LF[[j]], neClim)
  modRAW_LF <- predict(krillmaxMods_LF[[j]], neClim, args='outputformat=raw')
  krillmaxStack_LF <- stack(krillmaxStack_LF, mod_LF)
  krillmaxStackRAW_LF <- stack(krillmaxStackRAW_LF, modRAW_LF)
}

plot(krillmaxStack_LF, col=rgb.tables(1000))
plot(mean(krillmaxStack_LF), col=rgb.tables(1000)) # mean of the five models
plot(calc(krillmaxStack_LF, sd), col=rgb.tables(1000)) #std dev of the five models







# MODEL 2 -----------------------------------------------------------------
#### MAXENT - DEFAULT FEATURES
# Same as above, but now using default features
krillmaxMods_allF <- list()
for(f in 1:sets){
  print(f)
  krillmaxMods_allF[[f]] <- maxent(neClim, krillTrain[[f]], 
                                   args=c("-P",c("-m", 10000)))
}
# Predict to geography and stack
krillmaxStack_allF <- predict(krillmaxMods_allF[[1]], neClim)
krillmaxStackRAW_allF <- predict(krillmaxMods_allF[[1]], neClim, args='outputformat=raw')
for(j in 2:sets){
  print(j)
  mod_allF <- predict(krillmaxMods_allF[[j]], neClim)
  modRAW_allF <- predict(krillmaxMods_allF[[j]], neClim, args='outputformat=raw')
  krillmaxStack_allF <- stack(krillmaxStack_allF, mod_allF)
  krillmaxStackRAW_allF <- stack(krillmaxStackRAW_allF, modRAW_allF)
}

#plot the output (only one rep, so no mean or standard deviation)
plot(krillmaxStack_allF, col=rgb.tables(1000))

# MODEL 3 -----------------------------------------------------------------
#### MAXENT - LINEAR FEATURES + SAMPLING BIAS
# Account for smapling bias, linear features only
# 10,000 locations selected using probabilistic target-group sampling
# from KDE bias surface created above

# example of selecting background points
# to reflect bias in the presence data
# BG points are more likely to be selected where
# presence-only sampling is most dense

#this code creastes a raster surface with the underlying
#sampling bias estimated to account for sampling bias

bg <- KDEpts[sample(seq(1:nrow(KDEpts)), 
                    size=1000, 
                    replace=T, 
                    prob=KDEpts[,"layer"]),1:2]
plot(neClim[[1]])
points(bg, pch=20)

krillmaxMods_LF_bias <- list()
for(f in 1:sets){
  print(f)
  krillmaxMods_LF_bias[[f]] <- maxent(neClim, 
                                      krillTrain[[f]],
                                      # background points selected from KDEpts
                                      a=KDEpts[sample(seq(1:nrow(KDEpts)), 
                                                      size=10000, 
                                                      replace=T, 
                                                      prob=KDEpts[,"layer"]),1:2], 
                                      args=c(c("-h", "-q", "-p", "-P", "nothreshold"),c("-m", 10000)))
}

# Predict to geography and stack 
krillmaxStack_LF_bias <- predict(krillmaxMods_LF_bias[[1]], neClim)
krillmaxStackRAW_LF_bias <- predict(krillmaxMods_LF_bias[[1]], neClim, args='outputformat=raw')

for(j in 2:sets){
  print(j)
  mod_LF_bias <- predict(krillmaxMods_LF_bias[[j]], neClim)
  modRAW_LF_bias <- predict(krillmaxMods_LF_bias[[j]], neClim, args='outputformat=raw')
  krillmaxStack_LF_bias <- stack(krillmaxStack_LF_bias, mod_LF_bias)
  krillmaxStackRAW_LF_bias <- stack(krillmaxStackRAW_LF_bias, modRAW_LF_bias)
}


plot(krillmaxStack_LF_bias, col=rgb.tables(1000))
plot(mean(krillmaxStack_LF_bias), col=rgb.tables(1000)) # mean of the five models
plot(calc(krillmaxStack_LF_bias, sd), col=rgb.tables(1000)) #std dev of the five models


# MODEL 4 -----------------------------------------------------------------
#### MAXENT - DEFAULT FEATURES - BIAS CORRECTED BACKGROUND
krillmaxMods_allF_bias <- list()
for(f in 1:sets){
  print(f)
  krillmaxMods_allF_bias[[f]] <- maxent(neClim, 
                                        krillTrain[[f]], 
                                        a=KDEpts[sample(seq(1:nrow(KDEpts)), 
                                                        size=10000, 
                                                        replace=T, 
                                                        prob=KDEpts[,"layer"]),1:2], 
                                        args=c("-P",c("-m", 10000)))
}

# Predict to geography and stack
krillmaxStack_allF_bias <- predict(krillmaxMods_allF_bias[[1]], neClim)
krillmaxStackRAW_allF_bias <- predict(krillmaxMods_allF_bias[[1]], neClim, 
                                      args='outputformat=raw')
for(j in 2:sets){
  print(j)
  mod_allF_bias <- predict(krillmaxMods_allF_bias[[j]], neClim)
  modRAW_allF_bias <- predict(krillmaxMods_allF_bias[[j]], neClim, 
                              args='outputformat=raw')
  krillmaxStack_allF_bias <- stack(krillmaxStack_allF_bias, mod_allF_bias)
  krillmaxStackRAW_allF_bias <- stack(krillmaxStackRAW_allF_bias, modRAW_allF_bias)
}

plot(krillmaxStack_allF_bias, col=rgb.tables(1000))
plot(mean(krillmaxStack_allF_bias), col=rgb.tables(1000)) # mean of the five models
plot(calc(krillmaxStack_allF_bias, sd), col=rgb.tables(1000)) #std dev of the five models

#save workspace image
save.image(file=paste(krillSpp[a], ".RData", sep=""))
#}
################################################################################


# Next, we will calculate mean and standard deviation (sd) rasters for all the 
# models fitted above and save the prediction rasters to disk
################################################################################
# CHUNK 3: Write mean and sd rasters of predictions for evaluation, mapping, etc
################################################################################

#krillFile <- "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt/krill.RData"
#load(krillFile)

fileName <- "krill"

#maxent - LINEAR FEATURES
writeRaster(sum(krillmaxStack_LF)/dim(krillmaxStack_LF)[3], 
            paste0(fileName, "_maxent_prob_LF.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(krillmaxStack_LF, sd), 
            paste0(fileName, "_maxent_sd_LF.tiff",sep=""), 
            type="GTiff", overwrite=T)  

#maxent - ALL FEATURES
writeRaster(sum(krillmaxStack_allF)/dim(krillmaxStack_allF)[3], 
            paste0(fileName, "_maxent_prob_allF.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(krillmaxStack_allF, sd), 
            paste0(fileName, "_maxent_sd_allF.tiff",sep=""), 
            type="GTiff", overwrite=T)

#maxent - LINEAR FEATURES - ***bias corrected***
writeRaster(sum(krillmaxStack_LF_bias)/dim(krillmaxStack_LF_bias)[3], 
            paste0(fileName,"_maxent_prob_LF_biasCorrected.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(krillmaxStack_LF_bias, sd), 
            paste0(fileName,"_maxent_sd_LF_biasCorrected.tiff"), 
            type="GTiff", overwrite=T)  

#maxent - ALL FEATURES - ***bias corrected***
writeRaster(sum(krillmaxStack_allF_bias)/dim(krillmaxStack_allF_bias)[3], 
            paste0(fileName, "_maxent_prob_allF_biasCorrected.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(krillmaxStack_allF_bias, sd), 
            paste0(fileName,"_maxent_sd_allF_biasCorrected.tiff"), 
            type="GTiff", 
            overwrite=T)
################################################################################



################################################################################
# CHUNK 4: Calculate AICc for maxent models
################################################################################
library(PresenceAbsence)
library(raster)
library(dismo)
library(zoo)
library(ENMeval)

#work with the outdated ENMeval functions
library("remotes")
library("fs")


#create separate libraries for housing
dev_lib <- path_home_r("R/win-library/dev-versions/")
old_lib <- path_home_r("R/win-library/old-versions/")

#detach current version
detach("package:ENMeval")

install_version("ENMeval", version = "0.2.0", lib = old_lib)
library("ENMeval")
packageVersion("ENMeval")


#krillFile <- "C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/MaxEnt/krill.RData"
#load(krillFile)

aicc.LF <- aicc.allF <- aicc.LF_bias <- aicc.allF_bias <- NULL

#calc.aicc() is now aic.maxent(), so use ENMeval version 0.2.0
for(ii in 1:5){
  aicc.LF[ii] <- calc.aicc(nparam = get.params(krillmaxMods_LF[[ii]]), 
                           occ=krillGeoXY, 
                           krillmaxStack_LF[[ii]])$AICc
  
  #aicc.allF[ii] <- calc.aicc(nparam = get.params(krillmaxMods_allF[[ii]]), 
  #occ=krillGeoXY, 
  #krillmaxStack_allF[[ii]])$AICc
  
  aicc.LF_bias[ii] <- calc.aicc(nparam = get.params(krillmaxMods_LF_bias[[ii]]), 
                                occ=krillGeoXY, 
                                krillmaxStack_LF_bias[[ii]])$AICc
  
  aicc.allF_bias[ii] <- calc.aicc(nparam = get.params(krillmaxMods_allF_bias[[ii]]), 
                                  occ=krillGeoXY, 
                                  krillmaxStack_allF_bias[[ii]])$AICc
}    

meanaicc.LF <- mean(as.numeric(aicc.LF[1]),as.numeric(aicc.LF[2]), as.numeric(aicc.LF[3]), as.numeric(aicc.LF[4]), as.numeric(aicc.LF[5]))
meanaicc.LF_bias <- mean(as.numeric(aicc.LF_bias[1]),as.numeric(aicc.LF_bias[2]), as.numeric(aicc.LF_bias[3]), as.numeric(aicc.LF_bias[4]), as.numeric(aicc.LF_bias[5]))
meanaicc.allF_bias <- mean(as.numeric(aicc.allF_bias[1]),as.numeric(aicc.allF_bias[2]), as.numeric(aicc.allF_bias[3]), as.numeric(aicc.allF_bias[4]), as.numeric(aicc.allF_bias[5]))



################################################################################

#evaluate variables and collinearity
library(usdm)
vifCovariates <- as.character(vifstep(neClim)@results$Variables)
bioRastsKeep <- neClim[[vifCovariates]]
plot(bioRastsKeep, col=rgb.tables(1000), box=F, legend=T, axes=T)

################################################################################
# CHUNK 5: Evaluate models
################################################################################
library(PresenceAbsence)
library(raster)
library(dismo)
library(zoo)
library(ecospat)


######### maxent LF ############# 
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(krillmaxStack_LF, randomPoints(neClim, 10000))
probBG <- extract(krillmaxStack_LF, x)

for(ii in 1:dim(krillmaxStack_LF)[3]){    
  probTest <- as.numeric(na.omit(extract(krillmaxStack_LF[[ii]], krillTest[[ii]])))
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(krillmaxStack_LF[[ii]], krillTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_LF <- rbind(Boyce, AUCmod, meanProb, meanBG)

######### maxent allF #############  
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(krillmaxStack_allF, randomPoints(neClim, 10000))

for(ii in 1:dim(krillmaxStack_allF)[3]){    
  
  probTest <- as.numeric(na.omit(extract(krillmaxStack_allF[[ii]], krillTest[[ii]])))
  
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(krillmaxStack_allF[[ii]], krillTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_allF <- rbind(Boyce, AUCmod, meanProb, meanBG)

######### maxent LF bias corrected ############# 
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(krillmaxStack_LF_bias, randomPoints(neClim, 10000))

for(ii in 1:dim(krillmaxStack_LF_bias)[3]){    
  
  probTest <- as.numeric(na.omit(extract(krillmaxStack_LF_bias[[ii]], krillTest[[ii]])))
  
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(krillmaxStack_LF_bias[[ii]], krillTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_LF_bias <- rbind(Boyce, AUCmod, meanProb, meanBG)

######### maxent allF bias corrected #############  
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(krillmaxStack_allF_bias, randomPoints(neClim, 10000))

for(ii in 1:dim(krillmaxStack_allF_bias)[3]){    
  
  probTest <- as.numeric(na.omit(extract(krillmaxStack_allF_bias[[ii]], krillTest[[ii]])))
  
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(krillmaxStack_allF_bias[[ii]], krillTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_allF_bias <- rbind(Boyce, AUCmod, meanProb, meanBG)

rowMeans(maxentEval_LF)
rowMeans(maxentEval_allF)
rowMeans(maxentEval_LF_bias)
rowMeans(maxentEval_allF_bias)
################################################################################

#######################Cold Pool Area From Raster##############################

#this is 1/39 similar script files, email if you want them all

# load packages  
library(sf) 
library(ncdf4)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(ncdf4)
library(gunzip)
library(purrr)
library(R.utils)
library(gridExtra)
library(stringr)
library(palr)
library(dplyr)
library(sp)
library(rgdal)
library(gdata)

# set path and filename
files<- list.files("D:/Cold Pool/1980",pattern='*.nc',full.names=TRUE)
path <- "D:/Cold Pool/1980/"
dname <- "TEMP"

#extent
#Ext <- extent(c(222.6804, 584.4472, 109.7872, 349.5629))
drawExt2 <- extent(c(259.3191, 455.6267, 137.5159, 354.8564))

# rasterVis plot
library(RColorBrewer)
rbcols <- brewer.pal(11, 'RdBu')
newcol <- colorRampPalette(rbcols)
ncols <- 1000
rbcols2 <- newcol(ncols)#apply the function to get 100 colours

mapTheme <- rasterTheme(region = rev(rbcols2))
cutpts <- c(seq(-2, 16, by=0.25))

raster_list = list()
plot_list = list()
months_list = list("-01", "-02", "-03", "-04", "-05", "-06", "-07", "-08", "-09", "-10", "-11", "-12")

setwd("D:/Cold Pool/1980")

#if error, check that all files openable. Example:
#test <- nc_open("D:/Cold Pool/1980/R2200aRBRZBGCcaaa03a.pop.h.1980-12.nc")
#print(test)


#loop through files
for (i in seq_along(files)) {
  nc <- nc_open(files[i])
  ncfname <- files[i]
  #print(nc)
  
  #raster brick for temperature
  tmp_raster <- brick(ncfname, varname=dname)
  tmp_raster; class(tmp_raster)
  #crs
  crs(tmp_raster) <- "EPSG:3573"
  #crop the outline
  tmp_raster.crop <- crop(tmp_raster, drawExt2)
  
  #label
  label <- toString(ncatt_get(nc,0,"title"))
  label <- as.list(strsplit(label, "TRUE, ")[[1]])
  title <- label[2]
  filetitle <- paste(title, months_list[i], ".tif", sep="")
  
  #find the minimum temperature and plot
  minTemp <- calc(tmp_raster.crop, min, na.rm=TRUE, forcefun=FALSE, forceapply=FALSE, colNA='black')
  
  # Not sure if these are needed, but match the official example
  options = c("COMPRESS=LZW", "BIGTIFF=YES", "TILED=TRUE", "BLOCKXSIZE=256",
              "BLOCKYSIZE=256")
  
  writeRaster(minTemp, 
              filename=file.path(getwd(), filetitle), 
              datatype='INT1U',
              NAflag=0,
              options=options,
              overwrite=TRUE)
  raster_list[[i]] = minTemp
  
  #get coldpool shape
  coldPool <- rasterToPolygons(minTemp, fun=function(x){x< -1})
  #get resolution of raster
  resolution <- res(minTemp)
  print(resolution)
  #save shapefiles
  raster::shapefile(coldPool, paste(path, "1980", months_list[i], "coldPool.shp", sep=""), overwrite=TRUE)
  
  
  pltMin <- levelplot(minTemp, margin = F,  at=cutpts, cuts=101, pretty=TRUE, par.settings = mapTheme,
                      main=title)
  plot_list[[i]] = pltMin
  
  pltMin <- levelplot(minTemp, margin = F,  at=cutpts, cuts=1001, pretty=TRUE, par.settings = mapTheme,
                      main=title)
  plot_list[[i]] = pltMin
  
  rgb0 <- image_raster(minTemp, col = rev(rbcols2))
  
  writeRaster(rgb0, 
              filename=file.path(getwd(), filetitle), 
              datatype='INT1U',
              NAflag=0,
              options=options,
              overwrite=TRUE)
  
  raster_list[[i]] = rgb0
  nc_close(nc)
  
  nc_close(nc)
}

dev.off()

# Save plots to tiff. Makes a separate file for each plot.
for (i in 1:length(plot_list)) {
  file_name = paste(path, "1980", months_list[i], ".tiff", sep="")
  tiff(file_name)
  print(plot_list[[i]])
  dev.off()
}

#set up list of shapes
shapes <- list.files("D:/Cold Pool/1980",pattern='*.shp',full.names=TRUE)
shape_list <- c("pool1", "pool2", "pool3", "pool4", "pool5", "pool6", "pool7", "pool8", "pool9", "pool10", "pool11", "pool12")
color_list <- c("red", "orange", "yellow", "green", "blue", "purple", "pink", "grey", "brown", "cyan", "gold", "tan")
area_list <- c("area1", "area2", "area3", "area4", "area5", "area6", "area7", "area8", "area9", "area10", "area11", "area12")

for (i in seq_along(shapes)) {
  #read in shape
  shp <- readOGR(shapes[i])
  #plot shape
  plot(shp, col=color_list[i], main=shape_list[i])
  #check the CRS
  crs(shp)
  #calculate the area
  area <- sum(area(shp))
  #rename the shape
  mv(from="shp", to=shape_list[i])
  #rename the area
  mv(from="area", to=area_list[i])
}

#overlap all of them just to show that they maintain coordinates (ie visual of "crs(pool1)")
together <- rbind(pool1, pool2, pool3, pool4, pool5, pool6, pool7, pool8, pool9, pool10, pool11, pool12)
plot(together)

#create a dataframe to summarize areas
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
area <- c(area1, area2, area3, area4, area5, area6, area7, area8, area9, area10, area11, area12)
cpData <- data.frame(month, area)

#get total raster area
total <- rasterToPolygons(minTemp, fun=NULL)
#plot(total)
totalArea <- sum(area(total))
totalArea

#compute percent coldpool
cpData$percent <- (cpData$area/totalArea)*100
#check dataframe
cpData
write.csv(cpData, "area1980.csv", row.names=FALSE, quote=FALSE)

########################Cold Pool Area & Extent Graphs#########################

library(plotly)

setwd("D:/Cold Pool")
areaData <- read.csv("areaSummary.csv")
percentData <- read.csv("percentSummary.csv")

month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
           'August', 'September', 'October', 'November', 'December')

areaData$X <- month

#The default order will be alphabetized unless specified as below:
areaData$X = factor(areaData$X, levels = month.name)

#Make the Area Graph
fig <- plot_ly(x = ~areaData$X) 
fig <- fig %>% add_lines(y = ~areaData$X1980, name = "1980", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1981, name = "1981", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1982, name = "1982", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1983, name = "1983", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1984, name = "1984", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1985, name = "1985", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1986, name = "1986", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1987, name = "1987", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1988, name = "1988", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1989, name = "1989", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1990, name = "1990", line = list(shape = "linear"))

fig <- fig %>% add_lines(y = ~areaData$X1991, name = "1991", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1992, name = "1992", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1993, name = "1993", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1994, name = "1994", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1995, name = "1995", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1996, name = "1996", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1997, name = "1997", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1998, name = "1998", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1999, name = "1999", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2000, name = "2000", line = list(shape = "linear"))

fig <- fig %>% add_lines(y = ~areaData$X2001, name = "2001", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2002, name = "2002", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2003, name = "2003", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2004, name = "2004", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2005, name = "2005", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2006, name = "2006", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2007, name = "2007", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2008, name = "2008", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2009, name = "2009", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2010, name = "2010", line = list(shape = "linear"))

fig <- fig %>% add_lines(y = ~areaData$X2011, name = "2011", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2012, name = "2012", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2013, name = "2013", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2014, name = "2014", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2015, name = "2015", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2016, name = "2016", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2017, name = "2017", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2018, name = "2018", line = list(shape = "linear"))

#formatting
fig <- fig %>% layout(title = "Area of < -1C Bottom Water (RASM)",
                      xaxis = list(title = "Months",
                                   showgrid = FALSE,
                                   showline = TRUE,
                                   showticklabels = TRUE,
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Area (Raster Tiles)",
                                   showgrid = FALSE,
                                   showline = TRUE,
                                   showticklabels = TRUE,
                                   ticks = 'outside',
                                   zeroline = FALSE))

fig


#Make the Percent Graph
percentData$X <- month

#The default order will be alphabetized unless specified as below:
percentData$X = factor(percentData$X, levels = month.name)

per <- plot_ly(x = ~areaData$X) 
per <- per %>% add_lines(y = ~percentData$X1980, name = "1980", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1981, name = "1981", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1982, name = "1982", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1983, name = "1983", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1984, name = "1984", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1985, name = "1985", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1986, name = "1986", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1987, name = "1987", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1988, name = "1988", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1989, name = "1989", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1990, name = "1990", line = list(shape = "linear"))

per <- per %>% add_lines(y = ~percentData$X1991, name = "1991", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1992, name = "1992", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1993, name = "1993", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1994, name = "1994", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1995, name = "1995", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1996, name = "1996", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1997, name = "1997", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1998, name = "1998", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1999, name = "1999", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2000, name = "2000", line = list(shape = "linear"))

per <- per %>% add_lines(y = ~percentData$X2001, name = "2001", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2002, name = "2002", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2003, name = "2003", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2004, name = "2004", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2005, name = "2005", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2006, name = "2006", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2007, name = "2007", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2008, name = "2008", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2009, name = "2009", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2010, name = "2010", line = list(shape = "linear"))

per <- per %>% add_lines(y = ~percentData$X2011, name = "2011", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2012, name = "2012", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2013, name = "2013", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2014, name = "2014", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2015, name = "2015", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2016, name = "2016", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2017, name = "2017", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2018, name = "2018", line = list(shape = "linear"))

#formatting
per <- per %>% layout(title = "Percent of Extent Occupied by < -1C Bottom Water (RASM)",
                      xaxis = list(title = "Months",
                                   showgrid = FALSE,
                                   showline = TRUE,
                                   showticklabels = TRUE,
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Percent",
                                   showgrid = FALSE,
                                   showline = TRUE,
                                   showticklabels = TRUE,
                                   ticks = 'outside',
                                   zeroline = FALSE))

per

#################Cold Pool Centroids & Fragmentation############################

#number of fragments
#total area of the cold pool
#size of the fragments -- take centroid of the largest one
#extract size of the area of the alrgest fragments
#cluster analysis of the centroids, size, and number of fragments
#ultimately compare to the velocity of currents and wind (for Wenjing and Jian)
#compare the krill (remember to thin & resample krill by year grouping)
#to the groupings of coldpool years

library(tidycensus)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(raster)
library(rasterVis)
library(purrr)
library(R.utils)
library(gridExtra)
library(stringr)
library(palr)
library(dplyr)
library(sp)
library(rgdal)
library(gdata)
library(rgeos)
library(usmap)
library(lwgeom)

setwd("D:/Cold Pool/coldPool/centroids")
path <- "D:/Cold Pool/coldPool/centroids/"


#set up list of shapes
shapes <- list.files("D:/Cold Pool/1980",pattern='*.shp',full.names=TRUE)
shape_list <- c("p1980.jan", "p1980.feb", "p1980.mar", "p1980.apr", "p1980.may", "p1980.jun", "p1980.jul", "p1980.aug", "p1980.sep", "p1980.oct", "p1980.nov", "p1980.dec")
color_list <- c("red", "orange", "yellow", "green", "blue", "purple", "pink", "grey", "brown", "cyan", "gold", "tan")

#read in 1980
for (i in seq_along(shapes)) {
  #read in shape
  shp <- readOGR(shapes[i])
  #plot shape
  plot(shp, col=color_list[i], main=shape_list[i])
  #check the CRS
  crs(shp)
  #rename the shape
  mv(from="shp", to=shape_list[i])
}

# Merging cells into shapes
a1980.jan <- gUnion(p1980.jan, p1980.jan, byid = FALSE)
a1980.feb <- gUnion(p1980.feb, p1980.feb, byid = FALSE)
a1980.mar <- gUnion(p1980.mar, p1980.mar, byid = FALSE)
a1980.apr <- gUnion(p1980.apr, p1980.apr, byid = FALSE)
a1980.may <- gUnion(p1980.may, p1980.may, byid = FALSE)
a1980.jun <- gUnion(p1980.jun, p1980.jun, byid = FALSE)
a1980.jul <- gUnion(p1980.jul, p1980.jul, byid = FALSE)
a1980.aug <- gUnion(p1980.aug, p1980.aug, byid = FALSE)
a1980.sep <- gUnion(p1980.sep, p1980.sep, byid = FALSE)
a1980.oct <- gUnion(p1980.oct, p1980.oct, byid = FALSE)
a1980.nov <- gUnion(p1980.nov, p1980.nov, byid = FALSE)
a1980.dec <- gUnion(p1980.dec, p1980.dec, byid = FALSE)

par(mfrow=c(3,4))

plot(a1980.jan, col="blue", main="Jan")
plot(a1980.feb, col="blue", main="Feb")
plot(a1980.mar, col="blue", main="Mar")
plot(a1980.apr, col="blue", main="Apr")
plot(a1980.may, col="blue", main="May")
plot(a1980.jun, col="blue", main="Jun")
plot(a1980.jul, col="blue", main="Jul")
plot(a1980.aug, col="blue", main="Aug")
plot(a1980.sep, col="blue", main="Sep")
plot(a1980.oct, col="blue", main="Oct")
plot(a1980.nov, col="blue", main="Nov")
plot(a1980.dec, col="blue", main="Dec")

#overlap all of them just to show that they maintain coordinates (ie visual of "crs(pool1)")
together <- gUnion(a1980.jan, a1980.feb, byid = FALSE)
together <- gUnion(together, a1980.mar, byid = FALSE)
together <- gUnion(together, a1980.apr, byid = FALSE)
together <- gUnion(together, a1980.may, byid = FALSE)
together <- gUnion(together, a1980.jun, byid = FALSE)
together <- gUnion(together, a1980.jul, byid = FALSE)
together <- gUnion(together, a1980.aug, byid = FALSE)
together <- gUnion(together, a1980.sep, byid = FALSE)
together <- gUnion(together, a1980.oct, byid = FALSE)
together <- gUnion(together, a1980.nov, byid = FALSE)
together <- gUnion(together, a1980.dec, byid = FALSE)

plot(together)
#save as shp
raster::shapefile(together, paste(path, "together.shp", sep=""), overwrite=TRUE)

#read in shp file 
together_shp <- st_read('together.shp')
#find centroid
together_cent <- gCentroid(as(together_shp, "Spatial"), byid = TRUE)
#plot centroid on shp
ggplot() + 
  geom_sf(data = together_shp, fill = 'white') +
  geom_sf(data = together_cent %>% st_as_sf, color = 'blue')

#save 1980 months as shps
raster::shapefile(a1980.jan, paste(path, "a1980jan.shp", sep=""), overwrite=TRUE)
raster::shapefile(a1980.feb, paste(path, "a1980feb.shp", sep=""), overwrite=TRUE)
raster::shapefile(a1980.mar, paste(path, "a1980mar.shp", sep=""), overwrite=TRUE)
raster::shapefile(a1980.apr, paste(path, "a1980apr.shp", sep=""), overwrite=TRUE)
raster::shapefile(a1980.may, paste(path, "a1980may.shp", sep=""), overwrite=TRUE)
raster::shapefile(a1980.jun, paste(path, "a1980jun.shp", sep=""), overwrite=TRUE)
raster::shapefile(a1980.jul, paste(path, "a1980jul.shp", sep=""), overwrite=TRUE)
raster::shapefile(a1980.aug, paste(path, "a1980aug.shp", sep=""), overwrite=TRUE)
raster::shapefile(a1980.sep, paste(path, "a1980sep.shp", sep=""), overwrite=TRUE)
raster::shapefile(a1980.oct, paste(path, "a1980oct.shp", sep=""), overwrite=TRUE)
raster::shapefile(a1980.nov, paste(path, "a1980nov.shp", sep=""), overwrite=TRUE)
raster::shapefile(a1980.dec, paste(path, "a1980dec.shp", sep=""), overwrite=TRUE)

#read in shp files 
s1980.jan <- st_read('a1980jan.shp')
s1980.feb <- st_read('a1980feb.shp')
s1980.mar <- st_read('a1980mar.shp')
s1980.apr <- st_read('a1980apr.shp')
s1980.may <- st_read('a1980may.shp')
s1980.jun <- st_read('a1980jun.shp')
s1980.jul <- st_read('a1980jul.shp')
s1980.aug <- st_read('a1980aug.shp')
s1980.sep <- st_read('a1980sep.shp')
s1980.oct <- st_read('a1980oct.shp')
s1980.nov <- st_read('a1980nov.shp')
s1980.dec <- st_read('a1980dec.shp')

#find centroids
cent1980.jan <- gCentroid(as(s1980.jan, "Spatial"), byid = TRUE)
cent1980.feb <- gCentroid(as(s1980.feb, "Spatial"), byid = TRUE)
cent1980.mar <- gCentroid(as(s1980.mar, "Spatial"), byid = TRUE)
cent1980.apr <- gCentroid(as(s1980.apr, "Spatial"), byid = TRUE)
cent1980.may <- gCentroid(as(s1980.may, "Spatial"), byid = TRUE)
cent1980.jun <- gCentroid(as(s1980.jun, "Spatial"), byid = TRUE)
cent1980.jul <- gCentroid(as(s1980.jul, "Spatial"), byid = TRUE)
cent1980.aug <- gCentroid(as(s1980.aug, "Spatial"), byid = TRUE)
cent1980.sep <- gCentroid(as(s1980.sep, "Spatial"), byid = TRUE)
cent1980.oct <- gCentroid(as(s1980.oct, "Spatial"), byid = TRUE)
cent1980.nov <- gCentroid(as(s1980.nov, "Spatial"), byid = TRUE)
cent1980.dec <- gCentroid(as(s1980.dec, "Spatial"), byid = TRUE)

#get coordinates
coords1980.jan <- coordinates(cent1980.jan)
coords1980.feb <- coordinates(cent1980.feb)
coords1980.mar <- coordinates(cent1980.mar)
coords1980.apr <- coordinates(cent1980.apr)
coords1980.may <- coordinates(cent1980.may)
coords1980.jun <- coordinates(cent1980.jun)
coords1980.jul <- coordinates(cent1980.jul)
coords1980.aug <- coordinates(cent1980.aug)
coords1980.sep <- coordinates(cent1980.sep)
coords1980.oct <- coordinates(cent1980.oct)
coords1980.nov <- coordinates(cent1980.nov)
coords1980.dec <- coordinates(cent1980.dec)

#make dataframe
cent1980 <- data.frame(month=c(seq(1,12,by=1)), x=c(coords1980.jan[1], coords1980.feb[1], coords1980.mar[1], coords1980.apr[1], coords1980.may[1], coords1980.jun[1], coords1980.jul[1], coords1980.aug[1], coords1980.sep[1], coords1980.oct[1], coords1980.nov[1], coords1980.dec[1]), y=c(coords1980.jan[2], coords1980.feb[2], coords1980.mar[2], coords1980.apr[2], coords1980.may[2], coords1980.jun[2], coords1980.jul[2], coords1980.aug[2], coords1980.sep[2], coords1980.oct[2], coords1980.nov[2], coords1980.dec[2]))
write.csv(cent1980, "D:/Cold Pool/coldPool/centroids/cent1980.csv", row.names=FALSE)

#plot centroid on shp
ggplot() + 
  geom_sf(data = s1980.jan, fill = 'white') +
  geom_sf(data = cent1980.jan %>% st_as_sf, color = 'blue')
ggplot() + 
  geom_sf(data = s1980.feb, fill = 'white') +
  geom_sf(data = cent1980.feb %>% st_as_sf, color = 'blue')
ggplot() + 
  geom_sf(data = s1980.mar, fill = 'white') +
  geom_sf(data = cent1980.mar %>% st_as_sf, color = 'blue')
ggplot() + 
  geom_sf(data = s1980.apr, fill = 'white') +
  geom_sf(data = cent1980.apr %>% st_as_sf, color = 'blue')
ggplot() + 
  geom_sf(data = s1980.may, fill = 'white') +
  geom_sf(data = cent1980.may %>% st_as_sf, color = 'blue')
ggplot() + 
  geom_sf(data = s1980.jun, fill = 'white') +
  geom_sf(data = cent1980.jun %>% st_as_sf, color = 'blue')
ggplot() + 
  geom_sf(data = s1980.jul, fill = 'white') +
  geom_sf(data = cent1980.jul %>% st_as_sf, color = 'blue')
ggplot() + 
  geom_sf(data = s1980.aug, fill = 'white') +
  geom_sf(data = cent1980.aug %>% st_as_sf, color = 'blue')
ggplot() + 
  geom_sf(data = s1980.sep, fill = 'white') +
  geom_sf(data = cent1980.sep %>% st_as_sf, color = 'blue')
ggplot() + 
  geom_sf(data = s1980.oct, fill = 'white') +
  geom_sf(data = cent1980.oct %>% st_as_sf, color = 'blue')
ggplot() + 
  geom_sf(data = s1980.nov, fill = 'white') +
  geom_sf(data = cent1980.nov %>% st_as_sf, color = 'blue')
ggplot() + 
  geom_sf(data = s1980.dec, fill = 'white') +
  geom_sf(data = cent1980.dec %>% st_as_sf, color = 'blue')

#calculate perimeters
per1980.jan <- st_perimeter(s1980.jan)
per1980.feb <- st_perimeter(s1980.feb)
per1980.mar <- st_perimeter(s1980.mar)
per1980.apr <- st_perimeter(s1980.apr)
per1980.may <- st_perimeter(s1980.may)
per1980.jun <- st_perimeter(s1980.jun)
per1980.jul <- st_perimeter(s1980.jul)
per1980.aug <- st_perimeter(s1980.aug)
per1980.sep <- st_perimeter(s1980.sep)
per1980.oct <- st_perimeter(s1980.oct)
per1980.nov <- st_perimeter(s1980.nov)
per1980.dec <- st_perimeter(s1980.dec)

#calculate area
area1980.jan <- area(a1980.jan)
area1980.feb <- area(a1980.feb)
area1980.mar <- area(a1980.mar)
area1980.apr <- area(a1980.apr)
area1980.may <- area(a1980.may)
area1980.jun <- area(a1980.jun)
area1980.jul <- area(a1980.jul)
area1980.aug <- area(a1980.aug)
area1980.sep <- area(a1980.sep)
area1980.oct <- area(a1980.oct)
area1980.nov <- area(a1980.nov)
area1980.dec <- area(a1980.dec)

#calculate index of fragmentation
frag1980.jan <- as.numeric(per1980.jan/area1980.jan)
frag1980.feb <- as.numeric(per1980.feb/area1980.feb)
frag1980.mar <- as.numeric(per1980.mar/area1980.mar)
frag1980.apr <- as.numeric(per1980.apr/area1980.apr)
frag1980.may <- as.numeric(per1980.may/area1980.may)
frag1980.jun <- as.numeric(per1980.jun/area1980.jun)
frag1980.jul <- as.numeric(per1980.jul/area1980.jul)
frag1980.aug <- as.numeric(per1980.aug/area1980.aug)
frag1980.sep <- as.numeric(per1980.sep/area1980.sep)
frag1980.oct <- as.numeric(per1980.oct/area1980.oct)
frag1980.nov <- as.numeric(per1980.nov/area1980.nov)
frag1980.dec <- as.numeric(per1980.dec/area1980.dec)

#make dataframe
frag1980 <- data.frame(month=c(seq(1,12,by=1)), frag=c(frag1980.jan, frag1980.feb, frag1980.mar, frag1980.apr, frag1980.may, frag1980.jun, frag1980.jul, frag1980.aug, frag1980.sep, frag1980.oct, frag1980.nov, frag1980.dec))
write.csv(frag1980, "D:/Cold Pool/coldPool/centroids/frag1980.csv", row.names=FALSE)

#########################Cold Pool Space-State Model###########################

library(dlm)
library(tsm)

#local level
rm(list = ls())
graphics.off()

area <- read.csv("D:/Cold Pool/areaSummary.csv")
percent <- read.csv("D:/Cold Pool/percentSummary.csv")

#load in the area data
a <- c(area$X1980,area$X1981,area$X1982,area$X1983,area$X1984, area$X1985,
       area$X1986, area$X1987, area$X1988, area$X1989, area$X1990, area$X1991,
       area$X1992, area$X1993, area$X1994, area$X1995, area$X1996, area$X1997,
       area$X1998, area$X1999, area$X2000, area$X2001, area$X2002, area$X2003,
       area$X2004, area$X2005, area$X2006, area$X2007, area$X2008, area$X2009,
       area$X2010, area$X2011, area$X2012, area$X2013, area$X2014, area$X2015,
       area$X2016, area$X2017, area$X2018)

#load in the percent extent data
p <- c(percent$X1980,percent$X1981,percent$X1982,percent$X1983,percent$X1984, percent$X1985,
       percent$X1986, percent$X1987, percent$X1988, percent$X1989, percent$X1990, percent$X1991,
       percent$X1992, percent$X1993, percent$X1994, percent$X1995, percent$X1996, percent$X1997,
       percent$X1998, percent$X1999, percent$X2000, percent$X2001, percent$X2002, percent$X2003,
       percent$X2004, percent$X2005, percent$X2006, percent$X2007, percent$X2008, percent$X2009,
       percent$X2010, percent$X2011, percent$X2012, percent$X2013, percent$X2014, percent$X2015,
       percent$X2016, percent$X2017, percent$X2018)

#create matrices of both
z <- matrix(a)
w <- matrix(p)

#count up by months starting at 1980
Area <- ts(z, start = c(1980, 1), frequency = 12)

plot.ts(Area)

#define the function
fn <- function(parm) {
  dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2]))
}


#define the fit
fit <- dlmMLE(Area, rep(0, 2), build = fn, hessian = TRUE)
(conv <- fit$convergence)

#create loglikelihood
loglik <- dlmLL(Area, dlmModPoly(1))
n.coef <- 2
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef))  #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(Area))) * (n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- W(mod)

filtered <- dlmFilter(Area, mod = mod)
smoothed <- dlmSmooth(filtered)

#get the residuals
resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s)
mu.1 <- mu[1]
mu.end <- mu[length(mu)]

#plot results
par(mfrow = c(2, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(Area, col = "darkgrey", xlab = "", ylab = "", lwd = 1.5)
lines(mu, col = "black")
legend("topright", legend = c("Observed Deflator", "Stochastic level"), 
       lwd = c(2, 1), col = c("darkgrey", "black"), bty = "n")

###############################################################################

#seasonal and local level
Area <- ts(z, start = c(1980, 1), frequency = 12)

plot.ts(Area)

#get function
fn <- function(parm) {
  mod <- dlmModPoly(order = 1) + dlmModSeas(frequency = 12)
  V(mod) <- exp(parm[1])
  diag(W(mod))[1:2] <- exp(parm[2:3])
  return(mod)
}

#assign fit
fit <- dlmMLE(Area, rep(0, 3), build = fn, hessian = TRUE)
conv <- fit$convergence  # zero for converged

#log likelihood
loglik <- dlmLL(Area, dlmModPoly(1) + dlmModSeas(4))
n.coef <- 3
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef))  #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(Area))) * (n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- diag(W(mod))

#apply filter
filtered <- dlmFilter(Area, mod = mod)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s[, 1])
gammas <- dropFirst(smoothed$s[, 2])
mu.1 <- mu[1]
mu.end <- mu[length(mu)]
gammas.1 <- gammas[1]
gammas.end <- gammas[length(mu)]

#plot the results
par(mfrow = c(3, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(Area, col = "darkgrey", xlab = "", ylab = "", lwd = 2)
lines(mu, col = "black")
legend("topright", legend = c("Observed Deflator", "Stochastic level"), 
       lwd = c(2, 1), col = c("darkgrey", "black"), bty = "n")

plot.ts(gammas, col = "darkgrey", xlab = "", ylab = "", 
        lwd = 2)
legend("topright", legend = "Seasonal", lwd = 2, col = "darkgrey", 
       bty = "n")

plot.ts(resids, ylab = "", xlab = "", col = "darkgrey", 
        lwd = 2)
abline(h = 0)
legend("topright", legend = "Residuals", lwd = 2, col = "darkgrey", 
       bty = "n")

#combine to check what's influencing the model
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
alpha <- mu + gammas
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(Area, col = "darkgrey", xlab = "", ylab = "", lwd = 2)
lines(alpha, col = "black")
legend("topright", legend = c("Observed Deflator", "State Components"), 
       lwd = c(2, 1), col = c("darkgrey", "black"), bty = "n")

#check the AIC and BIC etc
cat("AIC", r.aic)

cat("BIC", r.bic)

cat("V.variance", obs.error.var)

cat("W.variance", state.error.var)

ac(resids)  # acf

#check for joint autocorrelation
Box.test(resids, lag = 12, type = "Ljung", fitdf = 2)  # joint autocorrelation

shapiro.test(resids)  # normality

#plot confidence intervals--NOT WORKING YET!
plot.ts(Area)

conf.tmp <- unlist(dlmSvd2var(smoothed$U.S, smoothed$D.S))
conf <- ts(as.numeric(conf.tmp)[-1], start = c(1960, 2), 
           frequency = 4)
wid <- qnorm(0.05, lower = FALSE) * sqrt(conf)

conf.pos <- mu + wid
conf.neg <- mu - wid

mu.f <- dropFirst(filtered$a)
cov.tmp <- unlist(dlmSvd2var(filtered$U.R, filtered$D.R))
if (sum(dim(mod$FF)) == 2) {
  variance <- cov.tmp + as.numeric(V(mod))
} else {
  variance <- (sapply(cov.tmp, function(x) mod$FF %*% 
                        x %*% t(mod$FF))) + V(mod)
}

#final plot of results
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(Area, col = "darkgrey", xlab = "", ylab = "", lwd = 1.5)
lines(mu, col = "black")
lines(conf.pos, col = "red")
lines(conf.neg, col = "red")
legend("topright", legend = c("Observed Deflator", "Stochastic level", 
                              "Confidence Interval"), lwd = c(1.5, 1, 1), col = c("darkgrey", 
                                                                                  "black", "red"), bty = "n")

####################################Static Wind Plot###########################

# libraries we need
libs <- c(
  "tidyverse", "sf", "giscoR",
  "lubridate", "classInt",
  "rWind", "metR", "oce"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))


# 1. GET WIND DATA
#----------------------

get_wind_data <- function(time_range, mean_wind_data, eur_wind_df) {
  
  time_range <- seq(ymd_hms(paste(2022, 8, 27, 00, 00, 00, sep = "-")),
                    ymd_hms(paste(2022, 8, 28, 00, 00, 00, sep = "-")),
                    by = "1 hours"
  )
  
  mean_wind_data <- rWind::wind.dl_2(time_range, -179, -152, 50.7, 63) %>%
    rWind::wind.mean()
  
  eur_wind_df <- as.data.frame(mean_wind_data)
  return(eur_wind_df)
}

#call the function
eur_wind_df <- get_wind_data()



get_wind_points <- function() {
  # convert wind data into points
  eur_wind_pts <- eur_wind_df %>%
    sf::st_as_sf(coords = c("lon", "lat")) %>%
    sf::st_set_crs(4326)
  
  return(eur_wind_pts)
}

eur_wind_pts <- get_wind_points()



get_wind_grid <- function() {
  eur_wind_grid <- eur_wind_pts %>%
    sf::st_make_grid(n = c(80, 100)) %>%
    sf::st_sf() %>%
    dplyr::mutate(id = row_number())
  
  return(eur_wind_grid)
}

eur_wind_grid <- get_wind_grid()




get_wind_grid_aggregated <- function() {
  
  eur_wind_grid_agg <- 
    sf::st_join(eur_wind_pts, eur_wind_grid, 
                join = sf::st_within) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      n = n(), u = mean(ugrd10m), 
      v = mean(vgrd10m), speed = mean(speed)
    ) %>%
    dplyr::inner_join(eur_wind_grid, by="id") %>%
    dplyr::select(n, u, v, speed, geometry) %>%
    sf::st_as_sf() %>%
    na.omit()
  
  return(eur_wind_grid_agg)
}

eur_wind_grid_agg <- get_wind_grid_aggregated()




get_wind_coords <- function() {
  coords <- eur_wind_grid_agg %>%
    st_centroid() %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(lon = X, lat = Y)
  return(coords)
}

coords <- get_wind_coords()





get_wind_df <- function() {
  eur_df <- coords %>%
    bind_cols(sf::st_drop_geometry(eur_wind_grid_agg))
  return(eur_df)
}

eur_df <- get_wind_df()




## interpolate the U component
get_u_interpolation <- function() {
  wu <- oce::interpBarnes(
    x = eur_df$lon,
    y = eur_df$lat,
    z = eur_df$u
  )
  return(wu)
}

wu <- get_u_interpolation()





dimension <- data.frame(lon = wu$xg, wu$zg) %>% dim()



## make a U component data table from interpolated matrix
get_u_table <- function() {
  udf <- data.frame(
    lon = wu$xg,
    wu$zg
  ) %>%
    gather(key = "lata", value = "u", 2:dimension[2]) %>%
    mutate(lat = rep(wu$yg, each = dimension[1])) %>%
    select(lon, lat, u) %>%
    as_tibble()
  
  return(udf)
}

udf <- get_u_table()



## interpolate the V component
get_u_interpolation <- function() {
  wv <- oce::interpBarnes(
    x = eur_df$lon,
    y = eur_df$lat,
    z = eur_df$v
  )
  return(wv)
}

wv <- get_u_interpolation()

## make the V component data table from interpolated matrix
get_v_table <- function() {
  vdf <- data.frame(lon = wv$xg, wv$zg) %>%
    gather(key = "lata", value = "v", 2:dimension[2]) %>%
    mutate(lat = rep(wv$yg, each = dimension[1])) %>%
    select(lon, lat, v) %>%
    as_tibble()
  return(vdf)
}

vdf <- get_v_table()




## merge the V and U component tables and compute velocity
get_final_table <- function() {
  df <- udf %>%
    bind_cols(vdf %>% select(v)) %>%
    mutate(vel = sqrt(u^2 + v^2))
  return(df)
}

df <- get_final_table()






head(df)




# 5. MAP WIND DATA
#-------------------------

get_europe_sf <- function() {
  eur_sf <- giscoR::gisco_get_countries(
    year = "2016", epsg = "4326",
    resolution = "10", region = c("United States")
  )
  
  return(eur_sf)
}

# bounding box
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_bounding_box <- function(bbox, bb) {
  bbox <- st_sfc(
    st_polygon(list(cbind(
      c(-179, -152, -152, -179, -179),
      c(50.7, 50.7, 63, 63, 50.7)
    ))),
    
    crs = crsLONGLAT
  )
  
  bb <- sf::st_bbox(bbox)
  
  return(bb)
}



# colors
cols <- c(
  '#FDFDD7', '#C5E8B2', '#3EB6C3', '#253493'
)

newcol <- colorRampPalette(cols)
ncols <- 6
cols2 <- newcol(ncols)

# breaks
vmin <- min(df$vel, na.rm = T)
vmax <- max(df$vel, na.rm = T)

brk <- classInt::classIntervals(df$vel,
                                n = 6,
                                style = "fisher"
)$brks %>%
  head(-1) %>%
  tail(-1) %>%
  append(vmax)

breaks <- c(vmin, brk)



make_wind_map <- function(eur_sf, bb) {
  eur_sf <- get_europe_sf()
  bb <- get_bounding_box()
  
  p <- df %>%
    ggplot() +
    metR::geom_streamline(
      data = df,
      aes(
        x = lon, y = lat, dx = u, dy = v,
        color = sqrt(..dx..^2 + ..dy..^2)
      ),
      L = 2, res = 2, n = 60,
      arrow = NULL, lineend = "round",
      alpha = .85
    ) +
    geom_sf(
      data = eur_sf,
      fill = NA,
      color = "#07CFF7",
      size = .25,
      alpha = .99
    ) +
    coord_sf(
      crs = crsLONGLAT,
      xlim = c(bb["xmin"], bb["xmax"]),
      ylim = c(bb["ymin"], bb["ymax"])
    ) +
    scale_color_gradientn(
      name = "Average speed (m/s)",
      colours = cols2,
      breaks = breaks,
      labels = round(breaks, 1),
      limits = c(vmin, vmax)
    ) +
    #guides(
    #fill = "none",
    #color = guide_legend(
    #override.aes = list(size = 3, alpha = 1, shape = 15),
    #direction = "horizontal",
    #keyheight = unit(2.5, units = "mm"),
    #keywidth = unit(15, units = "mm"),
    #title.position = "top",
    #title.hjust = .5,
    #label.hjust = .5,
    #nrow = 1,
  #byrow = T,
  #reverse = F,
  #label.position = "bottom"
  #)
  #) +
  theme_bw() +
    theme(
      #text = element_text(family = "Times"),
      strip.background = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      #legend.text = element_blank(),
      legend.position ="none",
      #legend.text = element_text(size = 10, color = "white"),
      #legend.title = element_text(size = 15, color = "white"),
      #legend.key = element_blank(),
      #plot.margin = unit(c(t = 1, r = -2, b = -1, l = -2), "lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      #plot.title = element_text(
      #face = "bold", size = 20,
      #color = "white", hjust = .5
      #),
      #plot.caption = element_text(
      #size = 10, color = "grey80",
      #hjust = .25, vjust = -10
      #),
      #plot.subtitle = element_blank(),
      plot.background = element_rect(fill = "#070C33", color = NA),
      panel.background = element_rect(fill = "#070C33", color = NA),
      #legend.background = element_rect(fill = "#070C33", color = NA),
      #legend.spacing.y = unit(.5, "pt"),
      #panel.border = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      #title = "Average wind speed",
      #subtitle = "",
      #caption = ""
    )
  return(p)
}

p <- make_wind_map()

ggsave(
  filename = "beringSeaUS_wind_27august2022.png",
  width = 8.5, height = 7, dpi = 600, device = "png", p
)

