#load packages
library(raster)
library(dismo)

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
  write.log.file = TRUE,
  log.file = "spatial_thin_log.txt",
  verbose = TRUE
)

thinned9500 <- data.frame(thin9500[[1]])

library(usmap)
alaska <- plot_usmap(regions=c("state"), include=c("AK"), linewidth=1, labels = FALSE)

#transform data
transk9500 <- usmap_transform(k9500, input_names = c("LON", "LAT"), output_names = c("x", "y") )
trans9500 <- usmap_transform(thinned9500, input_names = c("Longitude", "Latitude"), output_names = c("x", "y") )


#plot with transformed data
alaska + geom_point(data=transk9500, aes(x=x, y=y, colour="raw"), size=1.5, alpha=I(0.5)) +
  geom_point(data=trans9500, aes(x=x, y=y, colour="thin"), size=1.5, alpha=I(0.5))

#plot side-by-side
par(mfrow=c(1,2))
raw9500 <- alaska + geom_point(data=transk9500, aes(x=x, y=y, colour="raw"), size=1.5, alpha=I(0.5)) + labs(title = "Raw")
thin9500 <- alaska + geom_point(data=trans9500, aes(x=x, y=y, colour="thin"), size=1.5, alpha=I(0.5)) + labs(title = "Thinned")
par(mfrow=c(1,2))
raw9500
thin9500

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

###############################################################################

#test for normality
par(mfrow=c(1,1))

#x coordinate (long)
#thinned
qqnorm(trans9500$x, pch = 1, frame = FALSE)
qqline(trans9500$x, col = "steelblue", lwd = 2)

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans9500$x)

#original
qqnorm(transk9500$x, pch = 1, frame = FALSE)
qqline(transk9500$x, col = "steelblue", lwd = 2)

#y coordinate (lat)
#thinned
qqnorm(trans9500$y, pch = 1, frame = FALSE)
qqline(trans9500$y, col = "steelblue", lwd = 2)

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans9500$y)

#original
qqnorm(transk9500$y, pch = 1, frame = FALSE)
qqline(transk9500$y, col = "steelblue", lwd = 2)

##############################################################################

#x coordinate (long)
#thinned
qqnorm(trans0106$x, pch = 1, frame = FALSE)
qqline(trans0106$x, col = "steelblue", lwd = 2)

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans0106$x)

#original
qqnorm(transk0106$x, pch = 1, frame = FALSE)
qqline(transk0106$x, col = "steelblue", lwd = 2)

#y coordinate (lat)
#thinned
qqnorm(trans0106$y, pch = 1, frame = FALSE)
qqline(trans0106$y, col = "steelblue", lwd = 2)

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans0106$y)

#original
qqnorm(transk0106$y, pch = 1, frame = FALSE)
qqline(transk0106$y, col = "steelblue", lwd = 2)

##############################################################################

#x coordinate (long)
#thinned
qqnorm(trans0712$x, pch = 1, frame = FALSE)
qqline(trans0712$x, col = "steelblue", lwd = 2)

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans0712$x)

#original
qqnorm(transk0712$x, pch = 1, frame = FALSE)
qqline(transk0712$x, col = "steelblue", lwd = 2)

#y coordinate (lat)
#thinned
qqnorm(trans0712$y, pch = 1, frame = FALSE)
qqline(trans0712$y, col = "steelblue", lwd = 2)

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans0712$y)

#original
qqnorm(transk0712$y, pch = 1, frame = FALSE)
qqline(transk0712$y, col = "steelblue", lwd = 2)

##############################################################################

#x coordinate (long)
#thinned
qqnorm(trans1318$x, pch = 1, frame = FALSE)
qqline(trans1318$x, col = "steelblue", lwd = 2)

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans1318$x)

#original
qqnorm(transk1318$x, pch = 1, frame = FALSE)
qqline(transk1318$x, col = "steelblue", lwd = 2)

#y coordinate (lat)
#thinned
qqnorm(trans1318$y, pch = 1, frame = FALSE)
qqline(trans1318$y, col = "steelblue", lwd = 2)

#Shapiro-Wilk’s method (>0.05 is normal)
shapiro.test(trans1318$y)

#original
qqnorm(transk1318$y, pch = 1, frame = FALSE)
qqline(transk1318$y, col = "steelblue", lwd = 2)
