###################################################
## R Recitation Animal Movement tracking
###################################################
# This is example code for calculating distance, 
# speed and bearing between track positions in R.

#set working directory
setwd("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/Week13/R recitation")

# install.packages("argosfilter")
# install.packages("adehabitatLT")
# install.packages("maptools")
# install.packages("rgeos")
# install.packages("rgdal")
library("maptools")
library(rgeos)
library(rgdal)
library(argosfilter)
library(adehabitatLT)
#suppressWarnings(RNGversion("3.5.0"))

#####################################################
# Look at some example CRW changing the correlation between steps
#####################################################
# Simulation of a Correlated Random Walk
set.seed(876)
?simm.crw
#change only r, 
# r is the concentration parameter for wrapped normal 
# distribution of turning angles
u <- simm.crw(1:500, r = 0.99, burst = "r = 0.99", h = 4)
v <- simm.crw(1:500, r = 0.9, burst = "r = 0.9", h = 4)
w <- simm.crw(1:500, r = 0.6, burst = "r = 0.6", h = 4)
x <- simm.crw(1:500, r = 0, burst = "r = 0 (Uncorrelated \nrandom walk)",
              h = 4)
z <- c(u, v, w, x)
plot(z, addpoints = FALSE, perani = FALSE)

#change only h, the scaling parameter for the movement length
u <- simm.crw(1:500, r = 0.6, burst = "h = 2", h = 2)
v <- simm.crw(1:500, r = 0.6, burst = "h = 5", h = 7)
w <- simm.crw(1:500, r = 0.6, burst = "h = 7", h = 10)
x <- simm.crw(1:500, r = 0.6, burst = "h = 10", h = 15)
z <- c(u, v, w, x)
plot(z, addpoints = FALSE, perani = FALSE)

#change both h and r
u <- simm.crw(1:500, r = 0.99, burst = "r = 0.99")
v <- simm.crw(1:500, r = 0.9, burst = "r = 0.9", h = 2)
w <- simm.crw(1:500, r = 0.6, burst = "r = 0.6", h = 5)
x <- simm.crw(1:500, r = 0, burst = "r = 0 (Uncorrelated \nrandom walk)",
              h = 0.1)
z <- c(u, v, w, x)
plot(z, addpoints = FALSE, perani = FALSE)

#Simulates a Levy Walk
set.seed(411)
w <- simm.levy(1:500, mu = 1.5)
u <- simm.levy(1:500, mu = 2)
v <- simm.levy(1:500, mu = 2.5)
x <- simm.levy(1:500, mu = 3)

par(mfrow=c(2,2))
plot(w, perani = FALSE, main= "mu = 1.5",xlab="",ylab="")
plot(u, perani = FALSE, main= "mu = 2",xlab="",ylab="")
plot(v, perani = FALSE, main= "mu = 2.5",xlab="",ylab="")
plot(x, perani = FALSE, main= "mu = 3",xlab="",ylab="")

dev.off()

#####################################################
#1. Work with real tracks of Leatherback Turtles
#####################################################
# Open track daily positions data file in R. 
# Don't forger to set the working directory to wherever the file is stored 
t<-read.csv("LeatherbackTurtle_Example_41697.csv",header=T,sep=",")
names(t)

# Plot track on a map
## Note: Adjust x and y limits if track gets cut-off in image
#maptools settings
CRy<-c(-35,20)
CRx<-c(220,310)
m<-system.file("share/gshhs_c.b",package="maptools")
CR<-Rgshhs(m,xlim=CRx,ylim=CRy,level=1)
#start with Pacific coast of Central and South America
plot(CR$SP,col="grey",xaxs="i",yaxs="i",axes=TRUE,las=1)
#add the turtle tracks
points((t$long+360),t$lat,pch=20,cex=0.5,col="red")
points((t$long[1]+360),t$lat[1],pch=20,cex=1,col="blue")
legend("bottomleft",legend=c("Leatherback #41697"),pch=20, col="red")

#save this plot to a file
## Note: Change width and height if needed for map to fill page. 
## Change legend name to ID number.
png(filename = "LeatherbackTurtle_41697_map.png",  width = 530, height = 580, units = "px", pointsize = 14, bg = "white")
plot(CR$SP,col="grey",xaxs="i",yaxs="i",axes=TRUE,las=1)
points((t$long+360),t$lat,pch=20,cex=0.5,col="red")
points((t$long[1]+360),t$lat[1],pch=20,cex=1,col="blue")
legend("bottomleft",legend=c("Leatherback #41697"),pch=20, col="red")
dev.off()

#####################################################
#2. Calculate distances between the daily positions
#####################################################
# Calculate distances between the daily positions in units of km 
# There are several packages in R that will calculate distances 
# between geographic coordinates (e.g. adehabitatLT). 
# I used the package "argosfilter" here just as an example. 
# Need to assign the last position a distance of zero 
# since there are no further positions.
# The 'for' loop calculates the distance between consecutive points.
l<-length(t$long)
t$dist<-numeric(l)
for (i in 1:(l-1)) {
t$dist[i]<-distance(t$lat[i],t$lat[i+1],t$lon[i],t$lon[i+1])
}
t$dist[l]<-0
summary(t$dist)

# plot step length histogram on log scale
# can consider all step lengths >1 (there are no zero steps)
# Add 1 to the frequencies so that the log isn't infinite for occasions 
# where a bin has a frequency of 0.
# Change plot filename to appropriate ID number.
h<-hist(t$dist, breaks=12,xlim=c(0,100),xlab="Daily distance (km)",
        main="Histogram of step lengths",las=1)
h
step<-h$breaks[h$breaks>1]
freq<-h$counts+1
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b")

#save this plot to a file
png(filename = "Tag41697_StepLengthDistribution.png",  
    width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b",las=1)
dev.off()

#####################################################
#3. Determine whether distribution of step lengths resembles a 
#   Levy flight model distribution.
#####################################################
# Estimate the Levy parameter mu from a regression model 
# (indicated by 'lm' for linear model) of 
# log frequency versus log step length.
model<-lm(log10(freq)~log10(step))
summary(model)
# For the example track #41697, the log10(step) coefficient is -1.6229. 
# This gives a Mu value of 1.62, which is consistent with a Levy flight
#(absolute value between 1 and 3).
# Reminder of the range of Mu values for Levy Flight? 1-3

#####################################################
#4. Calculate speed for the track
#####################################################
# Calculate speed as distance divided by time (km/h), 
# where the time interval between positions is a day, i.e. 24 hours.
t$speed<-t$dist/24
summary(t$speed)
summary(t$speed[t$speed>0]) #where speed is >0
## The median speed is 0.64 km/h with a maximum speed of 3.85 km/h.

#####################################################
#5. Plot the net squared displacement of the track and 
#   the expected net squared displacement for a CRW and BRW
#   using the step lengths, turning angles, and bearing from 
#   each track's distribution
#####################################################
# Can use various functions in adehabitatLT package after creating an 
# object of class ltraj, which requires formatting the date and converting 
# lat/long to x/y coordinates in km using distance and bearing trigonometry.
# as.POSIXct is a date-time conversion function. The format can be adjusted 
# as needed depending on your dataset (see help for 'strptime').

# We have already calculated the distances (step lengths).
# In order to convert the positions from lat/long to x/y coordinates we need 
# to know the distance and angle between consecutive points. 
# We are calculating the angles (bearings) in the variable 'b'. 
# The function in the argosfilter gives values -180 to +180 so in the 'for' loop 
# we convert to a 0-360 range by adding 360 to the negative values. 
# We then convert the values in degrees to radians.
# The x/y coordinates are calculated using the start position as (0,0) and 
# then using the sequence of distances and angles using trigonometry to 
# estimate the x/y coordinates of subsequent positions.

da<-as.POSIXct(strptime(t$date,format="%m/%d/%y"))

library(argosfilter)
b<-bearingTrack(t$lat,t$long)
lb<-length(b)
l<-length(t$dist)
dist<-t$dist[t$dist>0]
for (i in 1:lb){
  if(b[i]>0) {
    b[i]<-b[i]
  } else {
    b[i]<-360+b[i]
  }
}
br<-radian(b)
x<-numeric(l)
y<-numeric(l)
x[1]<-0
y[1]<-0
for (i in 2:l) {
  x[i]<-(dist[i-1]*sin(br[i-1]))+x[i-1]
  y[i]<-(dist[i-1]*cos(br[i-1]))+y[i-1]
}


# Create object of class 'ltraj' so adehabitatLT package can read 
# data file and calculate movement metrics.
xy<-data.frame(x,y)
path1<-as.ltraj(xy,date=da,id="T41697") #change id for turtle
summary.ltraj(path1)

# 'head' returns the first part of an object so you can see the columns 
# and variables within the ltraj created by the 'as.ltraj' function.
head(path1[[1]])

# calculate observed net squared displacement
# "R2n" is the net squared displacement and "turning" is the 
# turning angle (in radians). 
R2n<-path1[[1]][1:l,8]
turning<-path1[[1]][1:l,10]
summary(R2n)
summary(turning)

# Calculate expected net squared displacement for CRW model
# The variable 'm1' is the mean step length and 'm2' is the 
# mean squared step length.
m1<-mean(t$dist)
m2<-mean((t$dist)^2)

# The variable 'c' is the mean of the cosines of the turning angle.
c<-mean(cos(turning),na.rm=TRUE)
CRW<-numeric(l)
CRW[1]<-0
CRW[l]<-0

# The variable 'move' gives the move number where positions along the 
# track were numbered sequentially.
move<-seq(from=0,to=l,by=1)

# The 'for' loop calculates the expected net squared displacement 
# for a CRW model given the mean step length and turning angles calculated 
# from the track.
for (i in 2:l-1) {
  CRW[i]<-move[i]*m2+(2*(m1)^2)*(c/(1-c))*(move[i]-((1-c^move[i])/(1-c)))
}

summary(CRW)

# Calculate expected net squared displacement for BRW model
# Used bearing "b" because the absolute move direction because those calculated 
# from ltraj didn't seem to be accurate.
brad<-b*pi/180

# variable 'theta' is the mean of the cosine of the absolute move (compass) direction.
theta<-mean(cos(brad),na.rm=TRUE)

BRW<-numeric(l)
BRW[1]<-0
BRW[l]<-0

# The 'for' loop calculates the expected net squared displacement for a BRW model 
# given the mean step length and compass direction calculated from the track.
for (i in 2:l-1) {
  BRW[i]<-move[i]*m2+move[i]*(move[i]-1)*(m1^2)*theta^2
}

summary(BRW) #biased random walk model summary

# Plot actual track net squared displacement and lines for the expected 
# net squared displacements for CRW and BRW models.
# The track net squared displacement will be close to those for a 
# CRW or BRW if following those random walk models. If the track NSD is below 
# these lines, the turtle is exhibiting more residential behavior, and if it is 
# above the lines it indicates more directed behavior.
# Change ylim and legend title as needed.

plot(move[move>0],(R2n/100000),ylim=c(0,150),xlab="Number of steps",ylab="Net squared displacement (x 100,000 km)",type="l",las=1)
lines(move[1:l-1],(CRW[1:l-1]/100000),lty=2)
lines(move[1:l-1],(BRW[1:l-1]/100000),lty=2,col="grey")
legend("topleft",legend=c("CRW","BRW"),title=c("Track 41697"),lty=2,col=c("black","grey"),cex=0.8)
                                                
png(filename = "Tag41697_NetSquaredDisplacement.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
plot(move[move>0],(R2n/100000),ylim=c(0,150),xlab="Number of steps",ylab="Net squared displacement (x 100,000 km)",type="l",las=1)
lines(move[1:l-1],(CRW[1:l-1]/100000),lty=2)
lines(move[1:l-1],(BRW[1:l-1]/100000),lty=2,col="grey")
legend("topleft",legend=c("CRW","BRW"),title=c("Track 41697"),lty=2,col=c("black","grey"),cex=0.8)
dev.off()




# FIRST PASSAGE TIME
# Not in the HW
# Create positions at regular distance intervals (r=interval in km)
# The function 'redisltraj' creates a trajectory with regular step length 
# or duration. Here we give the new step length as 10km so positions will be 
# created every 10km along the track. The 'nnew' parameter specifies the maximum 
# ratio between the number of relocations compared to the original trajectory 
# (i.e. if nnew=10, it limits the new track to 10 times the number of positions 
# as the original track)
# The term 'n' gives a sequence of values specified as 
# from = 10, to = 2 x max # moves, by =  10 km. 
# The 'fpt' function calculates the first-passage time along the track. 
# You specify the track (here 'tr' as the version with positions at regular 
# distance intervals), circle radii, and the time units of the first-passage 
# time results.
# The 'meanfpt' function gives the mean first-passage time.
# The 'varlogfpt' function gives the variance of the log (base 10) of the first-passage time for each circle radius.

r<-10
tr<-redisltraj(path1,r,nnew=10)
plot.ltraj(tr)

moves<-t$dist[t$dist>0]
max<-max(moves)
n<-seq(10,2*max,10)

f<-fpt(tr, n, units=c("days"))
meanfpt(f,graph=TRUE)
varlogfpt(f,graph=TRUE)

png(filename = "Tag41697_VarianceLogFPT.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
varlogfpt(f,graph=TRUE)
dev.off()


# Spatial scale at peak variance of log(FPT) and time of this peak in plot.
# The 'for' loop is to create a variable with the variance of the log(FPT), which is then combined with the corresponding circle radius size ('n') in a dataframe.
# Use 'max' function to identify the maximum variance of the lof(FPT) and the circle radius size ('sc') at which it occurs to identify the spatial scale of the intensively used area(s).

vf<-varlogfpt(f,graph=TRUE)
vf
lf<-length(vf)
vf2<-numeric(lf)
for (i in 1:lf) {
  vf2[i]<-vf[,i]
}
vf3<-data.frame(n,vf2)
vf3

m<-max(vf3$vf2,na.rm=TRUE)
m
sc<-vf3$n[vf3$vf2==m]
sc
plot(f,scale=sc)

png(filename = "Tag41697_FPTvTime.png",  width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
plot(f,scale=sc)
dev.off()



