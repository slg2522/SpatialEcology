##Homework 7 – Analyzing animal movements
##Handout 21 November 2022
##Due Friday 5 December 2022
##Adult female leatherback turtles were Argos satellite tagged off Playa Grande,
##Costa Rica, in the Pacific Ocean. These tracks have been filtered to remove
##spurious locations and regularized to give daily positions as latitudes and
##longitudes (decimal degrees). In  the Week 13 homework folder on the Google
##drive you will find these daily position data files for each of five tracked
##leatherback turtles (ID 1, 2, 6, 7, and 8) named in the format
##“LeatherbackTurtle_ID1_DataForStudents.csv”. Use these data to complete the
##following tasks and questions for each individual tracked turtle:
  
##1. Map the track of each leatherback turtle.
##It can be five separate maps for each individual or all five tracks plotted
##on one map with a legend.

library("maptools")
library(rgeos)
library(rgdal)
library(argosfilter)
library(adehabitatLT)

#set working directory
setwd("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/Homework7")

# read in all of the turtle track csv files and assign
t1 <-read.csv("LeatherbackTurtle_ID1_DataForStudents.csv",header=T,sep=",")
names(t1)
head(t1)
t2 <-read.csv("LeatherbackTurtle_ID2_DataForStudents.csv",header=T,sep=",")
head(t2)
t6 <-read.csv("LeatherbackTurtle_ID6_DataForStudents.csv",header=T,sep=",")
head(t6)
t7 <-read.csv("LeatherbackTurtle_ID7_DataForStudents.csv",header=T,sep=",")
head(t7)
t8 <-read.csv("LeatherbackTurtle_ID8_DataForStudents.csv",header=T,sep=",")
head(t8)

# Plot track on a map
## Note: Adjust x and y limits if track gets cut-off in image
#maptools settings
CRy<-c(-40,20)
CRx<-c(220,310)
m<-system.file("share/gshhs_c.b",package="maptools")
CR<-Rgshhs(m,xlim=CRx,ylim=CRy,level=1)
#start with Pacific coast of Central and South America
plot(CR$SP,col="grey",xaxs="i",yaxs="i",axes=TRUE,las=1)
#add the turtle tracks
#turtle1
points((t1$long+360),t1$lat,pch=20,cex=0.5,col="red")
points((t1$long[1]+360),t1$lat[1],pch=20,cex=1,col="red")
#turtle2
points((t2$long+360),t2$lat,pch=20,cex=0.5,col="orange")
points((t2$long[1]+360),t2$lat[1],pch=20,cex=1,col="orange")
#turtle6
points((t6$long+360),t6$lat,pch=20,cex=0.5,col="yellow")
points((t6$long[1]+360),t6$lat[1],pch=20,cex=1,col="yellow")
#turtle7
points((t7$long+360),t7$lat,pch=20,cex=0.5,col="green")
points((t7$long[1]+360),t7$lat[1],pch=20,cex=1,col="green")
#turtle8
points((t8$long+360),t8$lat,pch=20,cex=0.5,col="blue")
points((t8$long[1]+360),t8$lat[1],pch=20,cex=1,col="blue")
legend("bottomright",legend=c("Leatherback #1", "Leatherback #2", "Leatherback #6", "Leatherback #7", "Leatherback #8"),pch=20, col=c("red", "orange", "yellow", "green", "blue"))

#save this plot to a file
png(filename = "LeatherbackTurtles_map.png",  width = 782, height = 480, units = "px", pointsize = 14, bg = "white")
#start with Pacific coast of Central and South America
plot(CR$SP,col="grey",xaxs="i",yaxs="i",axes=TRUE,las=1)
#add the turtle tracks
#turtle1
points((t1$long+360),t1$lat,pch=20,cex=0.5,col="red")
points((t1$long[1]+360),t1$lat[1],pch=20,cex=1,col="red")
#turtle2
points((t2$long+360),t2$lat,pch=20,cex=0.5,col="orange")
points((t2$long[1]+360),t2$lat[1],pch=20,cex=1,col="orange")
#turtle6
points((t6$long+360),t6$lat,pch=20,cex=0.5,col="yellow")
points((t6$long[1]+360),t6$lat[1],pch=20,cex=1,col="yellow")
#turtle7
points((t7$long+360),t7$lat,pch=20,cex=0.5,col="green")
points((t7$long[1]+360),t7$lat[1],pch=20,cex=1,col="green")
#turtle8
points((t8$long+360),t8$lat,pch=20,cex=0.5,col="blue")
points((t8$long[1]+360),t8$lat[1],pch=20,cex=1,col="blue")
legend("bottomright",legend=c("Leatherback #1", "Leatherback #2", "Leatherback #6", "Leatherback #7", "Leatherback #8"),pch=20, col=c("red", "orange", "yellow", "green", "blue"))
dev.off()



##2. Plot step length versus frequency on logarithmic scales for each
##leatherback turtle track.

#for loop to calculate distance between consecutive points in km
#turtle1
l<-length(t1$long)
t1$dist<-numeric(l)
for (i in 1:(l-1)) {
  t1$dist[i]<-distance(t1$lat[i],t1$lat[i+1],t1$lon[i],t1$lon[i+1])
}

#assign the last position a distance of zero (no future steps)
t1$dist[l]<-0
summary(t1$dist)

#turtle2
l<-length(t2$long)
t2$dist<-numeric(l)
for (i in 1:(l-1)) {
  t2$dist[i]<-distance(t2$lat[i],t2$lat[i+1],t2$lon[i],t2$lon[i+1])
}

#assign the last position a distance of zero (no future steps)
t2$dist[l]<-0
summary(t2$dist)

#turtle6
l<-length(t6$long)
t6$dist<-numeric(l)
for (i in 1:(l-1)) {
  t6$dist[i]<-distance(t6$lat[i],t6$lat[i+1],t6$lon[i],t6$lon[i+1])
}

#assign the last position a distance of zero (no future steps)
t6$dist[l]<-0
summary(t6$dist)

#turtle7
l<-length(t7$long)
t7$dist<-numeric(l)
for (i in 1:(l-1)) {
  t7$dist[i]<-distance(t7$lat[i],t7$lat[i+1],t7$lon[i],t7$lon[i+1])
}

#assign the last position a distance of zero (no future steps)
t7$dist[l]<-0
summary(t7$dist)

#turtle8
l<-length(t8$long)
t8$dist<-numeric(l)
for (i in 1:(l-1)) {
  t8$dist[i]<-distance(t8$lat[i],t8$lat[i+1],t8$lon[i],t8$lon[i+1])
}

#assign the last position a distance of zero (no future steps)
t8$dist[l]<-0
summary(t8$dist)

#Turtle1 Plotting
# plot the step length histogram on log scale (with step + 1 to avoid infinites)
# Change plot filename to appropriate ID number.
h<-hist(t1$dist, breaks=12,xlim=c(0,100),xlab="Daily distance (km)",
        main="Turtle 1 Histogram of step lengths",las=1)
h
step<-h$breaks[h$breaks>1]
freq<-h$counts+1
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b")

#save this plot to a file
png(filename = "Turtle1StepLengthDistribution.png",  
    width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b",las=1)
dev.off()

#does step lengths distribution resemble a  Levy flight model?
#estimate the Levy parameter mu from a linear model of 
#log frequency versus log step length.
model<-lm(log10(freq)~log10(step))
summary(model)

#Turtle2 Plotting
# plot the step length histogram on log scale (with step + 1 to avoid infinites)
# Change plot filename to appropriate ID number.
h<-hist(t2$dist, breaks=12,xlim=c(0,100),xlab="Daily distance (km)",
        main="Turtle 2 Histogram of step lengths",las=1)
h
step<-h$breaks[h$breaks>1]
freq<-h$counts+1
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b")

#save this plot to a file
png(filename = "Turtle2StepLengthDistribution.png",  
    width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b",las=1)
dev.off()

#does step lengths distribution resemble a  Levy flight model?
#estimate the Levy parameter mu from a linear model of 
#log frequency versus log step length.
model<-lm(log10(freq)~log10(step))
summary(model)

#Turtle6 Plotting
# plot the step length histogram on log scale (with step + 1 to avoid infinites)
# Change plot filename to appropriate ID number.
h<-hist(t6$dist, breaks=12,xlim=c(0,100),xlab="Daily distance (km)",
        main="Turtle 6 Histogram of step lengths",las=1)
h
step<-h$breaks[h$breaks>1]
freq<-h$counts+1
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b")

#save this plot to a file
png(filename = "Turtle6StepLengthDistribution.png",  
    width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b",las=1)
dev.off()

#does step lengths distribution resemble a  Levy flight model?
#estimate the Levy parameter mu from a linear model of 
#log frequency versus log step length.
model<-lm(log10(freq)~log10(step))
summary(model)

#Turtle7 Plotting
# plot the step length histogram on log scale (with step + 1 to avoid infinites)
# Change plot filename to appropriate ID number.
h<-hist(t7$dist, breaks=12,xlim=c(0,100),xlab="Daily distance (km)",
        main="Turtle 1 Histogram of step lengths",las=1)
h
step<-h$breaks[h$breaks>1]
freq<-h$counts+1
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b")

#save this plot to a file
png(filename = "Turtle7StepLengthDistribution.png",  
    width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b",las=1)
dev.off()

#does step lengths distribution resemble a  Levy flight model?
#estimate the Levy parameter mu from a linear model of 
#log frequency versus log step length.
model<-lm(log10(freq)~log10(step))
summary(model)

#Turtle8 Plotting
# plot the step length histogram on log scale (with step + 1 to avoid infinites)
# Change plot filename to appropriate ID number.
h<-hist(t8$dist, breaks=12,xlim=c(0,100),xlab="Daily distance (km)",
        main="Turtle 8 Histogram of step lengths",las=1)
h
step<-h$breaks[h$breaks>1]
freq<-h$counts+1
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b")

#save this plot to a file
png(filename = "Turtle8StepLengthDistribution.png",  
    width = 600, height = 600, units = "px", pointsize = 14, bg = "white")
plot(log10(step),log10(freq),xlab="Log(step length)", 
     ylab="Log(frequency)",type="b",las=1)
dev.off()

#does step lengths distribution resemble a  Levy flight model?
#estimate the Levy parameter mu from a linear model of 
#log frequency versus log step length.
model<-lm(log10(freq)~log10(step))
summary(model)


##3. For each turtle ID, does the distribution of step lengths fit a Lévy
##flight model distribution?

#Calculations are completed in-line above in question 2 with the following
#format:
#model<-lm(log10(freq)~log10(step))
#summary(model)

#Turtle1
#Call:
#  lm(formula = log10(freq) ~ log10(step))
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.78760 -0.49222  0.08548  0.51424  0.56890 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   4.1558     1.0308   4.031  0.00378 **
#  log10(step)  -1.8255     0.6124  -2.981  0.01758 * 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.5849 on 8 degrees of freedom
#Multiple R-squared:  0.5262,	Adjusted R-squared:  0.467 
#F-statistic: 8.886 on 1 and 8 DF,  p-value: 0.01758

# For turtle1, the log10(step) coefficient is -1.8255. 
# This gives a Mu value of 1.8255, which is consistent with a Levy flight
#(absolute value between 1 and 3).

#Turtle2
#Call:
#  lm(formula = log10(freq) ~ log10(step))
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-0.6845 -0.2934  0.1434  0.2298  0.4992 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.9957     0.4400   4.535 0.000394 ***
#  log10(step)  -0.6890     0.2766  -2.491 0.024957 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.3824 on 15 degrees of freedom
#Multiple R-squared:  0.2926,	Adjusted R-squared:  0.2454 
#F-statistic: 6.204 on 1 and 15 DF,  p-value: 0.02496

# For turtle2, the log10(step) coefficient is -0.6890. 
# This gives a Mu value of 0.6890, which is inconsistent with a Levy flight
#(absolute value between 1 and 3).

#Turtle6
#Call:
#  lm(formula = log10(freq) ~ log10(step))
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.50803 -0.18610  0.07658  0.24985  0.37956 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.7027     0.4540   8.156 9.94e-06 ***
#  log10(step)  -1.6626     0.3117  -5.335 0.000331 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.3393 on 10 degrees of freedom
#Multiple R-squared:   0.74,	Adjusted R-squared:  0.714 
#F-statistic: 28.46 on 1 and 10 DF,  p-value: 0.0003308

# For turtle6, the log10(step) coefficient is -1.6626. 
# This gives a Mu value of 1.6626, which is consistent with a Levy flight
#(absolute value between 1 and 3).

#Turtle7
#Call:
#  lm(formula = log10(freq) ~ log10(step))
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.43138 -0.19072 -0.02458  0.17492  0.33692 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)   1.6463     0.5073   3.245   0.0141 *
#  log10(step)  -0.4367     0.3085  -1.416   0.1998  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.2726 on 7 degrees of freedom
#Multiple R-squared:  0.2226,	Adjusted R-squared:  0.1115 
#F-statistic: 2.004 on 1 and 7 DF,  p-value: 0.1998

# For turtle7, the log10(step) coefficient is -0.4367. 
# This gives a Mu value of 0.4367, which is inconsistent with a Levy flight
#(absolute value between 1 and 3).


#Turtle8
#Call:
#  lm(formula = log10(freq) ~ log10(step))
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.51507 -0.19213 -0.07224  0.22607  0.66044 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   3.5136     0.7987   4.399  0.00316 **
#  log10(step)  -1.2582     0.4857  -2.590  0.03593 * 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.4292 on 7 degrees of freedom
#Multiple R-squared:  0.4894,	Adjusted R-squared:  0.4165 
#F-statistic:  6.71 on 1 and 7 DF,  p-value: 0.03593

# For turtle8, the log10(step) coefficient is -1.2582. 
# This gives a Mu value of 1.2582, which is consistent with a Levy flight
#(absolute value between 1 and 3).


  
  
##4. Calculate the median and maximum speeds for each turtle track
##(where speeds>0).

#turtle1
# Calculate speed as distance divided by time (km/h), 
# where the time interval between positions is 24 hours.
t1$speed<-t1$dist/24
summary(t1$speed)
summary(t1$speed[t1$speed>0]) #where speed is >0
## The median speed is 1.0597 km/h with a maximum speed of 4.1023 km/h.

#turtle2
# Calculate speed as distance divided by time (km/h), 
# where the time interval between positions is 24 hours.
t2$speed<-t2$dist/24
summary(t2$speed)
summary(t2$speed[t2$speed>0]) #where speed is >0
## The median speed is 1.18292 km/h with a maximum speed of 3.37830 km/h.

#turtle6
# Calculate speed as distance divided by time (km/h), 
# where the time interval between positions is 24 hours.
t6$speed<-t6$dist/24
summary(t6$speed)
summary(t6$speed[t6$speed>0]) #where speed is >0
## The median speed is 0.529316 km/h with a maximum speed of 2.348022 km/h.

#turtle7
# Calculate speed as distance divided by time (km/h), 
# where the time interval between positions is 24 hours.
t7$speed<-t7$dist/24
summary(t7$speed)
summary(t7$speed[t7$speed>0]) #where speed is >0
## The median speed is 1.10291 km/h with a maximum speed of 3.50967 km/h.

#turtle8
# Calculate speed as distance divided by time (km/h), 
# where the time interval between positions is 24 hours.
t8$speed<-t8$dist/24
summary(t8$speed)
summary(t8$speed[t8$speed>0]) #where speed is >0
## The median speed is 1.1830 km/h with a maximum speed of 3.5555 km/h.



##5. Plot the net squared displacement for each turtle track (solid black line),
##the expected net squared displacement for a correlated random walk (CRW, black 
##dashed line), and for a biased random walk (BRW, gray dashed line) using the
##step lengths, turning angles and bearing from each track’s distribution. There
##should be one graph for each track (five graphs total).






##6. For each turtle ID, did they fit a correlated random walk, a biased random
##walk model, or neither?
  



  
##7. What do these results suggest about the turtles’ behavior during this
##tracking period? (A couple of sentences is fine)




