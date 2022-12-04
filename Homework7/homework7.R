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







##3. For each turtle ID, does the distribution of step lengths fit a Lévy
##flight model distribution?





  
  
##4. Calculate the median and maximum speeds for each turtle track
##(where speeds>0).






##5. Plot the net squared displacement for each turtle track (solid black line),
##the expected net squared displacement for a correlated random walk (CRW, black 
##dashed line), and for a biased random walk (BRW, gray dashed line) using the
##step lengths, turning angles and bearing from each track’s distribution. There
##should be one graph for each track (five graphs total).






##6. For each turtle ID, did they fit a correlated random walk, a biased random
##walk model, or neither?
  



  
##7. What do these results suggest about the turtles’ behavior during this
##tracking period? (A couple of sentences is fine)




