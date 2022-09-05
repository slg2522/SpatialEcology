# Spatial data in R Tutorial #

# The goal of this tutorial is to teach basic methods for working with raster
# and vector data in R. In addition to demonstrating how to manipulate 
# spatial data objects, the tutorial demonstrates working with vector and
# raster data, some of the most common GIS operations, and reading / writing 
# spatial data.


# INTRODUCTION TO VECTOR DATA IN R ---------------------------------------------
# Let's start by creating some point data
longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5, -120.8, -119.5, 
               -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9, 36.2, 39, 41.6, 36.9)
# note order (x=longitude, y=latitude)! We often say "lat-long", which is (y,x)
lonlat <- cbind(longitude, latitude)
head(lonlat)
class(lonlat)
plot(lonlat)

#### Create SpatialPoints object ####
# The lonlat object is class = matrix
# We will convert it to a spatial object
library(sp)
pts <- sp::SpatialPoints(lonlat) # convert to spatial object
class(pts)
showDefault(pts) # show aspects of the object
plot(pts) # compare this to plot(lonlat)
# things to note:
# coordinates
# bbox = bounding box / spatial EXTENT
# proj4string = stores coordinate reference system or "CRS". The 
# CRS = NA because we haven't defined it

# Define the coordinate reference system ####
# More details about this later.
?CRS
# Create a CRS object - this is called PROJ.4 format. We will come back to
# this in the 'advanced' tutorial.
crsRef <- CRS('+proj=longlat +datum=WGS84')
crsRef
# We can also use "EPSG" codes
# See: https://epsg.io/
crsRef <- CRS('+init=epsg:4326')
crsRef
# now we define the CRS of the 'pts' object
pts <- SpatialPoints(lonlat, proj4string=crsRef)
showDefault(pts)
pts

#### Create SpatialPointsDataFrame object ####
# Generate random values, same number as in the 'pts' object
# We will assume these are precipitation measurements
precipValue <- runif(nrow(lonlat), min=0, max=100) # see ?runif
# make a dataframe that combines points and the new data values
df <- data.frame(ID=1:nrow(lonlat), precip=precipValue)
# combine spatial + attribute data
ptsdf <- SpatialPointsDataFrame(pts, data=df)
ptsdf
str(ptsdf)
showDefault(ptsdf)

# the 'attribute' data are in slot @data
ptsdf@data

# the 'spatial data' (coordinates) are in slot @coords
ptsdf@coords


#### Create SpatialLines & SpatialPolygons ####
# We will need the 'raster' package to do this.
# Note that when you load certain libraries, it can change how things behave
# in your R session. For example, loading 'raster' changes the way the pts 
# object is printed
pts # prints object
# load raster package
library(raster)
pts # prints just a summary
lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6) 
lonlat <- cbind(lon, lat)

# make spatial lines object
lns <- spLines(lonlat, crs=crsRef)
lns

# make spatial polygon object
pols <- spPolygons(lonlat, crs=crsRef)
pols
str(pols) 
# can be complex if multiple polygons and holes

# make a plot
plot(pols, axes=TRUE, las=1) #las = style of axis labels
plot(pols, border='blue', col='yellow', lwd=3, add=TRUE)
points(pts, col='red', pch=20, cex=3)
dev.off()
plot(lns)


# INTRODUCTION TO RASTER DATA IN R ---------------------------------------------
#### Create a RasterLayer ####
library(raster)
# see ?raster.  There are MANY ways to make a raster. Here we are providing
# only the size and extent of the raster, but no data.
r <- raster(ncol=10, nrow=10, xmx=-80, xmn=-150, ymn=20, ymx=60)
r
# things to note:
# dimensions
# resolution
# extent
# crs (the raster function will default to angular coordinates and WGS84)
plot(r) # error! There's no data to plot.

# Let's add some random values
values(r) <- runif(ncell(r)) # random numbers
r
plot(r)

# Add sequence of numbers
values(r) <- 1:ncell(r)
r
plot(r) # note the order in which values were added = row-wise


#### Create a RasterStack ####
# Let's create a couple of new rasters. Here we create two new rasters using
# simple math functions
r2 <- r * r 
r3  <- sqrt(r)
# 'stack' function will combine the three rasters into one object with three 
# layers
s <- stack(r, r2, r3)
# !IMPORTANT! You can only stack rasters that have the same
# resolution and extent. You will learn that this can become
# a major challenge when working with multiple raster datasets
# from different sources that differ in resolution, extent, etc.
s
plot(s)


#### Create a RasterBrick ####
# In many ways a brick is the same as a stack. The only real differences is 
# how the files are saved to disk. A stack is comprised of multiple raster
# files, whereas a brick is a single file with multiple layers. Once they
# are read into R, they are basically identical.
b <- brick(s)
b
plot(b)


# READING / WRITING SPATIAL DATA -----------------------------------------------
#### Read in a shapefile from disk ####
library(raster)
# Here we will load an example file provided with the raster package & so that is
# why we are using the 'system.file' function. This DOES NOT work for files that
# are not included as part of an R package.
filename <- system.file("external/lux.shp", package="raster")
filename

# lots of ways to read .shp, 
# 'shapefile' is a good option from the raster package
?shapefile
s <- shapefile(filename)
s
# s is a SpatialPolygonsDataFrame
# it has 12 features each with 4 attributes. Make sense?

#### Save a shapefile to disk ####
# Provide Spatial* object and filename to the 'shapefile' function
outfile <- 'test.shp'
shapefile(s, outfile, overwrite=TRUE)

# let's have a look at what files were written
ff <- list.files(patt="^test")
ff # note that four files were written
# delete the file from disk
file.remove(ff)


#### Read in a raster from disk ####
f <- system.file("external/rlogo.grd", package="raster")
f # this is a .grd file. There are many raster file types.
# raster function reads in a file as a RasterLayer
r1 <- raster(f)
r1 # note number of 'bands;
class(r1)
plot(r1)
r2 <- raster(f, band=2)
r2
plot(r2)

# For multi-band rasters (a single raster file with multiple layers), 
# you really want it to be a 'RasterBrick' object
b <- brick(f)
b #RasterBrick
plot(b)
plotRGB(b) # plot bands to the Red, Green, Blue (RGB)
plot(b[[3]]) # Use double brackets [[3]] to plot third layer / band only

# stack
s <- stack(f) #RasterStack
s # note number of 'layers'
plot(s)
plotRGB(s)

#### Save a raster to disk ####
# Here, we will write the file as a GeoTiff (.tif)
# Provide Raster* object and filename. The file format is assumed from 
# the filename extension. Or you can use 'format' argument.
# The 'datatype' argument can be used to set whether data are
# integer, float, etc.
x <- writeRaster(s, 'output.tif', overwrite=F)
x # a single file with multiple bands
list.files(patt="^output")
file.remove('output.tif')



# In-class tutorial
# Coordinate Reference Systems in R


# COORDINATE REFERENCE SYSTEMS  ------------------------------------------------
library(rgdal)
library(raster)

#### Projecting vector data ####
# lets have a look at an example
f <- system.file("external/lux.shp", package="raster")
p <- shapefile(f)
p
plot(p)
crs(p)

# What if our data do not come with a defined CRS?
pp <- p
crs(pp) <- NA
crs(pp)
# *If* you know the projection you can *assign* it by providing an 
# appropriate text string for the CRS (more on this later).
crs(pp) <- CRS("+init=epsg:4326") 
crs(pp)

# IMPORTANT! You cannot use the CRS function to CONVERT
# to a different CRS - the CRS function is used only to ASSIGN a 
# known CRS.
# To CONVERT to a new CRS, you must PROJECT / TRANSFORM your data.
# A CRS is only a LABEL.

# Let's transform / project the data to a new CRS using the 'spTransform' 
# function.
# First, define the CRS
newcrs <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
pProj <- spTransform(p, newcrs) # this performs the projection
pProj # note change in units (extent)
# now let's plot both versions of the data
dev.off() # clear existing plot window
par(mfrow=c(1,2)) # plot two plots in same window, one row two columns
plot(pProj, axes=T, main="Projected")
plot(p, axes=T, main="long-lat / angular")
dev.off()
# transform back to long/lat
p2 <- spTransform(pProj, CRS("+proj=longlat +datum=WGS84"))


#### Projecting raster data ####
# IMPORTANT! If you want to have your vector and raster data
# in the same projection, it is better to transform the VECTOR
# data to the projection of the raster data than vice versa.
# Let's look at why
r <- raster(xmn=-110, xmx=-90, ymn=40, ymx=60, ncols=40, nrows=40)
r <- setValues(r, 1:ncell(r))
r # assumes long/lat and WGS84
plot(r)
ncell(r)

# define new projection
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"

# 'projectRaster' function from raster pacakge
pr1 <- projectRaster(r, crs=newproj)
crs(pr1)
pr1 # note the min / max values have changed
ncell(pr1) # note the number of cells has changed!
# WHAT HAPPENED HERE?

# let's define cell size using 'res' argument
pr2 <- projectRaster(r, crs=newproj, res=20000)
pr2
plot(pr2)

# To have more control over the projection of rasters, you can provide an 
# existing raster to inform the transformation. First, we use 'projectExtent' 
# because we do not have an existing raster object.
pr3 <- projectExtent(r, newproj)
pr3

# next we use 'projectRaster' and provide both rasters to the function instead
# of one raster and a CRS
pr3 <- projectRaster(r, pr3)
pr3
plot(pr3)

# Important Notes:
# Transforming a raster changes the values in the raster
# because new values must be estimated for the new # of cells.
# How might this process differ for continuous vs. categorical data?
# See:
?projectRaster

# Also, when projecting rasters, it is best to use an "equal area projection"
# if possible to ensure grid cells are the same size.



# WORKING WITH VECTOR DATA -----------------------------------------------------
library(raster)
library(rgeos)
library(rgdal)

# load example data & plot
f <- system.file("external/lux.shp", package="raster")
p <- shapefile(f)
p
par(mai=c(0,0,0,0)) # set the margins to make the plot larger
plot(p)

# Let's extract the DATA ATTRIBUTES from the SpatialPolygonsDataFrame 
d <- data.frame(p)
head(d)
# same as 
d <- p@data
head(d)

# To extract GEOMETRY / SPATIAL ATTRIBUTES
g <- geom(p)
head(g)

# See variable names
p$NAME_2

# What if we want to retain the spatial info, but extract only the 'NAME_2' 
# attribute data?
s <- p[, 'NAME_2'] 
s # note the object class
par(mai=c(0,0,0,0))
plot(s)

# What if we want to retain the spatial info, but want only one row of data?
s <- p[5, ] 
s # note the object class
par(mai=c(0,0,0,0))
plot(s)

# select specific records (rows) 
i <- which(p$NAME_1 == 'Grevenmacher')
g <- p[i,]
g
plot(g)

# add a new attribute named 'new'
set.seed(0) # sets the sequence of random generation
p$new <- sample(letters, length(p))
p

# assign new values to an existing attribute 
p$new <- sample(LETTERS, length(p))
p

# remove an attribute
p$new <- NULL

# join a data frame with a Spatial* object 
dfr <- data.frame(District=p$NAME_1, Canton=p$NAME_2, 
                  Value=round(runif(length(p), 100, 1000)))
head(dfr)
# sort data by Canton
dfr <- dfr[order(dfr$Canton), ]
# Now we merge the spatial data and the new data frame
# specify columns in common between dfr and p
pm <- merge(p, dfr, by.x=c('NAME_1', 'NAME_2'), by.y=c('District', 'Canton'))
pm
data.frame(pm) # look at the attribute data

# interactively query records
?click
plot(p)
click(p) # move your cursor over the map and click
click(p, n=3) # three queries

# interactively select records
?select
# move your cursor over the map and click the region (rectangle) to select
sel <- select(p) 
sel # selected object
plot(sel)

#### Append / aggregate / overlay data ####
# First, we create a polygon comprised of 4 smaller polygons
# Create a raster
z <- raster(p, nrow=2, ncol=2, vals=1:4)
names(z) <- 'Zone'
# coerce RasterLayer to SpatialPolygonsDataFrame
z <- as(z, 'SpatialPolygonsDataFrame')
z
plot(z) 

# Now, we extract just the second polygon
z2 <- z[2,] # select polygon #2 
plot(p)
plot(z, add=TRUE, border='blue', lwd=5)
plot(z2, add=TRUE, border='red', lwd=2, density=3, col='red')

#### Overlay ####  
# erase a part of a SpatialPolygon
plot(p)
plot(z2, add=T)
e <- erase(p, z2) # note warnings, what do these warnings mean?
plot(e)

# Same as:
ee <- p - z2
plot(ee)

#### Intersect ####
i <- intersect(p, z2)
plot(i)
# or use:
ii <- p * z2
plot(ii)

# intersect / crop with an 'extent' object
# can be very useful when working with rasters as well
?extent
e <- extent(c(xmin=6, xmax=6.4, ymin=49.7, ymax=50))
pe <- crop(p, e)
plot(p)
plot(pe, col='light blue', add=TRUE)
plot(e, add=TRUE, lwd=3, col='red')

#### Union ####
u <- union(aggregate(p), z)
crs(p) <- crs(z)
u
plot(u)
# Color each resulting polygon using a color selected at random
set.seed(5)
# u is comprised of numerous polygons that result from the union of p and z
plot(u, col=sample(rainbow(length(u))))

#### Perform spatial queries ####
pts <- matrix(c(6, 6.1, 5.9, 5.7, 6.4, 50, 49.9, 49.8, 49.7, 49.5), ncol=2)
spts <- SpatialPoints(pts, proj4string=crs(p))
plot(z, col='light blue', lwd=2)
points(spts, col='light gray', pch=20, cex=6)
text(spts, 1:nrow(pts), col='red', font=2, cex=1.5)
lines(p, col='blue', lwd=2)
# perform query at points using 'over' function
over(spts, p) # why are rows #4 and #5 NA?
over(spts, z) # why is point #4 NA?


# WORKING WITH RASTER DATA -----------------------------------------------------
library(raster)

# create a RasterLayer with the default parameters
x <- raster()
x
# note default extent, resolution, etc
# 1 deg x 1 deg
# covers all longitudes and latitudes on Earth

# create a RasterLayer with specific spatial settings
x <- raster(ncol=36, nrow=18, xmn=-1000, xmx=1000, ymn=-100, ymx=900)
x
# set the resolution = grid size
res(x)
res(x) <- 100
res(x)

# change the number of columns
ncol(x)
ncol(x) <- 18
ncol(x)
res(x) # note change in resolution to match the # of columns

# set the crs, remember this is not the same as transforming to a new CRS
projection(x) <- "+proj=utm +zone=48 +datum=WGS84"
x

# Now, let's add values to an empty raster
r <- raster(ncol=10, nrow=10)
ncell(r)
hasValues(r)
set.seed(0)
values(r) <- runif(ncell(r))
hasValues(r)
dev.off()
plot(r, main='Raster with 100 cells')


#### Raster algebra ####
r <- raster(ncol=10, nrow=10)
values(r) <- 1:ncell(r)
# any mathematical operation is possible
s <- r + 10
s <- sqrt(s)
s <- s * r + 5

# working with stacks
r <- raster(ncol=5, nrow=5)
r[] <- 1
s <- stack(r, r+1)
q <- stack(r, r+2, r+4, r+6)
q
plot(q)
plot(q+3) # adds value to all rasters in stack

# summary functions - always return a RasterLayer
a <- mean(q) # mean(1+3+5+7)
plot(a)
b <- sum(r,s)
st <- stack(r, s, a, b)
sst <- sum(st)
sst
plot(sst)

# if you want a numerical summary value, not a raster use 'cellStats'
?cellStats
class(st)
cellStats(st, 'sum')
class(sst)
cellStats(sst, 'sum')


#### High-level functions ####
# These functions take Raster* objects and then perform some process.
# If you specify a fileName, they are saved to disk, otherwise
# they are stored in memory (or written to a temp file if 
# too large for memory). 
r <- raster()
r[] <- 1:ncell(r)
r
# reduce the resolution of the raster by aggregating cells together
?aggregate
# make cells 20x larger, and take mean
ra <- aggregate(r, #input raster
                fact=20, # integer(s) number of cells to aggregate over
                fun=mean) # function to use when aggregating
ra

ncell(r) # 64800
ncell(ra) # = ncell(r)/20/20

par(mfrow=c(1,2))
plot(r)
plot(ra)
dev.off()

# increase the resolution of the raster by breaking cells apart
rd <- disaggregate(ra, 20)
rd
ncell(rd) # back to original cell number
ncell(r)
# but not the same data pattern
par(mfrow=c(1,2))
plot(r)
plot(rd)
dev.off()

# crop a raster
?crop
r1 <- crop(r, extent(-50,0,0,30))
r1
r2 <- crop(r, extent(-10,50,-20, 10))
r2
par(mfrow=c(1,2))
plot(r1)
plot(r2)
dev.off()

# merge cropped rasters into a single raster
# must have same resolution and origin
?merge
# merge and write to disk
m <- merge(r1, r2, filename='test.grd', overwrite=TRUE)
plot(m)
ff <- list.files(patt="^test")
ff #.grd rasters are comprised of two files
file.remove(ff)

# 'calc' function lets you apply a custom functions to a Raster* object
r <- raster(ncol=3, nrow=2)
r[] <- 1:ncell(r)
getValues(r)
#  set all vales < 4 to NA
s <- calc(r, fun=function(x){ x[x < 4] <- NA; return(x)} )
plot(r)
plot(s)

# reclassify a raster
?reclassify
x <- reclassify(r, #input raster 
                c(0,2,1,  2,5,2, 4,10,3)) #matrix for reclassification
# from 0, to 2, change to 1, ect
as.matrix(r)                                
as.matrix(x)

#### Accessing data within rasters ####
# Learning how to extract or change specific values of a raster
# is a critical skill to master
# Let's try a few methods
r <- raster(system.file("external/test.grd", package="raster"))
dev.off() # clear plots
plot(r)
# get ALL cell values
v <- getValues(r)
v[1:50] # What is going on here? Why all NAs?

# using indexing to get values
# get data from row 50 only
v <- getValues(r, row=50)
v
values(r)[1:5] # get the first 5 values
r[10,] # tenth row of values
r[,4] # fourth column of values
r[10,70] # row 10, column 70
r[5] # fifth value

r[10,70] <- 0 # change the value in row 10, column 70

# get a block of values by defining a rectangle of rows and columns
getValuesBlock(r, row=50, nrows=1, col=35, ncols=5)

# get values using cell numbers
cells <- cellFromRowCol(r, 50, 35:39)
cells
extract(r, cells)
r[cells] 

# get values using xy coordinates
xy <- xyFromCell(r, cells)
xy
plot(r)
points(xy, cex=0.25, pch=20)
extract(r, xy)

