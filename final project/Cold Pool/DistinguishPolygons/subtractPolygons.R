library(sf)

shape1 <- st_read("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Regions/region1/region1Location.shp")
plot(shape1)
shape2 <- st_read("C:/Users/hongs/OneDrive - University of Maryland/Desktop/University of Maryland/Classes/SpatialEcology/final project/Regions/region2/region2Location.shp")
plot(shape2)
head(shape1)
head(shape2)
identical(shape1,shape2)

shape1$area <- st_area(st_geometry(shape1)) #calculate the area of the polgons and add a new attribute table called "area"
shape2$area <- st_area(st_geometry(shape2)) #calculate the area of the polgons and add a new attribute table called "area"
areascf <- as.data.frame(cbind(shape1$area,shape2$area)) #bind the two columns together into a new dataframe. This will only work if both the original files have the same number of rows. if we are not certain that this is the case, we can check with length(st_geometry(new_SE)) for each file. 
#and inspect the new data frame we created:

head(areascf)

#rename the columns something more useful
colnames(areascf) <- c("shape1","shape2")
#create a new column showing the difference in area between the two shape files
#I rounded up to 3 dps to avoid very small differences. 
areascf$diff <- round(areascf$shape1-areascf$shape2,3)
areascf
plot(shape1)
plot(shape2, add=TRUE)




l = vector("list", 2)
l[[1]]=shape1
l[[2]]=shape2


library(sf)
m = rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))
p = st_polygon(list(m))
n = 2
l = vector("list", n)
for (i in 1:n)
  l[[i]] = p + 10 * runif(2)
s = st_sfc(l)
plot(l, col = sf.colors(categorical = TRUE, alpha = .5))
title("overlapping squares")

d = st_difference(s) # sequential differences: s1, s2-s1, s3-s2-s1, ...
plot(d, col = sf.colors(categorical = TRUE, alpha = .5))
title("non-overlapping differences")

i = st_intersection(s) # all intersections
plot(i, col = sf.colors(categorical = TRUE, alpha = .5))
title("non-overlapping intersections")

summary(lengths(st_overlaps(s, s))) # includes self-counts!
summary(lengths(st_overlaps(d, d)))
summary(lengths(st_overlaps(i, i)))
sf = st_sf(s)
i = st_intersection(sf) # all intersections
plot(i["n.overlaps"])

summary(i$n.overlaps - lengths(i$origins))
# A helper function that erases all of y from x:
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
poly = st_polygon(list(cbind(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0))))
lines = st_multilinestring(list(
  cbind(c(0, 1), c(1, 1.05)),
  cbind(c(0, 1), c(0, -.05)),
  cbind(c(1, .95, 1), c(1.05, .5, -.05))
))
snapped = st_snap(poly, lines, tolerance=.1)
plot(snapped, col='red')
plot(poly, border='green', add=TRUE)
plot(lines, lwd=2, col='blue', add=TRUE)

