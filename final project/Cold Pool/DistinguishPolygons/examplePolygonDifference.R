library(sf)
old_SE <- st_read("siose05_REC_COMP.shp")
new_SE <- st_read("siose05_REC_COMP_MADRID_SE.shp")
head(old_SE)  #check the file and show the first 6 records in the attribute table
head(new_SE) #check the file and show the first 6 records in the attribute table
#first, check that they are not actually identical
identical(new_SE,old_SE)

old_SE$area <- st_area(st_geometry(old_SE)) #calculate the area of the polgons and add a new attribute table called "area"
new_SE$area <- st_area(st_geometry(new_SE)) #calculate the area of the polgons and add a new attribute table called "area"
areascf <- as.data.frame(cbind(old_SE$area,new_SE$area)) #bind the two columns together into a new dataframe. This will only work if both the original files have the same number of rows. if we are not certain that this is the case, we can check with length(st_geometry(new_SE)) for each file. 
#and inspect the new data frame we created:
head(areascf)

#rename the columns something more useful
colnames(areascf) <- c("old","new")
#create a new column showing the difference in area between the two shape files
#I rounded up to 3 dps to avoid very small differences. 
areascf$diff <- round(areascf$old-areascf$new,3)
areascf