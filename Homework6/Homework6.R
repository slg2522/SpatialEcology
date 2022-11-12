##Lab 6: Home Range and Resource Selection
##Central question
##Endangered FL Panthers have large home ranges in areas where there is also considerable human 
##development. This movement potentially puts them at risk for human-wildlife interactions.
##Therefore, researchers have tracked several FL Panthers to determine their space use, the extent 
##to which they use and/or avoid developed areas, and how much of their home range is within 
##protected vs unprotected lands. You will be graded on your answers and your ability to produce 
##clean, well commented R code that performs the tasks listed below.
##Files: The data for this homework was all included in the class recitation (with the exception of 
##the protected areas file).
##Landcover raster: panther_landcover.grd
##Florida Panther tracking locations: panthers.shp
##Landcover reclassification scheme: resistance reclass.txt
##Shapefile of protected areas: panther_publicland.shp





##1. Home Range
##Young (subadult) cats are known to move between protected and unprotected properties.
##Managers would like to know what proportion of the home ranges of subadults is on public 
##(protected) land. They would further like to know how much the method used to measure the 
##home range influence the estimate of the proportion of use on public land.
##Instructions: Use the landcover provided, the protected areas file, and the tracking data for the
##subadult cats (age class = SA (for subadult)) with the following home range estimators: MCP, 
##KDE (95th percentile), and Brownian Bridge KDE (95th percentile). (Hint: look at the help file 
##for “crop” in the raster library)
##In your response include: 
##1. one or more maps of young cat home ranges and protected areas
##2. the proportional measures of use
##3. your interpretation of the results, answering the question for managers.





##2. Resource Use
##Managers would also like to know if young Panthers, in particular, are avoiding developed land 
##cover types. 
##Instructions: Use the landcover provided and the tracking data for subadult cats (age class is SA 
##for subadult). The selection method that you use is up to you. Rather than comparing the results 
##from different methods (as above), just use one approach and describe your approach with a 
##justification of why you think this is the best approach to address the question.
##Hint: for urban, land_sub==8
##In your response include:
##1. a description of and justification for the resource selection design approach and statistical test
##used to answer the question
##2. the statistical results 
##3. your interpretation of the results, answering the question for managers.





