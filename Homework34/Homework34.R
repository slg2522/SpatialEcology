#HW 3 & 4 ~ Autocorrelation and Understanding G, F, and K-tests
#Due Wednesday, October 5, 2022 at 11:00AM

#Assignment: This is a two-part HW assignment. The first part involves analyses of spatial structure in
#bird abundance data using variograms and correlograms (HW3). The second set of questions is related to
#analyses of three simulated point-pattern datasets (HW4). As in HW2, you will be graded on your answers
#and your ability to produce clean, well commented R code that performs the tasks listed below.

#PART 1 - HW3 ~ Variograms & Correlograms:
#With this assignment, you are provided with a raster map of estimated Carolina wren abundance in North
#America from the eBird database (carolinaWren.tif). Our goals are to (1) use correlograms to try to
#understand the spatial structure in these data and (2) variograms to inform how you might go about designing
#a field sampling study in the hopes of minimizing autocorrelation. Perform the following tasks and answer
#the associated questions.



#1. Use the sampleRegular function in the raster package to generate a sample of Carolina wren abundance
#at about 300-500 locations. To avoid sampling outside of the primary range of the Carolina wren (i.e.,
#outside of where abundance is relatively high), limit the extent of your sampling to the region where
#abundance is greater than zero. See Figure 1. The drawExtent function might be useful to help you
#define the sampling extent.







#Once you have generated the regular grid of samples, make a map that shows Carolina wren abundance
#and your sampling locations, but plot only those sampling locations that overlap the land surface, as
#shown below.




#Next, we will produce and plot a correlogram using the regular grid of abundance samples. I have found
#the correlog function in the ncf package to be one of the more easy methods to produce and plot
#correlograms in R. The following example code worked well for me:
#library(ncf)
#cor <- correlog(x=samps2@coords[,1],
#y=samps2@coords[,2],
#z=samps2@data$carolinaWren,
#increment = 75000,
#resamp = 1000)
#plot(cor)




#What is your interpretation of the resulting plot? In other words, what does it tell you about the spatial
#pattern in abundance of the Carolina wren? Do you think the correlogram would look different if we sampled
#random locations instead of using a regularly spaced grid? Keep in mind that we have VERY good data in
#this HW example, including a detailed raster map of range-wide abundance. Data this good are unusual (but
#becoming more common!) - so when answering these questions, try to think about what you could learn from
#just the correlogram if you had abundance data at a few dozen locations instead of these detailed data. This
#paper might be helpful to guide your interpretation:

#Brown, James H., David W. Mehlman, and George C. Stevens. “Spatial variation in abundance.” Ecology
#76.7 (1995): 2028-2043.




#4. Next, use the variogram function in the gstat package to calculate and plot a sample variogram from
#the abundance data. Note that you are not required to fit a statistical model, but you can if you want.
#Two questions: (1) Is the abundance pattern isotropic (use variograms to support your answer)? (2) If
#you were to design a study to sample abundance of the Carolina wren, is there a distances at which
#you could space the sample sites to eliminate spatial structure in the observations (again, refer to your
#variogram(s) to support your answer)?




#PART 2 - HW4 ~ point-pattern Analysis:
#For this assignment, write R scripts to complete the following tasks and answer each question.

#1. Simulate three types of point-patterns: (1) Complete Spatial Randomness, (2) clustered, and (3)
#segregated. For the point-pattern with CSR: what is lambda?





#2. Examine and interpret each of your simulated patterns using the G-, K- and F-tests.




#3. Plot: (i) your simulated point-patterns (be sure to add an appropriate title so I know whcih is which)
#and (ii) the results of the G-, K- and F-tests for each pattern. Provide a brief interpretation of the G-,
#K- and F-test results.





#Hints & Questions
#-See rThomas or rMatClust for functions to generate a clustered point-pattern.

#-There are different ways to produce a segregated point-pattern. One might be to create a grid of equally
#spaced points (50 or more) and then use the jitter function on the coordinates to add a bit of noise.
#See as.ppp to coerce the object to a ppp object as needed for the G-, K- and F-tests.