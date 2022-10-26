#Boosted Regression Tree Methods

# Important functions

# (1) gbm.step - Fits a gbm model to one or more response variables, 
# using cross-validation to estimate the optimal number of trees. 
# This requires use of the utility functions roc, calibration and calc.deviance.

# (2) gbm.fixed, gbm.holdout - Alternative functions for fitting gbm models, 
# implementing options provided in the gbm package.

# (3) gbm.simplify - Code to perform backwards elimination of variables, 
# to drop those that give no evidence of improving predictive performance.

# (4) gbm.plot - Plots the partial dependence of the response on one or 
# more predictors.

# (5) gbm.plot.fits - Plots the fitted values from a gbm object returned by 
# any of the model fitting options. This can give a more reliable guide to 
# the shape of the fitted surface than can be obtained from the individual 
# functions, particularly when predictor variables are correlated and/or 
# samples are unevenly distributed in environmental space.

# (6) gbm.interactions - Tests whether interactions have been detected and 
# modeled, and reports the relative strength of these. Results can be 
# visualised with 'gbm.perspec'.

# (7) predict.gdm - Predicts a fitted model to map distributions


## Load libraries and data -----------------------------------------------------
library(dismo)
library(gbm)
data(Anguilla_train)
head(Anguilla_train)

## ----Fit a gbm model ---------------------------------------------------------
?gbm.step # note defaults, e.g.,
# nfolds = 10 = 10-fold cross validation
# site.weights = 1 changing this can be useful when fitting with background data
# inital model to get started with lr = 0.01
angaus.tc5.lr01 <- gbm.step(data=Anguilla_train, # input data
                            gbm.x = 3:13, # columns of predictors
                            gbm.y = 2, # column of response (p-a)
                            family = "bernoulli", # for p-a data (binomial)
                            tree.complexity = 5, # model interactions, etc?
                            learning.rate = 0.01, # influence of each tree 
                            bag.fraction = 0.5) # proportion of data used to select variables

names(angaus.tc5.lr01) # lots of stuff in the model object
summary(angaus.tc5.lr01) # nice variable importnace plot


# Now, let's reduce the learning rate to lr=0.005
angaus.tc5.lr005 <- gbm.step(data=Anguilla_train, 
                             gbm.x = 3:13, 
                             gbm.y = 2,
                             family = "bernoulli",
                             tree.complexity = 5,
                             learning.rate = 0.005,
                             bag.fraction = 0.5)

summary(angaus.tc5.lr005)

# It could be worthwhile to systematically explore different settings for
# tree complexity, learning rate, and bag fraction and compare the results. 


# GBM model selection ---------------------------------------------------------
# can we remove predictors?
# Can be slow!
angaus.simp <- gbm.simplify(angaus.tc5.lr005, # a fitted GBM object
                            n.drops = 5) # how many variables to try dropping?
# plot suggest we can drop two variables
summary(angaus.simp)

# let's now fit a new model using the predictors that were retained
keepPreds <- angaus.simp$pred.list[[2]] # variables that were kept
# fit the new model
# note that there may be little harm in using all the original variables
angaus.tc5.lr005.simp <- gbm.step(Anguilla_train, # input data
                   gbm.x=keepPreds, # preds
                   gbm.y=2, #response
                   tree.complexity=5, 
                   learning.rate=0.005)


# GBM is great for building response curves
# here we plot all variables in a 4 x 3 panel layout
gbm.plot(angaus.tc5.lr005, # a model object
         n.plots=11, 
         plot.layout=c(4, 3), 
         write.title = FALSE)

# plot the fitted values in relation to each of the predictors 
# used in the model
gbm.plot.fits(angaus.tc5.lr005)


# We can also check if there are important interactions between variables
?gbm.interactions
find.int <- gbm.interactions(angaus.tc5.lr005)
find.int$interactions
find.int$rank.list # ranked by strength of interaction
# lets plot the interaction between var 7 (USRainDays) and
# var 1 (SegSumT)
gbm.perspec(angaus.tc5.lr005, # fitted model object
            x=7, # first variable to be plotted
            y=1, # second variable to be plotted
            y.range=c(15,20), # specify range for plotting
            z.range=c(0,0.6))


# Now that we have scrutinized the model, let's do some predictions
data(Anguilla_test) # testing data

# predict to the test data
preds <- predict.gbm(angaus.tc5.lr005, # fitted model to predict
                     Anguilla_test, # data to predict to
                     n.trees=angaus.tc5.lr005$gbm.call$best.trees, # see help
                     type="response") # predict probabilities

# evaluate the prediction to the test data
d <- cbind(Anguilla_test$Angaus_obs, preds)
pres <- d[d[,1]==1, 2]
abs <- d[d[,1]==0, 2]
e <- evaluate(p=pres, a=abs)
e
plot(e, 'ROC')

# now let's make some maps!
data(Anguilla_grids)
plot(Anguilla_grids) # stream variables

# Note that there is not a raster for "fishing method", so we will
# create a data frame 
Method <- factor('electric', levels = levels(Anguilla_train$Method))
add <- data.frame(Method)
p <- predict(Anguilla_grids, # raster stack 
             angaus.tc5.lr005, # fitted model
             const=add, # categorical fishing method
             n.trees=angaus.tc5.lr005$gbm.call$best.trees, # see help 
             type="response") # predict probabilities

plot(p, main='Angaus - BRT prediction')

