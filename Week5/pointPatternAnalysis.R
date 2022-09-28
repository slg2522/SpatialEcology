
# Example point-pattern analyses in R  

# load libraries
library(spatstat)

# Let's start with the simplest point pattern: complete spatial randomness (CSR).
# homogeneous Poisson process
set.seed(698) 
dims <- 100 # window size
pts <- 500 # number of points
lambda <- pts/dims/dims # what is the definition of lambda?
lambda

# generate a point pattern with csr using the rpoispp function
csr <- rpoispp(lambda, 
               win=owin(xrange=c(0,dims), yrange=c(0,dims)),
               nsim=1) 

# Let's look at the details of the csr object
summary(csr)
summary(csr)$intensity

# plot the csr point-pattern
plot(csr, pch=20)

# note that if we want an exact number of 'events', we can:
csr.n <- runifpoint(1000) # gives n points exactly
plot(csr.n, pch=20)

### Now, let's make intensity vary across space, i.e., an inhomogeneous 
# Poisson process (ipp) -> First-order structure.
# Instead of a number (n/A), lambda is now a function to vary intensity across space
lambda <- function(x,y) {500 * (x^3+y^3)} 
ipp <- rpoispp(lambda)

# let's plot the result & do a simple quadrat count summary
par(mfrow=c(1,2)) # two-panel (1 row, 2 columns) plot window
plot(ipp, pch=20)
summary(ipp)
quadratcount(ipp)
plot(quadratcount(ipp), add=T) #lay the points on top of the quadrat
# a few other ways to visualize the pattern
plot(density(ipp))
contour(density(ipp), add=T) #plot the density with contours
dev.off()

#example of a completely random process
#likely not first order structure
plot(density(csr.n))
contour(density(csr.n), add=T)

### Initial steps to infer process from pattern: a test for CSR
# test of Complete Spatial Randomness for a given point pattern
# based on quadrat counts
plot(csr, pch=20)
plot(ipp, pch=20)
#for the random pattern, the MonteCarlo is insignificant
quadrat.test(csr, method="MonteCarlo") # not significant
quadrat.test(ipp, method="MonteCarlo") #significant (for first order structure)

# plot the test graphic
#provides the count (left), expected value (right), and test statistic (bottom)
plot(quadrat.test(csr, method="MonteCarlo"), main="csr")
plot(quadrat.test(ipp, method="MonteCarlo"), main="ipp")

# Kolmogorov-Smirnov (KS) test
#first statistical test
#cdf=cumulative distribution function
cdf.test(csr, # point pattern
         "x", # covariate - in this case, the 'x' coordinate
         test="ks") # test to be performed ks = Kolmogorov-Smirnov
cdf.test(ipp, "x", test="ks") # significant
cdf.test(ipp, "y", test="ks") # significant

par(mfrow=c(1,3))
plot(cdf.test(csr, "x"), main="K-S test, csr") #random pattern with no deviation
plot(cdf.test(ipp, "x"), main="K-S test-x, ipp") #significant deviation
plot(cdf.test(ipp, "y"), main="K-S test-y, ipp") #significant deviation
dev.off()


# Now, let's look at some distance-based tests
### Second-order structure, but not first order
set.seed(978)
# random point pattern from a Thomas cluster process
?rThomas
 # note kappa = intensity of cluster centers
thom <- rThomas(kappa=30, scale=0.02, mu=10, win=c(0,1,0,1))
plot(thom, pch=20,  main="Thomas process with constant intensity") #fairly clustered dataset with only second order structure

# F = Empty space function = point-to-nearest-event distribution
?Fest
Fest(csr, # point pattern
     correction = "none") # none is specified as which edge correction to use?
Fest(thom, correction = "none")

# F-test on pattern with csr (no distance-dependent interactions or spatial structure)
par(mfrow=c(1,2))
plot(csr)
plot(Fest(csr, correction = "none"), main="F-test, csr")
dev.off()

# F-test on ipp pattern
par(mfrow=c(1,2))
plot(thom, pch=20)
plot(Fest(thom, correction = "none"), main="F-test, 2nd-order") # departs from theoretical expectation
dev.off()

# G = Nearest Neighbour Distance function = event-to-event distribution
par(mfrow=c(1,2))
plot(csr)
plot(Gest(csr, correction = "none"), main="G-test, 2nd-order") #no departure from theoretical
dev.off()
par(mfrow=c(1,2))
plot(thom, pch=20)
plot(Gest(thom, correction = "none"), main="G-test, 2nd-order")
dev.off()

# Ripley's K
par(mfrow=c(1,2))
plot(Kest(csr, correction = "none"), main="K-test, csr")
plot(Kest(thom, correction = "none"), main="K-test, 2nd-order") #significant clustering at small distances but that goes away as distance increases

### Envelope tests employ a Monte Carlo approach to provide a means to test 
# for statistical significance
#Envelopes
par(mfrow=c(1,2))
env.csr <- envelope(csr, Kest, nsim=99, nrank=1) #test you want to run, number of simulations, and rank (1 means minimum and maximum values generated will be the envelope bounds) 
plot(env.csr, main="K-test, csr with envelope")

#the non-random pattern with second order structure:
env.thom <- envelope(thom, Kest, nsim=99, nrank=1)
plot(env.thom, main="K-test, 2nd-order with envelope") #clustering at small values of r and some at larger ones (less important)

# The cells dataset shows significant dispersion to distances of r~0.15
?cells
par(mfrow=c(1,2))
plot(cells, pch=20)
env.cells <- envelope(cells, Kest, nsim=99, nrank=1)
#evidence for significant over dispersion
plot(env.cells, main="K-test on cells with envelope")
# the range of simulated values is quite large - why might that be the case? 1) you may need to run more simulations under the 95% confidence interval instead of the min and max; 2) there are not that many datapoints (only 42)

### The redwood dataset shows significant clustering to distances of r~0.2
?redwood
par(mfrow=c(1,2))
plot(redwood, pch=20)
plot(envelope(redwood, Kest, nsim=99, nrank=1), main="K-test on redwood with envelope")
#significant clustering at a range of distances
#stair-step like patterns reflect a low amount of data
dev.off()

# generate a point pattern with both first- and second-order structure
set.seed(983)
kappa <- function(x,y) {50*exp(-5*x*y)} #intensity of cluster centers
thom.ipp <- rThomas(kappa, scale=0.02, mu=10, c(0,1,0,1))
plot(thom.ipp)
#both first (variation of intensity of the clusters across space) and second order (clear clustering) structure
?Kinhom
#inhomogeneous K-function
plot(Kinhom(thom.ipp, correction="best")) #use best edge correction
#if you don't accomodate the edge effects (ie. specify non) the theoretical envelope is outside the line
#very strong spatial structure with the clear clustering and some dispersion at large radii
plot(envelope(thom.ipp, Kinhom, nsim=999, correction="best"))

# Analysis of marked point patterns
set.seed(4321)

# Exploratory analyses / plotting
data(lansing)
?lansing
#frequency of each mark type, the proportion of the data, and the intensity under unit area of 1
summary(lansing)
plot(lansing)
#plot each species individually
plot(split(lansing))
#split by species with density graph
plot(density(split(lansing)))

# simulating Poisson marked point processes, multivariate
# same number of points for each 
par(mfrow=c(1,2))
#random marked poison distribution point process
dat1 <- rmpoispp(100, types=c("A", "B", "C")) #average of 100
plot(dat1)

# different number of points
dat2 <- rmpoispp(c(100, 50, 20), types=c("A", "B", "C")) #different numbers for each marked point type
plot(dat2)


### Compare point patterns with different marks
dat <- rmpoispp(500, types=c("Plant", "Ant")) #one type if plant one type is plant
#expect neither plants nor ants to be aggregated or segregated with not interaction between them (due to specified random marks by poisson point process)
plot(dat)

# summary functions
?Gcross
plot(Gcross(dat, "Ant", "Plant")) #the legend shows the different ways of estimating the gfunction
?alltypes
#alltypes to see the different interactions
plot(alltypes(dat, "G")) #pairwise matrix of the different marked types


# generalization of the function Gest to multitype point patterns
data(ants)
plot(ants) #looks like they may aggregate nonrandomly
plot(Gcross(ants, "Cataglyphis", "Messor")) #not a lot of data so stairstep pattern, some evidence of clustering at larger distances and some evidence of segregation at shorter distances
# deviation from theoretical may suggest dependence between the points
plot(alltypes(ants, "G"))
plot(Gdot(ants, "Messor")) # i-to-any test (dispersion at all scales?)

#all of the possible pairwise comparisons across the species
#evidence of segregation between hickory and maple
#black oak segregates from miscellaneous
plot(alltypes(lansing, "G"))
plot(alltypes(lansing, "G", envelope = T))


################################################################################
# Fitting spatial models to point patterns 
dev.off()
dat <- rpoispp(500)
log(500)
ppm(dat, ~1)

set.seed(1004)
lambda <- function(x, y){
  100*exp(-3*x+4*y)
}

pp <- rpoispp(lambda)
plot(pp, pch=20)

mle <- ppm(pp, ~x+y)
mle
coef(mle)

### Now, lets include covariates that might be driving spatial variation in intensity.
# load and plot tree data
data(bei)
?bei
grad <- bei.extra$grad # slope
par(mfrow=c(1,2))
plot(grad, main="Slope")
points(bei, pch=20, col=rgb(0,0,0,0.5))
plot(density(bei), main="Density")
points(bei, pch=20, col=rgb(0,0,0,0.5))

# run spatial models on tree data using slope (grad) as a covariate
mle.bei <- ppm(bei, ~slope, 
               covariates=list(slope=grad), 
               na.action="na.exclude")
mle.bei
#plot(predict(mle.bei, type = "trend"))

# create a random covaraite and fit model
rando <- bei.extra$grad
rando[] <- runif(20301)
bei.extra$rando <- rando
mle.bei <- ppm(bei, ~rando, 
               covariates=list(rando=rando), 
               na.action="na.exclude")
mle.bei

# run spatial models on tree data using both slope (grad) and elevation 
elev <- bei.extra$elev
par(mfrow=c(1,2))
plot(elev, main="Elevation")
points(bei, pch=20, col=rgb(0,0,0,0.5))
plot(density(bei), main="Density")
points(bei, pch=20, col=rgb(0,0,0,0.5))
mle.bei <- ppm(bei, ~elev+grad+rando, covariates=bei.extra)
mle.bei
#plot(predict(mle.bei, type = "trend"))

# run spatial models on tree data w/ two covariates and trend
mle.bei <- ppm(bei, ~x+elev+grad+rando, covariates=bei.extra)
mle.bei
#plot(predict(mle.bei, type = "trend"))
dev.off()

### We can also check how well the models fit the observed spatial 
# pattern & perform model selection
M <- quadrat.test(mle.bei, nx = 4, ny = 2) #rejects fitted model
M
plot(bei, pch = ".")
plot(M, add = TRUE, cex = 1.5, col = "red")

plot(predict(mle.bei))
plot(bei, add = TRUE, pch = "+")
diagnose.ppm(mle.bei, which = "smooth")

#Lurking variable plot
fitx <- ppm(bei, ~x)
?lurking
lurking(fitx, grad, type="raw")

fit.slope <- ppm(bei, ~grad, covariates=list(grad=grad))
lurking(fit.slope, elev, type="raw")

# model selection
mle1 <- ppm(bei, ~grad, covariates=list(grad=grad))
mle2 <- ppm(bei, ~elev+grad, covariates=bei.extra)
mle.null <- ppm(bei, ~1)

# likelihood ratio test
anova(mle.null, mle1, test="Chi")
anova(mle.null, mle2, test="Chi")
anova(mle1, mle2, test="Chi")

#AIC
mle.prop <- ppm(bei, ~offset(log(grad)), covariates=list(grad=grad))
AIC(mle.prop)
AIC(mle.null)
AIC(mle.null) - AIC(mle.prop)

# Check if the covariates account for the spatial trend. 
pred <- predict(mle.bei, locations=bei)
Ki <- Kinhom(bei, pred)
plot(Ki, main="Inhomogeneous K function")

# Model fitting for marked patterns
ppm(dat, ~marks) # marks is a factor, model has one parameter for each level

ppm(lansing, ~marks)
ppm(lansing, ~marks+x) # log(lambda(x,y,m)) = am + bx (different intercept, same slope)
ppm(lansing, ~marks*x) # log(lambda) = am + bmx (different slope and intercept)

