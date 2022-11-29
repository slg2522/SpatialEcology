library(statespacer)

area <- read.csv("D:/Cold Pool/areaSummary.csv")
percent <- read.csv("D:/Cold Pool/percentSummary.csv")

a <- c(area$X1980,area$X1981,area$X1982,area$X1983,area$X1984, area$X1985,
       area$X1986, area$X1987, area$X1988, area$X1989, area$X1990, area$X1991,
       area$X1992, area$X1993, area$X1994, area$X1995, area$X1996, area$X1997,
       area$X1998, area$X1999, area$X2000, area$X2001, area$X2002, area$X2003,
       area$X2004, area$X2005, area$X2006, area$X2007, area$X2008, area$X2009,
       area$X2010, area$X2011, area$X2012, area$X2013, area$X2014, area$X2015,
       area$X2016, area$X2017, area$X2018)

p <- c(percent$X1980,percent$X1981,percent$X1982,percent$X1983,percent$X1984, percent$X1985,
       percent$X1986, percent$X1987, percent$X1988, percent$X1989, percent$X1990, percent$X1991,
       percent$X1992, percent$X1993, percent$X1994, percent$X1995, percent$X1996, percent$X1997,
       percent$X1998, percent$X1999, percent$X2000, percent$X2001, percent$X2002, percent$X2003,
       percent$X2004, percent$X2005, percent$X2006, percent$X2007, percent$X2008, percent$X2009,
       percent$X2010, percent$X2011, percent$X2012, percent$X2013, percent$X2014, percent$X2015,
       percent$X2016, percent$X2017, percent$X2018)

z <- matrix(a)
w <- matrix(p)

fit <- statespacer(y = z,
                   local_level_ind = TRUE,
                   initial = 10)

plot(1:468, fit$function_call$y, type = 'p', 
     xlab = "time (months past Jan 1980)", ylab = "area (raster tiles)")
lines(1:468, fit$smoothed$level, type = 'l')
lines(1:468, fit$smoothed$level + qnorm(0.95) * sqrt(fit$smoothed$V[1,1,]),
      type = 'l', col = 'gray'
)
lines(1:468, fit$smoothed$level - qnorm(0.95) * sqrt(fit$smoothed$V[1,1,]),
      type = 'l', col = 'gray'
)
