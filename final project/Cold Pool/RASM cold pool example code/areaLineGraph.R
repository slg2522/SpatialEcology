library(plotly)

setwd("D:/Cold Pool")
areaData <- read.csv("areaSummary.csv")
percentData <- read.csv("percentSummary.csv")

month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
           'August', 'September', 'October', 'November', 'December')

areaData$X <- month

#The default order will be alphabetized unless specified as below:
areaData$X = factor(areaData$X, levels = month.name)

#Make the Area Graph
fig <- plot_ly(x = ~areaData$X) 
fig <- fig %>% add_lines(y = ~areaData$X1980, name = "1980", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1981, name = "1981", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1982, name = "1982", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1983, name = "1983", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1984, name = "1984", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1985, name = "1985", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1986, name = "1986", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1987, name = "1987", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1988, name = "1988", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1989, name = "1989", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1990, name = "1990", line = list(shape = "linear"))

fig <- fig %>% add_lines(y = ~areaData$X1991, name = "1991", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1992, name = "1992", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1993, name = "1993", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1994, name = "1994", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1995, name = "1995", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1996, name = "1996", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1997, name = "1997", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1998, name = "1998", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X1999, name = "1999", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2000, name = "2000", line = list(shape = "linear"))

fig <- fig %>% add_lines(y = ~areaData$X2001, name = "2001", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2002, name = "2002", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2003, name = "2003", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2004, name = "2004", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2005, name = "2005", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2006, name = "2006", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2007, name = "2007", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2008, name = "2008", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2009, name = "2009", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2010, name = "2010", line = list(shape = "linear"))

fig <- fig %>% add_lines(y = ~areaData$X2011, name = "2011", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2012, name = "2012", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2013, name = "2013", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2014, name = "2014", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2015, name = "2015", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2016, name = "2016", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2017, name = "2017", line = list(shape = "linear"))
fig <- fig %>% add_lines(y = ~areaData$X2018, name = "2018", line = list(shape = "linear"))

#formatting
fig <- fig %>% layout(title = "Area of < -1C Bottom Water (RASM)",
                      xaxis = list(title = "Months",
                                   showgrid = FALSE,
                                   showline = TRUE,
                                   showticklabels = TRUE,
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Area (Raster Tiles)",
                                   showgrid = FALSE,
                                   showline = TRUE,
                                   showticklabels = TRUE,
                                   ticks = 'outside',
                                   zeroline = FALSE))

fig


#Make the Percent Graph
percentData$X <- month

#The default order will be alphabetized unless specified as below:
percentData$X = factor(percentData$X, levels = month.name)

per <- plot_ly(x = ~areaData$X) 
per <- per %>% add_lines(y = ~percentData$X1980, name = "1980", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1981, name = "1981", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1982, name = "1982", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1983, name = "1983", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1984, name = "1984", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1985, name = "1985", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1986, name = "1986", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1987, name = "1987", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1988, name = "1988", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1989, name = "1989", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1990, name = "1990", line = list(shape = "linear"))

per <- per %>% add_lines(y = ~percentData$X1991, name = "1991", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1992, name = "1992", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1993, name = "1993", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1994, name = "1994", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1995, name = "1995", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1996, name = "1996", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1997, name = "1997", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1998, name = "1998", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X1999, name = "1999", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2000, name = "2000", line = list(shape = "linear"))

per <- per %>% add_lines(y = ~percentData$X2001, name = "2001", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2002, name = "2002", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2003, name = "2003", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2004, name = "2004", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2005, name = "2005", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2006, name = "2006", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2007, name = "2007", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2008, name = "2008", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2009, name = "2009", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2010, name = "2010", line = list(shape = "linear"))

per <- per %>% add_lines(y = ~percentData$X2011, name = "2011", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2012, name = "2012", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2013, name = "2013", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2014, name = "2014", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2015, name = "2015", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2016, name = "2016", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2017, name = "2017", line = list(shape = "linear"))
per <- per %>% add_lines(y = ~percentData$X2018, name = "2018", line = list(shape = "linear"))

#formatting
per <- per %>% layout(title = "Percent of Extent Occupied by < -1C Bottom Water (RASM)",
                      xaxis = list(title = "Months",
                                   showgrid = FALSE,
                                   showline = TRUE,
                                   showticklabels = TRUE,
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Percent",
                                   showgrid = FALSE,
                                   showline = TRUE,
                                   showticklabels = TRUE,
                                   ticks = 'outside',
                                   zeroline = FALSE))

per
