install.packages('nutshell')
library(nutshell)
data(sanfrancisco.home.sales)

-------------------------------------------------------------------------------------------------------------------
# load in the shapefile
install.packages('sp')
library(sp)
install.packages('maptools')
library(maptools)
# ca.neighborhood.shapes <- read.shape("ZillowNeighborhoods-CA.shp")
ca.neighborhood.shapes <- readShapePoly("ZillowNeighborhoods-CA.shp")
# extract san francisco coordinates
sf.neighborhood.shapes <-
  ca.neighborhood.shapes[ca.neighborhood.shapes$CITY=="San Francisco",]
# function to look up shapes
neighborhood <- function(s, lon, lat) {
  names <- s$NAME;
  for (name in names) {
    lons <- s[s$NAME==name,]@polygons[[1]]@Polygons[[1]]@coords[,1];
    lats <- s[s$NAME==name,]@polygons[[1]]@Polygons[[1]]@coords[,2];
    res <- point.in.polygon(lon,lat,lons,lats);
    if (res==1) {
      return(name);
    }
  }
  return(NA);
}
map_neighborhoods <- function(s, lons, lats) {
  neighborhoods <- rep(NA,length(lons));
  for (i in 1:length(lons)) {
    neighborhoods[i] <- neighborhood(s, lons[i], lats[i]);
  }
  return(neighborhoods);
}
# loading sf data with coordinates
sanfrancisco.home.sales.raw <- read.csv("san_fran_re_sales_wcoors.csv")
# exclude bad coordinates (outside SF)
sanfrancisco.home.sales.clean <- transform(sanfrancisco.home.sales.raw,
                                           latitude=ifelse(latitude>37.7&latitude<37.85,latitude,NA),
                                           longitude=ifelse(latitude>37.7&latitude<37.85,longitude,NA),
                                           date=as.Date(date,format="%m/%d/%Y"),
                                           lotsize=ifelse(lotsize<10000,lotsize,NA),
                                           month=cut(as.Date(date,format="%m/%d/%Y"),"month"),
                                           lotsize=ifelse(lotsize<15000,lotsize,NA)
)
# transform date fields

# finally, build the data set with properly named neighborhoods
sanfrancisco.home.sales <- transform(sanfrancisco.home.sales.clean,
                                     neighborhood=map_neighborhoods(
                                       sf.neighborhood.shapes, longitude, latitude))
save(sanfrancisco.home.sales,file="sanfrancisco.home.sales.RData")

-------------------------------------------------------------------------------------------------------------------
#Intuitively, it doesn’t make sense for real estate prices to be normally distributed.
#There are far more people with below-average incomes than above-average incomes.
#The lowest recorded price in the data set is $100,000; the highest is $9,500,000.
#Let’s take a look at this distribution with qqmath:

install.packages('lattice')
library(lattice)
qqmath(~price,data=sanfrancisco.home.sales)

#As expected, the distribution is not normal.
#It looks exponential, so let’s try a log transform:

qqmath(~log(price),data=sanfrancisco.home.sales)
#A log transform yields a distribution that looks pretty close to normally distributed
# To do this, we’ll split the distribution into groups and change
# the way the points are plotted. Specifically, we’ll plot smooth lines instead of individual
# points. (Point type is actually an argument for panel.xyplot, which is used to
#          draw the chart.) We’ll add a key to the plot (using auto.key=TRUE). We’ll pass an
# explicit subset as an argument to the function instead of using the subset argument.
# (This helps clean up the key, which would show unused factor levels otherwise.)
qqmath(~log(price),groups=bedrooms,
       + subset(sanfrancisco.home.sales,!is.na(bedrooms)&bedrooms>0&bedrooms<7), auto.key=TRUE,drop.unused.levels=TRUE,type="smooth")
dev.off()
# Notice that the lines are separate, with higher values for higher numbers of bedrooms
#. We can do the same thing for square footage. (I used the function cut2 from the package 
#HMisc to divide square footages into six even quantiles.)
install.packages('Hmisc')
library(Hmisc)
qqmath(~log(price),groups=cut2(squarefeet,g=6),
       + data=subset(sanfrancisco.home.sales,!is.na(squarefeet)),
       + auto.key=TRUE, drop.unused.levels=TRUE, type="smooth")
#Here, the separation is even more clear. We can see the same separation by neighborhood.

#Bivariate Trellis Plots
xyplot(price~squarefeet,data=sanfrancisco.home.sales)
table(subset(sanfrancisco.home.sales,!is.na(squarefeet),select=zip))

# The results of this command are shown. It looks like there is a rough
# correspondence between size and price (the plot looks vaguely cone shaped). This
# chart is hard to read, so let’s try modifying it. Let’s trim outliers (sales prices over
# 4,000,000 and properties over 6,000 square feet) using the subset argument. Additionally,
# let’s take a look at how this relationship varies by zip code. San Francisco
# is a pretty big place, and not all neighborhoods are equally in demand. (You probably
# know the cliché about the first three rules of real estate: location, location, location.)
# So, additionally, let’s try splitting this relationship by zip code.
# Before plotting the price data, let’s pick a subset of zip codes to plot. A few parts of
# the city are sparsely populated (like the financial district, 94104) and don’t have
# enough data to make plotting interesting. Also, let’s exclude zip codes where square
# footage isn’t available:

table(subset(sanfrancisco.home.sales,!is.na(squarefeet),select=zip))
# So, we’ll exclude 94100, 94104, 94108, 94111, 94133, and 94158 because there are
# too few sales to be interesting. (Note the strip argument. This simply prints the zip
#                                   codes with the plots.)
trellis.par.set(fontsize=list(text=7))
xyplot(price~squarefeet|zip, data=sanfrancisco.home.sales,
       + subset=(zip!=94100 & zip!=94104 & zip!=94108 &
                   + zip!=94111 & zip!=94133 & zip!=94158 &
                   + price<4000000 &
                   + ifelse(is.na(squarefeet),FALSE,squarefeet<6000)),
       + strip=strip.custom(strip.levels=TRUE))  
# The resulting plot is shown . Now, the linear relationship is much
# more pronounced. Note the different slopes in different neighborhoods. As you
# might expect, some up-and-coming neighborhoods (like zip code 94110, which includes
#the Mission and Bernal Heights) are more shallowly sloped, while ritzy neighborhoods
# (like zip code 94123, which includes the Marina and Cow Hollow) are
# more steeply sloped.

# We can make this slightly more readable by using neighborhood names. Let’s rerun
# the code, conditioning by neighborhood. We’ll also add a diagonal line to each plot
# (through a custom panel function) to make the charts even easier to read. We’ll also
# change the default points plotted to be solid (through the pch=19 argument) and
# shrink them to a smaller size (through the cex=.2 argument):
trellis.par.set(fontsize=list(text=7))
dollars.per.squarefoot <- mean(
  + sanfrancisco.home.sales$price / sanfrancisco.home.sales$squarefeet,
  + na.rm=TRUE);
xyplot(price~squarefeet|neighborhood,
       + data=sanfrancisco.home.sales,
       + pch=19,
       + cex=.2,
       + subset=(zip!=94100 & zip!=94104 & zip!=94108 &
                   zip!=94111 & zip!=94133 & zip!=94158 &
                   price<4000000 &
                   ifelse(is.na(squarefeet),FALSE,squarefeet<6000)),
       + strip=strip.custom(strip.levels=TRUE,
                            + horizontal=TRUE,
                            + par.strip.text=list(cex=.8)),
       + panel=function(...) {
         + panel.abline(a=0,b=dollars.per.squarefoot);
         + panel.xyplot(...);
       }
        )

# Box plots in lattice
# The San Francisco home sales data set was taken from a particularly interesting time:
# the housing market crash. (The market fell a little late in San Francisco compared
# to other cities.) Let’s take a look at how prices changed over time during this period.
# We could plot just the median price or mean price, or the number of sales. However,
# the lattice package gives us tools that will let us watch how the whole distribution
# changed over time. Specifically, we can use box plots.
# 
# Box plots in the lattice package are just like box plots drawn with the graphics
# package, as described in “Box Plots” on page 240. The boxes represent prices from
# the 25th through the 75th percentiles (the interquartile range), the dots represent
# median prices, and the whiskers represent the minimum or maximum values. (When
# there are values that stretch beyond 1.5 times the length of the interquartile range,
# the whiskers are truncated at those extremes.)

# Let’s show a set of box plots, with one plot per month. We’ll need to round the date
# ranges to the nearest month. A convenient way to do this in R is with the cut function.
# Here’s the number of sales by month in this data set:
table(cut(sanfrancisco.home.sales$saledate,"month"))
# As you may remember from above, the cutoff dates don’t fall neatly on the beginning
# and ending of each month:
min(sanfrancisco.home.sales$saledate)
max(sanfrancisco.home.sales$saledate)
# So don’t focus too much on the volumes in February 2008 or July 2009. (Volume
# was much lower in the spring.) Let’s take a look at the distribution of sales prices
# by month. Here’s the code to present this data using the default representation:
bwplot(price~cut(saledate,"month"),data=sanfrancisco.home.sales)
# Unfortunately, this doesn’t produce an easily readable plot, as you can see in Figure.
# It’s clear that there are a large number of outliers that are making the plot
# hard to see. Box plots assume a normal distribution, but this doesn’t make
# intuitive sense for real estate prices (as we saw in “Univariate quantile-quantile
# plots” on page 284). Let’s try plotting the box plots again, this time with the logtransformed
# values. To make it more readable, we’ll change to vertical box plots
# and rotate the text at the bottom:
bwplot(log(price)~cut(saledate,"month"),
       + data=sanfrancisco.home.sales,
       + scales=list(x=list(rot=90)))
# Taking a look at the plot (shown in Figure 15-26), we can more clearly see some
# trends. Median prices moved around a lot during this period, though the interquartile
# range moved less. Moreover, it looks like sales at the high end of the market
# slowed down quite a bit (looking at the outliers on the top and the top whiskers).
# But, interestingly, the basic distribution appears pretty stable from month to month.

#Level Plots

# As an example of level plots, we will look at the San Francisco home sales data set.
# Let’s start by looking at the number of home sales in different parts of the city. To
# do this, we’ll need to use that coordinate data in the San Francisco home sales data
# set. Unfortunately, we can’t use the coordinates directly; the coordinates are too
# precise, so the levelplot function simply plots a large number of points. (Try executing
# levelplot(price~latitude+longitude) to see what I mean.)
# We’ll need to break the data into bins and count the number of homes within each
# bin. To do this, we’ll use the table and cut functions:
attach(sanfrancisco.home.sales)
levelplot(table(cut(longitude,breaks=40),
                cut(latitude,breaks=40)),
          scales=list(y=list(cex=.5),
                      x=list(rot=90,cex=.5)))
# This plot is shown in Figure 15-27. If we were interested in looking at the average
# sales price by area, we could use a similar strategy. Instead of table, we’ll use the
# tapply function to aggregate observations. And while we’re at it, we’ll cut out the
# axis labels:
levelplot(tapply(price,
                 + INDEX=list(cut(longitude,breaks=40),
                              + cut(latitude, breaks=40)),
                 + FUN=mean),
          + scales=list(draw=FALSE))
# This figure is shown in Figure 15-28. And, of course, you can use conditioning values
# with level plots. Let’s look at the number of home sales, by numbers of bedrooms.
# We’ll simplify the data slightly by looking at houses with zero to four bedrooms and
# then houses with five bedrooms or more. We’ll also cut the number of breaks to
# keep the charts legible:
bedrooms.capped <- ifelse(bedrooms<5,bedrooms,5);
levelplot(table(cut(longitude,breaks=25),
                cut(latitude,breaks=25),
                bedrooms.capped),
          scales=list(draw=FALSE))
