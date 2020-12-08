
library(raster)
library("KernSmooth")
library("raster")
require(ggplot2)

set.seed(123)

loc <- data.frame(x=sample(1:500, size = 400, replace = T), y=sample(1:500, size = 200, replace = T))
loc$type = c(rep("red", 100), rep("blue", 100), rep("green", 100), rep("purple", 100))
loc_red <- loc[loc$type == "red", c("x", "y")]
loc_blue <- loc[loc$type == "blue", c("x", "y")]
loc_green <- loc[loc$type == "green", c("x", "y")]
loc_purple <- loc[loc$type == "purple", c("x", "y")]


# compute the 2D binned kernel density estimate
est <- bkde2D(loc_blue, 
              bandwidth=c(10,10), #How do define bandwidth?
              gridsize=c(max(loc$x), max(loc$y)),
              range.x=list(c(min(loc$x), max(loc$x)),c(min(loc$y), max(loc$y))))

est.raster = raster(list(x=est$x1,y=est$x2,z=est$fhat))
plot(est.raster)

#Standardise between 0 and 1
r= est.raster
r_est <- (r-cellStats(r,"min"))/(cellStats(r,"max")-cellStats(r,"min"))

distro_red <- extract(r_est, loc_red, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
        fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
distro_purple <- extract(r_est, loc_purple, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                      fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
distro_green <- extract(r_est, loc_green, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                      fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)

df_green = data.frame(val=distro_green, class="green")
df_red = data.frame(val=distro_red, class="red")
df_purple = data.frame(val=distro_purple, class="purple")

df_niche = rbind(df_green, df_purple, df_red)


require(ggplot2)

ggplot(data=df_niche, aes(x=val, fill=class)) +
scale_fill_manual(values=c("green", "purple", "red"))+
geom_histogram() +
guides(fill=F)+
facet_wrap( ~ class, ncol = 1)