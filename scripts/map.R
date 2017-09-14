###############################################################################
## MAPS for PopHzn 14.2. "spatial" patterns paper
###############################################################################
## 0. preliminaries
## 1. plot
###############################################################################


## 0. preliminaries
###############################################################################
require(tidyverse)
require(rworldmap)
require(rworldxtra)
library(RColorBrewer)
.pardefault <- par(no.readonly = T)

## get cluster membership data and country list
df <- read.csv("data/clusters.csv")

# get country and region code table
urlfile<-'https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv'
dsin <-read.csv(urlfile)
## This looks like a very useful file, so I'll save it for future use:
write.csv(dsin, file = "data/ISO.country.codes.csv", row.names = FALSE)
## ToDo: at some point add UN regions!

## get data frame ready
left_join(df, dsin, by = c("Country" = "name")) %>% 
  droplevels() -> df

# ## starter map
# sPDF <- getMap(resolution = "li")

## add the data to the starter map
sPDF <- joinCountryData2Map(df, joinCode="ISO3", nameJoinColumn="ISO3166.1.Alpha.3")

## select mena subset
sPDF.mena<-sPDF[which(sPDF$ISO3 %in% df$ISO3166.1.Alpha.3),]


## 1. plot
###############################################################################


par(.pardefault)
cluster1 <- c(brewer.pal(8, "YlOrRd"))[3:5]
cluster2 <- c(brewer.pal(8, "PuBu"))[5:7]
cluster3 <- c(brewer.pal(8, "YlGnBu"))[3:5]
pal1 <- c(cluster1[1], cluster2[1], cluster3[1])
pal2 <- c(cluster1[2], cluster2[2], cluster3[2])
pal3 <- c(cluster1[3], cluster2[3], cluster3[3])

leg.x <- -12
leg.y <- 45
w = 5
h = 2.5
cex = 0.9
yi = 1.2
#par(mfrow=c(3,1))
par(xpd = TRUE)
par(.pardefault)
par(mar = c(1,1,1,1))


# plot background
plot(sPDF, xlim = c(-12, 62), ylim = c(15, 42),
     border="grey70", col="white")


mapPolys(sPDF.mena, nameColumnToPlot = "Clustering50",addLegend=FALSE,
         mapTitle ="",  borderCol="white", numCats = 3, catMethod = "categorical",
         add = TRUE, colourPalette = pal1)
mtext( "A1", cex = 1)
legend(x=leg.x, y=leg.y,c("C11", "C21", "C31") , fill = pal1,
       cex = cex ,bty = "n", y.intersp = yi)


dev.copy2eps(file="figures/map1.eps", height=h, width=w)


# plot background
plot(sPDF, xlim = c(-12, 62), ylim = c(15, 42),
     border="grey70", col="white")

mapPolys(sPDF.mena, nameColumnToPlot = "Clustering80",addLegend=FALSE,
         mapTitle ="",  borderCol="white", numCats = 3, catMethod = "categorical",
         add = TRUE, colourPalette =pal2)

mtext( "A2", cex = 1)

legend(x=leg.x, y=leg.y,c("C12", "C22", "C32") , fill = pal2,
       cex = cex ,bty = "n", y.intersp = yi)
dev.copy2eps(file="figures/map2.eps", height=h, width=w)

# plot background
plot(sPDF, xlim = c(-12, 62), ylim = c(15, 42),
     border="grey70", col="white")

mapPolys(sPDF.mena, nameColumnToPlot = "Clustering10",addLegend=FALSE,
         mapTitle ="",  borderCol="white", numCats = 3, catMethod = "categorical",
         add = TRUE, colourPalette = pal3)
mtext( "A3", cex = 1)

legend(x=leg.x, y=leg.y,c("C13", "C23", "C33") , fill = pal3,
       cex = cex ,bty = "n", y.intersp = yi)
dev.copy2eps(file="figures/map3.eps", height=h, width=w)

par(.pardefault)

