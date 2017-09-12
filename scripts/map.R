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
pal1 <- c(brewer.pal(8, "Set3")[c(4,3)], brewer.pal(8, "Set2")[c(6)])
pal2 <- c(brewer.pal(8, "Set2")[c(4,3)],brewer.pal(8, "Dark2")[c(6)])
pal3 <- c(brewer.pal(8, "Dark2")[c(4,5)], brewer.pal(5, "Accent")[5])
leg.x <- -20
leg.y <- 45
par(mfrow=c(3,1))
par(mar = c(1,1,2,1))
par(xpd = TRUE)
# plot background
plot(sPDF, xlim = c(-12, 62), ylim = c(10, 42),
     border="grey70", col="white")

mapPolys(sPDF.mena, nameColumnToPlot = "Clustering50",addLegend=FALSE,
         mapTitle ="",  borderCol="white", numCats = 3, catMethod = "categorical",
         add = TRUE, colourPalette = pal1)
mtext( "Clustering 1950/55", cex = 1.5)
legend(x=leg.x, y=leg.y,c("Cluster 1", "Cluster 2", "Cluster 3") , fill = pal1,
       title = "Clustering 1950/55", 
       cex = 1.5 , bty = "n", y.intersp = 0.5)

# plot background
plot(sPDF, xlim = c(-12, 62), ylim = c(10, 42),
     border="grey70", col="white")

mapPolys(sPDF.mena, nameColumnToPlot = "Clustering80",addLegend=FALSE,
         mapTitle ="",  borderCol="white", numCats = 3, catMethod = "categorical",
         add = TRUE, colourPalette =pal2)

mtext( "Clustering 1980/85", cex = 1.5)

legend(x=leg.x, y=leg.y,c("Cluster 1", "Cluster 2", "Cluster 3") , fill = pal2,
       title = "Clustering 1980/85",
       cex = 1.5 , bty = "n", y.intersp = 0.5)
# plot background
plot(sPDF, xlim = c(-12, 62), ylim = c(10, 42),
     border="grey70", col="white")

mapPolys(sPDF.mena, nameColumnToPlot = "Clustering10",addLegend=FALSE,
         mapTitle ="",  borderCol="white", numCats = 3, catMethod = "categorical",
         add = TRUE, colourPalette = pal3)
mtext( "Clustering 2010/15", cex = 1.5)

legend(x=leg.x, y=leg.y,c("Cluster 1", "Cluster 2", "Cluster 3") , fill = pal3,
       title = "Clustering 2010/15",
       cex = 1.5 , bty = "n", y.intersp = 0.5)


dev.copy2pdf(file="figures/draftMaps.pdf", height=20, width=12)

