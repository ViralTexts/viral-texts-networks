library(dplyr)
library(ggplot2)
library(stringdist)
library(geosphere)

#import a folder of CSV files into one dataframe
files <- dir(path = "./data", pattern = '*.csv', full.names = TRUE)

clusters <- do.call(rbind, lapply(files, read_csv)) %>%
  select(cluster, date, series, title) %>%
  mutate(title = gsub('(.*)\\.*\\(.*','\\1',title)) %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))

#import gazetteer file
gazetteer <- select(read.csv(file = "./geoData/vgaz_out_sorted.csv", header = TRUE), series, latitude, longitude)

#join gazetteer with clusters
clustersGeo <- left_join(clusters, gazetteer, by = "series", match = "all") %>%
  #omit lines with missing lat/long data
   na.omit()

#create pairwise data with lat/longs
clusterGeoPairs <- full_join(clustersGeo, clustersGeo, by = "cluster") %>%
  filter(date.x < date.y)


#calculate geographical distance between papers in each edge
clusterGeoPairs$edgeDist <- distHaversine(matrix(c(clusterGeoPairs$longitude.x, clusterGeoPairs$latitude.x), ncol = 2), matrix(c(clusterGeoPairs$longitude.y, clusterGeoPairs$latitude.y), ncol = 2))
#A 0 value (as when 2 papers are printed in the same city) confuses the calculation, so we need to replace those 0 values with 1 so their weights will be unaffected by distance in the adjusted calculation
clusterGeoPairs$edgeDist[clusterGeoPairs$edgeDist==0]<-1

#adjust weight for distance
clusterGeoPairs$distWeight <- 1 / as.numeric(clusterGeoPairs$edgeDist)
geoEdges <- clusterGeoPairs %>% group_by(title.x,title.y) %>%
  summarize(meanDist = mean(edgeDist), distWeight = sum(distWeight), rawWeight = n())
geoEdges$distEffect <- geoEdges$rawWeight / geoEdges$distWeight

#output gephi compartible CSV
colnames(geoEdges) <- c("source", "target", "distance", "weight", "rawWeight", "distEffect")
write.csv(geoEdges, file="./output/geoNetwork.csv")
