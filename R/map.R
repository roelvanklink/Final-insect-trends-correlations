plots<-read.csv( file = "csvs/PlotData 5.0.csv"); dim(plots)
UKfwPlots<- read.csv( file = "csvs/UKfwSites.csv")
plots<- rbind(plots[, names(UKfwPlots)], UKfwPlots)
studies<-read.csv(file = "csvs/studies 5.2.csv", header = T); dim(studies)



filenames <- list.files("/model-outputs/prior sd1/", pattern="*corSum*", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
cors<- do.call(rbind.data.frame, ldf); dim(cors)
cors<- subset(cors, numberOfGoodDatasets >4); dim(cors) # 

filenamesSlopes <- list.files("/model-outputs/prior sd1/", pattern="slope*", full.names=TRUE)
ldf <- lapply(filenamesSlopes, readRDS)
slopes<- do.call(rbind.data.frame, ldf)
dim(slopes)
slopes<- subset(slopes, task.id %in% cors$task.id); dim(slopes)

studiesOrder<- unique(slopes$Datasource_ID)


filenames <- list.files("/model-outputs/rest taxa prior sd1/", pattern="*corSum*", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
Gcors<- do.call(rbind.data.frame, ldf)
Gcors<- subset(Gcors, numberOfGoodDatasets >4 & numberOfGoodPlots > 19); dim(Gcors) # 


filenamesSlopes <- list.files("/model-outputs/rest taxa prior sd1/", pattern="slope*", full.names=TRUE)
ldf <- lapply(filenamesSlopes, readRDS)
slopes<- do.call(rbind.data.frame, ldf)
dim(slopes)
slopes<- subset(slopes, task.id %in% Gcors$task.id); dim(slopes)

studiesGroup<- unique(slopes$Datasource_ID)
setdiff(studiesOrder, studiesGroup)


includedStudies<- c(studiesGroup, studiesOrder);
includedStudies<- sort(unique(includedStudies))
length(includedStudies)

includedPlots <- plots[ plots$Datasource_ID %in% includedStudies, ]
dim(includedPlots)# these were not all analyzed 
length(unique(includedPlots$Datasource_ID))


includedStudies[!includedStudies %in% plots$Datasource_ID]

includedPlots<- merge(includedPlots, studies); dim(includedPlots)

geoInfo<- aggregate(.~ Datasource_ID +Datasource_name + Realm + Continent,     data = includedPlots[, c("Datasource_ID", "Datasource_name", "Latitude", "Longitude", "Realm", "Continent" )], FUN = mean)
table(geoInfo$Continent, geoInfo$Realm)/ rep(table(geoInfo$Realm), each = 7 ) # proprtions for each continent 
table(geoInfo$Continent, geoInfo$Realm)
table(geoInfo$Realm)


sort(includedStudies)







library(rgdal)
library(sp)
library(broom)
library(reshape2)
library(tidyverse)
library(beepr)
library(reshape2)
library(tidyverse)
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4")
theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(),
                                   panel.background = element_blank(), 
                                   axis.line = element_line(colour = "black") , 
                                   legend.key=element_blank())
studies<-read.csv(file = "/csvs/studies 5.2.csv", header = T); dim(studies)





# take most recent files

#  ++++ RUN TWICE ####
pts.wgs <- geoInfo
pts.wgs <- SpatialPointsDataFrame(coords = data.frame(lon = pts.wgs$Longitude,
                                                      lat = pts.wgs$Latitude),
                                  proj4string = CRS(WGS84),
                                  data = pts.wgs)


source("/R/map_preparation.R")



#map.w.Dataset locations<-  
  p.wgs+
  geom_point(data = pts.rob@data ,  size = 2,#pch = 3,
             aes(x = x,   y = y, color = Realm,   group = NULL), 
             position=position_jitter(h=1, w=1)) +     
  theme(legend.position = c(0.1, 0.1),
        legend.background=element_blank(), 
        legend.key=element_blank())+
  scale_color_manual(values=col.scheme.realm) +
  guides(color=guide_legend(title="Dataset locations"))








