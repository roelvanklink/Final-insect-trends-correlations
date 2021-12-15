rm(list=ls()) 
# run everything! 

setwd("View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence230120191043238647.xlsx\\data/")
library(tidyverse)
library(reshape2)


dat<-     read.csv("View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence170320211313433873.csv", sep = "\t")

dim(dat) # dims differ 

sum(duplicated(dat))
head(dat[duplicated(dat), ])
tail(dat[duplicated(dat), ])




#Check dates
unique(dat[,1])  #443 dates
#remove one wrong date with 0 observations
dat<- subset(dat,ï..Date != "9999-08-15" )

dat$date<- as.Date(dat[,1])
dat$Year<- format (dat$date, "%Y")
dat$Period<- format (dat$date, "%m")



# Check some years that were reported to be problematic: 


# data from 2011 were corrupted in a previous version check if fixed now: 
dat2011<- subset(dat, Year == 2011)
dat2011
tail(dat2011, 30)
sample_n(subset(dat, Year == 2011), 25)
unique(dat2011$date) # looks unproblematic to me 


# the samples from 2010 were lost in transport, but have recently (march 2021) turned up in Cambridge (pers comm Niels Martin Schmidt)
dat2010<- subset(dat, Year == 2010)
unique(dat2010$date)
dat<- subset(dat, Year != 2010)

# art1 = windowtrap 
# rest are yellow pitfalls in different vegetation zones
# coordinates: 74o28'N, 20o34'W

dat[dat == -9999]<- NA
dat[dat == -999]<- NA







#check Species

dat$uniq.sp<- apply(dat[,13:17],MARGIN = 1,  function(x) x[max(which(!is.na(x)))])
as.data.frame(unique(dat$uniq.sp) )  # 91, but messy 
as.data.frame(unique(dat[, c(13:17, 32)]) )

subset(dat, is.na(uniq.sp)) # none

dat$uniq.sp[dat$uniq.sp == "unidentified"]<- "Araneae" # fuckup in names


# first calc number ind per trap per day
dat$mn.A<- dat$A / dat$Days.A
dat$mn.B<- dat$B / dat$Days.B
dat$mn.C<- dat$C / dat$Days.C
dat$mn.D<- dat$D / dat$Days.D
dat$mn.E<- dat$E / dat$Days.E
dat$mn.F<- dat$F / dat$Days.F
dat$mn.G<- dat$G / dat$Days.G
dat$mn.H<- dat$H / dat$Days.H
dat$totalCatch<- rowSums(dat[, c("A", "B",  "C",  "D",  "E",  "F" , "G" , "H")], na.rm = T)


# then calc mean value of all traps per plot: 
dat$all.mn<- apply(dat[,  c("mn.A","mn.B","mn.C", "mn.D", "mn.E", "mn.F","mn.G","mn.H")], MARGIN = 1, mean, na.rm = T) # seems fine 
dim( subset(dat, all.mn != "NaN"))
dat<- subset(dat, all.mn != "NaN") # remove not-sampled dates

# there are Inf values here! 
subset(dat, all.mn == Inf)


# assign plot names
dat$Plot_ID<-NA
dat$Plot_ID[dat$Plot.ID == "Art1"]<-1053
dat$Plot_ID[dat$Plot.ID == "Art2"]<-1054
dat$Plot_ID[dat$Plot.ID == "Art3"]<-1055
dat$Plot_ID[dat$Plot.ID == "Art4"]<-1056
dat$Plot_ID[dat$Plot.ID == "Art5"]<-1057
dat$Plot_ID[dat$Plot.ID == "Art7"]<-1058
dat$Plot_ID[dat$Plot.ID == "Art6"]<-1880


gl<- dat  # for richness rarefaction, see below



# Best to take mean of all active traps at each date. 
# this should then also somehow be standardized for trapping days 

# selection of good data

dat<- subset(dat, Plot.ID != "Art6")  # remove Art6 (only sampled 96-98)
window<-subset(dat, Plot.ID == "Art1") # separate window trap from rest 
dat<- subset(dat, Plot.ID != "Art1") # exclude window trap


sum(duplicated(dat)) #21
sum(duplicated(window)) # 115




dim(dat) # 30.000 lost 



# calc mean dates
# not possible, becaue Days is actually more a reflection of trapping effort (also correcting for number of pooled traps)
# just take date trap was emptied



# make file ready for use 

Datasource_name <- "Greenland arthropods"
Unit<- "abundance"
Transformed_number<- NA;    Sex <- NA
Error <- NA;               
Sample_ID<-292

Greenland<-data.frame(Datasource_name, 
                             Plot_ID = dat$Plot_ID, 
                             Plot_name = dat$Plot.ID, 
                             Sample_ID, 
                             Year = dat$Year,
                             Period = dat$Period,
                             Date = dat$date,
                             Taxon = dat$uniq.sp, 
                             Sex, 
                             Unit, 
                             Original_number  = dat$all.mn, 
                             Transformed_number, 
                             Number = dat$all.mn, 
                             Error
)

dim(subset(Greenland, Number == 0)) # 50.000 zeros
dim(Greenland) # 85537 on 19-12-19

# duplicates?
sum(duplicated(Greenland)) # 69
subset(Greenland[duplicated(Greenland),], Number !=0) # 4 

dim(Greenland)
Greenland<- subset(Greenland, !duplicated(Greenland) | Number >0)


# windowtrap (sum of all partial traps , in stead of mean )

window$Plot_ID<-NA
window$Plot_ID[window$Plot.ID == "Art1"]<-1053

window$w.sum<- apply(window[,  c("mn.A","mn.B","mn.C", "mn.D")], MARGIN = 1, sum, na.rm = T)

Greenland.window<-data.frame(Datasource_name, 
                      Plot_ID = window$Plot_ID, 
                      Plot_name = window$Plot.ID, 
                      Sample_ID = 291, 
                      Year = window$Year,
                      Period = window$Period,
                      Date = window$date,
                      Taxon = window$uniq.sp, 
                      Sex, 
                      Unit, 
                      Original_number  = window$w.sum, 
                      Transformed_number, 
                      Number = window$w.sum, 
                      Error
)

dim(Greenland.window) # 12683 on 19-12-19
dim(subset(Greenland.window, Number == 0)) # 7900 zeros

# duplicates
sum(duplicated(Greenland.window)) # yes! 119 
subset(Greenland.window[duplicated(Greenland.window),], Number !=0)
# all zero's -> remove

Greenland.window<- Greenland.window[!duplicated(Greenland.window),]





Greenland.all<- rbind(Greenland.window, Greenland)


# remove 2010 because moste samples were lost 

Greenland.all<- subset(Greenland.all, Year != 2010) 


subset(Greenland.all, Number == Inf)
dim(subset(Greenland.all, Number == 0)) # 50.000 zeros
dim(Greenland.all)

Greenland.all$Taxon<-gsub(" ", "_", Greenland.all$Taxon)


dcast(Greenland, Year ~ Plot_ID, value.var = "Number", sum, na.rm = T)

sum(duplicated(Greenland.all)) # 188

write.csv(Greenland.all, "/Greenland2021meansPerDay.csv")


