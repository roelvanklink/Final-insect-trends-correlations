

# load all data files ##### 
rm(list=ls()) 
 

library(reshape2)
library(tidyverse)
library(beepr)



taxa<-read.csv( file = "taxa5.2.csv"); dim(taxa)
plots<-read.csv( file = "/PlotData 5.0.csv"); dim(plots)
UKfwPlots<- read.csv( file = "/UKfwSites.csv")
plots<- rbind(plots[, names(UKfwPlots)], UKfwPlots)

samples <-read.csv( file = "Sample_Info 5.2.csv"); dim(samples)
database <-read.csv( file = "Data 5.2.csv"); dim(database)
database<- subset(database, Note != "remove");dim(database)
unique(database$Datasource_name)
studies<-read.csv(file = "studies 5.2.csv", header = T); dim(studies)
#studies1 <-read.table( file = "clipboard", header = T, sep = "\t"); dim(studies1) 
#write.csv(studies1, file = "studies 5.2.csv", row.names = F)

#Add taxonomic level to Taxon table
taxa<- taxa[, 1:14] 
 taxa$Level<- NA
  taxa$Level[taxa$Phylum!= ""]<- "Phylum"
  taxa$Level[taxa$Class!= ""]<- "Class"
  taxa$Level[taxa$Subclass!= ""]<- "Subclass"
  taxa$Level[taxa$Order!= ""]<- "Order"
  taxa$Level[taxa$Suborder!= ""]<- "Suborder"
  taxa$Level[taxa$Family!= ""]<- "Family"
  taxa$Level[taxa$Subfamily!= ""]<- "Subfamily"
  taxa$Level[taxa$Genus!= ""]<- "Genus"
  taxa$Level[taxa$Species!= ""]<- "Species"
  taxa$Level <- factor(taxa$Level, ordered = TRUE, 
                      levels = c("Phylum",  "Class", "Subclass", "Order","Suborder",  "Family",
                                 "Subfamily","Genus" ,"Species" ))
  taxa$Rank<-as.numeric(taxa$Level) 

  
  
# assign common group names
taxa$commonGroup<- NA
taxa$commonGroup[taxa$Family== "Carabidae"] <-  "Carabidae"
Weevils <- c("Brentidae", "Apionidae", "Anthribidae", "Attelabidae", "Curculionidae" )
taxa$commonGroup[taxa$Family %in% Weevils] <- "Curculionoidea"
taxa$commonGroup[taxa$Family== "Chrysomelidae"] <-  "Chrysomelidae"
taxa$commonGroup[taxa$Family== "Staphylinidae"] <-   "Staphylinidae"
taxa$commonGroup[taxa$Family== "Syrphidae"] <- "Syrphidae"
taxa$commonGroup[taxa$Order== "Neuroptera"] <- "Neuroptera"

                                          
bees<- c("Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae", "Melittidae", "Stenotritidae" )
taxa$commonGroup[taxa$Family %in% bees] <- "Bees"

butterflies <- c("Nymphalidae", "Hesperiidae",  "Lycaenidae",  "Pieridae", "Papilionidae", "Riodinidae")
taxa$commonGroup[taxa$Family %in% butterflies] <- "Butterflies"

taxa$commonGroup[taxa$Family== "Coccinellidae"] <-  "Coccinellidae"
taxa$commonGroup[taxa$Subclass== "Collembola"] <- "Collembola"
taxa$commonGroup[taxa$Order== "Araneae"] <- "Araneae"
taxa$commonGroup[taxa$Order== "Orthoptera"] <- "Orthoptera"

hymFam<- sort(unique(taxa$Family[taxa$Order == "Hymenoptera"]))
parasitoids <- hymFam[! hymFam %in% c(bees, "Formicidae",  "Vespidae", "Agaonidae", "Argidae", "Cynipidae", "Cynipoidea", "Figitidae" , 
                                      "Xiphydriidae" , "Tenthredinidae" ,"")] # this excludes all other families , including bees 
taxa$commonGroup[taxa$Family %in% parasitoids] <- "Parasitoid wasps"

lepFam<- sort(unique(taxa$Family[taxa$Order == "Lepidoptera"]))
moths <-  lepFam[!lepFam %in% c(butterflies, "Zygaenidae", "")]
taxa$commonGroup[taxa$Family %in% moths] <- "Moths"


colFam<- sort(unique(taxa$Family[taxa$Order == "Coleoptera"]))


taxa$commonGroup[taxa$Suborder== "Heteroptera"] <- "Heteroptera"
taxa$commonGroup[taxa$Suborder== "Auchenorrhyncha"] <- "Auchenorrhyncha" 
Dungbeetles <- c("Geotrupidae", "Aphodiinae" , "Scarabaeinae")
taxa$commonGroup[taxa$Family %in% Dungbeetles | taxa$Subfamily %in% Dungbeetles] <- "Dungbeetles"

taxa$commonGroup[taxa$Family== "Formicidae"] <-  "Formicidae"

taxa$commonGroup[taxa$Order== "Odonata"] <- "Odonata"
taxa$commonGroup[taxa$Order== "Trichoptera"] <- "Trichoptera"
taxa$commonGroup[taxa$Order== "Ephemeroptera"] <- "Ephemeroptera"
taxa$commonGroup[taxa$Order== "Plecoptera"] <- "Plecoptera"
taxa$commonGroup[taxa$Family== "Chironomidae"] <- "Chironomidae"
waterbeetles<- c(  "Dytiscidae",   "Gyrinidae" , "Haliplidae" , "Noteridae",   "Amphizoidae",
                   "Hygrobiidae", "Meruidae", "Hydroscaphidae" , "Hydrophilidae", "Lutrochidae", 
  "Dryopidae",  "Elmidae","Eulichadidae", "Heteroceridae", "Limnichidae", "Psephenidae", 
  "Ptilodactylidae", "Torridincolidae", "Sphaeriusidae")
taxa$commonGroup[taxa$Family %in% waterbeetles] <- "Coleoptera"
taxa$commonGroup[taxa$Family== " "] <- "coleoptera"
taxa$commonGroup[taxa$Family== "Culicidae"] <- "Culicidae"
taxa$commonGroup[taxa$Family== "Simuliidae"] <- "Simuliidae"
waterbugs <- c("Belostomatidae",  "Nepidae", "Corixidae", "Naucoridae", "Ochteridae", "Gelastocoridae", "Aphelocheiridae", "Notonectidae", "Pleidae", "Helotrephidae")
taxa$commonGroup[taxa$Family %in%  waterbugs] <- "Hemiptera"

taxa$commonGroup[taxa$Subclass== "Acari"] <- "Acari"

#check waht's not assigned
unique(subset(taxa[, c("Class" , "Subclass", "Order", "Suborder", "Family",  "commonGroup")], is.na(commonGroup) & Class == "Arachnida"))  
  # nothing big missing 
  
  
  
  
  
  
  # remove repeated column names
  names(studies) # no redundancy
  studies<- studies[, 1:31]
  names(database) # remove redundant columns later
  names(samples) # remove redundant comuns
  samples<- samples[, c("Sample_ID", "Datasource_ID", "Datasource_nameREDUNDANT", "Data_source", "Extraction_method", "Sampling_method", "Stratum", 
                    "Sample_area", "Ref.to.methods", "Number_of_replicates", "Aggregation_of_replicates", "Taxon_in_Data",            
                    "OLDabundance_analysis" , "NEWabundance_analysis", "PurifiedAnalysis", "Biomass_Abundance_analysis", "taxonomic_paper", "non.insects" , 
                    "taxon_comparison", "order_comparison", "family_comparison", "Populations",
                    "non.insect_proportion", "Original_unit", "Calculations", "Unit",  "Richness_precision", "Error_unit", "Flag_taxonomy"  )   ]
  names(plots) # remove redundant columns
  plots<- plots[, c("Plot_ID", "Datasource_ID", "Location", "Plot_name", "Details.plots",  "Experimental_Treatment", "Details_expt_trt",
                   "Process_of_change", "notes_change", "invasives", "Coord_system", "Original_Latitude", "Original_Longitude", "Latitude",
                   "Longitude", "Elevation", "Source_geogr_data")]
  names(taxa) # no redundancy
  taxa<-taxa[, c("ID","Phylum", "Class", "Subclass", "Suborder",  "Order", "Family","Subfamily", "Genus",     "Species",   "Taxon", "Level", "Rank", "Note", "commonGroup")]
  
# clean some datasets from the raw database: 
  # remove some years in Brazil freshwater 
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 1999)  
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 1999)
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 2001 )
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 2002 )
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 2005)
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 2008 ) 
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Plot_name == "Ipatinga"     |  Year != 2006 ) 
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Plot_name == "SantaBarbara" |  Year != 2004 ) 
  dim(database)

# remove some years for Alaska
  database<- subset(database,  Datasource_name != "Alaska freshwater" | Year != 1994) # some taxa not counted  
  database<- subset(database,  Datasource_name != "Alaska freshwater" | Year != 1985) # some taxa not counted
  database<- subset(database,  Datasource_name != "Alaska freshwater" | Year != 1986) # some taxa not counted
 
   # homogenize some dates in Sweden where different water layers were sampled on different dates: 
  database$Date[database$Plot_ID == 1195 & database$Year == 2001 & database$Date == "15-10-2001"]  <- "25-10-2001"
  database$Date[database$Plot_ID == 1195 & database$Year == 2007 & database$Date == "17-10-2007"]  <- "16-10-2007"
  database$Date[database$Plot_ID == 1230 & database$Year == 2012 & database$Date == "9-10-2012"]  <- "19-10-2012"
  database$Date[database$Plot_ID == 1274 & database$Year == 2015 & database$Date == "5-10-2015"]  <- "6-10-2015"
  

  # load processed greenland data (not allowed to share derived product, see greenland processing code)
  Greenland<- read.csv( file = "C:/Dropbox/Insect Biomass Trends/csvs/Greenland2021meansPerDay.csv", header = T); 


  
#  combine files: 
    allData <- rbind(
    Greenland[, -(1)], 
    database[, -(1)]  ) ; dim(allData)#  
  
names(allData)[names(allData) == "Unit"]<-"Unit_in_data" # rename to avoid confusion 
allData<- allData[, -c( which(names(allData) == "Plot_name"), which(names(allData) == "Datasource_name"))  ]
    




    # remove useless metrics 
unique(allData$Unit_in_data)
allData<- subset(allData, Unit_in_data == "abundance" |  Unit_in_data =="density" ); dim(allData)

    # remove non insects  after merging with taxa 
allData<- merge(allData, taxa, by = "Taxon"); dim(allData)
#unique(allData$Taxon)[!unique(allData$Taxon)  %in% test$Taxon   ]
allData<-subset(allData, Class ==  "Insecta" | Class == "Arachnida" | Class == "Entognatha" ); dim(allData)


    # select correct datasets (after merging with SampleData)
allData<- merge(allData, samples, by = "Sample_ID"); dim(allData)
allData<- merge(allData, plots); dim(allData)
#anti_join(allData2, allData1)[, 1:5]

allData<- merge(allData, studies, by = "Datasource_ID"); dim(allData)

names(allData)

dim(allData)
allData<- subset(allData, taxon_comparison == "y"| order_comparison == "y" |  family_comparison == "y")
dim(allData)


# exclude experimental data (locations where researchers manipulated the environment)
exptPlots<- c(5, # alaska
              921, 922, 924,925, #smes
              643, 644, 646, 647, # hemlock removal
              137, 138, 139  #brazil fragmentation experiment
)
exptDatasources<- c(300,1364, 1357,1410) #Kellogg, Luiquillo CTE, Cedar creek big bio, some german grassland

allData<- allData[!allData$Datasource_ID %in% exptDatasources, ]
allData<- allData[!allData$Plot_ID %in% exptPlots, ]
dim(allData)




  
# make metadata file of number of orders / families AND number of years per plot 
metadata_per_plot<-  allData %>% 
  group_by(  Plot_ID) %>%
  summarise(
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name), 
    Plot_name = unique(Plot_name),
    Start = min(Year, na.rm = T),
    End = max(Year, na.rm = T),
    Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
    NumberOfOrders = length(unique(Order)), 
    NumberOfFamilies = length(unique(Family)), 
    NumberOfCommonTaxa = length(unique(commonGroup))
    )

plotfamilyClean<- (subset(metadata_per_plot,   Duration >9 & NumberOfFamilies>1 )) ; dim(plotfamilyClean)
plotorderClean<-  (subset(metadata_per_plot,   Duration >9 & NumberOfOrders>1 )) ; dim(plotorderClean)
plotCommonClean<-  (subset(metadata_per_plot,   Duration >9 & NumberOfCommonTaxa>1 )) ; dim(plotCommonClean)
# revove plots with < 10 years 
    # remove PLOTS without multiple taxa / too few individuals of taxa

allDataFam <-subset(allData, Plot_ID %in% plotfamilyClean$Plot_ID & family_comparison == "y" );dim(allDataFam)  #536617
allDataOrd <-subset(allData, Plot_ID %in% plotorderClean$Plot_ID  & order_comparison == "y"  );dim(allDataOrd)  #452874     
allDataCommon <-subset(allData, Plot_ID %in% plotCommonClean$Plot_ID  & order_comparison == "y" | 
                                Plot_ID %in% plotCommonClean$Plot_ID  & family_comparison == "y" );dim(allDataCommon)  # 460718





# ORDER level comparisons #####

#select columns we need
allDataOrd_select <- allDataOrd %>%
  select(c(Datasource_ID, Datasource_name, Location, Realm, Plot_ID, Year, Period, Date,  Order, Number,  Flag_taxonomy)) %>%
  filter(!is.na(Order)) %>%
  filter(Order!="")%>%
  filter(!is.na(Number))

#identify rarely sampled orders (les sthan 5 datasets and less than 20 plots go out )
rareOrder <- allDataOrd_select %>%
  group_by(Order) %>%
  summarise(nuDatasets = length(unique(Datasource_ID)), 
            nuPlots = length(unique(Plot_ID))) %>%
  filter(nuDatasets<5 & nuPlots <20)

#aggregate (across species) to order and remove rarely sampled orders #####
allDataOrd_aggregated <- allDataOrd_select %>%
  filter(!Order %in% rareOrder$Order) %>%
  group_by(Datasource_ID, Datasource_name, Location, Plot_ID, Realm, Year, Period, Date, Order, Flag_taxonomy) %>%
  summarise(Number = sum(Number)) 
allDataOrd_aggregated$Period[is.na(allDataOrd_aggregated$Period)] <- 1
allDataOrd_aggregated$Date[is.na(allDataOrd_aggregated$Date)] <- 1
dim(allDataOrd_aggregated) #79324














# now add zeroes (run time 3 minutes or so)  #####

addZeroes<- function (dat )  {
  datZero<- NULL
  
  for(i in 1:length(unique(dat$Plot_ID))){
    
    plt<- sort(unique(dat$Plot_ID))[i]
    myData<- dat[dat$Plot_ID == plt , ]
    
    myData$Period[is.na(myData$Period)] <- 1
    myData$Period[myData$Period == ""] <- 1
    #myData$Date  [is.na(myData$Date)] <- myData$Year
    #myData$Date  [myData$Date == ""] <- myData$Year
    if (length(unique (myData$Date)) == 1) {myData$Date<- paste0(myData$Year, "_", myData$Period)}
    
    #expand grid to include 0 counts  # note that the 'date' variable is removed here. 
    # Date plays no role in the analysis, 
    # and in case multiple weeks were sampled within a month, these are thus seen as "replicates" within a month. 
    # month is accounted for as random effect
    constantData <- unique(myData[,c("Datasource_ID", "Year", "Period", "Date",  "Plot_ID")])#these are defo unique
    allgrid <- expand.grid(Plot_ID = unique(myData$Plot_ID),
                           Date  = unique(myData$Date),
                           Order = unique(myData$Order))
    
    allgrid <- merge(allgrid,constantData, all.x=T)
    
    #add observed data
    myData1 <- merge(allgrid, myData[, c( "Order",  "Plot_ID", "Date",   "Number")],  #"classes",
                     by=c( "Order",  "Plot_ID", "Date" ),all=T)
    if (nrow(myData1) > nrow(allgrid)){ print("WARNING: something's going wrong here")}
    # add descriptors
    myData <- merge(myData1, unique(myData[ ,c("Plot_ID",  "Location", "Datasource_name", "Realm", "Flag_taxonomy" )]),
                    by="Plot_ID",all=T)
    if (nrow(myData) > nrow(myData1)){ print("WARNING: something's going wrong here")}
    
    #print(plt)
    
    myData$Number[is.na(myData$Number)] <- 0 
    
    datZero<-rbind (datZero,myData)
    print(plt)
    
  }
  beep(2)
  return(datZero)  }


allDataOrdzero <- addZeroes(allDataOrd_aggregated)

#does every sample and order have 1 observation?
dcast(allDataOrdzero, Plot_ID +  Date +Datasource_ID + Year+Period  +Location+  Datasource_name + Realm ~ Order , value.var = "Number")
nrow(unique(allDataOrdzero[, 1:6])) # should have same nrow as aallDataOrdzero


# check for outliers (indicative of taxon not counted )

outlierCheck<-  allDataOrdzero %>% 
  group_by(  Plot_ID, Order) %>%
  summarise(
    Realm = unique(Realm), 
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name), 
    mean = mean(log10(Number+1)), 
    sd = sd(log10(Number+1)), 
    sd2 = sd(log10(Number+1) *2), 
    zeroes = sum(Number == 0),
    lower2sd  = (mean(log10(Number+1)) - sd(log10(Number+1))*2) 
  ) 
print(subset(outlierCheck, zeroes !=0 & lower2sd >0), n = Inf )
# excessive outliers (>1) in Alaska  are fixed. Only a few leftover in Sweden and new zealand 






    
metadata_per_order_per_plot<-  allDataOrdzero %>% 
  mutate(sample = paste(Year, Period, Date)) %>%
  group_by(  Plot_ID, Order) %>%
  summarise(
    Realm = unique(Realm), 
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name), 
    NumberOfIndPerOrder = sum(Number, na.rm = T ),
    NumberOfOccPerOrder = sum(Number != 0, na.rm = T ),
    NumberOfYears = length(unique(Year)),
    NumberOfSamples = length(unique(sample))
    ) %>% mutate(meanIndPerSample = NumberOfIndPerOrder / NumberOfSamples, 
                 meanOccPerSample = NumberOfOccPerOrder / NumberOfSamples)
    


# list of observations to be excluded, bacause they were observed in less than half of the samples 
exclude<- subset(metadata_per_order_per_plot,  floor(NumberOfIndPerOrder) ==  NumberOfIndPerOrder &    meanOccPerSample <0.5)
dim(exclude) # 1768
print(exclude, n = 100)

# remove these
# remove data deficient orders 
dim(allDataOrdzero)
index1<- paste(allDataOrdzero$Plot_ID, allDataOrdzero$Order)
index2<- paste(exclude$Plot_ID, exclude$Order)
length(index1[! index1 %in% index2]) # to check what the nrow of the df should be 
allDataOrdzero<- anti_join(allDataOrdzero, exclude[, 1:3]); dim(allDataOrdzero) # Remove rare comparisons. 92136  this is correct 


# check other rare species
check<- subset(metadata_per_order_per_plot,  floor(NumberOfIndPerOrder) ==  NumberOfIndPerOrder &    meanOccPerSample >0.5 & meanOccPerSample <1)
print(check, n = Inf)
# this seems fine to me: these are taxa that were present in most years and can be analysed 




# which comparisons are left? ####
# now we want to know in how many datasets different orders co-occur , and remove the comparisons that are too flimsy (<5 datasets or <20 plots)
countDatasets<- dcast(allDataOrdzero, Realm + Datasource_ID + Order  ~ "Count", value.var = "Number" , sum)
coocDfw<- crossprod(table(subset(countDatasets, Realm == "Freshwater") [,c("Datasource_ID","Order")]))
coocDt<-  crossprod(table(subset(countDatasets, Realm == "Terrestrial")[,c("Datasource_ID","Order")]))
diag(coocDfw) <- 0
diag(coocDt) <- 0
coocDfw_long <- reshape2::melt(as.data.frame(coocDfw), 
                             value.name = "Datasets",
                             variable.name = "Taxon2")
coocDfw_long$Taxon1 <- rownames(coocDfw) # add rownames 
coocDfw_long$Realm <- "Freshwater"
coocDt_long <- reshape2::melt(as.data.frame(coocDt), 
                             value.name = "Datasets",
                             variable.name = "Taxon2")
coocDt_long$Taxon1 <- rownames(coocDt)
coocDt_long$Realm <- "Terrestrial"
coocD_long<- rbind(coocDt_long, coocDfw_long); dim(coocD_long)
 length (unique(coocD_long$Taxon1))

countPlots<- dcast(allDataOrdzero, Realm + Plot_ID + Order  ~ "Count", value.var = "Number" , sum)
dim(countPlots)
countPlots$Count <- 1 # convert to only existence of the data
coocPfw<- crossprod(table(subset(countPlots, Realm == "Freshwater")  [,c("Plot_ID","Order")]))
coocPt <- crossprod(table(subset(countPlots, Realm == "Terrestrial") [,c("Plot_ID","Order")]))
diag(coocPfw) <- 0
diag(coocPt) <- 0
coocPfw_long <- reshape2::melt(as.data.frame(coocPfw), 
                               value.name = "Plots",
                               variable.name = "Taxon2")
coocPfw_long$Taxon1 <- rownames(coocPfw) # add rownames 
coocPfw_long$Realm <- "Freshwater"
coocPt_long <- reshape2::melt(as.data.frame(coocPt), 
                              value.name = "Plots",
                              variable.name = "Taxon2")
coocPt_long$Taxon1 <- rownames(coocPt)
coocPt_long$Realm <- "Terrestrial"
coocP_long<- rbind(coocPt_long, coocPfw_long); dim(coocP_long)


cooc_long<- merge(coocP_long, coocD_long)

# checks
sum(cooc_long$Plots< cooc_long$Datasets)
cooc_long[cooc_long$Plots< cooc_long$Datasets, ] # these will all be excluded because of too little data

# remove duplicates: 
cooc_long<-   cooc_long[!duplicated(data.frame(t(apply(cooc_long[1:3], 1, sort)))),]
cooc_long$Taxon2 <- as.character(cooc_long$Taxon2)
dim( cooc_long)

fair_comparisons_order<- subset(cooc_long, Datasets > 5 | Plots > 20) # 
dim(fair_comparisons_order)

ggplot(subset(cooc_long, Datasets > 4 & Plots > 19) ) +
  geom_tile(aes(x=Taxon1, y=Taxon2 , fill=Datasets))+
  scale_fill_viridis_c()+
  facet_wrap(.~Realm, scales = "free")+
  theme(axis.text.x = element_text(angle=90))

ggplot(subset(cooc_long, Datasets > 5 ) ) +
  geom_tile(aes(x=Taxon1, y=Taxon2 , fill=Datasets))+
  scale_fill_viridis_c()+
  facet_wrap(.~Realm, scales = "free")+
  theme(axis.text.x = element_text(angle=90)) # this looks more reasonable 


ok_comparisons_order<- subset(cooc_long, Datasets <5 & Plots > 19); dim(ok_comparisons_order) # 69
#fair_comparisons_order<- subset(cooc_long, Datasets < 5 & Plots > 20); dim(fair_comparisons_order) # 42
good_comparisons_order<- subset(cooc_long, Datasets >= 5 & Plots > 19); dim(good_comparisons_order) #50

# make job array file 
good_comparisons_order$modelName<- paste0(substr(good_comparisons_order$Taxon1,1,4), "_",
                                          substr(good_comparisons_order$Taxon2, 1,4), "_", 
                                          substr(good_comparisons_order$Realm, 1,1))
ok_comparisons_order$modelName<- paste0(substr(ok_comparisons_order$Taxon1,1,4), "_",
                                          substr(ok_comparisons_order$Taxon2, 1,4), "_", 
                                          substr(ok_comparisons_order$Realm, 1,1))

write.csv(good_comparisons_order, "/comparison_jobs.csv")
write.csv(ok_comparisons_order, "/comparison_jobs_less_good.csv")








saveRDS(allDataOrdzero, file = "/Fulldata allorders.rds")












# group level comparisons #####
# Same procedure for commonly assessed taxa: 

unique(allDataCommon$commonGroup) 



#select columns we need, and remove taxa that are not in these groups 
allDataCommon_select <- allDataCommon %>%
  select(c(Datasource_ID, Datasource_name, Location, Realm, Plot_ID, Year, Period, Date,  commonGroup, Number,  Flag_taxonomy)) %>%
  filter(!is.na(commonGroup)) %>%
  filter(commonGroup != "")%>%
  filter(!is.na(Number))

  


#identify rarely sampled orders (les sthan 5 datasets and less than 20 plots go out )
rareGroup <- allDataCommon_select %>%
  group_by(commonGroup) %>%
  summarise(nuDatasets = length(unique(Datasource_ID)), 
            nuPlots = length(unique(Plot_ID))) %>%
  filter(nuDatasets<5 & nuPlots <20)
rareGroup # none, duh

#aggregate (across species) to order and remove rarely sampled orders #####
allDataCommon_aggregated <- allDataCommon_select %>%
  filter(!commonGroup %in% rareGroup$commonGroup) %>%
  group_by(Datasource_ID, Datasource_name, Location, Plot_ID, Realm, Year, Period, Date, commonGroup, Flag_taxonomy) %>%
  summarise(Number = sum(Number)) 
allDataCommon_aggregated$Period[is.na(allDataCommon_aggregated$Period)] <- 1
allDataCommon_aggregated$Date[is.na(allDataCommon_aggregated$Date)] <- 1
dim(allDataCommon_aggregated) #94931




# now add zeroes (run time 3 minutes or so)  #####

addZeroes<- function (dat )  {
  datZero<- NULL
  
  for(i in 1:length(unique(dat$Plot_ID))){
    
    plt<- sort(unique(dat$Plot_ID))[i]
    myData<- dat[dat$Plot_ID == plt , ]
    
    myData$Period[is.na(myData$Period)] <- 1
    myData$Period[myData$Period == ""] <- 1
    #myData$Date  [is.na(myData$Date)] <- myData$Year
    #myData$Date  [myData$Date == ""] <- myData$Year
    if (length(unique (myData$Date)) == 1) {myData$Date<- paste0(myData$Year, "_", myData$Period)}
    
    #expand grid to include 0 counts  # note that the 'date' variable is removed here. 
    # Date plays no role in the analysis, 
    # and in case multiple weeks were sampled within a month, these are thus seen as "replicates" within a month. 
    # month is accounted for as random effect
    constantData <- unique(myData[,c("Datasource_ID", "Year", "Period", "Date",  "Plot_ID")])#these are defo unique
    allgrid <- expand.grid(Plot_ID = unique(myData$Plot_ID),
                           Date  = unique(myData$Date),
                           commonGroup = unique(myData$commonGroup))
    
    allgrid <- merge(allgrid,constantData, all.x=T)
    
    #add observed data
    myData1 <- merge(allgrid, myData[, c( "commonGroup",  "Plot_ID", "Date",   "Number")],  #"classes",
                     by=c( "commonGroup",  "Plot_ID", "Date" ),all=T)
    if (nrow(myData1) > nrow(allgrid)){ print("WARNING: something's going wrong here")}
    # add descriptors
    myData <- merge(myData1, unique(myData[ ,c("Plot_ID",  "Location", "Datasource_name", "Realm", "Flag_taxonomy" )]),
                    by="Plot_ID",all=T)
    if (nrow(myData) > nrow(myData1)){ print("WARNING: something's going wrong here")}
    
    #print(plt)
    
    myData$Number[is.na(myData$Number)] <- 0 
    
    datZero<-rbind (datZero,myData)
    print(plt)
    
  }
  beep(2)
  return(datZero)  }


allDataCommonzero <- addZeroes(allDataCommon_aggregated)

#does every sample and order have 1 observation?
dcast(allDataCommonzero, Plot_ID +  Date +Datasource_ID + Year+Period  +Location+  Datasource_name + Realm ~ commonGroup , value.var = "Number")
nrow(unique(allDataCommonzero[, 1:6])) # should have same nrow as aallDataOrdzero



# check which comparisons make sense in each plot regarding data availability per plot 

metadata_per_group_per_plot<-  allDataCommonzero %>% 
  mutate(sample = paste(Year, Period, Date)) %>%
  group_by(  Plot_ID, commonGroup) %>%
  summarise(
    Realm = unique(Realm), 
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name), 
    NumberOfIndPerGroup = sum(Number, na.rm = T ),
    NumberOfOccPerGroup = sum(Number != 0, na.rm = T ),
    NumberOfYears = length(unique(Year)),
    NumberOfSamples = length(unique(sample))
  ) %>% mutate(meanIndPerSample = NumberOfIndPerGroup / NumberOfSamples, 
               meanOccPerSample = NumberOfOccPerGroup / NumberOfSamples)



# list of observations to be excluded, bacause they were observed in less than half of the samples 
exclude<- subset(metadata_per_group_per_plot,  floor(NumberOfIndPerGroup) ==  NumberOfIndPerGroup &    meanOccPerSample <0.5)
dim(exclude) # 2295
print(exclude, n = 100)

# remove these
# remove data deficient orders 
dim(allDataCommonzero)
index1<- paste(allDataCommonzero$Plot_ID, allDataCommonzero$commonGroup)
index2<- paste(exclude$Plot_ID, exclude$commonGroup)
length(index1[! index1 %in% index2]) # to check what the nrow of the df should be 
allDataCommonzero<- anti_join(allDataCommonzero, exclude[, 1:3]); dim(allDataCommonzero) # Remove rare comparisons.   this is correct 


# check other rare species
check<- subset(metadata_per_group_per_plot,  floor(NumberOfIndPerGroup) ==  NumberOfIndPerGroup &    meanOccPerSample >0.5 & meanOccPerSample <1)
print(check, n = 100)
# this seems fine to me: these are taxa that were present in most years and can be analysed 









# which comparisons are left? ####
# now we want to know in how many datasets different orders co-occur , and remove the comparisons that are too flimsy (<5 datasets or <20 plots)
countDatasets<- dcast(allDataCommonzero, Realm + Datasource_ID + commonGroup  ~ "Count", value.var = "Number" , sum)
coocDfw<- crossprod(table(subset(countDatasets, Realm == "Freshwater") [,c("Datasource_ID","commonGroup")]))
coocDt<-  crossprod(table(subset(countDatasets, Realm == "Terrestrial")[,c("Datasource_ID","commonGroup")]))
diag(coocDfw) <- 0
diag(coocDt) <- 0
coocDfw_long <- reshape2::melt(as.data.frame(coocDfw), 
                               value.name = "Datasets",
                               variable.name = "Taxon2")
coocDfw_long$Taxon1 <- rownames(coocDfw) # add rownames 
coocDfw_long$Realm <- "Freshwater"
coocDt_long <- reshape2::melt(as.data.frame(coocDt), 
                              value.name = "Datasets",
                              variable.name = "Taxon2")
coocDt_long$Taxon1 <- rownames(coocDt)
coocDt_long$Realm <- "Terrestrial"
coocD_long<- rbind(coocDt_long, coocDfw_long); dim(coocD_long)
length (unique(coocD_long$Taxon1))

countPlots<- dcast(allDataCommonzero, Realm + Plot_ID + commonGroup  ~ "Count", value.var = "Number" , sum)
dim(countPlots)
countPlots$Count <- 1 # convert to only existence of the data
coocPfw<- crossprod(table(subset(countPlots, Realm == "Freshwater")  [,c("Plot_ID","commonGroup")]))
coocPt <- crossprod(table(subset(countPlots, Realm == "Terrestrial") [,c("Plot_ID","commonGroup")]))
diag(coocPfw) <- 0
diag(coocPt) <- 0
coocPfw_long <- reshape2::melt(as.data.frame(coocPfw), 
                               value.name = "Plots",
                               variable.name = "Taxon2")
coocPfw_long$Taxon1 <- rownames(coocPfw) # add rownames 
coocPfw_long$Realm <- "Freshwater"
coocPt_long <- reshape2::melt(as.data.frame(coocPt), 
                              value.name = "Plots",
                              variable.name = "Taxon2")
coocPt_long$Taxon1 <- rownames(coocPt)
coocPt_long$Realm <- "Terrestrial"
coocP_long<- rbind(coocPt_long, coocPfw_long); dim(coocP_long)


cooc_long<- merge(coocP_long, coocD_long)

# checks
sum(cooc_long$Plots< cooc_long$Datasets)
cooc_long[cooc_long$Plots< cooc_long$Datasets, ] # these will all be excluded because of too little data

# remove duplicates: 
cooc_long<-   cooc_long[!duplicated(data.frame(t(apply(cooc_long[1:3], 1, sort)))),]
cooc_long$Taxon2 <- as.character(cooc_long$Taxon2)
dim( cooc_long)

fair_comparisons_group<- subset(cooc_long, Datasets > 5 | Plots > 20) # 

ggplot(subset(cooc_long, Datasets > 4 & Plots > 19) ) +
  geom_tile(aes(x=Taxon1, y=Taxon2 , fill=Datasets))+
  scale_fill_viridis_c()+
  facet_wrap(.~Realm, scales = "free")+
  theme(axis.text.x = element_text(angle=90))

ggplot(subset(cooc_long, Datasets <5 & Plots > 19 ) ) +
  geom_tile(aes(x=Taxon1, y=Taxon2 , fill=Datasets))+
  scale_fill_viridis_c()+
  facet_wrap(.~Realm, scales = "free")+
  theme(axis.text.x = element_text(angle=90)) # this looks more reasonable 


ok_comparisons_group<- subset(cooc_long, Datasets <5 & Plots > 19); dim(ok_comparisons_group) # 160
#fair_comparisons_group<- subset(cooc_long, Datasets < 5 & Plots > 20); dim(fair_comparisons_group) # 87
good_comparisons_group<- subset(cooc_long, Datasets >= 5 & Plots > 19); dim(good_comparisons_group) #77 

# make job array file 
good_comparisons_group$modelName<- paste0(substr(good_comparisons_group$Taxon1,1,4), "_",
                                          substr(good_comparisons_group$Taxon2, 1,4), "_", 
                                          substr(good_comparisons_group$Realm, 1,1))
ok_comparisons_group$modelName<- paste0(substr(ok_comparisons_group$Taxon1,1,4), "_",
                                          substr(ok_comparisons_group$Taxon2, 1,4), "_", 
                                          substr(ok_comparisons_group$Realm, 1,1))

head(good_comparisons_group)
good_comparisons_group[good_comparisons_group$modelName %in% good_comparisons_order$modelName ,]

good_comparisons_group<- good_comparisons_group[!good_comparisons_group$modelName %in% good_comparisons_order$modelName ,]

ok_comparisons_group[ok_comparisons_group$modelName %in% c(ok_comparisons_order$modelName, good_comparisons_order$modelName) ,]

ok_comparisons_group<- ok_comparisons_group[!ok_comparisons_group$modelName %in% c(ok_comparisons_order$modelName, good_comparisons_order$modelName),]



write.csv(good_comparisons_group, "R/submit scripts and jobs/comparison_jobs_groups.csv")
write.csv(ok_comparisons_group,   "R/submit scripts and jobs/comparison_jobs_groups_less_good.csv")







# check for outliers (indicative of taxon not counted )

outlierCheck<-  allDataCommonzero %>% 
  group_by(  Plot_ID, commonGroup) %>%
  summarise(
    Realm = unique(Realm), 
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name), 
    mean = mean(log10(Number+1)), 
    sd = sd(log10(Number+1)), 
    sd2 = sd(log10(Number+1) *2), 
    zeroes = sum(Number == 0),
    lower2sd  = (mean(log10(Number+1)) - sd(log10(Number+1))*2) 
  ) 
print(subset(outlierCheck, zeroes !=0 & lower2sd >0), n = Inf )
# excessive outliers (>1) in Alaska  are fixed. Only a few leftover in Sweden and new zealand 

saveRDS(allDataCommonzero, file = "/Fulldata allgroups.rds")












