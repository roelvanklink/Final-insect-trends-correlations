setwd("C:\\Dropbox\\Insect Biomass Trends\\Map_slopes_Roel")
#setwd("C:\\Users\\roelv\\Dropbox\\Insect Biomass Trends\\Map_slopes_Roel")

library(rgdal)
library(sp)
library(ggplot2)
library(broom)

# define the projections that we'll use, in a PROJ4 string format
WGS84 <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
MOLLWEIDE <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
ROBINSON <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))

# load the borderless land shapefile
land.wgs <- readOGR(dsn = "shapefiles", layer = "GSHHS_c_L1") 

# load the shapefile with country borders
borders.wgs <- readOGR(dsn = "shapefiles", layer = "TM_WORLD_BORDERS-0.3") 

# load Roel's .csv data
#pts.wgs <- RandEfDataset
#pts.wgs$slope<- pts.wgs$slope
#pts.wgs$slope.scal<-pts.wgs$slope # rescale slopes
#pts.wgs$slope.scal[pts.wgs$slope.scal<(-0.02)]<- -0.02 # ~5% decrease per year
#pts.wgs$slope.scal[pts.wgs$slope.scal>(0.02)]<- 0.02

#plots.wgs <- RandEfPlot
#plots.wgs$slope<- plots.wgs$'PlotID_slope_ mean'
#plots.wgs$slope.scal<-plots.wgs$slope # rescale slopes
#plots.wgs$slope.scal[plots.wgs$slope.scal<(-0.02)]<- -0.02 # ~5% decrease per year
#plots.wgs$slope.scal[plots.wgs$slope.scal>(0.02)]<- 0.02





# ------------------------------------------------------------------------------

# delete Antarctica from the borders shapefile
borders.wgs <- borders.wgs[borders.wgs$NAME != "Antarctica",]

islands<- c("Ã…land Islands",  "American Samoa", "Bermuda", "British Virgin Islands", "Cayman Islands", "Christmas Island",
"Cocos (Keeling) Islands","Bouvet Island" , "British Indian Ocean Territory", "Comoros", "Cook Islands",   
"Falkland Islands (Malvinas)" ,  "Faroe Islands", "Fiji",  "French Polynesia",  "French Southern and Antarctic Lands",                                            
  "Grenada",   "Guadeloupe",   "Guernsey", "Heard Island and McDonald Islands",  "Isle of Man"  , "Jersey",     "Maldives"  ,
"Marshall Islands", "Mayotte",   "Micronesia, Federated States of",   "Montserrat", "New Caledonia", "Niue",  "Norfolk Island",                           
  "Northern Mariana Islands",   "Palau" , "Pitcairn Islands",  "Reunion",   "Saint Barthelemy","Saint Helena",
"Saint Kitts and Nevis", "Saint Lucia"  ,"Saint Martin",  "Saint Pierre and Miquelon",  "Saint Vincent and the Grenadines"  ,
"Samoa" ,"San Marino",  "Sao Tome and Principe", "Seychelles",  "Solomon Islands",  "South Georgia South Sandwich Islands", 
"Timor-Leste",  "Tokelau",  "Tonga", "Trinidad and Tobago",  "Turks and Caicos Islands" ,  "Tuvalu", 
"United States Minor Outlying Islands"  ,    "United States Virgin Islands" ,  "Vanuatu",    "Wallis and Futuna Islands")                
                            

borders.wgs <- borders.wgs[!borders.wgs$NAME %in% islands,]



# reprojecting the WGS84 map to prettier Mollweide projection
land.moll <- spTransform(land.wgs, CRSobj = MOLLWEIDE)
borders.moll <- spTransform(borders.wgs, CRSobj = MOLLWEIDE)
pts.moll <- spTransform(pts.wgs, CRSobj = MOLLWEIDE)

land.rob <- spTransform(land.wgs, CRSobj = ROBINSON)
borders.rob <- spTransform(borders.wgs, CRSobj = ROBINSON)
pts.rob <- spTransform(pts.wgs, CRSobj = ROBINSON)
NE_graticules_rob <- spTransform(NE_graticules, CRSobj = ROBINSON)
NE_box_rob        <- spTransform(NE_box, CRSobj = ROBINSON)

# add the new coordinates to the pts.moll object
pts.moll@data <- data.frame(pts.moll@data, 
                            x = coordinates(pts.moll)[,1],
                            y = coordinates(pts.moll)[,2])

pts.rob@data <- data.frame(pts.rob@data, 
                            x = coordinates(pts.rob)[,1],
                            y = coordinates(pts.rob)[,2])

# -----------------------------------------------------------------------------
# prepare the data for ggplot2  using 'tidy' function from 'broom'

borders.moll.gg <- tidy(borders.moll)
borders.wgs.gg <- tidy(borders.wgs)
borders.rob.gg <- tidy (borders.rob)

# ------------------------------------------------------------------------------
# define the blank ggplot2 theme

blank.theme <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     panel.background=element_blank(),panel.border=element_blank(),
                     panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),plot.background=element_blank())

# ------------------------------------------------------------------------------
# plotting with ggplot2

# general maps
p.wgsLIGHT <- ggplot(borders.rob.gg, aes(long, lat, group=group)) + 
 # geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black", fill="transparent", size = 0.25) +
  # add graticules projected to Robinson
  geom_path(data=NE_graticules_rob, aes(long, lat, group=group),  color="grey90", size = 0.1) + #linetype="dotted",
  
  geom_polygon(fill="grey70", colour="grey50", size = 0.1 ) +
  labs(colour ='Slope') +
  blank.theme
# 
p.wgsDARK <- ggplot(borders.rob.gg, aes(long, lat, group=group)) + 
  # add graticules projected to Robinson
  geom_path(data=NE_graticules_rob, aes(long, lat, group=group),  color="grey90", size = 0.1) + #linetype="dotted",
#  geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black", fill="transparent", size = 0.25) +
  geom_polygon(fill="grey50", colour="grey60", size = 0.05 ) +
  labs(colour ='Slope') +
  blank.theme

#light land  Publication version
p.wgs <- ggplot(borders.rob.gg, aes(long, lat, group=group)) + 
  geom_polygon(fill="grey70", colour="grey50", size = 0.05 ) +
  labs(colour ='Slope') +
  blank.theme



setwd("C:\\Dropbox\\Insect Biomass Trends\\csvs")
#setwd("C:\\Users\\roelv\\Dropbox\\Insect Biomass Trends\\csvs")
