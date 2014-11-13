# Political strongholds and organized violence in Kenya
# Francisco Villamil
# November 2014
# -----------------------------------------------------------
# DATA CREATION (SOURCE: ACLED) AND EVENTS & FATALITIES PLOTS

# (OPTIONAL R code file)


setwd("...")
library(rgdal)
library(XLConnect)
# library(sp)
library(ggplot2)
library(scales)
library(RColorBrewer)
#library(reshape2)
library(plyr)

# Data download and cleaning
# --------------------------

# Download ACLED Kenya data file
acled.url = "http://www.acleddata.com/wp-content/uploads/2014/07/ACLED-Kenya_19970101-to-20131231_final_updated.xlsx"
download.file(acled.url, destfile = "acled-kenya.xlsx")

# Load .xlsx into data frame
acled = readWorksheet(loadWorkbook("acled-kenya.xlsx"), sheet = "Sheet1", header = TRUE)
rm(acled.url)

# Drop unnecessary variables
acled = subset(acled, 
	select = c("EVENT_DATE", "EVENT_TYPE", "LATITUDE", "LONGITUDE", "FATALITIES"))

# Change date variable from POXISct to Date
acled$EVENT_DATE = as.Date(acled$EVENT_DATE) + 1

# Limit sample to period 28 December - 28 February
acled = acled[!(acled$EVENT_DATE > "2008-02-28" | acled$EVENT_DATE < "2007-12-28"),]

# Clean EVENT_TYPE of empty factors and simplify "battle" type level
acled$EVENT_TYPE = factor(acled$EVENT_TYPE)
levels(acled$EVENT_TYPE)[1] = "Battle"


# Spatial overlay to get constituency
# -----------------------------------

# Read constituency shapefile
#	(downloaded from ArcGIS.com, URL: http://www.arcgis.com/home/item.html?id=12a8bd218a944078b79f1f43e0a00786)
#	NOTE: Folder name ("constituency") must be kept as in .zip file
const = readOGR("constituency/constituency.shp", layer = "constituency")

# Correct georeferencing errors in ACLED data:
# -0.51667, 34.45 fall on the sea, should be in HOMA BAY (correct to LAT = -0.52667, LON = 34.458)
# 0.46670, 34.10 fall on Uganda, but should be in BUSIA (correct to LAT = 0.463, LON = 34.105)
# -3.63333, 39.85 fall on the sea, but it should be in KILIFI (correct LAT to -3.63)
acled$LONGITUDE[c(12, 26, 59, 83, 84, 89, 147, 177, 178, 180, 181, 201, 202, 223, 224, 226)] = 
	c(34.105, 39.850, 34.458, 34.105, 34.105, 34.458, 34.105, 34.105,
	34.105, 34.458, 34.458, 34.105, 34.105, 34.105, 34.105, 34.458)
acled$LATITUDE[c(12, 26, 59, 83, 84, 89, 147, 177, 178, 180, 181, 201, 202, 223, 224, 226)] = 
	c(0.463000, -3.63000, -0.52667, 0.463000, 0.463000, -0.52667,0.463000, 0.463000,
	0.463000, -0.52667, -0.52667, 0.463000, 0.463000, 0.463000, 0.463000, -0.52667)

# Separate coordinates from ACLED data in a data.frame & assign coordinate class data
kdf = data.frame(x = acled$LONGITUDE, y = acled$LATITUDE)
coordinates(kdf) = ~x+y

# Assign same CRS for both datasets 
proj4string(kdf) = proj4string(const)

# Do the spatial overlay,  attach the column to the main dataset, and delete useless objects
constituency.event = over(kdf, const)$NAME
acled = cbind(acled, CONSTITUENCY = constituency.event)		
rm(constituency.event, kdf)

# # Save acled as a .csv and constituency shapefile as an R object
# write.csv(file = "acled.csv", acled, row.names = FALSE)
# save(const, file = "const.rda")


# Fatalities aggregation
# ----------------------

# Fatalities in riots/protests events
f.riots = as.numeric()
for (i in levels(acled$CONSTITUENCY)){
	f.riots = c(f.riots, sum(acled$FATALITIES[acled$CONSTITUENCY == i & acled$EVENT_TYPE == "Riots/Protests"]))
	}

# Fatalities in battles & violence against civilians events
f.org = as.numeric()
for (i in levels(acled$CONSTITUENCY)){
	f.org = c(f.org, sum(acled$FATALITIES[acled$CONSTITUENCY == i & !acled$EVENT_TYPE == "Riots/Protests"]))
	}

# Merge variables in a data frame with constituency name
fatalities = data.frame(constituency = rep(levels(acled$CONSTITUENCY), 2),
	fatalities = c(f.riots, f.org),
	type = rep(c("Non-organized violence", "Organized violence"), each = length(f.riots)))
rm(f.org, f.riots, i)


# Events by type in time plot (figure 1)
# --------------------------------------

events = ggplot(acled, aes(x = EVENT_DATE, y = ..count.., fill = EVENT_TYPE)) +
	geom_histogram(binwidth = 1) + xlab("") + ylab("Number of daily events") +
	scale_fill_brewer(name = "Event type", palette = "Set1") +
	scale_x_date(breaks = "1 week", 
		minor_breaks = "1 day", 
		labels = date_format("%b %d")) +
	theme_bw() + theme(legend.justification=c(1,1), legend.position=c(1,1))
ggsave(plot = events, file = "events.pdf", width = 10, height = 3)


# Fatalities per constituency map (figure 2)
# ------------------------------------------

# Re-load and prepare shapefile
const = readOGR("constituency/constituency.shp", layer = "constituency")
c.map = fortify(const, region = "NAME")
names(c.map)[7] = "constituency"
names(const)[8] = "constituency"
c.map = join(c.map, const@data, by = "constituency")
rm(const)
map.data = join(c.map, fatalities, by = "constituency")

# Turn into discrete values
map.data$fatalities[map.data$fatalities > 0 & map.data$fatalities < 6] = 3
map.data$fatalities[map.data$fatalities > 5 & map.data$fatalities < 11] = 8
map.data$fatalities[map.data$fatalities > 10 & map.data$fatalities < 21] = 15
map.data$fatalities[map.data$fatalities > 20 & map.data$fatalities < 50] = 35
map.data$fatalities[map.data$fatalities > 49] = 60
map.data$fatalities = factor(map.data$fatalities, levels = c("0","3","8","15","35","60"))

# Make plot & save
map = ggplot(map.data, aes(x = long, y = lat, group = group, fill = fatalities)) + 
	geom_polygon(color = "black", size = 0.05) + 
	scale_fill_manual(values = c("#FFFFFF", "#CCCCCC", "#999999", "#666666", "#333333", "#000000"),
					labels = c("0", "1-5", "6-10", "11-20", "21-50", ">50")) + 
	theme_bw() +
	facet_wrap(~type, ncol = 2) +
	theme(legend.position = "top",
		legend.title = element_blank(),
		axis.line = element_blank(),
		axis.text.x = element_blank(),
		axis.text.y = element_blank(),
		axis.ticks = element_blank(),
		panel.grid.major = element_blank(),
		panel.border = element_blank()) +
	xlab("") + ylab("")
ggsave(map, file = "map.pdf", height = 6, width = 9)


# --------------------------------------------------------
# APPENDIX (This is the variable used to limit the sample, which corresponds to "events.org" in data.csv)
# Any "battle" or "violence against civilians" event reported?
events.org = as.data.frame(table(acled$CONSTITUENCY[!acled$EVENT_TYPE == "Riots/Protests"]))$Freq
events.org[events.org > 0] = 1
