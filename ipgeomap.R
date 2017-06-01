## ---- XW 2017-06-01 ----
## ---- Clear the console ----
rm(list=ls())


## ---- This handles packages ----
required_packages <- c("rgeolocate", "ggplot2", "raster", "rgdal", "gpclib", "maptools", "dplyr")
package_to_install <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(package_to_install)) install.packages(package_to_install)



## ---- Read in system variables ---- 
## 2 arguments
## (1) r working directory
## (2) .csv file with name
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} 

WDIR <- args[1]
datafile <- args[2]


## ---- The analysis start here ----
library(rgeolocate)

## download maxmind db
# https://dev.maxmind.com/geoip/legacy/geolite/

setwd(WDIR)
analyseDate <- format(Sys.Date(), format = "%Y%m%d")

sess <- read.csv(file = datafile, header = T, stringsAsFactors = FALSE)	
df <- data.frame(user_id = sess$user_id, ip = sess$source_ip_address)
rm(sess)

m <- dim(df)[1]

df$city <- character(length = m)
df$lat <- numeric(length = m)
df$long <- numeric(length = m)

## check the maxmind database
tmp <- maxmind(df$ip, file = "GeoLite2-City.mmdb", fields = c("continent_name", "country_name", "country_code", "latitude", "longitude", "city_name"))

df$city <- tmp$city_name
df$lat <- tmp$latitude
df$long <- tmp$longitude

library(ggplot2)
library(raster)
library(rgdal)
library(gpclib)
library(maptools)
library(dplyr)
# sweAdm1 <- getData('GADM', country = 'SWE', level = 1)
sweAdm2 <- getData('GADM', country = 'SWE', level = 2)
sweAdm2@data$id <- rownames(sweAdm2@data)

df1 <- sweAdm2@data
df1_cnty1 <- fortify(sweAdm2)
df1_cnty2 <- left_join(df1, df1_cnty1, by = 'id')


## remove coordinates outside the map
df$lat <- as.numeric(df$lat)
df$long <- as.numeric(df$long)
idx <- which(df$lat > 55 & df$lat < 70 & df$long > 10 & df$long < 25)
df_se <- df[idx, ]
df_others <- df[-idx, ]




## add county names
df_countynames <- data.frame(county = unique(df1$NAME_2), x = 0, y = 0)

m <- dim(df_countynames)[1]
for(i in 1:m){
	idx <- which(df1_cnty2$NAME_2 == df_countynames$county[i])
	df_countynames$x[i] = median(df1_cnty2$long[idx])
	df_countynames$y[i] = median(df1_cnty2$lat[idx])
}

# -- move Sundbyberg
idx <- which(df_countynames$county == "Sundbyberg")
df_countynames$x[idx] <- df_countynames$x[idx] - 0.01
df_countynames$y[idx] <- df_countynames$y[idx] - 0.02




ggplot() +
geom_polygon(data = df1_cnty2, aes(x = long, y = lat, group = group), fill = "#edcf3d", color = "white", size = 0.01)  +
geom_point(data = df_se, aes(x = long, y = lat), position = position_jitter(width = 0.02, height = 0.02), shape = 21, alpha = 0.1, color = "#1dba61") +
#geom_density2d(data = df_se, aes(x = long, y = lat), size = 0.01, n = 200, color = "#1dba61") +
geom_text(data = df_countynames, aes(x = x, y = y, label = county), size = 1, alpha = 1, color = "white") +
theme(aspect.ratio = 1.6, 
	panel.background = element_rect(fill = '#e5e3d4'),
	panel.grid.major = element_blank(), 
	panel.grid.minor = element_blank())

ggsave(file = paste(analyseDate, "_login_ip_by_county.pdf", sep = ""), width = 20, height = 32, useDingbats=FALSE)
dev.off()	









