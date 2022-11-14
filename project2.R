# the Twitter data is loaded into R
# list the column names of the imorted .csv file
Tweets2014 <- read.csv("pu2014.csv")
library("sp")
library("sf")
library("tmap")
library("tmaptools")
library("plyr")
library("dplyr")
library("tidyverse")
library("rgdal")
library("anytime")
library("mapview")
library("OpenStreetMap")
library("lubridate")
# Create a sp object, first create a database of only long, lat values from the Twitter data
sp_coords <- select(Tweets2014, longitude, latitude)
epoch_userid <- select(Tweets2014,epoch, user_id)
# Create SpatialPoints object with sp_coords and a proper coordinate reference system
sp_points <- SpatialPoints(coords = sp_coords,proj4string = CRS("+proj=longlat +datum=WGS84"))
# Create a SpatialPoints DataFrame to add additional input data from the Tweets Data
# Add epoch and user_id to the dataframe
points_spdf <- SpatialPointsDataFrame(coords = sp_coords,data = epoch_userid,proj4string = CRS("+proj=longlat +datum=WGS84"))
# Create an sf object with a coordinate reference system, crs is using the EPSG code for WGS 84
# This creates a dataframe in one step instead of the multiple steps required using the sp, it also has all of the data from the original excel file
sf_points <- st_as_sf(Tweets2014, coords = c("longitude", "latitude"), crs = 4326)
# Create a shapefile of the points_spdf SpatialPointsDataFrame
writeOGR(obj=points_spdf, dsn='C:/Users/pclea/Documents/Fall 19/Data Analytics/Project 2', layer='points_spdf', driver= 'ESRI Shapefile', overwrite_layer=T)
# Create a shapefile of the sf_points sf DataFrame
write_sf(obj=sf_points, dsn='C:/Users/pclea/Documents/Fall 19/Data Analytics/Project 2', layer='sf_points', driver= 'ESRI Shapefile')
# Create a kml file from the long and lat coordinates of the original data
writeOGR(obj=points_spdf, dsn='points_spdf.KML', layer='points_spdf_kml', driver= 'KML')
# Create a bounding box from each of the sf and sp objects
bb_points_spdf <- bb(points_spdf)
bb_sf_points <- st_bbox(sf_points)
# get the dimensions of each bounding box and check that they have the same values
typeof(bb_points_spdf); dim(bb_points_spdf); length(bb_points_spdf);bb_points_spdf
typeof(bb_sf_points); dim(bb_sf_points); length(bb_sf_points);bb_sf_points
#Check CRS of sf object sf_points, correctly reports WGS 84
st_crs(sf_points)
#Check CRS of sp object points_spdf, correctly reports WGS 84
st_crs(points_spdf)
# Read in the points_spdf shapefile, this is the shapefile created from the sp object
readin_points_spdf <- readOGR(dsn = 'C:/Users/pclea/Documents/Fall 19/Data Analytics/Project 2/Project2/Shapefiles', layer = 'points_spdf')
# Read in the sf_points shapefile, this is the shapefile created from the sf object
readin_sf_points <- st_read(dsn = 'C:/Users/pclea/Documents/Fall 19/Data Analytics/Project 2/Project2/Shapefiles', layer = 'sf_points')
# Read in the points_spdf.KML file, this is the kml file (Google Earth format) created from the sp object
readin_points_spdf_kml <- st_read("C:/Users/pclea/Documents/Fall 19/Data Analytics/Project 2/Project2/points_spdf.KML")
# convert epoch time to day-month-year, day of week, and local time
epoch <- Tweets2014[[1]]
dates <- anytime(epoch)
dates2 <- as_datetime(epoch)
# Convert the format of the dates to Day-Month-Year
dmyDates <- format(dates,'%d-%m-%Y')
# convert the format of the time to Hours, Minutes, Seconds
hmstime <- format(as.POSIXct(dates, '%H:%M:%S'))
# Lines 53 - 56 is the process employed to seperate the date from the time
temp <- strsplit(hmstime, " ")
mat  <- matrix(unlist(temp), ncol=2, byrow=TRUE)
dftemp  <- as.data.frame(mat)
hmstime <- dftemp[[2]]
# Creat a column of just hours using lubridate
temptime <- hms(as.character(hmstime))
hrs <- hour(temptime)
# Display the day of the week of the tweet
dayofweek <- weekdays(dates)
# Add this new data to a new subset
Tweetssubset <- select(Tweets2014,epoch,user_id,longitude,latitude)
Tweetssubset <- cbind(Tweetssubset,dmyDates,hmstime,dayofweek,hrs,dates2) 
View(Tweetssubset)
# Create an sf object from the Tweetssubset data frame, it is not possible to add columns to an sf object 
sf_Tweets <- st_as_sf(Tweetssubset, coords = c("longitude", "latitude"), crs = 4326)  # Will use this sf object for the rest of the project
# Create a new bbox for sf_Tweets around West Lafayett
new_bb = c(-86.963, 40.416, -86.898, 40.472)
names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
attr(new_bb, "class") = "bbox"
attr(st_geometry(sf_Tweets), "bbox") = new_bb
# Return the bbox of sf_Tweets
bb_sf_Tweets <- st_bbox(sf_Tweets)
typeof(bb_sf_Tweets); dim(bb_sf_Tweets); length(bb_sf_Tweets);bb_sf_Tweets
# Before mapping check the crs of sf_Tweets; confirms sf_Tweets has a crs using long,lat on the WGS84 datum
crs <- st_crs(sf_Tweets)
crs
# Map all tweets over the area for 2014
tmap_mode("plot")
b_map <- read_osm(st_bbox(sf_Tweets))  #use open street map as basemap
tm_shape(b_map) + tm_rgb() + tm_shape(sf_Tweets) + tm_dots(col = "blue") + tm_style('white') + tm_layout(bg.color = 'grey85', title.size = 1.5, title = "West Lafayette Tweets 2014", title.position = c('right','top'), scale = 1, legend.outside = TRUE, legend.text.size = 1) + tm_compass(position = c("left","bottom"), type = "arrow", show.labels = 1,size = 1.5, text.size = 1) + tm_credits(position = c("left","bottom"), size = 0.5, text = "Author: Paul Cleary, Coordinate System = WGS 84 (long,lat)")
#Color-coded map based on day of week
tmap_mode("plot")
#tm_shape(b_map) + tm_rgb() + tm_shape(sf_Tweets) + tm_dots("dayofweek", palette = "RdYlBu") + tm_style('white') + tm_layout(legend.title.size = .7, legend.height = .2, legend.frame = TRUE, legend.frame.lwd = 1.5, legend.bg.color = TRUE, legend.width = .3, legend.position = c("right","center"), bg.color = 'grey85', title.size = 1.5, title = "Tweets Over The Week, 2014", title.position = c('center','top'), scale = 1, legend.outside = FALSE, legend.text.size = .5) + tm_compass(position = c("left","bottom"), type = "arrow", show.labels = 1,size = 1.5, text.size = 1) + tm_credits(position = c("left","bottom"), size = 0.5, text = "Author: Paul Cleary, Coordinate System = WGS 84 (long,lat)") + tm_scale_bar(position = c("right","bottom"))
# Find out/show the spatial-temporal distribution/trajectory of selected 3 individuals
tm_shape(b_map) + tm_rgb() + tm_shape(sf_Tweets, filter = sf_Tweets$user_id == "2840944768") + tm_dots(col = "red") + tm_shape(sf_Tweets, filter = sf_Tweets$user_id == "861913753") + tm_dots(col = "blue") + tm_shape(sf_Tweets, filter = sf_Tweets$user_id == "175271622") + tm_dots(col = "green") + tm_style('white') + tm_layout(legend.title.size = .7, legend.height = .2, legend.frame = TRUE, legend.frame.lwd = 1.5, legend.bg.color = TRUE, legend.width = .3, legend.position = c("right","center"), bg.color = 'grey85', title.size = 1.5, title = "Three People: A Partier, Studier, and Sports Fan", title.position = c('center','top'), scale = 1, legend.outside = FALSE, legend.text.size = .5) + tm_compass(position = c("left","bottom"), type = "arrow", show.labels = 1,size = 1.5, text.size = 1) + tm_credits(position = c("left","bottom"), size = 0.5, text = "Author: Paul Cleary, Coordinate System = WGS 84 (long,lat)") + tm_scale_bar(position = c("right","bottom"))
# Color-code the map based on selected individual users; sports fans vs all others on saturdays
date1 <- ymd_hms("2014-08-15 00:00:01")
date2 <- ymd_hms("2014-12-30 23:59:59")
fall14 <- interval(date1, date2)
date3 <- ymd_hms("2014-01-01 00:00:01")
date4 <- ymd_hms("2014-05-01 23:59:59")
spring14 <- interval(date3,date4)
sportsfans_id <- c("546912657", "401752590", "351249382", "370802122", "470819940", "977080802", "39490620", "359394820", "521392179", "468332046") 
tm_shape(b_map) + tm_rgb() + tm_shape(sf_Tweets, filter = sf_Tweets$dayofweek == "Saturday" & sf_Tweets$dates2 %within% spring14) + tm_dots(col = "black") + tm_shape(sf_Tweets, filter = sf_Tweets$user_id %in% sportsfans_id & sf_Tweets$dayofweek == "Saturday" & sf_Tweets$dates2 %within% spring14) + tm_dots(col = "red") + tm_style('white') + tm_layout(legend.title.size = .7, legend.height = .2, legend.frame = TRUE, legend.frame.lwd = 1.5, legend.bg.color = TRUE, legend.width = .3, legend.position = c("right","center"), bg.color = 'grey85', title.size = 1.5, title = "Basketball Season Tweets on Saturdays: Fans vs Everyone Else", title.position = c('center','top'), scale = 1, legend.outside = FALSE, legend.text.size = .5) + tm_compass(position = c("left","bottom"), type = "arrow", show.labels = 1,size = 1.5, text.size = 1) + tm_credits(position = c("left","bottom"), size = 0.5, text = "Author: Paul Cleary, Coordinate System = WGS 84 (long,lat)") + tm_scale_bar(position = c("right","bottom"))
tm_shape(b_map) + tm_rgb() + tm_shape(sf_Tweets, filter = sf_Tweets$dmyDates == "18-01-2014") + tm_dots(col = "red") + tm_facets(by = "hrs", free.coords = FALSE)
