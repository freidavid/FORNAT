###############################################################################################
###############################################################################################
#Calculation of mean velocities from camera trap data for REM population density estimation
###############################################################################################
###############################################################################################
#Distances in each frame need to be estimated beforehand, we used the approach by Haucke et al. :
#Haucke, T., Kühl, H. S., Hoyer, J., & Steinhage, V. (2022). Overcoming the distance estimation bottleneck in estimating animal abundance with camera traps. Ecological Informatics, 68, 101536. https://doi.org/10.1016/j.ecoinf.2021.101536
#https://github.com/timmh/distance-estimation
###############################################################################################


#Load all required packages
library(secr)
library(dplyr)
library(lubridate)
library(sp)
library(ggplot2)
library(geosphere)

###############################################################################################
###############################################################################################
#Load and prepare input data
###############################################################################################

#Load input data
results <- read.csv("~/distances.csv", sep=";")

#adjustements to input data 
names <- colnames(results)
names[5] <- "distance"
colnames(results) <- names

#load second input data table and merge it with first one by pictureID
observations <- read.csv("~/observations.csv",sep=";")
combined <- merge(observations, results, by.x="pictureID", by.y="frame_id")

#Load third data table, create id and merge it to create one input table
timelapse <- read.csv("~/timelapse.csv",sep=";")
timelapse$identifier <- paste0(timelapse$RelativePath, timelapse$File)
all_data <- merge(combined, timelapse, by.x="pictureID", by.y="identifier")
columns<-colnames(all_data)
columns[2]<-"Region.Label"
columns[3]<-"Sample.Label"
colnames(all_data)<-columns

###############################################################################################
###############################################################################################
#Load and prepare coordinate data
###############################################################################################
#load coordinates of camera traps
koord <- read.csv("~/koordinaten_standorte.csv", sep=";")#read file, three columns: continous numbering ("Stichproben_Nr"), ID of camera trap, coordinates
koord$Stichproben_Nr<-paste0("Standort_",koord$Stichproben_Nr)#This refers to column 1 

#split up coordinates into x and y coordinates
koordx<-rep(NA,length(all_data$pictureID))#create vector for x coords
koordy<-rep(NA,length(all_data$pictureID))#create vector for y coords

for(i in 1:length(koord$Stichproben_Nr)){
  replacement<-rep(koord[i,]$X,length(which(all_data$Station==koord[i,]$Stichproben_Nr)))#extract x coords
  koordx[which(all_data$Station==koord[i,]$Stichproben_Nr)]<-replacement#store them

  replacement<-rep(koord[i,]$Y,length(which(all_data$Station==koord[i,]$Stichproben_Nr)))#extract y coords
  koordy[which(all_data$Station==koord[i,]$Stichproben_Nr)]<-replacement#store them
  }

#Add to the dataframe with all data
all_data<-cbind(all_data,koordx,koordy)#store coordinates in the dataframe with all information
#create subset of data to work with
data<-as.data.frame(cbind(all_data$Station,all_data$EventID.x,koordx,koordy,all_data$count,all_data$DateTimeOriginal))#subset data
data <- data.frame(Fotofalle = data$V1,
                   Event=data$V2,
                        X = data$koordx,
                        Y = data$koordy,
                        Begegnung = data$V5,
                        time=data$V6)#labeling the columns
data$time <- as.POSIXct(data$time)#correction of time columnn
data <- na.omit(data)#remove NA's 


# Convert date and time to POSIXct-format 
data$datetime <- as.POSIXct(data$time, format="%Y-%m-%d %H:%M:%S")

#Rename coordinate column according to package and make them numeric
data$longitude <- as.numeric(data$X)
data$latitude <- as.numeric(data$Y)

#Remove empty entries
data <- data %>% filter(!is.na(longitude) & !is.na(latitude))

#Set coordinates and CRS (this is the CRS of the input data)
coordinates(data) <- ~longitude+latitude
proj4string(data) <- CRS("+init=epsg:21781") # Swiss CRS

#Transformation to WGS84-Koordinatensystem
data_wgs84 <- spTransform(data, CRS("+proj=longlat +datum=WGS84"))

#Check if it worked
print(proj4string(data_wgs84))

#Convert back to a dataframe
data_wgs84 <- as.data.frame(data_wgs84)


###############################################################################################
###############################################################################################
#Calculation of daily range
###############################################################################################


#Calculation of distance between two consecutive sightings
data_wgs84 <- data_wgs84 %>% arrange(time)
data_wgs84$dist_to_next <- c(NA, distVincentySphere(
  cbind(data_wgs84$coords.x1[-nrow(data_wgs84)], data_wgs84$coords.x2[-nrow(data_wgs84)]),
  cbind(data_wgs84$coords.x1[-1], data_wgs84$coords.x2[-1])
))

#Add a date and time column
data_wgs84$date <- as.Date(data_wgs84$time)


#Calculation of daily ranges
daily_distances <- data_wgs84 %>%
  group_by(date) %>%
  summarize(daily_distance = sum(dist_to_next, na.rm = TRUE))
print(daily_distances)


#Calculation of average daily ranges
average_daily_distance <- mean(daily_distances$daily_distance, na.rm = TRUE)

 ggplot(daily_distances, aes(x=date, y=daily_distance)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title="Täglich zurückgelegte Distanzen", x="Datum", y="Distanz (Meter)")#Plot results



###############################################################
###############################################################
#Calculation of mean velocities with daily ranges
###############################################################
 
  
  #Adding a date and time column
  data_wgs84$date <- as.Date(data_wgs84$datetime)
  
  
  # Caluclation of distances and time intervals between two consecutive sighting
    data_wgs84 <- data_wgs84 %>%
    arrange(datetime) %>%
    mutate(
      dist_to_next = c(NA, distVincentySphere(
        cbind(coords.x1[-n()], coords.x2[-n()]),
        cbind(coords.x1[-1], coords.x2[-1])
      )),
      time_to_next = c(NA, as.numeric(difftime(datetime[-1], datetime[-n()], units = "secs")))
    )
  
  # Check if everything worked properly
  print(head(data_wgs84))
  
  # Calculation of daily covered distances and total time
  daily_data <- data_wgs84 %>%
    group_by(date) %>%
    summarize(
      daily_distance = sum(dist_to_next, na.rm = TRUE),
      daily_time = sum(time_to_next, na.rm = TRUE)
    )
  
  # Calculation of average velocities
  daily_data <- daily_data %>%
    mutate(average_speed = daily_distance / daily_time * 3600) # transformation to meter per hour

  # Calculation of average velocity in km/h
  total_distance <- sum(daily_data$daily_distance) / 1000  # total distance in kilometer
  total_time <- sum(daily_data$daily_time) / 3600  # total time in hours
  average_speed_kmh <- total_distance / total_time  # average velocity in km/h
  
  ###############################################################
  
  #Plotting:
  #Transormation from m/h to km/h
  meters_per_second_to_kmh <- 3.6
  
  # Plot daily ranges
  plot_distances <- ggplot(daily_data, aes(x = date, y = daily_distance / 1000)) + # Umrechnung in Kilometer
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    theme_minimal() +
    labs(
      title = "Täglich zurückgelegte Distanzen",
      x = "Datum",
      y = "Distanz (km)"
    )
  
  # Plot average velocities
  plot_speed <- ggplot(daily_data, aes(x = date, y = average_speed * meters_per_second_to_kmh / 1000)) + # Umrechnung in km/h
    geom_line(color = "red") +
    geom_point(color = "red") +
    theme_minimal() +
    labs(
      title = "Durchschnittliche Geschwindigkeit",
      x = "Datum",
      y = "Geschwindigkeit (km/h)"
    )
  
  # Print plots next to each other
  grid.arrange(plot_distances, plot_speed, ncol = 1)
  
###############################################################
###############################################################
###############################################################
  
  
  
  
  
  
