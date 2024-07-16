
###############################################################################################
###############################################################################################
#Calculation of population densities from camera trap data  using an REM approach
###############################################################################################
###############################################################################################
#We use the package and approach published by Caravaggi et al.:
#Caravaggi A. (2017) remBoot: An R package for Random Encounter Modelling. Journal of Open Source Software. 2(10). doi: 10.21105/joss.00176
#
#1. Distances in each frame need to be estimated beforehand, we used the approach by Haucke et al. :
#Haucke, T., Kühl, H. S., Hoyer, J., & Steinhage, V. (2022). Overcoming the distance estimation bottleneck in estimating animal abundance with camera traps. Ecological Informatics, 68, 101536. https://doi.org/10.1016/j.ecoinf.2021.101536
#https://github.com/timmh/distance-estimation
#
#2. From the distances and coordinates of the camera traps, the mean velocity of the studied 
#species needs to be estimated. This value needs to be known before hand. The script to do so
#can be found here: https://github.com/freidavid/FORNAT/blob/main/R/Daily_Range.R
###############################################################################################


#load all required packages
library(devtools)
library(remBoot)#devtools::install_github("arcaravaggi/remBoot")
library(dplyr)
library(lubridate)

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

#make a new dataframe and rename the columns
df<-all_data
df_summary <- df %>%
  group_by(EventID.x) %>%
  slice(which.max(count)) %>%
  ungroup()

remDat<-df_summary
remDat <- remDat %>% 
  rename("cam"=transect_id,
         "count"=count)


#subset datframe to needed columns
remDat<-as.data.frame(cbind(as.numeric(factor(remDat$cam)),as.numeric((remDat$count)))) #subset
colnames(remDat) <- c("cam", "count") #rename columns


###############################################################################################
###############################################################################################
#Calculation of population densities
###############################################################################################

#Set parameters
remDat$dist<-max(df$distance)/1000 #set distance
remDat$theta<-40*(pi/180) #fraction of viewangle
nboots <- 1000 #number of bootsraps
tm <- 14*24*(length(unique(cams))) #Total camera hours
v <- 0.2314 #Estimated mean velocity from https://github.com/freidavid/FORNAT/blob/main/R/Daily_Range.R

#estimate densities 
density_data<-rem(dat = remDat, tm=tm, v=v) #actual density estimation
density_data <- data.frame(season=c("Sommer"), 
                        species="Wildschwein", 
                        density=c(density_data), 
                        sd=NA) #store results
density_data$sd<- sd(boot_sd(remDat))#add the standard deviation
print(density_data)#print the results

###############################################################################################
