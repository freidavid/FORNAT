#Load all required packages
library("activity")
library("Distance")
library("plyr")
library("dplyr")
library("lubridate")
library("stringr")
library("glmmTMB") 
library("mgcv")
library("DHARMa")
library("emmeans")
library("data.table")
library("boot")
library("ggplot2")
library("gridExtra")
library("cowplot")

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

#Load third data table, create identifiere and merge it to create one input table
timelapse <- read.csv("~/timelapse.csv",sep=";")
timelapse$identifier <- paste0(timelapse$RelativePath, timelapse$File)
all_data <- merge(combined, timelapse, by.x="pictureID", by.y="identifier")
columns<-colnames(all_data)
columns[2]<-"Region.Label"
columns[3]<-"Sample.Label"
colnames(all_data)<-columns

###############################################################################################
###############################################################################################
#First inspection of data
###############################################################################################



Artname <- "Wildschwein"#Here, we use wild boar as an example. 


#plot histogram with all distances observed
breakpoints <- c(0,seq(2,25,1)) #create breakpoints for plot
hist(all_data$distance, breaks=breakpoints, main=Artname,xlab="Distanz [m]",ylab="Häufigkeit",freq=T)#plot histogram
trunc<- 10#decision about where to truncate data
abline(v=trunc,lty=3,col="red",lwd=2) 

#calculate how much data will be lost in consequence of trucation decision
length(combined$distance[combined$distance > trunc]) / length(combined$distance) * 100






###############################################################################################
###############################################################################################
#Data Analysis according to tutorial of Howe et al. 2023
###############################################################################################

#calculate activity
all_data$rtime <- gettime(all_data$DateTimeOriginal,  format="%Y-%m-%d %H:%M:%S", scale ="radian")
act_result <- fitact(all_data$rtime, sample="data", reps=100)
#plot an print results
plot(act_result, xlab="Uhrzeit", ylab="Häufigkeit", main=paste0("Aktivität ", Artname))
print(act_result@act)

#calculate correction factor for daily activity and time of camera operation (here it was 24 hours, so it acutally doesn't matter)
camera.operation.per.day <- 24
prop.camera.time <- camera.operation.per.day / 24
avail <- list(creation=data.frame(rate = act_result@act[1] / prop.camera.time,SE = act_result@act[2]/prop.camera.time))

#Calculation of area covered by camera based on detection angle (in this case 40 degrees)
Area=(pi*10^2)*40/360
all_data$Area <- (pi*10^2)*40/360  

#Total effort (time cameras were running in seconds)
all_data$Effort<-1.21e+6

#Area covered
all_data$Area <- all_data$Area / (1000*1000)
all_data$object <- NA
all_data$object[!is.na(all_data$distance)] <- 1:sum(!is.na(all_data$distance))


#Define parameters for model selection
trunc.list <- list(left=0, right=trunc)
mybreaks <- c(0,2:trunc)
conversion <- convert_units("meter", NULL, "square kilometer")

#Make sure objects to store output of models are empty
uni1<-NULL
uni2<-NULL
uni3<-NULL

hn0<-NULL
hn1<-NULL
hn2<-NULL

hr0<-NULL
hr1<-NULL


#Run Models
uni1 <- ds(all_data, transect = "point", key="unif", adjustment = "cos",
           nadj=1, convert_units = conversion,
           cutpoints = mybreaks, truncation = trunc.list)
uni2 <- ds(all_data, transect = "point", key="unif", adjustment = "cos",
           nadj=2, convert_units = conversion,
           cutpoints = mybreaks, truncation = trunc.list)
uni3 <- ds(all_data, transect = "point", key="unif", adjustment = "cos",
           nadj=3, convert_units = conversion,
           cutpoints = mybreaks, truncation = trunc.list)

hn0 <- ds(all_data, transect = "point", key="hn", adjustment = NULL,
          convert_units = conversion, cutpoints = mybreaks, truncation = trunc.list)
hn1 <- ds(all_data, transect = "point", key="hn", adjustment = "cos",
          nadj=1, convert_units = conversion,
          cutpoints = mybreaks, truncation = trunc.list)
hn2 <- ds(all_data, transect = "point", key="hn", adjustment = "cos",
          nadj=2, convert_units = conversion,
          cutpoints = mybreaks, truncation = trunc.list)

hr0 <- ds(all_data, transect = "point", key="hr", adjustment = NULL,
          convert_units = conversion, cutpoints = mybreaks, truncation = trunc.list)
hr1 <- ds(all_data, transect = "point", key="hr", adjustment = "poly",
          nadj=1, convert_units = conversion,
          cutpoints = mybreaks, truncation = trunc.list)


#Model selection step 1
un <- QAIC(uni1,uni2,uni3)
hn <- QAIC(hn0,hn1,hn2)
hr <- QAIC(hr0,hr1)

results_un <- data.frame( un)
results_hn <- data.frame( hn)
results_hr <- data.frame( hr)
 

#Model selection step 2
color.un <- which(un$QAIC == min(un$QAIC))
kable(un,digits=2, row.names = TRUE)%>% kable_paper(full_width = FALSE) %>%row_spec(color.un, bold=TRUE,  background = "orange")

color.hn <- which(hn$QAIC == min(hn$QAIC))
kable(hn,digits=2, row.names = TRUE)%>% kable_paper(full_width = FALSE) %>%row_spec(color.hn, bold=TRUE,  background = "orange")

color.hr <- which(hr$QAIC == min(hr$QAIC))
kable(hr,digits=2, row.names = TRUE)%>% kable_paper(full_width = FALSE) %>%row_spec(color.hr, bold=TRUE,  background = "orange")


chats <- chi2_select(uni1, hn2, hr0)$criteria
modnames <- unlist(lapply(list(uni1, hn2, hr0), function(x) x$ddf$name.message))
results <- data.frame(modnames, chats)
results.sort <- results[order(results$chats),]
kable(results.sort, digits=2, row.names = FALSE) %>% kable_paper(full_width = FALSE) %>%row_spec(1, bold=TRUE,  background = "#4da6ff")


#Store relevant parameters of best fitting model (here it was hr0)
p_a <- hr0$ddf$fitted[1]
w <- range(mybreaks)[2] - range(mybreaks)[1]
rho <- sqrt(p_a * w^2)

#Plot activity and model fit
plot(hr0, main="Daytime activity", xlab="Distance (m)",
     showpoints=FALSE, lwd=3, xlim=c(0, 15))



#population density estimation
viewangle <- 40# degrees
samfrac <- viewangle / 360 #fraction covered by view angle

density <- dht2(hr0, flatfile= all_data, strat_formula = ~1,
                      sample_fraction = samfrac, er_est = "P2", multipliers = avail,
                      convert_units = conversion)#estimate density
print(density, report="density")#print result


#Bootstrap
#multiplier
mult <- list(availability= make_activity_fn(all_data$rtime, sample="data",
                                            detector_daily_duration=camera.operation.per.day))
#summary
mysummary <- function(ests, fit){
  return(data.frame(Label = ests$individuals$D$Label,
                    Dhat = ests$individuals$D$Estimate))
}


#actual bootstrap
n.cores <- parallel::detectCores()
daytime.boot.uni <- bootdht(model=hr0, flatfile=all_data,
                            resample_transects = TRUE, nboot=10000, 
                            cores = n.cores - 4,
                            summary_fun=mysummary, sample_fraction = samfrac,
                            convert_units = conversion, multipliers=mult)
print(summary(daytime.boot.uni))



#plot bootstrap results
hist(daytime.boot.uni$Dhat, breaks = 20, 
     xlab="Estimated density", main="D-hat estimates bootstraps")
abline(v = quantile(daytime.boot.uni$Dhat, probs = c(0.025,0.975), na.rm=TRUE), lwd=2, lty=3)



