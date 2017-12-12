#Libraries

library(dplyr)
library(stringr)
library(maptools)
library(parallel)
library(reshape2)
library(imputeTS)
library(plyr)
library(abind)
library(zoo)
library(stringr)
library(RMAWGEN)
library(ggplot2)



#Load functions
source("HOURLYCONTROL.R")
source("DAILYCONTROL.R")
source("FORMATRMAWGEN.R")
source("RANDOMFOREST.R")
source("RMAWGEN.R")




#The setwd() must be directory where are the source above.
#Put into the folder Original_Data all weaher files data.
 



#Create folders
mainDir <- getwd()
dir.create(file.path(mainDir, "Original_Data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "AfterHourlyControl_Data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "AfterDailyControl_Data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "RandomForest"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Rmawgen"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Graphics"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Results"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Final_Data"), showWarnings = FALSE)
mainDir <- paste0(mainDir,"/", "Rmawgen" )
dir.create(file.path(mainDir, "Files_By_Station" ), showWarnings = FALSE)




#Restrictions as data frame
Variables <- c("Vmin", "Vmax")
TX <- c(41,0)
TM <- c(41,0)
SR <- c(1600,0)
RH <- c(100,0)
LONG <- c(-110.119, NA)
LAT <- c(27.51458, NA)
TZ <- c("Etc/GMT+7", NA)
Hourly_restric <- data.frame(TX, TM, SR,RH, LONG, LAT, TZ)

#Variables 
Start_date <- c("2013-1-1")
End_date <- c("2017-12-31")
Percentage <- 0.8

#Change directory Original_Data
setwd("./Original_Data")


#Hourly Control
#final_results <- mclapply(list.files(), results, restricfile = Hourly_restric ,mc.cores=20)
final_results <- lapply(list.files(), results, restricfile = Hourly_restric)

#Results of Hourly Control
final_results <- do.call("rbind", final_results)
colnames(final_results) <- c("Station_Name", "Variable_Name", "OriginalData_Size", "CleanData_Size", "ErrorData_Size")
write.csv(final_results, file = "../Results/Results_HourlyControl.csv")


#Change Directory to After Hourly Data
setwd("../AfterHourlyControl_Data")


#Hourly to Daily
#The percentage is for checking if a station has enough data per day.   
#mclapply (list.files(pattern = "\\.txt$"), Hour_to_Day, percentage = 0.8,mc.cores=20)
lapply (list.files(), Hour_to_Day, percentage = Percentage)

#Results Daily Control
results <- lapply(list.files(), info_station, percentage=Percentage)
final_results <- do.call("rbind", results)
colnames(final_results) <- c("Station_Name", "Variable_Name", "Star_Data", "End_Data", "Total_Days", "Acceptable_Days","Percentage" )
write.csv(final_results, file = paste0("../Results/","Results_DailyControl.csv") )


#Change Directory to After Daily Data
setwd("../AfterDailyControl_Data")


#File with format for using  Rmwagen
put_rmawgenformat(list.files(), 'TX', Start_date, End_date)
put_rmawgenformat(list.files(), 'TM', Start_date, End_date)
put_rmawgenformat(list.files(), 'P', Start_date, End_date)


#Using Rmwagen 
setwd("../Rmawgen")
graph_all (list.files(pattern = "\\.csv$"), "../Results/Results_DailyControl.csv", "TEMPERATURE_MAX", 'Temperatura_Máxima')
graph_all (list.files(pattern = "\\.csv$"), "../Results/Results_DailyControl.csv", "TEMPERATURE_MIN", 'Temperatura_Mínima')
graph_all (list.files(pattern = "\\.csv$"), "../Results/Results_DailyControl.csv", "PRECIPITATION", "Precipitación")


#move files to Rmawgen folder
Rmawgen <- paste(getwd(), "Rmawgen", sep = "/")
move_files_txt(from = getwd(), to = Rmawgen, format = "\\.csv$")


setwd(Rmawgen)

