#Load functions
source("HOURLYCONTROL.R")
source("CONVERTHOURLYTODAILY.R")

#Set working directory where are weathers data.
setwd("D:/OneDrive - CGIAR/GitHub/Nuevo Codigo Clima/Datos_Sonora/Datos")
setwd("C:/Users/JCRIVERA/Documents/Codigo_Nuevo_Clima/Datos/Datos_Prueba")

#Create folders
mainDir <- getwd()
dir.create(file.path(mainDir, "Orginal_Data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "AfterHourlyControl_Data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "AfterDailyControl_Data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "RandomForest"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Rmawgen"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Graphics"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Results"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Final_Data"), showWarnings = FALSE)





#Restrictions as data frame
Variables <- c("Vmin", "Vmax")
TX <- c(41,0)
TM <- c(41,0)
SR <- c(1600,0)
RH <- c(100,0)
LONG <- c(-110.119, NA)
LAT <- c(27.51458, NA)
TZ <- c("Etc/GMT+7", NA)
#Start_date <- c("2013-1-1")
#End_date <- c("2017-12-31")

Start_date <- c("2013-1-1")
End_date <- c("2017-12-31")

Hourly_restric <- data.frame(TX, TM, SR,RH, LONG, LAT, TZ)

final_results <- mclapply(list.files(), results, restricfile = Hourly_restric ,mc.cores=20)
final_results <- do.call("rbind", final_results)
colnames(final_results) <- c("Station_Name", "Variable_Name", "OriginalData_Size", "CleanData_Size", "ErrorData_Size")
write.csv(final_results, file = "Results_HourlyControl.csv")


#Path for hourly files. Copy and paste files to Hourly_Data
dir.create(file.path(getwd(), "Hourly_Data"), showWarnings = FALSE)


#Path for hourly files. Copy and paste files to Hourly_Data
path_hourly <- paste(getwd(), "Hourly_Data", sep = "/")
move_files_txt(from = getwd(), to = path_hourly)

#Change Directory
setwd(path_hourly)

#Hourly to Daily
#The percentage is used for checking if a day has enough data.   
mclapply (list.files(pattern = "\\.txt$"), Hour_to_Day, percentage = 0.8,mc.cores=20)

#Change Directory
path_daily <- paste(getwd(), "Daily_Data", sep = "/")
setwd(path_daily)

#Path for files with Rmwagen format. Copy and paste files to Rmwagen
dir.create(file.path(getwd(), "Rmawgen"), showWarnings = FALSE)

#Variables para Rmwagen
put_rmawgenformat(list.files(pattern = "\\.txt$"), 'TX', Start_date, End_date)
put_rmawgenformat(list.files(pattern = "\\.txt$"), 'TM', Start_date, End_date)
put_rmawgenformat(list.files(pattern = "\\.txt$"), 'P', Start_date, End_date)


#move files to Rmawgen folder
Rmawgen <- paste(getwd(), "Rmawgen", sep = "/")
move_files_txt(from = getwd(), to = Rmawgen, format = "\\.csv$")


setwd(Rmawgen)

