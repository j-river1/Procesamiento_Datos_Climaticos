#Load functions
source("HOURLYCONTROL.R")

#Set working directory where are weathers data.
setwd("D:/OneDrive - CGIAR/GitHub/Nuevo Codigo Clima/Datos_Sonora/Datos")

#Restrictions as data frame
Variables <- c("Vmin", "Vmax")
TX <- c(40,0)
TM <- c(40,0)
SR <- c(1600,0)
LONG <- c(-110.119, NA)
LAT <- c(27.51458, NA)
TZ <- c("Etc/GMT+7", NA)

Hourly_restric <- data.frame(TX, TM, SR, LONG, LAT, TZ)






results <- lapply(list.files(), function (x, restricfile = Hourly_restric) results(x))
results <- lapply(list.files(), results, restricfile = Hourly_restric)

