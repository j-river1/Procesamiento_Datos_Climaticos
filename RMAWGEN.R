
#list.files()[1] resumefile
#list.files()[130]


#Making a folder for saving files TX, TM

#generate_missing_values generates missing values
#Arguments     -weather list of files datas
#



#generate_missing_values generates values of all stations using rmawgen.
#Arguments    -ListFiles. Lisf of files with format for rmawgen 
#             -resumefile. Lsit with resumen all stations 

#generate_missing_values(list.files(), list.files()[1],  "PRECIPITATION")
generate_missing_values <- function (listFiles, resumefile, variable)
{
    
    station_info <- choose_stations(resumefile)
    TEMPERATURE_MAX <- read.csv(listFiles[which(listFiles=="TX.csv")], header=T)
    TEMPERATURE_MIN <- read.csv(listFiles[which(listFiles=="TM.csv")], header =T)
    PRECIPITATION <-   read.csv(listFiles[which(listFiles=="P.csv")], header= T)
    
    
    if(variable=='TEMPERATURE_MAX')
    {
        #Name files
        generator_values <- lapply(station_info, applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=1)
    }
    
    if(variable=='TEMPERATURE_MIN')
    {
        generator_values <- lapply(station_info[1:3], applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=2)
        
    }    
    
    if(variable=='PRECIPITATION')
    {
        #Each 3 stations for using precipitation.
        station_two <- do.call("rbind", station_info[1:3])
        chunk <- 3
        n <- nrow(station_two)
        r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
        station_info <- split(station_two,r) 
        
        
        #Name files
        generator_values <- lapply(station_info[11], applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=3)
        
    }
    
    
    #TEMPERATURE_MAX <- read.csv(list.files()[which(list.files()=="TX.csv")], header=T)
    #TEMPERATURE_MIN <- read.csv(list.files()[which(list.files()=="TM.csv")], header=T)
    #PRECIPITATION <-   read.csv(list.files()[which(list.files()=="P.csv")], header= T)
    
    #station_info <- choose_stations(list.files()[1])
    
    #station_info <- choose_stations(resumefile)
    #Menu. Temperatura Maxima 1
    #      Temperatura Minima 2
    #      Precipitation      3
    #generator_missing_temperaMax <- lapply(station_info[3], applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=1)
    #generator_missing_temperaMin <- lapply(station_info[1], applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION,  menu=2)
    #generator_missing_precipitation <- lapply(station_info[1], applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION,  menu=3)
    
    
    
    
    
    return(generator_values)
}

#Applying rmawagen works rmwagen function 
#Arguments - info_station: dataframe with station name, start and  end date

applying_rmwagen <- function (info_station, TEMPERATURE_MAX, TEMPERATURE_MIN, PRECIPITATION, n_GPCA_iter = 10, n_GPCA_iteration_residuals =10, lag=2, p_prec = 3, p_test =2,menu)
{
    #Arguments for rmwagen
    station <- as.vector(info_station$station_names)
    year_min <- as.Date(info_station$date_min, "%Y-%m-%d")
    year_max <- as.Date(info_station$date_max, "%Y-%m-%d")
    
    year_min <- as.numeric(format(year_min[1], "%Y"))
    year_max <- as.numeric(format(year_max[1], "%Y"))
    
    #Start and End Data
    Start_Data_Sta <- unique(year_min)
    End_Data_Sta <- unique(year_max)
    All_data <- seq(as.Date(paste0(year_min,"-1-1")), as.Date(paste0(year_max, "-12-31")), by="days")
    
    #year_min <- as.Date(info_station[[4]]$date_min, "%Y-%m-%d")[1]
    #year_max <- as.Date(info_station[[4]]$date_max, "%Y-%m-%d")[1]
    #year_min <- as.numeric(format(year_min, "%Y"))
    #year_max <- as.numeric(format(year_max, "%Y"))
    
    
    
    #origin <- unique(as.Date(info_station$date_min, format = "%Y-%m-%d"))
    #origin <- unique(as.Date(info_station$date_min, format = "%Y-%m-%d"))
    
    
    generationTemperature <- ComprehensiveTemperatureGenerator(
        station=station,
        Tx_all=TEMPERATURE_MAX,
        Tn_all=TEMPERATURE_MIN,
        year_min=year_min,
        year_max=year_max,
        p=5,
        n_GPCA_iteration=n_GPCA_iter,
        n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
        sample="monthly"
    )
    
    if(menu == 1 )
    { 
        
        real_data <- generationTemperature$input$Tx_mes
        fill_data <-generationTemperature$out$Tx_gen
    } 
    
    if(menu == 2)
    {
        real_data <- generationTemperature$input$Tn_mes
        fill_data <-generationTemperature$out$Tn_gen
    }
    
    if(menu == 3)
    {
        #Temperature as exogenous variable
        exogen_sim <- cbind(generation00_temp$output$Tx_gen,generation00_temp$output$Tn_gen)
        exogen_sim <- cbind(generationTemperature$out$Tx_gen,generationTemperature$out$Tn_gen)
        names(exogen_sim) <- cbind(paste(names(generationTemperature$out$Tx_gen),"_Tx",sep=""),paste(names(generationTemperature$out$Tn_gen),"_Tn",sep=""))
        
        exogen <- cbind(generation00_temp$input$Tx_mes,generation00_temp$input$Tn_mes)
        exogen <- cbind(generationTemperature$input$Tx_mes,generationTemperature$input$Tn_mes)
        
        names(exogen) <- cbind(paste(names(generation00_temp$input$Tx_mes),"_Tx",sep=""),paste(names(generation00_temp$input$Tn_mes),"_Tn",sep=""))
        names(exogen) <- cbind(paste(names(generationTemperature$input$Tx_mes),"_Tx",sep=""),paste(names(generationTemperature$input$Tn_mes),"_Tn",sep=""))
        
        #generation_prec <- ComprehensivePrecipitationGenerator(station=stationUSE,
        #                                                         prec_all=precipitation,
        #                                                         year_min=year_min,
        #                                                         year_max=year_max,
        #                                                         exogen=exogen,
        #                                                         exogen_sim=exogen_sim,
        #                                                         p=5,n_GPCA_iteration=n_GPCA_iter_prec,
        #                                                         n_GPCA_iteration_residuals=n_GPCA_iteration_residuals_prec,
        #                                                         sample="monthly",valmin=valmin,extremes=TRUE,no_spline = TRUE,
        #                                                         activateVARselect = FALSE)
        
        generation_prec <- ComprehensivePrecipitationGenerator(
            station=station,
            prec_all=PRECIPITATION,
            year_min=year_min,
            year_max=year_max,
            exogen=exogen,
            p= 3,
            exogen_sim=exogen_sim ,
            exogen = NULL,
            n_GPCA_iteration= 10,
            n_GPCA_iteration_residuals= 10,
            sample = "monthly",
            no_spline = FALSE,
            leap = TRUE)
        
        #real_data <- generation_prec$prec_mes
        #fill_data <- generation_prec$prec_gen
        
        real_data <- PRECIPITATION[c(station, "year")]
        fill_data <- PRECIPITATION[c(station, "year")]
        
        real_data <- subset(real_data, year >=year_min  & year <=year_max)
        fill_data <- subset(fill_data, year >=year_min  & year <=year_max)
        
        real_data <- real_data[,-which(colnames(real_data)=="year")]
        fill_data <- fill_data[,-which(colnames(fill_data)=="year")]
        
        #nrow_realdata <- nrow(real_data)
        #nrow_filldata <- nrow(fill_data)
        
        #if(nrow_realdata < nrow_filldata) 
        #{
        # n <- nrow_filldata - nrow_realdata
        #  fill_data <- real_data[-rep(1:n),]
        #  All_data <-All_data[-rep(1:n)]
        #}
        
        #if(nrow_filldata < nrow_realdata)
        #{
        # n <- nrow_realdata - nrow_filldata
        #real_data <- fill_data[--rep(1:n),]
        #All_data <-All_data[-rep(1:n)]
        #}
        
    }
    
    result <- list(real_data= real_data, estimated_data = fill_data, date=All_data)
    
    return(result)
    
    
}




#graph_station plots graph per variable
#- Arguments.   Station_table 

graph_station <- function (Station_table, variable)
{
    
    #Parameters
    real_data <- Station_table$Real_Data
    estimated_data <- Station_table$Estimated_Data
    dates <- Station_table$Date
    name <- unique(Station_table$Station_Names)
    
    #Units
    if(variable == 'Temperatura_Máxima' || variable == 'Temperatura_Mínima')
    {
        y = "Grados_Centigrados"
    }
    
    if(variable == 'Precipitación')
    {
        
        y = "Mililitros"
    }
    
    if(variable == 'Radiación_Solar')
    {
        y = "Calorias_cm2_diarios"
    }
    
    if(variable == 'Humedad_Relativa')
    {
        y = "Valor"
    }
    
    #Total Data
    dates <- as.Date(dates, format = "%Y-%m-%d")
    
    #Count NA
    NAs <-  which((is.na(real_data=="NA"))==TRUE)
    real_data[NAs] <- estimated_data[NAs]
    
    
    real_dat <- cbind.data.frame(dates, real_data)
    estimated_dat <- cbind.data.frame(dates, estimated_data)
    
    colnames(real_dat) <- c("Dates", "Value")
    colnames(estimated_dat) <- c("Dates", "Value")
    
    #Variable for plot
    #grafica <- rbind(real_dat, estimated_dat)
    grafica <- real_dat
    #grafica$Datos <- c(rep("Datos_Reales", nrow(real_dat)), rep("Datos_Estimados", nrow(estimated_dat)))
    grafica$Datos <- c(rep("Datos_Reales", nrow(real_dat)))
    grafica$Datos[NAs] <- c("Datos_Estimados")
    
    #Graph
    graph <- ggplot(data=grafica, aes(x=Dates, y=Value, col=Datos)) + geom_point() +ggtitle(paste0(name,"\n",variable)) + theme(plot.title = element_text(hjust = 0.5)) + ylab(y) + xlab("Dias")
    name <- paste0(paste(name,variable, sep="_"),".pdf")
    namefile <-  paste0("..", "/", "Graphics", "/", name)
    ggsave(namefile, plot=graph)
    #ggsave(paste0(paste(name,variable, sep="_"),".pdf"), plot=graph)
    
    #return(name)
    #write.table (aftercleaning, file = originaldata, row.names = FALSE, quote = FALSE, sep = "\t", col.names = TRUE)
    
    if(variable == 'Temperatura_Máxima' )
    {
        
        namefile = "TX"
    }
    
    if(variable == 'Temperatura_Mínima' )
    {
        
        namefile = "TM"
    }
    
    
    if(variable == 'Temperatura_Mínima' )
    {
        
        namefile = "TM"
    }
    
    if(variable == 'Precipitación')
    {
        
        namefile = "P"
    }
    
    if(variable == 'Radiación_Solar')
    {
        namefile = "SR"
    }
    
    if(variable == 'Humedad_Relativa')
    {
        namefile = "RH"
    }
    
    name <- paste0(name,"_", namefile, ".txt")
    weather_data <- paste0(".", "/", "Files_By_Station", "/", name )   
    
    
   # write.table(real_dat, file = paste0(name,"_", namefile, ".txt"), row.names = FALSE, quote = FALSE, sep = "\t", col.names = TRUE)
   write.table(real_dat, file = weather_data, row.names = FALSE, quote = FALSE, sep = "\t", col.names = TRUE)
   
}

#generate_missing_values <- function (listFiles, resumefile, variable)
#graph_ graphs all stations estimated and real data
#Arguments    -listFiles. List of files with format rmawgen 
#             -variable_rmw. variable for using in rmwagen
#             -variable_plot. variable for plot
#Return graphs

graph_all <- function(listFiles, resumefile, variable_rmw, variable_plot)
{
    
    #Data
    data_all <- generate_missing_values (listFiles, resumefile, variable_rmw)
    
    #Table per station
    table_station <- lapply(data_all, table_graph, resumefile = resumefile)
    lapply(table_station, function(x) lapply(x, graph_station, variable = variable_plot))
}



#table_graph makes tables for plotting

table_graph <- function(list, resumefile)
{
    
    #List with data
    data_real  <- list$real_data
    data_estimated <- list$estimated_data
    date <- list$date
    
    #Number of stations
    num_stations <- length(colnames(data_real)) 
    
    #Extract start and end data
    info_station <- read.csv(resumefile, header= TRUE)
    info_station$Station_Name <- as.character(info_station$Station_Name)
    #info_station$Star_Data <- as.Date(info_station$Star_Data, format = "%m/%d/%Y")
    info_station$Star_Data <- as.Date(info_station$Star_Data, format = "%Y-%m-%d")
    #info_station$End_Data <- as.Date(info_station$End_Data, format = "%m/%d/%Y")
    info_station$End_Data <- as.Date(info_station$End_Data, format = "%Y-%m-%d")
    
    info_station <- unique(info_station[,c("Station_Name", "Star_Data",  "End_Data" ) ])
    info_station <- subset(info_station, Station_Name %in% colnames(data_real))   
    
    station <- list()
    name_station <- list()
    
    #Station 
    for ( i in 1 :num_stations)
    {
        name_station[[i]]  <- rep(colnames(data_real)[i], length(data_real[,i]))
        station[[i]] <- data.frame(date, data_real[,i], data_estimated[,i],name_station[[i]])
        names(station[[i]]) <- c("Date", "Real_Data", "Estimated_Data", "Station_Names")
        star_date <- subset(info_station, Station_Name %in% colnames(data_real)[i])
        star <- star_date$Star_Data
        end <- star_date$End_Data
        station[[i]]  <- subset(station[[i]], Date >= star & Date <= end )
        
    }    
    
    names(station) <- colnames(data_real)
    
    return (station)
}

#paste_columns paste three columns  three columns
paste_columns <- function(column_date, colum_real, colum_estima)
{
    table <- data.frame(column_date, colum_real, colum_estima)
    names(table) <- c("Date","Real_Value","Estimated_Value")
    
    return(table)
    
}

