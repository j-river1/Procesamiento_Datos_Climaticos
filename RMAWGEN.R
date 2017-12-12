
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
        generator_values <- lapply(station_info, applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=2)
        
    }    
    
    if(variable=='PRECIPITATION')
    {

        generator_values <- lapply(station_info, applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=3)
         
         
    }
       
    
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
        #Error distribution. Check if precipitation distribuition is biased to zero.  
        median <- lapply(PRECIPITATION[station], median, na.rm =T)
        test <- lapply(mediana, function(x) { if (x < 1) {result <- TRUE} else {result <- FALSE}})
        
        if(any (test == TRUE))
        {
            warning("There is a problem with distribution of Precipitation. This is very biased to zero")
            PRECIPITATION[station] <- PRECIPITATION[station] + 1
            generation_prec <- ComprehensivePrecipitationGenerator(
                station=station,
                prec_all=PRECIPITATION,
                year_min=year_min,
                year_max=year_max,
                p= 3,
                n_GPCA_iteration= 10,
                n_GPCA_iteration_residuals= 0,
                sample = "monthly",
                no_spline = FALSE,
                nscenario = 20)
            
            real_data <- generation_prec$prec_mes - 1
            fill_data <- generation_prec$prec_gen - 1
            
        }
        
        else 
        {
            generation_prec <- ComprehensivePrecipitationGenerator(
                station=station,
                prec_all=PRECIPITATION,
                year_min=year_min,
                year_max=year_max,
                p= 3,
                n_GPCA_iteration= 10,
                n_GPCA_iteration_residuals= 0,
                sample = "monthly",
                no_spline = FALSE,
                nscenario = 20)
            
            real_data <- generation_prec$prec_mes
            fill_data <- generation_prec$prec_gen 
            
        }
        


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
    grafica <- real_dat
    grafica$Datos <- c(rep("Datos_Reales", nrow(real_dat)))
    grafica$Datos[NAs] <- c("Datos_Estimados")
    
    #Graph
    graph <- ggplot(data=grafica, aes(x=Dates, y=Value, col=Datos)) + geom_point() +ggtitle(paste0(name,"\n",variable)) + theme(plot.title = element_text(hjust = 0.5)) + ylab(y) + xlab("Dias")
    name_grap <- paste0(paste(name,variable, sep="_"),".pdf")
    nameFile <-  paste0("..", "/", "Graphics", "/", name_grap)
    ggsave(nameFile, plot=graph)

    
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
    

    name_file <- paste0(paste(name,namefile, sep="_"),".txt")
    weather_data <- paste0(".", "/", "Files_By_Station", "/", name_file )   
    
    

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
    info_station$Star_Data <- as.Date(info_station$Star_Data, format = "%Y-%m-%d")
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

