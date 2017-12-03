#vamos hacer una parabola
#range
horasdias <- seq(from =0, to =23.999, by = 10/60)
#Vertices de la parabola. Vamos a leer 10 dias.

#Estacion TresCarlosElGirasol
TresCarlosElGirasol <- list.files()[184]

#Leer el Archivo
Leer_formato <- convert_units(TresCarlosElGirasol, date_format="%Y-%m-%d")

#Escoger los dias
dias_division <- divide_by_day (Leer_formato)

graficas_dias <- function(datosdias)
{
    
    #26.9134187  -109.4954788
    
    dias <- unique((datosdias$Date))
    hora_amanecer <- hour_solarnoon_sunrise(day = dias, lat = 26.9134187, long =-109.4954788, timezo=OlsonNames()[126], "sunrise") 
    hora_mediodia <-  hour_solarnoon_sunrise(day = dias, lat = 26.9134187, long =-109.4954788, timezo=OlsonNames()[126], "solarnoon") 
    
    hora_amanecer <- hour_to_number(hora_amanecer)
    hora_mediodia <- hour_to_number(hora_mediodia)
    
    valoresparabola <- parabola_per_day(vertex_h = hora_mediodia, vertex_k= 1600, pointsunshine_x = hora_amanecer, pointsunshine_y = 0, horasdias)
    
    
    paraboladia <- data.frame(Horas=horasdias, RadiacionSolar=valoresparabola)
    valoresdiario <- data.frame(Horas=unlist(datosdias$HourDecimal), RadiacionSolar=datosdias$Value)
    
    grafica <- rbind(paraboladia, valoresdiario)
    grafica$Datos <- c(rep("Parabola_Por_Dia", nrow(paraboladia)), rep("Datos_Reales", nrow(valoresdiario)))
    
    ggplot(data=grafica, aes(x=Horas, y=RadiacionSolar, col=Datos)) + geom_point() +ggtitle(paste0("Tres Carlos El Girasol","\n",dias)) + theme(plot.title = element_text(hjust = 0.5))
    
}

data <- Check_Day_Station (TresCarlosElGirasol, 0.8)

#"D:/OneDrive - CGIAR/GitHub/Nuevo Codigo Clima/Originales_after_HourlyControl"
index <- seq(from=3, to= 184, by = 5)
lista_archivos_SR <- list.files(pattern = '*.txt')[index]

dimensiones <- lapply(lista_archivos_SR, Check_Day_Station, percentage=0.8)

informacion_estacion <- function(archivo, percentage)
{
    nombre_estacion <- split_name(archivo)[1]
    variable <- split_name(archivo)[2]
    
    Leer_formato <- convert_units(archivo, date_format="%Y-%m-%d")
    dias <- sort(Leer_formato$Date)
    
    dia_inicio <- dias[1]
    dia_final <- dias[length(dias)]
    
    cantidad_dias <- as.Date(as.character(dia_final), format="%Y-%m-%d")-
        as.Date(as.character(dia_inicio), format="%Y-%m-%d")
    
    cantidad_dias <- as.double(cantidad_dias)
    
    dias_aceptables <- Check_Day_Station(archivo, percentage)
    dias_aceptables <- length(dias_aceptables)
    
    resultado <- data.frame(nombre_estacion, variable, dia_inicio, dia_final, cantidad_dias, dias_aceptables, percentage)
    
    return(resultado)
}

lista_archivos <- list.files(pattern = '*.txt')
#Percentage 
Percentage = 0.8
informacion <- lapply(lista_archivos, informacion_estacion, percentage=Percentage)
final_results <- do.call("rbind", informacion)
colnames(final_results) <- c("Station_Name", "Variable_Name", "Star_Date", "End_Data", "Total_Days", "Acceptable_Days","Percentage" )
write.csv(final_results, file = paste0(percentage,"_HourlyControl.csv") )

