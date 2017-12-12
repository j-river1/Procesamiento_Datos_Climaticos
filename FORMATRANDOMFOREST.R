#format for random forest 

#match_files merge the files according their names
#Arguments      -listFiles list of files

subDir <- "Random_Forest"
subDir <- "File_Variables"

dir.create(file.path(getwd(), subDir), showWarnings = FALSE)

match_files  <- function(listFiles, resumefile)
{

  
  filenames <- read.csv(resumefile, header=T)
  names_stations <- unique(filenames$Station_Name)
  
  
  
  
  #Grouping according names station
  files  <- lapply(names_stations, merge_files, listfiles = list.files())

  return (files)
}

#merge_files merges files according to names
merge_files <- function (name, listfiles)
{
  name <- as.character(name)
  
  files <-  list.files(pattern= name)
  read_files <- lapply(files,label_for_files)
  tablaPerday <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Date", all = TRUE),read_files)
  
  namec <- paste0(name,".txt" )
  write.table(tablaPerday, file = paste(getwd(),"Random_Forest", namec, sep =  "/"), row.names = FALSE, quote = FALSE, col.names = TRUE)
  return(tablaPerday)
}

#label_for_files puts label a files.

label_for_files <- function (name)
{
  file <- read.table(name, header=T)
  variable <-  split_name(name)[2]
  names(file) <- c("Date", variable)
    
  return(file) 
} 

#Read

leer_label <- function(name)
{
  file <- read.table(name, header=T)
  variable <-  split_name(name)[2]
  names(file) <- c("Date", variable)
  
  return(file) 
  
}
  
#move files SR and HR from Daily Hour to Files_by_station

move_files_SR_HR <- function ()
{
    list.files("../../AfterDailyControl_Data", pattern ="_SR_")
    paste0("")
    list.files("../../AfterDailyControl_Data", pattern ="_RH_")
    
    
}

paste_name <_ function()





w <- lapply(files, function(x){read.table(x, header=TRUE)})

lapply(name_estaciones, merge_files, listfiles =list.files() )


