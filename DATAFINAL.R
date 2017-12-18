#Read files of HR and SR
#Arguments listfiles is a list of files with pattern txt.
read_files <- function (listfiles)    
{
    
    
    
    files_merge <- lapply(listfiles,merge_tables)
    names_filemerge <- lapply(listfiles, function(x) {names = split_name(x) 
                                                         return (names)})
    names(files_merge) <- names_filemerge
    variable_names <- names(files_merge)

#     save_files <- lapply(files_merge, function(x) { name <- names(files_merge)
#                                                     
#                                                     write.table(files_merge, file = paste0("./Original_Data/", name, ".txt" ))})
        
    save_files <- lapply(seq_along(files_merge), function(y,n,i){
                                                                name <- n[[i]]
                                                                write.table(y[[i]], file = paste0("../Final_Data/", name, ".txt" ))
                                                                }, y=files_merge, n=names(files_merge))
    
#     #Read files with missing values
#     names_Incomple <- lapply(listfiles, function(x) {names = split_name(x) 
#                                                      return (names)})
#     files_Incomple <- lapply(listfiles, function(x)  { file = read.table(x, header= TRUE)  
#                                                        return (file)})
#     
#     names(files_Incomple) <- names_Incomple
#     files_Incomple <- files_Incomple[order(names(files_Incomple))]
#     
#     
#     
#     
#     
#     
#     #Read files with SR value
#     names_SRfiles <- lapply(list.files(path= "./SR"), function(x) {names = split_name(x)[1]
#                                                                    return (names)})
#     files_SR <-lapply(list.files(path= "./SR"), function(x)  { file = read.table(paste0("./SR/",x), header= TRUE)
#                                                                names(file) = c("Date", "SR" ) 
#                                                                return (file) })
#     names(files_SR) <- names_SRfiles
#     
#     files_SR <- files_SR[order(names(files_SR))]
#     
#     
#     
#     
#     #Read files with RH value
#     names_RHfiles <- lapply(list.files(path= "./RH"), function(x) {names = split_name(x)[1]
#                                                                    return (names)})
#             
# #     files_RH <-lapply(list.files(path= "./RH"), function(x) {file = read.table(paste0("./RH/",x), header= TRUE)    
# #                                                                  names(file) = c("Date", "RH" )
# #                                                                                    return (file)
# #                                                                                               })
#     
#      files_RH <-lapply(list.files(path= "./RH"), function(x) {file = read.table(paste0("./RH/",x), header= TRUE)    
#                                                                   names(file) = c("Date", "RH" )
#                                                                                     return (file)
#                                                                                                })
# 
# 
# 
# 
#     names(files_RH) <- names_RHfiles
#     
#     files_RH <- files_RH[order(names(files_RH))]
#     
#     
#     
#     #merge tables Incomple with SR
# 
#     
#      w <- mapply(merge_tables,files_Incomple, files_SR = files_SR)
    return ( files_merge)




    
}


#merge_tables 
#listfiles with partern

merge_tables <- function(namefile)
{
    #incomplete name
    file <- read.table(namefile, header= TRUE) 
    file$SR <- NULL
    file$RH <- NULL
    
    
    
    #File SR
    filename <- split_name(namefile)
    files_SR <- read.table(paste0("./SR/",filename, "_SR.txt"), header= TRUE)
    names(files_SR) = c("Date", "SR" ) 
    files_SR$Date <- NULL
    
    #File RH
    filename <- split_name(namefile)
    files_RH <- read.table(paste0("./RH/",filename, "_RH.txt"), header= TRUE)
    names(files_RH) = c("Date", "RH" ) 
    files_RH$Date <- NULL
   
    
    
    file_total <- cbind(file, files_SR, files_RH)                                                         
    
    return(file_total)
    
    
}