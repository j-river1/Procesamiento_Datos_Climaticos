#Read files of HR and SR
#Arguments listfiles is a list of files.
read_files <- function (listfiles)
{
    
    files_Incomple <- lapply(listfiles, function(x)  { file = read.table(x, header= TRUE)                                       
                                                                            return (file)                                                                                                           
                                                                                       })
    
    
    files_SR <-lapply(list.files(path= "./SR"), function(x)  { file = read.table(x, header= TRUE)
                                                                   names(file) = c("Date", "SR" )                                       
                                                                                    return (file)                                                                                                           
                                                                                               })
    
    files_HR <-lapply(list.files(path= "./RH"), function(x) {file = read.table(x, header= TRUE)    
                                                                 names(file) = c("Date", "RH" )
                                                                                   return (file)
                                                                                              })
    
    return (files_Incomple)
    
}
