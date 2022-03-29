





filter_cat <- function( data , search )
  
{
  num <- grep(search, data$category, ignore.case = T) 
  dat.cat <- data.frame(data[num,])
  return( dat.cat )
}



