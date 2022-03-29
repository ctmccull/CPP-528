




# function 1
filter_cat <- function( data , search )
  
{
  num <- grep(search, data$category, ignore.case = T) 
  dat.cat <- data.frame(data[num,])
  return( dat.cat )
}

# import data dictionary
dd <- LTDB_DATA_DICTIONARY

# function2
search_var <- function(string)
{
vector <- dd$definition
these <- grepl( string, vector, ignore.case=T )
dat.sub <- dd[ these, ]
return( dat.sub )
}

