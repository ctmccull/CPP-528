




# function 1
filter_cat <- function( data , search )
  
{
  num <- grep(search, data$category, ignore.case = T) 
  dat.cat <- data.frame(data[num,])getwd
  return( dat.cat )
}

# import data dictionary
dd <- LTDB_DATA_DICTIONARY

# function 2
search_var <- function(string)
{
vector <- dd$definition
these <- grepl( string, vector, ignore.case=T )
dat.sub <- dd[ these, ]
return( dat.sub )
}

# Function 3 filter by year, revised after Lab02

filter_year <- function( year )
{
  
  dd.year <- dd %>% dplyr::select( contains( year ), ) %>%
    filter( any( !is.na( c_across( everything() ) ) ) )
  
  return( dd.year )
  
}

