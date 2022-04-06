

lab03_ar




main
# function 1
filter_cat <- function( data , search )
  
{
  num <- grep(search, data$category, ignore.case = T) 
 lab03_ar
  dat.cat <- data.frame(data[num,])

  
  dat.cat <- data.frame(data[num,])getwd
main
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

lab03_ar
# Function 3 filter by year
=======
# Function 3 filter by year, revised after Lab02
main

filter_year <- function( year )
{
  
 lab03_ar
  dd.year <- dd %>% dplyr::select( contains( year ))
=======
  dd.year <- dd %>% dplyr::select( contains( year ), ) %>%
    filter( any( !is.na( c_across( everything() ) ) ) )
main
  
  return( dd.year )
  
}
 lab03_ar
=======

main
