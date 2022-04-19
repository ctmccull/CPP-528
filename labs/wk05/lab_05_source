#
# Author:   Cristian Nuno
# Date:     April 6, 2021
# Purpose:  Store relevant lab 05 objects in this source file
#

# load necessary packages ----
`%>%` <- magrittr::`%>%`

# load necessary constants ----

# stargazer settings
S_TYPE <- "html"

# inflation rate
INFLATION_RATE <- 1.28855 

# load necessary data ----

# remember to use here::here() when importing data
d1 <- readRDS( here::here( "data/rodeo/LTDB-2000.rds" ) )
d2 <- readRDS( here::here( "data/rodeo/LTDB-2010.rds" ) )
md <- readRDS( here::here( "data/rodeo/LTDB-META-DATA.rds" ) )

d1 <- dplyr::select( d1, - year )
d2 <- dplyr::select( d2, - year )

d <- merge( d1, d2, by="tractid" )
d <- merge( d, md, by="tractid" )

# keep only the urban tracts
d <- dplyr::filter( d, urban == "urban" )

# obtain NMTC data
NMTC_URL <- "https://raw.githubusercontent.com/DS4PS/cpp-528-spr-2020/master/labs/data/raw/NMTC/nmtc-sheet-01.csv"
nmtc <- read.csv( NMTC_URL, stringsAsFactors=F )

# obtain LIHTC data
LIHTC_URL <- "https://raw.githubusercontent.com/DS4PS/cpp-528-spr-2020/master/labs/data/raw/LIHTC/LIHTCPUB.csv"
lihtc <- read.csv( LIHTC_URL, stringsAsFactors=F )

# create a key that will allow us to obtain federal data for each tract ----
# remove anything not a number from the string
d$id2 <- gsub( "[^0-9]", "", d$tractid )

# fix IDs so they are match
d$id2 <- as.numeric( d$id2 )

# aggregate federal programs such that there is one record per tract ----
lihtc.dollars <-
  lihtc %>% 
  dplyr::filter( yr_alloc >= 2000 & yr_alloc <= 2010 ) %>%
  dplyr::group_by( fips2010 ) %>%
  dplyr::summarize( num.lihtc = dplyr::n(), lihtc.total = sum( allocamt, na.rm=T ) )

# need to convert from currency to numeric
# current format: 
# head( nmtc$QLICI.Amount )
# [1] "$300,000.00 "   "$1,008,750.00 " "$977,000.00 "

# remove dollar sign and commas
nmtc$amount <- gsub( "[,$]", "", nmtc$QLICI.Amount )

# head(  nmtc$amount  )
# "300000.00 "  "1008750.00 " "977000.00 "

# convert characters to numeric 
nmtc$amount <- as.numeric( nmtc$amount ) %>% round(0)

# head(  nmtc$amount  )
# [1]  300000 1008750  977000

nmtc.dollars <- 
  nmtc %>% 
  dplyr::filter( Origination.Year >= 2000 & Origination.Year <= 2010 ) %>%
  dplyr::group_by( X2010.Census.Tract ) %>% 
  dplyr::summarize( num.nmtc = dplyr::n(), nmtc.total = sum( amount, na.rm=T ) )

# merge federal data onto census tracts ----
d <- merge( d, nmtc.dollars, by.x="id2", by.y="X2010.Census.Tract", all.x=T )
d <- merge( d, lihtc.dollars, by.x="id2", by.y="fips2010", all.x=T )

# recode tracts that had no grants from NA to 0 ---
d$num.nmtc[ is.na(d$num.nmtc) ] <- 0
d$nmtc.total[ is.na(d$nmtc.total) ] <- 0

d$num.lihtc[ is.na(d$num.lihtc) ] <- 0 
d$lihtc.total[ is.na(d$lihtc.total) ] <- 0


# adjust 2000 home values for inflation 
mhv.00 <- d$mhmval00 * INFLATION_RATE  
mhv.10 <- d$mhmval12

# change in MHV in dollars
mhv.change <- mhv.10 - mhv.00


# drop low year 2000 median home values
# to avoid unrealistic growth rates.
#
# tracts with homes that cost less than
# $10,000 are outliers
# approximately 200 out of 59,000 cases 
sum( mhv.00 < 10000 ) 
mhv.00[ mhv.00 < 10000 ] <- NA

# change in MHV in percent
mhv.growth <- 100 * ( mhv.change / mhv.00 )

# store mini data frame to be used for descriptive statistics ----
df <- data.frame( MedianHomeValue2000=mhv.00, 
                  MedianHomeValue2010=mhv.10, 
                  MHV.Change.00.to.10=mhv.change,
                  MHV.Growth.00.to.12=mhv.growth )

# add variables to the main data frame ----
d$mhv.00 <- mhv.00
d$mhv.10 <- mhv.10
d$mhv.change <- mhv.change
d$mhv.growth <- mhv.growth 

# select a few variables ----
d <- dplyr::select( d, 
             
             tractid, cbsa, cbsaname,            # ids / units of analysis
             
             mhv.00, mhv.10, mhv.change, mhv.growth,    # home value 
             
             hinc00, hu00, own00, rent00,        # ses
             hinc12, hu10, own10, rent10,
             
             empclf00, clf00, unemp00, prof00,   # employment 
             empclf12, clf12, unemp12, prof12,
             
             dpov00, npov00,                     # poverty
             dpov12, npov12,
             
             ag25up00, hs00, col00,              # education 
             ag25up12, hs12, col12,
             
             pop00.x, nhwht00, nhblk00, hisp00, asian00,   # race
             pop10, nhwht10, nhblk10, hisp10, asian10,
             
             num.nmtc, nmtc.total,              # tax policy data
             num.lihtc, lihtc.total             # aggregated by census tract
             
) # end select


# create new variables ----
d <- 
  d %>%
  dplyr::mutate( 
    # 2000 variables
    p.white.00 = 100 * nhwht00 / pop00.x,
    p.black.00 = 100 * nhblk00 / pop00.x,
    p.hisp.00 = 100 * hisp00 / pop00.x, 
    p.asian.00 = 100 * asian00 / pop00.x,
    p.hs.edu.00 = 100 * (hs00+col00) / ag25up00,
    p.col.edu.00 = 100 * col00 / ag25up00,
    p.prof.00 = 100 * prof00 / empclf00,
    p.unemp.00 = 100 * unemp00 / clf00,
    pov.rate.00 = 100 * npov00 / dpov00,
    
    # 2010 variables
    p.white.10 = 100 * nhwht10 / pop10,
    p.black.10 = 100 * nhblk10 / pop10,
    p.hisp.10 = 100 * hisp10 / pop10, 
    p.asian.10 = 100 * asian10 / pop10,
    p.hs.edu.10 = 100 * (hs12+col12) / ag25up12,
    p.col.edu.10 = 100 * col12 / ag25up12,
    p.prof.10 = 100 * prof12 / empclf12,
    p.unemp.10 = 100 * unemp12 / clf12,
    pov.rate.10 = 100 * npov12 / dpov12 ) %>%
  # remove any NA or Inf values
  na.omit(use = "everything")

# inflation adjust income  ----
d$hinc00 <- INFLATION_RATE * d$hinc00

# create new variables by cbsa ----
d <-
  d %>%
  dplyr::group_by( cbsaname ) %>%
  dplyr::mutate( # metro rank of home value in 2000
    metro.mhv.pct.00 = dplyr::ntile( mhv.00, 100 ),
    # metro rank of home value in 2010
    metro.mhv.pct.10 = dplyr::ntile( mhv.10, 100 ),
    # median pay for metro area 2000
    metro.median.pay.00 = median( hinc00, na.rm=T ),
    # median pay for metro area 2010
    metro.median.pay.10 = median( hinc12, na.rm=T ),
    # tract rank in metro area for diversity (% non-white)
    metro.diversity.rank.00 = dplyr::ntile( (100-p.white.00), 100 ),
    # metro total population 2000
    metro.pop.total.00 = sum( pop00.x, na.rm=T ),
    # metro total population 2010
    metro.pop.total.10 = sum( pop10, na.rm=T ) ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate( # change in relative rank of mhv in metro area 2000-2010
    metro.mhv.pctile.change = metro.mhv.pct.10 - metro.mhv.pct.00,
    # growth in ave pay in metro
    metro.pay.change = metro.median.pay.10 - metro.median.pay.00,
    # metro population growth 2000-2010
    metro.pop.growth = ( metro.pop.total.10 - metro.pop.total.00 ) / metro.pop.total.00,
    # increase in the proportion of whites in tract 
    increase.p.white = p.white.10 - p.white.00  )

# Create a true/false code for recipient tracts ----
d$LIHTC <- ifelse( d$num.lihtc > 0, "YES", "NO" )
d$NMTC <- ifelse( d$num.nmtc > 0, "YES", "NO" )

# create a growth column within the data frame ----
d$growth <- d$mhv.growth
d$growth[ d$growth > 200 ] <- NA

# store plots in a list for easy access ----
PLOTS <-
  list(
    "pov_rate_2000" = list(
      "nmtc" = ggplot2::ggplot( d, ggplot2::aes(x=pov.rate.00, fill=NMTC )) +
        ggplot2::geom_density(alpha=0.4) + 
        ggplot2::ggtitle("2000 Poverty Rate Comparison of \nRecipient and Non-Recipient Communities"),
      "lihtc" = ggplot2::ggplot( d, ggplot2::aes(x=pov.rate.00, fill=LIHTC)) +
        ggplot2::geom_density(alpha=0.4) +
        ggplot2::ggtitle("2000 Poverty Rate Comparison of \nRecipient and Non-Recipient Communities")
    ),
    "mhv_2000" = list(
      "nmtc" = ggplot2::ggplot( d, ggplot2::aes(x=log10(mhv.00), fill=NMTC )) +
        ggplot2::geom_density( alpha=0.4 ) +
        ggplot2::ggtitle("2000 Median Home Value Comparison of \nRecipient and Non-Recipient Communities"),
      "lihtc" = ggplot2::ggplot( d, ggplot2::aes(x=log10(mhv.00), fill=LIHTC )) +
        ggplot2::geom_density( alpha=0.4 )  +
        ggplot2::ggtitle("2000 Median Home Value Comparison of \nRecipient and Non-Recipient Communities")
    ),
    "mhv_growth" = list(
      "nmtc" = ggplot2::ggplot( d, ggplot2::aes(x=growth, fill=NMTC )) +
        ggplot2::geom_density( alpha=0.4 )  +
        ggplot2::ggtitle("Comparision of MHV Growth 2000 to 2010: \nRecipients vs Non-Recipients"),
      "lihtc" = ggplot2::ggplot( d, ggplot2::aes(x=growth, fill=LIHTC )) +
        ggplot2::geom_density( alpha=0.4 )  +
        ggplot2::ggtitle("Comparision of MHV Growth 2000 to 2010: \nRecipients vs Non-Recipients")
    )
  )

# create the difference in difference dataset ----

# log the two variables
y1 <- log( d$mhv.00 )
y2 <- log( d$mhv.10 )

# create a variable that identifies if a tract received NMTC funding
treat <- as.numeric( d$num.nmtc > 0 )

# store the year 2000 data
d1 <- data.frame( y=y1, treat=treat, post=0 )
# store the year 2010 data
d2 <- data.frame( y=y2, treat=treat, post=1 )

# stack the two time periods together
d3 <- rbind( d1, d2 )
