#
# Author:   Cristian Nuno
# Date:     March 30, 2021
# Purpose:  Store relevant lab 04 objects in this source file
#

# load necessary packages ----
`%>%` <- magrittr::`%>%`

# load necessary constants ----

# stargazer settings
S_TYPE <- "html"

# inflation rate
INFLATION_RATE <- 1.28855 

# load custom functions ----

# Helper functions for the **pairs()** correlation table 
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  
  test <- cor.test(x,y)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  
  text(0.5, 0.5, txt, cex = 1.5 )
  text(.7, .8, Signif, cex=cex, col=2)
}

panel.smooth <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                          cex = 0.5, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = 19, col = gray(0.7,0.2), bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, lwd=2, ...)
}

# custom plot
jplot <- function( x1, x2, lab1="", lab2="", draw.line=T, ... )
{
  
  plot( x1, x2,
        pch=19, 
        col=gray(0.6, alpha = 0.2), 
        cex=0.5,  
        bty = "n",
        xlab=lab1, 
        ylab=lab2, cex.lab=1.5,
        ... )
  
  if( draw.line==T ){ 
    ok <- is.finite(x1) & is.finite(x2)
    lines( lowess(x2[ok]~x1[ok]), col="red", lwd=3 ) }
  
}

# load necessary data ----
# remember to use the here::here() function
d1 <- readRDS( here::here( "data/rodeo/LTDB-2000.rds" ) )
d2 <- readRDS( here::here( "data/rodeo/LTDB-2010.rds" ) )
md <- readRDS( here::here( "data/rodeo/LTDB-META-DATA.rds" ) )

d1 <- dplyr::select( d1, - year )
d2 <- dplyr::select( d2, - year )

d <- merge( d1, d2, by="tractid" )
d <- merge( d, md, by="tractid" )

# filter rural districts
d <- dplyr::filter( d, urban == "urban" )

d <- dplyr::select( d, tractid, 
             mhmval00, mhmval12, 
             hinc00, 
             hu00, vac00, own00, rent00, h30old00,
             empclf00, clf00, unemp00, prof00,  
             dpov00, npov00,
             ag25up00, hs00, col00, 
             pop00.x, nhwht00, nhblk00, hisp00, asian00,
             cbsa, cbsaname )


d <- 
  d %>%
  dplyr::mutate( p.white = 100 * nhwht00 / pop00.x,
          p.black = 100 * nhblk00 / pop00.x,
          p.hisp = 100 * hisp00 / pop00.x, 
          p.asian = 100 * asian00 / pop00.x,
          p.hs = 100 * (hs00+col00) / ag25up00,
          p.col = 100 * col00 / ag25up00,
          p.prof = 100 * prof00 / empclf00,
          p.unemp = 100 * unemp00 / clf00,
          p.vacant = 100 * vac00 / hu00,
          mhv.change.00.to.10 = mhmval12 - mhmval00,
          p.mhv.change = 100 * (mhmval12 - mhmval00) / mhmval00,
          pov.rate = 100 * npov00 / dpov00 )


# adjust 2000 home values for inflation 
mhv.00 <- d$mhmval00 * INFLATION_RATE 
mhv.10 <- d$mhmval12

# change in MHV in dollars
mhv.change <- mhv.10 - mhv.00


# drop low 2000 median home values
# to avoid unrealistic growth rates.
#
# tracts with homes that cost less than
# $10,000 are outliers
mhv.00[ mhv.00 < 10000 ] <- NA

# change in MHV in percent
mhv.growth <- 100 * ( mhv.change / mhv.00 )

d$mhv.00 <- mhv.00
d$mhv.10 <- mhv.10
d$mhv.change <- mhv.change
d$mhv.growth <- mhv.growth 

# create mini data frame
df <- data.frame( MedianHomeValue2000=mhv.00, 
                  MedianHomeValue2010=mhv.10, 
                  MHV.Change.00.to.10=mhv.change,
                  MHV.Growth.00.to.12=mhv.growth )

# average growth in median home value for the city
cbsa_stats_df <- 
  d %>%
  dplyr::group_by( cbsaname ) %>%
  dplyr::summarize( metro.mhv.change = median( mhv.change, na.rm=T ),
             metro.mhv.growth = 100 * median( mhv.growth, na.rm=T ) ) %>%
  dplyr::ungroup() 
