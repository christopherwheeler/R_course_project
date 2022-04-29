Untitled
================

# Script purpose and organization

## Prepare the R environment

We will load any relevant packages and set the default for our code
visibility in the final report.

``` r
knitr::opts_chunk$set(echo = TRUE)
library(dataRetrieval)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggplot2)
library(plotly)
```

    ## Warning: package 'plotly' was built under R version 4.1.2

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(cowplot)
```

    ## Warning: package 'cowplot' was built under R version 4.1.2

    ## 
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     stamp

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v tibble  3.1.2     v purrr   0.3.4
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## Warning: package 'tidyr' was built under R version 4.1.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x lubridate::as.difftime() masks base::as.difftime()
    ## x lubridate::date()        masks base::date()
    ## x plotly::filter()         masks dplyr::filter(), stats::filter()
    ## x lubridate::intersect()   masks base::intersect()
    ## x dplyr::lag()             masks stats::lag()
    ## x lubridate::setdiff()     masks base::setdiff()
    ## x cowplot::stamp()         masks lubridate::stamp()
    ## x lubridate::union()       masks base::union()

``` r
library(fs)
```

    ## Warning: package 'fs' was built under R version 4.1.1

``` r
library(Rcpp)
library(sp)
library(raster)
```

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

    ## The following object is masked from 'package:plotly':
    ## 
    ##     select

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(rgdal)
```

    ## rgdal: version: 1.5-23, (SVN revision 1121)
    ## Geospatial Data Abstraction Library extensions to R successfully loaded
    ## Loaded GDAL runtime: GDAL 3.2.1, released 2020/12/29
    ## Path to GDAL shared files: C:/Users/Christopher/Documents/R/R-4.1.0/library/rgdal/gdal
    ## GDAL binary built with GEOS: TRUE 
    ## Loaded PROJ runtime: Rel. 7.2.1, January 1st, 2021, [PJ_VERSION: 721]
    ## Path to PROJ shared files: C:/Users/Christopher/Documents/R/R-4.1.0/library/rgdal/proj
    ## PROJ CDN enabled: FALSE
    ## Linking to sp version:1.4-5
    ## To mute warnings of possible GDAL/OSR exportToProj4() degradation,
    ## use options("rgdal_show_exportToProj4_warnings"="none") before loading rgdal.
    ## Overwritten PROJ_LIB was C:/Users/Christopher/Documents/R/R-4.1.0/library/rgdal/proj

``` r
library(rasterVis)
```

    ## Loading required package: terra

    ## terra version 1.3.4

    ## 
    ## Attaching package: 'terra'

    ## The following object is masked from 'package:rgdal':
    ## 
    ##     project

    ## The following object is masked from 'package:zoo':
    ## 
    ##     time<-

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     src

    ## Loading required package: lattice

    ## Loading required package: latticeExtra

    ## 
    ## Attaching package: 'latticeExtra'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     layer

``` r
library(sf)
```

    ## Linking to GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1

``` r
library(tidyverse)
library(dplyr)
library(EcoHydRology)
```

    ## Loading required package: operators

    ## 
    ## Attaching package: 'operators'

    ## The following object is masked from 'package:sf':
    ## 
    ##     %>%

    ## The following object is masked from 'package:forcats':
    ## 
    ##     %>%

    ## The following object is masked from 'package:stringr':
    ## 
    ##     %>%

    ## The following object is masked from 'package:purrr':
    ## 
    ##     %>%

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     %>%

    ## The following object is masked from 'package:tibble':
    ## 
    ##     %>%

    ## The following object is masked from 'package:plotly':
    ## 
    ##     %>%

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     %>%

    ## The following objects are masked from 'package:base':
    ## 
    ##     options, strrep

    ## Loading required package: topmodel

    ## Loading required package: DEoptim

    ## Loading required package: parallel

    ## 
    ## DEoptim package
    ## Differential Evolution algorithm in R
    ## Authors: D. Ardia, K. Mullen, B. Peterson and J. Ulrich

    ## Loading required package: XML

``` r
library(dataRetrieval)
library(cowplot)
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(EcoHydRology)
library(magrittr)
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:operators':
    ## 
    ##     %>%

    ## The following objects are masked from 'package:terra':
    ## 
    ##     extract, inset

    ## The following object is masked from 'package:raster':
    ## 
    ##     extract

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
library(mapview)
library(tidyr)
library(readxl)
library(janitor)
```

    ## Warning: package 'janitor' was built under R version 4.1.3

    ## 
    ## Attaching package: 'janitor'

    ## The following object is masked from 'package:operators':
    ## 
    ##     %>%

    ## The following object is masked from 'package:terra':
    ## 
    ##     crosstab

    ## The following object is masked from 'package:raster':
    ## 
    ##     crosstab

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

## Bring in and Tidy Konza GW data

Konza map and backgorund here

## 3. Tidy, gap-fill, QAQC data

In this section, we will perform some basic data cleaning operations to
get our data ready for further analysis

``` r
# bring in and examine konza groundwater data set
kgw <- read_csv("kgw.csv", col_types = cols(WLDate = col_character()))
head(kgw)
```

    ## # A tibble: 6 x 36
    ##   DATACODE RECTYPE Location Trans  Plot Geology Recyear WLDate  Elevation SWDate
    ##   <chr>      <dbl> <chr>    <dbl> <dbl> <chr>     <dbl> <chr>   <chr>     <chr> 
    ## 1 AGW01          1 1-1AL        1     1 AL         1990 1990-0~ 364.47    na    
    ## 2 AGW01          1 2-1Mor       2     1 Mor        1990 1990-0~ 365.44    na    
    ## 3 AGW01          1 2-4Mor       2     4 Mor        1990 1990-0~ 365.28    na    
    ## 4 AGW01          1 2-5Mor       2     5 Mor        1990 1990-0~ 365.18    na    
    ## 5 AGW01          1 2-6Mor       2     6 Mor        1990 1990-0~ 365.26    na    
    ## 6 AGW01          1 3-3Eis       3     3 Eis        1990 1990-0~ 371.87    na    
    ## # ... with 26 more variables: Na1 <chr>, Na2 <chr>, K1 <chr>, K2 <chr>,
    ## #   Li <chr>, NH4_N <chr>, Ca1 <chr>, Ca2 <chr>, Mg1 <chr>, Mg2 <chr>,
    ## #   Sr <chr>, Ba <chr>, SO4 <chr>, F <chr>, Cl <chr>, NO3_N <chr>,
    ## #   HPO4_P <chr>, Alkalinity <chr>, pH1 <chr>, DDB <chr>, pH2 <chr>,
    ## #   Temp <chr>, Si1 <chr>, Si2 <chr>, B <chr>, Conduct <chr>

``` r
names(kgw)
```

    ##  [1] "DATACODE"   "RECTYPE"    "Location"   "Trans"      "Plot"      
    ##  [6] "Geology"    "Recyear"    "WLDate"     "Elevation"  "SWDate"    
    ## [11] "Na1"        "Na2"        "K1"         "K2"         "Li"        
    ## [16] "NH4_N"      "Ca1"        "Ca2"        "Mg1"        "Mg2"       
    ## [21] "Sr"         "Ba"         "SO4"        "F"          "Cl"        
    ## [26] "NO3_N"      "HPO4_P"     "Alkalinity" "pH1"        "DDB"       
    ## [31] "pH2"        "Temp"       "Si1"        "Si2"        "B"         
    ## [36] "Conduct"

``` r
summary(kgw)
```

    ##    DATACODE            RECTYPE    Location             Trans     
    ##  Length:7631        Min.   :1   Length:7631        Min.   :1.00  
    ##  Class :character   1st Qu.:1   Class :character   1st Qu.:2.00  
    ##  Mode  :character   Median :1   Mode  :character   Median :3.00  
    ##                     Mean   :1                      Mean   :2.89  
    ##                     3rd Qu.:1                      3rd Qu.:4.00  
    ##                     Max.   :1                      Max.   :4.00  
    ##                                                    NA's   :10    
    ##       Plot         Geology             Recyear        WLDate         
    ##  Min.   :1.000   Length:7631        Min.   :1990   Length:7631       
    ##  1st Qu.:2.000   Class :character   1st Qu.:2000   Class :character  
    ##  Median :4.000   Mode  :character   Median :2008   Mode  :character  
    ##  Mean   :4.018                      Mean   :2007                     
    ##  3rd Qu.:6.000                      3rd Qu.:2014                     
    ##  Max.   :7.000                      Max.   :2019                     
    ##  NA's   :10                                                          
    ##   Elevation            SWDate              Na1                Na2           
    ##  Length:7631        Length:7631        Length:7631        Length:7631       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##       K1                 K2                 Li               NH4_N          
    ##  Length:7631        Length:7631        Length:7631        Length:7631       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##      Ca1                Ca2                Mg1                Mg2           
    ##  Length:7631        Length:7631        Length:7631        Length:7631       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##       Sr                 Ba                SO4                 F            
    ##  Length:7631        Length:7631        Length:7631        Length:7631       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##       Cl               NO3_N              HPO4_P           Alkalinity       
    ##  Length:7631        Length:7631        Length:7631        Length:7631       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##      pH1                DDB                pH2                Temp          
    ##  Length:7631        Length:7631        Length:7631        Length:7631       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##      Si1                Si2                 B               Conduct         
    ##  Length:7631        Length:7631        Length:7631        Length:7631       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ## 

``` r
# from the summary, it appears that all of the actual chem data is in string format, so we need to convert to numeric
kgw[,11:ncol(kgw)]<- lapply(kgw[,11:ncol(kgw)], as.numeric)
```

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

    ## Warning in lapply(kgw[, 11:ncol(kgw)], as.numeric): NAs introduced by coercion

``` r
#now convert date from character to proper format and clean up column names
kgw <- kgw %>% 
  mutate(date = lubridate::ymd(WLDate)) %>% 
  clean_names() %>% 
  dplyr::select(-wl_date)
```

    ## Warning: 3877 failed to parse.

``` r
# now look at data and col types again
head(kgw)
```

    ## # A tibble: 6 x 36
    ##   datacode rectype location trans  plot geology recyear elevation sw_date   na1
    ##   <chr>      <dbl> <chr>    <dbl> <dbl> <chr>     <dbl> <chr>     <chr>   <dbl>
    ## 1 AGW01          1 1-1AL        1     1 AL         1990 364.47    na         NA
    ## 2 AGW01          1 2-1Mor       2     1 Mor        1990 365.44    na         NA
    ## 3 AGW01          1 2-4Mor       2     4 Mor        1990 365.28    na         NA
    ## 4 AGW01          1 2-5Mor       2     5 Mor        1990 365.18    na         NA
    ## 5 AGW01          1 2-6Mor       2     6 Mor        1990 365.26    na         NA
    ## 6 AGW01          1 3-3Eis       3     3 Eis        1990 371.87    na         NA
    ## # ... with 26 more variables: na2 <dbl>, k1 <dbl>, k2 <dbl>, li <dbl>,
    ## #   nh4_n <dbl>, ca1 <dbl>, ca2 <dbl>, mg1 <dbl>, mg2 <dbl>, sr <dbl>,
    ## #   ba <dbl>, so4 <dbl>, f <dbl>, cl <dbl>, no3_n <dbl>, hpo4_p <dbl>,
    ## #   alkalinity <dbl>, p_h1 <dbl>, ddb <dbl>, p_h2 <dbl>, temp <dbl>, si1 <dbl>,
    ## #   si2 <dbl>, b <dbl>, conduct <dbl>, date <date>
