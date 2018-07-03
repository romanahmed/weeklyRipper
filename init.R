# Housekeeping ----
rm(list=ls())
gc()
if(length(dev.list()) != 0) dev.off()
cat("\014")

# Initialize base object ----
baseObj <- NULL


# Load libraries ----
# List required pacakges here
pkgList <- c('tidyverse', # tidyverse ecosystem which includes
             'ggplot2', # visualisation
             'dplyr', # data manipulation
             'stringr', # string manipulation
             'forcats', # factor manipulation
             'tibble', # data wrangling
             'tidyr', # data wrangling
             'readr', # input/output
             'purrr', # string manipulation
             
             'scales', # visualisation helper 
             'grid', # visualisation helper 
             'RColorBrewer', # visualisation helper 
             'corrplot', # visualisation helper 
             'ggrepel', # visualisation helper 
             'ggridges', # visualisation helper 
             'ggExtra', # visualisation helper 
             'ggforce', # visualisation helper 
             'viridis', # visualisation helper 
             'wesanderson', # visualisation helper 
             
             'data.table', # data manipulation
             
             'lazyeval', # data wrangling
             'broom', # data wrangling
             
             'lubridate', # date and time
             'timeDate', # date and time
             'tsibble', # tibble for time series
             'sugrrants', # visualization of HD time series
             'tseries', # time series analysis
             'forecast', # frequentist forecasting
             'prophet', # Bayesian forecasting
             'timetk', # time series analysis
             'opera', # combining forecast
             'forecastHybrid', # forecast ensemble
             
             'geosphere', # geospatial locations
             'leaflet', # mapping functionality
             'leaflet.extras', # mapping functionality
             'maps', # mapping functionality
             
             'mice', # better than ice
             'corrr', # yet another correlation
             'gridExtra', # gruid for graphics
             'GGally' # Extension to ggplot2
)

# If pacakges are not installed yet install it first
pkgNew <- pkgList[!(pkgList %in% installed.packages()[,"Package"])]
if(length(pkgNew)) install.packages(pkgNew, dependencies = TRUE)
# Load the packages
lapply(pkgList, require, character.only = TRUE)


baseObj$packages <- pkgList

# Declare directory strcuture ----
baseObj$workDir <- "C:/Users/roman/OneDrive/Documents/Coles/weeklyRipper"
baseObj$dataDir <- stringr::str_c(baseObj$workDir, 
                                  "/data")
baseObj$scriptDir <- stringr::str_c(baseObj$workDir, 
                                    "/scripts")


# Data files to read ----
baseObj$dataFiles <- list.files(baseObj$dataDir)

# Load data read utility function ----
source('~/Coles/weeklyRipper/scripts/tmpUnzip.R')

# Load data ----
air_visits <- tempUnzip(str_c(baseObj$dataDir,
                              "/",
                              'air_visit_data.csv.zip'),
                        fread) %>%
  as.tibble

air_reserve <- tempUnzip(str_c(baseObj$dataDir,
                               "/",
                               'air_reserve.csv.zip'),
                         fread) %>%
  as.tibble

hpg_reserve <- tempUnzip(str_c(baseObj$dataDir,
                               "/",
                               'hpg_reserve.csv.zip'),
                         fread) %>%
  as.tibble

air_store <- tempUnzip(str_c(baseObj$dataDir,
                             "/",
                             'air_store_info.csv.zip'),
                       fread) %>%
  as.tibble


hpg_store <- tempUnzip(str_c(baseObj$dataDir,
                             "/",
                             'hpg_store_info.csv.zip'),
                       fread) %>%
  as.tibble


holidays <- tempUnzip(str_c(baseObj$dataDir,
                            "/",
                            'date_info.csv.zip'),
                      fread) %>%
  as.tibble

store_ids <- tempUnzip(str_c(baseObj$dataDir,
                             "/",
                             'store_id_relation.csv.zip'),
                       fread) %>%
  as.tibble

# Assign appropriate calss to data ----
air_visits <- air_visits %>%
  mutate(visit_date = ymd(visit_date))

air_reserve <- air_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

hpg_reserve <- hpg_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

air_store <- air_store %>%
  mutate(air_genre_name = as.factor(air_genre_name),
         air_area_name = as.factor(air_area_name))

hpg_store <- hpg_store %>%
  mutate(hpg_genre_name = as.factor(hpg_genre_name),
         hpg_area_name = as.factor(hpg_area_name))

holidays <- holidays %>%
  mutate(holiday_flg = as.logical(holiday_flg),
         date = ymd(calendar_date))


# Load plot utility function ----
source('~/Coles/weeklyRipper/scripts/utilityFunc.R')
