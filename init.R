# Housekeeping ----
rm(list=ls())
gc()
if(length(dev.list()) != 0) dev.off()
cat("\014")

# Start initialization process ----
baseObj <- NULL


### Load libraries ----
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
             
             'scales', # visualisation helper package
             'grid', # visualisation helper package
             'RColorBrewer', # visualisation helper package
             'corrplot', # visualisation helper 
             'ggrepel', # visualisation helper package
             'ggridges', # visualisation helper package
             'ggExtra', # visualisation helper package
             'ggforce', # visualisation helper package
             'viridis', # visualisation helper package
             'wesanderson', # visualisation helper package
             
             'data.table', # data manipulation
             
             'lazyeval', # data wrangling
             'broom', # data wrangling
             
             'lubridate', # date and time
             'timeDate', # date and time
             'tseries', # time series analysis
             'forecast', # time series analysis
             'prophet', # time series analysis
             'timetk', # time series analysis
             
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

# Directory strcuture ----
baseObj$workDir <- "C:/Users/roman/OneDrive/Documents/Coles/weeklyRipper"
baseObj$dataDir <- stringr::str_c(baseObj$workDir, 
                                  "/data")
baseObj$scriptDir <- stringr::str_c(baseObj$workDir, 
                                    "/scripts")


# Load data ----

air_visits <- fread(unzip(str_c(baseObj$dataDir,
                                "/",
                                'air_visit_data.csv.zip'))) %>%
  as.tibble()

air_reserve <- fread(unzip(str_c(baseObj$dataDir,
                                 "/",
                                 'air_reserve.csv.zip'))) %>%
  as.tibble()

hpg_reserve <- fread(unzip(str_c(baseObj$dataDir,
                                 "/",
                                 'hpg_reserve.csv.zip'))) %>%
  as.tibble()


air_store <- fread(unzip(str_c(baseObj$dataDir,
                               "/",
                               'air_store_info.csv.zip'))) %>%
  as.tibble()

hpg_store <- fread(unzip(str_c(baseObj$dataDir,
                               "/",
                               'hpg_store_info.csv.zip'))) %>%
  as.tibble()

holidays <- fread(unzip(str_c(baseObj$dataDir,
                              "/",
                              'date_info.csv.zip'))) %>%
  as.tibble()

store_ids <- fread(unzip(str_c(baseObj$dataDir,
                               "/",
                               'store_id_relation.csv.zip'))) %>%
  as.tibble()
