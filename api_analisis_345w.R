
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, openxlsx, writexl, jsonlite,dplyr,stringi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

source('utils.R')
options(warn = -1)


# VARIABLE ----------------------------------------------------------------
reported_date<-'2024_05'


# READ FILES --------------------------------------------------------------
folder <- getwd()

### API URL
file1 <- "API.xlsx"
api_url <- read.xlsx(xlsxFile = paste0(folder,'/01_input/',file1))


### HRP TARGET VALUES ###
file2 <- "2024_2025_hrp_metas.xlsx"
file_path <- file.path(folder, '01_input', file2)
# Get sheet names
sheet_names <- excel_sheets(file_path)
# Read each sheet into a dataframe, assign it the corresponding name, and put it into a list
hrp_data <- lapply(sheet_names, function(sheet_name) {
  read.xlsx(xlsxFile = file_path, sheet = sheet_name, startRow = 2)
})
names(hrp_data) <- sheet_names


### HRP ACTIVITY LIST ###
file3 <- '2024_2025_hrp_activity.xlsx'
activity_code <-read.xlsx(xlsxFile = paste0(folder,'/01_input/',file3), sheet ='activity_code', startRow = 1)
activity_unit <-read.xlsx(xlsxFile = paste0(folder,'/01_input/',file3), sheet ='activity_unit', startRow = 2)


### ACTIVITY UNIT VARIABLE FOR DATA CHECKS
unit_people <- tolower(activity_unit %>% filter(unit_people == 'yes') %>% pull(!!sym(col.activity.unit)))
unit_replace_with_1 <- tolower(activity_unit %>% filter(replace_with_1 == 'yes') %>% pull(!!sym(col.activity.unit)))


### DISAGGREGATION AND METADATA OF THE 345W ###
file4 <-'345w_metadata.xlsx'
file_path <- file.path(folder, '01_input', file4)
# Get sheet names
sheet_names <- excel_sheets(file_path)
# Read each sheet into a dataframe, assign it the corresponding name, and put it into a list
disagg_data <- lapply(sheet_names, function(sheet_name) {
  read.xlsx(xlsxFile = file_path, sheet = sheet_name, startRow = 1)
})%>% set_names(sheet_names)


### COLUMN NAMES PLATFORM 345W ###
col_names_345w <- disagg_data[['metadata']]


### CLUSTERS INDICATORS ###
file5 <-'2024_clusters_indicadores.xlsx'
file_path <- file.path(folder, '01_input', file5)
# Get sheet names
sheet_names <- excel_sheets(file_path)
# Read each sheet into a dataframe, assign it the corresponding name, and put it into a list
indicators <- lapply(sheet_names, function(sheet_name) {
  read.xlsx(xlsxFile = file_path, sheet = sheet_name, startRow = 1, fillMergedCells = TRUE, colNames = TRUE)
}) %>% set_names(sheet_names)

# add activity number column
indicators<- lapply(names(indicators), function(i){
  res<-indicators[[i]]%>% 
    mutate(number_activity = sub("([^:]*).*", "\\1", activity))
})%>% set_names(sheet_names)


### DISAGGREGATION LABELS ###
file6 <-"desagregacion_label.xlsx"
file_path <- file.path(folder, '01_input', file6)
# Get sheet names
sheet_names <- excel_sheets(file_path)
# Read each sheet into a dataframe, assign it the corresponding name, and put it into a list
name_labels <- lapply(sheet_names, function(sheet_name) {
  read.xlsx(xlsxFile = file_path, sheet = sheet_name, startRow = 1)
}) %>% set_names(sheet_names)


# ACTIVITIES PER SECTOR ---------------------------------------------------
# Create column with number of the activity (1.01, 2.03...)
activity_code<-activity_code %>%
  mutate(number_activity = sub("([^:]*).*", "\\1", !!sym(col.activity))) 


api_url$sector
# CREATE FOLDERS ----------------------------------------------------------
lapply(1:length(api_url$sector), function(i){
  sector<- api_url$sector[i]
  if (!dir.exists(paste0('./02_output/',sector))) dir.create(paste0('./02_output/',sector), recursive = TRUE)
})

# GET DATA ----------------------------------------------------------------
# READ 345W excel files
# data_sector_345w <- c("ASH", "EDU", "NUT", "PRONNA")
# raw1 <- lapply(data_sector_345w, function(data_sector) {
#   print(paste0(folder, '/01_input/',data_sector))
#   file_path<-file.path(folder,'01_input', paste0(data_sector, '.xlsx'))
# 
#   read.xlsx(xlsxFile = file_path, startRow = 1)
# }) %>% set_names(data_sector_345w)
  

# Get data using API URL
# Loop for each sector API, return a list of dataset per sector
raw <- getDataFromAPI(api_url, activity_code, disagg_data, col_names_345w)

data<-raw
sector_list <- names(raw)


# FILTER DATA -------------------------------------------------------------
# filter to get validated data
data <- lapply(1:length(data), function(i){
  sector<- names(data)[i]
  data <- filter_data_sector_validated(data[[i]])
}) %>% set_names(sector_list)


# CLEAN LOCATION NAME -----------------------------------------------------
# Create supporting column called check_name
# Remove all special caracters, set everything to lower case
# remove symbols, remove spaces from the name of #location

data <- lapply(1:length(data), function(i){
  sector<- names(data)[i]
  data<-location_name_tolower_remove_space_dots(data[[sector]], location)
}) %>% set_names(sector_list)


# CLEAN DATA --------------------------------------------------------------
# Step 1: Clean data by activity and disaggregation group target
# Activities for children, set 0 to adult disaggregation
# Activities for adults, set 0 to children disaggregation
data <- lapply(1:length(data), function(i){
  sector<- names(data)[i]
  res<-clean_age_group_by_activity(data[[sector]], disagg_data[[sector]], sector)
}) %>% set_names(sector_list)


# Step 2: Calculate indicator reached by activity unit
# CHECK 1 #
# check if activity unit is people and #reached is 0
# Replace it with #activity+indicator+reached if it is not 0
# CHECK 2 #
# If the indicator unit is people
# Replace #activity+indicator+unit with #reached value
# CHECK 3 #
# If activity unit is establishment, schools, assessments...(not people, children, teachers)
# Replace #activity+indicator+reached with 1 (if it is not 1 already)
data_cleaned_reached <- lapply(1:length(data), function(i){
  sector<- names(data)[i]
  res<-calculate_reached_by_activity_unit(data[[sector]][['DATA']], sector)
}) %>% set_names(sector_list)


# Step 3: Merge clean_log dataframes
data <- map2(data, data_cleaned_reached, ~ {
  list(DATA = .y$DATA,
       clean_log = bind_rows(.x$clean_log, .y$clean_log))
}) %>% set_names(sector_list)
rm(data_cleaned_reached)


# CALCULATED REACHED ------------------------------------------------------
# sector_scripts <- setNames(paste0(names(data), '.R'), names(data))
# reached <- lapply(names(data), function(sector) {
#   print(sector)
#   
#   df <- data[[sector]][['DATA']] %>% 
#     filter(tolower(iconv(!!sym(col.recurrent), from = "UTF-8", to = "ASCII//TRANSLIT")) != 'si')
#   indicators <- indicators[[sector]]
#   indicators.list <- unique(indicators[c('indicator')])
#   name_labels <- name_labels[[sector]]
#   
#   # Check if the sector exists in sector_scripts and if the file exists
#   if (sector %in% names(sector_scripts) && file.exists(sector_scripts[[sector]])) {
#     source(sector_scripts[[sector]], local = TRUE)
#   } else {
#     if (!sector %in% names(sector_scripts)) {
#       warning(paste("No script file mapping found for", sector))
#     } else {
#       warning(paste("Script file", sector_scripts[[sector]], "not found."))
#     }
#   }
# })
# rm(reached)


# CALCULATE REACHED OVER TARGET BY ACTIVITY -------------------------------
result <- lapply(1:length(data), function(i){
  sector<- names(data)[i]

  # list of sector activities
  act_list <- activity_code %>%
    filter(!!sym(col.sector.code) == sector)

  # base code of sector activity (CLWSH,CLNUT,.. )
  act_code <-unique(act_list %>% pull(!!sym(col.sector.acronym))) %>% na.omit()

  # 345w disaggregations
  all_disaggregation<- c(disagg_data[[sector]][["disaggregation"]]) %>% na.omit()

  # Calculate reached and target
  res<-analysis_345w(data[[sector]][['DATA']], activity_unit, hrp_data[[sector]], act_list, all_disaggregation, act_code,sector)

}) %>% set_names(sector_list)


# EXPORT ------------------------------------------------------------------
# Create list of dataframes
df_list<-lapply(1:length(data), function(i){
  sector<- names(data)[i]
  df_list <- list(
    'DATA' = data[[sector]][['DATA']],
    'RAW' = raw[[sector]],
    'clean_log' = data[[sector]][['clean_log']],
    'adm0' = result[[sector]][['adm0']],
    'adm1' = result[[sector]][['adm1']],
    'adm2' = result[[sector]][['adm2']]
  )

})%>% set_names(sector_list)


hs <- createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
  fontName = "Arial Narrow", fgFill = "#2a2a2a"
)

res<-lapply(seq_along(names(data)), function(i) {
  sector<- names(data)[i]
  write.xlsx(df_list[[i]],
             file = paste0('./02_output/', reported_date, '_', sector, '_analisis_actividad_345w.xlsx'),
             colWidths = 14,
             headerStyle = hs)
})

# options(warn = 0)
