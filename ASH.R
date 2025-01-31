# rm(list=ls())
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tidyverse, readxl, openxlsx, writexl, dplyr)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

# FUNCTION ----------------------------------------------------------------

# Function to calculate sum based on specified activity, aggregation, and targets to summarize
calculate_sum <- function(data, activity, aggregation, targets_summarise){
  # Filter data based on activity if it is not 'none'
  if (activity[1] != 'none') {
    data <- filter(data, grepl(paste(activity, collapse = "|"), get(col.activity)))
  } 
  # Group by specified aggregation columns and summarise the targets
  data <- data %>%
    group_by_at(vars(all_of(c(aggregation))), .drop = F) %>% 
    summarise(across(all_of(c(targets_summarise)), ~ sum(., na.rm = TRUE))) %>% ungroup()
  return(data)
}


calculate_max <- function(data, activity, aggregation, targets_summarise){
  # Filter data based on activity if it is not 'none'
  if (activity[1] != 'none') {
    data <- filter(data, grepl(paste(activity, collapse = "|"), get(col.activity)))
  }
  
  data.agg <- data %>%
    group_by_at(vars(aggregation)) %>%
    summarize(across(all_of(targets_summarise), max)) %>%
    ungroup()
  
  # # Semi-join data with aggregated data to get filtered data
  # filtered_data <- semi_join(data, data.agg, by = c(aggregation))
  return(data.agg)
}

# Arrange data for reached people
arrange_data_target <- function(data, adm.level){
  # arrange data
  data <- data %>% 
    gather(key = "desagregacion", value = "personas",  -adm.level)
  return(data)
}


# calculate percentage for reached people
calculate_percentage_target <- function(data, adm.level){  
  # get total values for each indicator/objective
  data_total<-data %>% 
    filter(desagregacion == col.reached) %>% 
    rename('total_personas' = personas)
  
  # add total alcanzados to data as new column
  data <- data %>% 
    left_join(data_total %>% select(-desagregacion), by = c(adm.level), relationship = "many-to-one")
  
  # calculate percentage 
  data <- data %>% 
    mutate(personas_pct = round((personas / total_personas) * 100, 2))
  
  return(data)
}

# START -------------------------------------------------------------------

# VARIABLES ---------------------------------------------------------------
col.act <- '#activity+name+selected'
col.reach.type <-c('#reached+indigenous',	'#reached+disabled', '#reached+lgbti',	'#victims+trafficking')
col.reach.disagg <- c('#reached','#reached+f+children+age0_17', '#reached+m+children+age0_17',
                      '#reached+f+adult+age18_59','#reached+m+adult+age18_59',
                      '#reached+f+elderly+age60_',	'#reached+m+elderly+age60_')

col.totals <- c('#reached+f+children+age0_17', '#reached+m+children+age0_17',
                                    '#reached+f+adult+age18_59','#reached+m+adult+age18_59',
                                    '#reached+f+elderly+age60_',	'#reached+m+elderly+age60_')

					
col.reached <- '#reached'
col.recurrent <- '#activity+recurrent'
col.validated <- "#validated"
col.sector <- "#sector"

# Administrative Levels
adm0 <- c("#adm0+name", "#adm0+code")
adm1 <- c("#adm1+code", "#adm1+name")
adm12 <- c("#adm1+code", "#adm1+name", "#adm2+code", "#adm2+name")
adm123 <- c("#adm1+code", "#adm1+name", "#adm2+code", "#adm2+name", "#adm3+code", "#adm3+name")
adm4.code <- "#adm4+code"
# Administrative Level List
adm.level <- list(adm1, adm12, adm123)
# location <- '#location'
# updated.date <- '#date+updated'

# ORGANISE DATA -----------------------------------------------------------

clean_data<-df
location <- 'name_check'


# ANALYSE -----------------------------------------------------------------

# REACHED PEOPLE -------------------------------------------------------------
df.check <- df
temp <-calculate_max(data = df.check, activity = 'none', aggregation = c(adm.level[[3]],location), targets_summarise = c(col.reach.type, col.reach.disagg))

# Administrative level 0
# Create the adm0 columns with desired values

total.adm0 <- calculate_sum(data = temp %>% mutate(!!sym(adm0[1]) := 'VE1', !!sym(adm0[2]) := 'Venezuela'),
                            activity = 'none',
                            aggregation = c(adm0), targets_summarise = c(col.reach.type, col.reach.disagg))

total.adm0 <- total.adm0 %>% 
  mutate(!!sym(col.reached) := rowSums(select(., any_of(col.totals)), na.rm = TRUE))

# Administrative level 1

total.adm1 <- calculate_sum(data = temp,
                          activity = 'none',
                          aggregation = c(adm.level[[1]]), targets_summarise = c(col.reach.type, col.reach.disagg))
total.adm1 <- total.adm1 %>% 
  mutate(!!sym(col.reached) := rowSums(select(., any_of(col.totals)), na.rm = TRUE))

# Administrative level 2
total.adm2 <- calculate_sum(data = temp,
                          activity = 'none',
                          aggregation = c(adm.level[[2]]), targets_summarise = c(col.reach.type, col.reach.disagg))

total.adm2 <- total.adm2 %>% 
  mutate(!!sym(col.reached) := rowSums(select(., any_of(col.totals)), na.rm = TRUE))


# REACHED PER ACTIVITY ----------------------------------------------------
df.check <- df
temp <-calculate_max(data = df.check, activity = 'none', aggregation = c(adm.level[[3]], location,col.act), targets_summarise = c(col.reach.type, col.reach.disagg))
temp<- temp %>% mutate(!!sym(adm0[1]) := 'VE1', !!sym(adm0[2]) := 'Venezuela')

n.act.adm0 <- temp %>% group_by_at(vars(all_of(c(adm0[[1]],col.act))), .drop = F) %>% summarise(n_activity=n())%>% mutate(percentage_of_total = n_activity / sum(n_activity) * 100)
n.act.adm1 <- temp %>% group_by_at(vars(all_of(c(adm.level[[1]],col.act))), .drop = F) %>% summarise(n_activity=n())%>% mutate(percentage_of_total = n_activity / sum(n_activity) * 100)
n.act.adm2 <- temp %>% group_by_at(vars(all_of(c(adm.level[[2]],col.act))), .drop = F) %>% summarise(n_activity=n())%>% mutate(percentage_of_total = n_activity / sum(n_activity) * 100)

# Administrative level 0
# Create the adm0 columns with desired values
act.adm0 <- calculate_sum(data = temp,
                            activity = 'none',
                            aggregation = c(col.act,adm0), targets_summarise = c(col.reach.type, col.reach.disagg))
act.adm0 <- act.adm0 %>% 
  mutate(!!sym(col.reached) := rowSums(select(., any_of(col.totals)), na.rm = TRUE))

# Administrative level 1
act.adm1 <- calculate_sum(data = temp,
                            activity = 'none',
                            aggregation = c(col.act,adm.level[[1]]), targets_summarise = c(col.reach.type, col.reach.disagg))
act.adm1 <- act.adm1 %>% 
  mutate(!!sym(col.reached) := rowSums(select(., any_of(col.totals)), na.rm = TRUE))

# Administrative level 2
act.adm2 <- calculate_sum(data = temp,
                            activity = 'none',
                            aggregation = c(col.act,adm.level[[2]]), targets_summarise = c(col.reach.type, col.reach.disagg))

act.adm2 <- act.adm2 %>% 
  mutate(!!sym(col.reached) := rowSums(select(., any_of(col.totals)), na.rm = TRUE))


# percentages -------------------------------------------------------------
pct.adm0 <- calculate_percentage_target(arrange_data_target(total.adm0,adm.level = adm0), adm.level = adm0)
pct.adm1 <- calculate_percentage_target(arrange_data_target(total.adm1,adm.level = adm1), adm.level = adm.level[[1]])
pct.adm2 <- calculate_percentage_target(arrange_data_target(total.adm2,adm.level = adm12), adm.level = adm.level[[2]])


df_list <- list(
  'DATA' = clean_data,
  'total_adm0' = total.adm0,
  'pct_adm0' = pct.adm0,
  'total_adm1' = total.adm1,
  'pct_adm1' = pct.adm1,
  'total_adm2' = total.adm2,
  'pct_adm2' = pct.adm2)

# Write each dataframe to a separate sheet in the Excel workbook
write_xlsx(
  df_list,
  path = paste0('./02_output/',sector,'//',reported_date,'_cluster_wash_345w.xlsx')
)
