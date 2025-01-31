# CLUSTER EDUCATION 345W ANALYSIS - VENEZUELA 2024
# CALCULATION OF INDICATIORS AND REACHED PEOPLE

# rm(list=ls())
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tidyverse, readxl, openxlsx, writexl, jsonlite, dplyr)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()


# SOURCES -----------------------------------------------------------------
# Functions and Variables
source("utils_edu.R")

# START -------------------------------------------------------------------
clean_data<-df
location <- 'name_check'

# ANALYSE -----------------------------------------------------------------

# INDICATOR 1.1 -------------------------------------------------------------
# 1.1: # de niñas, niños y adolescentes de 3 a 17 años dentro y fuera de la escuela que participan de programas de educación alternativa
# 1.01+1.02+1.03+1.05

name.act <- unlist((indicators %>% filter(indicator == indicators.list[1, "indicator"]) %>% select(number_activity)))
name.ind <- unique(unlist((indicators %>% filter(indicator == indicators.list[1, "indicator"]) %>% select(indicator))))

# Calculate the sum by adm (in schools or others)
ind.11 <- calculate_sum(data = df,
                        activity = name.act,
                        aggregation = c(adm.level[[3]]),
                        targets_summarise = c(col.reach.type, col.reach.disagg))

# add indicator column
ind.11<- ind.11 %>% 
  mutate(indicator = unique(name.ind),.before = 1)

# # replace with 0 ages not in the range f the activity
# ind.11t <- ind.11 %>%
#   mutate_at(vars(all_of(c(age.adult,age.0.2))),~0)


# INDICATOR 1.2 -------------------------------------------------------------
# Por escuela: Max(1.06,1.07)

name.act <- unlist((indicators %>% filter(indicator == indicators.list[2, "indicator"]) %>% select(number_activity)))
name.ind <- unique(unlist((indicators %>% filter(indicator == indicators.list[2, "indicator"]) %>% select(indicator))))

# get max value for each disaggregation group at location level
temp <-calculate_max(data = df, activity = name.act, aggregation = c(adm.level[[3]],col.location.type,location), targets_summarise = c(col.reach.type, col.reach.disagg))
# temp <- distinct(temp, across(all_of(c(adm.level,location))), .keep_all = TRUE)

# Calculate aggregation at adm level
ind.12 <- calculate_sum(data = temp,
                        activity = 'none',
                        aggregation = c( adm.level[[3]]),
                        targets_summarise = c(col.reach.type, col.reach.disagg))
# add indicator column
ind.12 <-ind.12  %>% 
  mutate(indicator = unique(name.ind),.before = 1)


# INDICATOR 1.3 -----------------------------------------------------------
# Por escuela: Max(1.08, 1.09, 1.10)

name.act <- unlist((indicators %>% filter(indicator == indicators.list[3, "indicator"]) %>% select(number_activity)))
name.ind <- unique(unlist((indicators %>% filter(indicator == indicators.list[3, "indicator"]) %>% select(indicator))))

# Calculate the max value by location (in school or others)
temp <-calculate_max(data = df, activity = name.act, aggregation = c(adm.level[[3]],col.location.type,location), targets_summarise = c(col.reach.type, col.reach.disagg))

# Calculate aggregation at adm level
ind.13 <- calculate_sum(data = temp,
                        activity = 'none',
                        aggregation = c(adm.level[[3]]),
                        targets_summarise = c(col.reach.type, col.reach.disagg))
# add indicator column
ind.13 <-ind.13 %>% 
  mutate(indicator = unique(name.ind),.before = 1)


# INDICATOR 2.1 -----------------------------------------------------------
# 2.1: # de docentes y otro personal educativo que participan de programas de formación docente continua
# Por escuela:    Max(2.01 a 2.08,2.10,2.13,2.14)

name.act <- unlist((indicators %>% filter(indicator == indicators.list[4, "indicator"]) %>% select(number_activity)))
name.ind <- unique(unlist((indicators %>% filter(indicator == indicators.list[4, "indicator"]) %>% select(indicator))))

# Calculate the max value by location (in school or others)
temp <-calculate_max(data = df, activity = name.act, aggregation = c(adm.level[[3]],col.location.type,location), targets_summarise = c(col.reach.type, col.reach.disagg))

# Calculate aggregation at adm level
ind.21 <- calculate_sum(data = temp,
                        activity = 'none',
                        aggregation = c(adm.level[[3]]),
                        targets_summarise = c(col.reach.type, col.reach.disagg))
# add indicator column
ind.21 <-ind.21 %>% 
  mutate(indicator = unique(name.ind),.before = 1) 


# INDICATOR 2.2 -----------------------------------------------------------
# Max(2.09, 2.10,2.11)+2.12

name.act <- unlist((indicators %>% filter(indicator == indicators.list[5, "indicator"]) %>% select(number_activity)))
name.ind <- unique(unlist((indicators %>% filter(indicator == indicators.list[5, "indicator"]) %>% select(indicator))))

# Aggregation at adm level by specific activity
sum.act<- c('2.12')
temp.sum<- calculate_sum(data = df,
                 activity = sum.act,
                 aggregation = c(col.activity, adm.level[[3]]),
                 targets_summarise = c(col.reach.type, col.reach.disagg))

# Calculate max out of selected indicators by location
max.act <- c('2.09', '2.10', '2.11')
temp.max <-calculate_max(data = df, activity = max.act, aggregation = c(adm.level[[3]],col.location.type,location), targets_summarise = c(col.reach.type, col.reach.disagg))

# Bind datasets
temp <- bind_rows(temp.sum, temp.max)

# Calculate Indicator aggregation by adm level
ind.22 <- calculate_sum(data = temp,
                        activity = 'none',
                        aggregation = c(adm.level[[3]]),
                        targets_summarise = c(col.reach.type, col.reach.disagg))

# add indicator column
ind.22 <-ind.22 %>% 
  mutate(indicator = unique(name.ind),.before = 1)


# INDICATOR 3.1 -----------------------------------------------------------
# Max(3.01,3.04) estudiantes, Max(3.03,3.05) docentes y Max(3.02,3.06) escuelas

name.act <- unlist((indicators %>% filter(indicator == indicators.list[6, "indicator"]) %>% select(number_activity)))
name.ind <- unique(unlist((indicators %>% filter(indicator == indicators.list[6, "indicator"]) %>% select(indicator))))


# Calculate max out of selected indicators
max.act <- c('3.01', '3.04')
temp.max.student <- calculate_max(data = df, activity = max.act, aggregation = c(adm.level[[3]],col.location.type,location), targets_summarise = c(col.reach.type, col.reach.disagg))

max.act <- c('3.03', '3.05')
temp.max.teacher <- calculate_max(data = df, activity = max.act, aggregation = c(adm.level[[3]],col.location.type,location), targets_summarise = c(col.reach.type, col.reach.disagg))

max.act <- c('3.02', '3.06')
temp.max.school <- calculate_max(data = df, activity = max.act, aggregation = c(adm.level[[3]],col.location.type,location), targets_summarise = c(col.reach.type, col.reach.disagg))

# Bind datasets
temp <- bind_rows(temp.max.student, temp.max.teacher, temp.max.school)

# Calculate aggregation by adm level
ind.31 <- calculate_sum(data = temp,
                        activity = 'none',
                        aggregation = c(adm.level[[3]]),
                        targets_summarise = c(col.reach.type, col.reach.disagg))

# add indicator column
ind.31 <-ind.31 %>% 
  mutate(indicator = name.ind,.before = 1)

  
# INDICATOR ENTRE 3 Y 5 ANOS ----------------------------------------------
# Max(1.01, 1.08, 1.09., 1.10,3.01.)

name.act <- unlist((indicators %>% filter(indicator == indicators.list[7, "indicator"]) %>% select(number_activity)))
name.ind <- unique(unlist((indicators %>% filter(indicator == indicators.list[7, "indicator"]) %>% select(indicator))))

# Get max value by location in schools
temp <-calculate_max(data = df, activity = name.act, aggregation = c(adm.level[[3]],col.location.type,location),targets_summarise = c(col.reach.type, col.reach.disagg))

# Aggregate at adm level
# Calculate total
ind.anos.3.5 <- calculate_sum(data = temp,
                      activity = 'none',
                      aggregation = c(adm.level[[3]]),
                      targets_summarise = c(age.3.5))

# add indicator column
ind.anos.3.5 <- ind.anos.3.5 %>% 
  mutate(indicator = name.ind,.before = 1) 


# INDICATOR ENTRE 6 Y 11 ANOS ---------------------------------------------
# Max(1.02+1.03+1.05, 1.08, 1.09, 1.10,3.01)

name.act <- unlist((indicators %>% filter(indicator == indicators.list[8, "indicator"]) %>% select(number_activity)))
name.ind <- unique(unlist((indicators %>% filter(indicator == indicators.list[8, "indicator"]) %>% select(indicator))))

# Calculate (1.02+1.03+1.05) at location level
sum.act<- c('1.02','1.03','1.05')
temp <- calculate_sum(data = df,
                      activity = sum.act,
                      aggregation = c(col.activity,adm.level[[3]], col.location.type,location),
                      targets_summarise = c(col.reach.type,col.reach.disagg))

# add to main dataset
temp <- bind_rows(temp, df)

# Calculate Max(sum, 1.08, 1.09, 1.10,3.01) at location level
temp <-calculate_max(data = temp, activity = name.act, aggregation = c(adm.level[[3]],col.location.type,location),targets_summarise = c(col.reach.type, col.reach.disagg))

# Aggregate at adm level
# Calculate total
ind.anos.6.11 <- calculate_sum(data = temp,
                               activity = 'none',
                               aggregation = adm.level[[3]],
                               targets_summarise = c(age.6.11))

# add indicator column
ind.anos.6.11 <-ind.anos.6.11 %>% 
  mutate(indicator = unique(name.ind),.before = 1)  


# INDICATOR ENTRE 12 Y 17 ANOS --------------------------------------------
# Max(1.02+1.03+1.05, 1.06, 1.07, 1.08, 1.09, 1.10,2.10,2.11,3.01)

name.act <- unlist((indicators %>% filter(indicator == indicators.list[9, "indicator"]) %>% select(number_activity)))
name.ind <- unique(unlist((indicators %>% filter(indicator == indicators.list[9, "indicator"]) %>% select(indicator))))

# Calculate (1.02+1.03+1.05) at location level
sum.act<- c('1.02','1.03','1.05')
temp <- calculate_sum(data = df,
                      activity = sum.act,
                      aggregation = c(adm.level[[3]], col.location.type,location),
                      targets_summarise = c(col.reach.type, col.reach.disagg))

# add to main dataset
temp <- bind_rows(temp, df)

# Calculate Max(sum, 1.08, 1.09, 1.10,2.10,2.11,3.01) at location level
temp <-calculate_max(data = temp, activity = name.act, aggregation = c(adm.level[[3]],col.location.type,location),targets_summarise = c(col.reach.type, col.reach.disagg))

# Aggregate at adm level
# Calculate total
ind.anos.12.17 <- calculate_sum(data = temp,
                                activity = 'none',
                                aggregation = adm.level[[3]],
                                targets_summarise = c(age.12.17))
# add indicator column
ind.anos.12.17 <-ind.anos.12.17 %>% 
  mutate(indicator = unique(name.ind),.before = 1)


# INDICATOR ENTRE 18 A 60+ ANOS -------------------------------------------------
# Max(2.01 a 2.08,2.10,2.13,2.14) + Max(2.09, 2.10,2.11) + 2.12

name.act <- unlist((indicators %>% filter(indicator == indicators.list[10, "indicator"]) %>% select(number_activity)))
name.ind <- unique(unlist((indicators %>% filter(indicator == indicators.list[10, "indicator"]) %>% select(indicator))))

# Calculate Max(2.01 a 2.08,2.10,2.13,2.14) at location level
max.act<- c('2.01','2.02','2.03','2.04','2.05','2.06','2.07','2.08',
            '2.10', '2.13','2.14')
temp.max1 <- calculate_max(data = df, activity = max.act, aggregation = c(adm.level[[3]],col.location.type,location), targets_summarise = c(col.reach.type, col.reach.disagg))


# Calculate Max(2.09, 2.10,2.11) at location level
max.act<- c('2.09','2.10','2.11')
temp.max2 <- calculate_max(data = df, activity = max.act, aggregation = c(adm.level[[3]],col.location.type,location), targets_summarise = c(col.reach.type, col.reach.disagg))
# temp.max2 <- distinct(temp.max2, across(all_of(c(adm.level,location))), .keep_all = TRUE)

# bind datasets
temp.2.12 <- filter(df, grepl(paste('2.12', collapse = "|"), get(col.activity)))
temp<- bind_rows(temp.2.12,temp.max1, temp.max2)

# Calculate aggregation by adm level
temp <- calculate_sum(data = temp,
                      activity = 'none',
                      aggregation = c(adm.level[[3]]),
                      targets_summarise = c(age.adult))

# Add calculation total age and gender
ind.anos.18.60 <-temp %>% 
  mutate(indicator = unique(name.ind),.before = 1)


# INDICATOR TOTAL ---------------------------------------------------------

temp <- bind_rows(ind.11,ind.12, ind.13,
                  ind.21, ind.22, ind.31)

ind.total <- calculate_sum(data = temp,
                           activity = 'none',
                           aggregation = 'indicator',
                           targets_summarise = c(col.reach.type,col.reach.disagg))

# Calculate disaggregation by age and sex (ninas, ninos, mujeres, homres, mujeres mayor y hombres mayor)
ind.total<-calculate_age_gender_disaggregation(ind.total)
ind.11<-calculate_age_gender_disaggregation(ind.11)
ind.12<-calculate_age_gender_disaggregation(ind.12)
ind.13<-calculate_age_gender_disaggregation(ind.13)
ind.21<-calculate_age_gender_disaggregation(ind.21)
ind.22<-calculate_age_gender_disaggregation(ind.22)
ind.31<-calculate_age_gender_disaggregation(ind.31)


# Calculate indicators by administrative level
temp <- bind_rows(ind.11,ind.12, ind.13,
                  ind.21, ind.22, ind.31)

ind.adm3 <- calculate_sum(data = temp ,activity = 'none',aggregation = c('indicator', adm.level[[3]]), targets_summarise =c(col.reach.type,col.reach.disagg, col.totals, col.soma))
ind.adm2 <- calculate_sum(data = ind.adm3 ,activity = 'none',aggregation = c('indicator',adm.level[[2]]), targets_summarise =c(col.reach.type,col.reach.disagg,col.totals, col.soma))
ind.adm1 <- calculate_sum(data = ind.adm3 ,activity = 'none',aggregation = c('indicator', adm.level[[1]]), targets_summarise =c(col.reach.type,col.reach.disagg,col.totals, col.soma))
ind.adm0 <- calculate_sum(data = ind.adm3 ,activity = 'none',aggregation = c('indicator'), targets_summarise =c(col.reach.type,col.reach.disagg, col.totals, col.soma))


# SECTORIAL OBJECTIVES ----------------------------------------------------

objectives.list<-as.vector(unique(indicators$sectorial_objective) %>% na.omit())
oe.list <- lapply(1:length(objectives.list), function(i){
  name.ind<-unique(unlist((indicators %>% filter(sectorial_objective == objectives.list[i]) %>% select(indicator))))

  temp <- filter(ind.adm3, grepl(paste(name.ind, collapse = "|"),indicator))
  oe.adm3 <- calculate_sum(data = temp, activity = 'none', aggregation = adm.level[[3]], targets_summarise =c(col.reach.type,col.reach.disagg,col.totals, col.soma)) %>%
    mutate(indicator = objectives.list[[i]], .before = 1)

  oe.adm2 <- calculate_sum(data = oe.adm3, activity = 'none', aggregation = c('indicator', adm.level[[2]]), targets_summarise =c(col.reach.type,col.reach.disagg,col.totals, col.soma))
  oe.adm1 <- calculate_sum(data = oe.adm3, activity = 'none', aggregation = c('indicator', adm.level[[1]]), targets_summarise =c(col.reach.type,col.reach.disagg,col.totals, col.soma))
  oe.adm0 <- calculate_sum(data = oe.adm3, activity = 'none', aggregation = c('indicator'), targets_summarise =c(col.reach.type,col.reach.disagg,col.totals, col.soma))
  result <- list(oe.adm0, oe.adm1, oe.adm2, oe.adm3)
})

oe.adm0 <- bind_rows(oe.list[[1]][[1]], oe.list[[2]][[1]], oe.list[[3]][[1]])
oe.adm1 <- bind_rows(oe.list[[1]][[2]], oe.list[[2]][[2]], oe.list[[3]][[2]])
oe.adm2 <- bind_rows(oe.list[[1]][[3]], oe.list[[2]][[3]], oe.list[[3]][[3]])
oe.adm3 <- bind_rows(oe.list[[1]][[4]], oe.list[[2]][[4]], oe.list[[3]][[4]])


# TOTAL ALCANZADOS --------------------------------------------------------

children<-bind_rows(ind.anos.3.5,ind.anos.6.11, ind.anos.12.17)
# Calculate children by adm level
children <- calculate_age_gender_disaggregation(children %>% mutate(!!sym(col.reached) := 0))
adults <- calculate_age_gender_disaggregation(ind.anos.18.60 %>% mutate(!!sym(col.reached) := 0))

# Create dataframe with adults and children
temp <- bind_rows(children, adults) %>% select(-indicator)
target.adm3 <- calculate_sum(data = temp,activity = 'none',aggregation = adm123,targets_summarise = c(col.totals,col.soma,col.reached))
target.adm2 <- calculate_sum(data = temp,activity = 'none',aggregation = adm12,targets_summarise = c(col.totals,col.soma,col.reached))
target.adm1 <- calculate_sum(data = temp,activity = 'none',aggregation = adm1,targets_summarise = c(col.totals,col.soma,col.reached))
target.adm0 <- calculate_sum(data = temp %>% mutate('#adm0+name'='Venezuela', '#adm0+code'='VE1'),activity = 'none',aggregation = c('#adm0+name','#adm0+code'),targets_summarise = c(col.totals,col.soma,col.reached))


# CALCULATE PERCENTAGE ----------------------------------------------------
# Calculate Percentages
# Add columns with percentage values
# cols.pct<-c(col.reach.type,col.reach.disagg,col.totals)
# cols.pct <- cols.pct[cols.pct != c('#reached',"total_alcanzados")]

# Calculate percentage of indicator and objective
df.adm0 <- calculate_percentage(arrange_data(bind_rows(oe.adm0, ind.adm0), adm.level = 'adm0'), adm.level = 'adm0')%>%
  mutate('#adm0+name'='Venezuela', '#adm0+code'='VE1', .before=1)
df.adm1 <- calculate_percentage(arrange_data(bind_rows(oe.adm1, ind.adm1), adm.level = adm1), adm.level = adm.level[[1]])%>%
  select(all_of(adm.level[[1]]), everything())
df.adm2 <- calculate_percentage(arrange_data(bind_rows(oe.adm2, ind.adm2), adm.level = adm12), adm.level = adm.level[[2]]) %>% 
  select(all_of(adm.level[[2]]), everything())
df.adm3 <- calculate_percentage(arrange_data(bind_rows(oe.adm3, ind.adm3), adm.level = adm123), adm.level = adm.level[[3]]) %>% 
  select(all_of(adm.level[[3]]), everything())

t.adm0 <- calculate_percentage_target(arrange_data_target(target.adm0,adm.level = adm0), adm.level = adm0) 
t.adm1 <- calculate_percentage_target(arrange_data_target(target.adm1,adm.level = adm.level[[1]]), adm.level = adm.level[[1]]) 
t.adm2 <- calculate_percentage_target(arrange_data_target(target.adm2,adm.level = adm.level[[2]]), adm.level = adm.level[[2]]) 
t.adm3 <- calculate_percentage_target(arrange_data_target(target.adm3,adm.level = adm.level[[3]]), adm.level = adm.level[[3]]) 


# ADD LABELS --------------------------------------------------------------
# df.adm0<- df.adm0%>% 
#   left_join(name_labels) %>%
#   select(adm0,"indicator","descripcion","personas","total_personas","personas_pct") %>% 
#   rename("desagregación" = "descripcion",
#          "indicador" = "indicator")
# 
# df.adm1 <- df.adm1 %>% 
#   left_join(name_labels) %>%
#   select(adm.level[[1]],"indicator","descripcion","personas","total_personas","personas_pct") %>% 
#   rename("desagregación" = "descripcion",
#          "indicador" = "indicator")
# 
# df.adm2 <- df.adm2 %>% 
#   left_join(name_labels) %>%
#   select(adm.level[[2]],"indicator","descripcion","personas","total_personas","personas_pct") %>% 
#   rename("desagregación" = "descripcion",
#          "indicador" = "indicator")
# 
# t.adm0 <- t.adm0 %>% 
#   left_join(name_labels) %>%
#   select(adm0,"descripcion","personas","total_personas","personas_pct") %>% 
#   rename("desagregacion" = "descripcion")
# 
# t.adm1 <- t.adm1 %>% 
#   left_join(name_labels) %>%
#   select(adm.level[[1]],"descripcion","personas","total_personas","personas_pct") %>% 
#   rename("desagregacion" = "descripcion")
# 
# t.adm2 <- t.adm2 %>% 
#   left_join(name_labels) %>%
#   select(adm.level[[2]],"descripcion","personas","total_personas","personas_pct") %>% 
#   rename("desagregacion" = "descripcion")

# EXPORT ------------------------------------------------------------------

# Create list of dataframes
df_list <- list(
  'DATA' = clean_data,
  'adm0_target' = t.adm0,
  'adm0_ind' = df.adm0,
  
  'adm1_target' = t.adm1,
  'adm1_ind' = df.adm1,
  
  'adm2_target' = t.adm2,
  'adm2_ind' = df.adm2)

# Get the names of dataframes and remove the first name
sheet_names <- names(df_list)[-1]

# Replace NA values with another value (e.g., "Missing") in each dataframe
for (sheet_name in sheet_names) {
  temp <- df_list[[sheet_name]]
  temp[is.na(temp)] <- 0
  df_list[[sheet_name]] <- temp
}


# Write each dataframe to a separate sheet in the Excel workbook
hs <- createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
  fontName = "Arial Narrow", fgFill = "#0047b2"
)
write.xlsx(df_list, paste0('./02_output/',sector,'//',reported_date,'_cluster_education_analisis_345w.xlsx'), colWidths=20,  headerStyle = hs)


# Ocha
# Write each dataframe to a separate sheet in the Excel workbook
write_xlsx(
  target.adm2 %>% select(all_of(c(adm.level[[2]],col.reached,col.totals))),
  path = paste0('./02_output/',sector,'//',reported_date,'_cluster_education_345w_ocha.xlsx')
)
