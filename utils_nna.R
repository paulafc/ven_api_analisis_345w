#ADR NNA 2024
# CALCULATION OF INDICATIORS AND REACHED PEOPLE
# FUNCTIONS
# VARIABLES

# FUNCTION ----------------------------------------------------------------
# function to remote special caracters, spaces, comma, dot,etc from location name
data_name_tolower_remove_space_dots <- function(data, data.name) {
  data <- data %>%
    mutate(name_check = str_to_lower(str_trim(str_replace_all(!!sym(data.name), "\\s+|\\.|-|\\(|\\)|\\[]|,|:|º|°", ""))))
  data$name_check<-stringi::stri_trans_general(data$name_check, "Latin-ASCII")
  return(data)
}


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


# Function to calculate maximum based on specified activity and aggregation
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


# Calculate total counts for each gender and age group
calculate_age_gender_disaggregation <- function(data){
  data <- data %>%
    mutate(!!sym(col.nina) := rowSums(select(., any_of(ninas)), na.rm = TRUE),
           !!sym(col.nino) := rowSums(select(., any_of(ninos)), na.rm = TRUE),
           !!sym(col.mujer) := rowSums(select(., any_of(mujeres)), na.rm = TRUE),
           !!sym(col.hombre) := rowSums(select(., any_of(hombres)), na.rm = TRUE),
           !!sym(col.mujer.mayor) := rowSums(select(., any_of(mujeres_mayor)), na.rm = TRUE),
           !!sym(col.hombre.mayor) := rowSums(select(., any_of(hombres_mayor)), na.rm = TRUE),
           !!sym(col.nina.mujer) := rowSums(select(., any_of(ninas_mujeres)), na.rm = TRUE),
           !!sym(col.nino.hombre) := rowSums(select(., any_of(ninos_hombres)), na.rm = TRUE))
  
  # Calculate total reached
  data <- data %>% select(-!!sym(col.reached))
  data <- data %>%
    mutate(!!sym(col.reached) := rowSums(select(., any_of(col.totals)), na.rm = TRUE))
  
  return(data)
}



# Arrange data 
arrange_data <- function(data, adm.level){
  if ('adm0' %in% adm.level) {
    data <- data %>% 
      gather(key = "desagregacion", value = "personas",  -indicator)
  }else
    # arrange data
    data <- data %>% 
      gather(key = "desagregacion", value = "personas",  -indicator, -adm.level)
  
  return(data)
}


# calculate percentage
calculate_percentage <- function(data, adm.level){  
  # get total values for each indicator/objective
  data_total<-data %>% 
    filter(desagregacion == col.reached) %>% 
    rename('total_personas' = personas)
  
  # data <- data %>% 
  #   filter(variable != 'total_alcanzados')
  
  
  if ('adm0' %in% adm.level) {
    # add total alcanzados to data as new column
    data <- data %>% 
      left_join(data_total %>% select(-desagregacion), by = c('indicator'), relationship = "many-to-one")
    
  }
  else
    # add total alcanzados to data as new column
    data <- data %>% 
      left_join(data_total %>% select(-desagregacion), by = c('indicator', adm.level), relationship = "many-to-one")
  
  # calculate percentage 
  data <- data %>% 
    mutate(personas_pct = round((personas / total_personas) * 100, 2))
  
  return(data)
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

retrieve_data <- function(baseurl) {
  # Retrieve the first page to determine the total number of pages
  first_page <- fromJSON(paste0(baseurl, "&page=1"))
  total_pages <- first_page[["total_pages"]] # Adjust this based on your API's response structure
  message("Total pages to retrieve: ", total_pages)
  
  # Retrieve data for each page using lapply
  pages <- lapply(1:(total_pages+1), function(i) {
    mydata <- fromJSON(paste0(baseurl, "&page=", i))
    message("Retrieving page ", i)
    mydata[['data']]
  })
  
  # Filter out NULL and zero-length elements
  cleaned_pages <- Filter(function(x) !is.null(x) && length(x) > 0, pages)
  # Unnest wider for each dataframe in the list
  res <- lapply(cleaned_pages, function(df) {
    df<-df %>% 
      unnest_wider(c(reached_disaggregation, other_fields))
  })
  # Combine all cleaned pages into one data frame
  # combined_data <- bind_rows(cleaned_pages)
  
  return(res)
}

retrieve_disaggregation <- function(baseurl) {
  data <- fromJSON(baseurl, flatten = TRUE)
  data <- data[["disaggregations"]]
  return(data)
}

retrieve_field <- function(baseurl) {
  data <- fromJSON(baseurl, flatten = TRUE)
  data <- data[["fields"]]
  return(data)
}


filter_data_sector_validated <- function(df,sector){
  # Filter by sector, activity recurrent and validated
  df <- df %>%
    filter(tolower(iconv(!!sym(col.sector), from = "UTF-8", to = "ASCII//TRANSLIT")) == tolower(sector),
           # tolower(iconv(!!sym(col.recurrent), from = "UTF-8", to = "ASCII//TRANSLIT")) == 'no',
           tolower(iconv(!!sym(col.validated), from = "UTF-8", to = "ASCII//TRANSLIT")) == 'si')
  return(df)
}


# clean_age_group_by_activity <- function (data, disagg_data){
#   # Organise selection of activities by indicator
#   clean_data<-disagg_data %>%
#     mutate(number_activity = sub("([^:]*).*", "\\1", activity)) 
#   
#   all_disaggregation <- clean_data$disaggregation %>% na.omit()
#   control_loop <- length(clean_data$activity %>% na.omit())
#   
#   # Clean activities based on disaggregation
#   #Activities for children, set 0 on adult disaggregation
#   # Activities for adults, set 0 on children disaggregation
#   res <- lapply(1:control_loop,function(i){
#     act <- clean_data$number_activity[i]
#     disaggregation <- as.vector(unlist(str_split(clean_data$group[i], "\n")))
#     diff.disaggregation <- setdiff(all_disaggregation,disaggregation)
#     act.check <- clean_data$check[i]
#     res<-data
#     res<- res %>%
#       filter(grepl(paste(act, collapse = "|"), !!sym(col.activity)))
#     
#     # Check if activity needto be cleaned based on disaggregation
#     if (act.check == 'yes') {
#       res<- res %>%
#         mutate(across(all_of(diff.disaggregation), ~ 0))
#     }
#     return(res)
#   })
#   
#   # clean dataset
#   df<- bind_rows(res)
#   df <- df %>%
#     mutate(!!sym(col.reached) := rowSums(across(all_of(all_disaggregation)), na.rm = TRUE))
#   return(df)
# }

# function to remote special caracters, spaces, comma, dot,etc from location name
# location_name_tolower_remove_space_dots <- function(data, data.name) {
#   data <- data %>%
#     mutate(name_check = str_to_lower(str_trim(str_replace_all(!!sym(data.name), "\\s+|\\.|-|\\(|\\)|\\[]|,|:|º|°", ""))))
#   data$name_check<-stringi::stri_trans_general(data$name_check, "Latin-ASCII")
#   return(data)
# }


# VARIABLES ---------------------------------------------------------------

# AoR or Cluster
col.sector <- '#sector'

# Administrative Levels
adm0 <- c("#adm0+name", "#adm0+code")
adm1 <- c("#adm1+code", "#adm1+name")
adm12 <- c("#adm1+code", "#adm1+name", "#adm2+code", "#adm2+name")
adm123 <- c("#adm1+code", "#adm1+name", "#adm2+code", "#adm2+name", "#adm3+code", "#adm3+name")
adm4.code <- "#adm4+code"
# Administrative Level List
adm.level <- list(adm1, adm12, adm123)

# Location Identifier
location <- "#location"
col.location <- "#location"
col.location.type <- "#location+type"

# Columns related to Activities
col.activity <- "#activity+name+selected"
# activity<- "#activity+name+selected"
col.recurrent <- "#activity+recurrent"
col.validated <- "#validated"
col.hrp <- "#meta+hrp"

# Columns related to Reach and Demographics
col.reached <- '#reached'
col.reach.type <- c("#reached+indigenous", "#reached+disabled", "#reached+f+pregnant", "#reached+uasc", "#reached+physical_legal_protection", 
                    "#reached+lgbti", "#victims+trafficking")
col.reach.disagg <- c("#reached", "#reached+f+children+age0_5", "#reached+m+children+age0_5", "#reached+f+children+age6_9",
                      "#reached+m+children+age6_9", "#reached+f+children+age10_11", "#reached+m+children+age10_11", "#reached+f+children+age12_14", 
                      "#reached+m+children+age12_14", "#reached+f+children+age15_17", "#reached+m+children+age15_17", "#reached+f+adult+age18_59", 
                      "#reached+m+adult+age18_59", "#reached+f+elderly+age60_", "#reached+m+elderly+age60_", "#reached+legal+f+children+age0_11", 
                      "#reached+legal+m+children+age0_11", "#reached+legal+f+children+age12_13", "#reached+legal+m+children+age12_13", 
                      "#reached+legal+f+children+age14_17", "#reached+legal+m+children+age14_17", "#reached+birth+f+children+age0_3m", 
                      "#reached+birth+m+children+age0_3m", "#reached+birth+f+children+age4m_8", "#reached+birth+m+children+age4m_8", 
                      "#reached+birth+f+children+age9_18", "#reached+birth+m+children+age9_18")

# Demographic Categories
ninas <- c("#reached+f+children+age0_5", "#reached+f+children+age6_9",
           "#reached+f+children+age10_11", "#reached+f+children+age12_14",
           "#reached+f+children+age15_17", "#reached+legal+f+children+age0_11",
           "#reached+legal+f+children+age12_13", "#reached+legal+f+children+age14_17",
           "#reached+birth+f+children+age0_3m", "#reached+birth+f+children+age4m_8",
           "#reached+birth+f+children+age9_18")
ninos <- c("#reached+m+children+age0_5", "#reached+m+children+age6_9",
           "#reached+m+children+age10_11", "#reached+m+children+age12_14",
           "#reached+m+children+age15_17", "#reached+legal+m+children+age0_11",
           "#reached+legal+m+children+age12_13", "#reached+legal+m+children+age14_17",
           "#reached+birth+m+children+age0_3m", "#reached+birth+m+children+age4m_8",
           "#reached+birth+m+children+age9_18")
mujeres <- c("#reached+f+adult+age18_59")
hombres <- c("#reached+m+adult+age18_59")
mujeres_mayor <- c("#reached+f+elderly+age60_")
hombres_mayor <- c("#reached+m+elderly+age60_")
ninas_mujeres <- c(ninas, mujeres, mujeres_mayor)
ninos_hombres <- c(ninos, hombres, hombres_mayor)

# Calculation Columns
col.totals <- c("#reached+f+children", "#reached+m+children", "#reached+f+adults", "#reached+m+adults",
                "#reached+f+elderly", "#reached+m+elderly")
col.soma <- c("#reached+f+children+adults+elderly", "#reached+m+children+adults+elderly")

# Individual Demographic Columns
col.nina <- "#reached+f+children"
col.nino <- "#reached+m+children"
col.mujer <- "#reached+f+adults"
col.hombre <- "#reached+m+adults"
col.mujer.mayor <- "#reached+f+elderly"
col.hombre.mayor <- "#reached+m+elderly"
col.nina.mujer <- "#reached+f+children+adults+elderly"
col.nino.hombre <- "#reached+m+children+adults+elderly"