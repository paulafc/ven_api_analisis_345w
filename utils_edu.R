# 
# location_name_tolower_remove_space_dots <- function(data, data.name) {
#   data <- data %>%mutate_at(vars(!!sym(col.adm4Pcode), !!sym(data.name)), na_if, "")
#   data <- data %>%
#     mutate(name_check = str_to_lower(str_trim(str_replace_all(!!sym(data.name), "[^[:alnum:]]", ""))),
#            name_check = ifelse(!is.na(!!sym(col.adm4Pcode)), !!sym(col.adm4Pcode), name_check))
#   data$name_check<-stringi::stri_trans_general(data$name_check, "Latin-ASCII")
#   return(data)
# }

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
    summarize(across(targets_summarise, max)) %>%
    ungroup()
  
  # # Semi-join data with aggregated data to get filtered data
  # filtered_data <- semi_join(data, data.agg, by = c(aggregation))
  return(data.agg)
}


calculate_age_gender_disaggregation <- function(data){
  # Calculate total counts for each gender and age group
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


# Arrange data 
arrange_data_target <- function(data, adm.level){
  # arrange data
  data <- data %>% 
    gather(key = "desagregacion", value = "personas",  -adm.level)
  return(data)
}


# calculate percentage
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


# filter_data_sector_validated <- function(df,sector){
#   # Filter by sector, activity recurrent and validated
#   df <- df %>%
#     filter(tolower(iconv(!!sym(col.sector), from = "UTF-8", to = "ASCII//TRANSLIT")) == tolower(sector),
#            tolower(iconv(!!sym(col.validated), from = "UTF-8", to = "ASCII//TRANSLIT")) == 'si')
#   return(df)
# }


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



# if reached = 0 and unit is people, then get resource quantity value
# if unit is people, resource quantity = reached
# if activity unit is establishment, commeetes, schools,and resource quantity is 0, replace with 1.
# if if activity unit is establishment, commeetes, schools, and reach = resource quantity, replace resource with 1
# If activity unit is Establishments and reached =0, get resource quantity = reached
# If activity unit is Establishments, set resource quantity to 1

# calculate_reached_by_activity_unit <- function(df, act_data){
#   df<-df %>%
#     mutate(
#       !!sym(col.reached) := ifelse(tolower(!!sym(col.activity.unit)) %in% unit_people & !!sym(col.reached) == 0, !!sym(col.resource.quantity), !!sym(col.reached)),
#       !!sym(col.resource.quantity) := ifelse(tolower(!!sym(col.activity.unit)) %in% unit_people, !!sym(col.reached),!!sym(col.resource.quantity)),
#       !!sym(col.resource.quantity):= ifelse(!(tolower(!!sym(col.activity.unit)) %in% unit_people) & !!sym(col.resource.quantity)==0, 1, !!sym(col.resource.quantity)),
#       !!sym(col.resource.quantity):= ifelse(tolower(!!sym(col.activity.unit)) %in% unit_replace_with_1 & !!sym(col.resource.quantity) == !!sym(col.reached), 1,!!sym(col.resource.quantity)),
#       !!sym(col.reached):= ifelse(tolower(!!sym(col.activity.unit)) == tolower('Establishments') & !!sym(col.reached) == 0, !!sym(col.resource.quantity),!!sym(col.reached)),
#       !!sym(col.resource.quantity):= ifelse(tolower(!!sym(col.activity.unit)) %in% tolower(unit_replace_with_1), 1,!!sym(col.resource.quantity))
#     ) 
#   return(df)
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

col.adm0Name <- '#adm0+name'
col.adm1Pcode <- '#adm1+code'
col.adm1Name <- '#adm1+name'
col.adm2Pcode <- '#adm2+code'
col.adm2Name <- '#adm2+name'
col.adm3Name <- '#adm3+name'
col.adm3Pcode <- '#adm3+code'
col.adm4Pcode <- '#adm4+code'
col.severity <- 'SEVERITY'
col.priorization <- 'Priorizacion'

# Location Identifier
location <- "#location"
col.location <- "#location"
col.location.type <- "#location+type"
adm.school <- c('#adm4+name', '#adm4+code')
col.loc.type <- "#location+type"


# Columns related to Activities
col.activity.id <- '#activity+id'
col.activity.unit<-'#activity+indicator+unit'
col.activity <- '#activity+name+selected'
col.recurrent <- "#activity+recurrent"
col.validated <- "#validated"
col.sector <- '#sector'
col.sector.code <- '#sector+code'
col.sector.acronym <- '#sector+acronym+hpctools'
col.hrp <- "#meta+hrp"

# Columns related to Reach and Demographics
col.reached <- '#reached'
col.reach.type <-c('#reached+indigenous',	'#reached+disabled', '#reached+lgbti',	'#victims+trafficking')
col.reach.disagg <- c('#reached','#reached+f+children+age0_2', '#reached+m+children+age0_2',	'#reached+f+children+age3_5',	'#reached+m+children+age3_5',	'#reached+f+children+age6_11',
                      '#reached+m+children+age6_11','#reached+f+children+age12_17','#reached+m+children+age12_17', '#reached+f+children+age18_19',	'#reached+m+children+age18_19',
                      '#reached+f+adult+age20_59', '#reached+m+adult+age20_59',	'#reached+f+elderly+age60_',	'#reached+m+elderly+age60_')

# Age variables
age.0.2<- c('#reached+f+children+age0_2', '#reached+m+children+age0_2')
age.3.5<-c('#reached+f+children+age3_5',	'#reached+m+children+age3_5')
age.6.11 <- c('#reached+f+children+age6_11', '#reached+m+children+age6_11')
age.12.17 <- c('#reached+f+children+age12_17', '#reached+m+children+age12_17')
age.adult <- c('#reached+f+children+age18_19', '#reached+m+children+age18_19',
               '#reached+f+adult+age20_59', '#reached+m+adult+age20_59',
               '#reached+f+elderly+age60_', '#reached+m+elderly+age60_')

# Gender and age disaggregation
ninas <- c('#reached+f+children+age0_2','#reached+f+children+age3_5', '#reached+f+children+age6_11','#reached+f+children+age12_17')
ninos <-c('#reached+m+children+age0_2','#reached+m+children+age3_5', '#reached+m+children+age6_11','#reached+m+children+age12_17')
mujeres <-c('#reached+f+children+age18_19','#reached+f+adult+age20_59')
hombres<- c('#reached+m+children+age18_19','#reached+m+adult+age20_59')
mujeres_mayor <-c('#reached+f+elderly+age60_')
hombres_mayor <- c('#reached+m+elderly+age60_')
ninas_mujeres <- c(ninas, mujeres,mujeres_mayor)
ninos_hombres <- c(ninos, hombres, hombres_mayor)

# Calculation Columns
col.totals <- c("#reached+f+children", "#reached+m+children", "#reached+f+adults", "#reached+m+adults",
                "#reached+f+elderly+age60_", "#reached+m+elderly+age60_")
col.soma <- c("#reached+f+children+adults+elderly", "#reached+m+children+adults+elderly")

# Individual Demographic Columns
col.nina <- "#reached+f+children"
col.nino <- "#reached+m+children"
col.mujer <- "#reached+f+adults"
col.hombre <- "#reached+m+adults"
col.mujer.mayor <- "#reached+f+elderly+age60_"
col.hombre.mayor <- "#reached+m+elderly+age60_"
col.nina.mujer <- "#reached+f+children+adults+elderly"
col.nino.hombre <- "#reached+m+children+adults+elderly"