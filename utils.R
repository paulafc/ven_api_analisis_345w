
# VARIABLE ----------------------------------------------------------------
# Columns related to Activities
col.reached <- '#reached'
col.org.impl <- '#org+impl'
col.activity <- "activity_name"
col.hrp <- "#meta+hrp"
col.recurrent <- "#activity+recurrent" 
col.validated <- '#validated'
col.activity.id <- '#activity+id'
col.activity.unit<-'#activity+indicator+unit'
col.activity <- '#activity+name+selected'
col.sector <- '#sector'
col.sector.code <- '#sector+code'
col.sector.acronym <- '#sector+acronym+hpctools'
col.resource.quantity <- '#activity+indicator+reached'
location <- '#location'
col.location.type <-"#location+type"
col.id <- '#id'
col.date.update <- '#date+updated'

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


# FUNCTION ----------------------------------------------------------------
getDataFromAPI <- function(api_url, activity_code, disagg_data, col_names_345w) {
  sector_loop <-api_url$sector
  result <- lapply(1:length(sector_loop), function(i){
    
    ### VARIABLES
    sector <- api_url$sector[[i]]
    print(api_url$sector[[i]])
    
    # list of sector activities
    act_list <- activity_code %>% 
      filter(!!sym(col.sector.code) == sector) 
    
    # base code of sector activity (CLWSH,CLNUT,.. )
    act_code <-unique(act_list %>% pull(!!sym(col.sector.acronym))) %>% na.omit()
    
    # 345w disaggregations
    all_disaggregation<- c(disagg_data[[sector]][["disaggregation"]]) %>% na.omit()
    
    ### GET DATA FROM URL
    # Get URL for the data
    baseurl <- api_url$data[i]
    data<- retrieve_data(baseurl)
    
    # Get URL for the disaggregation codes column names
    baseurl<-api_url$disaggregation[i]
    disaggregation<-retrieve_disaggregation(baseurl)
    
    
    # Get URL for the custom fields codes column names
    baseurl<-api_url$custom_fields[i]
    custom_fields<-retrieve_field(baseurl)
    
    
    ### DATA ORGANISATION
    # Create column with number of the activity
    data <- lapply(data, function(df) {
      df<-df %>%
        mutate(number_activity = sub("([^:]*).*", "\\1", !!sym("activity_name"))) 
      return(df)
    })
    
    # add column with activity code, join by the activity number
    data <- lapply(data, function(df) {
      df<-df %>%
        left_join(act_list %>% select(all_of(c("number_activity", col.activity.id, col.activity.unit))),
                  by = "number_activity")
      return(df)
    })
    
    # transform disaggregation into numeric column type, if NA replace with 0
    data <- lapply(data, function(df) {
      cols_to_convert <- grep(paste(as.character(disaggregation$id), collapse = "|"), names(df), value = TRUE)
      df[cols_to_convert] <- lapply(df[cols_to_convert], as.numeric)
      return(df)
    })
    
    # Combine data
    data <- bind_rows(data)
    data <- data %>%
      mutate(across(all_of(as.character(disaggregation$id)), ~ coalesce(as.numeric(.), 0)))
    
    # Filter column names metadata to include only the columns present in the data
    valid_col_names_345w <- col_names_345w %>%
      filter(data_name %in% colnames(data))
    
    # Change column names to match the column names of the 345w platform
    #Disaggregation, metadada, Custom Fields
    data <- data %>%
      rename(!!!setNames(as.character(disaggregation$id), as.character(disaggregation$xhl))) %>%
      rename(!!!setNames(contains(as.character(custom_fields$id)), as.character(custom_fields$xhl))) %>%
      rename(!!!setNames(as.character(valid_col_names_345w$data_name),as.character(valid_col_names_345w$platform_name)))
    
    # Change validated and HRP from TRUE/False to Si/No
    data <- data %>%
      mutate(!!sym(col.validated) := case_when(
        tolower(!!sym(col.validated)) == 'true' ~ 'Sí',
        tolower(!!sym(col.validated)) == 'false' ~ 'No',
        TRUE ~ NA_character_
      ),
      !!sym(col.hrp) := case_when(
        tolower(!!sym(col.hrp)) == 'true' ~ 'Sí',
        tolower(!!sym(col.hrp)) == 'false' ~ 'No',
        TRUE ~ NA_character_
      ))
    
    return(data)
  })
  
  names(result) <- C(api_url$sector)
  return(result)
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


filter_data_sector_validated <- function(data){
  
  ## variables
  # sector.name <- tolower(iconv(unique(data %>% pull(!!sym(col.sector))), from = "UTF-8", to = "ASCII//TRANSLIT"))
  
  # filter
  data <- data %>%
    filter(tolower(iconv(!!sym(col.validated), from = "UTF-8", to = "ASCII//TRANSLIT")) == 'si')
    # filter(tolower(iconv(!!sym(col.sector), from = "UTF-8", to = "ASCII//TRANSLIT")) == tolower(sector.name),
    #        tolower(iconv(!!sym(col.validated), from = "UTF-8", to = "ASCII//TRANSLIT")) == 'si')

  return(data)    

}


clean_age_group_by_activity <- function(data, disagg_data, sector) {
  # Variables
  print(sector)
  df <- data
  
  # Check for 'check' column in disagg_data
  if (!'check' %in% colnames(disagg_data)) {
    print('no check needed')
    return(list(DATA = df, clean_log = NULL))  # Skip to next iteration if 'check' column is found
  }
  print('check needed')
  # Organize selection of activities by indicator
  clean_data <- disagg_data %>%
    mutate(number_activity = sub("([^:]*).*", "\\1", activity))
  
  all_disaggregation <- clean_data$disaggregation %>% na.omit()
  control_loop <- length(clean_data$activity %>% na.omit())
  
  # Clean activities based on disaggregation
  # Activities for children, set 0 on adult disaggregation
  # Activities for adults, set 0 on children disaggregation
  res <- lapply(1:control_loop, function(j) {
    act <- clean_data$number_activity[j]
    disaggregation <- as.vector(unlist(str_split(clean_data$group[j], "\n")))
    diff_disaggregation <- setdiff(all_disaggregation, disaggregation)
    act_check <- clean_data$check[j]
    res <- df %>%
      filter(grepl(paste(act, collapse = "|"), !!sym(col.activity)))
    
    # Check if activity needs to be cleaned based on disaggregation
    if (act_check == 'yes') {
      print(paste0('activity: ', act))
      clean_log <- lapply(1:length(diff_disaggregation), function(k) {
        print(diff_disaggregation[k])
        print(nrow(res %>% filter(!!sym(diff_disaggregation[k]) != 0)))
        clean_log <- res %>%
          filter(!!sym(diff_disaggregation[k]) != 0) %>%
          transmute(!!sym(col.id) := !!sym(col.id),
                    date = !!sym(col.date.update),
                    organisation = !!sym(col.org.impl),
                    sector = sector,
                    activity = !!sym(col.activity),
                    activity_unit = !!sym(col.activity.unit),
                    variable = deparse(substitute(!!sym(diff_disaggregation[k]))),
                    value = !!sym(diff_disaggregation[k]),
                    new_value = 0,
                    comment = paste0('Activity with wrong disaggregation', sector))
        return(clean_log)
      })
      
      clean_log<- bind_rows(clean_log)

      # Clean the dataset
      res <- res %>%
        mutate(across(all_of(diff_disaggregation), ~ 0))
      
      # Calculate new #reached
      clean_log_reached <- res %>%
        mutate(check = rowSums(select(., all_of(all_disaggregation))))
      
      clean_log_reached <- clean_log_reached %>% 
        filter(!!sym(col.reached) != check) %>% 
        transmute(!!sym(col.id) := !!sym(col.id),
                  date = !!sym(col.date.update),
                  organisation = !!sym(col.org.impl),
                  sector = sector,
                  activity = !!sym(col.activity),
                  activity_unit = !!sym(col.activity.unit),
                  variable = deparse(substitute(!!sym(col.reached))),
                  value = !!sym(col.reached),
                  new_value = check,
                  comment = paste0('Calculating new reached after cleaning by activity disaggregation'))

      #bind to clean log
      clean_log<- bind_rows(clean_log,clean_log_reached)
      
      res<-res %>% 
        mutate(!!sym(col.reached) := rowSums(select(., all_of(all_disaggregation))))
      
      # Store cleaned data and cleaning log in results list
      result <- list(DATA = res, clean_log = clean_log)
      
      return(result)
    } else { return(list(DATA = res, clean_log = NULL))}
    
  })
  # Extract all DATA frames and bind them together
  merged_data <- do.call(rbind, lapply(res, function(x) x$DATA))
  
  # Extract all clean_log frames and bind them together
  merged_clean_log <- do.call(rbind, lapply(res, function(x) x$clean_log))
  
  result <- list(DATA = merged_data, clean_log = merged_clean_log)
  return(result)
  
}


# CLEAN CHECKS
calculate_reached_by_activity_unit <- function(df, sector){
  
  # CHECK 1 #
  # check if activity unit is people and #reached is 0
  # Replace it with #activity+indicator+reached if it is not 0
  merged_clean_log<-df %>%
    filter(tolower(!!sym(col.activity.unit)) %in% unit_people & !!sym(col.reached) == 0 & !!sym(col.resource.quantity) != 0) %>% 
    transmute(!!sym(col.id) := !!sym(col.id),
              date := !!sym(col.date.update),
              organisation = !!sym(col.org.impl),
              sector = sector,
              activity = !!sym(col.activity),
              activity_unit = !!sym(col.activity.unit),
              variable = deparse(substitute(!!sym(col.reached))),
              value = !!sym(col.reached),
              new_value = !!sym(col.resource.quantity),
              comment = paste('Indicator unit is',!!sym(col.activity.unit) ,'but #reached is 0 - use the value of ',deparse(substitute(!!sym(col.resource.quantity))), 'if it is not 0'))
  
  df<-df %>%
    mutate(
      !!sym(col.reached) := ifelse(tolower(!!sym(col.activity.unit)) %in% unit_people & !!sym(col.reached) == 0, !!sym(col.resource.quantity), !!sym(col.reached)))
  
  
  # CHECK 2 #
  # If the indicator unit is people
  # Replace #activity+indicator+unit with #reached value
  merged_clean_log<- bind_rows(merged_clean_log,    
                               df %>%
                                 filter(tolower(!!sym(col.activity.unit)) %in% unit_people & !!sym(col.reached) != !!sym(col.resource.quantity)) %>% 
                                 transmute(!!sym(col.id) := !!sym(col.id),
                                           date := !!sym(col.date.update),
                                           organisation = !!sym(col.org.impl),
                                           sector = sector,
                                           activity = !!sym(col.activity),
                                           activity_unit = !!sym(col.activity.unit),
                                           variable = deparse(substitute(!!sym(col.resource.quantity))),
                                           value = !!sym(col.resource.quantity),
                                           new_value = !!sym(col.reached),
                                           comment = paste('Activity unit is',!!sym(col.activity.unit),',',deparse(substitute(!!sym(col.resource.quantity))), 'should be number of reached people (#reached)'))
  )
  
  df<-df %>%
    mutate(!!sym(col.resource.quantity) := ifelse(tolower(!!sym(col.activity.unit)) %in% unit_people, !!sym(col.reached),!!sym(col.resource.quantity)))
  
  
  # CHECK 3 #
  # If activity unit is establishment, schools...(not people, children, teachers)
  # Replace #activity+indicator+reached with 1 (if it is not 1 already)
  merged_clean_log<- bind_rows(merged_clean_log,
                               df %>%
                                 filter(tolower(!!sym(col.activity.unit)) %in% tolower(unit_replace_with_1) & !!sym(col.resource.quantity) != 1) %>% 
                                 transmute(!!sym(col.id) := !!sym(col.id),
                                           date := !!sym(col.date.update),
                                           organisation = !!sym(col.org.impl),
                                           sector = sector,
                                           activity = !!sym(col.activity),
                                           activity_unit = !!sym(col.activity.unit),
                                           variable = deparse(substitute(!!sym(col.resource.quantity))),
                                           value = !!sym(col.resource.quantity),
                                           new_value = 1,
                                           comment = paste('Activity unit is',!!sym(col.activity.unit), ', replace #activity+indicator+reached with 1'))
  )
  
  df<- df %>% 
    mutate(!!sym(col.resource.quantity):= ifelse(tolower(!!sym(col.activity.unit)) %in% tolower(unit_replace_with_1), 1,!!sym(col.resource.quantity)))
  
  
  result <- list(DATA = df, clean_log = merged_clean_log)
  return(result)
}


# function to remote special characters, spaces, comma, dot,etc from location name
location_name_tolower_remove_space_dots <- function(data, data.name) {
  data <- data %>%mutate_at(vars(!!sym(col.adm4Pcode), !!sym(data.name)), na_if, "")
  data <- data %>%
    mutate(name_check = str_to_lower(str_trim(str_replace_all(!!sym(data.name), "[^[:alnum:]]", ""))),
           name_check = ifelse(!is.na(!!sym(col.adm4Pcode)), !!sym(col.adm4Pcode), name_check))
  data$name_check<-stringi::stri_trans_general(data$name_check, "Latin-ASCII")
  return(data)
}


analysis_345w <- function(data, activity_unit, hrp_data, act_data, all_disaggregation, act_id_code,sector){
  adm.level.names <- list(col.adm0Name,
                          c(col.adm0Name, col.adm1Name, col.adm1Pcode),
                          c(col.adm0Name, col.adm1Name, col.adm1Pcode,col.adm2Name, col.adm2Pcode),
                          c(col.adm0Name, col.adm1Name, col.adm1Pcode,col.adm2Name, col.adm2Pcode, col.adm3Name, col.adm3Pcode))
  adm.level <- list(col.adm0Name,
                    c(col.adm0Name, col.adm1Pcode),
                    c(col.adm0Name,col.adm1Pcode,col.adm2Pcode),
                    c(col.adm0Name, col.adm1Pcode,col.adm2Pcode,col.adm3Pcode))
  
  # List of disaggregation
  # all_disaggregation <- c(all_disaggregation)
  
  check_unit<- activity_unit %>% filter(count_duplicated_location == 'yes') %>% pull(col.activity.unit)
  df<-data %>% mutate(!!col.adm0Name := 'VEN')
  
  ##### CALCULATE NUMBER OF ACTIVITY #####
  # Calculate number of total activity
  res <- df %>%
    group_by(
      across(c(adm.level[[4]], !!sym(col.location.type), name_check,!!sym(col.activity), !!sym(col.activity.id), !!sym(col.activity.unit), number_activity))
    ) %>%
    summarise(
      n_activity = n(),  # calculate number of activities including recurrent
      n_activity_hrp = sum(tolower(iconv(!!sym(col.hrp), from = "UTF-8", to = "ASCII//TRANSLIT")) == 'si')
    )
  
  
  ##### CALCULATE REACHED #####
  # Calculate reached for non recurrent activity
  # Reached is calculated at location level and based on the unit, get max (people units) or sum (other units)
  
  df_reached<- df %>%
    filter(tolower(iconv(!!sym(col.recurrent), from = "UTF-8", to = "ASCII//TRANSLIT")) != 'si') %>%
    group_by_at(vars(adm.level[[4]], !!sym(col.location.type), name_check,!!sym(col.activity.id))) %>%
    summarise(
      disagg_sum = sum(across(all_of(all_disaggregation), ~ sum(., na.rm = TRUE)), na.rm = TRUE),
      disagg_max = sum(across(all_of(all_disaggregation), ~ max(., na.rm = TRUE)), na.rm = TRUE),
      reached_sum = sum(across(!!sym(col.reached), ~ sum(., na.rm = TRUE)), na.rm = TRUE),
      reached_max = sum(across(!!sym(col.reached), ~ max(., na.rm = TRUE)), na.rm = TRUE),
      !!sym(col.resource.quantity) := ifelse(!!sym(col.activity.unit) %in% check_unit,
                                             sum(!!sym(col.resource.quantity), na.rm = TRUE),
                                             max(!!sym(col.resource.quantity), na.rm = TRUE)),.groups = 'drop') %>%
    distinct()
  
  # join reached values to the dataframe
  # print('jOIN REACHED')
  res <- res %>%
    left_join(df_reached, by = c(adm.level[[4]], col.location.type, 'name_check', col.activity.id)) %>% 
    mutate(
      !!sym(col.reached) :=
        case_when(
          !!sym(col.activity.unit) %in% check_unit & disagg_sum != 0 ~ disagg_sum,
          !!sym(col.activity.unit) %in% check_unit & disagg_sum == 0 ~ reached_sum,
          !(!!sym(col.activity.unit) %in% check_unit) & disagg_max != 0 ~ disagg_max,
          !(!!sym(col.activity.unit) %in% check_unit) & disagg_max == 0 ~ reached_max,
          
          # NUTRITION SUM ALL DISAGGREGATION ON LOCATIONS (dont take max value)
          !!sym(col.activity.unit) %in% unit_people & sector == 'NUT' ~ reached_sum,
          TRUE ~ NA_real_  # Default value if none of the conditions are met
        )
    )
  
  # set unit indicator = reached if unit is people
  res <- res %>%
    mutate(!!sym(col.resource.quantity):= ifelse(tolower(!!sym(col.activity.unit)) %in% unit_people,
                                                 !!sym(col.reached),
                                                 !!sym(col.resource.quantity)))
  
  ##### CALCULATE AGGREGATIONS BY ADM LEVEL #####
  result_adm_level_list <- lapply(1:(length(adm.level)-1), function(i){
    
    print(i)
    # print('caculate reached aggregation')
    res<-res %>%
      group_by(
        across(c(adm.level[[i]],!!sym(col.activity), !!sym(col.activity.id),!!sym(col.activity.unit), number_activity))) %>%
      summarise(
        n_activity = sum(n_activity, na.rm = TRUE),
        n_activity_hrp = sum(n_activity_hrp, na.rm = TRUE),
        # n_activity_no_recurrent = sum(n_activity_no_recurrent,na.rm = TRUE),
        !!sym(col.reached) := sum(!!sym(col.reached), na.rm = TRUE),
        !!sym(col.resource.quantity) := sum(!!sym(col.resource.quantity), na.rm = TRUE)
      )
    
    ##### CALCULATE NUMBER OF ORGANISATIONS #####
    # Calculate number of organisations and get organisations names
    # add name of organizations
    # print('caculate number of organisations and names')
    res<-res %>% 
      left_join(
        df %>% 
          select(adm.level[[i]], !!sym(col.activity.id), !!sym(col.org.impl)) %>%
          distinct() %>%
          group_by(
            across(c(adm.level[[i]], !!sym(col.activity.id)))) %>%
          summarise(n_organisation = n(), .groups = 'drop'),  # calculate number of organisations
        # by = c(adm.level[[i]], col.activity.id)
        by = c(setNames(adm.level[[i]], adm.level[[i]]), col.activity.id)
      ) %>%     
      left_join(
        df %>%
          group_by(
            across(c(adm.level[[i]], !!sym(col.activity.id)))) %>%
          summarise(
            org_impl_name = paste(unique(!!sym(col.org.impl)), collapse = ", "), # Get organisation names
            .groups = 'drop'),
        by = c(setNames(adm.level[[i]], adm.level[[i]]), col.activity.id)
      )
    
    ##### ADD HRP TARGET #####
    # addd target information
    # calcuate percentage of reached over target
    # hrp_data<- hrp_data[[sector]]
    # act_id_code<-act_code
    # act_data<-act_list
    hrp_data <- hrp_data %>%
      mutate(!!col.adm0Name := 'VEN') 
    df_hrp<- hrp_data %>%
      select(adm.level[[3]],starts_with(act_id_code),col.priorization,col.severity) %>%
      gather(key = !!sym(col.activity.id), value = 'target',-c(adm.level[[3]],col.priorization,col.severity)) %>% 
      group_by(across(c(adm.level[[i]], col.activity.id,col.priorization, col.severity))) %>% 
      summarise(target= sum(target,na.rm = TRUE), .groups = 'drop' ) %>% 
      left_join(act_data, by=col.activity.id)
    
    # adm1 and adm0 doesnt have severity and priorization
    if (i != 3) {
      # print('HRP summarise ADM0,1,2')
      df_hrp <- df_hrp %>% 
        group_by(across(c(adm.level[[i]], col.activity,col.activity.id, col.activity.unit, number_activity))) %>% 
        summarise(target= sum(target,na.rm = TRUE), .groups = 'drop' )
    }
    # add hrp target data
    # print('Join HRP to data')
    res <- res %>%
      full_join(df_hrp, by = c(adm.level[[i]],col.activity.id, col.activity.unit, 'number_activity'),suffix = c('','_2')) 
    
    # Calculate percentage
    # print('calculate percentage')
    res <- res %>%
      mutate(percentage_indicator = ifelse(!!sym(col.activity.unit) %in% unit_people,
                                           round((!!sym(col.reached)/target)*100, digits =2),
                                           round((!!sym(col.resource.quantity)/target)*100, digits =2)))
    
    
    ##### DATA ORGANISATION #####
    # clean data, replacing NA activities to their respective activity names
    # print('data organisation: add activity to NA')
    res <- res %>%
      mutate(!!sym(col.activity) := !!sym(paste0(col.activity,'_2'))) %>% 
      select(-!!sym(paste0(col.activity,'_2')))
    
    # add administrative names
    # print('data organisation: add adm names')
    res <- res %>%
      left_join(hrp_data %>% select(adm.level.names[[i]]), by = adm.level[[i]]) %>% distinct()
    
    # organize data column order for adm level 2
    if (i==3) {
      # print('data organisation: organise column adm 2')
      # adm 2 level  sorting
      # arrange data based on pcode and activity number
      res<-res%>%
        mutate(number = as.numeric(str_extract(!!sym(col.adm2Pcode), "(?<=VE0)\\d+"))) %>%
        arrange(as.numeric(number), as.numeric(number_activity))
      
      res <- res %>% 
        select(adm.level.names[[i]],col.priorization,col.severity,col.activity,col.activity.id,col.activity.unit,
               'org_impl_name',"n_organisation", "n_activity",'n_activity_hrp',col.resource.quantity, col.reached,"target","percentage_indicator")
      
    } else {
      if (i==2){
        # print('data organisation: organise column adm 1')
        # adm 1 level  sorting
        # arrange data based on pcode and activity number
        res<-res%>%
          mutate(number = as.numeric(str_extract(!!sym(col.adm1Pcode), "(?<=VE0)\\d+"))) %>%
          arrange(as.numeric(number), as.numeric(number_activity))
      } else if (i==1){
        # print('data organisation: organise column adm 0')
        # adm 0 level  sorting
        # arrange data based on activity number
        res<-res %>% arrange(as.numeric(number_activity))
      }
      # organize data column order for adm level 0 and adm level 1
      # print('data organisation: organise column adm 0 and 1')
      res <- res %>% 
        select(adm.level.names[[i]],col.activity,col.activity.id,col.activity.unit,
               'org_impl_name',"n_organisation", "n_activity",'n_activity_hrp',col.resource.quantity, col.reached,"target","percentage_indicator")
    }
    
  })
  
  
  # Assigning names to the list elements
  names(result_adm_level_list) <- c("adm0", "adm1", "adm2")
  return(result_adm_level_list)
}
