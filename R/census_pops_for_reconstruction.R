

# R script that extracts population by age and sex to input to Mark's population reconstruction
# Returns a list of data frames with population counts by single year of age and sex
# pop_cen is adjusted/smoothed/graduated census populations from the Peter Johnson methods protocol
#     open age is always 105+ and time_start refers to the census reference date
# pop_hmd is the 1x1 January 1 population from HMD
#     open age is always 110+
# pop_eur is the EuroStat population by single year of age and sex
#     open age varies


## EXAMPLES:
# root_dir <- "C:/Users/SARAH/OneDrive - United Nations/WPP2021/Census/"  # This is the Sharepoint folder for WPP2021
# 
# locid = 380
# italy <- census_pops_for_reconstruction(locid = locid,
#                                         times = 1945:2021,
#                                         census_adj_file_path = paste0(root_dir,"data/adjusted/",locid,"_census_adjusted.rda"),
#                                         getHMD = TRUE,
#                                         data_source_year_hmd = 2021,
#                                         getEuroStat = TRUE,
#                                         data_source_year_eurostat = 2020)
# 
# locid = 840
# usa <- census_pops_for_reconstruction(locid = locid,
#                                         times = 1945:2021,
#                                         census_adj_file_path = paste0(root_dir,"data/adjusted/",locid,"_census_adjusted.rda"),
#                                         getHMD = TRUE,
#                                         data_source_year_hmd = 2021,
#                                         getEuroStat = TRUE,
#                                         data_source_year_eurostat = 2020)

library(tidyverse)

census_pops_for_reconstruction <- function(locid,
                                           times = 1945:year(today()), # range of years to extract data
                                           census_adj_file_path, # file path of output data from census adjustment protocol
                                           getHMD = FALSE, # should we extract HMD populations from DemoData?
                                           data_source_year_hmd = 2021, # data source year for HMD
                                           getEuroStat = FALSE, # should we extract EuroStat populations from DemoData?
                                           data_source_year_eurostat = 2020)  { # data source year for EuroStat
  
  # extract the final output adjusted census population counts
  load(census_adj_file_path)
  cen_dts <- lapply(census_adj_all, "[[", "census_reference_date")
  cen_pop <- lapply(census_adj_all, "[[", "census_pop_out")
  
  # append the census reference date to the population data
  for (i in 1:length(cen_dts)) {
    cen_pop[[i]]$census_reference_date <- cen_dts[[i]]
  }
  pop_cen <- do.call(rbind, cen_pop) %>% 
    arrange(census_reference_date, SexID, AgeStart) %>% 
    dplyr::filter(census_reference_date >= times[1]) %>% 
    mutate(data_source = "census_adjusted",
           sex = ifelse(SexID ==1, "male", "female"),
           time_start = census_reference_date,
           time_span = 0,
           age_start = AgeStart,
           age_span = 1,
           age_span = replace(age_span, age_start == max(age_start), 1000),
           value = DataValue) %>% 
    dplyr::select(data_source, time_start, time_span, sex, age_start, age_span, value)
  
  if (getHMD) {
    
    tryCatch({
      
      pop_hmd <- DDSQLtools::get_recorddata(locIds = locid,
                                            dataProcessIds = 6, # Estimates
                                            indicatorIds = 60, # Population by single year of age and sex
                                            startYear = times[1],
                                            endYear = times[length(times)] + 1,
                                            locAreaTypeIds = 2,
                                            subGroupIds = 2,
                                            dataSourceShortNames = "HMD",
                                            dataSourceYears = data_source_year_hmd)
      
    }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})
    
    if (exists('pop_hmd')) {
      
      pop_hmd <- pop_hmd %>%
        dplyr::filter(SexID %in% c(1,2) & !(AgeStart == 0 & AgeSpan == -1)) %>%
        dplyr::mutate(data_source = DataSourceShortName,
                      time_start = floor(TimeMid),
                      time_span = 0,
                      age_start = AgeStart,
                      age_span = AgeSpan,
                      sex = ifelse(SexID == 1, "male", "female"),
                      value = DataValue) %>%
        dplyr::select(data_source, time_start, time_span, age_start, age_span, sex, value) %>%
        dplyr::arrange(data_source, time_start, sex, age_start)
      
     
    } else {
      
      pop_hmd <- NULL
      print("No HMD population by single year of age and sex available for selected times and data source year.")
      
    }
  } else {
    pop_hmd <- NULL
  }
  
  if (getEuroStat) {
    
    tryCatch({
      
      pop_eur <- DDSQLtools::get_recorddata(locIds = locid,
                                            dataProcessIds = c(6,10), # Estimates and registers
                                            indicatorIds = 60, # Population by single year of age and sex
                                            startYear = times[1],
                                            endYear = times[length(times)] + 1,
                                            locAreaTypeIds = 2,
                                            subGroupIds = 2,
                                            dataSourceShortNames = "EuroStat",
                                            dataSourceYears = data_source_year_eurostat)
      
    }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})
    
    if (exists('pop_eur')) {
      
      # keep only series with open age group 100+
      pop_eur <- pop_eur %>%
        dplyr::filter(SexID %in% c(1,2) & !(AgeStart == 0 & AgeSpan == -1) & AgeStart != -2) %>%
        dplyr::mutate(data_source = DataSourceShortName,
                      time_start = floor(TimeMid),
                      time_span = 0,
                      age_start = AgeStart,
                      age_span = AgeSpan,
                      sex = ifelse(SexID == 1, "male", "female"),
                      value = DataValue) %>%
        dplyr::select(data_source, time_start, time_span, age_start, age_span, sex, value) %>%
        dplyr::arrange(data_source, time_start, sex, age_start) %>% 
        group_by(time_start) %>% 
        mutate(age_span = replace(age_span, age_start == max(age_start),1000)) %>% 
        ungroup()
      pop_eur <- as.data.frame(pop_eur)
      

    } else {
      
      pop_eur <- NULL
      print("No EuroStat population by single year of age and sex available for selected times and data source year.")
      
    }
  } else {
    pop_eur <- NULL
  }
  
  pop_count_age_sex <- list(pop_cen = pop_cen,
                            pop_hmd = pop_hmd,
                            pop_eur = pop_eur)
  
  return(pop_count_age_sex)
  
  
}

