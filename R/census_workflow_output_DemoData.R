Output_DemoData <- function(census_adj, census_pop, DataSources, Template, Staff_Member, myLocID){
  
  Census_SingleAge <- census_adj[["census_pop_out"]]
  
  Census_SingleAge <- Census_SingleAge%>% 
    mutate(AgeEnd = case_when(AgeStart %in% 0:(max(AgeStart)-1) ~ AgeStart + 1,
                              AgeStart == max(AgeStart) ~ 0),
           AgeSpan = case_when(AgeStart %in% 0:(max(AgeStart)-1) ~ 1,
                               AgeStart == max(AgeStart) ~ -1),
           Sex = case_when(SexID == 1 ~ "Male",
                           SexID == 2 ~ "Female"),
           IndicatorID = 60,
           Indicator = Template$Indicator[Template$IndicatorID == 60])
  
  Abr_M <- DemoTools::single2abridged(Census_SingleAge$DataValue[Census_SingleAge$SexID == 1])
  Abr_F <- DemoTools::single2abridged(Census_SingleAge$DataValue[Census_SingleAge$SexID == 2])
  AgeStart <- names(Abr_M)
  
  Census_Abr_M <- as.data.frame(cbind(rep(1, length(Abr_M)), AgeStart, Abr_M))
  Census_Abr_F <- as.data.frame(cbind(rep(2, length(Abr_F)), AgeStart, Abr_F))
  
  names(Census_Abr_M) = names(Census_Abr_F) = c("SexID", "AgeStart", "DataValue")
  
  Census_Abr <- rbind(Census_Abr_M, Census_Abr_F)
  
  ## compute total
  Census_Total <- Census_Abr %>% as_tibble() %>% select(SexID, DataValue) %>% group_by(SexID) %>% summarise(DataValue = sum(as.numeric(DataValue))) %>% 
    mutate(SexID = as.numeric(SexID), 
           AgeStart = 0,
           AgeEnd = -1,
           AgeSpan = -1,
		   Sex = case_when(SexID == 1 ~ "Male",
                           SexID == 2 ~ "Female"),
           IndicatorID = 52,
           Indicator = Template$Indicator[Template$IndicatorID == 52])
		   
  ## compute abridged
  Census_Abr <- Census_Abr %>% as_tibble() %>%
    mutate(SexID = as.numeric(SexID), 
           AgeStart = as.numeric(AgeStart),
           AgeEnd = case_when(AgeStart == 0 ~ 1,
                              AgeStart == 1 ~ 5,
                              AgeStart == max(AgeStart) ~ 0,
                              TRUE ~ AgeStart + 5),
           AgeSpan = case_when(AgeStart == 0 ~ 1,
                               AgeStart == 1 ~ 4,
                               AgeStart == max(AgeStart) ~ -1,
                               TRUE ~ 5),
           Sex = case_when(SexID == 1 ~ "Male",
                           SexID == 2 ~ "Female"),
		   DataValue = as.numeric(DataValue),									   
           IndicatorID = 58,
           Indicator = Template$Indicator[Template$IndicatorID == 58])
		   
  ## compute < 5 aggregate
  Census_Abr_04 <- Census_Abr %>% dplyr::filter(AgeStart < 5) %>% dplyr::select(IndicatorID, Indicator, SexID, Sex, DataValue) %>% group_by(IndicatorID, Indicator, SexID, Sex) %>% summarise(DataValue = sum(as.numeric(DataValue))) %>% 
    mutate(SexID = as.numeric(SexID), 
           AgeStart = 0,
           AgeEnd = 5,
           AgeSpan = 5)

  ## compute broad age groups 
  ## get list of relevant age groups from DB
  library(DDSQLtools)  
  myDT <- NULL
  myDT <- tryCatch(
      {myDT = data.table(get_recorddataadditional(dataProcessTypeIds = c(2, 6, 9, 14),
                        startYear = 1945,
                        endYear = year(now()),
		##				dataTypeIds =  myDataTypeID,
						dataSourceShortNames = c("UIS.Stat", "IDEA"),
                        indicatorIds = 62,
                        locIds = myLocID,
                        locAreaTypeIds = 2,     ## "Whole area"
                        subGroupIds = 2,        ## "Total or All groups"
						includeUncertainty = FALSE,
						collapse_id_name = FALSE))
	  },       
      error = function(e) {myDT <- NULL})
  
  myGroups <- data.table(IndicatorID=rep(62, 9), Indicator=rep("Population by age and sex - broad age groups", 9), AgeStart=c(0,5,20,50,60,65,75,85,100), AgeSpan=c(5,15,30,15,-1,-1,-1,-1,-1), AgeEnd=c(5,20,50,65,0,0,0,0,0))
  if (!is.null(myDT)){
	myGroups <- unique(rbind(myGroups, unique(myDT[, .(IndicatorID, Indicator=IndicatorName, AgeStart, AgeSpan, AgeEnd)])))
  }
  
  Census_BroadAges <- Census_SingleAge %>% dplyr::select(SexID, Sex, Age=AgeStart, DataValue) %>% 
	merge(., myGroups, by = NULL) %>% 
	dplyr::filter((AgeSpan > 0 & Age >= AgeStart & Age < AgeEnd) | (AgeSpan == -1 & Age >= AgeStart)) %>%	
	group_by(IndicatorID, Indicator, SexID, Sex, AgeStart, AgeSpan, AgeEnd) %>% 
	summarise(DataValue = sum(as.numeric(DataValue)))
  
  Census_out <- rbind(Census_SingleAge, Census_Abr, Census_Abr_04, Census_Total, Census_BroadAges)
    
  ## compute both sexes
  Census_out_BS <- Census_out %>% group_by(IndicatorID, Indicator, AgeStart, AgeEnd, AgeSpan) %>% summarise(DataValue = sum(as.numeric(DataValue))) %>%
	mutate(SexID = 3, Sex = "Both sexes")
  
  Census_out <- rbind(Census_out, Census_out_BS)
 
  Census_out <- Census_out %>% 
    left_join(census_pop %>% 
                dplyr::select(LocID, LocName, LocTypeName, LocAreaTypeName, SubGroupName, SubGroupTypeName,
                              DataCatalogID, DataCatalogName, DataProcess, DataSourceID, DataStatusName, 
                              StatisticalConceptName, DataReliabilityName, SexID) %>% 
                dplyr::rename(Location = LocName,
                              LocType =	LocTypeName,
                              LocAreaType	= LocAreaTypeName,
                              SubGroup1	= SubGroupName,
                              SubGroupType1 =	SubGroupTypeName,
                              DataStatus =	DataStatusName,
                              StatisticalConcept =	StatisticalConceptName,
                              DataReliability =	DataReliabilityName) %>% 
                distinct(),
              by = "SexID")
  
  Census_out <- Census_out %>% 
    left_join(DataSources %>% 
                dplyr::rename(DataSourceID = PK_DataSourceID,
                              DataSourceName = Name,
                              DataSourceShortName = ShortName,
                              DataSourceYear = Year, 
                              DataSourceType = DataSourceTypeName,
                              DataSourceStatus = DataSourceStatusName,
                              DataSourceCitation = Citation,
                              DataSourceURL = URL) %>% 
                dplyr::select(DataSourceID, DataSourceName, DataSourceShortName, DataSourceAuthor, DataSourceYear, 
                              DataSourceType, DataSourceStatus, DataSourceCitation, DataSourceURL),
              by = "DataSourceID") 
  
  today <- format(Sys.Date(), "%m/%d/%Y")
  
  Output <- Template %>%
    full_join(Census_out,
              by = c("LocID",
                     "Location",
                     "LocType",
                     "LocAreaType",
                     "SubGroup1",
                     "SubGroupType1",
                     "IndicatorID",
                     "Indicator",
                     "DataCatalogID",
                     "DataCatalogName",
                     "DataProcess",
                     "DataSourceName",
                     "DataSourceAuthor",
                     "DataSourceYear",
                     "DataSourceShortName",
                     "DataSourceStatus",
                     "DataSourceCitation", 
                     "DataSourceURL",
                     "DataSourceType",
                     "DataStatus",
                     "StatisticalConcept",
                     "Sex",
                     "AgeStart",
                     "AgeEnd", 
                     "AgeSpan",
                     "DataReliability",
                     "DataValue")) %>%
    mutate(AgeUnit = AgeUnit[2],
           DataType = DataType[2],
           ModelPattern = ModelPattern[2],
           TimeUnit = TimeUnit[2],
           PeriodType = PeriodType[2],
           PeriodGroup = PeriodGroup[2],
           StaffMember = Staff_Member,
           UpdateTime = today,
           Footnote1 = as.character(Footnote1),
           Footnote2 = as.character(Footnote2),
           Footnote3 = as.character(Footnote3),
           Footnote4 = as.character(Footnote4),
           Footnote5 = as.character(Footnote5))
  
  ## recode 
  
  if(!is.null(census_adj[["pes_adjustment"]])){
    Output$Footnote1  <- "Data adjusted for under/over count by age and sex using [model-based estimates of Post-Enumeration Survey results] / [Post-Enumeration Survey results] / [demographic analysis results]"
	Output$DataStatus[Output$DataStatus=="Provisional"]         <- "Provisional, adjusted"
	Output$DataStatus[Output$DataStatus=="Final"]               <- "Final, adjusted"
	Output$DataStatus[Output$DataStatus=="Final, non-adjusted"] <- "Final, adjusted"
	Output$DataStatus[Output$DataStatus=="Unknown"]             <- "Unknown, adjusted"
  } else {
    Output$Footnote1  <- "Data NOT adjusted for under/over count by age and sex using [model-based estimates of Post-Enumeration Survey results] / [Post-Enumeration Survey results] / [demographic analysis results]"
  }
  if(census_adj[["census_input_max_age"]] < 105){
    Output$Footnote2 <- "Data for open age group extended up to age 100 using underlying life table assumption for stationary population"
  }
  if(census_adj[["best_smooth_adult"]] %in% c("bestMavN =  1", "bestMavN =  2", "bestMavN =  4", "bestMavN =  6", "bestMavN =  10") | 
     census_adj[["best_smooth_child"]] %in% c("bestMavN =  1", "bestMavN =  2", "bestMavN =  4", "bestMavN =  6", "bestMavN =  10") ){
    Output$Footnote3 <- "Data by age smoothed using moving averages based on the degree of age heaping measured by Bachi index for under age 15 and 15 and over and average number of years of education for adults"
	Output$DataStatus[Output$DataStatus=="Provisional"]         <- "Provisional, adjusted"
	Output$DataStatus[Output$DataStatus=="Final"]               <- "Final, adjusted"
	Output$DataStatus[Output$DataStatus=="Final, non-adjusted"] <- "Final, adjusted"
	Output$DataStatus[Output$DataStatus=="Unknown"]             <- "Unknown, adjusted"
  }
  if(census_adj[["best_smooth_adult"]] %in% c("bestGrad5 =  1", "bestGrad5 =  2", "bestGrad5 =  4") | 
     census_adj[["best_smooth_child"]] %in% c("bestGrad5 =  1", "bestGrad5 =  2", "bestGrad5 =  4") ){
    Output$Footnote4 <- "Data by age groups graduated into single ages using a monotonic spline on the cumulative distribution"
	Output$DataStatus[Output$DataStatus=="Provisional"]         <- "Provisional, adjusted"
	Output$DataStatus[Output$DataStatus=="Final"]               <- "Final, adjusted"
	Output$DataStatus[Output$DataStatus=="Final, non-adjusted"] <- "Final, adjusted"
	Output$DataStatus[Output$DataStatus=="Unknown"]             <- "Unknown, adjusted"
  }
  if(!is.null(census_adj[["pop_basepop"]])){
    Output$Footnote5 <- "Data adjusted by age and sex for under enumeration of children under age 15 using demographic analysis methods and recent fertility and mortality time trends"
	Output$DataStatus[Output$DataStatus=="Provisional"]         <- "Provisional, adjusted"
	Output$DataStatus[Output$DataStatus=="Final"]               <- "Final, adjusted"
	Output$DataStatus[Output$DataStatus=="Final, non-adjusted"] <- "Final, adjusted"
	Output$DataStatus[Output$DataStatus=="Unknown"]             <- "Unknown, adjusted"
  }
  
  ColumnsFinal <- names(Template)
  
  Output <- Output %>% 
    dplyr::select(all_of(ColumnsFinal)) %>% 
    dplyr::filter(!is.na(DataValue))
  
  return(Output)
}
