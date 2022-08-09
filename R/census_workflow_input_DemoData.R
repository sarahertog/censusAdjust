Input_DemoData <- function(census_adj, census_pop, DataSources, Template, Staff_Member){
  
  use_series <- census_adj[["census_input_age_structure"]] 
  myPopulation <- census_adj[["census_pop_in"]]

  if (use_series=="single"){
	 Census_SingleAge <- myPopulation
	 
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
  } else if (use_series=="abridged") {
	 Census_SingleAge <- NULL
	 Census_Abr <- myPopulation 
  } else if (use_series=="five_year") {
	 Census_SingleAge <- NULL
	 Census_5year <- myPopulation 
  }

    
  ## compute abridged
  Census_Pop5 <- NULL
  if (!use_series=="five_year"){
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
			Indicator = Template$Indicator[Template$IndicatorID == 58],
			id = unique(myPopulation$id))
	Census_Pop5 <- Census_Abr
  }

  ## compute five_year
  if (use_series=="five_year"){
	Census_5year <- Census_5year %>% as_tibble() %>%
		mutate(SexID = as.numeric(SexID), 
			AgeStart = as.numeric(AgeStart),
			AgeEnd = case_when(AgeStart == 0 ~ 5,
								AgeStart == max(AgeStart) ~ 0,
								TRUE ~ AgeStart + 5),
			AgeSpan = case_when(AgeStart == 0 ~ 5,
								AgeStart == max(AgeStart) ~ -1,
								TRUE ~ 5),
			Sex = case_when(SexID == 1 ~ "Male",
							SexID == 2 ~ "Female"),
			DataValue = as.numeric(DataValue),
			IndicatorID = 58,
			Indicator = Template$Indicator[Template$IndicatorID == 58],
			id = unique(myPopulation$id))
	Census_Pop5 <- Census_5year			
  }

  ## compute total
  Census_Total <- Census_Pop5 %>% as_tibble() %>% select(SexID, DataValue) %>% group_by(SexID) %>% summarise(DataValue = sum(as.numeric(DataValue))) %>% 
    mutate(SexID = as.numeric(SexID), 
           AgeStart = 0,
           AgeEnd = -1,
           AgeSpan = -1,
		   Sex = case_when(SexID == 1 ~ "Male",
                           SexID == 2 ~ "Female"),
           IndicatorID = 52,
           Indicator = Template$Indicator[Template$IndicatorID == 52],
		   id = unique(myPopulation$id))
  
  ## compute < 5 aggregate
  Census_Abr_04 <- NULL
  if (!use_series=="five_year"){
	Census_Abr_04 <- Census_Abr %>% dplyr::filter(AgeStart < 5) %>% dplyr::select(id, IndicatorID, Indicator, SexID, Sex, DataValue) %>% group_by(id, IndicatorID, Indicator, SexID, Sex) %>% summarise(DataValue = sum(as.numeric(DataValue))) %>% 
		mutate(SexID = as.numeric(SexID), 
			AgeStart = 0,
			AgeEnd = 5,
			AgeSpan = 5)
  }

  Census_in <- rbind(Census_SingleAge, Census_Pop5, Census_Abr_04, Census_Total)
  
  ## compute both sexes
  Census_in_BS <- Census_in %>% group_by(id, IndicatorID, Indicator, AgeStart, AgeEnd, AgeSpan) %>% summarise(DataValue = sum(as.numeric(DataValue))) %>%
	mutate(SexID = 3, Sex = "Both sexes")
  
  Census_in <- rbind(Census_in, Census_in_BS)
  
  
  Census_in <- Census_in %>% 
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
  
  Census_in <- Census_in %>% 
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
  
  Input <- Template %>%
    full_join(Census_in,
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
           DataType = "Direct (age standardized)",    ## DataTypeID=130
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
    
  ColumnsFinal <- names(Template)
  
  Input <- Input %>% 
    dplyr::select(all_of(ColumnsFinal)) %>% 
    dplyr::filter(!is.na(DataValue))
  
  return(Input)
}
