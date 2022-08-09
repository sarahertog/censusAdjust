# this function parses information from the input excel file for one country and carries out the
# Peter Johnson census adjustment and smoothing protocol


census_workflow_one_location <- function(excel_input_file_path, root_dir, mainDir, plots_dir, DDloader_dir, Staff_Member, WPP_RevID, myLocID, ISO3, LocName, isSmall, isLocal) {

  # get data source data if not available
  if(!exists("DataSources")){DataSources <- data.table(get_datasources())}
  
  # get the template used for the data to be loaded in DemoData
  Template <- data.table(read_excel(path = file.path(mainDir, "AuxFiles/GeneralDataloader_Template_CensusPop.xlsx"),
                                    sheet = "DB",
                                    col_names = T,
                                    col_types = NULL,
                                    skip = 0,
                                    na = ""))
  # load the PES results 
  if (!exists("TotNetEnum")) {load(file.path(root_dir, "GlobalFiles/TotNetEnum.rda"))}
  if (!exists("DiffNCE1")) {load(file.path(root_dir, "GlobalFiles/DiffNCE1.rda"))}
  if (!exists("DiffNCEAbr")) {load(file.path(root_dir, "GlobalFiles/DiffNCEAbr.rda"))}
  
  # load Covariates
  if (!exists("Covariates")) {load(file.path(root_dir,"GlobalFiles/Covariates.rda"))}

  # read metadata from parameters sheet of excel input file
  meta <-   readxl::read_xlsx(path = excel_input_file_path,
                              sheet = "parameters")
  
  meta <- meta %>% 
    dplyr::select(parameter, value) %>% 
    dplyr::filter(!is.na(parameter))
  
  meta.list <- list()
  for (i in 1:nrow(meta)) {
    meta.list[[i]] <- ifelse(!is.na(suppressWarnings(as.numeric(meta$value[i]))), as.numeric(meta$value[i]), meta$value[i])
    names(meta.list)[i] <- gsub(" ", "_", meta$parameter[i])
  }
  ## rm(meta)
  
  # parse locid
  locid <- as.numeric(meta.list$LocationID)
  
  # we allow some substitute locids for locations that might not have covariate or PES data
  locid_EduYrs <- ifelse(!is.na(meta.list$AltLocationID_Covariates), as.numeric(meta.list$AltLocationID_Covariates), locid)
  locid_PES <- ifelse(!is.na(meta.list$AltLocationID_PES), as.numeric(meta.list$AltLocationID_PES), locid)
  
  # Here analysts can require using abridged data even if complete are available
  pop_structure <- meta.list$Age_specific_input_Population_data
  
  # check whether there is a harmonized census file available for this country
  harmonized_file <- paste0(mainDir, "/data/harmonized/",locid,"_census_harmonized.rda")
  has_harmonized <- file.exists(harmonized_file)
  
 	  
  has_run_DD_CensusPop_harmonization <- 0
  has_harmonized_date <- NA
  if (has_harmonized) {
    has_harmonized_date <- format(file.info(harmonized_file)$ctime, format="%Y-%m-%d")
  if (!is.null(meta.list$run_DD_CensusPop_harmonization)) {
	if (!is.na(meta.list$run_DD_CensusPop_harmonization)) {
		has_run_DD_CensusPop_harmonization <- 1
		if (meta.list$run_DD_CensusPop_harmonization=="TRUE" | is.na(has_harmonized_date) | (!is.na(has_harmonized_date) & has_harmonized_date < "2022-01-11")) {		
			has_run_DD_CensusPop_harmonization <- 2
		}
	}
  }
  }
  
  # initialize the adjusted census file
  census_adj_all <- NULL
  
  if (!has_harmonized | has_run_DD_CensusPop_harmonization %in% c(0,2)) {
  
	if (!isLocal){
		## connect to SQL server and insert SeriesID/SeriesKeys for uploaded records
		## if (!require(RODBC)) install.packages('RODBC')
		## library(RODBC)
		
		# connect to database to cleanup existing records
		cn <- odbcDriverConnect('driver={SQL Server}; server=dfs-desapsql-52.dpko.un.org; database=DemodataOps; uid=PublicDataWebUser; pwd=PublicDataWebUser')
		myIndicatorID <- "52, 58, 60, 62"
		myDataTypeID  <- "130, 181"
	
		# run the stored procedure
		sqlQuery(cn, paste0("EXEC spDDDeletePopulationData '", as.character(myLocID), "', '", myIndicatorID, "', '", myDataTypeID, "'"), errors=TRUE)	
		odbcCloseAll()
	}


	## call ddHarmony to download harmonized census data
	ddharmony_census_harmonized(mainDir, myLocID)
  
	# check whether there is a harmonized census file available for this country
	has_harmonized <- file.exists(paste0(mainDir, "/data/harmonized/",locid,"_census_harmonized.rda"))
	
	if (has_harmonized){
		parameters <- data.table(readxl::read_xlsx(path = excel_input_file_path, sheet = "parameters"))
	
		## new set of parameters (sorted)
		myparameters <- data.table(parameter=c(
		  "Location",
		  "LocationID",
		  "isSmall",
		  "Age_specific_input_Population_data",
		  "Age_specific_input_VR_data",
		  "AltLocationID_InputData",
		  "AltLocationID_Covariates",
		  "AltLocationID_PES",
		  "run_DD_CensusPop_harmonization",
		  "Intercensal_gap_maximum",
		  "Estimates_First_Year",
		  "Base_Year",
		  "base_population_source",
		  "adjust_basepop_1950",
		  "adjust_basepop_1950_maxage",
		  "adjust_basepop_1950_scaling_factor",
		  "adjust_basepop_1950_scaling_minage",
		  "adjust_basepop_1950_scaling_maxage",
		  "Projection_First_Year",
		  "Projection_Last_Year",
		  "Type_of_Projection",
		  "Is_Estimation_Final",
		  "Is_Projection_Final"), 
		  SortOrderKey = 1:23)
		
		## merge and inputs default if applicable
		parameters <- merge(myparameters, parameters, by="parameter", all.x=TRUE, all.y=FALSE)
		setorder(parameters, SortOrderKey)
		
		parameters[parameter=="Location", value := LocName]
		parameters[parameter=="run_DD_CensusPop_harmonization", value := "FALSE"]
		parameters[parameter=="base_population_source" & is.na(value), value := "WPP19"]
		parameters[parameter=="base_population_source" & !(tolower(value) %in% c("wpp19","wpp21","census")), value := "WPP19"]
		parameters[parameter=="adjust_basepop_1950" & is.na(value), value := "TRUE"]
		parameters[parameter=="adjust_basepop_1950_maxage" & is.na(value), value := 0]
		parameters[parameter=="adjust_basepop_1950_scaling_factor" & is.na(value), value := 1]
		parameters[parameter=="adjust_basepop_1950_scaling_minage" & is.na(value), value := 0]
		parameters[parameter=="adjust_basepop_1950_scaling_maxage" & is.na(value), value := 130]
		
		parameters$SortOrderKey <- NULL
		parameters <- unique(parameters)
	
		wb <- openxlsx::loadWorkbook(excel_input_file_path)
		openxlsx::deleteData(wb, sheet = "parameters", cols = 1:10, rows = 2:31, gridExpand = TRUE)
		openxlsx::removeComment(wb, sheet = "parameters", cols = 2:30, rows = 2:31, gridExpand = TRUE)
		openxlsx::modifyBaseFont(wb, fontSize = 11, fontColour = "#366092", fontName = "Calibri")
		openxlsx::writeData(wb, sheet = "parameters", x = parameters, startCol = 1, startRow = 2, colNames = FALSE, rowNames=FALSE, borders = "rows", borderStyle = "medium")
	
		## style for comments
		s1 <- createStyle(fontSize = 12, fontColour = "red", textDecoration = c("BOLD"))
		s2 <- createStyle(fontSize = 10, fontColour = "black", textDecoration = c("BOLD"))

		c1 <- createComment(comment = c("Required: ", "Either TRUE or FALSE (Default=FALSE for regular location) except for smaller locations which with limited disaggregated data."), style = c(s1, s2), visible = FALSE, width = 4, height = 4)
		writeComment(wb, "parameters", col = "B", row = 4, comment = c1)
		c1 <- createComment(comment = c("Required: ", "'Complete' or 'Abridged' age distribution used as default input for population (single age data are used by default upon availability, evaluated and smoothed if necessary, otherwise abridged data are used and evaluated or smoothed, and graduated)."), style = c(s1, s2), visible = FALSE, width = 5, height = 5)
		writeComment(wb, "parameters", col = "B", row = 5, comment = c1)
		c1 <- createComment(comment = c("Required: ", "'Complete' or 'Abridged' age distribution used as default input for vital rates (upon data availability)."), style = c(s1, s2), visible = FALSE, width = 4, height = 4)
		writeComment(wb, "parameters", col = "B", row = 6, comment = c1)
		c1 <- createComment(comment = c("Optional: ", "Alternative LocationID (one or more codes comma separated) to be used as additional or substitute location(s) for situations with no data (e.g., use vital rates from Switzerland for Liechtenstein)."), style = c(s1, s2), visible = FALSE, width = 4, height = 5)
		writeComment(wb, "parameters", col = "B", row = 7, comment = c1)
		c1 <- createComment(comment = c("Optional: ", "Alternative LocationID (one or more codes comma separated) to be used as additional or substitute location(s) for covariates)."), style = c(s1, s2), visible = FALSE, width = 4, height = 5)
		writeComment(wb, "parameters", col = "B", row = 8, comment = c1)
		c1 <- createComment(comment = c("Optional: ", "Alternative LocationID (one or more codes comma separated) to be used as additional or substitute location(s) for PES model-based estimates)."), style = c(s1, s2), visible = FALSE, width = 4, height = 4)
		writeComment(wb, "parameters", col = "B", row = 9, comment = c1)
		c1 <- createComment(comment = c("Required: ", "Download and harmonize census population by age/sex from DemoData, and create an harmonized version (initial default=TRUE). Upon initial download, switched to FALSE to speed up rerun of 'population' evaluation and adjustments. Swicth back to TRUE to force new download if input census data were added or cleaned-up from the DB."), style = c(s1, s2), visible = FALSE, width = 5, height = 6)
		writeComment(wb, "parameters", col = "B", row = 10, comment = c1)
		c1 <- createComment(comment = c("Required: ", "Maximum number of years between pairs of censuses used to compute intercensal estimates (used for population censuses and migrations)."), style = c(s1, s2), visible = FALSE, width = 4, height = 4)
		writeComment(wb, "parameters", col = "B", row = 11, comment = c1)
		
		c1 <- createComment(comment = c("Required: ", "Source of data and estimation method used to derive the 1950 base population. Use 'WPP19' for WPP 2019 revision 1950 population. Use 'Census' for back projection of the earliest census population (protocol adjusted) available between 1950 and 1950+Intercensal_gap_maximum.  Use 'WPP21' if you want to use your own user-defined version as entered in pop_count_age_sex_base worksheet."), style = c(s1, s2), visible = FALSE, width = 5, height = 6)
		writeComment(wb, "parameters", col = "B", row = 14, comment = c1)		
		c1 <- createComment(comment = c("Required: ", "Adjust the 1950 base population to insure that the number of children on 1 Jan 1950 is consistent with the updated ASFR, SRB and life table values in the country input file (default=TRUE, otherwise use FALSE to disable adjustment)."), style = c(s1, s2), visible = FALSE, width = 4, height = 5)
		writeComment(wb, "parameters", col = "B", row = 15, comment = c1)
		c1 <- createComment(comment = c("Required: ", "Max. age of basepop adjustment to 1950 population (e.g., use 0 to adjust infants only; use 4 to adjust ages 0 thru 4; use 9 to adjust ages 0 thru 9) if 'adjust_basepop_1950'=TRUE. "), style = c(s1, s2), visible = FALSE, width = 4, height = 4)
		writeComment(wb, "parameters", col = "B", row = 16, comment = c1)	
		c1 <- createComment(comment = c("Required: ", "Scaling factor to be applied to 1950 base year population. 1 = no adjustment"), style = c(s1, s2), visible = FALSE, width = 4, height = 4)
		writeComment(wb, "parameters", col = "B", row = 17, comment = c1)	
		c1 <- createComment(comment = c("Required: ", "Minimum age to which base population scaling factor will be applied."), style = c(s1, s2), visible = FALSE, width = 4, height = 4)
		writeComment(wb, "parameters", col = "B", row = 18, comment = c1)	
		c1 <- createComment(comment = c("Required: ", "Maximum age to which base population scaling factor will be applied."), style = c(s1, s2), visible = FALSE, width = 4, height = 4)
		writeComment(wb, "parameters", col = "B", row = 19, comment = c1)	
		
		c1 <- createComment(comment = c("Required: ", "0 or 1 dummy value (default=0 until analyst finalizes estimates)."), style = c(s1, s2), visible = FALSE, width = 4, height = 4)
		writeComment(wb, "parameters", col = "B", row = 22, comment = c1)
		c1 <- createComment(comment = c("Required: ", "0 or 1 dummy value (default=0 until global run are finalized)."), style = c(s1, s2), visible = FALSE, width = 4, height = 4)
		writeComment(wb, "parameters", col = "B", row = 23, comment = c1)
		
		update_status <- data.table(readxl::read_xlsx(path = excel_input_file_path, sheet = "update_status"))
		changeCols <- colnames(update_status)[which(as.vector(update_status[,lapply(.SD, class)]) %in% c("logical", "numeric"))]
		update_status[,(changeCols):= lapply(.SD, as.character), .SDcols = changeCols]	
		now <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
		update_status[worksheet %in% c("parameters"), last_update := now]
		
		writeData(wb, sheet = "update_status", update_status$last_update, startCol=3, startRow=2, colNames = FALSE, rowNames=FALSE)
		saveWorkbook(wb, excel_input_file_path, overwrite = T)
	}

  }
  
  
  if (has_harmonized) {
    
	## read input data file that defines which censuses to use
	## and update contents based on DataCatalog & shortnotes Used selection
	census_used_write_to_input_excel_file(excel_input_file_path, WPP_RevID, myLocID, isSmall)
	
    # load input data file that defines which censuses to use
    pop_inputs <-   readxl::read_xlsx(path = excel_input_file_path, sheet = "POP_INPUTS") %>% 
      dplyr::filter(!is.na(DataCatalogID))

    pop_inputs <- pop_inputs %>% dplyr::filter(Include == TRUE)
    
    # if there are any censuses to be evaluated, do that here
    if (nrow(pop_inputs) > 0) {
      
      # load the harmonized census data
      load(paste0(mainDir, "/data/harmonized/", locid, "_census_harmonized.rda"))
      census_all <- pop %>% dplyr::filter(non_standard == FALSE & AgeStart >= 0) # added this to get rid of some -2 age starts for Bolivia IPUMS?
      rm(pop)
      
      # for small countries, use only five-year data
      if (tolower(pop_structure) != "complete") {
        census_all <- census_all %>% dplyr::filter(five_year == TRUE) 
      }
      
      # import life tables from excel input file
      life_table_age_sex <-   readxl::read_xlsx(path = excel_input_file_path, sheet = "life_table_age_sex") 
      # import age specific fertility rates from excel input file
      fert_rate_age_f <-   readxl::read_xlsx(path = excel_input_file_path, sheet = "fert_rate_age_f") 
      # import sex ratios at birth from excel input file
      srb <-   readxl::read_xlsx(path = excel_input_file_path, sheet = "srb") 
      
      census_adj_all <- list()
      for (i in 1:nrow(pop_inputs)) {
        
        census_pop <- census_all %>% dplyr::filter(DataCatalogID == pop_inputs$DataCatalogID[i])
        
        if (nrow(census_pop) > 0) {
        
        # multiply census population by coverage scaling factor
        adjust_coverage <- pop_inputs$adjust_coverage[i]
        census_pop$DataValue <- census_pop$DataValue * adjust_coverage
        
        # parse controls for additional adjustments and smoothing
        adjust_pes <- pop_inputs$adjust_pes[i]
        pes_adjustment <- as.numeric(pop_inputs$pes_adjustment[i])
		if (is.na(pes_adjustment)) { adjust_pes <- FALSE }												  

        adjust_smooth <- pop_inputs$adjust_smooth[i]
        adjust_basepop <- pop_inputs$adjust_basepop[i]
        
        census_reference_date <- as.numeric(census_pop$TimeMid[1])
        census_reference_year <- ifelse(census_reference_date >= 1950, floor(census_reference_date), 1950)
        
        # get lx for OPAG
        lxMale <- life_table_age_sex$value[life_table_age_sex$indicator == "lt_lx" &
                                            life_table_age_sex$time_start == census_reference_year &
                                            life_table_age_sex$sex == "male"]
        
        lxFemale <- life_table_age_sex$value[life_table_age_sex$indicator == "lt_lx" &
                                             life_table_age_sex$time_start == census_reference_year &
                                             life_table_age_sex$sex == "female"]
        
        Age_lx <- 1:length(lxMale)-1
        
        # if now life table values in the input file, then set these to NULL. DemoTools functions will then pull values from WPP19
        if (is.na(lxMale[1]) | is.na(lxFemale[1])) {
          
          lxMale <- NULL
          lxFemale <- NULL
          Age_lx <- NULL
          
        }
        

        # If adjust_basepop == TRUE, then get the inputs needed for that
        if (adjust_basepop) {
          
          nLxDatesIn <- census_reference_date - c(0.5, 2.5, 7.5)
        
          parse_columns <- ifelse(nLxDatesIn < 1950, 1950, nLxDatesIn)
        
          # parse nLx for males and females transform into the matrices needed for basepop
          
          nLxMatMale <- life_table_age_sex %>% 
            dplyr::filter(indicator == "lt_nMx" & sex == "male") 
          
          nLxMatFemale <- life_table_age_sex %>% 
            dplyr::filter(indicator == "lt_nMx" & sex == "female") 
          
          if (!is.na(nLxMatMale$value[1]) & !is.na(nLxMatFemale$value[1])) {
            
            nLxMatMale <- nLxMatMale %>% 
              pivot_wider(names_from = "time_start", values_from = "value") %>% 
              dplyr::select(-indicator, -time_span, -sex, -age_start, -age_span) %>% 
              DemoTools::interp(datesIn = seq(min(life_table_age_sex$time_start), max(life_table_age_sex$time_start), 1),
                                datesOut = nLxDatesIn, extrap = TRUE, method = "linear") %>%  # interpolate to 0.5, 2.5 and 7.5 years prior to the census
              as.data.frame() %>% 
              apply(MARGIN = 2, FUN = function(S) {DemoTools::lt_single_mx(nMx = S, Age = 1:length(S)-1, radix = 100000, Sex = "m", OAnew = 100)$nLx}) %>% # compute nLx single
              apply(MARGIN = 2, FUN = function(S) {DemoTools::single2abridged(Age = S)}) %>% # group to abridged
              as.matrix()
            
            nLxMatFemale <- nLxMatFemale %>% 
              pivot_wider(names_from = "time_start", values_from = "value") %>% 
              dplyr::select(-indicator, -time_span, -sex, -age_start, -age_span) %>% 
              DemoTools::interp(datesIn = seq(min(life_table_age_sex$time_start), max(life_table_age_sex$time_start), 1),
                                datesOut = nLxDatesIn, extrap = TRUE, method = "linear") %>%  # interpolate to 0.5, 2.5 and 7.5 years prior to the census
              as.data.frame() %>% 
              apply(MARGIN = 2, FUN = function(S) {DemoTools::lt_single_mx(nMx = S, Age = 1:length(S)-1, radix = 100000, Sex = "f", OAnew = 100)$nLx}) %>% # compute nLx single
              apply(MARGIN = 2, FUN = function(S) {DemoTools::single2abridged(Age = S)}) %>% # group to abridged
              as.matrix()
            
            colnames(nLxMatMale) <- nLxDatesIn
            rownames(nLxMatMale) <- c(0,1,seq(5,100,5))
            colnames(nLxMatFemale) <- nLxDatesIn
            rownames(nLxMatFemale) <- c(0,1,seq(5,100,5))
            nLxMatDatesIn <- nLxDatesIn
            radix <- life_table_age_sex$value[life_table_age_sex$indicator=="lt_lx" & 
                                                life_table_age_sex$age_start == 0][1]
            
          } else { # if no values on input excel file, then set to NULL.  DemoTools will pull values from WPP19
            
            nLxMatMale <- NULL
            nLxMatFemale <- NULL
            nLxMatDatesIn <- NULL
            radix <- NULL
            
          }
          
          # parse ASFR and transform into the matrix needed for basepop
          
          AsfrMat <- fert_rate_age_f[, c("time_start", "age_start", "value")] 
          
          if (!is.na(fert_rate_age_f$value[1])) {
            
            AsfrMat <- AsfrMat %>% 
              mutate(age5 = 5 * floor(age_start/5)) %>% 
              group_by(time_start, age5) %>% 
              summarise(asfr = mean(value)) %>% 
              dplyr::filter(age5 >=15 & age5 <= 45) %>% 
              pivot_wider(names_from = "time_start", values_from = "asfr") %>% 
              dplyr::select(-age5) %>% 
              DemoTools::interp(datesIn = seq(min(fert_rate_age_f$time_start), max(fert_rate_age_f$time_start), 1),
                                datesOut = nLxDatesIn, extrap = TRUE, method = "linear") %>%  # interpolate to 0.5, 2.5 and 7.5 years prior to the census
              as.matrix()
            
            
            colnames(AsfrMat) <- nLxDatesIn
            rownames(AsfrMat) <- seq(15,45,5)
            AsfrDatesIn <- nLxDatesIn 
            
          } else {
            
            AsfrMat <- NULL
            AsfrDatesIn <- NULL
            
          }
          
          # get SRB
          SRBDatesIn <- floor(census_reference_date - c(0.5, 2.5, 7.5))
          
          parse_columns <- ifelse(SRBDatesIn < 1950, 1950, SRBDatesIn)
          
          SRB <- NULL
          for (k in 1:length(parse_columns)) {
            SRB <- c(SRB, srb$value[srb$time_start == parse_columns[k]])
          }
          SRBDatesIn <- SRBDatesIn + 0.5
        
        } else { # if no basepop adjustment, set these inputs as NULL
          
          nLxMatMale = NULL
          nLxMatFemale = NULL
          nLxMatDatesIn = NULL
          AsfrMat = NULL
          AsfrDatesIn = NULL
          SRB = NULL
          SRBDatesIn = NULL
          
        }
        
        if (any(adjust_pes, adjust_smooth)) {
        # Inputs needed for PES adjustment and smoothing
        
          # Average years of Education at census reference date
          EduYrs_m <- dplyr::filter(Covariates,LocID==locid_EduYrs, Year==census_reference_year)$EducYrsM
          EduYrs_f <- dplyr::filter(Covariates,LocID==locid_EduYrs, Year==census_reference_year)$EducYrsF
          EduYrs   <- min(EduYrs_m, EduYrs_f)
          
          # parse total adjustment factor from models
          PES_factor_model <- dplyr::filter(NCE, LocID == locid_PES, Year == census_reference_year)$NetEnum
          PES_factor_Total <- ifelse(!is.na(pes_adjustment), pes_adjustment, PES_factor_model) # if user-specified PES is available, use that one instead of model result

          # parse sex and age-specific adjustment factors for single year of age
          PES_factor_single_Male    <- dplyr::filter(DiffNCE1, LocID == locid_PES, Year == census_reference_year)$NetEnum_M_Diff
          PES_factor_single_Female  <- dplyr::filter(DiffNCE1, LocID == locid_PES, Year == census_reference_year)$NetEnum_F_Diff
          
          # adjust age-specific adjustment for difference between model result and analyst-specified total adjustment
          
          # parse sex and age-specific adjustment factors for abridged age groups
          PES_factor_abridged_Male     <- dplyr::filter(DiffNCEAbr, LocID == locid_PES, Year == census_reference_year)$NetEnum_M_Diff
          PES_factor_abridged_Female     <- dplyr::filter(DiffNCEAbr, LocID == locid_PES, Year == census_reference_year)$NetEnum_F_Diff
          
        } else { # if no pes adjustment or smoothing, then set these inputs as NULL
          
          EduYrs = NULL
          PES_factor_Total = NULL
          PES_factor_single_Male = NULL
          PES_factor_single_Female = NULL
          PES_factor_abridged_Male = NULL
          PES_factor_abridged_Female = NULL
          
        }
        
        
        census_adj <- census_workflow_for_one_census(dd_census_extract = census_pop, # a census extract returned by DDharmonize_validate_PopCounts()
                                                     LocID = locid,
                                                     locid_DemoTools = locid,
                                                     census_reference_date = census_reference_date, # decimal year
                                                     adjust_pes = adjust_pes, # should census be adjusted based on pes models?
                                                     adjust_smooth = adjust_smooth, # should census be smoothed according to age heaping assessment?
                                                     adjust_basepop = adjust_basepop, # should child counts be adjusted per basepop analysis?
                                                     lxMale = lxMale, # single or abridged male lx at census reference year. if NULL then will use DemoToolsData
                                                     lxFemale = lxFemale, # single or abridged female lx at census reference year.
                                                     Age_lx = Age_lx, # single or abridged
                                                     nLxMatMale = nLxMatMale, # matrix of abridged nLx for males. if NULL then will use DemoToolsData
                                                     nLxMatFemale = nLxMatFemale, # abridged
                                                     nLxMatDatesIn = nLxMatDatesIn,
                                                     AsfrMat = AsfrMat, # 5-year age groups from 15 to 45 only
                                                     AsfrDatesIn = AsfrDatesIn,
                                                     SRB = SRB,
                                                     SRBDatesIn = SRBDatesIn,
                                                     radix = radix,
                                                     EduYrs = EduYrs,
                                                     PES_factor_Total = PES_factor_Total,
                                                     PES_factor_single_Male = PES_factor_single_Male,
                                                     PES_factor_single_Female = PES_factor_single_Female,
                                                     PES_factor_abridged_Male = PES_factor_abridged_Male,
                                                     PES_factor_abridged_Female = PES_factor_abridged_Female)
        
        # create a new item with the dataset that will be loaded in DemoData
       census_adj[["census_pop_in_DD"]]  <-  Input_DemoData(census_adj, census_pop, DataSources, Template, Staff_Member)
       census_adj[["census_pop_out_DD"]] <-  Output_DemoData(census_adj, census_pop, DataSources, Template, Staff_Member, myLocID)
        
        } else {
          census_adj <- NULL
        }

        census_adj_all[[i]] <- census_adj
      
        } # close loop for census
      
    } # close for if(nrow(pop_inputs) > 0)
    
  } else { # close for if harmonized census data are available
    
    print(paste0("There are no harmonized census data available for ",meta.list$LocationName," (Locid = ", meta.list$LocationID,")."))
 	log_print(paste0("There are no harmonized census data available for ",meta.list$LocationName," (Locid = ", meta.list$LocationID,")."), msg = TRUE, console=FALSE)
  }
  
  census_adj_all[sapply(census_adj_all, is.null)] <- NULL # remove null for unprocessed censuses
  
  # save the results in one country file, list with one object per census
  if (!is.null(census_adj_all)){
	save(census_adj_all, file = file.path(mainDir, "/data/adjusted/", paste0(locid, "_census_adjusted.rda")))
	## load(file.path(mainDir, "/data/adjusted/", paste0(locid, "_census_adjusted.rda")))
	
	
	# write the adjustment information to the input excel file
	census_adj_write_to_input_excel_file(excel_input_file_path, census_adj_all)
	
	# write the total population figures to input excel file
	census_totals_write_to_input_excel_file(excel_input_file_path, census_adj_all)
	
	# loop through the items to get all the datasets in the format for DemoData
	In_DD <- lapply(1:length(census_adj_all), function(i){
		return(census_adj_all[[i]][["census_pop_in_DD"]])
	})
	
	# create the final input for DemoData 
	In_DD <- do.call(rbind,In_DD)
	# save the input for DemoData
	fwrite(In_DD, 
			file = paste0(DDloader_dir, "/data/CensusPop_Input_DemoData_", locid, ".csv"), 
			append = FALSE,
			col.names = TRUE,
			scipen = 100)
			
	## repeat for outputs
	Out_DD <- lapply(1:length(census_adj_all), function(i){
		return(census_adj_all[[i]][["census_pop_out_DD"]])
	})
	
	# create the final output for DemoData 
	Out_DD <- do.call(rbind, Out_DD)
	# save the output for DemoData
	fwrite(Out_DD, 
			file = paste0(DDloader_dir, "/data/CensusPop_Output_DemoData_", locid, ".csv"), 
			append = FALSE,
			col.names = TRUE,
			scipen = 100)
	
	# produce some plots
	if (length(census_adj_all) >= 1) {
			
		census_workflow_plots(ISO3 = ISO3,
							census_adjusted_output = census_adj_all,
							plots_dir = plots_dir)
	  
	  census_cohort_plots(ISO3 = ISO3,
	                        census_adjusted_output = census_adj_all,
	                        plots_dir = plots_dir)
	}
  }
  
} # close function
