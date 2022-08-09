

# this function computes the total population at each of the census adjustment steps and
# writes that info back to the total_pop sheet of the input excel file
# it also writes total population information from other data sources if the sheet
# has not already been populated with those data

census_totals_write_to_input_excel_file <- function(excel_input_file_path, census_adj_all) {

  hs1 <- createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")
  	
  editStyleHeader <- createStyle(halign = "CENTER", textDecoration = "Bold", border = "LeftRight", fontColour = "#FF0000", bgFill = "#FFFF00")
  editStyleColumn <- createStyle(halign = "CENTER", textDecoration = "Bold", border = "LeftRight", fontColour = "#FF0000")
  editStyleColumnBold <- createStyle(halign = "CENTER", textDecoration = "Bold", border = "LeftRight")
  
  ## style for comments
  s1 <- createStyle(fontSize = 12, fontColour = "red", textDecoration = c("BOLD"))
  s2 <- createStyle(fontSize = 10, fontColour = "black", textDecoration = c("BOLD"))
  
  fm0 <- createStyle(numFmt = "0")
  fm1 <- createStyle(numFmt = "0.0")
  fm2 <- createStyle(numFmt = "0.00")
  fm3 <- createStyle(numFmt = "0.000")
  fm5 <- createStyle(numFmt = "0.00000")
  
  cen_dates <- lapply(census_adj_all, "[[", "census_reference_date")
  
  # get totals from raw (harmonized) input 
  pops <- lapply(census_adj_all, "[[", "census_pop_in")
  for (i in 1:length(pops)) {
    pops[[i]]$census_reference_date <- cen_dates[[i]]
  }
  pops_raw <- do.call(rbind, pops) %>% 
    dplyr::select(census_reference_date, DataValue) %>% 
    dplyr::filter(!(is.na(DataValue))) %>% # remove data from any census years that do not have these values
    group_by(census_reference_date) %>% 
    summarise(value = sum(DataValue))
  
  # get totals from adjusted
  pops <- lapply(census_adj_all, "[[", "pop_adjusted")
  for (i in 1:length(pops)) {
    pops[[i]]$census_reference_date <- cen_dates[[i]]
  }
  pops <- rbindlist(pops, use.names=TRUE, fill=TRUE)  
  pops_adj <- NULL
  if (sum(colnames(pops) %in% c("DataValue"))==1){
    pops <- pops[!is.na(AgeStart)]
    pops <- as.data.frame(pops)
 	pops_adj <- pops %>% 
				dplyr::select(census_reference_date, DataValue) %>% 
				dplyr::filter(!(is.na(DataValue))) %>% # remove data from any census years that do not have these values
				group_by(census_reference_date) %>% 
				summarise(value = sum(DataValue))
  }
	
   
  # get totals from smoothed
  pops <- lapply(census_adj_all, "[[", "pop_smoothed")
  for (i in 1:length(pops)) {
    pops[[i]]$census_reference_date <- cen_dates[[i]]
  }
  pops <- rbindlist(pops, use.names=TRUE, fill=TRUE) 
  pops_smth <- NULL
  if (sum(colnames(pops) %in% c("DataValue"))==1){
	pops <- pops[!is.na(AgeStart)]
	pops <- as.data.frame(pops)
	pops_smth <- pops %>% 
			dplyr::select(census_reference_date, DataValue) %>% 
			dplyr::filter(!(is.na(DataValue))) %>% # remove data from any census years that do not have these values
			group_by(census_reference_date) %>% 
			summarise(value = sum(DataValue))
  }
  
  # get totals from basepop
  pops <- lapply(census_adj_all, "[[", "pop_basepop")
  for (i in 1:length(pops)) {
    pops[[i]]$census_reference_date <- cen_dates[[i]]
  }
  pops <- rbindlist(pops, use.names=TRUE, fill=TRUE)  
  pops_bp <- NULL
  if (sum(colnames(pops) %in% c("DataValue"))==1){
    pops <- pops[!is.na(AgeStart)]
    pops <- as.data.frame(pops)
	pops_bp <- pops %>% 
			dplyr::filter(BPLabel == "BP4") %>% # keep final bp result
			dplyr::select(census_reference_date, DataValue) %>% 
			dplyr::filter(!(is.na(DataValue))) %>% # remove data from any census years that do not have these values
			group_by(census_reference_date) %>% 
			summarise(value = sum(DataValue))
  }

  
# read total pop sheet from the input excel file
  total_pop <- readxl::read_xlsx(excel_input_file_path, sheet = "total_pop")
  
  ## reset datasheet
  total_pop <- as.data.frame(total_pop) %>% mutate_at(vars(3:ncol(total_pop)), ~replace(., !is.na(.), NA))
  
  # replace values re: census evaluation with those from the adjusted census output
  for (i in 1:length(cen_dates)) {
    total_pop$TimeMid_census[total_pop$time_start == floor(cen_dates[[i]])] <- cen_dates[[i]]
    total_pop$census_raw[total_pop$time_start == floor(cen_dates[[i]])] <- pops_raw$value[pops_raw$census_reference_date==cen_dates[[i]]]
	
	if (!is.null(pops_adj$value[pops_adj$census_reference_date==cen_dates[[i]]])){
		if (length(pops_adj$value[pops_adj$census_reference_date==cen_dates[[i]]])>0){
			total_pop$adjust_pes[total_pop$time_start == floor(cen_dates[[i]])] <- pops_adj$value[pops_adj$census_reference_date==cen_dates[[i]]]
		}
	}
	if (!is.null(pops_smth$value[pops_smth$census_reference_date==cen_dates[[i]]])){
		if (length(pops_smth$value[pops_smth$census_reference_date==cen_dates[[i]]])>0){
			total_pop$adjust_smooth[total_pop$time_start == floor(cen_dates[[i]])] <- pops_smth$value[pops_smth$census_reference_date==cen_dates[[i]]]
		}
	}
	if (!is.null(pops_bp$value[pops_bp$census_reference_date==cen_dates[[i]]])){
		if (length(pops_bp$value[pops_bp$census_reference_date==cen_dates[[i]]])>0){
			total_pop$adjust_basepop[total_pop$time_start == floor(cen_dates[[i]])] <- pops_bp$value[pops_bp$census_reference_date==cen_dates[[i]]]
		}
	}
  }
  
  # compute percentage differences for adjustments vs raw
  total_pop <- total_pop %>% 
    mutate(census_adjust_pes_percdiff = round((adjust_pes - census_raw)/census_raw * 100, 2),
           census_adjust_smooth_percdiff = round((adjust_smooth - census_raw)/census_raw * 100, 2),
           census_adjust_basepop_percdiff = round((adjust_basepop - census_raw)/census_raw * 100, 2))
  
  # if the total populations from other data sources fields aren't populated already, get them now
  
  # WPP2019
  if (is.na(total_pop$previous_WPP_estimates[1])) {
    pop <- DemoToolsData::WPP2019_pop %>%
      dplyr::filter(LocID == census_adj_all[[1]]$LocID) %>% 
      group_by(Year) %>% 
      summarise(value = sum(PopMale) + sum(PopFemale)) %>% 
      mutate(time_start = floor(Year)) %>% 
      dplyr::select(time_start, value)
    
    total_pop <- merge(total_pop, pop, by = "time_start", all.x = TRUE, all.y = FALSE) %>% 
      mutate(previous_WPP_estimates = value) %>% 
      dplyr::select(-value) %>% 
      mutate(census_previous_WPP_estimates_percdiff = round((previous_WPP_estimates-census_raw) / census_raw * 100, 2))

  }
  # GBD 2019
  if (is.na(total_pop$GBD_estimates[1])) {
	gbd <- NULL
    gbd <- tryCatch(
      {gbd = get_recorddata(dataProcessTypeIds = 6, 
                            indicatorIds = 52, 
                            locIds = census_adj_all[[1]]$LocID, 
                            locAreaTypeIds = 2, 
                            subGroupIds = 2, 
                            dataSourceShortNames = "GBD 2019")
      ## return(gbd)
	  },       
      error = function(e) {gbd <- NULL})
    if (!is.null(gbd)){
      gbd <- gbd %>% 
        dplyr::filter(SexID == 3) %>% 
        dplyr::select(TimeStart, DataValue) %>% 
        mutate(time_start = as.numeric(substr(TimeStart, nchar(TimeStart)-3, nchar(TimeStart))),
               value = DataValue) %>% 
        dplyr::filter(time_start %in% total_pop$time_start) %>% 
        dplyr::select(time_start, value)
      
      total_pop <- merge(total_pop, gbd, by = "time_start", all.x = TRUE, all.y = FALSE) %>% 
        mutate(GBD_estimates = value) %>% 
        dplyr::select(-value)
    }
  }
  
  # IDB
  if (is.na(total_pop$IDB_estimates[1])) {
	idb <- NULL
    idb <- tryCatch(
      {idb = get_recorddata(dataProcessTypeIds = 6, 
                            indicatorIds = 52, 
                            locIds = census_adj_all[[1]]$LocID, 
                            locAreaTypeIds = 2, 
                            subGroupIds = 2, 
                            dataSourceShortNames = "IDB")
      ##return(idb)
	  },       
      error = function(e) {idb <- NULL})
    if (!is.null(idb)){
      idb <- idb %>% 
        dplyr::filter(SexID == 3) %>% 
        dplyr::select(TimeStart, DataValue) %>% 
        mutate(time_start = as.numeric(substr(TimeStart, nchar(TimeStart)-3, nchar(TimeStart))),
               value = DataValue) %>% 
        dplyr::filter(time_start %in% total_pop$time_start) %>% 
        dplyr::select(time_start, value)
      
      total_pop <- merge(total_pop, idb, by = "time_start", all.x = TRUE, all.y = FALSE) %>% 
        mutate(IDB_estimates = value) %>% 
        dplyr::select(-value)
    }
  }
  
  # NSO
  ## disable condition, and systematically update with latest version available from DB
  ## if (is.na(total_pop$NSO_DF_estimates[1])) {
	dyb <- NULL
    dyb <- tryCatch(
      {dyb = get_recorddata(dataProcessTypeIds = 6, 
                            indicatorIds = 52, 
                            locIds = census_adj_all[[1]]$LocID, 
                            locAreaTypeIds = 2, 
                            subGroupIds = 2, 
                            dataSourceShortNames = "DYB")
      ## return(dyb)
	  },       
      error = function(e) {dyb <- NULL})
	  
    if (!is.null(dyb)){
      dyb <- dyb %>% 
        dplyr::filter(SexID == 3) %>% 
        dplyr::select(StatisticalConceptName, DataSourceYear, TimeStart, DataValue) %>% 
        mutate(time_start = as.numeric(substr(TimeStart, nchar(TimeStart)-3, nchar(TimeStart))),
               value = DataValue) %>% 
        dplyr::filter(time_start %in% total_pop$time_start) %>% 
        dplyr::select(StatisticalConceptName, DataSourceYear, time_start, value) %>% 
        distinct()
      
      # De-facto
      dyb_df <- dyb %>% 
        dplyr::filter(StatisticalConceptName == "De-facto")
	  if (nrow(dyb_df)>0) {             
        dyb_df <- dyb_df %>% group_by(time_start) %>% 
        dplyr::filter(DataSourceYear == max(DataSourceYear)) %>% 
        mutate(ind = 1:length(time_start)) %>% # if there's still more than one record, then just keep the first
        dplyr::filter(ind == 1) %>% 
        ungroup() %>% 
        dplyr::select(-StatisticalConceptName, -DataSourceYear, -ind)

       total_pop <- merge(total_pop, dyb_df, by = "time_start", all.x = TRUE, all.y = FALSE) %>% 
          mutate(NSO_DF_estimates = value) %>% 
          dplyr::select(-value)
      } else {
	    dyb_df <- NULL
	  }
	      
      # De-jure
      dyb_dj <- dyb %>% 
        dplyr::filter(StatisticalConceptName == "De-jure") 
	  if (nrow(dyb_dj)>0) {             
        dyb_dj <- dyb_dj %>% group_by(time_start) %>% 
        dplyr::filter(DataSourceYear == max(DataSourceYear)) %>% 
        mutate(ind = 1:length(time_start)) %>% # if there's still more than one record, then just keep the first
        dplyr::filter(ind == 1) %>% 
        ungroup() %>% 
        dplyr::select(-StatisticalConceptName, -DataSourceYear, -ind)

       total_pop <- merge(total_pop, dyb_dj, by = "time_start", all.x = TRUE, all.y = FALSE) %>% 
          mutate(NSO_DJ_estimates = value) %>% 
          dplyr::select(-value)
      } else {
	    dyb_dj <- NULL
	  }
			   
    } ## end dyb
  ##}

  ## update update_status
  update_status <- data.table(readxl::read_xlsx(path = excel_input_file_path, sheet = "update_status"))
  # convert date columns
  changeCols <- colnames(update_status)[which(as.vector(update_status[,lapply(.SD, class)]) %in% c("logical", "numeric"))]
  update_status[,(changeCols):= lapply(.SD, as.character), .SDcols = changeCols]	
  now <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
  update_status[worksheet=="total_pop", last_update := now]
 
  # write the total_pop table back to the input excel file
  
  # open the excel input file workbook
  wb <- openxlsx::loadWorkbook(file = excel_input_file_path)
  
  # delete the existing data in the total_pop sheet
  openxlsx::deleteData(wb, sheet = "total_pop", cols = 1:ncol(total_pop), rows = 2:(nrow(total_pop)+1), gridExpand = TRUE)
  # write the new data into the total_pop sheet
  openxlsx::writeData(wb, sheet = "total_pop", x = total_pop, startCol = 1, startRow = 2, colNames = FALSE)
 
  conditionalFormatting(wb, "total_pop", cols = 9, rows = 1, rule="A$2>0", style = editStyleHeader, stack=TRUE)
  conditionalFormatting(wb, "total_pop", cols = 9, rows = 2:(nrow(total_pop)+1), rule="A$2>0", style = editStyleColumn, stack=TRUE)
  addStyle(wb, "total_pop", style = fm2, cols = 3, rows = 2:(nrow(total_pop)+1), gridExpand = TRUE)
  addStyle(wb, "total_pop", style = fm0, cols = 4:14, rows = 2:(nrow(total_pop)+1), gridExpand = TRUE)
  addStyle(wb, "total_pop", style = fm2, cols = 15:19, rows = 2:(nrow(total_pop)+1), gridExpand = TRUE) 
  setColWidths(wb, "total_pop", 1:ncol(total_pop), widths="auto") 
  setColWidths(wb, "total_pop", 1:ncol(total_pop), widths = c(9, 9, rep(15,5), rep(20,7), 24, 28, 29, 33, 37)) 
  freezePane(wb, "total_pop", firstRow = TRUE, firstCol = TRUE)
 
  writeData(wb, sheet = "update_status", update_status$last_update, startCol=3, startRow=2, colNames = FALSE, rowNames=FALSE)
  setColWidths(wb, "update_status", 1:ncol(update_status), widths = c(22, rep(20, ncol(update_status)-1))) 

  # save the workbook
  openxlsx::saveWorkbook(wb, excel_input_file_path, overwrite = T)
  
}
