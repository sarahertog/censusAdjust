## this function queries Shortnotes (and DataCatalog) for list of censuses, and their used/include status
## and writes back to the POP_INPUTS sheet of the input excel file
## PG: also updates initial default value if last_update is NA in update_status sheet
## i.e., pes_adjustment = min(WPP_Net_Error, PES_Net_Error) if WPP_Net_Error < 0 else use max()

census_used_write_to_input_excel_file <- function(excel_input_file_path, WPP_RevID, myLocID, isSmall) {

  # library(devtools)
  # devtools::install_github("timriffe/DemoTools", force=TRUE)
  library(DemoTools)
  # devtools::install_github("timriffe/DDSQLtools", force=TRUE)
  library(DDSQLtools)
  ## production server for UNPD (in Valencia)
  options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")
  options(scipen=999999)
  
  shortnotes_URL <- "https://popdiv.dfs.un.org/peps/eagle/api/notes/get/longcatalogs/" 

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
  
  # read the census population info from the input Excel file
  pop_eval <- data.table(readxl::read_xlsx(excel_input_file_path, sheet = "POP_INPUTS"))
  update_status <- data.table(readxl::read_xlsx(excel_input_file_path, sheet = "update_status"))
  
  ## get DataCatalog list of censuses
  DataCatalog <- data.table(get_datacatalog(locIds = myLocID, dataProcessTypeIds = 2, addDefault = "false", isSubnational = FALSE))
  DataCatalog$FieldWorkMiddle[is.na(DataCatalog$FieldWorkMiddle)] <- DataCatalog$ReferenceYearMid[is.na(DataCatalog$FieldWorkMiddle)]
  DataCatalog <- DataCatalog[isSubnational==FALSE & FieldWorkMiddle >= 1946, .(DataCatalogID, ShortName, Year=trunc(FieldWorkMiddle))]
  
  DT_years_1946 <- data.table(time_start = 1946:year(now()))
  POP_INPUTS <- merge(DT_years_1946, DataCatalog[, .(Year, DataCatalogID, DataCatalogShortName=ShortName)], by.x="time_start", by.y="Year", all.x=TRUE, all.y=FALSE)
  POP_INPUTS[, time_span := 0]
  
  ## query Shortnotes
  shortnotes <- NULL
  shortnotes <- fromJSON(paste0(shortnotes_URL, WPP_RevID, "/", myLocID, "/Population"), flatten=TRUE)
  
  shortnotes1 <- data.table(shortnotes$DataCatalogsWithSelections)
  shortnotes2 <- data.table(shortnotes$IndicatorMetadata)
  shortnotes2 <- data.table(shortnotes$IndicatorMetadata)
  shortnotes <- merge(shortnotes1[,.(DataCatalogID, IndicatorInternalName, SelectionValue)], shortnotes2[, .(IndicatorInternalName, DataProcessTypeIDs, DataStatusIDs, DataTypeIDs, IndicatorIDs)], by="IndicatorInternalName", all.x=TRUE, all.y=FALSE)
  ## shortnotes_used <- shortnotes[SelectionValue %in% c("Used", "Considered", "NotConsidered", "NotTouched_UpdatedData", "NotTouched")]
  shortnotes_used <- shortnotes[SelectionValue %in% c("Used", "NotTouched_UpdatedData", "NotTouched", "NotConsidered")]
  shortnotes_used[, LocID := myLocID]
  
 if (nrow(shortnotes) > 0) {
  	shortnotes_used <- shortnotes_used[IndicatorInternalName %in% c("CU", "CA", "CAS", "CABP"), .(DataCatalogID, IndicatorInternalName, SelectionValue)]
	## shortnotes_used <- data.table::dcast(shortnotes_used, DataCatalogID ~ IndicatorInternalName, value.var=c("SelectionValue"))
	## merge stepwise because CA, CAS or CABP does nor always exists
  	POP_INPUTS <- merge(POP_INPUTS, shortnotes_used[IndicatorInternalName=="CU"], by="DataCatalogID", all.x=TRUE, all.y=FALSE)
  	POP_INPUTS[, Include := FALSE]
  	POP_INPUTS[is.na(DataCatalogID)==FALSE & SelectionValue %in% c("Used", "NotTouched_UpdatedData"), Include := TRUE]	
  	POP_INPUTS[is.na(DataCatalogID)==FALSE & SelectionValue %in% c("NotConsidered"), Include := FALSE]	
	POP_INPUTS <- POP_INPUTS[,.(time_start, time_span, DataCatalogID, DataCatalogShortName, Include)]
	
  	POP_INPUTS <- merge(POP_INPUTS, shortnotes_used[IndicatorInternalName=="CA"], by="DataCatalogID", all.x=TRUE, all.y=FALSE)
  	POP_INPUTS[, adjust_pes := FALSE]
  	POP_INPUTS[is.na(DataCatalogID)==FALSE & Include==TRUE & IndicatorInternalName=="CA" & SelectionValue %in% c("Used", "NotTouched_UpdatedData", "NotTouched"), adjust_pes := TRUE]
 	POP_INPUTS[is.na(DataCatalogID)==FALSE & Include==TRUE & IndicatorInternalName=="CA" & SelectionValue %in% c("NotConsidered"), adjust_pes := FALSE]	
	POP_INPUTS <- POP_INPUTS[,.(time_start, time_span, DataCatalogID, DataCatalogShortName, Include, adjust_pes)]
	
  	POP_INPUTS <- merge(POP_INPUTS, shortnotes_used[IndicatorInternalName=="CAS"], by="DataCatalogID", all.x=TRUE, all.y=FALSE)
  	POP_INPUTS[, adjust_smooth := FALSE]
  	POP_INPUTS[is.na(DataCatalogID)==FALSE & Include==TRUE & IndicatorInternalName=="CAS" & SelectionValue %in% c("Used", "NotTouched_UpdatedData", "NotTouched"), adjust_smooth := TRUE]
 	POP_INPUTS[is.na(DataCatalogID)==FALSE & Include==TRUE & IndicatorInternalName=="CAS" & SelectionValue %in% c("NotConsidered"), adjust_smooth := FALSE]	
	POP_INPUTS <- POP_INPUTS[,.(time_start, time_span, DataCatalogID, DataCatalogShortName, Include, adjust_pes, adjust_smooth)]

  	POP_INPUTS <- merge(POP_INPUTS, shortnotes_used[IndicatorInternalName=="CABP"], by="DataCatalogID", all.x=TRUE, all.y=FALSE)
  	POP_INPUTS[, adjust_basepop := FALSE]
  	POP_INPUTS[is.na(DataCatalogID)==FALSE & Include==TRUE & IndicatorInternalName=="CABP" & SelectionValue %in% c("Used", "NotTouched_UpdatedData", "NotTouched"), adjust_basepop := TRUE]
 	POP_INPUTS[is.na(DataCatalogID)==FALSE & Include==TRUE & IndicatorInternalName=="CABP" & SelectionValue %in% c("NotConsidered"), adjust_basepop := FALSE]	
	POP_INPUTS <- POP_INPUTS[,.(time_start, time_span, DataCatalogID, DataCatalogShortName, Include, adjust_pes, adjust_smooth, adjust_basepop)]
  } else {
  	POP_INPUTS[is.na(DataCatalogID)==FALSE, Include := FALSE]
  }  
 
 
  ## merge existing worksheet with latest server query
  pop_eval$DataCatalogShortName <- NULL
  pop_eval$Include              <- NULL
  pop_eval$adjust_pes           <- NULL
  pop_eval$adjust_basepop       <- NULL
  pop_eval$adjust_smooth        <- NULL
  POP_INPUTS <- merge(POP_INPUTS, pop_eval, by=c("time_start", "time_span", "DataCatalogID"), all.x=TRUE, all.y=FALSE)
  
  POP_INPUTS[is.na(DataCatalogID)==FALSE & is.na(adjust_coverage), adjust_coverage := 1]
  
  POP_INPUTS[is.na(DataCatalogID)==FALSE & is.na(adjust_pes),     adjust_pes     := TRUE]
  POP_INPUTS[is.na(DataCatalogID)==FALSE & is.na(adjust_smooth),  adjust_smooth  := TRUE]
  POP_INPUTS[is.na(DataCatalogID)==FALSE & is.na(adjust_basepop), adjust_basepop := TRUE]	
  
  POP_INPUTS <- unique(POP_INPUTS[,.(time_start, time_span, DataCatalogID, DataCatalogShortName, 
	Include, adjust_coverage, adjust_pes, pes_adjustment, 
	note_adjustment, PES_Net_Error, DA_Net_Error, WPP_Net_Error, 
	adjust_smooth, adjust_basepop, input_age_structure, input_max_age, 
	age_redist_start, best_smooth_adult, best_smooth_child, 
	bachi_adult, bachi_child, ageRatio_adult_orig, ageRatio_child_orig, 
	ageRatio_adult_mav2, ageRatio_child_mav2, EduYrs, data_source)])
  
  ## get last available WPP_Net_Error
  maxYear <- max(POP_INPUTS[!is.na(WPP_Net_Error), time_start])
  POP_INPUTS[is.na(WPP_Net_Error) & time_start > maxYear, WPP_Net_Error := POP_INPUTS[!is.na(WPP_Net_Error) & time_start==maxYear, WPP_Net_Error]]
  
  ## impute adjust_pes for initial status
  if (nrow(update_status[worksheet=="POP_INPUTS" & is.na(last_update)])>0){
	POP_INPUTS[is.na(DataCatalogID)==FALSE, pes_adjustment := WPP_Net_Error]
	POP_INPUTS[is.na(DataCatalogID)==FALSE & WPP_Net_Error < 0, pes_adjustment := min(WPP_Net_Error, PES_Net_Error, na.rm=TRUE), by=list(DataCatalogID)]
	POP_INPUTS[is.na(DataCatalogID)==FALSE & WPP_Net_Error > 0, pes_adjustment := max(WPP_Net_Error, PES_Net_Error, na.rm=TRUE), by=list(DataCatalogID)]
  } else { ## impute only if NA
	POP_INPUTS[is.na(DataCatalogID)==FALSE & is.na(pes_adjustment) & WPP_Net_Error < 0, pes_adjustment := min(WPP_Net_Error, PES_Net_Error, na.rm=TRUE), by=list(DataCatalogID)]
	POP_INPUTS[is.na(DataCatalogID)==FALSE & is.na(pes_adjustment) & WPP_Net_Error > 0, pes_adjustment := max(WPP_Net_Error, PES_Net_Error, na.rm=TRUE), by=list(DataCatalogID)]
  }
  
  ## if (parameters[parameter == "isSmall", value]==TRUE) {
  if (isSmall){
   	POP_INPUTS[is.na(DataCatalogID)==FALSE, adjust_basepop := FALSE]	
  }
  
  setorder(POP_INPUTS, time_start)

  ## update update_status
  update_status <- data.table(readxl::read_xlsx(path = excel_input_file_path, sheet = "update_status"))
  # convert date columns
  changeCols <- colnames(update_status)[which(as.vector(update_status[,lapply(.SD, class)]) %in% c("logical", "numeric"))]
  update_status[,(changeCols):= lapply(.SD, as.character), .SDcols = changeCols]	
  now <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
  update_status[worksheet=="POP_INPUTS", last_update := now]


  # write the POP_INPUTS table back to the input excel file
  # open the excel input file workbook
  wb <- openxlsx::loadWorkbook(file = excel_input_file_path)
  
  # delete the existing data in the POP_INPUTS sheet
  openxlsx::deleteData(wb, sheet = "POP_INPUTS", cols = 1:ncol(POP_INPUTS), rows = 2:(nrow(POP_INPUTS)+1), gridExpand = TRUE)
  # write the new data into th emig_net_count_age_sex sheet
  openxlsx::writeData(wb, sheet = "POP_INPUTS", x = POP_INPUTS, startCol = 1, startRow = 2, colNames = FALSE, rowNames=FALSE, borders = "rows", borderStyle = "medium")

  conditionalFormatting(wb, "POP_INPUTS", cols = 5:9, rows = 1, rule="B$2==0", style = editStyleHeader, stack=TRUE)
  conditionalFormatting(wb, "POP_INPUTS", cols = 13:14, rows = 1, rule="B$2==0", style = editStyleHeader, stack=TRUE)
  conditionalFormatting(wb, "POP_INPUTS", cols = 5:9, rows = 2:(nrow(POP_INPUTS)+1), rule="B$2==0", style = editStyleColumn, stack=TRUE)
  conditionalFormatting(wb, "POP_INPUTS", cols = 13:14, rows = 2:(nrow(POP_INPUTS)+1), rule="B$2==0", style = editStyleColumn, stack=TRUE)
  addStyle(wb, "POP_INPUTS", style = fm2, cols = 8, rows = 2:(nrow(POP_INPUTS)+1), gridExpand = TRUE, stack=TRUE)
  addStyle(wb, "POP_INPUTS", style = fm2, cols = 10:12, rows = 2:(nrow(POP_INPUTS)+1), gridExpand = TRUE, stack=TRUE)
  addStyle(wb, "POP_INPUTS", style = fm2, cols = 20:26, rows = 2:(nrow(POP_INPUTS)+1), gridExpand = TRUE, stack=TRUE)
  
  writeData(wb, sheet = "update_status", update_status$last_update, startCol=3, startRow=2, colNames = FALSE, rowNames=FALSE)
  setColWidths(wb, "update_status", 1:ncol(update_status), widths = c(22, rep(20, ncol(update_status)-1))) 
  
  # save the workbook
  openxlsx::saveWorkbook(wb, excel_input_file_path, overwrite = T)

}