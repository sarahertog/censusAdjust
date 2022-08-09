

# this function compiles the census adjustment info developed through the workflow and writes
# that info back to the POP_INPUTS sheet of the input excel file

census_adj_write_to_input_excel_file <- function(excel_input_file_path, census_adj_all) {

  # From extract the census adjustment information to be written back to the input excel file
  cid <- lapply(census_adj_all, "[[", "DataCatalogID")
  c1 <- lapply(census_adj_all, "[[", "best_smooth_adult")
  c2 <- lapply(census_adj_all, "[[", "best_smooth_child")
  c3 <- lapply(census_adj_all, "[[", "bachi_adult")
  c4 <- lapply(census_adj_all, "[[", "bachi_child")
  c5 <- lapply(census_adj_all, "[[", "ageRatio_adult_orig")
  c6 <- lapply(census_adj_all, "[[", "ageRatio_child_orig")
  c7 <- lapply(census_adj_all, "[[", "ageRatio_adult_mav2")
  c8 <- lapply(census_adj_all, "[[", "ageRatio_child_mav2")
  c9 <- lapply(census_adj_all, "[[", "EduYrs")
  c10 <- lapply(census_adj_all, "[[", "census_input_age_structure")
  c11 <- lapply(census_adj_all, "[[", "census_input_max_age")
  c12 <- lapply(census_adj_all, "[[", "age_redist_start")
  c13 <- lapply(census_adj_all, "[[", "census_data_source")
  
  # read the census population info from the input Excel file
  pop_eval <- readxl::read_xlsx(excel_input_file_path, sheet = "POP_INPUTS")
  
  # replace values re: census evaluation with those from the adjusted census output
  for (i in 1:length(cid)) {
    pop_eval$input_age_structure[pop_eval$DataCatalogID == cid[[i]]] <- c10[[i]]
    pop_eval$input_max_age[pop_eval$DataCatalogID == cid[[i]]] <- c11[[i]]
    pop_eval$age_redist_start[pop_eval$DataCatalogID == cid[[i]]] <- c12[[i]]
    pop_eval$best_smooth_adult[pop_eval$DataCatalogID == cid[[i]]] <- c1[[i]]
    pop_eval$best_smooth_child[pop_eval$DataCatalogID == cid[[i]]] <- c2[[i]]
    pop_eval$bachi_adult[pop_eval$DataCatalogID == cid[[i]]] <- c3[[i]]
    pop_eval$bachi_child[pop_eval$DataCatalogID == cid[[i]]] <- c4[[i]]
    pop_eval$ageRatio_adult_orig[pop_eval$DataCatalogID == cid[[i]]] <- c5[[i]]
    pop_eval$ageRatio_child_orig[pop_eval$DataCatalogID == cid[[i]]] <- c6[[i]]
    pop_eval$ageRatio_adult_mav2[pop_eval$DataCatalogID == cid[[i]]] <- c7[[i]]
    pop_eval$ageRatio_child_mav2[pop_eval$DataCatalogID == cid[[i]]] <- c8[[i]]
	if (!is.null(c9[[i]])){
		pop_eval$EduYrs[pop_eval$DataCatalogID == cid[[i]]] <- c9[[i]]
	} else {
		pop_eval$EduYrs[pop_eval$DataCatalogID == cid[[i]]] <- NA
	}
    pop_eval$data_source[pop_eval$DataCatalogID == cid[[i]]] <- c13[[i]]
    
  }

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
  openxlsx::deleteData(wb, sheet = "POP_INPUTS", cols = 1:ncol(pop_eval), rows = 2:(nrow(pop_eval)+1), gridExpand = TRUE)
  # write the new data into th emig_net_count_age_sex sheet
  openxlsx::writeData(wb, sheet = "POP_INPUTS", x = pop_eval, startCol = 1, startRow = 2, colNames = FALSE)

  writeData(wb, sheet = "update_status", update_status$last_update, startCol=3, startRow=2, colNames = FALSE, rowNames=FALSE)
  setColWidths(wb, "update_status", 1:ncol(update_status), widths = c(22, rep(20, ncol(update_status)-1))) 

  # save the workbook
  openxlsx::saveWorkbook(wb, excel_input_file_path, overwrite = T)

}

