library(tidyverse)

## plot birth cohorts across adjusted censuses to look for any inconsistencies (e.g. crossovers) with time

census_cohort_plots <- function(ISO3,
                          census_adjusted_output,
                          plots_dir) {

	  now <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
      out_all <- census_adjusted_output
      
      cohorts_out <- list()
      cohorts_out_noBP <- list()
      
      for (i in 1:length(out_all)){
        
        CensusDate   <- out_all[[i]]$census_reference_date
        
        Males    <- out_all[[i]]$census_pop_out$DataValue[out_all[[i]]$census_pop_out$SexID==1]
        Females  <- out_all[[i]]$census_pop_out$DataValue[out_all[[i]]$census_pop_out$SexID==2]
        Males_noBP<- out_all[[i]]$pop_smoothed$DataValue[out_all[[i]]$pop_smoothed$SexID==1]
        Females_noBP<- out_all[[i]]$pop_smoothed$DataValue[out_all[[i]]$pop_smoothed$SexID==2]
        Age      <- 0:105
        
        cohortsM <- DemoTools::birthCohorts(Males, Age, CensusDate, cohortSize = 1)
        cohortsF <- DemoTools::birthCohorts(Females, Age, CensusDate, cohortSize = 1)
        
        if (!is.null(Males_noBP) & !is.null(Females_noBP)) {
        cohortsM_noBP <- DemoTools::birthCohorts(Males_noBP, Age, CensusDate, cohortSize = 1)
        cohortsF_noBP <- DemoTools::birthCohorts(Females_noBP, Age, CensusDate, cohortSize = 1)
        } else {
          cohortsM_noBP <- cohortsM
          cohortsM_noBP$Births <- NA
          cohortsF_noBP <- cohortsF
          cohortsF_noBP$Births <- NA
        }
        
        cohorts <- data.frame(cohortsM = cohortsM,
                              cohortsF = cohortsF)
        cohorts$CensusDate <- CensusDate
        
        cohorts_out[[i]] <- cohorts
        
        cohorts_noBP <- data.frame(cohortsM_noBP = cohortsM_noBP,
                                   cohortsF_noBP = cohortsF_noBP)
        cohorts_noBP$CensusDate <- CensusDate
        
        cohorts_out_noBP[[i]] <- cohorts_noBP
        
      }
      
      cohorts_out <- do.call(rbind, cohorts_out)
      
      cohorts_out_noBP <- do.call(rbind, cohorts_out_noBP)
      
      cenyrs <- unique(cohorts_out$CensusDate)
      colors <- rainbow(length(cenyrs))
      locname <- out_all[[1]]$LocName
      
      # plot males
      svg(file.path(plots_dir, paste0(ISO3, "_PAG", "_SM_XCensus Cohorts.svg")), width=22*.9, height=12*.9) 
      par(oma=c(0, 0, 0, 0)) # no margins
      
      plot(c(min(cohorts_out$cohortsM.Year), max(cohorts_out$cohortsM.Year)), 
           c(0, max(cohorts_out$cohortsM.Births)), 
           main = paste("Adjusted census pop by birth cohort: ", locname, "males", sep = " - "), type = "n",
           xlab = "Birth year",
           ylab = "Population")
      
      for (i in 1:length(cenyrs)) {
        lines(cohorts_out$cohortsM.Year[cohorts_out$CensusDate == cenyrs[i]], 
              cohorts_out$cohortsM.Births[cohorts_out$CensusDate == cenyrs[i]], 
              col = colors[i], lwd = 2)
        lines(cohorts_out_noBP$cohortsM_noBP.Year[cohorts_out_noBP$CensusDate == cenyrs[i]], 
              cohorts_out_noBP$cohortsM_noBP.Births[cohorts_out_noBP$CensusDate == cenyrs[i]], 
              col = colors[i], lty = 2, lwd = 2)
      }
      
      grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted")
      
      legend("topleft", legend = c(round(floor(cenyrs)), "Without basepop adj"), lty = c(rep(1,length(cenyrs)),2), 
             lwd = 2, col = c(colors, "black") , title = "Census year")
      
	  mtext(now, side=1, outer=TRUE, line = -1, cex=0.85, adj=1)
      dev.off()
      
      # plot females
      svg(file.path(plots_dir, paste0(ISO3, "_PAG", "_SF_XCensus Cohorts.svg")), width=22*.9, height=12*.9) 
      par(oma=c(0, 0, 0, 0)) # no margins
      
      plot(c(min(cohorts_out$cohortsF.Year), max(cohorts_out$cohortsF.Year)), 
           c(0, max(cohorts_out$cohortsF.Births)), 
           main = paste("Adjusted census pop by birth cohort: ", locname, "females", sep = " - "), type = "n",
           xlab = "Birth year",
           ylab = "Population")
      
      for (i in 1:length(cenyrs)) {
        lines(cohorts_out$cohortsF.Year[cohorts_out$CensusDate == cenyrs[i]], 
              cohorts_out$cohortsF.Births[cohorts_out$CensusDate == cenyrs[i]], 
              col = colors[i], lwd = 2)
        lines(cohorts_out_noBP$cohortsF_noBP.Year[cohorts_out_noBP$CensusDate == cenyrs[i]], 
              cohorts_out_noBP$cohortsF_noBP.Births[cohorts_out_noBP$CensusDate == cenyrs[i]], 
              col = colors[i], lty = 2, lwd = 2)
      }
      
      grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted")
      
      legend("topleft", legend = c(round(floor(cenyrs)), "Without basepop adj"), lty = c(rep(1,length(cenyrs)),2), 
             lwd = 2, col = c(colors, "black") , title = "Census year")
      
	  mtext(now, side=1, outer=TRUE, line = -1, cex=0.85, adj=1)
      dev.off()
      
} # end plot function
    
  