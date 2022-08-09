library(tidyverse)

## plot census workflow results and basepop steps

census_workflow_plots <- function(ISO3,
                          census_adjusted_output,
                          plots_dir) {

	  now <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
      out_all <- census_adjusted_output
      
      # census workflow plots
      for (i in 1:length(out_all)){
        
        locname <- out_all[[i]]$LocName
        refpd   <- out_all[[i]]$census_reference_period
        pop_in  <- out_all[[i]]$census_pop_in
        ageint_in <- DemoTools::age2int(unique(pop_in$AgeStart))
        
        # for input series that were abridged or five-year, graduate to single year for plotting
        if (max(ageint_in, na.rm = TRUE) >1) {
          popM_in <- DemoTools::graduate_mono(Value = pop_in$DataValue[pop_in$SexID==1], 
                                   Age = pop_in$AgeStart[pop_in$SexID==1],
                                   AgeInt = DemoTools::age2int(pop_in$AgeStart[pop_in$SexID==1]))
          popF_in <- DemoTools::graduate_mono(Value = pop_in$DataValue[pop_in$SexID==2], 
                                   Age = pop_in$AgeStart[pop_in$SexID==2],
                                   AgeInt = DemoTools::age2int(pop_in$AgeStart[pop_in$SexID==2]))
          if (!is.null(out_all[[i]]$pop_adjusted)){
          popM_adj <- DemoTools::graduate_mono(Value = out_all[[i]]$pop_adjusted$DataValue[out_all[[i]]$pop_adjusted$SexID ==1],
                                    Age = pop_in$AgeStart[pop_in$SexID==1],
                                    AgeInt = DemoTools::age2int(pop_in$AgeStart[pop_in$SexID==1]))
          popF_adj <- DemoTools::graduate_mono(Value = out_all[[i]]$pop_adjusted$DataValue[out_all[[i]]$pop_adjusted$SexID ==2],
                                    Age = pop_in$AgeStart[pop_in$SexID==2],
                                    AgeInt = DemoTools::age2int(pop_in$AgeStart[pop_in$SexID==2]))
          } else {
            popM_adj <- NULL
            popF_adj <- NULL
          }
          popM_ext <- DemoTools::graduate_mono(Value = out_all[[i]]$pop_extended$DataValue[out_all[[i]]$pop_extended$SexID ==1],
                                    Age = out_all[[i]]$pop_extended$AgeStart[out_all[[i]]$pop_extended$SexID==1],
                                    AgeInt = DemoTools::age2int(out_all[[i]]$pop_extended$AgeStart[out_all[[i]]$pop_extended$SexID==1]))
          popF_ext <- DemoTools::graduate_mono(Value = out_all[[i]]$pop_extended$DataValue[out_all[[i]]$pop_extended$SexID ==2],
                                    Age = out_all[[i]]$pop_extended$AgeStart[out_all[[i]]$pop_extended$SexID==2],
                                    AgeInt = DemoTools::age2int(out_all[[i]]$pop_extended$AgeStart[out_all[[i]]$pop_extended$SexID==2]))
          
          
        } else {
          popM_in <- pop_in$DataValue[pop_in$SexID==1]
          popF_in <- pop_in$DataValue[pop_in$SexID==2]
          if (!is.null(out_all[[i]]$pop_adjusted)){
          popM_adj <- out_all[[i]]$pop_adjusted$DataValue[out_all[[i]]$pop_adjusted$SexID ==1]
          popF_adj <- out_all[[i]]$pop_adjusted$DataValue[out_all[[i]]$pop_adjusted$SexID ==2]
          } else {
            popM_adj <- NULL
            popF_adj <- NULL
          }
          popM_ext <- out_all[[i]]$pop_extended$DataValue[out_all[[i]]$pop_extended$SexID ==1]
          popF_ext <- out_all[[i]]$pop_extended$DataValue[out_all[[i]]$pop_extended$SexID ==2]
        }
        popM_smt <- out_all[[i]]$pop_smoothed$DataValue[out_all[[i]]$pop_smoothed$SexID ==1]
        popF_smt <- out_all[[i]]$pop_smoothed$DataValue[out_all[[i]]$pop_smoothed$SexID ==2]
        popM_bp  <- out_all[[i]]$pop_basepop$DataValue[out_all[[i]]$pop_basepop$SexID ==1 & out_all[[i]]$pop_basepop$BPLabel =="BP4"]
        popF_bp  <- out_all[[i]]$pop_basepop$DataValue[out_all[[i]]$pop_basepop$SexID ==2 & out_all[[i]]$pop_basepop$BPLabel =="BP4"]
        
        ymax <- max(max(popM_in),max(popF_in),max(popM_adj),max(popF_adj),max(popM_bp),max(popF_bp))
        
        # plot males
        svg(file.path(plots_dir, paste0(ISO3, "_PAG_Y", refpd,"_SM_XCensus Workflow.svg")), width=22*.9, height=12*.9) 
        par(oma=c(0, 0, 0, 0)) # no margins
        
        plot(c(0, 105), c(0, ymax), main = paste(locname, refpd, "census workflow adjustments - males", sep = " - "), type = "n", xlab = "Age", ylab = "Population")
        points(c(0:((length(popM_in))-1)), popM_in, col = "black")
        points(c(0:((length(popM_adj))-1)), popM_adj, col = "orange", pch = 19)
        lines(0:105, popM_ext, col = "orange")
        lines(0:105, popM_smt, col = "green", lwd = 5)
        lines(0:105, popM_bp, col = "black", lwd = 4, lty = 3)
        
        
        legend("topright", c("raw","pes adjusted","extended","smoothed","bp adjusted"), pch = c(1,19,NA,NA,NA),
               lty = c(NA,NA,1,1,4), lwd = c(NA,NA,1,5,4), col = c("black","orange","orange","green","black"))
        
		mtext(now, side=1, outer=TRUE, line = -1, cex=0.85, adj=1)
        dev.off()
        
        # plot females
        svg(file.path(plots_dir, paste0(ISO3, "_PAG_Y", refpd,"_SF_XCensus Workflow.svg")), width=22*.9, height=12*.9) 
        par(oma=c(0, 0, 0, 0)) # no margins
        
        plot(c(0, 105), c(0, ymax), main = paste(locname, refpd, "census workflow adjustments - females", sep = " - "), type = "n", xlab = "Age", ylab = "Population")
        points(c(0:((length(popF_in))-1)), popF_in, col = "black")
        points(c(0:((length(popF_adj))-1)), popF_adj, col = "orange", pch = 19)
        lines(0:105, popF_ext, col = "orange")
        lines(0:105, popF_smt, col = "green", lwd = 5)
        lines(0:105, popF_bp, col = "black", lwd = 4, lty = 3)
        
        legend("topright", c("raw","pes adjusted","extended","smoothed","bp adjusted"), pch = c(1,19,NA,NA,NA),
               lty = c(NA,NA,1,1,4), lwd = c(NA,NA,1,5,4), col = c("black","orange","orange","green","black"))
        
		mtext(now, side=1, outer=TRUE, line = -1, cex=0.85, adj=1)
        dev.off()

      } # end loop for censuses
      
      # basepop plots
      for (i in 1:length(out_all)){
        
        locname <- out_all[[i]]$LocName
        refpd   <- out_all[[i]]$census_reference_period
        pop_in  <- out_all[[i]]$census_pop_in
        ageint_in <- DemoTools::age2int(unique(pop_in$AgeStart))
        
        # for input series that were abridged or five-year, graduate to single year for plotting
        if (max(ageint_in, na.rm = TRUE) >1) {
          popM_ext <- DemoTools::graduate_mono(Value = out_all[[i]]$pop_extended$DataValue[out_all[[i]]$pop_extended$SexID ==1],
                                    Age = out_all[[i]]$pop_extended$AgeStart[out_all[[i]]$pop_extended$SexID==1],
                                    AgeInt = DemoTools::age2int(out_all[[i]]$pop_extended$AgeStart[out_all[[i]]$pop_extended$SexID==1]))
          popF_ext <- DemoTools::graduate_mono(Value = out_all[[i]]$pop_extended$DataValue[out_all[[i]]$pop_extended$SexID ==2],
                                    Age = out_all[[i]]$pop_extended$AgeStart[out_all[[i]]$pop_extended$SexID==2],
                                    AgeInt = DemoTools::age2int(out_all[[i]]$pop_extended$AgeStart[out_all[[i]]$pop_extended$SexID==2]))
          
          
        } else {
          popM_ext <- out_all[[i]]$pop_extended$DataValue[out_all[[i]]$pop_extended$SexID ==1]
          popF_ext <- out_all[[i]]$pop_extended$DataValue[out_all[[i]]$pop_extended$SexID ==2]
        }
        popM_smt <- out_all[[i]]$pop_smoothed$DataValue[out_all[[i]]$pop_smoothed$SexID ==1]
        popF_smt <- out_all[[i]]$pop_smoothed$DataValue[out_all[[i]]$pop_smoothed$SexID ==2]
        popM_bp1  <- out_all[[i]]$pop_basepop$DataValue[out_all[[i]]$pop_basepop$SexID ==1 & out_all[[i]]$pop_basepop$BPLabel =="BP1"]
        popF_bp1  <- out_all[[i]]$pop_basepop$DataValue[out_all[[i]]$pop_basepop$SexID ==2 & out_all[[i]]$pop_basepop$BPLabel =="BP1"]
        popM_bp2  <- out_all[[i]]$pop_basepop$DataValue[out_all[[i]]$pop_basepop$SexID ==1 & out_all[[i]]$pop_basepop$BPLabel =="BP2"]
        popF_bp2  <- out_all[[i]]$pop_basepop$DataValue[out_all[[i]]$pop_basepop$SexID ==2 & out_all[[i]]$pop_basepop$BPLabel =="BP2"]
        popM_bp3  <- out_all[[i]]$pop_basepop$DataValue[out_all[[i]]$pop_basepop$SexID ==1 & out_all[[i]]$pop_basepop$BPLabel =="BP3"]
        popF_bp3  <- out_all[[i]]$pop_basepop$DataValue[out_all[[i]]$pop_basepop$SexID ==2 & out_all[[i]]$pop_basepop$BPLabel =="BP3"]
        popM_bp4  <- out_all[[i]]$pop_basepop$DataValue[out_all[[i]]$pop_basepop$SexID ==1 & out_all[[i]]$pop_basepop$BPLabel =="BP4"]
        popF_bp4  <- out_all[[i]]$pop_basepop$DataValue[out_all[[i]]$pop_basepop$SexID ==2 & out_all[[i]]$pop_basepop$BPLabel =="BP4"]
        
        
        
        ymax <- max(max(popM_ext),max(popF_ext),max(popM_smt),max(popF_smt),max(popM_bp1),max(popF_bp1),
                    max(popM_bp2),max(popF_bp2),max(popM_bp3),max(popF_bp3),max(popM_bp4),max(popF_bp4))
        
        # plot males
        svg(file.path(plots_dir, paste0(ISO3, "_PAG_Y", refpd,"_SM_XCensus Basepop.svg")), width=22*.9, height=12*.9) 
        par(oma=c(0, 0, 0, 0)) # no margins
        
        plot(c(0, 20), c(0, ymax), main = paste(locname, refpd, "census basepop adjustment - males", sep = " - "), type = "n", xlab = "Age", ylab = "Population")
        points(0:105, popM_ext, col = "orange", pch = 19)
        lines(0:105, popM_ext, col = "orange")
        lines(0:105, popM_smt, col = "blue", lwd = 5)
        lines(0:105, popM_bp1, col = "red", lwd = 4)
        lines(0:105, popM_bp2, col = "gray", lwd = 2)
        lines(0:105, popM_bp3, col = "violet", lwd = 4, lty = 2)
        lines(0:105, popM_bp4, col = "black", lwd = 4, lty = 3)
        
        
        legend("bottomleft", c("unsmoothed","smoothed","BP1","BP2","BP3","BP4"), pch = c(19,NA,NA,NA,NA,NA),
               lwd = c(1,5,4,2,4,4), lty = c(1,1,1,1,2,3), col = c("orange","blue","red","gray","violet","black"))

		mtext(now, side=1, outer=TRUE, line = -1, cex=0.85, adj=1)        
        dev.off()
        
        # plot females
        svg(file.path(plots_dir, paste0(ISO3, "_PAG_Y", refpd,"_SF_XCensus Basepop.svg")), width=22*.9, height=12*.9) 
        par(oma=c(0, 0, 0, 0)) # no margins
        
        plot(c(0, 20), c(0, ymax), main = paste(locname, refpd, "census basepop adjustment - females", sep = " - "), type = "n", xlab = "Age", ylab = "Population")
        points(0:105, popF_ext, col = "orange", pch = 19)
        lines(0:105, popF_ext, col = "orange")
        lines(0:105, popF_smt, col = "blue", lwd = 5)
        lines(0:105, popF_bp1, col = "red", lwd = 4)
        lines(0:105, popF_bp2, col = "gray", lwd = 2)
        lines(0:105, popF_bp3, col = "violet", lwd = 4, lty = 2)
        lines(0:105, popF_bp4, col = "black", lwd = 4, lty = 3)
        
        
        legend("bottomleft", c("unsmoothed","smoothed","BP1","BP2","BP3","BP4"), pch = c(19,NA,NA,NA,NA,NA),
               lwd = c(1,5,4,2,4,4), lty = c(1,1,1,1,2,3), col = c("orange","blue","red","gray","violet","black"))
        
		mtext(now, side=1, outer=TRUE, line = -1, cex=0.85, adj=1)
        dev.off()
        
      }
      
  
} # end plot function
    
  