

#Get BestGrad5 from smoothing results
getBestGrad5 <- function(AgeRatioScore_orig, AgeRatioScore_mav2, EduYrs, subgroup = c("adult","child")) {
  
 if (subgroup == "adult") {
  # select whether to use the straight  5-year data 
  # or use Mav2 or mav4 of the 5-year data
  BestGrad5 <- NA
  if (AgeRatioScore_orig < 4 ) {
    BestGrad5 <- 1
  } else {
    if (EduYrs >= 4) {
      BestGrad5 <-1
    } else {
      if (AgeRatioScore_mav2 < 4) {
        BestGrad5 <- 2
      } else {
        BestGrad5 <- 4
      }
    }
  }
 }
  
  if (subgroup == "child") {

    BestGrad5 <- NA
    if (AgeRatioScore_orig < 4 ) {
      BestGrad5 <- 1
    } else {
      if (EduYrs >= 4) {
        BestGrad5 <-1
      } else {
          BestGrad5 <- 2
        } 
      }
    }
  
  
  
  return(BestGrad5)
  
}

