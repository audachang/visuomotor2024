require(PMCMRplus)

proc <- function(sids_select){ 

  pcheck <- grepl(paste(sids_select, collapse = "|"), errfns)
  errfns_select <- errfns[pcheck]
  dall <- tibble()
  for (fn in errfns_select){
    d <- readRDS(fn)
    
    
    
    dall <- rbind(dall, d)
    
  }  
  

  
  out <- gesdTest(dall$angle, 20)
  
  dall2 <- dall[-out$ix[1:7],]
  
  dall.summ <- dall2 %>%
    group_by(errblock, condition) %>%
    summarise(error = mean(angle), 
              sd = sd(angle),
              n = n()) %>%
    mutate(se = sd/n, 
           upper = error + sd,
           lower = error - sd) 

return(dall.summ)

}

