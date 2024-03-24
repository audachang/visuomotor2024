# For data acquired 
require(rio)
require(reticulate)
require(dplyr)
require(ggplot2)
require(tidyr)
require(REdaS)
require(stringr)

errfns <- list.files(path = '../plot_individuals/processed', 
                     pattern = '2023.*error.RDS', 
                     full.names = T)
sids <- 
  str_extract(errfns, '[:number:]{8}_[:alpha:]{2}')




