# For data acquired 
require(rio)
require(reticulate)
require(dplyr)
require(ggplot2)
require(tidyr)
require(REdaS)


#define variables
droot1 <- '../vrot_first/data'
droot2 <- '../vrot_second/data'

source_python('compute_intersect.py')

#target position order: [target_left, target_right, target_up, target_down]
tarpos_cords <- 
  tibble(x = c(-0.4, 0.4, 0, 0), 
         y = c(0, 0, 0.4, -0.4))


# Define functions

#compute signed angle 
# https://math.stackexchange.com/questions/317874/calculate-the-angle-between-two-vectors
# https://stackoverflow.com/questions/65156011/how-to-calculate-signed-angles-between-3-points

comp_angle <- function(u,v){
  if (any(is.na(u)) | any(is.na(v))) {return(NA)}
  #theta = atan2(u[1]*v[2]-v[1]*u[2], u[1]*u[2]+v[1]*v[2]) 
  theta = atan2(v[2], v[1]) - atan2(u[2],u[1])
  theta <- rad2deg(theta)
  if (theta > 180) {theta = 360 - theta}
  #x1y2−y1x2,x1x2+y1y2
  return(theta)
}


getfns <- function(droot, sid){
  pat <- paste("^",droot,"$",sep="")
  subpaths <- list.dirs(path = droot)
  tmp <- !grepl(pattern= pat, x = subpaths)
  subpaths <- subpaths[tmp]
  subids <- basename(subpaths)
  dat_dir <- subpaths[subids == sid]
  print(dat_dir)
  CONfns <- list.files(path = dat_dir, 
                       pattern = 'CON_.*.npy',
                       full.names = T)
  
  ADPfns <- list.files(path = dat_dir, 
                       pattern = 'ADP_.*.npy',
                       full.names = T)
  
  DADfns <- list.files(path = dat_dir, 
                       pattern = 'DAD_.*.npy', 
                       full.names= T)
  
  
  info <- list(CONfns = CONfns, ADPfns = ADPfns, DADfns = DADfns)
  
  return(info)
  
}

stack_data <- function(fns, d_all, condstr, trial){
  
  for (fn in fns){
    #message(fn)
    d <- np$load(file.path(fn))
    # Assuming d is a 2D array; convert it to an R matrix first
    #d_matrix <- py_to_r(d)
    d <- as_tibble(as.data.frame(d))
    names(d) <- c('x','y','inflag','xr',
                  'yr','tarpos', 'xrc', 'yrc',
                  't1','t3','t4')
    d$trial <- trial
    d$condition <- condstr
    trial <- trial + 1
    d_all <- rbind(d_all, d)
    
  }
  return(d_all)
}
#use_python("C:/python310/python.exe")
np <- reticulate::import("numpy")

#sids <- c( '20221021_YS', '20221024_JN', 
#           '20221025_CU', '20221025_SU',
#           '20221026_IC', '20221026_ZW')


#
droot <- '../vrot_first/data/'
#sids <- list.files(droot, pattern = '^20230[12]\\d{2}_[A-Z]{2}$')
sids <- list.files(droot, pattern = '^202[23]\\d{4}_[A-Z]{2}$')


#sids <- c( '20221219_ZH')
sids2 <- sids
for (sid in sids2){
  message(sprintf('Now processing %s ...', sid))
  
  source('ana_path_drive.R')
  
}

