library(jsonlite)
library(dplyr)
library(stringr)
library(doParallel)
library(doSNOW)

readBig <- function(){

  filenames <- dir(".", pattern="*", recursive = TRUE)

  registerDoParallel()
  cl <- makeCluster(4)
  registerDoSNOW(cl)
  
  pb <- txtProgressBar(max = length(filenames), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  b <- foreach(f = 1:length(filenames), .combine = 'rbind', .options.snow = opts, .packages = c('jsonlite','dplyr','stringr', 'doParallel')) %dopar% {
  
    sd <- read_json(path = filenames[f], flatten = TRUE, simplifyVector = TRUE)
    
    tempret <- foreach(i = 1:length(sd), .combine = 'rbind', .packages = c('jsonlite','dplyr','stringr', 'doParallel')) %do% {
    
      date <- as.POSIXct(names(sd)[i], format="%Y%m%d%H%M") 
    
      singleday <- sd[[i]]
      
      if ( length(singleday) == 0 )
      {
        warning(paste("No data in ",filename))
        return()
      }  
      
      singleday <- singleday[ , !names(singleday) %in% c(
        "discriminator", # tylko vehicle
        "color", # bialy
        "description", # pusty
        "id",    # nie potrzeba
        "metadata", # pusty
        "locationDescription",# puste
        "picture.id",   # nie potrzeba
        "picture.name", # nie potrzeba
        "picture.extension", # nie potrzeba
        "platesNumber", # sideNumber
        "type"   # CAR
        
      )]
      
      singleday$date <- date
      
      singleday <- rename(singleday, "lat"="location.latitude")
      singleday <- rename(singleday, "long"="location.longitude")
      
      return (singleday);    
    
    }
    
    return (tempret)
  }
  close(pb)
  stopCluster(cl) 

  return (b)
  
}

system.time( x <- readBig() )

x$status <- as.factor(x$status)
x$sideNumber <- as.numeric(x$sideNumber)
vozilla <- x

vozilla$name <- gsub("NISSAN ", "", vozilla$name)
vozilla$name <- gsub("e-", "", vozilla$name)
vozilla$name <- as.factor(vozilla$name)

rm (readBig, x)


