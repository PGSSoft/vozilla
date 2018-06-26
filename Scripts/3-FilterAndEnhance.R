library(purrr)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(doParallel)
library(doSNOW)
library(magrittr)
library(data.table)

Sys.time()

#============================================================================================

# Dodanie bieżącej temperatury otoczenia do zbioru danych Vozilli
temperature$yyyymmddhh <- force_tz(temperature$yyyymmddhh, "")
vozilla$yyyymmddhh <- lubridate::round_date(vozilla$date,'1 hour')
vozilla <- left_join(vozilla, temperature)
vozilla$yyyymmddhh <- NULL

#============================================================================================
# nadanie identyfikatorów seriom (streakom)
Sys.time()

registerDoParallel()

cl <- makeCluster(4)

registerDoSNOW(cl)

pb <- txtProgressBar(max = 200, style = 3)


progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

system.time(  
  vozilla2 <- foreach(i = 1:200, 
                      .combine = 'rbind', 
                      .options.snow = opts,                       
                      .packages = c('jsonlite','dplyr','stringr', 'lubridate')
  ) %dopar% {
    for (st in levels(vozilla$status)){
      vozilla$streakid[vozilla$status==st & vozilla$sideNumber==i] <- vozilla$sideNumber[vozilla$status==st & vozilla$sideNumber==i] + 10000*as.integer(vozilla$status[vozilla$status==st & vozilla$sideNumber==i]) + 1000000 *( as.integer(vozilla$date[vozilla$status==st & vozilla$sideNumber==i])/60-seq.int(length(vozilla$status[vozilla$status==st & vozilla$sideNumber==i])))
    }
    
    return (vozilla %>% filter(sideNumber==i))
  }
)

close(pb)
stopCluster(cl)  
vozilla <- vozilla2
rm(vozilla2)

#============================================================================================
# funkcja tworząca pojedyncze streaki z wielu obserwacji minutowych


processStreak <- function(streakId, src){
  
  streak <- src[streakid == streakId]
  
  streakFirst <- streak[1,]
  streakLast <- streak[nrow(streak),]
  
  x <- data.frame(
    streakid = streakId,
    sideNumber = streakFirst$sideNumber,
    latStart = streakFirst$lat,
    longStart = streakFirst$long,
    latEnd = streakLast$lat,
    longEnd = streakLast$long,
    batStart = streakFirst$batteryLevelPct,
    batEnd = streakLast$batteryLevelPct,
    batGain = streakLast$batteryLevelPct - streakFirst$batteryLevelPct,
    duration = difftime(streakLast$date, streakFirst$date, unit="mins")+1,
    minDayStart = hour(streakFirst$date)*60+minute(streakFirst$date),
    dateStart = streakFirst$date,
    dateLast = streakLast$date,
    name = streakFirst$name,
    status = streakFirst$status
  )
  
  return(x)
  
}

#utworzenie streaków podstawowych, na bazie kompletnych i pełnych danych z października


vdt <- as.data.table(vozilla)
setkey(vdt, streakid, physical = TRUE)

streakIDs <- vdt %>% select (streakid) %>% unique() %>% pull 

registerDoParallel()

cl <- makeCluster(4)

registerDoSNOW(cl)

pb <- txtProgressBar(max = length(streakIDs), style = 3)


progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

system.time(  
  streaksPrimary <- foreach(i = 1:length(streakIDs), 
                            .combine = 'rbind', 
                            .options.snow = opts,  
                            .packages = c('jsonlite','dplyr','stringr', 'lubridate','data.table')
  ) %dopar% {
    
    streakId <- streakIDs[i]
    
    return (processStreak(streakId, vdt))
    
  }
)

close(pb)
stopCluster(cl)  

rm(i, processStreak, cp, pb, streakIDs, streakId, st)




#============================================================================================

# tutaj rejestrujemy, w których momentach nie mieliśmy w ogóle obserwacji, żeby nie uzupełnić
# tam dziur fałszywymi wartościami
NO_HOURS <- setdiff(seq(min(vozilla$date),max(vozilla$date), by="min"), vozilla %>% select(date) %>% distinct() %>% pull )


addMissingEntriesPrimary <- function(carSideNumber){ 
  addMissingEntries(carSideNumber, streaksPrimary)
}


addMissingEntries <- function(carSideNumber, streakCollection){
  
  out <- streakCollection[FALSE,]
  
  events <- streakCollection %>% filter(sideNumber==carSideNumber) %>% arrange(dateStart)
  
  if (nrow(events)<2){
    return (out)
  }
  
  for (i in 1:(nrow(events)-1)){
    e1 <- events[i,]
    e2 <- events[i+1,]
    
    if(difftime((e2$dateStart-60), (e1$dateLast+60), unit="mins")>0){
      
      newStatus <- ifelse( 
        (e2$batStart - e1$batEnd)>2 | difftime((e2$dateStart-60), (e1$dateLast+60), unit="mins")>54 ,
        "UNAVAILABLE", 
        "RENTED")
      
      tryCatch({
        x <- streakCollection[FALSE,] # this is a workaround to preserve status factor levels
        x[nrow(x)+1,] <- data.frame(
          streakid = 0, # TEMPORARY ONLY, TO BE CALCULATED
          sideNumber = e1$sideNumber,
          latStart = e1$latEnd,
          longStart = e1$longEnd,
          latEnd = e2$latStart,
          longEnd = e2$longStart,
          batStart = e1$batEnd,
          batEnd = e2$batStart,
          batGain = e2$batStart - e1$batEnd,
          duration = difftime((e2$dateStart-60), (e1$dateLast+60), unit="mins")+1,
          minDayStart = hour(e1$dateLast+60)*60+minute(e2$dateStart-60),
          dateStart = e1$dateLast+60,
          dateLast = e2$dateStart-60,
          name = e1$name,
          status = newStatus 
        )
      }, error = function(e){
        stop(paste("Cannot parse carnumber",carSideNumber, ", i=",i, "  e1$dateLast=", e1$dateLast, "  e=", e))
      } )
      
      newStatusLevel = as.numeric(x$status)
      
      newStreakId <-               
        carSideNumber + 
        10000 * as.integer( newStatusLevel ) + 
        1000000*( floor(e1$streakid/1000000) )
      
      while ( streakCollection %>% filter(streakid==newStreakId) %>% nrow() > 0 ){
        newStreakId <- newStreakId + 1000000
        #warning(paste("Warning, new streak id already exists, bumping: ", newStreakId, ", i=", i))
      }
      
      x$streakid <- newStreakId
      
      
      out <- out %>% rbind(x)
    }
    
  }
  
  for (x in NO_HOURS){
    out %<>% filter( x<dateStart | dateLast<x )
  }
  
  
  return(out)
  
}

processAllFactors <- function(){
  vvProcessed <- vozilla[FALSE,]
  
  registerDoParallel()
  cl <- makeCluster(4)
  registerDoSNOW(cl)
  
  pb <- txtProgressBar(max = 200, style = 3)
  
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  x <- data.frame()
  
  system.time(
    x <- foreach(i = 1:200, 
                 .combine = 'rbind', 
                 .options.snow = opts, 
                 .packages = c('jsonlite','dplyr','stringr', 'lubridate', 'magrittr'), 
                 .export= ls(envir=globalenv())
    ) %dopar% {
      
      addMissingEntriesPrimary(i)
      
    }
  )
  
  close(pb)
  stopCluster(cl)   
  
  return(x)  
  
}

system.time( streaksSecondary <- processAllFactors() )

rm (addMissingEntries, addMissingEntriesPrimary, processAllFactors, cl, opts, progress)



#============================================================================================

streaksAll <- streaksPrimary %>% rbind(streaksSecondary)

streaksAll <- streaksAll %>% group_by(sideNumber) %>% arrange(dateStart) %>% mutate(statusPrev = lag(status), statusNext=lead(status)) %>% ungroup


Sys.time()

