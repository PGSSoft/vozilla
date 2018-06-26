library(purrr)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(doParallel)
library(doSNOW)
library(magrittr)
library(data.table)
library(ggmap)
library(raster)
library(zoo)
library(scales)
library(viridis)


myggsave = function(filename, width, height, dpi){
  ggsave(paste("images/",language, filename, sep=""), width=width, height=height, dpi=100)
  ggsave(paste("images/BIG",language,filename, sep=""), width=width, height=height, dpi=300)
}

language <- "PL"
#language <- "EN"

lang = function(strPL, strEN){
  if (!exists("language")){
    strPL
  } else {
    if (language != "en" & language!="EN" ){
      strPL
    } else {
      strEN
    }
  }
}

#===========================================================================================================
# Liczba samochodów we flocie Vozilli

vozilla %>% group_by(sideNumber) %>%            
  summarize(day=min(as.Date(date, tz=""))) %>%  
  ungroup %>% dplyr::select(day) %>%
  group_by(day) %>%
  summarize(n=n()) %>% mutate(nsum = cumsum(n) ) %>%      
  full_join ( data.frame(vozilla %>% mutate(day = as.Date(date, tz="")) %>% dplyr::select(day) %>% distinct() )  )  %>% arrange(day) %>%
  mutate(n=ifelse(is.na(n),0,n), nsum=na.locf(nsum, na.rm=FALSE)) %>%
  ggplot(aes(x=day, y=nsum)) + geom_line() + geom_point() + theme_minimal() + 
  geom_vline(col="red", xintercept=ymd("2017-12-01"), size=1) +
  geom_vline(col="red", xintercept=ymd("2018-03-01"), size=1) +   
  labs(title=lang("Liczba samochodów we flocie Vozilli", "Cars in the Vozilla fleet"), 
       subtitle = lang(
         "liczony jest dzień pierwszego pojawienia się na liście aut, z dowolnym statusem", 
         "the day on which a car first appeared on the vehicles list with any status"),
       x=NULL, y=lang("Liczba samochodów", "No. of cars")) + 
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) + 
  scale_y_continuous(breaks=seq(0,200,10)) +  
  annotate("text", x=as.Date("2017-12-01"), y=190, label=lang("0,50 zł/min","PLN 0.50/min"), color="red", hjust=1.2, vjust=-1) +
  annotate("text", x=as.Date("2017-12-01"), y=190, label=lang("0,75 zł/min","PLN 0.75/min"), color="red", hjust=-0.2, vjust=-1) +
  annotate("text", x=as.Date("2018-03-01"), y=190, label=lang("0,90 zł/min","PLN 0.90/min"), color="red", hjust=-0.2, vjust=-1) +    
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

myggsave("01.png", width = 8.5, height=6, dpi=100)




#===========================================================================================================
# Liczba samochodów dostępnych w danym dniu do wypożyczenia

vozilla %>% filter(status=="AVAILABLE") %>%     
  group_by(day = as.Date(date, tz="")) %>%      
  summarise(n=n_distinct(sideNumber)) %>%  
  ggplot(aes(x=day, y=n)) + geom_point()  +geom_line() + theme_minimal() + 
  geom_vline(col="red", xintercept=ymd("2017-12-01"), size=1) +
  geom_vline(col="red", xintercept=ymd("2018-03-01"), size=1) +   
  labs(title=lang(
    "Liczba samochodów dostępnych w danym dniu do wypożyczenia",
    "Number of cars available for rent on a given day"), 
    x=NULL, 
    y=lang("Liczba samochodów", "No. of cars")) + 
  scale_y_continuous(breaks=seq(0,200,10)) +  
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) + 
  annotate("text", x=as.Date("2017-12-01"), y=150, label=lang("0,50 zł/min","PLN 0.50/min"), color="red", hjust=1.2, vjust=-1) +
  annotate("text", x=as.Date("2017-12-01"), y=150, label=lang("0,75 zł/min","PLN 0.75/min"), color="red", hjust=-0.2, vjust=-1) +
  annotate("text", x=as.Date("2018-03-01"), y=150, label=lang("0,90 zł/min","PLN 0.90/min"), color="red", hjust=-0.2, vjust=-1) +  
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

myggsave("02.png", width = 8.5, height=6, dpi=100)

#===========================================================================================================
# Dzienne wypożyczenia całej floty, sumarycznie w minutach

vozilla %>% filter(status=="RENTED") %>%    
  group_by(day = as.Date(date, tz="")) %>%  
  summarise(n=n()) %>%              
  ggplot(aes(x=day, y=n)) + geom_line() + geom_point() + theme_minimal() + 
  geom_vline(col="red", xintercept=ymd("2017-12-01"), size=1) +
  geom_vline(col="red", xintercept=ymd("2018-03-01"), size=1) +    
  labs(title=lang(
    "Dzienne wypożyczenia całej floty, sumarycznie w minutach",
    "Daily renting events of the whole fleet; totals in minutes"), 
    x=NULL, 
    y=lang("Minuty wypożyczeń", "Minutes") ) + 
  scale_x_date(date_breaks = "week", minor_breaks =  NULL, limits = c(as.Date(min(vozilla$date)), as.Date( max(vozilla$date)))) + 
  annotate("text", x=as.Date("2017-12-01"), y=26500, label=lang("0,50 zł/min","PLN 0.50/min"), color="red", hjust=1.2, vjust=-1) +
  annotate("text", x=as.Date("2017-12-01"), y=26500, label=lang("0,75 zł/min","PLN 0.75/min"), color="red", hjust=-0.2, vjust=-1) +
  annotate("text", x=as.Date("2018-03-01"), y=26500, label=lang("0,90 zł/min","PLN 0.90/min"), color="red", hjust=-0.2, vjust=-1) +  
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

myggsave("03.png", width = 8.5, height=6, dpi=100)


#===========================================================================================================
# Lokalizacja miejsc ładowania samochodów Vozilli

charging <- vozilla %>% filter(status=="UNAVAILABLE") %>% 
  mutate(batteryLevelPctPrev = lag(batteryLevelPct)) %>% 
  filter(batteryLevelPct > batteryLevelPctPrev+25)

wroclaw <- get_map(location  = c(mean(charging$long), mean(charging$lat)), zoom = 12, color="bw", crop = TRUE)

ggmap(wroclaw) + 
  stat_density_2d(data = charging, aes(x=long, y=lat, fill=..level.., alpha=..level../10), geom="polygon", bins=10, show.legend = FALSE)+ 
  geom_density_2d(data = charging, aes(x=long, y=lat), bins=10, show.legend = FALSE)+ 
  scale_fill_gradient(low="white", high="red") +
  labs(title=lang( 
    "Lokalizacja miejsc ładowania samochodów Vozilli", "Vozilla car battery charging stations"), 
    x=NULL, 
    y=NULL)

myggsave("04.png", width = 8.5, height=6, dpi=100)

rm (charging, wroclaw)

#===========================================================================================================
# jazda do Wałbrzycha streak 25164050020018

walbrzychTrip <- vozilla %>% filter(streakid==25164050020018) %>% dplyr::select(lat, long, batteryLevelPct)

mapka <- get_map(location  = c(mean(walbrzychTrip$long)+0.1, mean(walbrzychTrip$lat)), zoom = 10, color="bw")
ggmap(mapka) + coord_map() + geom_path(data = walbrzychTrip, aes(x=long, y=lat, col=batteryLevelPct), size=1.2) +
  scale_color_gradient(low = "red", high = "#3cb44b", name=lang("Stan baterii", "Battery\ncharge\nlevel"), labels = scales::unit_format(unit="%", sep="")  ) +
  labs(title=lang("Jedna z wycieczek zakończonych poza granicami Wrocławia","One of the trips outside Wrocław"), 
    x=NULL, y=NULL)

myggsave("05.png", width = 8.5, height=6, dpi=100)

rm(walbrzychTrip, mapka)


#===========================================================================================================
# "Wycieczki poza miasto zakończone w pobliżu startu"

drawMap <- function(streaks){
  colfunc<-colorRampPalette(c("red","green"))
  center = c(17.11088, 51.05554)
  wroclaw <- get_map(location  = center, zoom = 10, color="bw")
  map <- ggmap(wroclaw) + scale_color_gradient(low = "red", high = "#3cb44b", name=lang("Stan baterii", "Battery\ncharge\nlevel"), labels = scales::unit_format(unit="%", sep="")  ) +
    labs(title=lang("Wycieczki poza miasto zakończone w pobliżu startu","Trips outside the city finished near the start"), 
         x=NULL, y=NULL)
  
  for (streak in streaks){
    mapkaloc <- vozilla %>% filter(streakid==streak) %>% dplyr::select(lat, long, batteryLevelPct)
    map <- map + geom_path(data = mapkaloc, aes(x=long, y=lat, col=batteryLevelPct), size=1.2)
  }

  return (map)
}

drawMap(c(25190941020038, 25183055020124, 25164649020047, 25183345020054, 25172485020050))

myggsave("06.png", width = 8.5, height=6, dpi=100)

rm(drawMap, map)

#===========================================================================================================

maxSpeed <- function(streaks, minutes){
  
  out <- data_frame(1, 2)
  out <- out[FALSE,]
  outp1 <- 0
  outp2 <- 0
  
  for (streak in streaks){
    
    vs <- vozilla %>% filter(streakid == streak) %>% arrange(date)
    
    if (nrow(vs)>=minutes){
      
      maximum <- 0
      
      for (i in 1:(nrow(vs)-minutes)){
        
        p1 <- vs[i,]
        p2 <- vs[i+minutes,]
        
        maxCandidate <- (pointDistance(c(p1$long,p1$lat), c(p2$long,p2$lat), TRUE) / minutes /1000)*60
        if (maximum < maxCandidate){
          maximum <- maxCandidate
          outp1 <- p1$date
          outp2 <- p2$date
        }
        
      }
      
      out <- out %>% rbind( data_frame(streak, maximum, outp1, outp2) )
    }
  }
  return (out)
  
}

##wizualizacja na mapie:
drawMap <- function(streak){
  colfunc<-colorRampPalette(c("red","green"))
  mapka <- vozilla %>% filter(streakid==streak) %>% dplyr::select(lat, long, batteryLevelPct)
  mapka1 <- mapka[1,]
  center = c(mean(mapka$long), mean(mapka$lat))
  wroclaw <- get_map(location  = center, zoom = 11, color="bw")
  map <- ggmap(wroclaw) + coord_map() + geom_path(data = mapka, aes(x=long, y=lat, col=batteryLevelPct), size=1.2) +
    scale_color_gradient(low = "red", high = "#3cb44b", name=lang("Stan baterii", "Battery\ncharge\nlevel"), labels = scales::unit_format(unit="%", sep="")  ) +
    geom_point(data = mapka,aes(x=long, y=lat), col="blue") +
    labs(title=lang("Przykład szybkiej jazdy na Autostradowej Obwodnicy Wrocławia","Fast drive on the Wrocław Motorway Ring"),
         x=NULL, y=NULL)

  return (map)
}

#rysowanie wyniku prędkości
drawResult <- function(n){
  speed <- n
  mapp <- drawMap(speed$streak)
  p1 <- vozilla %>% filter(streakid==speed$streak & date==speed$outp1 )
  p2 <- vozilla %>% filter(streakid==speed$streak & date==speed$outp2 )
  mapp + geom_point(data = p1,aes(x=long, y=lat), col="purple", size=4) +
    geom_point(data = p2,aes(x=long, y=lat), col="purple", size=4)
}

drawResult(maxSpeed(25191010020103, 5))

myggsave("07.png", width = 8.5, height=6, dpi=100)

rm (drawMap, drawResult, maxSpeed)


#===========================================================================================================

set.seed(42)

vozilla %>%
  sample_n(25000) %>% 
  ggplot( aes( x = batteryLevelPct, y = rangeKm )) + 
  geom_jitter(alpha=0.07, width=1.5) + 
  theme_minimal() + 
  labs(
    title=lang("Prognozowany zasięg w zależności od naładowania baterii","Range forecast vs battery charge level"), 
    x=lang("Poziom naładowania baterii (%)", "Battery charge level (%)"), 
    y=lang("Zasięg samochodu (km)", "Range (km)")) 
myggsave("08.png", width = 8.5, height=6, dpi=100)

#===========================================================================================================


set.seed(42)

vozilla %>% filter(status!="RETURNED") %>%
  group_by(status) %>%
  sample_n(5000) %>%  
  ungroup() %>%
  mutate(week = week(date)) %>%
  ggplot( aes( x = batteryLevelPct, y = rangeKm )) + 
  geom_jitter(alpha=0.1) +  
  facet_wrap(~status, ncol = 2) +
  theme_minimal() + 
  labs(
    title=lang("Prognozowany zasięg w zależności od naładowania baterii i statusu","Range forecast vs battery charge level and status"), 
    x=lang("Poziom naładowania baterii (%)", "Battery charge level (%)"), 
    y=lang("Zasięg samochodu (km)", "Range (km)")) 

myggsave("09.png", width = 8.5, height=6, dpi=100)

#===========================================================================================================

set.seed(1)

vozilla %>% filter (status %in% c("AVAILABLE", "RENTED")) %>%
  sample_n(500000) %>%  
  ggplot( aes( x = batteryLevelPct, y = rangeKm )) +  
  stat_density2d(aes(fill = ..level..), n = 100, geom="polygon", contour = TRUE) + 
  guides(fill = FALSE) + 
  scale_fill_viridis(direction = -1, option = "inferno") +
  theme_minimal() + 
  labs(
    title=lang("Prognozowany zasięg dla samochodów gotowych do jazdy","Range forecast for cars ready for driving"), 
    x=lang("Poziom naładowania baterii (%)", "Battery charge level (%)"), 
    y=lang("Zasięg samochodu (km)", "Range (km)") ) 

myggsave("10.png", width = 8.5, height=6, dpi=100)

#===========================================================================================================

tempMinus <- -12
tempPlus  <-  25

ggplot(temperature, aes(x=yyyymmddhh, y=temperature)) + 
  geom_line(col="gray") + 
  geom_point(data = temperature %>% filter(temperature > tempPlus), aes(x=yyyymmddhh, y=temperature), color="red", size=1.7) + 
  geom_point(data = temperature %>% filter(temperature < tempMinus), aes(x=yyyymmddhh, y=temperature), color="blue", size=1.7) + 
  geom_line(aes(x=yyyymmddhh, y=rollmean(temperature,24,fill = list(NA, NULL, NA))), col="black") + 
  theme_minimal() + 
  scale_x_datetime(labels = date_format("%Y-%m-%d"), date_breaks = "week", minor_breaks =  NULL) +  
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title=lang("Temperatura powietrza w badanym okresie","Air temperature during the studied period"), 
       x=NULL, 
       y=lang("Temperatura (°C)","Temperature (°C)")) 

myggsave("11.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================

set.seed(42)

vozilla %>% 
  filter (status %in% c("AVAILABLE", "RESERVED")) %>% 
  filter(temperature < tempMinus | temperature>tempPlus) %>% 
  mutate( temperature2 = floor(temperature/5)*5) %>% group_by(temperature2) %>% 
  sample_n(10000) %>% ungroup() %>% sample_n(20000) %>% 
  ggplot( aes( x = batteryLevelPct, y = rangeKm, col=temperature )) + 
  geom_jitter(size=0.2, width = 3, height = 3) + 
  theme_minimal() + 
  scale_color_gradient(lang("Temperatura","Temperature"), low="blue", high="red") +
  labs(title=lang("Prognozowany zasięg dla samochodów gotowych do jazdy","Range forecast for cars ready to drive"),
       subtitle = paste(
         lang("dla temperatur poniżej","for temperatures below"), 
         tempMinus, 
         lang("i powyżej","and above"), 
         tempPlus, 
         lang("stopni Celsjusza","°C")),
       x=lang("Poziom naładowania baterii (%)", "Battery charge level (%)"), 
       y=lang("Zasięg samochodu (km)", "Range (km)") ) 

myggsave("12.png", width = 8.5, height=6, dpi=100)


#===========================================================================================================

set.seed(4321)

vozilla %>% 
  filter( rangeKm > batteryLevelPct/2 ) %>% 
  filter(status %in% c("AVAILABLE", "RESERVED")) %>% 
  filter(temperature < tempMinus | temperature > tempPlus)  %>% 
  mutate( temperature2 = as.factor( floor(temperature/5)*5)) %>% 
  group_by(temperature2) %>% 
  sample_n(40000) %>% 
  group_by(temperature2) %>% 
  ggplot( aes( x = batteryLevelPct, y = rangeKm)) + 
  stat_density2d(geom="density2d", aes(color = temperature2, alpha=2*..level..), size=1, contour=TRUE) + 
  guides( alpha=FALSE ) + 
  scale_color_manual(lang("Temperatura","Temperature"), 
                     labels = c(paste(lang("poniżej","below"), tempMinus,"°C"), paste(lang("powyżej","above"),tempPlus,"°C")), values = c("blue", "red")) + 
  theme_minimal() + 
  stat_smooth(method="auto", aes(color=temperature2), se=FALSE, size=1.2) + 
  geom_polygon(data = data.frame(x=c(0,100,100), y=c(0,0,50)), aes(x=x,y=y), fill="orange", alpha=0.2)+
  labs(title=lang("Prognozowany zasięg dla samochodów gotowych do jazdy","Range forecast for cars ready to drive"),
       subtitle = paste(
         lang("dla temperatur poniżej","for temperatures below"), 
         tempMinus, 
         lang("i powyżej","and above"), 
         tempPlus, 
         lang("stopni Celsjusza","°C")),
       x=lang("Poziom naładowania baterii (%)", "Battery charge level (%)"), 
       y=lang("Zasięg samochodu (km)", "Range (km)") )

myggsave("13.png", width = 8.5, height=6, dpi=100)

#===========================================================================================================


streaksBool <- streaksAll %>% filter(status %in% c("RENTED","UNAVAILABLE")) %>% filter(dateStart < ymd("2017-12-05")) %>% mutate(serviced = status=="UNAVAILABLE", durationMin = as.integer(duration))
streaksBool$status <- NULL
streaksBool$sideNumber <- NULL
streaksBool$dateStart <- NULL
streaksBool$dateLast<- NULL
streaksBool$streakid<- NULL
streaksBool$statusPrev <- NULL
streaksBool$statusNext <- NULL


streaksStree <- FFTrees(data = streaksBool, formula=serviced ~ ., decision.labels=c("Rent", "Service"))
plot(streaksStree)

png(filename="images/14.png", width = 850, height=600)
plot(streaksStree)
dev.off()

rm (streaksBool, streaksStree)

#===========================================================================================================
# przygotowanie danych do następnych wykresów

# wypożyczenia wg dni
days <- vozilla %>% mutate(date = as.Date(date, tz="")) %>% dplyr::select(date) %>% distinct() %>% arrange(date) %>% pull
dayRents <- data.frame(
  day=vozilla %>% mutate(date = as.Date(date, tz="")) %>% dplyr::select(date) %>% distinct() %>% arrange(date) %>% pull, 
  rents = 0,
  reservations = 0,
  cars = vozilla %>% group_by(date = as.Date(date, tz="")) %>% filter(status=="AVAILABLE") %>% summarize(cars=n_distinct(sideNumber)) %>% dplyr::select (cars) %>% pull
)

for (n in 1:nrow(streaksAll)){
  row <- streaksAll[n,]
  if (row$status!="RENTED" & row$status!="RESERVED"){
    next
  }
  if (row$status=="RENTED"){
    if (yday(row$dateStart)==yday(row$dateLast)){
      dayRents[dayRents$day==as.Date(row$dateStart, tz=""),]$rents <- dayRents[dayRents$day==as.Date(row$dateStart, tz=""),]$rents + as.numeric(row$duration)
    } else {
      dayRents[dayRents$day==as.Date(row$dateStart, tz=""),]$rents <- dayRents[dayRents$day==as.Date(row$dateStart, tz=""),]$rents+ as.numeric(abs(difftime(row$dateStart, ceiling_date(row$dateStart, unit = "days") , unit="mins")))
      dayRents[dayRents$day==as.Date(row$dateLast, tz=""),]$rents <- dayRents[dayRents$day==as.Date(row$dateLast, tz=""),]$rents + 1 + as.numeric(abs(difftime(row$dateLast, floor_date(row$dateLast, unit = "days") , unit="mins")))
    }
  }
  if (row$status=="RESERVED"){
    if (yday(row$dateStart)==yday(row$dateLast)){
      dayRents[dayRents$day==as.Date(row$dateStart, tz=""),]$reservations <- dayRents[dayRents$day==as.Date(row$dateStart, tz=""),]$reservations + as.numeric(row$duration)
    } else {
      dayRents[dayRents$day==as.Date(row$dateStart, tz=""),]$reservations <- dayRents[dayRents$day==as.Date(row$dateStart, tz=""),]$reservations+ as.numeric(abs(difftime(row$dateStart, ceiling_date(row$dateStart, unit = "days") , unit="mins")))
      dayRents[dayRents$day==as.Date(row$dateLast, tz=""),]$reservations <- dayRents[dayRents$day==as.Date(row$dateLast, tz=""),]$reservations + 1 + as.numeric(abs(difftime(row$dateLast, floor_date(row$dateLast, unit = "days") , unit="mins")))
    }
  }
}

dayRents$carsAll <- vozilla %>% group_by(sideNumber) %>%        
  summarize(day=min(as.Date(date, tz=""))) %>% 
  ungroup %>% dplyr::select(day) %>% 
  group_by(day) %>% 
  summarize(n=n()) %>% mutate(nsum = cumsum(n) ) %>%  
  full_join ( data.frame(vozilla %>% mutate(day = as.Date(date, tz="")) %>% dplyr::select(day) %>% distinct() )  )  %>% arrange(day) %>% 
  mutate(n=ifelse(is.na(n),0,n), nsum=na.locf(nsum, na.rm=FALSE)) %>% pull (nsum)

rm (n)

#===========================================================================================================


# wykres wypożyczeń wg dni
ggplot(dayRents, aes(x=day, y=rents))+
  geom_smooth(fill="lightgray") + 
  geom_line()+
  geom_point() + 
  theme_minimal() +
  geom_vline(col="red", xintercept=ymd("2017-12-01"), size=1) +
  geom_vline(col="red", xintercept=ymd("2018-03-01"), size=1) +   
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) +   
  annotate("text", x=as.Date("2017-12-01"), y=25000, label=lang("0,50 zł/min","PLN 0.50/min"), color="red", hjust=1.5, vjust=-1) +
  annotate("text", x=as.Date("2017-12-01"), y=25000, label=lang("0,75 zł/min","PLN 0.75/min"), color="red", hjust=-0.2, vjust=-1) +
  annotate("text", x=as.Date("2018-03-01"), y=25000, label=lang("0,90 zł/min","PLN 0.90/min"), color="red", hjust=-0.2, vjust=-1) +  
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title=
         lang("Dzienne sumaryczne wypożyczenia samochodów Vozilli","Daily renting events of Vozilla cars"), 
       x=NULL, y=lang("Minuty", "Minutes")) 

myggsave("15.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================

ggplot(dayRents, aes(x=day, y=(rents/carsAll)))+
  geom_vline(col="red", xintercept=ymd("2017-12-01"), size=1) +
  geom_vline(col="red", xintercept=ymd("2018-03-01"), size=1) +   
  geom_smooth(fill="lightgray") + 
  geom_line()+
  geom_point() + 
  theme_minimal() +
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) +   
  annotate("text", x=as.Date("2017-12-01"), y=189, label=lang("0,50 zł/min","PLN 0.50/min"), color="red", hjust=1.4, vjust=-1) +
  annotate("text", x=as.Date("2017-12-01"), y=189, label=lang("0,75 zł/min","PLN 0.75/min"), color="red", hjust=-0.2, vjust=-1) +
  annotate("text", x=as.Date("2018-03-01"), y=189, label=lang("0,90 zł/min","PLN 0.90/min"), color="red", hjust=-0.2, vjust=-1) +  
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title=
         lang("Dzienne wypożyczenia aut Vozilli w przeliczeniu na jedno auto floty","Daily renting events of Vozilla cars per one fleet car"),
         x=NULL, y=lang("Minuty", "Minutes"))

myggsave("16.png", width = 8.5, height=4, dpi=100)


#===========================================================================================================

#liczba wypożyczeń jednego jeżdżącego auta dziennie
streaksAll %>% 
  filter(status=="RENTED") %>% 
  group_by(day=as.Date(dateStart, tz="")) %>% 
  summarize(n=n_distinct(streakid)/n_distinct(sideNumber)) %>% 
  ggplot(aes(x=day, y=n))+
  geom_vline(col="red", xintercept=ymd("2017-12-01"), size=1) +
  geom_vline(col="red", xintercept=ymd("2018-03-01"), size=1) +    
  geom_smooth(fill="lightgray") + 
  geom_line()+
  geom_point() + 
  annotate("text", x=as.Date("2017-12-01"), y=7.19, label=lang("0,50 zł/min","PLN 0.50/min"), color="red", hjust=1.2, vjust=-1) +
  annotate("text", x=as.Date("2017-12-01"), y=7.19, label=lang("0,75 zł/min","PLN 0.75/min"), color="red", hjust=-0.2, vjust=-1) +
  annotate("text", x=as.Date("2018-03-01"), y=7.19, label=lang("0,90 zł/min","PLN 0.90/min"), color="red", hjust=-0.2, vjust=-1) +  
  theme_minimal() +
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) +   
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title=lang("Średnia liczba wypożyczeń jednego dostępnego samochodu","Average number of renting events of one available car"), 
       subtitle=lang("średnia dla samochodów, które były danego dnia wypożyczane co najmniej raz","mean for cars that were rented at least once on a given day"),
       x=NULL, y=lang("Liczba wypożyczeń","Renting events"))

myggsave("17.png", width = 8.5, height=4, dpi=100)


#===========================================================================================================

streaksAll %>% 
  filter(status=="RENTED") %>% 
  group_by(day=as.Date(dateStart, tz="")) %>% 
  summarize(n=n_distinct(streakid)) %>% 
  cbind(carsAll=dayRents$carsAll) %>% 
  mutate(n=n/carsAll) %>% 
  ggplot(aes(x=day, y=n))+
  geom_vline(col="red", xintercept=ymd("2017-12-01"), size=1) +
  geom_vline(col="red", xintercept=ymd("2018-03-01"), size=1) +    
  geom_smooth(fill="lightgray") + 
  geom_line()+
  geom_point() + 
  annotate("text", x=as.Date("2017-12-01"), y=6.1, label=lang("0,50 zł/min","PLN 0.50/min"), color="red", hjust=1.2, vjust=-1) +
  annotate("text", x=as.Date("2017-12-01"), y=6.1, label=lang("0,75 zł/min","PLN 0.75/min"), color="red", hjust=-0.2, vjust=-1) +
  annotate("text", x=as.Date("2018-03-01"), y=6.1, label=lang("0,90 zł/min","PLN 0.90/min"), color="red", hjust=-0.2, vjust=-1) +  
  theme_minimal() +
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) +   
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title=lang("Średnia liczba wypożyczeń jednego samochodu","Average number of renting events of one car"), 
       subtitle=lang("średnia dla wszystkich samochodów włączonych do floty","mean for all cars in the fleet"),
       x=NULL, y=lang("Liczba wypożyczeń","Renting events"))

myggsave("18.png", width = 8.5, height=4, dpi=100)


#===========================================================================================================

library(alluvial)

allu <- streaksAll %>% 
  filter(status != "RETURNED" & statusNext != "RETURNED" & !is.na(statusNext) & status!=statusNext) %>% 
  group_by(status, statusNext) %>% 
  summarize(n=n()) 

png(filename="images/19.png", width = 850, height=600)
alluvial(allu[,1:2], freq=allu$n, col=ifelse(allu$status=="AVAILABLE", "green","gray"), 
         axis_labels = c("Status obecny", "Status kolejny" ) )
dev.off()

png(filename="images/EN19.png", width = 850, height=600)
alluvial(allu[,1:2], freq=allu$n, col=ifelse(allu$status=="AVAILABLE", "green","gray"), 
         axis_labels = c("Current status", "Next status" ) )
dev.off()

#===========================================================================================================

medianReservation <- streaksAll %>% filter(status=="RESERVED"& duration<=30) %>% pull(duration) %>% median %>% as.integer

streaksAll %>% 
  group_by(sideNumber) %>% 
  filter(!is.na(statusPrev)) %>% 
  ungroup %>% 
  filter(status=="RESERVED" & duration<=1500) %>% 
  dplyr::select(duration) %>% 
  mutate(duration = ifelse(duration>30,30,duration)) %>%
  ggplot(aes(x=duration)) + 
  theme_minimal() +
  geom_histogram(binwidth = 1, alpha=0.5, fill="red") + 
  geom_vline(data=streaksAll, aes(xintercept = medianReservation ), color="darkred")  +
  scale_x_continuous(breaks=seq(0,30,5), minor_breaks = seq(0,30,1)) +  
  annotate("text", x=medianReservation, y=17000, label=paste(lang("Mediana = ", "Median = "),medianReservation), color="darkred", hjust=1, vjust=-0.5, angle = 90 ) +
  labs(
    title=lang("Rozkład długości rezerwacji","Reservation duration distribution"),
    x=lang("Czas trwania rezerwacji (minuty)", "Reservation duration (minutes)"),
    y=lang("Liczba rezerwacji", "Reservations") )

myggsave("20.png", width = 8.5, height=4, dpi=100)



#===========================================================================================================


allu2 <- streaksAll %>% 
  filter(status == "RESERVED" & statusNext %in% c("RENTED", "AVAILABLE") & !is.na(statusNext)) %>% 
  mutate(Text = ifelse(duration>=15, 
                       lang("Rezerwacja\ntrwająca\n15 minut\nlub więcej", "Reservation\nlasting 15\nminutes or\nlonger"),
                       lang("Rezerwacja\ntrwająca\nponiżej\n15 minut","Reservation\nshorter than\n15 minutes") ), 
         ReservedMax=duration>=15) %>% 
  group_by(Text, statusNext,ReservedMax) %>% 
  summarize(n=n()) 

png(filename="images/21.png", width = 850, height=400)
alluvial(allu2[,1:2], freq=allu2$n, col=ifelse(allu2$ReservedMax=="TRUE", "red","green"), 
         axis_labels = c(
           "Czas trwania rezerwacji", 
           "Status po rezerwacji" ))
dev.off()

png(filename="images/EN21.png", width = 850, height=400)
alluvial(allu2[,1:2], freq=allu2$n, col=ifelse(allu2$ReservedMax=="TRUE", "red","green"), 
         axis_labels = c(
           "Reservation duration", 
           "Status after reservation"  ))
dev.off()


#===========================================================================================================

streaksAll %>% 
  filter(status == "RESERVED" & statusNext != "RETURNED" & !is.na(statusNext)) %>% 
  mutate(duration = ifelse(duration>=15,15,duration), ThenRented=statusNext=="RENTED") %>% 
  group_by(duration, ThenRented) %>% 
  ggplot(aes(x=duration, group=ThenRented, fill=ThenRented))+
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,15,1), minor_breaks = seq(0,15,1)) +  
  scale_y_continuous(labels=scales::percent_format()) +
  geom_bar(position = "fill", alpha=0.5) +
  scale_fill_manual(labels = c(lang("Nie", "No"), lang("Tak", "Yes") ), values = c("red", "green")) +
  labs(
    title=lang("Rezerwacje zakończone wypożyczeniem zależnie od czasu trwania rezerwacji","Reservations resulting in renting vs reservation duration"), 
    subtitle=lang("rezerwacje dłuższe niż 15 minut zgrupowano w ostatniej kolumnie","reservations longer than 15 minutes are grouped in the last column"),
    x=lang("Czas trwania rezerwacji (minuty)", "Reservation duration (minutes)"),
    y=lang("Udział procentowy", "Share in per cent"),
    fill=lang("Rezerwacja\nzakończona\nwypożyczeniem","Reservation\nresults in\nrenting")   )

myggsave("22.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================

streaksAll %>% 
  filter(status=="RENTED" & duration<=600) %>% 
  group_by(hour = hour(dateStart)) %>% 
  ggplot()+ 
  theme_minimal() +
  geom_histogram(aes(x=hour, fill=..x..), binwidth = 1, col="white") +
  scale_x_continuous(breaks=seq(0,23,3), minor_breaks = seq(0,23,1)) +  
  scale_fill_gradient(low = "blue", high = "darkblue", guide=FALSE) +
  labs(
    title=lang("Liczba wypożyczeń w zależności od godziny","Reservations vs time of the day"), 
    subtitle=lang("przypisano według godziny rozpoczęcia wypożyczenia","assigned according to the reservation start time"),
    x=lang("Godzina zegarowa", "Hour"),
    y=lang("Liczba wypożyczeń", "Number of renting events") )

myggsave("23.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================

streaksAll %>% 
  filter(status=="RENTED" & duration<=600) %>% 
  group_by(hour = hour(dateStart), weekend=( lubridate::wday(dateStart, week_start = getOption("lubridate.week.start", 1)) >=6 ) ) %>% 
  ggplot(aes(x=hour))+ 
  geom_histogram(aes(fill=..x..), binwidth = 1, col="white") + 
  facet_wrap(~ifelse(!weekend, lang("Dni robocze","Weekdays"), lang("Sobota i niedziela", "Saturdays and Sundays")), scales = "free_y" ) +
  scale_x_continuous(breaks=seq(0,23,3), minor_breaks = seq(0,23,1)) +  
  scale_fill_gradient(low = "blue", high = "darkblue", guide=FALSE) +
  theme_minimal() +
  labs(
    title=lang("Liczba wypożyczeń wg godzin w dni robocze i weekendy", "Renting events vs time of the day on weekdays and weekends"), 
    subtitle=lang("przypisano według godziny rozpoczęcia wypożyczenia", "assigned according to the renting start time"),
    x=lang("Godzina zegarowa", "Hour"),
    y=lang("Liczba wypożyczeń", "Number of renting events") )

myggsave("24.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================

streaksAll %>% 
  filter(status=="RENTED" & batGain<0) %>% 
  count( weekday=lubridate::wday(dateStart, week_start = "1"), hour=hour(dateStart) ) %>% 
  ggplot() +
  geom_tile(aes(weekday, hour, fill = n)) +
  scale_fill_gradient(lang("Liczba wypożyczeń","Renting events"), low = "darkred", high = "yellow") +
  scale_x_continuous(breaks = 1:7, minor_breaks = NULL, 
                     labels=c(
                       lang("Poniedziałek", "Mon"),
                       lang("Wtorek", "Tue"), 
                       lang("Środa", "Wed"), 
                       lang("Czwartek", "Thu"), 
                       lang("Piątek", "Fri"), 
                       lang("Sobota", "Sat"), 
                       lang("Niedziela", "Sun")   )) +
  scale_y_continuous(breaks = seq(0, 23, 3), minor_breaks=seq(0, 23, 1), trans = "reverse") +
  theme_minimal() +
  labs(
    title=lang("Liczba wypożyczeń z podziałem na godziny i dni tygodnia", "Renting events vs time of day and day of the week"),
    subtitle=lang("przypisano według godziny rozpoczęcia wypożyczenia", "assigned according to the renting start time"),
    y=lang("Godzina zegarowa", "Hour"),
    x=NULL)


myggsave("25.png", width = 8.5, height=6, dpi=100)

#===========================================================================================================

medDur <- streaksAll %>% filter(status=="RENTED" & duration<=600) %>% pull(duration) %>% median %>% as.integer

streaksAll %>% 
  filter(status=="RENTED" & duration<=600) %>% 
  dplyr::select(duration) %>% 
  mutate(duration = ifelse(duration>120,120,duration)) %>% 
  ggplot(aes(x=duration)) + 
  geom_histogram(binwidth = 1, alpha=0.5, fill="red") + 
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,120,20)) +  
  geom_vline(data=streaksAll, aes(xintercept = medDur), colour="darkred") +
  annotate("text", x=medDur, y=250, label=paste(lang("Mediana = ", "Median = "),medDur), color="darkred", hjust=0, vjust=-0.5, angle = 90 ) +
  labs(
    title=lang("Rozkład czasu trwania wypożyczeń", "Distribution of renting event duration"),
    subtitle=lang("wypożyczenia dłuższe od 120 minut zostały dodane do ostatniej kolumny", "renting events longer than 120 minutes have been added to the last column"),
    x=lang("Czas trwania wypożyczenia (minuty)", "Renting event duration (minutes)"),
    y=lang("Liczba wypożyczeń", "Number of renting events") )

myggsave("26.png", width = 8.5, height=4, dpi=100)


#===========================================================================================================

streaksAll %>% 
  filter(status == "RENTED" & batGain<0 & duration<120) %>%
  group_by(hour=hour(dateStart)) %>% 
  ggplot(aes(x=hour, y=duration, group=hour))+
  geom_jitter(col="darkgray", alpha=0.04) + 
  geom_boxplot(col="blue", outlier.shape = NA) + 
  coord_cartesian(ylim=c(0,50)) +
  scale_x_continuous(breaks=seq(0,23,3), minor_breaks = seq(0,23,1)) +  
  theme_minimal() +
  labs(
    title=lang("Czas trwania wypożyczenia w zależności od pory dnia", "Renting event duration vs time of day"),
    y=lang("Czas trwania wypożyczenia (minuty)", "Renting event duration (minutes)"),
    x=lang("Godzina rozpoczęcia wypożyczenia", "Renting event start time") )


myggsave("27.png", width = 8.5, height=6, dpi=100)

#===========================================================================================================

streaksAll %>% 
  filter(status == "RENTED" & batGain<0 & duration<120) %>%
  group_by(hour=hour(dateStart), weekend=( lubridate::wday(dateStart, week_start = getOption("lubridate.week.start", 1)) >=6 ) ) %>% 
  ggplot(aes(x=hour, y=duration, group=hour))+
  geom_jitter(col="darkgray", alpha=0.04) + 
  geom_boxplot(col="blue", outlier.shape = NA) + 
  coord_cartesian(ylim=c(0,50)) +
  scale_x_continuous(breaks=seq(0,23,3), minor_breaks = seq(0,23,1)) +  
  theme_minimal() +
  labs(
    title=lang("Czas trwania wypożyczenia w zależności od pory dnia", "Renting event duration vs time of day"),
    y=lang("Czas trwania wypożyczenia (minuty)", "Renting event duration (minutes)"),
    x=lang("Godzina rozpoczęcia wypożyczenia", "Renting event start time") ) +
  facet_wrap(~ifelse(!weekend, lang("Dni robocze","Weekdays"), lang("Sobota i niedziela", "Saturdays and Sundays") ) ) 

myggsave("28.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================

streaksAll %>% 
  filter(status == "RENTED" & batGain<0 & duration<120) %>% 
  rowwise %>% 
  mutate(distance= pointDistance(c(longStart, latStart), c(longEnd, latEnd), TRUE )/1000) %>% 
  filter(distance>0.5) %>% 
  mutate(distance=ifelse(distance>20,20,distance)) %>% 
  ggplot()+
  geom_histogram(aes(x=distance, fill=..x..), binwidth = 1, color="white")+
  scale_fill_gradient(low = "blue", high = "darkblue", guide=FALSE) +
  scale_x_continuous(breaks=seq(1,20,1)) +  
  theme_minimal() +
  labs(
    title=lang("Odległość między miejscami początku i końca wypożyczenia", "Distance between the renting start and end points"),
    subtitle=lang("odległość liczona w linii prostej", "straight line distance"),
    y=lang("Liczba wypożyczeń", "Number of renting events"),
    x=lang("Dystans w kilometrach","Distance in km")  ) 

myggsave("29.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================


streaksAll %>% 
  filter(status == "RENTED" & batGain<0 & duration<120) %>% 
  rowwise %>% 
  mutate(distance= pointDistance(c(longStart, latStart), c(longEnd, latEnd), TRUE )/1000) %>% 
  filter(distance>0.5) %>% 
  group_by(hour=hour(dateStart)) %>% 
  ggplot(aes(x=hour, y=distance, group=hour))+
  geom_jitter(alpha=0.01) + 
  geom_boxplot(col="blue", outlier.shape = NA) + 
  coord_cartesian(ylim=c(0,12)) +
  scale_y_continuous(breaks=seq(0,12,2), minor_breaks = seq(0,12,1)) +  
  scale_x_continuous(breaks=seq(0,23,3), minor_breaks = seq(0,23,1)) +    
  theme_minimal() +
  labs(
    title=lang("Przebyty dystans w zależności od pory dnia", "Distance travelled vs time of day"),
    subtitle=lang("odległość liczona w linii prostej między miejscami początku i końca wypożyczenia", "straight line distance between the renting start and end points"),
    y=lang("Dystans w km", "Distance in km"),
    x=lang("Godzina rozpoczęcia wypożyczenia", "Renting event start time") )

myggsave("30.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================


streaksAll %>% 
  filter(status == "RENTED" & batGain<0 & duration<120) %>% 
  rowwise %>% 
  mutate(distance= pointDistance(c(longStart, latStart), c(longEnd, latEnd), TRUE )/1000) %>% 
  filter(distance>0.5) %>% 
  group_by(hour=hour(dateStart), weekend=( lubridate::wday(dateStart, week_start = getOption("lubridate.week.start", 1)) >=6 ) ) %>% 
  ggplot(aes(x=hour, y=distance, group=hour))+
  geom_jitter(alpha=0.01) + 
  geom_boxplot(col="blue", outlier.shape = NA) + 
  coord_cartesian(ylim=c(0,12)) +
  scale_y_continuous(breaks=seq(0,12,2), minor_breaks = seq(0,12,1)) +  
  scale_x_continuous(breaks=seq(0,23,3), minor_breaks = seq(0,23,1)) +  
  theme_minimal() +
  labs(
    title=lang("Przebyty dystans w zależności od pory dnia", "Distance travelled vs time of day"),
    subtitle=lang("odległość liczona w linii prostej między miejscami początku i końca wypożyczenia", "straight line distance between the renting start and end points"),
    y=lang("Dystans w km", "Distance in km"),
    x=lang("Godzina rozpoczęcia wypożyczenia", "Renting event start time") ) +
  facet_wrap(~ifelse(!weekend, lang("Dni robocze","Weekdays"), lang("Sobota i niedziela", "Saturdays and Sundays") ), scales = "free_y" ) +

myggsave("31.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================

streaksAll %>% 
  filter(status == "RENTED" & batGain<0 & duration<120) %>% 
  rowwise %>% 
  mutate(hour=floor(hour(dateStart)/6), distance= pointDistance(c(longStart, latStart), c(longEnd, latEnd), TRUE )/1000) %>% 
  filter(distance>0.5) %>% 
  mutate(distance=ifelse(distance>20,20,distance)) %>% 
  ggplot(aes(x=duration, y=distance))+
  geom_jitter(alpha=0.03) +
  stat_density2d(aes(fill=..level..), geom="polygon", n=100, alpha=0.5)+
  guides(fill = FALSE) +
  scale_fill_viridis(direction = -1, option = "inferno") +
  coord_cartesian(xlim=c(0,40), ylim=c(0,10)) +
  theme_minimal() +
  labs(
    title=lang("Zależność przebytego dystansu od czasu jazdy", "Distance travelled vs travel time"),
    subtitle=lang("odległość liczona w linii prostej", "straight line distance"),
    y=lang("Dystans w km", "Distance in km"),
    x=lang("Czas jazdy w minutach","Travel time in minutes")  ) 

myggsave("32.png", width = 8.5, height=6, dpi=100)


#===========================================================================================================

streaksAll %>%
  filter(batGain>20) %>% 
  mutate(serwis = hour(dateStart), trasa=hour(dateLast)) %>%
  dplyr::select (serwis, trasa) %>%
  melt %>%
  ggplot(aes(value, group=variable, fill=variable))+
  scale_x_continuous(breaks=seq(0,23,1), minor_breaks = seq(0,23,1) ) +
  geom_histogram(position = position_dodge(width = 0.5), binwidth = 1, alpha=0.5) +
  scale_fill_manual(
    lang("Zdarzenie\n","Event\n"), 
    labels = c(
      lang("Wyłączenie\nz ruchu\n","Withdrawal"), 
      lang("Wprowadzenie\ndo ruchu\n", "Introduction") ), 
    values = c("red", "green")) +
  theme_minimal() +
  labs(
    title=lang("Liczba aut wprowadzanych i wyłączanych z ruchu w kolejnych godzinach", "Number of cars introduced to and withdrawn from traffic in successive hours"),
    y=lang("Liczba zdarzeń", "Number of events"),
    x=lang("Godzina", "Hour")  ) 
  
myggsave("33.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================

histRed <- streaksAll %>% filter(batGain>5) %>% pull(batStart) %>% median
histGreen <- streaksAll %>% filter(batGain>5) %>% pull(batEnd) %>% median


streaksAll %>% 
  filter(batGain>5) %>% 
  dplyr::select(batStart, batEnd) %>% 
  ggplot() + 
  geom_histogram(aes(x=batStart), binwidth = 2, alpha=0.5, fill="red") + 
  geom_histogram(aes(x=batEnd), binwidth = 2, alpha=0.5, fill="green") + 
  geom_vline(data=streaksAll, aes(xintercept = histGreen), colour="darkgreen") + 
  geom_vline(data=streaksAll, aes(xintercept = histRed), colour="darkred") +
  annotate("text", x=histRed, y=310, label=paste(lang("Mediana = ", "Median = "),histRed), color="darkred", hjust=0, vjust=-0.5, angle = 90 ) +
  annotate("text", x=histGreen, y=310, label=paste(lang("Mediana = ", "Median = "),histGreen), color="darkgreen", hjust=0, vjust=-0.5, angle = 90 ) +
  theme_minimal() +
  scale_x_continuous(breaks=seq(0,100,20), minor_breaks = seq(0,100,5)) +  
  labs(
    title=lang("Stan naładowania baterii samochodów wprowadzanych i wyłączanych z ruchu", "Battery level in cars withdrawn from and introduced to traffic"),
    subtitle=lang("obliczono na podstawie wydarzeń z co najmniej pięcioprocentowym wzrostem stanu baterii", "calculated based on events with at least a 5% increase of battery level"),
    y=lang("Liczba zdarzeń", "Number of events"),
    x=lang("Procent naładowania baterii", "Battery charge level") ) 

myggsave("34.png", width = 8.5, height=6, dpi=100)

#===========================================================================================================

#dane do Wykresu fali
fala <- streaksAll %>% filter(status=="RENTED") %>% group_by(dateStart = as.Date(dateStart, tz=""), sideNumber) %>% summarize(n=n()) %>% ungroup() %>% arrange(sideNumber, dateStart) %>% group_by(sideNumber) %>% mutate(ncum = cumsum(n)) %>% ungroup %>% dplyr::select(dateStart, sideNumber, ncum)
falaLast <- data.frame(sideNumber=vozilla %>% distinct(sideNumber) %>% arrange(sideNumber), dateStart=ymd("2018-04-30"), ncum= (fala %>% dplyr::select(sideNumber, ncum) %>% group_by(sideNumber) %>% summarize(ncum=max(ncum)) %>% dplyr::select(ncum) ))
stalled <- fala %>% group_by(sideNumber) %>% summarize(d = max(dateStart)) %>% arrange(d) %>% head(n=7) %>% pull(sideNumber)

#goły wykres fali
fala %>% rbind(falaLast) %>% mutate(isVan = sideNumber>190) %>% 
  ggplot(aes(x=dateStart, y=ncum, col=isVan, group=sideNumber))+
  geom_path(size=0.5, alpha=0.25) +
  theme_minimal() +
  scale_color_manual(values = c("darkgreen", "darkblue")) +
  guides( color=FALSE ) +
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) +  
  scale_y_continuous(breaks=seq(0,900,100)) +  
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(
    title=lang("Liczba wypożyczeń samochodów Vozilli w badanym okresie", "Number of Vozilla cars’ renting events over the studied period"),
    subtitle=lang("kolorem niebieskim oznaczono dostawcze e-NV200", "e-NV200 vans are marked in blue"),
    y=lang("Liczba wypożyczeń", "Number of renting events"), 
    x=NULL)

myggsave("35.png", width = 8.5, height=6, dpi=100)

#===========================================================================================================

#wykres fali z kolorkami
fala %>% rbind(falaLast) %>% mutate(isVan = sideNumber>190) %>% 
  ggplot(aes(x=dateStart, y=ncum, col=isVan, group=sideNumber))+
  geom_path(size=0.5, alpha=0.2) + 
  geom_line(data = fala %>% rbind(falaLast) %>% filter(sideNumber %in% stalled ), aes(x=dateStart, y=ncum, group=sideNumber), alpha=1, col="blue", size=0.5 ) +
  geom_line(data = fala %>% rbind(falaLast) %>% filter(sideNumber==132 | sideNumber==15), aes(x=dateStart, y=ncum, group=sideNumber), col="brown1", size=1.2, alpha=1 ) +
  theme_minimal() +
  scale_color_manual(values = c("darkgreen", "darkblue")) +
  guides( color=FALSE ) +  
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) +  
  scale_y_continuous(breaks=seq(0,900,100)) +  
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(
    title=lang("Liczba wypożyczeń samochodów Vozilli w badanym okresie", "Number of Vozilla cars’ renting events over the studied period"),
    subtitle=lang("na czerwono kolizje znane z mediów, na niebiesko auta które nie były wypożyczane w kwietniu", "collisions known from the mass media are marked in red; cars that were not rented in April are marked in blue"),
    y=lang("Liczba wypożyczeń", "Number of renting events"),
    x=NULL)

myggsave("36.png", width = 8.5, height=6, dpi=100)

#===========================================================================================================

streaksAll %>% 
  filter(status=="RENTED" & dateStart>=as.Date("2018-02-01", tz="")) %>% 
  mutate(isVan = sideNumber>190) %>% 
  group_by(day=as.Date(dateStart, tz=""), isVan) %>% 
  summarize(n=n() / n_distinct(sideNumber)) %>% 
  ggplot(aes(x=day, y=n, color=isVan))+
  geom_smooth(se = FALSE) + 
  geom_point() + 
  geom_line()+
  theme_minimal() +
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) +   
  scale_color_manual(
    lang("Typ samochodu", "Vehicle type"), 
    labels = c(lang("Osobowy Leaf", "Leaf passenger car") , lang("Dostawczy e-NV200", "e-NV200 van") ), values = c("red", "darkgreen")) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title=lang("Średnia liczba wypożyczeń jednego samochodu w zależności od typu", "Average number of renting events of a single vehicle vs vehicle type"), 
       subtitle=lang("w przeliczeniu na jeden samochód danego typu wypożyczany w danym dniu", "expressed as one vehicle of a given type rented on a given day"),
       x=NULL, y=lang("Dzienna liczba wypożyczeń","Daily number of renting events") )

myggsave("37.png", width = 8.5, height=4, dpi=100)


#===========================================================================================================

streaksAll %>% 
  filter(status=="RENTED" & dateStart>=as.Date("2018-02-01", tz="")) %>% 
  mutate(isVan = sideNumber>190) %>% 
  group_by(day=as.Date(dateStart, tz=""), isVan) %>% 
  summarize(n=sum(duration) / n_distinct(sideNumber)) %>% 
  ggplot(aes(x=day, y=n, color=isVan))+
  geom_smooth(se = FALSE) + 
  geom_point() + 
  geom_line()+
  theme_minimal() +
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) +   
  scale_color_manual(
    lang("Typ samochodu", "Vehicle type"), 
    labels = c(lang("Osobowy Leaf", "Leaf passenger car") , lang("Dostawczy e-NV200", "e-NV200 van") ), values = c("red", "darkgreen")) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title=lang("Średni łączny czas wypożyczeń jednego samochodu w zależności od typu", "Average total renting time of one vehicle vs vehicle type"),
       subtitle=lang("w przeliczeniu na jeden samochód danego typu wypożyczany w danym dniu", "expressed as one vehicle of a given type rented on a given day"), 
       x=NULL, 
       y=lang("Czas wypożyczeń w minutach", "Renting time in minutes") )

myggsave("38.png", width = 8.5, height=4, dpi=100)


#===========================================================================================================

ggplot(dayRents, aes(x=day, y=reservations*0.1+rents*(ifelse(day<ymd("2017-12-01"), 0.5, ifelse(day<ymd("2018-03-01"), 0.75, 0.90) ))))+
  geom_vline(col="red", xintercept=ymd("2017-12-01"), size=1) +
  geom_vline(col="red", xintercept=ymd("2018-03-01"), size=1) + 
  geom_smooth(fill="lightgray") + 
  geom_line()+
  geom_point() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(
    title=lang("Szacunkowe dzienne przychody z wypożyczeń floty Vozilla", "Estimated daily Vozilla revenues"), 
    y=NULL, 
    x=NULL) +
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) + 
  scale_y_continuous(labels = scales::unit_format(unit=lang("zł", "PLN"), sep=" ")) +
  annotate("text", x=as.Date("2017-12-01"), y=18000, label=lang("0,50 zł/min","PLN 0.50/min"), color="red", hjust=1.2, vjust=-1) +
  annotate("text", x=as.Date("2017-12-01"), y=18000, label=lang("0,75 zł/min","PLN 0.75/min"), color="red", hjust=-0.2, vjust=-1) +
  annotate("text", x=as.Date("2018-03-01"), y=18000, label=lang("0,90 zł/min","PLN 0.90/min"), color="red", hjust=-0.2, vjust=-1) +  
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

myggsave("39.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================

ggplot(dayRents, aes(x=day, y=(reservations*0.1+rents*(ifelse(day<ymd("2017-12-01"), 0.5, ifelse(day<ymd("2018-03-01"), 0.75, 0.90) )))/carsAll)) +
  geom_vline(col="red", xintercept=ymd("2017-12-01"), size=1) +
  geom_vline(col="red", xintercept=ymd("2018-03-01"), size=1) +   
  geom_smooth(fill="lightgray") + 
  geom_line()+
  geom_point() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(
    title=lang("Szacunkowe dzienne przychody z wypożyczeń w przeliczeniu na jedno auto", "Estimated daily Vozilla revenues per single car"),
    y=NULL, 
    x=NULL) +
  annotate("text", x=as.Date("2017-12-01"), y=110, label=lang("0,50 zł/min","PLN 0.50/min"), color="red", hjust=1.2, vjust=-1) +
  annotate("text", x=as.Date("2017-12-01"), y=110, label=lang("0,75 zł/min","PLN 0.75/min"), color="red", hjust=-0.2, vjust=-1) +
  annotate("text", x=as.Date("2018-03-01"), y=110, label=lang("0,90 zł/min","PLN 0.90/min"), color="red", hjust=-0.2, vjust=-1) +  
  scale_x_date(date_breaks = "week", minor_breaks =  NULL) +
  scale_y_continuous(labels = scales::unit_format(unit=lang("zł", "PLN"), sep=" ")) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

myggsave("40.png", width = 8.5, height=4, dpi=100)

#===========================================================================================================


