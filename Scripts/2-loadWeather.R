library(readr)
library(dplyr)
library(lubridate)

# download and unpack weather records from
# https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/terminowe/synop/
#
# file list:
# s_t_11_2017.csv
# s_t_12_2017.csv
# s_t_01_2018.csv
# s_t_02_2018.csv
# s_t_03_2018.csv
# s_t_04_2018.csv

temp201711raw <- read_csv("s_t_11_2017.csv", col_names = FALSE)
temp201712raw <- read_csv("s_t_12_2017.csv", col_names = FALSE)
temp201801raw <- read_csv("s_t_01_2018.csv", col_names = FALSE)
temp201802raw <- read_csv("s_t_02_2018.csv", col_names = FALSE)
temp201803raw <- read_csv("s_t_03_2018.csv", col_names = FALSE)
temp201804raw <- read_csv("s_t_04_2018.csv", col_names = FALSE)
temperature <- temp201711raw[temp201711raw$X1=="351160424",c(1,2,3,4,5,6,30)] %>% 
         rbind(temp201712raw[temp201712raw$X1=="351160424",c(1,2,3,4,5,6,30)]) %>%
         rbind(temp201801raw[temp201801raw$X1=="351160424",c(1,2,3,4,5,6,30)]) %>%
         rbind(temp201802raw[temp201802raw$X1=="351160424",c(1,2,3,4,5,6,30)]) %>%
         rbind(temp201803raw[temp201803raw$X1=="351160424",c(1,2,3,4,5,6,30)]) %>%
         rbind(temp201804raw[temp201804raw$X1=="351160424",c(1,2,3,4,5,6,30)])  
rm(temp201711raw, temp201712raw, temp201801raw, temp201802raw, temp201803raw, temp201804raw)
temperature$yyyymmddhh <- ymd_h(paste(temperature$X3, temperature$X4, temperature$X5, temperature$X6, sep = ""))
temperature$temperature <- temperature$X30
temperature <- temperature %>% select(yyyymmddhh, temperature)

