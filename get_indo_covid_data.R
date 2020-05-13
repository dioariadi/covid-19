## get data from google spreadsheet
## source @kawalcovid-19
## copy to my own google spreadsheet
## twitter: @dioariadi
## www.datawizart.com

## kawalcovid----
library(googlesheets4)
library(tidyverse)
rename_data <- c("Aceh","Bali","Banten","Bangka Belitung","Bengkulu","DI Yogyakarta",
                 "DKI Jakarta","Jambi","Jawa Barat","Jawa Tengah","Jawa Timur","Kalimantan Barat",
                 "Kalimantan Timur","Kalimantan Tengah","Kalimantan Selatan","Kalimantan Utara","Kepulauan Riau",
                 "Nusa Tenggara Barat","Sumatera Selatan","Sumatera Barat","Sulawesi Utara",
                 "Sumatera Utara","Sulawesi Tenggara","Sulawesi Selatan","Sulawesi Tengah",
                 "Lampung","Riau","Maluku Utara","Maluku","Papua Barat","Papua",
                 "Sulawesi Barat","Nusa Tenggara Timur","Gorontalo","no_define")
from_google <- function(sheet,variable_name){
  temp <- read_sheet("url_sheet_pribadi",sheet = sheet)
  colnames(temp)[2:36] <- rename_data
  temp <- temp %>% 
    pivot_longer(cols = 2:36,names_to = "province",values_to = variable_name)
  colnames(temp)[1] <- "data_date"
  return(temp)
}

df_indo_case <- from_google("Cumulative Case","total_case")
df_indo_daily_case <- from_google("Case Harian","daily_case")
df_indo_death <- from_google("meninggal","total_death")
df_indo_daily_death <- from_google("meninggal harian","daily_death")
missing_jkt <-read_sheet("https://docs.google.com/spreadsheets/d/1zObBKbGguENPhU3qKxU92oBxvvxFESgfq5i8sJ9f_QE/edit#gid=1283977822",sheet = "missing_jkt")

df_indo <- df_indo_case %>% 
  left_join(df_indo_death,by = c("province","data_date")) %>% 
  left_join(df_indo_daily_case,by = c("province","data_date")) %>% 
  left_join(df_indo_daily_death,by = c("province","data_date")) %>% 
  mutate_if(is.numeric,replace_na,replace=0)

df_indo <- df_indo %>% bind_rows(missing_jkt) 
indo_grid <- read.csv("~/indo_grid.csv",sep =  "\t",stringsAsFactors = FALSE)

df_indo <- df_indo %>%  
  complete( province,nesting(data_date),
            fill = list(total_case = 0,
                        total_death = 0,
                        daily_case = 0,
                        daily_death = 0))
df_indo <- df_indo %>% 
  left_join(indo_grid[,c(3,5)],by = c("province"="name_indo"))

write_rds(df_indo,"df_indo.rds")
write.csv(df_indo,"df_indo.csv",row.names = FALSE)

## get data from data.covid19.go.id----

list_of_province <- c('ACEH','BALI','BANTEN','KEPULAUAN BANGKA BELITUNG','BENGKULU','DAERAH ISTIMEWA YOGYAKARTA','DKI JAKARTA','JAMBI','JAWA BARAT','JAWA TENGAH','JAWA TIMUR','KALIMANTAN BARAT','KALIMANTAN TIMUR','KALIMANTAN TENGAH','KALIMANTAN SELATAN','KALIMANTAN UTARA','KEPULAUAN RIAU','NUSA TENGGARA BARAT','SUMATERA SELATAN','SUMATERA BARAT','SULAWESI UTARA','SUMATERA UTARA','SULAWESI TENGGARA','SULAWESI SELATAN','SULAWESI TENGAH','LAMPUNG','RIAU','MALUKU UTARA','MALUKU','PAPUA BARAT','PAPUA','SULAWESI BARAT','NUSA TENGGARA TIMUR','GORONTALO')
name_of_province <- c('Aceh','Bali','Banten','Bangka Belitung','Bengkulu','DI Yogyakarta','DKI Jakarta','Jambi','Jawa Barat','Jawa Tengah','Jawa Timur','Kalimantan Barat','Kalimantan Timur','Kalimantan Tengah','Kalimantan Selatan','Kalimantan Utara','Kepulauan Riau','Nusa Tenggara Barat','Sumatera Selatan','Sumatera Barat','Sulawesi Utara','Sumatera Utara','Sulawesi Tenggara','Sulawesi Selatan','Sulawesi Tengah','Lampung','Riau','Maluku Utara','Maluku','Papua Barat','Papua','Sulawesi Barat','Nusa Tenggara Timur','Gorontalo')
list_of_province <- gsub(" ","_",list_of_province)


df_covid_go_id <- list()
for (i in 1:length(list_of_province)) {
  print(i)
  df_covid_go_id[i] <- list(jsonlite::fromJSON(paste("https://data.covid19.go.id/public/api/prov_detail_",list_of_province[i],".json",sep = ""))$list_perkembangan)
}
jsonlite::fromJSON("https://data.covid19.go.id/public/api/prov_detail_DKI_JAKARTA.json")$list_perkembangan

for (i in 1:length(df_covid_go_id)) {
  df_covid_go_id[[i]][['province']] <- name_of_province[i]
}
df_covid_indo <-bind_rows(df_covid_go_id)



df_covid_indo$tanggal_bener <- lubridate::as_datetime(df_covid_indo$tanggal/1000)




