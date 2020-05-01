### get data source and combine into single one -----
library(tidyverse)
library(lubridate)

death_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
confirmed_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
recovered_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

tidy_covid <- function(url,naming_variable){
  df_temp <- readr::read_csv(url)
  colnames(df_temp)[1:2] <- c("province_state","country_region")
  df_temp <- df_temp %>% pivot_longer(cols = contains("/"),
               names_to = "data_date",
               values_to = naming_variable) %>% 
    mutate(data_date = mdy(data_date))
  return(df_temp)
}

confirmed_cek <- read_csv(confirmed_url)

death_data <- tidy_covid(death_url,"cnt_cumulative_death")
confirmed_data <- tidy_covid(confirmed_url,"cnt_cumulative_confirmed")
recovered_data <- tidy_covid(recovered_url,"cnt_cumulative_recovered")

# merging all variable ----
df_covid <- confirmed_data %>% 
  left_join(death_data %>% select(-c(Lat,Long)), by = c("province_state","country_region","data_date")) %>% 
  left_join(recovered_data %>% select(-c(Lat,Long)), by = c("province_state","country_region","data_date"))

df_covid_country <- df_covid %>% 
  mutate( country_region = case_when(province_state=="Hong Kong"~"Hong Kong",
                                     TRUE~country_region)) %>%
  group_by(country_region,data_date) %>% 
  summarise_if(is.numeric, sum) %>% 
  ungroup()

df_covid_country <- df_covid_country %>% 
  arrange(data_date) %>% 
  group_by(country_region) %>% 
  mutate(cnt_confirmed = cnt_cumulative_confirmed - lag(cnt_cumulative_confirmed,1),
         cnt_death = cnt_cumulative_death - lag(cnt_cumulative_death,1),
         cnt_recovered = cnt_cumulative_recovered - lag(cnt_cumulative_recovered,1)) %>% 
  ungroup()


write_csv(df_covid_country,"df_covid_country.csv")




### second try -----

home_page <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
ts_date <- seq.Date(from = as.Date("2020-01-22"),to = Sys.Date()-1,by = 1)
ts_date <- strftime(ts_date,format = "%m-%d-%Y")
ts_date <- paste(ts_date,".csv",sep = "")


df_list <- list()
for (i in 1:length(ts_date)) {
  df_list[i] <- list(read_csv(paste(home_page,ts_date[i],sep = ""))  )
}

#saveRDS(df_list,"df_list_covid.rds")


gsub(x = names(df_list[[i]]), pattern = "\\/", replacement = "_")  
df_list_copy <- df_list

df_list <- df_list_copy
ts_date <- seq.Date(from = as.Date("2020-01-22"),to = Sys.Date()-1,by = 1)
for (i in 1:length(df_list)) {
  df_list[[i]]['data_date'] <- ts_date[i]
  names(df_list[[i]]) <- gsub(x = names(df_list[[i]]), pattern = "\\/", replacement = "_")  
  df_list[[i]] <- df_list[[i]][c('Province_State','Country_Region','Confirmed','Deaths','Recovered','data_date')]
}


df_list_result <- bind_rows(df_list)


df_list_result_2 <-   df_list_result %>% 
  mutate( Country_Region = case_when(Province_State=="Hong Kong"~"Hong Kong",
                                     Country_Region=="Mainland China"~ "China",
                                     TRUE~Country_Region)) %>%
  group_by(Country_Region,data_date) %>% 
  summarise_if(is.numeric, sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  arrange(data_date) %>% 
  group_by(Country_Region) %>% 
  mutate(cnt_confirmed = Confirmed - lag(Confirmed,1),
         cnt_death = Deaths - lag(Deaths,1),
         cnt_recovered = Recovered - lag(Recovered,1)) %>% 
  ungroup()





## test 3

df_ourworld <- read.csv("Downloads/owid-covid-data.csv")


#3test 4

data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
data<- data %>% mutate(dateRep = dmy(dateRep))

View(data %>% filter(countriesAndTerritories=="France") %>% arrange(dateRep) %>% 
  mutate(cumsum(cases)))


