## to get all list of country and their url + Continent
## twitter: @dioariadi
## with help from https://github.com/sikucing
## https://github.com/dioariadi/covid-19
## created at 3 May 2020
## v0.1
## will take around 4-8 minutes depend(?)
library(rvest)
library(tidyverse)
url_link <- "https://www.worldometers.info/coronavirus/#countries"
list_of_country <- read_html(url_link) %>% html_nodes(".mt_a") %>% html_text()
link_to_the_country <- read_html(url_link) %>% html_nodes(".mt_a") %>% html_attr("href") %>% unique()
all_html <-read_html(url_link) %>%
  html_nodes("tr") 

print("Get all nodes")
# html_nodes("#main_table_countries_today") %>% html_node(".even") 
temp <- list()
temp2 <- list()
temp3 <- list()
for (i in 1:length(all_html)) {
  temp[i] <- list(all_html[i] %>%  
                    html_nodes("td") %>% 
                    html_attr("data-continent")%>% 
                    na.omit() %>% as.character() %>% 
                    unlist())
  temp<-lapply(temp, function(x) if(identical(x, character(0))) NA_character_ else x)
  temp2[i] <- list(all_html[i] %>% 
                     html_nodes("td .mt_a") %>%
                     html_attr("href") %>%
                     unlist())
  temp2<-lapply(temp2, function(x) if(identical(x, character(0))) NA_character_ else x)
  temp3[i] <- list(all_html[i] %>% html_nodes(".mt_a") %>% html_text() %>% unlist())
  temp3<-lapply(temp3, function(x) if(identical(x, character(0))) NA_character_ else x)
}

country_dataset <- data.frame(cbind(data_continent = (temp), url = (temp2),country = (temp3)))
country_dataset <- sapply(country_dataset,unlist)
country_dataset <- unique(country_dataset)
country_dataset <- na.omit(country_dataset)
country_dataset <- as.data.frame(country_dataset)

rm(temp,temp2,temp3)
get_worldmeters <- function(country){
  temp=read_html(paste("https://www.worldometers.info/coronavirus/",country,sep = "")) %>% 
    html_text("#coronavirus-cases-log")
  temp=strsplit(temp, "Highcharts.chart")
  temp <- unlist(temp)
  temp <- temp[grepl(pattern = "data\\:",x = temp)==TRUE]
  datanya <- lapply(temp,str_extract_all,pattern = "data:.+]")
  namenya <- lapply(temp,str_extract_all,pattern = "name:.*")
  tanggal <- lapply(temp,str_extract_all,pattern = "categories:.+]")
  namenya <- namenya %>% unlist() %>% gsub('name: ','',.) %>% 
    gsub("[[:punct:]]","",.) %>% 
    str_trim()
  datanya <- datanya %>% unlist() %>% gsub(pattern = 'data:','',.) %>% 
    gsub("\\[","",.) %>% 
    gsub("\\]","",.) %>% 
    gsub('\\}','',.) %>% 
    str_trim() %>% 
    strsplit(",") 
  tanggal <- tanggal %>% unlist() %>% gsub(pattern = 'categories:','',.) %>% 
    gsub("\\[","",.) %>% 
    gsub("\\]","",.) %>% 
    gsub('\\"',"",.) %>% 
    str_trim() %>% 
    strsplit(",") %>% 
    unique()
return(list(tanggal=tanggal,datanya=datanya,namenya = namenya))
}

#china <- get_worldmeters("china")
#france <- get_worldmeters("france")

tidying_data <- function(result_from_getworldmeters){
  
  df_list <- list()
  for (i in 1:length(result_from_getworldmeters[[2]])) {
    for (j in 1:length(result_from_getworldmeters[[1]])) {
      if (length(result_from_getworldmeters[[2]][[i]])==length(result_from_getworldmeters[[1]][[j]])) {
        naming_convention <- tolower(gsub(" ","_",result_from_getworldmeters[[3]][i]))
        temp_df <- data.frame(metric = naming_convention,
                              data_date = result_from_getworldmeters[[1]][[j]],
                              value = result_from_getworldmeters[[2]][[i]],
                              stringsAsFactors = FALSE)
        df_list[i] <- list(temp_df)
      } else {
      }
    }
  }
  final_list <- df_list %>% unique()
  final_list <- bind_rows(final_list)
  return(final_list)
}

retrieve_data_worldmeter <- function(url_){
  pb$tick()$print()
  tidying_data(get_worldmeters(country = url_)) 
}

time1 <- Sys.time()

print("retrieve from website")

pb <- progress_estimated(nrow(country_dataset))
df_worldmeter <- country_dataset %>% 
  mutate(get_data = map(url, retrieve_data_worldmeter))

df_worldmeter_analysis <- df_worldmeter %>% 
  unnest(cols = get_data) %>% 
  mutate_if(is.factor,as.character) %>% 
  mutate(data_date = as.POSIXct(strptime(data_date,format = "%b %d")),
         value = as.numeric(value)) %>% 
  filter(metric %in% c("cases","daily_cases","deaths","daily_deaths","new_cases")) %>% 
  pivot_wider(names_from = metric,values_from = value) %>% 
  mutate(load_date = Sys.Date())

saveRDS(df_worldmeter_analysis,"df_worldmeter_analysis.rds")
time2 <- Sys.time()

time2 - time1
