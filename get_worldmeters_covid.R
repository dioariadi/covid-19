library(rvest)

get_worldmeters <- function(country){
  temp=read_html(paste("https://www.worldometers.info/coronavirus/country/",country,"/",sep = "")) %>% 
    html_text("#coronavirus-cases-log")
  temp=strsplit(temp, "Highcharts.chart")
  temp <- unlist(temp)
  temp <- temp[grepl(pattern = "data\\:",x = temp)==TRUE]
  datanya <- lapply(temp,str_extract_all,pattern = "data:.+]")
  namenya <- lapply(temp,str_extract_all,pattern = "name:.*")
  tanggal <- lapply(temp,str_extract_all,pattern = "categories:.+]")
  namenya <- namenya %>% unlist() 
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

#testing <- get_worldmeters("indonesia")
#china <- get_worldmeters("china")
#france <- get_worldmeters("france")

tidying_data <- function(result_from_getworldmeters){
  df_list <- list()
  for (i in 1:length(result_from_getworldmeters[[2]])) {
    for (j in 1:length(result_from_getworldmeters[[1]])) {
      if (length(result_from_getworldmeters[[2]][[i]])==length(result_from_getworldmeters[[1]][[j]])) {
        temp_df <- data.frame(x=result_from_getworldmeters[[2]][[i]],y=result_from_getworldmeters[[1]][[j]])
        colnames(temp_df)[1] <- result_from_getworldmeters[[3]][i]
        df_list[i] <- list(temp_df)
      } else {
      }
    }
  }
  return(df_list %>% unique())
}

tidying_data(get_worldmeters("china")) 
