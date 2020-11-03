# required libraries
library(rvest)
library(xml2)
library(dplyr)
library(magrittr)
library(stringr)

path.html<-c("https://www.thetrainline.com/train-times/london-to-liverpool",
             "https://www.thetrainline.com/train-times/liverpool-to-manchester",
             "https://www.thetrainline.com/train-times/manchester-to-leeds")

do.call(rbind,lapply(path.html,get.TrainLine.Times))

get.TrainLine.Times<-function(x){

  chart_page <- xml2::read_html(x, fill = TRUE)
  
  # Freq per day
  freq_page <- chart_page %>% 
    rvest::html_node(".route-fact-per-day")%>%
    rvest::html_text()%>%
    str_extract(.,"\\d+")%>%
    as.integer()
    
  
  # Fastest time
  fast_page.h <- chart_page %>% 
    rvest::html_node(".route-fact-fastest")%>%
    rvest::html_text()%>%
    str_remove("Fastest route: ")%>%
      str_extract(.,"\\d+(?=h)")%>%
    as.integer()

  fast_page.m <- chart_page %>% 
    rvest::html_node(".route-fact-fastest")%>%
    rvest::html_text()%>%
    str_remove("Fastest route: ")%>%
    str_extract(.,"\\d+(?=m)")%>%
      as.integer()
  
  fast_time<-ifelse(is.na(fast_page.h),0,fast_page.h*60)+fast_page.m
  
  # Slowest time
  slow_page.h <- chart_page %>% 
    rvest::html_node(".route-fact-slowest")%>%
    rvest::html_text()%>%
    str_extract(.,"\\d+(?=h)")%>%
    as.integer()
  
  slow_page.m <- chart_page %>% 
    rvest::html_node(".route-fact-slowest")%>%
    rvest::html_text()%>%
    str_extract(.,"\\d+(?=m)")%>%
    as.integer()
  
  slow_time<-ifelse(is.na(slow_page.h),0,slow_page.h*60)+slow_page.m
  
  # Aveg time
  avg_page.h <- chart_page %>% 
    rvest::html_node(".route-fact-average")%>%
    rvest::html_text()%>%
    str_extract(.,"\\d+(?=h)")%>%
    as.integer()
    
  avg_page.m <- chart_page %>% 
    rvest::html_node(".route-fact-average")%>%
    rvest::html_text()%>%
    str_extract(.,"\\d+(?=m)")%>%
    as.integer()
    
  avg_time<-ifelse(is.na(avg_page.h),0,avg_page.h*60)+avg_page.m
  
  results<-data.frame(web=x,
                      freq=freq_page,
                      avg.time=avg_time,
                      slow.time=slow_time,
                      fast.time=fast_time)
  return(results)
}
