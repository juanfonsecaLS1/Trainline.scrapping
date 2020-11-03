# required libraries
library(rvest)
library(xml2)
library(dplyr)
library(magrittr)
library(stringr)

# vector with list of paths
path.html<-c("https://www.thetrainline.com/train-times/london-to-liverpool",
             "https://www.thetrainline.com/train-times/liverpool-to-manchester",
             "https://www.thetrainline.com/train-times/manchester-to-leeds")

# Define the function to capture train time data
get.TrainLine.Times<-function(x){
  
  # Read the web
  chart_page <- xml2::read_html(x, fill = TRUE)
  
  # Extract the data from defined nodes
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
  
  # Transform hours into minutes and calculates a total time
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
  
  # Transform hours into minutes and calculates a total time 
  slow_time<-ifelse(is.na(slow_page.h),0,slow_page.h*60)+slow_page.m
  
  # Average time
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
  
  # Direct or not
  
  direct.page<-chart_page %>% 
    rvest::html_node(".route-fact-changes")%>%
    rvest::html_text()%>%
    str_detect("Direct")
    
  # Transform hours into minutes and calculates a total time  
  avg_time<-ifelse(is.na(avg_page.h),0,avg_page.h*60)+avg_page.m
  
  # Prepare a data frame with the results
  results<-data.frame(web=x,
                      freq=freq_page,
                      avg.time=avg_time,
                      slow.time=slow_time,
                      fast.time=fast_time,
                      direct=direct.page)
  return(results)
}

# Process the list
Train.Times.df<-do.call(rbind,lapply(path.html,get.TrainLine.Times))
