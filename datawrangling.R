library(readxl)
library(tidyverse)
library(openxlsx)


compile <- function(filename,input_year){
  
  category_names <- readr::read_rds("category_names.rds") %>% str_remove_all(., ":")
  require(readxl)
  data <- read_excel(filename, 
                     col_names = FALSE, skip = 7)
  data <- data[,-c(3,5,7,9,11,13,15,17)]
  label <- c("occupation", "TotalPopulation","Number_of_Men","Number_of_Women",
                      "Women_Percentage","Earning","Men_earning","Women_earning",
                      "Compare_to_men")
  colnames(data) = make.names(label, unique=TRUE)
  data <- data[-c(558:568),]
  major_names <- c(
    "Management, Business, and Financial Occupations",
    "Computer, Engineering, and Science Occupations",
    "Education, Legal, Community Service, Arts, and Media Occupations",
    "Healthcare Practitioners and Technical Occupations",
    "Service Occupations",
    "Sales and Office Occupations",
    "Natural Resources, Construction, and Maintenance Occupations",
    "Production, Transportation, and Material Moving Occupations")
  career <- c(
    "Management Occupations",
    "Business and Financial Operations Occupations",
    "Computer and mathematical occupations",
    "Architecture and Engineering Occupations",
    "Life, Physical, and Social Science Occupations",
    "Community and Social Service Occupations",
    "Legal Occupations",
    "Education, Training, and Library Occupations",
    "Arts, Design, Entertainment, Sports, and Media Occupations",
    "Healthcare Practitioners and Technical Occupations",
    "Healthcare Support Occupations",
    "Protective Service Occupations",
    "Food Preparation and Serving Related Occupations",
    "Building and Grounds Cleaning and Maintenance Occupations",
    "Personal Care and Service Occupations",
    "Sales and Related Occupations",
    "Office and Administrative Support Occupations",
    "Farming, Fishing, and Forestry Occupations",
    "Construction and Extraction Occupations",
    "Installation, Maintenance, and Repair Occupations",
    "Production Occupations",
    "Transportation Occupations",
    "Material Moving Occupations")
  data <- data %>%
    filter(!str_detect(occupation, "Transportation and Material Moving Occupations"))%>% 
    mutate (occupation = category_names, year = input_year)  %>%
    mutate(major = case_when(occupation %in% major_names ~ occupation,
                             TRUE ~ NA_character_))%>% fill(major) %>% 
    mutate(major = str_remove(major, " Occupations"),
           major = str_remove(major, "occupations"))%>% 
    filter(!(occupation %in% career)) %>%
    filter(!str_detect(occupation, major)) %>%
    select(year,occupation, major, everything())
  
    return(data)
}


earning_2013 <- compile("median-earnings-2013-final.xlsx",2013)
earning_2014 <- compile("median-earnings-2014-final.xlsx",2014)
#write.xlsx(earning_2014, "Add_median-earnings-2014-final.xlsx")
earning_2015 <- compile("median-earnings-2015-final.xlsx",2015)
earning_2016 <- compile("median-earnings-2016-final.xlsx",2016)
earning_2017 <- compile("median-earnings-2017-final.xlsx",2017)

compile2 <- function(filename,input_year){
  #category_names <- readr::read_rds("category_names.rds") %>% str_remove_all(., ":")
  require(readxl)
  data <- read_excel(filename, 
                     col_names = FALSE, skip = 7)
  data <- data[,-c(3,5,7,9,11,13,15,17)]
  label <- c("occupation", "TotalPopulation","Number_of_Men","Number_of_Women",
             "Women_Percentage","Earning","Men_earning","Women_earning",
             "Compare_to_men")
  colnames(data) = make.names(label, unique=TRUE)
  data <- data[-c(599:609),]
  
  major_names <- c(
    "Management, Business, and Financial Occupations:",
    "Computer, Engineering, and Science Occupations:",
    "Education, Legal, Community Service, Arts, and Media Occupations:",
    "Healthcare Practitioners and Technical Occupations:",
    "Service Occupations:",
    "Sales and Office Occupations:",
    "Natural Resources, Construction, and Maintenance Occupations:",
    "Production, Transportation, and Material Moving Occupations:")
  career <- c(
    "Management Occupations:",
    "Business and Financial Operations Occupations:",
    "Computer and mathematical occupations:",
    "Architecture and Engineering Occupations:",
    "Life, Physical, and Social Science Occupations:",
    "Community and Social Service Occupations:",
    "Legal Occupations:",
    "Education, Training, and Library Occupations:",
    "Arts, Design, Entertainment, Sports, and Media Occupations:",
    "Healthcare Practitioners and Technical Occupations:",
    "Healthcare Support Occupations:",
    "Protective Service Occupations:",
    "Food Preparation and Serving Related Occupations:",
    "Building and Grounds Cleaning and Maintenance Occupations:",
    "Personal Care and Service Occupations:",
    "Sales and Related Occupations:",
    "Office and Administrative Support Occupations:",
    "Farming, Fishing, and Forestry Occupations:",
    "Construction and Extraction Occupations:",
    "Installation, Maintenance, and Repair Occupations:",
    "Production Occupations:",
    "Transportation Occupations:",
    "Material Moving Occupations:")
  data <- data %>%
    filter(!str_detect(occupation, "Transportation and Material Moving Occupations"))%>%
    mutate (year = input_year)%>% mutate(major = case_when(occupation %in% major_names ~ occupation,
                             TRUE ~ NA_character_)) %>% fill(major) %>%
    mutate(major = str_remove(major, " Occupations:"),
           major = str_remove(major, "occupations:"))%>%
    filter(!(occupation %in% career)) %>%
    filter(!str_detect(occupation, major))%>%
    select(year,occupation, major, everything()) 

  return(data)
}

earning_2018 <- compile2("median-earnings-2018-final.xlsx",2018)

earning_2019 <- compile2("median-earnings-2019-final.xlsx",2019)

data <- rbind(earning_2013,earning_2014,earning_2015,earning_2016,earning_2017,
                  earning_2018,earning_2019)


## Write into file, input for the final-code
write.csv(data,"earnings-2013-2019.csv")









