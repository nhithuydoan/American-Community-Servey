library(tidyverse)
library(readxl)
library(rvest)
#if(!require('unpivotr')) {
  install.packages('unpivotr')
  library('unpivotr')
}
#if(!require('tidyxl')) {
  install.packages('tidyxl')
  library('tidyxl')
}

col_nm <- c(
  "category", "total_estimate", "total_moe3", "men_estimate", "men_moe3",
  "women_estimate", "women_moe3", "percent_women", "percent_women_moe3",
  "total_earnings_estimate", "total_earnings_moe3", "total_earnings_men_estimate", 
  "total_earnings_men_moe3", "total_earnings_women_estimate", "total_earnings_women_moe3", 
  "wage_percent_of_mens_estimate", "wage_percent_of_mens_moe3"
)


category_names <- readr::read_rds("category_names.rds") %>% str_remove_all(., ":")
#View(category_names)
earnings_2013 <- readxl::read_excel("median-earnings-2013-final.xlsx", skip = 6) %>%
  janitor::clean_names() %>%
  set_names(nm = col_nm) %>%
  filter(!is.na(total_estimate)) %>%
  mutate(year = 2013L) %>%
  mutate(category = category_names)

earnings_2014 <- readxl::read_excel("median-earnings-2014-final.xlsx", skip = 6) %>%
  janitor::clean_names() %>%
  set_names(nm = col_nm) %>%
  filter(!is.na(total_estimate)) %>%
  mutate(year = 2014L) %>%
  mutate(category = category_names)

earnings_2015 <- readxl::read_excel("median-earnings-2015-final.xlsx", skip = 6) %>%
  janitor::clean_names() %>%
  set_names(nm = col_nm) %>%
  filter(!is.na(total_estimate)) %>%
  filter(!str_detect(category, "Transportation and Material Moving Occupations")) %>%
  mutate(year = 2015L) #%>%
  mutate(category = category_names)

earnings_2016 <- readxl::read_excel("median-earnings-2016-final.xlsx", skip = 6) %>%
  janitor::clean_names() %>%
  set_names(nm = col_nm) %>%
  filter(!is.na(total_estimate)) %>%
  filter(!str_detect(category, "Transportation and Material Moving Occupations")) %>%
  mutate(year = 2016L) %>%
  mutate(category = category_names)

all_years <- bind_rows(earnings_2013, earnings_2014, earnings_2015, earnings_2016) %>%
  filter(!str_detect(category, "Total"))
  
Create the major category
Grabbed the major categories from the table by hand and did some basic checks to make sure everything came out ok.


cat1 <- c(
  "Management, Business, and Financial Occupations",
  "Computer, Engineering, and Science Occupations",
  "Education, Legal, Community Service, Arts, and Media Occupations",
  "Healthcare Practitioners and Technical Occupations",
  "Service Occupations",
  "Sales and Office Occupations",
  "Natural Resources, Construction, and Maintenance Occupations",
  "Production, Transportation, and Material Moving Occupations"
)

length(cat1)

#category_names <- data.frame(category_names)
tibble(category_names) %>%
  filter(category_names %in% cat1) %>%
  nrow()

all.equal(cat1, tibble(category_names) %>%
            filter(category_names %in% cat1) %>%
            pull())

cat2 <- c(
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
  "Material Moving Occupations"
)

length(cat2)

tibble(category_names) %>%
  filter(category_names %in% cat2) %>%
  nrow()

all.equal(cat2, tibble(category_names) %>%
            filter(category_names %in% cat2) %>%
            pull())

###############################
category_added <- all_years %>%
  mutate(
    cat1 = case_when(
      category %in% cat1 ~ category,
      TRUE ~ NA_character_
    ),
    cat2 = case_when(
      category %in% cat2 ~ category,
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    cat1 = str_remove(cat1, " Occupations"),
    cat1 = str_remove(cat1, " occupations"),
    cat2 = str_remove(cat2, " Occupations"),
    cat2 = str_remove(cat2, " occupations"),
    category = str_remove(category, " Occupations"),
    category = str_remove(category, "occupations")
  )

clean_all <- category_added %>%
  fill(cat1) %>%
  fill(cat2)

# create an entire new dataset
nm_final <- c("year", "occupation", "major_category", "minor_category", "total_workers", "workers_male", "workers_female", "percent_female", "total_earnings", "total_earnings_male", "total_earnings_female", "wage_percent_of_male")

final_all <- clean_all %>%
  filter(!str_detect(category, cat1))  %>%
  filter(!str_detect(category, cat2)) %>%
  filter(category != "Management,  Business, Science, and Arts Occupations") %>%
  filter(category != "Management, Business, and Financial Occupations") %>%
  filter(category != "Management,  Business, Science, and Arts") %>%
  filter(!is.na(cat1)) %>%
  filter(!is.na(cat2)) %>%
  select(year, occupation = category, major_category = cat1, minor_category = cat2, everything()) %>%
  select(-contains("moe")) %>%
  mutate_at(c("total_earnings_estimate", "total_earnings_men_estimate", "total_earnings_women_estimate", "wage_percent_of_mens_estimate"), as.numeric) %>%
  set_names(nm = nm_final)

x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" #or whatever
#str_replace_all(x, "[[:punct:]]", " ")
gsub("[[:punct:]]", " ", x)

x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" 
