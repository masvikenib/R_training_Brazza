library(readxl)
library(dplyr)
library(here)
library(flextable)
library(gtsummary)
library(officer)
library(scales)
library(tidyr)
library(rio)
library(tidyverse)
library(janitor)

## load your data

cholera <- here("data", "raw", "cholera_gondowana_2019_10_21.xlsx") %>% 
  import()

## select your data (calculate any variables you like inside summary)

table2<-cholera %>% 
  group_by(evolution) %>% 
  summarise(
    cases = n(),
    average_age = round(mean(AGE_cas),2),
    max_age = max(AGE_cas), 
    female= sum(Sexe=="F", na.rm=T),
    male = sum(Sexe=="M", na.rm=T),
    pct_females = scales::percent(female / cases),
    cfr         = scales::percent(sum(evolution == "Decede",na.rm =T)/cases)
  ) %>% 
  adorn_totals() 

## add flextable to your table2

table2%>% 
  flextable() %>%   
  autofit()  %>% 
  bold(~ pct_females  == max(pct_females),7)%>% 
  bg(i = ~ average_age  == max(average_age), 
     j = 3,
     bg="pink")

## write to docx

setwd(here::here("output"))

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape"),
  type = "continuous",
  page_margins = page_mar()
)

save_as_docx(`new table` = table2, path = "new table.docx",
             pr_section =sect_properties)

