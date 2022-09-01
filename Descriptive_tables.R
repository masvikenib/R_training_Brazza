
## Installing
install.packages("flextable")
library(flextable)
install.packages("gtsummary")
library(gtsummary)
install.packages("officer")
library(officer)
install.packages("scales")
library(scales)
install.packages("tidyr")
library(tidyr)

library(lubridate)
library(here)
library(rio)
library(tidyverse)
library(linelist)
library(janitor)
library(knitr)
library(kableExtra)
library(skimr)
library(DescTools)
library(remotes)
library(readxl)
library(dplyr)
library(here)
library(flextable)

cholera <- here("data","raw",
                "cholera_gondowana_2019_10_21.xlsx") %>% 
  import(sheet=1, skip=0) %>% 
  as_tibble() %>% 
  clean_variable_names()


library(gtsummary) 

cholera %>%
  select(age_cas, sexe, evolution, aire_sante_res) %>% 
 tbl_summary(by = evolution) %>% 
add_stat_label()


#### Use dplyr for a tailored descriptive table

library(janitor)

table2 <-cholera %>% 
  group_by(evolution) %>% 
 ### group_by(aire_sante_res) %>% 
    summarise(
    cases = n(), 
    average_age=mean(age_cas,2),
    average_age=round (mean(age_cas),2),
    minimum_age= min(age_cas),
    maximum_age= max(age_cas),
    female=sum(sexe=="F", na.rm=TRUE),
    male=sum(sexe=="M", na.rm=TRUE),
    pct_females = scales:: percent(female/cases),
    cfr=scales::percent(sum(evolution=="Decede", na.rm=T)/cases)
  ) %>% 
 
  adorn_totals() %>% 
  flextable() %>% 
  autofit() %>% 
  bold(~maximum_age==max(maximum_age),3) %>% 
  bg(i= ~ average_age == max(average_age),
     j=3,
     bg="RED")
  

max(cholera$age_cas)

## creating age group cataegory
table(cholera$age_cas)
  tab_age <- cholera %>% 
    select(aire_sante_res, age_cas) %>%
    mutate(age_cas = replace(age_cas, age_cas==-8,8)) %>% 
    mutate(age_group=cut(age_cas, breaks=c(0,9,19,29,39,49,59,79,89))) %>% 
    count(age_group, aire_sante_res ) %>% 
    group_by(age_group) %>% 
    spread(aire_sante_res,n,fill = 0) %>% 
    adorn_totals() %>% 
    flextable() %>% 
    ##autofit() 
    
    tab_age
  

   ### Exporting Cases by Age Group Table to WORDWORD.DOCX     
setwd(here::here("Outputs"))

library(officer)
#format
sect_properties <- prop_section(
  page_size= page_size(orient= "landscape",
                        width =8.3, height=11.7),
  type= "continuous",
  page_margins=page_mar()
)

save_as_docx(cases_health_facility_table =  tab_age, path="cases_health_facility_table.docx")
    
    
### transposing data
library(tidyr) 
    

table1 <- cholera %>% 
  pivot_longer(cols = diarrhee:nausees, names_to = "Symptoms",
               values_to = "Response") %>% 
      group_by (Symptoms) %>% 
      
summarise(
         Coumt = sum(Response=="Oui"),
         pct_cases = scales:: percent(sum(Response== "Oui")/167),
         female = sum(Response=="Oui" & sexe=="F", na.rm=T)) %>% 
         
         flextable() %>% 
         autofit()               
                         
         table1             
  
### Exporting Cholera Symptoms  Table to WORD.DOCX                    
                    
 setwd(here::here("Outputs"))
         
     library(officer)
         #format
       sect_properties <- prop_section(
       page_size= page_size(orient= "landscape",
                                width =10, height=11.3),
           type= "continuous",
           page_margins=page_mar()
         )
         
 ##save_as_docx(Cholera_Symptoms_table =  table1, path="Cholera_Symptoms_table.docx")
 save_as_docx(New_Cholera_Symptoms_table =  table2, path="New_Cholera_Symptoms_table@.docx")
         









