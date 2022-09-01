library(dplyr)
library(lubridate)
library(here)
library(tidyverse)
library(rio)
library(farver)
library(ggplot2)

library(plotly)

setwd("C:/Users/masvikenib/Documents/R Training/data")

cholera <- here ("data","raw", "cholera_gondowana_2019_10_21.xlsx") %>%
import()

plot_data <- cholera %>%
  select(region_res, Sexe, Diarrhee:nausees) %>% 
  pivot_longer(cols = Diarrhee:nausees, names_to = "Symptoms",
               values_to = "Response") %>% 
  group_by(region_res,Sexe,Symptoms) %>% 
  summarise(count = sum(Response=="Oui")) %>% 
  arrange(count)
          
plot_data

#initialising the plot
ggplot(data = plot_data) +
  #generate a point plot
  #to have different colors add color in aes 
  geom_point(mapping = aes(x= Symptoms, y = count, color=Sexe ))+
  scale_color_manual(values = c("F"="blue", "M"="red"))+
  #scale_shape_manual(values = c("Boutou"=7, "Polalo"=8))
labs (x="Types of Symptoms",
     y="Number of YES values",
     title = "This is my First plot",
     subtitle = "for the cholera dataset",
     caption = paste("Dated: ",  Sys.Date()))

#axis title  did not change to red
theme (axis.title = element_text(color ="red", size= 15),
      plot.title =  element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5))
      

 ggplot(data = plot_data, aes(x=Symptoms, y=count))+
    geom_point(mapping = aes(color=Sexe ))+
    geom_line()

 #line and point plot for one region and sex
 plot_data %>% 
    dplyr::filter(Sexe=="F", region_res=="Boutou") %>% 
    ggplot(aes(x=Symptoms, y=count, group=1))+
    geom_line()+
    geom_point()
 
 #BAR
 plot_data %>% 
   dplyr::filter(Sexe=="F", region_res=="Boutou") %>% 
  # ggplot(aes(x=Symptoms,count))+
   ggplot(aes(reorder(Symptoms, count), count))+
   geom_col(fill="#007FFF")+
   geom_text(aes(label= count),vjust = -0.5)+
   labs(x="Symptoms")+
   coord_flip()

 
 ## Create a point and line plot for Sexe = Female and Boutou region
 #Add a label on top of points to display the count
 #Make the lines of blue color and points as red
 plot_data %>% 
   dplyr::filter(Sexe=="F", region_res=="Boutou") %>%  
   ggplot(aes(x=Symptoms, y=count, group=1))+
   geom_text(aes(label= count),vjust = 2)+
   geom_line(color="blue", size=2)+
   geom_point(color="red", size=3)+
 labs (x="Symptoms Types",
       y="Count of Symptoms",
       title = "Cholera Symptoms:Boutou ",
       #subtitle = "for the cholera dataset",
       caption = paste("Dated: ",  Sys.Date()))+
 theme(plot.title = element_text(hjust = 0.5, size=16, colour ="red" ))
 
 
 #stacked bar plot
 plot_data %>% 
   dplyr::filter(Sexe=="F", region_res=="Boutou") %>% 
   ggplot(aes(Symptoms,count, fill=Sexe))+
   geom_col()
 
 #stacked bar plot
 plot_data %>% 
   dplyr::filter(region_res=="Boutou") %>% 
   ggplot(aes(Symptoms,count, fill=Sexe))+
   geom_col()
  (position = "dodge")
 
 #FACETS
 plot_data %>% 
   ggplot(aes(Symptoms,count, fill=Sexe))+
  geom_col (position = "dodge")
   facet_wrap(~region_res)
   
## 
   plot_data %>% 
     ggplot(aes(Symptoms,count, fill=region_res))+
     geom_col (position = "dodge")+
     coord_flip()+
     facet_wrap(~Sexe)+
     theme(axis.text = element_text(angle = 45, vjust = 0.5, hjust = 1)) 
 
 ##
  plot_data %>% 
       ggplot(aes(Symptoms,count, fill=region_res))+
       geom_col (position = "dodge")+ 
       facet_wrap(~Sexe, scales = "free_y") 
  
#Facet grid
  plot_data %>% 
  ggplot(aes(Sexe,count))+
  geom_col (position = "dodge")+
  facet_grid(region_res~Symptoms)
  
  
#generate fake dataset
data <- data.frame (id= 1:1000,
   date= Sys.Date()+ sample(1000),
   value = runif(1000,10000, 100000))

data
head(data)
tail(data)

summary(data)

ggplot(data, aes(date, value)) + geom_line()


#Basic plot

ggplot(data, aes(date, value))+ geom_line()


monthly_plot <-data %>% 
  arrange(date) %>% 
    mutate(month=floor_date(date, "month")) %>% 
  group_by(month) %>% 
  summarise(avg_value=mean(value)) %>% 
  ggplot(aes(month, avg_value))+ 
  geom_line()+ 
  scale_x_date(date_labels ="%B-%y",
     date_breaks ="3 months")+
  labs(x="Month-Year",
       y="Average value")+
  scale_y_continuous(labels = scales::dollar)

#big.mark - TO ADD NUMBER SEPERATOR
#decimal.mark - TO ADD DECIMAL SEPERATOR
##scales::comma(x, big.mark = " ")


ggplotly(monthly_plot)

ggplotly(monthly_plot, tooltip = c("avg_value", "month"))


#Exercise

mtcars %>%
  group_by(cyl, gear) %>% 
  summarise(avg=mean(mpg)) %>% 
  ggplot(aes(cyl, avg, color=factor(gear)))+
  geom_line(size=3)+
  labs(x="Cyl")
       y="Average mpg"
       color="Gear"
       title="Cyl vs Mpg values for each gear"


skim(mtcars1)


install.packages("psych")
 library(psych)             
cholera %>% 
describe.by(cholera$region_res)
