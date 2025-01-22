library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(magrittr)
library(data.table)
library(readxl)
library(lubridate)

data <- read_csv("daily_SPEC_2014.csv")

# Question 1
Wisconsin <- data %>% filter(`State Name` == "Wisconsin", `Parameter Name` == "Bromine PM2.5 LC") %>%
  select(`State Name`, `Parameter Name`, `Arithmetic Mean`)

Wisconsin_Cleaned <- Wisconsin %>% subset(!is.na(`Arithmetic Mean`))

Wisconsin_Cleaned %>% pull(`Arithmetic Mean`) %>% mean()

#Question 2
Constituent_Highest_Average <- data %>% group_by(`Parameter Name`)%>%
  summarize(Average_Arithmetic_Mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(Average_Arithmetic_Mean))

#Question 3
Highets_Sulfate <- data %>% filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  group_by(`State Code`, `County Code`, `Site Num`) %>%
  select(`Parameter Name`, `State Code`, `County Code`, `Site Num`,`Arithmetic Mean`) %>%
  summarize(Average_Arithmetic_Mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(Average_Arithmetic_Mean))

# Question 4
ABS_Difference <- data %>% filter(`Parameter Name` == "EC PM2.5 LC TOR", `State Name`%in% c("California", "Arizona")) %>%
  select(`Parameter Name`, `State Name`,`Arithmetic Mean`) %>%
  group_by(`State Name`) %>%
  summarize(Average_Arithmetic_Mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>% 
  pull(Average_Arithmetic_Mean) %>%
  diff() %>%
  abs()

#Question 5
Median_Level <- data %>% filter(`Parameter Name` == "OC PM2.5 LC TOR", Longitude <= -100) %>%
  select(`Parameter Name`, `State Name`,`Arithmetic Mean`, Longitude) %>% 
  pull(`Arithmetic Mean`) %>% median()

#Question 6
data_new <- read_xlsx("aqs_sites.xlsx")

Resedential_Suurban <- data_new %>% filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN") %>%
  summarize(n())

#Question 7
Joined_data_median <- right_join(data, data_new, by = c("Longitude", "Latitude")) %>% 
  filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN" & Longitude >= -100 & `Parameter Name` == "EC PM2.5 LC TOR") %>% 
  select(Longitude, `Land Use`, `Arithmetic Mean`, `Location Setting`, `Parameter Name`) %>%
  pull(`Arithmetic Mean`) %>% median()

#Question 8
Joined_data_month <- right_join(data, data_new, by = c("Longitude", "Latitude")) %>% 
  filter(`Land Use` == "COMMERCIAL" & `Parameter Name` == "Sulfate PM2.5 LC") %>% 
  select(`Land Use`, `Arithmetic Mean`, `Parameter Name`, `Date Local`) %>%
  mutate(year = year(`Date Local`),month = months(`Date Local`)) %>%
  group_by(month) %>% summarize(Average_Arithmetic_Mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(Average_Arithmetic_Mean))

#Question 9
Joined_data_code <- right_join(data, data_new, by = c("Longitude", "Latitude")) %>%
  filter(`State Code.y` == 6 & `County Code.y` == 65 & `Site Number` == 8001 & `Parameter Name` %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
  select(`Arithmetic Mean`, `Parameter Name`, `Date Local`) %>%
  group_by(`Date Local`) %>%
  summarize(
    Average_Sulfate = mean(`Arithmetic Mean`[`Parameter Name` == "Sulfate PM2.5 LC"], na.rm = TRUE),
    Average_Nitrate = mean(`Arithmetic Mean`[`Parameter Name` == "Total Nitrate PM2.5 LC"], na.rm = TRUE)
  ) %>%
  mutate(Sum_Avg_Values = Average_Sulfate + Average_Nitrate) %>%
  filter(Sum_Avg_Values > 10) %>%
  summarize(n())

#Question 10
Joined_data_cor <- right_join(data, data_new, by = c("Longitude", "Latitude")) %>%
  filter( `Parameter Name` %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
  select(`State Code.y`, `County Code.y`, `Site Number`, `Arithmetic Mean`, `Parameter Name`, `Date Local`) %>%
  group_by(`State Code.y`, `County Code.y`, `Site Number`, `Date Local`)%>%
  summarize(
    Average_Sulfate = mean(`Arithmetic Mean`[`Parameter Name` == "Sulfate PM2.5 LC"], na.rm = TRUE),
    Average_Nitrate = mean(`Arithmetic Mean`[`Parameter Name` == "Total Nitrate PM2.5 LC"], na.rm = TRUE)
  )
