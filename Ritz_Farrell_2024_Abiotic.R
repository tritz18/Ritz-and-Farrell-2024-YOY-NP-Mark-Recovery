################################################################
#                                                              #
#   Figures and Summary Stats For Ritz & Farrell 2024 YOY NP   #
#                                                              #
################################################################

library(tidyverse)
library(readxl)
library(writexl)
library(patchwork)

## Set working directory and files ##
setwd("/Users/thorn/OneDrive/Desktop/Ritz_YOY_NP_2023/Abiotic/")
f = list.files(pattern="*.xlsx")

#### Clean logger data from folder. Collects all logger files ##
## and defines different variables in correct r format. ##
## Filters data to select stocking to catch ####

Abiotic_Raw <- purrr::map_df(f, function(x) {
  mydata <- read_excel(x)
  mydata$Date_Time <- as.POSIXct(mydata$Date_Time,  format="%Y/%m/%d %H:%M")
  mydata$Location<- as.factor(mydata$Location)
  mydata$Wetland<- as.factor(mydata$Wetland)
  mydata$Type <-as.factor(mydata$Type)
  mydata |>
    filter(Date_Time < "2022-07-01" & Date_Time > "2022-04-29") |>
    mutate(Date = as.Date(Date_Time)) |> 
    select(-Date_Time)
})

#### Filter out sites not used in this paper but monitored during this study ####

Abiotic<- Abiotic_Raw |>
  filter(Location %in% c("CHIP_REF3", "CHIP_REF4", "CHIP_SP2",
         "CM_REF4", "CM_REF5", "CM_SP1", "CM_SP2",
         "FC_REF7", "FC_SP5", "FC_SP6", 
         "PV_REF1", "PV_REF2", "PV_SP4", "PV_SP5"))


#### Generate summary file for mean daily DO TEMP  and SD ####

Abiotic_Summary<- Abiotic |>
  group_by(Wetland) |>
  summarize(Mean_TEMP=mean(TEMP), sd_TEMP=sd(TEMP), 
            Mean_DO= mean(DO), sd_DO=sd(DO))

#### Figure 4 Generation Water TEMP DO BOXPLOT ####

TEMP<- ggplot(Abiotic, aes(Wetland, TEMP))+
  stat_boxplot(geom="errorbar")+
         geom_boxplot(fill="#999999")+
  theme_bw()+
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=14),
  axis.title.x= element_blank())+
  scale_y_continuous(breaks=seq(4,30,6), limits=c(3.9, 31))+
  ylab("Water Temperature (C°)") 

TEMP

DO<- ggplot(Abiotic, aes(Wetland, DO))+
  stat_boxplot(geom="errorbar")+
  geom_boxplot(fill="#999999")+
  theme_bw()+
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        axis.title.x= element_blank())+
  scale_y_continuous(breaks=seq(0,20,4), limits=c(0,20))+
  ylab("Dissolved Oxygen (mg/l)") 


Figure_4<- TEMP/DO

ggsave("Figure_4.png", dpi = 300)



#### Abiotic Data Scale and Centering for Modeling ####

#### Daily DO Mean ####

Abiotic_Sum<- Abiotic_Raw |>
  summarize(Mean_DO= mean(DO), Mean_TEMP=mean(TEMP))

Chip_DO<- Abiotic_Sum |>
  filter(Wetland %in% "Chippewa Creek") |>
  select(Date, Mean_DO, Wetland) |>
  mutate(Mean_DO = scale(Mean_DO, center=TRUE ,scale=TRUE)) |>
  group_by(Date) |>
  arrange(Date) |>
  mutate(Study_Day = cur_group_id()) |>
  ungroup() |>
  select(-Date, -Wetland) |>
  pivot_wider(names_from = Study_Day, values_from = Mean_DO) 

Cran_DO<- Abiotic_Sum |>
  filter(Wetland %in% "Cranberry Creek") |>
  select(Date, Mean_DO, Wetland) |>
  mutate(Mean_DO = scale(Mean_DO, center=TRUE ,scale=TRUE)) |>
  group_by(Date) |>
  arrange(Date) |>
  mutate(Study_Day = cur_group_id()) |>
  ungroup() |>
  select(-Date, -Wetland) |>
  pivot_wider(names_from = Study_Day, values_from = Mean_DO) 

French_DO<- Abiotic_Sum |>
  filter(Wetland %in% "French Creek") |>
  select(Date, Mean_DO, Wetland) |>
  mutate(Mean_DO = scale(Mean_DO, center=TRUE ,scale=TRUE)) |>
  group_by(Date) |>
  arrange(Date) |>
  mutate(Study_Day = cur_group_id()) |>
  ungroup() |>
  select(-Date, -Wetland) |>
  pivot_wider(names_from = Study_Day, values_from = Mean_DO) 

PV_DO<- Abiotic_Sum |>
  filter(Wetland %in% "Point Vivian") |>
  select(Date, Mean_DO, Wetland) |>
  mutate(Mean_DO = scale(Mean_DO, center=TRUE ,scale=TRUE)) |>
  group_by(Date) |>
  arrange(Date) |>
  mutate(Study_Day = cur_group_id()) |>
  ungroup() |>
  select(-Date, -Wetland) |>
  pivot_wider(names_from = Study_Day, values_from = Mean_DO) 

Abiotic_DO<- bind_rows(Chip_DO, 
                       Cran_DO, 
                       French_DO,
                       PV_DO)
Abiotic_DO<- as.matrix(Abiotic_DO)
Abiotic_DO<- as.data.frame(Abiotic_DO)

write_xlsx(Abiotic_DO, "YOY_NP_DO_mean_Wetland.xlsx")

#### Daily Water Temperature Mean ####

Chip_TEMP<- Abiotic_Sum |>
  filter(Wetland %in% "Chippewa Creek") |>
  select(Date, Mean_TEMP, Wetland) |>
  mutate(Mean_TEMP = scale(Mean_TEMP, center=TRUE ,scale=TRUE)) |>
  group_by(Date) |>
  arrange(Date) |>
  mutate(Study_Day = cur_group_id()) |>
  ungroup() |>
  select(-Date, -Wetland) |>
  pivot_wider(names_from = Study_Day, values_from = Mean_TEMP) 

Cran_TEMP<- Abiotic_Sum |>
  filter(Wetland %in% "Cranberry Creek") |>
  select(Date, Mean_TEMP, Wetland) |>
  mutate(Mean_TEMP = scale(Mean_TEMP, center=TRUE ,scale=TRUE)) |>
  group_by(Date) |>
  arrange(Date) |>
  mutate(Study_Day = cur_group_id()) |>
  ungroup() |>
  select(-Date, -Wetland) |>
  pivot_wider(names_from = Study_Day, values_from = Mean_TEMP) 

French_TEMP <- Abiotic_Sum |>
  filter(Wetland %in% "French Creek") |>
  select(Date, Mean_TEMP, Wetland) |>
  mutate(Mean_TEMP = scale(Mean_TEMP, center=TRUE ,scale=TRUE)) |>
  group_by(Date) |>
  arrange(Date) |>
  mutate(Study_Day = cur_group_id()) |>
  ungroup() |>
  select(-Date, -Wetland) |>
  pivot_wider(names_from = Study_Day, values_from = Mean_TEMP) 

PV_TEMP<- Abiotic_Sum |>
  filter(Wetland %in% "Point Vivian") |>
  select(Date, Mean_TEMP, Wetland) |>
  mutate(Mean_TEMP = scale(Mean_TEMP, center=TRUE,scale=TRUE)) |>
  group_by(Date) |>
  arrange(Date) |>
  mutate(Study_Day = cur_group_id()) |>
  ungroup() |>
  select(-Date, -Wetland) |>
  pivot_wider(names_from = Study_Day, values_from = Mean_TEMP) 

Abiotic_TEMP<- bind_rows(Chip_TEMP, 
                         Cran_TEMP, 
                         French_TEMP, 
                         PV_TEMP)
Abiotic_TEMP<- as.matrix(Abiotic_TEMP)
Abiotic_TEMP<- as.data.frame(Abiotic_TEMP)

write_xlsx(Abiotic_TEMP, "YOY_NP_TEMP_mean_Wetland.xlsx")



