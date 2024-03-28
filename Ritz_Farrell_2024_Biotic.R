####################################################
#         Ritz & Farrell 2024 YOY NP Biotic        #
#                                                  #
###################################################

library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)
library(ggpubr)

#### Set working directory and files 
setwd("/Users/thorn/OneDrive/Desktop/Ritz_YOY_NP_2023/Catch/")
YOY_NP<- read_excel("YOY_NP_Catch_2022.xlsx")
YOY_NP$Wetland<- as.factor(YOY_NP$Wetland)
YOY_NP$Date<- as.Date(YOY_NP$Date,  format="%Y-%m-%d")
YOY_NP$Type<- as.factor(YOY_NP$Type)
YOY_NP$Status<- as.factor(YOY_NP$Status)

#### Summary data set creation - catch throughout sampling - Figure 3 ####

YOY_NP_Sum<- YOY_NP |>
  group_by(across(c(Date, Wetland, Status))) |>
  dplyr::summarize(Count=n()) |>
  arrange(Date) |>
  group_by(Date) |> 
  dplyr::mutate(Study_Day = cur_group_id())

YOY_NP_Sum$Study_Day<- as.numeric(YOY_NP_Sum$Study_Day)
YOY_NP_Sum$Count<- as.numeric(YOY_NP_Sum$Count)

min<- as.Date(c("2022-06-13"))
max<- as.Date(c("2022-07-02"))

ggplot(YOY_NP_Sum, aes(Date, Count, group=Status, lty=Status))+
  geom_point(fill = "black", size=2)+
  geom_line(linewidth=1)+
  scale_x_date(date_breaks = "2 days", expand = c(0,0),
               minor_breaks = "1 days", limits = c(min,max),
               date_labels = "%m/%d")+
  theme_bw()+
  theme(axis.text = element_text(size=10),legend.position = "bottom", legend.title = element_blank(),
        axis.title = element_text(size=12, face="bold"), 
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle=90),
        legend.text = element_text(size=13),
        strip.text = element_text(size=12),
        strip.background = element_rect (fill = "white"))+
  ylab("YOY NP Catch")+
  scale_y_continuous(breaks = seq(0,28,4), limits=c(0,28))+
  facet_wrap(~Wetland, scales="free")

ggsave("Figure_3.png", dpi=300, height = 5, width=8)


Catch_Sum<- YOY_NP_Sum |>
  group_by(Wetland) |>
  summarize(mean=mean(Count), sd=sd(Count))


#### Length Dataset Creation - Figure 4 ####

YOY_NP$Length<- as.numeric(YOY_NP$Length)
YOY_NP<-na.omit(YOY_NP)
YOY_NP$Location<-as.factor(YOY_NP$Location)


Length_Sum<- YOY_NP |>
  group_by(Wetland, Status) |>
  summarize(Mean_Length=mean(Length), SD_Length=sd(Length))

 
ggplot(YOY_NP, aes(Status, Length))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(fill="#999999", color="#111111", size=0.75)+
  theme_bw()+
  theme(legend.position = "bottom", axis.title.x=element_blank(), legend.text = element_text(size=12),
        axis.text=element_text(size=13), strip.text = element_text(size=13),
        legend.title = element_blank(),
        strip.background = element_rect (fill = "white"),
        axis.title = element_text(size=13))+
  ylab("Total Length (mm)")+
  scale_y_continuous(breaks = seq(40,130,10), limits = c(35,130))+
  facet_wrap(~Wetland, scales="free")
ggsave("Figure_4.png", dpi=300, width=8, height=8)
