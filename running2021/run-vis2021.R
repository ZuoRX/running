library(data.table)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(RColorBrewer)
library(hrbrthemes)
library(ggtext)
library(magrittr)
library(geojsonsf)
library(jsonlite)
library(worldtilegrid)
library(hotkeys)
library(cowplot)
library(patchwork)#双坐标
library(animation)
library(gganimate)
library(readxl)
#devtools::install_github("clauswilke/ggtext")

setwd("e:/temp/running/running2021")

data<-read_excel("running.xlsx") %>% 
  na.omit() %>% 
  purrr::set_names(c("date","time","distance","min","s","kj","heart",    "freq","step","place","item")) %>% 
  arrange(date)

names(data)
str(data)

####  弄个facet，看能做成动态图不
####  分区底色

#每月一公里最快配速趋势图

df<-data[,c(1,3,4,5)] %>% 
  mutate(date=ymd(date)) %>% 
  mutate(year=year(date)) %>% 
  mutate(month=month(date)) %>% 
  mutate(speed=(min*60+s)/(60*distance)) %>% 
  data.table() %>% 
  .[,.(speed=min(speed),distance=sum(distance)),by=c("year","month")] %>% 
  mutate(min1=floor(speed)) %>% 
  mutate(s1=floor((speed-min1)*60)) %>% 
  mutate(label=paste(min1,s1,sep=":")) %>% 
  mutate(date=paste(year,month,15,sep="-")) %>% 
  mutate(date=ymd(date)) %>% 
  select(date,speed,label,distance) %>% 
  mutate(label=case_when(label=="5:3"~"5:03",
                         label=="5:5"~"5:05",
                         label=="5:2"~"5:02",
                         T~label)) %>% 
  mutate(year=year(date)) %>% 
  mutate(date=as.POSIXct(date))

df %>% 
  data.table() %>% 
  .[,sum(distance),by=year]


windowsFonts(
  mini = windowsFont("迷你简凌波"),
  minij = windowsFont("迷你简瘦金书"),
  fF = windowsFont("腾祥范笑歌楷书繁"),
  fJ = windowsFont("腾祥范笑歌楷书简"),
  bF = windowsFont("腾祥伯当行楷繁"),
  bJ = windowsFont("腾祥伯当行楷简"),
  en = windowsFont("Times New Roman")
)

ggplot(df,aes(x=date))+
  geom_line(aes(y=speed),color="red",size=1)+
  geom_point(aes(y=speed),size=0.7)+
  geom_text(aes(y=speed,label=label))+
  #3.最快纪录标记解释
  geom_text(aes(x=as.POSIXct("2021-4-22"),y=4.5,label="2021.4.22,跑步机6公里"))+
  geom_text(aes(x=as.POSIXct("2020-10-31"),y=4.75,label="2020.10.31,田径场5公里"))+
  geom_segment(mapping = aes(x=as.POSIXct("2021-4-22"), y=6, 
                             xend=as.POSIXct("2021-6-15"), yend=5), 
               arrow = arrow(length=unit(0.2, "cm"))) +
  geom_text(aes(x=as.POSIXct("2021-4-21"),y=6,label="2021.6.21,东湖10公里"))+
  #1.起步5-6-7分割线
  geom_vline(xintercept=as.POSIXct("2020-11-19"),size=0.5)+
  geom_text(aes(x=as.POSIXct("2020-11-19"),y=6.7,label="2020.11.20"),family="bJ",size=6)+
  geom_vline(xintercept=as.POSIXct("2021-9-1"),size=0.5)+
  geom_text(aes(x=as.POSIXct("2021-9-1"),y=6.7,label="2021.09.01"),family="bJ",size=6)+
  #2.分割箭头和标签
  #起步5公里标签箭头
  geom_text(aes(x=as.POSIXct("2020-7-1"),y=6.7,label="起步5公里"),family="bJ",size=6)+
  geom_segment(mapping = aes(x=as.POSIXct("2020-10-1"), y=6.7, 
                             xend=as.POSIXct("2020-9-1"), yend=6.7),
               size=1.5,arrow = arrow(length=unit(0.2, "cm"))) +
  #起步6公里标签箭头
  geom_text(aes(x=as.POSIXct("2021-4-15"),y=6.7,label="起步6公里"),family="bJ",size=6)+
  geom_segment(mapping = aes(x=as.POSIXct("2021-1-15"), y=6.7, 
                             xend=as.POSIXct("2021-2-15"), yend=6.7),
               size=1.5,arrow = arrow(length=unit(0.2, "cm"))) +
  geom_segment(mapping = aes(x=as.POSIXct("2021-7-15"), y=6.7, 
                             xend=as.POSIXct("2021-6-15"), yend=6.7),
               size=1.5,arrow = arrow(length=unit(0.2, "cm"))) +
  #起步7公里标签箭头
  geom_text(aes(x=as.POSIXct("2021-12-15"),y=6.7,label="7公里"),family="bJ",size=6)+
  geom_segment(mapping = aes(x=as.POSIXct("2021-10-15"), y=6.7, 
                            xend=as.POSIXct("2021-11-15"), yend=6.7),
               size=1.5,arrow = arrow(length=unit(0.2, "cm"))) +
  #4.每年公里数
  geom_segment(mapping = aes(x=as.POSIXct("2019-7-3"), y=4.2,
                            xend=as.POSIXct("2019-12-28"), yend=4.2),
               size=0.7,arrow = arrow(length=unit(0.2, "cm"))) +
  geom_segment(mapping = aes(x=as.POSIXct("2020-1-3"), y=4.2,
                            xend=as.POSIXct("2020-12-28"), yend=4.2),
               size=0.7,arrow = arrow(length=unit(0.2, "cm"))) +
  geom_segment(mapping = aes(x=as.POSIXct("2021-1-3"), y=4.2,
                            xend=as.POSIXct("2021-12-28"), yend=4.2),
               size=0.7,arrow = arrow(length=unit(0.2, "cm"))) +
  geom_text(aes(x=as.POSIXct("2019-10-1"),y=4.3,label="2019年:78.92公里"),family="bJ",size=5)+
  geom_text(aes(x=as.POSIXct("2020-6-15"),y=4.3,label="2020年:585.96公里"),family="bJ",size=5)+
  geom_text(aes(x=as.POSIXct("2021-6-15"),y=4.3,label="2021年:868.66公里"),family="bJ",size=5)+
  scale_x_datetime(date_labels = "%m",
                   date_breaks = "1 month",
                   date_minor_breaks = "1 month")+
  xlab("时间")+
  ylab("速度(分/公里)")+
  ylim(4.2,7)+
  theme_bw()+
  labs(title = "2021年跑步年度总结")+
  theme(axis.text.x = element_text(hjust = -1))

ggsave("2021年跑步图.png",dpi=600,width=30, height=18,units="cm")

#可以考虑加上次数,刻度标签往右一点


#动图思路
#1.多个png，做成gif
#2.柱状图、折线图，逐月变化
#3.地图背景

