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

#devtools::install_github("clauswilke/ggtext")

setwd("e:/temp/running/running2022")

#图2可视化

data<-read_excel("running.xlsx") %>% 
  na.omit() %>% 
  purrr::set_names(c("date","time","distance","min","s","kj","heart","freq","step","place","item")) %>% 
  arrange(date)

#时间格式处理
data1<-data %>% 
  mutate(label=str_extract(time,"am|pm")) %>% 
  mutate(h=str_extract(time,"\\d+:")) %>% 
  mutate(h=gsub(":","",h)) %>%
  mutate(h=as.numeric(h)) %>%
  mutate(m=str_extract(time,":.+")) %>% 
  mutate(time1=case_when(label=="pm" & h<12 ~paste(h+12,m,sep = ""),T~time)) %>% 
  select(-label,-h,-m) %>% 
  mutate(time1=strptime(time1,"%H:%M")) %>% 
  mutate(time2=hour(time1)+minute(time1)/60)

#分时统计
data2<-data1 %>% 
  select(date,time2) %>% 
  mutate(time3=floor(time2)) %>% 
  data.table() %>% 
  .[,.(.N),by=time3] %>% 
  arrange(time3)

#平均心率统计 配速在5:30-6:30之间的数据，但
data3<-data1 %>% 
  subset(heart!=0) %>% 
  mutate(speed=(min*60+s)/(60*distance)) %>% 
  subset(speed>5.5 & speed <6.5) %>% 
  #mutate(speed=floor(speed))%>% 
  mutate(time3=floor(time2)) %>% 
  select(date,time3,speed,heart) %>% 
  data.table() %>% 
  .[,.(.N,m_heart=mean(heart)),by=c("time3")] %>% 
  arrange(time3) %>% 
  mutate(m_heart=ceiling(m_heart))

sum(data3$N)

df<-data2 %>% 
  left_join(.,data3,by="time3") %>% 
  select(1,2,4) %>% 
  purrr::set_names(c("time3","N","m_heart")) %>% 
  mutate(time4=paste(time3,":00",sep=""))

clock<-paste(1:24,":00",sep="")

ggplot(df) +
  geom_bar(aes(x=time3, y=N),width = 1,stat="identity",
           colour = "black",fill="#F8766D") +
  geom_bar(data=data1,aes(x=time2,y=1),width = 0.01,stat="identity",
           colour = "black",fill="#F8766D")+
  geom_text(aes(x=time3,y = N,label = N),color="black") +
  geom_line(aes(x=time3,y = m_heart),color="red") +
  geom_smooth(aes(x=time3,y = m_heart),color="#33A02C")+
  geom_text(aes(x=time3,y = m_heart,label=m_heart),color="red") +
  ylim(c(0,200))+
  geom_label(aes(x=6,y=25,label="累计跑次数"),size=4,color="#F8766D")+
  geom_label(aes(x=6,y=150,label="平均心率"),size=4,color="#33A02C")+
  #矩形框
  geom_rect(aes(xmin=4.5,xmax=7.5,ymin=165,ymax=180),color="red",alpha=0,linetype="dotted")+
  geom_text(aes(x=6,y=185,label="早上刚睡醒"),color="red")+
  geom_rect(aes(xmin=13.5,xmax=15.5,ymin=165,ymax=172),color="red",alpha=0,linetype="dotted")+
  geom_text(aes(x=14.5,y=177,label="中午刚睡醒"),color="red")+
  geom_vline(xintercept=12,size=0.5)+
  theme_few()+
  xlab("时刻")+
  ylab("跑步次数 & 平均心率")+
  scale_x_discrete(limits=clock)+
  labs(title = "晨跑、夜跑都好，不要刚睡醒就跑！",
       caption = "结论：尽量避免早睡或午睡刚醒来就跑，这时心率都略高\n (筛选配速在5:30-6:30之间的数据共164条进行时刻平均心率统计)"
  )

ggsave("2022年跑步图3.png",dpi=300,width=30, height=18,units="cm")

#平均心率
#累计公里




# pm24<-function(string){
#   #string<-"1:05pm"
#   label<-str_extract(string,"am|pm")
#   h<-str_extract(string,"\\d+:") %>% 
#     gsub(":","",.) %>% 
#     as.numeric()
#   m<-str_extract(string,":.+") 
#   if(label=="pm" & h<12){
#     result<-paste(h+12,m,sep = "")
#   }
#   return(result)
# }
# 
# pm24("1:05pm")
# 
# strptime("1:05pm", "%H:%M")
# strptime("1:05am", "%H:%M")
