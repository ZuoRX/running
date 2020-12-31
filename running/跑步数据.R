library(data.table)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(RColorBrewer)
library(hrbrthemes)
library(ReadAxfBOM)
library(ggtext)
library(magrittr)
library(geojsonsf)
library(jsonlite)
library(worldtilegrid)
library(hotkeys)
library(sf)
library(fs)
library(cowplot)
library(patchwork)
#devtools::install_github("clauswilke/ggtext")

setwd("c:/users/lenovo/desktop/running")

data<-read_excel("running.xlsx") %>% 
  na.omit() %>% 
  arrange(date)


#------------------------------#
#---1.生活压力大，随汗全挥下---#
#------------------------------#
#字体设置
windowsFonts(
  mini = windowsFont("迷你简凌波"),
  minij = windowsFont("迷你简瘦金书"),
  fF = windowsFont("腾祥范笑歌楷书繁"),
  fJ = windowsFont("腾祥范笑歌楷书简"),
  bF = windowsFont("腾祥伯当行楷繁"),
  bJ = windowsFont("腾祥伯当行楷简"),
  en = windowsFont("Times New Roman")
)

df<-data[-c(1:7),c(1,3)] %>% 
  mutate(date=as.POSIXct(date)) %>% 
  mutate(month=as.factor(month(date)))

df1<-df %>% 
  data.table() %>% 
  .[,sum(distance),by=month] %>% 
  mutate(date=c("2020-2-15","2020-3-15","2020-4-15","2020-5-15","2020-6-15","2020-7-15",
                "2020-8-15","2020-9-15","2020-10-15","2020-11-15","2020-12-15")) %>% 
  mutate(date=as.POSIXct(date)) %>% 
  purrr::set_names("month","distance","date") 



p1<-ggplot(df,aes(x=date,y=distance,color=month))+
  scale_y_continuous(
    name = "公里数",
    sec.axis = sec_axis(~.*10, name="月累计公里数/km")
  ) + 
  scale_x_datetime(date_labels = "%m/%d",
                   date_breaks = "1 month",
                   limits=c(as.POSIXct("2020-1-1"),as.POSIXct("2020-12-31")),
                   date_minor_breaks = "1 month")+
  scale_color_manual(values =c(brewer.pal(9,"Paired"),brewer.pal(9,"Set1")[8],"#F0027F"))+
  theme_bw()+
  ylab("公里数")+
  xlab("日期")+
  geom_line(size=1)+
  geom_point(size=1.5)+
  geom_point(data=df1,aes(x=date,y=distance/10),color="black",size=2)+
  geom_line(data=df1,aes(x=date,y=distance/10),color="red",size=1)+
  geom_text(data=df1,aes(x=date,y=distance/10+0.15,label=distance),
            color="black",family="bJ")+
  geom_hline(aes(yintercept=5.42), colour="#990000", linetype="dashed")+
  geom_text(aes(x=as.POSIXct("2020-1-15"),y=5.6,label="平均5.42km/次"),
            color="#990000",family="bJ")+
  theme_ipsum(base_family = "bJ",caption_family = "bJ",plot_title_family ="bJ",
              strip_text_face = "bJ") + 
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        axis.title.y.right = element_text(color = "red", size=10)) +
  labs(title = "图1：生活压力大，随汗全挥下",
       caption = "2020年累计跑步107次，总里程579.94公里。(数据：小米手环4)")
  
#ggsave("fig1_MonthDistance.png",dpi=600,width=20, height=15,units="cm")

#信息更新
mean(df$distance)
length(df$distance)
sum(df$distance)


#南丁格尔玫瑰图
#------------------------------#
#---2.健康有氧跑，燃脂提心率---#
#------------------------------#
#有氧无氧心率临界点：
#[220(最大心率)-27(年龄)]*0.8=154

#看颜色
#brewer.pal(9,"Paired") %>% scales::show_col()

df<-data[-c(1:7),c(1,7)] %>% 
  mutate(date=as.POSIXct(date)) %>% 
  mutate(month=as.factor(month(date))) %>% 
  subset(heart>=140)

#mean(df$heart)

p2<-ggplot(df,aes(x=date,y=heart))+
  ylab("心率")+
  xlab("日期")+
  geom_line(size=0.7,color="#A6CEE3")+
  geom_hline(aes(yintercept=165.3), colour="#33A02C", linetype="dashed")+
  geom_text(aes(x=as.POSIXct("2020-1-15"),y=167,label="平均165.3"),
            color="#33A02C",family="bJ")+
  geom_hline(aes(yintercept=154), colour="#33A02C", linetype="dashed")+
  geom_text(aes(x=as.POSIXct("2020-1-15"),y=155,label="临界点154"),
            color="#33A02C",family="bJ")+
  scale_x_datetime(date_labels = "%m/%d",
                   date_breaks = "1 month",
                   limits=c(as.POSIXct("2020-1-1"),as.POSIXct("2020-12-31")),
                   date_minor_breaks = "1 month")+
  theme_ipsum(base_family = "bJ",caption_family = "bJ",plot_title_family ="bJ",
              strip_text_face = "bJ") + 
  theme(legend.position="none",
        axis.line = element_line(colour = "black"),
        axis.title.y.right = element_text(color = "red", size=10)) +
  labs(title = "图2：燃脂提心率,健康有氧跑",
       caption = "有氧无氧心率临界点：\n[220(最大心率)-27(年龄)]*0.8=154")

#ggsave("fig2_heartbeat.png",dpi=600,width=20, height=15,units="cm")


#--------------------------------------#
#---3.提步频，压步幅，远伤增速不糊涂---#
#--------------------------------------#
#双坐标

df<-data[-c(1:7),c(1,8,9)] %>% 
  mutate(date=as.POSIXct(date)) %>% 
  mutate(month=as.factor(month(date))) %>% 
  subset(freq>=130) %>% 
  subset(step>=71)

# mean(df$freq)
# mean(df$step)

p3<-ggplot(df,aes(x=date,y=step))+
  scale_y_continuous(
    name = "厘米(步幅)",
    sec.axis = sec_axis(~.*2, name="步/分钟(步频)")
  ) + 
  scale_x_datetime(date_labels = "%m/%d",
                   date_breaks = "1 month",
                   limits=c(as.POSIXct("2020-1-1"),as.POSIXct("2020-12-31")),
                   date_minor_breaks = "1 month")+
  scale_color_manual(values =c(brewer.pal(9,"Paired"),brewer.pal(9,"Set1")[8],"#F0027F"))+
  theme_bw()+
  xlab("日期")+
  geom_line(size=1,color="#33A02C")+
  geom_smooth(color="#33A02C")+
  geom_line(aes(x=date,y=freq/2),color="red",size=1)+
  geom_smooth(aes(x=date,y=freq/2),color="red")+
  theme_ipsum(base_family = "bJ",caption_family = "bJ",plot_title_family ="bJ",
              strip_text_face = "bJ") + 
  theme(legend.position="none",
        plot.title = element_markdown(),
        axis.line = element_line(colour = "black"),
        axis.title.y = element_text(color = "#33A02C", size=10),
        axis.title.y.right = element_text(color = "red", size=10)) +
  labs(title = "图3：提<b style='color:#E31A1C'>步频</b>，压<b style='color:#009E73'>步幅</b>，远伤增速不糊涂",
       caption = "总计104条数据(数据：小米手环4)")

#ggsave("fig3_stepFreq.png",dpi=600,width=20, height=15,units="cm")


#-----------------------------------#
#---4.快乐肥宅水，喝完Run in Vain---#
#-----------------------------------#
#能量消耗图+可乐均线

df<-data[-c(1:7),c(1,3:6)] %>% 
  mutate(date=as.POSIXct(date)) %>% 
  mutate(month=as.factor(month(date))) %>% 
  mutate(kj_t=kj/(min+s/60)) %>% 
  mutate(kj_t=round(kj_t,1)*10) %>% 
  mutate(kj_d=kj/distance) %>% 
  mutate(kj_d=round(kj_d,1)*5) %>% 
  .[,c(1,6:8)]

# mean(df$kj_d)
# mean(df$kj_t)

p4<-ggplot(df,aes(x=date,y=kj_d))+
  scale_y_continuous(
    name = "5公里能耗(kj/5km)",
    sec.axis = sec_axis(~.*(1/4), name="10分钟能耗(kj/10min)")
  ) + 
  scale_x_datetime(date_labels = "%m/%d",
                   date_breaks = "1 month",
                   limits=c(as.POSIXct("2020-1-1"),as.POSIXct("2020-12-31")),
                   date_minor_breaks = "1 month")+
  theme_bw()+
  xlab("日期")+
  #kj/5km
  geom_line(aes(x=date,y=kj_d),size=1,color="#E31A1C")+
  geom_hline(aes(yintercept=mean(kj_d)), colour="#E31A1C", linetype="dashed")+
  geom_text(aes(x=as.POSIXct("2020-1-15"),y=mean(kj_d)+8,label="平均370kj/5km"),
            color="#E31A1C",family="bJ")+
  #kj/10min
  geom_line(aes(x=date,y=kj_t*4),size=1,color="#33A02C")+
  geom_hline(aes(yintercept=4*mean(kj_t)), colour="#33A02C", linetype="dashed")+
  geom_text(aes(x=as.POSIXct("2020-1-15"),y=4*mean(kj_t)+10,label="平均123kj/10min"),
            color="#33A02C",family="bJ")+
  #可乐 594
  geom_hline(aes(yintercept=594), colour="#1F78B4", linetype="dashed")+
  geom_text(aes(x=as.POSIXct("2020-2-5"),y=585,label="一罐可乐的能量=594kj"),
            color="#1F78B4",family="bJ")+
  theme_ipsum(base_family = "bJ",caption_family = "bJ",plot_title_family ="bJ",
              strip_text_face = "bJ") + 
  theme(legend.position="none",
        plot.title = element_markdown(),
        axis.line = element_line(colour = "black"),
        axis.title.y = element_text(color = "red", size=10),
        axis.title.y.right = element_text(color ="#33A02C" , size=10)) +
  labs(title = "图4：快乐肥宅水，喝完Run in Vain",
       caption = "一罐330ml可乐含能量594kj\n平均需跑步8.03公里或跑步48分18秒才可消耗")

#ggsave("fig4_kj_5km_10min.png",dpi=600,width=20, height=15,units="cm")

# #快乐肥宅水，
# #330ml可乐能量
# 180*3.3
# #10min消耗能量   5公里消耗能量
# 594*5/370
# 594/123
# 0.29268*60

#----------------------------------------#
#---5.跑过古都，跨过小山村，此身属江城---#
#----------------------------------------#
#西安-罗田-武汉   注释总里程

run<-read_excel("running.xlsx") %>% 
  na.omit() %>% 
  arrange(date) %>% 
  data.table() %>% 
  .[,sum(distance),by=place] %>% 
  purrr::set_names(c("place","distance")) %>% 
  mutate(distance=round(distance,1)) %>% 
  mutate(place=c("西安市","黄冈市","武汉市"))

shaanxi_map<-st_read("陕西省.geojson") %>%
  mutate(place_abbre=str_sub(.$name,1,2)) %>% 
  mutate(X=gsub("c\\(|,.+","",.$center)) %>% 
  mutate(Y=gsub("\\)|.+,","",.$center)) %>% 
  mutate_at(c("X","Y"),as.numeric) 

hugang_map<-st_read("黄冈市.json") %>%
  mutate(place_abbre=str_sub(.$name,1,2)) %>% 
  mutate(X=gsub("c\\(|,.+","",.$center)) %>% 
  mutate(Y=gsub("\\)|.+,","",.$center)) %>% 
  mutate_at(c("X","Y"),as.numeric)

hubei_map<-st_read("湖北省.json") %>% 
  mutate(place_abbre=str_sub(.$name,1,2)) %>% 
  mutate(X=gsub("c\\(|,.+","",.$center)) %>% 
  mutate(Y=gsub("\\)|.+,","",.$center)) %>% 
  mutate_at(c("X","Y"),as.numeric)

#合并画图数据
data<-rbind(hubei_map,hugang_map,shaanxi_map) %>% 
  mutate(name=as.character(name)) %>% 
  mutate(label='') 

data$label[1]<-data$name[1]
data$label[21]<-data$name[21]
data$label[28]<-data$name[28]


p5<-ggplot(data[-10,]) + 
  geom_sf(aes(geometry = geometry),
          size = 0.5, color = "black",alpha=0.9) + 
  geom_sf(data=data[10,],aes(geometry = geometry),
          size = 0.5, color = "black",alpha=0.9)+
  geom_point(data=data[c(1,21,28),],aes(x = X,y = Y),shape=1,size=1,color="black")+
  geom_point(aes(x=115.48826,y=30.64431),shape=16,size=1,color="red")+
  # geom_text(aes(x=116.28826,y=30.64431,label="My Home"),family="bJ",
  #           nudge_x = 0.06,nudge_y = 0.1,
  #           size=3.2,color="red")+
  geom_text(aes(x = X,y = Y+0.05,label=label),family="bJ",
            nudge_x = 0.06,nudge_y = 0.1,size=4,color="black")+
  #西安108.948024, 34.263161-->罗田x=115.48826,y=30.64431
  geom_curve(aes(x = 108.948024, y = 34.263161, xend = 115.48826, yend = 30.64431),
             angle = 90,curvature = -0.5,color="red",
               arrow = arrow(length = unit(0.2,"cm")))+
  #罗田-->武汉114.298572, 30.584355
  geom_curve(aes(x = 115.48826, y = 30.64431, xend = 114.298572, yend = 30.584355),
             angle = 90,curvature = -0.5,color="red",
             arrow = arrow(length = unit(0.2,"cm")))+
  theme_ipsum(base_family = 'bJ',caption_family = "bJ") + 
  labs(title = "图5：邂逅古都，源起小山村，此身属江城",
       caption = "2019年7月22日至今跑路里程\n西安:78.9km;罗田:342.1km;武汉:237.8km")

p5

ggsave("fig5_3place.png",dpi=600,width=17, height=18,units="cm")



p1 + plot_spacer() + p2 +
  plot_spacer() + p5 + plot_spacer() +
  p3 + plot_spacer() + p4

ggsave("fig_5.png",dpi=300,width=40, height=40,units="cm")



