
library(ggplot2)
library(tidyverse)
#library(pollen)
library(stringi)
library(lubridate)
#library(ggiraph)
library(viridis)
library(ggnewscale) # used to assign new colour pallete to each dataset
library(geomtextpath)
#library(cowplot)
library(patchwork)

threehour_data<-read.csv("data/3-hour_data.csv")

#Daily average lake temperature
lake_temp<-threehour_data %>% 
  select(c(date, lake_temp_c)) %>% 
  mutate(date2 = as.Date(date, format = "%m/%d/%Y"),
         doy = yday(date2)) %>% 
  separate( date2, c("Year", "Month", "Day"), sep="-") %>% 
  group_by(doy) %>% 
  summarize(lake_max = max(lake_temp_c, na.rm =TRUE),
            lake_min = min(lake_temp_c, na.rm = TRUE),
            lake_avg = mean(lake_temp_c, na.rm =TRUE)) 


### Import Daily air temperature data
daily_data<-read.csv("data/24-hour_data.csv")

### Daily average air temp
airTavg<-daily_data %>% 
  select(c(date, air_temp_max_5m, air_temp_min_5m)) %>% 
  mutate(date2 = as.Date(date, format = "%m/%d/%Y"),
         doy = yday(date2)) %>% 
  separate( date2, c("Year", "Month", "Day"), sep="-") %>% 
  group_by(doy) %>% 
  summarize(air_max = max(air_temp_max_5m, na.rm =TRUE),
            air_min = min(air_temp_min_5m, na.rm =TRUE),
            air_max_avg = mean(air_temp_max_5m, na.rm = TRUE),
            air_min_avg = mean(air_temp_min_5m, na.rm = TRUE))


## Import Hourly data
hourly_data<-read.csv("data/1-hour_data.csv")

## Filter Pyrometer data down to the values greater than 0.015
Pyrometer<-hourly_data %>%   mutate(date2 = as.Date(date),
                             doy = yday(date)) %>% 
  separate( date, c("Year", "Month", "Day"), sep="-") %>%
  filter(Year >2007) %>% 
  mutate(prya2 = if_else(pyranometer > 0.015,1,0)) %>% 
  group_by(Year,doy) %>% 
  summarise(pyra3 = sum(prya2, na.rm = TRUE)) %>% 
  group_by(doy) %>% 
  summarise(pyra4 = max(pyra3))

hourly_all<-hourly_data %>%   mutate(date2 = as.Date(date),
                             doy = yday(date)) %>% 
  separate( date, c("Year", "Month", "Day"), sep="-") %>%
  group_by(Year, doy) %>% 
  filter(Year>2006) %>% 
  summarise(rain2_2 = sum(rain2)) %>% 
  mutate(rain = if_else(rain2_2 > 0, 1, 0)) %>% 
  mutate(rain1 = as.character(rain))

daily_rain<-hourly_all %>%
  select(c(doy, rain1)) %>% 
  filter(!is.na(rain1))

##Read in all naturalist Journal data
nj_data<-read.csv("data/NJ_clim.csv")


## filter Naturalist journal to relevent units
nj<-nj_data %>% 
  filter(!is.na(aurora) & !is.na(cloud_cover) & !is.na(snow_cover) &!is.na(snowed_today)) %>% 
  group_by(Year,doy) %>% 
  reframe(aurora = aurora,
          snowed_today = snowed_today,
          cloud = if_else(cloud_cover > 4, 1,0),
          cloud1 = as.character(cloud),
          snow1 = if_else(snow_cover > 2, 1,0),
          snow2 = as.character(snow1))


### averged years
p<-ggplot() +
  geom_ribbon(data = airTavg, aes(x=doy, ymin=-50, ymax=0), fill = "light blue", alpha =0.2)+
  #geom_line(data = airTavg, aes(x=doy, y=0), colour = "cornflowerblue", linetype = "dotted")+
  geom_ribbon(data = airTavg,aes(x = doy,ymin = air_min,ymax = air_max), fill = "red", alpha=0.1) +
  geom_ribbon(data = airTavg,aes(x = doy,ymin = air_min_avg,ymax = air_max_avg), fill = "red", alpha=0.5) +
  # geom_point(data=h1_all, aes(x=doy, y=45, colour = quant1), size = 2, alpha =0.5)+
  scale_color_manual(values = c('NA', "yellow"))+
  new_scale_color()+
  geom_point(data = nj, aes(x=doy, y=42, colour = aurora), size =4, alpha=0.5)+
  scale_color_manual(values = c('NA', "green", 'NA'))+
  new_scale_color()+
  geom_point(data = nj, aes(x=doy, y=35, colour = cloud1), size = 4, alpha = 0.5)+
  scale_color_manual(values = c('NA', "light grey"))+
  new_scale_color()+
  geom_point(data=nj, aes(x=doy, y=25, colour = snow2),pch=16)+
  scale_color_manual(values = c('NA', "darkslategray3"))+
  new_scale_color()+
  geom_point(data=daily_rain, aes(x=doy, y=30, colour = rain1), size=2, stroke =0.55, alpha = 0.2)+
  scale_color_manual(values = c('NA', "cornflowerblue"))+
  new_scale_color()+
  geom_point(data=nj, aes(x=doy, y=30, colour = snowed_today), pch=8, size = 2, stroke =0.75, alpha = 0.7, na.rm = TRUE)+
  scale_color_manual(values = c('NA', "cadetblue2"))+
  new_scale_color()+
  geom_line(data = Pyrometer, aes(x=doy, y=50, colour = pyra4), size = 5)+
  scale_color_viridis(option = "C")+
  new_scale_color()+
  geom_line(data=lake_temp, aes(x=doy, y=lake_avg), colour="aquamarine4", size=1.5)+
  scale_x_continuous(
    breaks = c(15,45,76,106,137,167,198,228,259,290,320,351),
    minor_breaks = c(30,60,91,121,152,182,213,243,274,304,335,366), limits = c(0,366),labels = c("January", "February", "March", "April", "May","June","July", "August", "September", "October", "November", "December"))+
  scale_y_continuous(limits = c(-50, 50), breaks = c(-40,-20, 0, 20,50), minor_breaks = c(-40,-20, 0, 20)) +
  theme_bw()+
  ggtitle("Through the year at Toolik", subtitle = "Data from the TFS Met Station and Naturalist Journal (2007-2022)")+
  theme(axis.text.x = element_text(size = 14),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"),
        plot.title = element_text(hjust = 0.5, size=15),
        plot.subtitle = element_text(hjust=0.5, size = 10, face = "italic"))

p2<-p+coord_curvedpolar(clip = "on")+
  theme(axis.text.x = element_text(size = 12))

p2

p3<-ggplot()+
  scale_x_continuous(limits=c(0,100))+
  scale_y_continuous(limits=c(0,100))+
  annotate("segment", x=15, xend=30, y=15, yend=30, colour="blue")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        #panel.grid.minor.x = element_blank(),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.y = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

p2+inset_element(p3)
p2 + inset_element(p3, left = 0, bottom = 0, right = 1, top = 1)
