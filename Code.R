library(tidytuesdayR)
library(tidyverse)
library(lubridate)
tuesdata <- tidytuesdayR::tt_load('2022-02-08')
tuesdata$airmen->data

unique(data$pilot_type)
str_replace(data$pilot_type,"Liason pilot","Liaison pilot")->data$pilot_type
str_replace(data$pilot_type,"Liaison pilot","Liaison")->data$pilot_type

data%>%
  distinct()%>%
  mutate(Year=year(graduation_date))%>%
  select(c(name,class,pilot_type,Year))%>%
  drop_na()%>%
  group_by(pilot_type,Year)%>%
  count()%>%
  arrange(Year)->dataf

ggplot(dataf,aes(Year,pilot_type,size=n))+
geom_point(aes(size=n),pch=21,fill="yellow",colour="white")+
  scale_size_continuous(limits=c(0,300),breaks=c(0,50,100,150,200,250,300))+
  labs(size="Number of Pilots: ")+
  theme(plot.margin=unit(c(0.5,1.5,0.5,1.5),"cm"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_line(colour="gray10"),
        axis.text = element_text(colour="white",size=10,face="bold"),
        axis.title.x = element_text(colour="white",size=10, face="bold",margin=margin(t=15)),
        axis.title.y = element_text(colour="white",size=10, face="bold",margin=margin(r=15)),
        legend.background = element_rect(fill="black"),
        legend.key = element_rect(fill="black"),
        legend.text=element_text(colour="white",margin=margin(l=15),face="bold"),
        legend.key.height = unit(1,"cm"),
        legend.box = "vertical",
        legend.title = element_text(colour="white",face="bold",size=12,margin=margin(b=15)),
        legend.position = "right",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.border = element_rect(fill=NA,colour = "gray50"),
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
  labs(title="AFRICAN AMERICAN PILOTS WHO GRADUATED FROM 1942-1948",
       subtitle=str_wrap("The Tuskegee Airmen were the first African-American military aviators in the United States Armed Forces. During World War II, black Americans in many U.S. states were still subject to the Jim Crow laws and the American military was racially segregated, as was much of the federal government. The Tuskegee Airmen were subjected to discrimination, both within and outside of the army. The below visualization looks at the number of pilots under each type (liaison, single engine and twin engine) who graduated from 1942 to 1948",120),
       caption = "Data via Tidy Tuesday| Analysis and design: @annapurani93")+
  xlab("-------------YEAR---------------")+
  ylab("-------------PILOT TYPE--------------")->plot

ggsave("airmen.png",plot,width=11,height=7)
ggsave("airmen.pdf",plot,width=11,height=7)
