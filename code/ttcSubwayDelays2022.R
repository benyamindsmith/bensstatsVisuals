library(opendatatoronto)
library(tidyverse)
library(ggrepel)
library(ggdark)
library(rvest)
library(tidygeocoder)
library(extrafont)
font_import()
loadfonts(device = "win")

ttcSubwayShapefiles<- list_package_resources("c01c6d71-de1f-493d-91ba-364ce64884ac") %>% 
  get_resource() 

torontoMap <- list_package_resources("neighbourhoods") %>% 
  slice(1) %>% 
  get_resource()

delayTimes<-list_package_resources("996cfe8d-fb35-40ce-b569-698d51fc683b") %>% 
  slice(9) %>% 
  get_resource() %>% 
  filter(Station %>% grepl("STATION",.)) %>% 
  mutate(Station_new = Station %>% 
           str_remove_all("( MC )|( BD )|( SRT )| TO.*|\\(.*|( YUS )|( CTR )|(LA$)") %>% 
           str_replace_all("STATION"," STATION") %>% 
           str_replace_all("BLOOR","BLOOR–YONGE") %>%
           str_replace_all("ST ","ST. ") %>% 
           str_replace_all("\\s+"," ") %>% 
           trimws() %>% 
           str_to_title() %>% 
           str_replace_all("East\\.","East") %>% 
           str_replace_all("West\\.","West") %>%
           str_replace_all("Bathurst.","Bathurst") %>% 
           str_replace_all("North York","North York Centre") %>%
           str_replace_all("Mccowan","McCowan") %>%
           str_remove_all(" Station") %>% 
           str_replace_all("^Yonge$","Bloor–Yonge") %>% 
           str_replace_all("Sheppard-Yonge","Sheppard–Yonge") %>% 
           str_replace_all("Bay Lower","Bay") %>% 
           str_replace_all("^Sheppard$","Sheppard-West") %>% 
           str_replace_all("Downview","Downsview")) %>%
  left_join(subwayStations, by=c("Station_new"="Station")) %>% 
  group_by(Station_new) %>%
  summarize(avgDelay=mean(`Min Delay`)) %>% 
  left_join(subwayStations,by=c("Station_new"="Station"))

subwayStations<-read_html("https://en.wikipedia.org/wiki/List_of_Toronto_subway_stations")%>%
  html_elements(xpath='//*[@id="mw-content-text"]/div[1]/table[2]')%>%
  html_table() %>% 
  .[[1]] %>% 
  mutate(fullAddress = paste0(Station," Station, Toronto, Canada"))

# Doing some spot check geocoding
subwayStations <- subwayStations %>% 
  geocode(address=fullAddress) %>% 
  mutate(lat = case_when(Station == "St. Patrick" ~ 43.6548308,
                         Station == "Highway 407" ~ 43.783215,
                         Station == "Vaughan" ~ 43.7942439,
                         Station == "Ossington" ~43.6623565,
                         TRUE ~ lat),
         long = case_when(Station == "St. Patrick" ~ -79.3883485,
                          Station == "Highway 407" ~ -79.5237479,
                          Station == "Vaughan" ~ -79.5274867,
                          Station == "Ossington" ~ -79.4263675,
                          TRUE ~ long))


ggplot()+
   geom_sf(data=torontoMap,
          fill="#777777",
          alpha=0.1,lwd=0.7)+
  geom_sf(data=ttcSubwayShapefiles, mapping=aes(color=ROUTE_NAME),
          lwd=1.4)+
  geom_point(data=
delayTimes %>% mutate(colorLab= ifelse(Station_new %in% (delayTimes %>% 
                             slice_max(avgDelay,n=5) %>% 
                             pull(Station_new)),"Yes","No")),
             mapping=aes(x=long,y=lat,size=avgDelay),
             color=ifelse(delayTimes %>% mutate(colorLab= ifelse(Station_new %in% (delayTimes %>% 
                             slice_max(avgDelay,n=5) %>% 
                             pull(Station_new)),"Yes","No")) %>% 
                             pull(colorLab)=="Yes","red","white"))+
  ggrepel::geom_text_repel(data=delayTimes %>% slice_max(avgDelay,n=5),
             mapping=aes(x=long,y=lat,
                         label= paste0(Station_new,
                                       " Station, \n",round(avgDelay,1)," mins.")),
            vjust=case_when(delayTimes %>% slice_max(avgDelay,n=5) %>% pull(Station_new)=="Jane"~1.3,
                            delayTimes %>% slice_max(avgDelay,n=5) %>% pull(Station_new)=="Dupont"~-2,
                            delayTimes %>% slice_max(avgDelay,n=5) %>% pull(Station_new)=="Runnymede"~ -1,
                            TRUE ~ -1),
            family="TorontoSubwayW01-Regular",
            colour="white")+
  ggtitle("TTC Subway Average Delay Times 2022")+
  dark_theme_minimal()+
  theme(legend.position="right",
        legend.title= element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        text=element_text(family="TorontoSubwayW01-Regular"),
        plot.title=element_text(size=20,
                                hjust = 0.5))+
  scale_color_manual(values=c("#FECA0A",
                              "#00A54F",
                              "#009BDE",
                              "#B30174"))+
  scale_size_continuous(name="Avg. Delay Times (mins.)",range = c(1, 10))
  
