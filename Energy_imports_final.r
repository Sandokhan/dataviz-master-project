library(tidyverse)
library("readxl")
library(ggplot2)
library(rgeos)
library(rworldmap)
library(ggrepel)

years <- c(2000,	2001,	2002,	2003,	2004,	2005,	2006,	2007,
           2008,	2009,	2010,	2011,	2012,	2013, 2014,	2015,
           2016,	2017,	2018,	2019,	2020)

filename <- 'energy_imports.xlsx'
dfbp <- read_xlsx("Lig_Brasil_Port.xlsx")
dfang_p <- read_xlsx("Lig_Ang_Port.xlsx")
dfas_p <- read_xlsx("Lig_Ar_sau_Port.xlsx")
dfar_p_gas <- read_xlsx("Lig_Arg_Port_gas.xlsx")
dfar_p_petr <- read_xlsx("Lig_Arg_Port_petr.xlsx")
dfaz_p <- read_xlsx("Lig_Azer_Port.xlsx")
dfe_p <- read_xlsx("Lig_EspPort.xlsx")
dfn_p_gas <- read_xlsx("Lig_Nig_Port_gas.xlsx")
dfn_p_petr <- read_xlsx("Lig_Nig_Port_petr.xlsx")
dfru_p <- read_xlsx("Lig_Reino_Uni_Port.xlsx")
dfr_p_gas<- read_xlsx("Lig_Rus_Port_gas.xlsx")
dfr_p_petr<- read_xlsx("Lig_Rus_Port_petr.xlsx")
dfusa_p<- read_xlsx("Lig_USA_Port.xlsx")
dfqat_p<- read_xlsx("Lig_Qatar_Port.xlsx")

ds_city<-read_xlsx("Paises_coord.xlsx")

#Reading Sources
petrol <- read_excel(filename, range = "C8:L29", na = " ")
gas <- read_excel(filename, range = "N8:R29", na = " ")

petrol$total <- rowSums(petrol[,2:ncol(petrol)])
gas$total <- rowSums(gas[,2:ncol(gas)])

# Adding Years to dataset
gas <- cbind(years, gas)
petrol <- cbind(years, petrol)

# Inspecting dataframe
head(petrol)
head(gas)

# Inspecting data structure
str(petrol)
str(gas)

# # Get coordinates
# # get world map
# #wmap <- getMap(resolution="high")
# # get centroids
# #centroids <- gCentroid(wmap, byid=TRUE)
# # get a data.frame with centroids
# #df <- as.data.frame(centroids)
# country <- c('Angola','Russia','Algeria','Spain', 'Saudi Arabia', 'United Kingdom', 'Brazil', 'Nigeria', 'Portugal','Azerbaijan', 'Qatar', 'United States of America')
# df1<- df[c('Angola','Russia','Algeria','Spain', 'Saudi Arabia', 'United Kingdom', 'Brazil', 'Nigeria', 'Portugal','Azerbaijan', 'Qatar', 'United States of America'),] %>% 
#   rename(Long_cap=x, Lat_cap=y)
# df1<- cbind(country, df1)
# df1['country'][df1['country'] == 'Algeria'] <- 'Argélia'
# df1['country'][df1['country'] == 'Spain'] <- 'Espanha'
# df1['country'][df1['country'] == 'Saudi Arabia'] <- 'Arábia Saudita'
# df1['country'][df1['country'] == 'United Kingdom'] <- 'Reino Unido'
# df1['country'][df1['country'] == 'Brazil'] <- 'Brasil'
# df1['country'][df1['country'] == 'Nigeria'] <- 'Nigéria'
# df1['country'][df1['country'] == 'Azerbaijan'] <- 'Azerbaijão'
# df1['country'][df1['country'] == 'Qatar'] <- 'Catar'
# df1['country'][df1['country'] == 'United States of America'] <- 'EUA'
# df1


#Imports of Petróleo
petrol2 <- petrol %>% 
  filter(years >= 2016) %>% 
  mutate (Angola_pd = sum(Angola) / sum(total) * 100, Arabia_Saudita_pd = sum(`Arábia Saudita`) / sum(total) * 100, 
          Argelia_pd = sum(Argélia) / sum(total) * 100,Azerbaijao_pd= sum(Azerbaijão) / sum(total) * 100,Brasil_pd = sum(Brasil) / sum(total) * 100, Espanha_pd = sum(Espanha) / sum(total) * 100,
          Nigeria_pd= sum(Nigéria) / sum(total) * 100,Reino_unido_pd = sum(`Reino Unido`) / sum(total) * 100,Russia_pd = sum(Rússia) / sum(total) * 100 )%>% 
  select(years, Angola_pd ,Arabia_Saudita_pd, Argelia_pd, Azerbaijao_pd, Brasil_pd , Espanha_pd , Nigeria_pd,Reino_unido_pd, Russia_pd ) %>%
  rename(Angola=Angola_pd, "Arábia Saudita"=Arabia_Saudita_pd, Argélia=Argelia_pd, Azerbaijão=Azerbaijao_pd, Brasil=Brasil_pd, Espanha=Espanha_pd, Nigéria=Nigeria_pd, "Reino Unido"=Reino_unido_pd,Rússia= Russia_pd) %>% 
  gather (country, percentage , 2:10 ) %>% 
  distinct(country, .keep_all = TRUE) %>% 
  select(country, percentage) %>% 
  right_join(ds_city, by="country") %>% 
  mutate(percentage = as.numeric(gsub(",", "", percentage)),
         lon2 = Long_cap[country == "Portugal"],
         lat2 = Lat_cap[country == "Portugal"]-0.0005) %>% 
  drop_na()

petrol2$percentage<-format(round(petrol2$percentage, 1),nsmall=1)

#Imports of Gas
gas2 <- gas %>% 
  filter(years >= 2016) %>% 
  mutate ( Argelia_pd2 = sum(Argélia) / sum(total) * 100,Catar_pd2= sum(Catar) / sum(total) * 100,EUA_pd2 = sum(EUA) / sum(total) * 100, 
           Nigeria_pd2= sum(Nigéria) / sum(total) * 100,Russia_pd2 = sum(Rússia) / sum(total) * 100 )%>% 
  select(years, Argelia_pd2, Catar_pd2, EUA_pd2 , Nigeria_pd2,Russia_pd2 ) %>%
  rename(Argélia=Argelia_pd2, Catar=Catar_pd2, EUA=EUA_pd2, Nigéria=Nigeria_pd2, Rússia= Russia_pd2) %>% 
  gather (country, percentage , 2:6 ) %>% 
  distinct(country, .keep_all = TRUE) %>% 
  select(country, percentage) %>% 
  right_join(ds_city, by="country") %>% 
  mutate(percentage = as.numeric(gsub(",", "", percentage)),
         lon2 = Long_cap[country == "Portugal"],
         lat2 = Lat_cap[country == "Portugal"]-0.0005)%>% 
  drop_na()

gas2$percentage<-format(round(gas2$percentage, 1),nsmall=1)

countries = c('Brazil', 'Portugal', 'Spain', 'UK',
              'Angola', "Nigeria", "Azerbaijan", "Russia", 
              "Saudi Arabia",'Algeria', 'USA', 'Qatar')

countries_port = c('Portugal')

#first level map
world_coordinates <- map_data("world")

#second level map
map_data <- map_data('world')[map_data('world')$region %in% countries,]
#View(map_data)

map_data_port <- map_data('world')[map_data('world')$region %in% countries_port,]

#grupo petroleo

countries_an_as<-c('Angola')
ds_pd_final3<- petrol2%>% 
  filter(country %in% countries_an_as)

countries_an_as<-c('Arábia Saudita')
ds_pd_final4<- petrol2%>% 
  filter(country %in% countries_an_as)

countries_an_as<-c('Rússia')
ds_pd_final5<- petrol2%>% 
  filter(country %in% countries_an_as)

countries_br<-c('Brasil')
ds_pd_final6<- petrol2%>% 
  filter(country %in% countries_br)

countries_ar<-c('Argélia')
ds_pd_final7<- petrol2%>% 
  filter(country %in% countries_ar)

countries_ni<-c('Nigéria')
ds_pd_final8<- petrol2%>% 
  filter(country %in% countries_ni)

countries_az<-c('Azerbaijão')
ds_pd_final9<- petrol2%>% 
  filter(country %in% countries_az)

countries_es<-c('Espanha')
ds_pd_final10<- petrol2%>% 
  filter(country %in% countries_es)

countries_ru<-c('Reino Unido')
ds_pd_final11<- petrol2%>% 
  filter(country %in% countries_ru)

#grup gas

#grupo 
countries_r<-c('Rússia')
ds_pd_final12<- gas2%>% 
  filter(country %in% countries_r)

countries_qa<-c('Catar')
ds_pd_final13<- gas2%>% 
  filter(country %in% countries_qa)

countries_ar<-c('Argélia')
ds_pd_final14<- gas2%>% 
  filter(country %in% countries_ar)

countries_ni<-c('Nigéria')
ds_pd_final15<- gas2%>% 
  filter(country %in% countries_ni)

countries_usa<-c('EUA')
ds_pd_final16<- gas2%>% 
  filter(country %in% countries_usa)


map1<- ggplot() +  geom_map(
  data = world_coordinates, map = world_coordinates,
  aes(long, lat, map_id = region),
  color = NA, fill = '#e8ebea'
)+
  coord_fixed(xlim = c(-90, 60), ylim = c(-25, 60), expand = FALSE)+
  
  # Second layer: Country map
  geom_polygon(data = map_data,
               aes(x=long, y=lat, group = group),
               color = NA, fill = '#b5b8b7') +
  labs(x = "", y="")+
  theme(panel.border=element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank())

map1

#petroleo e derivados
map2_petro <- map1+geom_line (data=dfbp, aes(x=long_pb,y=lat_pb), size=12.9/4, color ="orange2")
map2_petro <- map2_petro+geom_line (data=dfang_p, aes(x=long_pb,y=lat_pb), size=18.8/4, color ="orange2")
map2_petro<- map2_petro+geom_line (data=dfn_p_petr, aes(x=long_pb,y=lat_pb), size=5/4, color ="orange2")
map2_petro <- map2_petro+geom_line (data=dfas_p, aes(x=long_pb,y=lat_pb), size=13.7/4, color ="orange2")
map2_petro <- map2_petro+geom_line (data=dfar_p_petr, aes(x=long_pb,y=lat_pb), size=8.1/4, color ="orange2")
map2_petro<- map2_petro+geom_line (data=dfaz_p, aes(x=long_pb,y=lat_pb), size=13.7/4, color ="orange2")
map2_petro <- map2_petro+geom_line (data=dfe_p, aes(x=long_pb,y=lat_pb), size=18.2/4, color ="orange2")
map2_petro<- map2_petro+geom_line (data=dfru_p, aes(x=long_pb,y=lat_pb), size=2.3/4, color ="orange2")
map2_petro<- map2_petro+geom_line (data=dfr_p_petr, aes(x=long_pb,y=lat_pb), size=25.6/4, color ="orange2")



map2_petro<-map2_petro+geom_text(data = ds_pd_final3, aes(x = Long_cap, y = Lat_cap), 
                                 label = paste (ds_pd_final3$country,":", ds_pd_final3$percentage,'%' ), size = 4, color = "black",fontface = "bold", hjust = 1.1, nudge_x = 1.25, vjust =-1, nudge_y = 0)

map2_petro<-map2_petro+geom_text(data = ds_pd_final4, aes(x = Long_cap, y = Lat_cap), 
                                 label = paste (ds_pd_final4$country,":", ds_pd_final4$percentage,'%' ), size = 4, color = "black",fontface = "bold",hjust = 0.5, nudge_x = 1.25, vjust =1.25, nudge_y = 0)

map2_petro<-map2_petro+geom_text(data = ds_pd_final5, aes(x = Long_cap, y = Lat_cap), 
                                 label = paste (ds_pd_final5$country,":"), size = 4, color = "black",fontface = "bold", hjust = 0.1, nudge_x = 1.25, vjust =-1.25, nudge_y = 0)
map2_petro<-map2_petro+geom_text(data = ds_pd_final5, aes(x = Long_cap, y = Lat_cap), 
                                 label = paste (ds_pd_final5$percentage,'%' ), size = 4, color = "black",fontface = "bold", hjust = 0.1, nudge_x = 1.25, vjust =0.25, nudge_y = 0)

map2_petro<-map2_petro+geom_text(data = ds_pd_final6, aes(x = Long_cap, y = Lat_cap), 
                                 label = paste (ds_pd_final6$country,":", ds_pd_final6$percentage,'%' ), size = 4, color = "black",fontface = "bold", hjust = 0.5, nudge_x = 1.25, vjust =1.25, nudge_y = 0)

map2_petro<-map2_petro+geom_text(data = ds_pd_final7, aes(x = Long_cap, y = Lat_cap), 
                                 label = paste (ds_pd_final7$country,":"), size = 4, color = "black",fontface = "bold", hjust = 0.0, nudge_x = 1.25, vjust =-1.5, nudge_y = 0)
map2_petro<-map2_petro+geom_text(data = ds_pd_final7, aes(x = Long_cap, y = Lat_cap), 
                                 label = paste (ds_pd_final7$percentage,'%' ), size = 4, color = "black",fontface = "bold", hjust = 0.1, nudge_x = 1.25, vjust =2.0, nudge_y = 0)

map2_petro<-map2_petro+geom_text(data = ds_pd_final8, aes(x = Long_cap, y = Lat_cap), 
                                 label = paste (ds_pd_final8$country,":"), size = 4, color = "black",fontface = "bold", hjust = 0.0, nudge_x = 1.25, vjust =-1.5, nudge_y = 0)
map2_petro<-map2_petro+geom_text(data = ds_pd_final8, aes(x = Long_cap, y = Lat_cap), 
                                 label = paste (ds_pd_final8$percentage,'%' ), size = 4, color = "black",fontface = "bold", hjust = 0.1, nudge_x = 1.25, vjust =2.5, nudge_y = 0)

map2_petro<-map2_petro+geom_text(data = ds_pd_final9, aes(x = Long_cap, y = Lat_cap), 
                                 label = paste (ds_pd_final9$country,":", ds_pd_final9$percentage,'%' ), size = 4, color = "black", fontface = "bold",hjust = 1.2, nudge_x = 1.25, vjust =-0.7, nudge_y = 0)

map2_petro<-map2_petro+geom_text(data = ds_pd_final10, aes(x = Long_cap, y = Lat_cap), 
                                 label = paste (ds_pd_final10$country,":", ds_pd_final10$percentage,'%' ), size = 4, color = "black",fontface = "bold", hjust = 0.1, nudge_x = 1.25 , vjust =0.1, nudge_y = 0)

map2_petro<-map2_petro+geom_text(data = ds_pd_final11, aes(x = Long_cap, y = Lat_cap),
                                 label = paste (ds_pd_final11$country,":", ds_pd_final11$percentage,'%' ), size = 4, color = "black", fontface = "bold",hjust = 0, nudge_x = 1.25, vjust =0.5, nudge_y = 0)


map2_petro


#map gas and derivates
map2_gas <- map2_petro+geom_line (data=dfn_p_gas, aes(x=long_pb,y=lat_pb), size=63.9/4, color ="deepskyblue")
map2_gas <- map2_gas+geom_line (data=dfar_p_gas, aes(x=long_pb,y=lat_pb), size=38.6/4, color ="deepskyblue")
map2_gas <- map2_gas+geom_line (data=dfr_p_gas, aes(x=long_pb,y=lat_pb), size=3.6/4, color ="deepskyblue")
map2_gas <- map2_gas+geom_line (data=dfusa_p, aes(x=long_pb,y=lat_pb), size=19.1/4, color ="deepskyblue")
map2_gas <- map2_gas+geom_line (data=dfqat_p, aes(x=long_pb,y=lat_pb), size=13.3/4, color ="deepskyblue")

map2_gas<-map2_gas+geom_text(data = ds_pd_final12, aes(x = Long_cap, y = Lat_cap), 
                             label = paste (ds_pd_final12$percentage,'%' ), size = 4, color = "black", fontface = "bold", hjust = 0.25, nudge_x = 1.25, vjust =1.75, nudge_y = 0)

map2_gas<-map2_gas+geom_text(data = ds_pd_final13, aes(x = Long_cap, y = Lat_cap), 
                             label = paste (ds_pd_final13$country,":", ds_pd_final13$percentage,'%' ), size = 4, color = "black",fontface = "bold", hjust = 0.6, nudge_x = 1.25, vjust =1.35, nudge_y = 0)

map2_gas<-map2_gas+geom_text(data = ds_pd_final14, aes(x = Long_cap, y = Lat_cap), 
                             label = paste (ds_pd_final14$percentage,'%' ), size = 4, color = "black", fontface = "bold",hjust = 0.0, nudge_x = 1.25, vjust =0.25, nudge_y = 0)

map2_gas<-map2_gas+geom_text(data = ds_pd_final15, aes(x = Long_cap, y = Lat_cap), 
                             label = paste (ds_pd_final15$percentage,'%' ), size = 4, color = "black",fontface = "bold", hjust = 0.0, nudge_x = 1.25, vjust =0.5, nudge_y = 0)

map2_gas<-map2_gas+geom_text(data = ds_pd_final16, aes(x = Long_cap, y = Lat_cap), 
                             label = paste (ds_pd_final16$country,":", ds_pd_final16$percentage,'%' ), size = 4, color = "black",fontface = "bold", hjust = 1.3, nudge_x = 1.25, vjust =0.5, nudge_y = 0)



map2_gas


#Map gas



# third layer: Country map
map3<-map2_gas+geom_polygon(data = map_data_port,
                            aes(x=long, y=lat, group = group),
                            color = NA, fill = 'orange3') +
  theme(panel.border=element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank())

map3

map4<- map3 + labs(title = "De onde vem o Petróleo e o Gás", fontface = "bold" , subtitle = "Média das principais importações nos últimos 5 anos (2016-2020)",
                   caption = "Fontes: DGEG/Pordata        *Percentagens sobre média do valor total de importações                                  Autores: Joaquim Barbosa, Rhaydrick Tavares e Vânia Ribeiro")

map4


ggsave( filename = "map_fnal.png", plot = map4 , width = 40, height = 25, units = "cm", dpi = 600)
