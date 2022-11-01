# Loading libraries
library(tidyverse)
library("readxl")
library(ggplot2)
library(rgeos)
library(rworldmap)
library(ggrepel)

# Create a sequence of years
years <- c(seq(from = 2000, to = 2020))

# Setting Country list
countries = c('Brazil', 'Portugal', 'Spain', 'UK',
              'Angola', "Nigeria", "Azerbaijan", "Russia", 
              "Saudi Arabia",'Algeria', 'USA', 'Qatar')

# Files
energy <- "./datasets/energy_imports.xlsx"
geo_data <- "./datasets/countries-geo.xlsx"

# Loading Dataset
petrol_raw <- read_excel(energy, range = "C8:L29", na = " ")
gas_raw    <- read_excel(energy, range = "N8:R29", na = " ")
df_geo     <- read_excel(geo_data)
dfbp <- read_xlsx("./datasets/Lig_Brasil_Port.xlsx")

# Adding total per Row 
petrol_raw$total <- rowSums(petrol_raw[, 1:ncol(petrol_raw)])
gas_raw$total <- rowSums(gas_raw[, 1:ncol(gas_raw)])

# Adding year to dataframe
gas_raw <- cbind(years, gas_raw)
petrol_raw <- cbind(years, petrol_raw)

# Inspecting first rowns dataframe
head(petrol_raw)
head(gas_raw)

# Inspecting data structure
str(petrol_raw)
str(gas_raw)

# Imports of petrol_raw
petrol <- petrol_raw %>%
    filter(years >= 2016) %>%
    mutate(
        Angola_pd = sum(Angola) / sum(total) * 100, Arabia_Saudita_pd = sum(`Arábia Saudita`) / sum(total) * 100,
        Argelia_pd = sum(Argélia) / sum(total) * 100, Azerbaijao_pd = sum(Azerbaijão) / sum(total) * 100,
        Brasil_pd = sum(Brasil) / sum(total) * 100, Espanha_pd = sum(Espanha) / sum(total) * 100,
        Nigeria_pd = sum(Nigéria) / sum(total) * 100, Reino_unido_pd = sum(`Reino Unido`) / sum(total) * 100,
        Russia_pd = sum(Rússia) / sum(total) * 100
    ) %>%
    select(years, Angola_pd, Arabia_Saudita_pd, Argelia_pd, Azerbaijao_pd, Brasil_pd, Espanha_pd, Nigeria_pd, Reino_unido_pd, Russia_pd) %>%
    rename(Angola = Angola_pd, "Arábia Saudita" = Arabia_Saudita_pd,
           Argélia = Argelia_pd, Azerbaijão = Azerbaijao_pd, Brasil = Brasil_pd,
           Espanha = Espanha_pd, Nigéria = Nigeria_pd,
           "Reino Unido" = Reino_unido_pd, Rússia = Russia_pd) %>%
    gather(country, percentage, 2:10) %>%
    distinct(country, .keep_all = TRUE) %>%
    select(country, percentage) %>%
    right_join(df_geo, by = "country") %>%
    mutate(
        percentage = as.numeric(gsub(",", "", percentage)),
        lon2 = Long_cap[country == "Portugal"],
        lat2 = Lat_cap[country == "Portugal"] - 0.0005
    ) %>%
    drop_na()

#Imports of Gas_Raw
gas <- gas_raw %>% 
  filter(years >= 2016) %>% 
  mutate( 
          Argelia_pd2 = sum(Argélia) / sum(total) * 100,Catar_pd2= sum(Catar) / sum(total) * 100,
          EUA_pd2 = sum(EUA) / sum(total) * 100, Nigeria_pd2= sum(Nigéria) / sum(total) * 100,
          Russia_pd2 = sum(Rússia) / sum(total) * 100 ) %>% 
  select(years, Argelia_pd2, Catar_pd2, EUA_pd2 , Nigeria_pd2,Russia_pd2 ) %>%
  rename(Argélia=Argelia_pd2, Catar=Catar_pd2,
         EUA=EUA_pd2, Nigéria=Nigeria_pd2,
         Rússia= Russia_pd2) %>% 
  gather (country, percentage , 2:6 ) %>% 
  distinct(country, .keep_all = TRUE) %>% 
  select(country, percentage) %>% 
  right_join(df_geo, by = "country") %>%
  mutate(percentage = as.numeric(gsub(",", "", percentage)),
         lon2 = Long_cap[country == "Portugal"],
         lat2 = Lat_cap[country == "Portugal"]-0.0005) %>% 
  drop_na()

# Rounding percentage values
petrol$percentage <- format(round(petrol$percentage, 1),nsmall=1)
gas$percentage    <- format(round(gas$percentage, 1),nsmall=1)


#first level map
world_coordinates <- map_data("world")

#second level map
map_data <- map_data('world')[map_data('world')$region %in% countries,]

map_data_port <- map_data('world')[map_data('world')$region == countries[2],]

# Plot Petrol

map1<- ggplot() +  geom_map(
  data = world_coordinates, map = world_coordinates,
  aes(long, lat, map_id = region),
  color = NA, fill = '#e8ebea') +
  coord_fixed(xlim = c(-90, 60), ylim = c(-25, 60), expand = FALSE) +
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

#map1

#petroleo e derivados
map2_petro <- map1 + geom_line(data = dfbp, aes(x = long_pb, y = lat_pb), size = 12.9 / 4, color = "orange2")
# map2_petro <- map2_petro + geom_line(data = dfang_p, aes(x = long_pb, y = lat_pb), size = 18.8 / 4, color = "orange2")
# map2_petro <- map2_petro + geom_line(data = dfn_p_petr, aes(x = long_pb, y = lat_pb), size = 5 / 4, color = "orange2")
# map2_petro <- map2_petro + geom_line(data = dfas_p, aes(x = long_pb, y = lat_pb), size = 13.7 / 4, color = "orange2")
# map2_petro <- map2_petro + geom_line(data = dfar_p_petr, aes(x = long_pb, y = lat_pb), size = 8.1 / 4, color = "orange2")
# map2_petro <- map2_petro + geom_line(data = dfaz_p, aes(x = long_pb, y = lat_pb), size = 13.7 / 4, color = "orange2")
# map2_petro <- map2_petro + geom_line(data = dfe_p, aes(x = long_pb, y = lat_pb), size = 18.2 / 4, color = "orange2")
# map2_petro <- map2_petro + geom_line(data = dfru_p, aes(x = long_pb, y = lat_pb), size = 2.3 / 4, color = "orange2")
# map2_petro <- map2_petro + geom_line(data = dfr_p_petr, aes(x = long_pb, y = lat_pb), size = 25.6 / 4, color = "orange2")

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Brasil', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Brasil', ]$country, ":", petrol[petrol$country == 'Brasil', ]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 0.5, nudge_x = 1.25, vjust = 1.25, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Angola', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Angola',]$country, " :", petrol[petrol$country == 'Angola',]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 1.1, nudge_x = 1.25, vjust = -1, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Arábia Saudita', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Arábia Saudita', ]$country, ":", petrol[petrol$country == 'Arábia Saudita', ]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 0.5, nudge_x = 1.25, vjust = 1.25, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Rússia', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Rússia', ]$country, ":"), petrol[petrol$country == 'Rússia', ]$percentage,
    size = 4, color = "black", fontface = "bold", hjust = 0.1,
    nudge_x = 1.25, vjust = -1.25, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Argélia', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Argélia', ]$country, ":", petrol[petrol$country == 'Argélia', ]$percentage),
    size = 4, color = "black", fontface = "bold",
    hjust = 0.0, nudge_x = 1.25, vjust = -1.5, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Nigéria', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Nigéria', ]$country, ":", petrol[petrol$country == 'Nigéria', ]$percentage),
    size = 4, color = "black", fontface = "bold",
    hjust = 0.0, nudge_x = 1.25, vjust = -1.5, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Azerbaijão', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Azerbaijão', ]$country, ":", petrol[petrol$country == 'Azerbaijão', ]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 1.2, nudge_x = 1.25, vjust = -0.7, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Espanha', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Espanha', ]$country, ":", petrol[petrol$country == 'Espanha', ]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 0.1, nudge_x = 1.25, vjust = 0.1, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Reino Unido', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Reino Unido', ]$country, ":", petrol[petrol$country == 'Reino Unido', ]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 0, nudge_x = 1.25, vjust = 0.5, nudge_y = 0
)


map2_petro
