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
geo_plot <- "./datasets/geo-coord.xlsx"

# Loading Dataset
petrol_raw <- read_excel(energy, range = "C8:L29", na = " ")
gas_raw    <- read_excel(energy, range = "N8:R29", na = " ")
df_geo     <- read_excel(geo_data)
geo_plot   <- read_excel(geo_plot)

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
petrol$percentage <- format(round(petrol$percentage, 0),nsmall=0)
gas$percentage    <- format(round(gas$percentage, 0),nsmall=0)


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
  coord_fixed(xlim = c(-92.5, 60), ylim = c(-25, 60), expand = FALSE) +
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
map2_petro <- map1 + geom_line(data = geo_plot[geo_plot$code == "dfbp", ], aes(x = long, y = lat), size = 12.9 / 4, color = "black")
map2_petro <- map2_petro + geom_line(data = geo_plot[geo_plot$code == "dfang_p", ], aes(x = long, y = lat), size = 18.8 / 4, color = "black")
map2_petro <- map2_petro + geom_line(data = geo_plot[geo_plot$code == "dfn_p_petr", ], aes(x = long, y = lat), size = 5 / 4, color = "black")
map2_petro <- map2_petro + geom_line(data = geo_plot[geo_plot$code == "dfas_p", ], aes(x = long, y = lat), size = 13.7 / 4, color = "black")
map2_petro <- map2_petro + geom_line(data = geo_plot[geo_plot$code == "dfar_p_petr", ], aes(x = long, y = lat), size = 8.1 / 4, color = "black")
map2_petro <- map2_petro + geom_line(data = geo_plot[geo_plot$code == "dfaz_p", ], aes(x = long, y = lat), size = 13.7 / 4, color = "black")
map2_petro <- map2_petro + geom_line(data = geo_plot[geo_plot$code == "dfe_p", ], aes(x = long, y = lat), size = 18.2 / 4, color = "black")
map2_petro <- map2_petro + geom_line(data = geo_plot[geo_plot$code == "dfru_p", ], aes(x = long, y = lat), size = 2.3 / 4, color = "black")
map2_petro <- map2_petro + geom_line(data = geo_plot[geo_plot$code == "dfr_p_petr", ], aes(x = long, y = lat), size = 25.6 / 4, color = "black")


map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == "Angola", ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == "Angola", ]$country, petrol[petrol$country == "Angola", ]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 1.1, nudge_x = 1.25, vjust = -1, nudge_y = 0
)
map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Arábia Saudita', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Arábia Saudita', ]$country, petrol[petrol$country == 'Arábia Saudita', ]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 0.5, nudge_x = 1.25, vjust = 1.25, nudge_y = 0
)
map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Rússia', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Rússia', ]$country),
    size = 4, color = "black", fontface = "bold",
    hjust = 0.1, nudge_x = 1.25, vjust = -1.25, nudge_y = 0
)
map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Rússia', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Rússia', ]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 0.1, nudge_x = 1.25, vjust = 0.25, nudge_y = 0
)
map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Brasil', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Brasil', ]$country, petrol[petrol$country == 'Brasil', ]$percentage, "%"),
    size = 4, color = "black",
    fontface = "bold", hjust = 0.5, nudge_x = 1.25,
    vjust = 1.25, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Argélia', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Argélia', ]$country), size = 4, color = "black",
    fontface = "bold", hjust = 0.0, nudge_x = 1.25,
    vjust = -1.5, nudge_y = 0
)
map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Argélia', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Argélia', ]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 0.1, nudge_x = 1.25, vjust = 2.0, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Nigéria', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Nigéria', ]$country),
    size = 4, color = "black", fontface = "bold",
    hjust = 0.0, nudge_x = 1.25, vjust = -1.5, nudge_y = 0
)
map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Nigéria', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Nigéria', ]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 0.1, nudge_x = 1.25, vjust = 2.5, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Azerbaijão', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Azerbaijão', ]$country, petrol[petrol$country == 'Azerbaijão', ]$percentage, "%"),
     size = 4, color = "black", fontface = "bold",
     hjust = 1.2, nudge_x = 1.25, vjust =-0.7, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Espanha', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Espanha', ]$country, petrol[petrol$country == 'Espanha', ]$percentage, "%"),
    size = 4, color = "black",fontface = "bold",
    hjust = 0.1, nudge_x = 1.25 , vjust =0.1, nudge_y = 0
)

map2_petro <- map2_petro + geom_text(
    data = petrol[petrol$country == 'Reino Unido', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(petrol[petrol$country == 'Reino Unido', ]$country, petrol[petrol$country == 'Reino Unido', ]$percentage, "%"),
    size = 4, color = "black", fontface = "bold",
    hjust = 0, nudge_x = 1.25, vjust =0.5, nudge_y = 0
)


#map2_petro

#map gas and derivates
map2_gas <- map2_petro + geom_line(data = geo_plot[geo_plot$code == "dfn_p_gas", ], aes(x = long, y = lat), size = 63.9 / 4, color = "cyan3")
map2_gas <- map2_gas + geom_line(data = geo_plot[geo_plot$code == "dfar_p_gas", ], aes(x = long, y = lat), size = 38.6 / 4, color = "cyan3")
map2_gas <- map2_gas + geom_line(data = geo_plot[geo_plot$code == "dfr_p_gas", ], aes(x = long, y = lat), size = 3.6 / 4, color = "cyan3")
map2_gas <- map2_gas + geom_line(data = geo_plot[geo_plot$code == "dfusa_p", ], aes(x = long, y = lat), size = 19.1 / 4, color = "cyan3")
map2_gas <- map2_gas + geom_line(data = geo_plot[geo_plot$code == "dfqat_p", ], aes(x = long, y = lat), size = 13.3 / 4, color = "cyan3")


map2_gas <- map2_gas + geom_text(
    data = gas[gas$country == 'Rússia', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(gas[gas$country == 'Rússia', ]$percentage, "%"),
    size = 4, color = "cyan4", fontface = "bold",
    hjust = 0.25, nudge_x = 1.25, vjust = 1.75, nudge_y = 0
)

map2_gas <- map2_gas + geom_text(
    data = gas[gas$country == 'Catar', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(gas[gas$country == 'Catar', ]$country),
    size = 4, color = "black", fontface = "bold",
    hjust = 1.1, nudge_x = 1.25, vjust = 1.35, nudge_y = 0
)
map2_gas <- map2_gas + geom_text(
    data = gas[gas$country == 'Catar', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(gas[gas$country == 'Catar', ]$percentage, "%"),
    size = 4, color = "cyan4", fontface = "bold",
    hjust = 0.0, nudge_x = 1.2, vjust = 1.35, nudge_y = 0
)

map2_gas <- map2_gas + geom_text(
    data = gas[gas$country == 'Argélia', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(gas[gas$country == 'Argélia', ]$percentage, "%"),
    size = 4, color = "cyan4", fontface = "bold",
    hjust = 0.0, nudge_x = 1.25, vjust = 0.25, nudge_y = 0
)

map2_gas <- map2_gas + geom_text(
    data = gas[gas$country == 'Nigéria', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(gas[gas$country == 'Nigéria', ]$percentage, "%"),
    size = 4, color = "cyan4", fontface = "bold",
    hjust = 0.0, nudge_x = 1.25, vjust = 0.5, nudge_y = 0
)

map2_gas <- map2_gas + geom_text(
    data = gas[gas$country == 'EUA', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(gas[gas$country == 'EUA', ]$country),
    size = 4, color = "black", fontface = "bold",
    hjust = 2.60, nudge_x = 1.25, vjust = 0.5, nudge_y = 0
)
map2_gas <- map2_gas + geom_text(
    data = gas[gas$country == 'EUA', ], aes(x = Long_cap, y = Lat_cap),
    label = paste(gas[gas$country == 'EUA', ]$percentage, "%"),
    size = 4, color = "cyan4", fontface = "bold",
    hjust = 1.4, nudge_x = 1.25, vjust = 0.5, nudge_y = 0
)


#map2_gas

# third layer: Country map
map3 <- map2_gas + geom_polygon(
    data = map_data_port,
    aes(x = long, y = lat, group = group),
    color = NA, fill = "grey19"
) +
    theme(
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(), # remove x axis labels
        axis.ticks.x = element_blank(), # remove x axis ticks
        axis.text.y = element_blank(), # remove y axis labels
        axis.ticks.y = element_blank()
    )

#map3

#title, subtitle and caption format
map4 <- map3 + geom_text(aes(
    x = -53, y = 55,
    label = "DE ONDE VEM O GÁS E O PETRÓLEO?"
), size = 9, color = "black", fontface = "bold", family = "sans") +
    geom_text(aes(
        x = -59, y = 50,
        label = "Média das principais importações entre 2016 e 2020"
    ), size = 6, color = "black", family = "sans") +
    geom_text(aes(
        x = -59, y = -22,
        label = "Fonte: Pordata        *Percentagens sobre média do valor total de importações"
    ), size = 4, color = "black", family = "sans") +
    geom_text(aes(
        x = 27, y = -22,
        label = paste("Autores: Joaquim Barbosa, Rhaydrick Tavares e Vânia Ribeiro. Data:", Sys.Date())
    ), size = 4, color = "black", family = "sans") +
    geom_line(aes(x = -85.5:54.8, y = -20), size = 0.5, color = "black") +
    theme(plot.margin = margin(t = 0, r = 0, l = 0, b = 0))


# legenda
xgas <- c(41, 45)
ygas <- c(-6, -6)
xlabel <- c(45)
ylabel <- c(-6)
label_gas <- c("Gás")

xpetr <- c(41, 45)
ypetr <- c(-9, -9)

xlabel2 <- c(45)
ylabel2 <- c(-9)
label_petr <- c("Petróleo")

map5 <- map4+geom_line (aes(x=xgas,y=ygas), size=6.5, color ="cyan3")
map5 <- map5+geom_line (aes(x=xpetr,y=ypetr), size=6.5, color ="black")

map5<-map5+geom_text(aes(xlabel,ylabel), 
                     label = paste(label_gas), size = 6.5, color = "black",
                     fontface = "bold", hjust = 0.0, nudge_x = 1.25,
                     vjust =0.4, nudge_y = 0)

map5<-map5+geom_text(aes(xlabel2,ylabel2), 
                     label = paste(label_petr), size = 6.5, color = "black",
                     fontface = "bold", hjust = 0.0, nudge_x = 1.25,
                     vjust =0.4, nudge_y = 0)

map5                               


ggsave(filename = "./img/map_fnal.png", plot = map5, width = 40.3, height = 25, units = "cm", dpi = 600)
