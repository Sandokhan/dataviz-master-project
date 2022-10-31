# Loading libraries
library(tidyverse)
library(ggplot2)
library(rgeos)
library(rworldmap)
library(ggrepel)

# Create a sequence of years
years <- c(seq(from = 2000, to = 2020))

# Files
energy <- "./datasets/energy_imports.xlsx"
geo_data <- "./datasets/countries-geo.xlsx"

# Loading Dataset
petrol_raw <- read_excel(energy, range = "C8:L29", na = " ")
gas_raw_raw    <- read_excel(energy, range = "N8:R29", na = " ")
df_geo     <- read_excel(geo_data)

# Adding total per Row 
petrol_raw$total <- rowSums(petrol_raw[, 2:ncol(petrol_raw)])
gas_raw$total <- rowSums(gas_raw[, 2:ncol(gas_raw)])

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
