library(dplyr)
library(terra)
library(rgbif)
library(sp)
library(leaflet)
library(ggplot2)

raw.data <- read.csv("data/CSD_Tables.csv")

# select the countries needed 
spawning.country <- raw.data %>% 
  filter(Country %in% c('Indonesia', 'Japan', 'Philippines', 'Taiwan', 'Australia', 'Fiji')) %>%
  filter(Genus %in% c('Diploastrea', 'Galaxea', 'Lobophyllia', 'Coelastrea', 'Dipsastraea', 'Favites', 
                        'Goniastrea', 'Leptoria', 'Merulina', 'Pectinia', 'Platygyra', 'Porites'))
  
# delete the unneeded columns
spawning.country[ , c('Subsite', 'O_n', 'Depth_m', 'N', 'No_start', 'Quality_start', 'No_end', 'Quality_end', 
                      'Gamete_release', 'Situation', 'Timezone', 'Reference', 'Comments')] <- list(NULL)
spawning.country$color <- ifelse(spawning.country$Country == "Japan", " #ee2c2c",
                                ifelse(spawning.country$Country == "Taiwan", "dodgerblue",
                                       ifelse(spawning.country$Country == "Philippines","forestgreen", "gold")))

# Mapping
leaflet() %>%
  addTiles() %>%
  setView(lng = 121.0, lat = 20, zoom = 3.5) %>% # Center the map on Taiwan
  addCircleMarkers(
    lng = spawning.country$Longitude,
    lat = spawning.country$Latitude,
    popup = spawning.country$Site,
    radius = 2,
    fillOpacity = 0.6,
    color = spawning.country$color)
----#trials----
# Genus vs. DoSRtNFM
genus.day <- ggplot(data = spawning.country,
                    aes(x = Genus, y = DoSRtNFM))+
  geom_boxplot()
genus.day

#Country vs. DoSRtNFM
Country.day <- ggplot(data = spawning.country,
                    aes(x = Country, y = DoSRtNFM))+
  geom_boxplot()
Country.day

------#twjp--------

spawning.tw.jp <- raw.data %>% 
  filter(Country %in% c('Japan', 'Taiwan')) %>%
  filter(Genus %in% c('Galaxea', 'Lobophyllia', 'Coelastrea', 'Dipsastraea', 'Favites', 
                      'Goniastrea', 'Platygyra', 'Porites'))
spawning.tw.jp[ , c('Subsite', 'O_n', 'Depth_m', 'N', 'No_start', 'Quality_start', 'No_end', 'Quality_end', 
                      'Gamete_release', 'Situation', 'Timezone', 'Reference', 'Comments')] <- list(NULL)

genus.tj.day <- ggplot(data = spawning.tw.jp,
                    aes(x = Genus, y = DoSRtNFM, color = Country))+
  geom_boxplot()
genus.tj.day

----#anova&tukey
spawning.aov <- aov(DoSRtNFM ~ Genus + Genus %in% Country, data = spawning.tw.jp)
summary(spawning.aov)

TukeyHSD(spawning.aov)

----#filter----
Coelastrea <- spawning.tw.jp %>%
  filter(Genus %in% "Coelastrea",
         Country %in% c("Taiwan", "Japan"))
Coelastrea.aov <- aov(DoSRtNFM ~ Country, data = Coelastrea)
summary(Coelastrea.aov)

Dipsastraea <- spawning.tw.jp %>%
  filter(Genus %in% "Dipsastraea",
         Country %in% c("Taiwan", "Japan"))
Dipsastraea.aov <- aov(DoSRtNFM ~ Country, data = Dipsastraea)
summary(Dipsastraea.aov)

----# with fiji----
spawning.tw.jp.f <- raw.data %>% 
  filter(Country %in% c('Japan', 'Taiwan', 'Fiji')) %>%
  filter(Genus %in% c('Diploastrea', 'Galaxea', 'Lobophyllia', 'Coelastrea', 'Dipsastraea', 'Favites', 
                      'Goniastrea', 'Leptoria', 'Merulina', 'Pectinia', 'Platygyra', 'Porites'))
spawning.tw.jp.f[ , c('Subsite', 'O_n', 'Depth_m', 'N', 'No_start', 'Quality_start', 'No_end', 'Quality_end', 
                    'Gamete_release', 'Situation', 'Timezone', 'Reference', 'Comments')] <- list(NULL)
genus.tjf.day <- ggplot(data = spawning.tw.jp.f,
                       aes(x = Genus, y = DoSRtNFM, color = Country))+
  geom_boxplot()
genus.tjf.day


