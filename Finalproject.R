library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(forcats)
library(tidyr)
library(emmeans)
library(RColorBrewer)

setwd("C:/Users/dan91/Rstudio/R_class")
raw.data <- read.csv("data/CSD_Tables.csv")

# select the countries needed (Taiwan & Japan)
spawning.jptw <- raw.data %>% 
  filter(Country %in% c('Japan', 'Taiwan')) %>%
  filter(Genus %in% c('Galaxea', 'Lobophyllia', 'Coelastrea', 'Dipsastraea', 'Favites', 'Goniastrea', 'Platygyra', 'Porites'))

# delete the unneeded columns
spawning.jptw[ , c('Subsite', 'O_n', 'Depth_m', 'N', 'No_start', 'Quality_start', 'No_end', 'Quality_end', 'Gamete_release', 'Situation', 'Timezone', 'Reference', 'Comments')] <- list(NULL)
spawning.jptw$color <- ifelse(spawning.jptw$Country == "Japan", "#F8766D", "#00BFC4")

# Mapping
leaflet() %>%
  addTiles() %>%
  setView(lng = 123, lat = 27, zoom = 4.5) %>% # Center the map on Taiwan
  addCircleMarkers(
    lng = spawning.jptw$Longitude,
    lat = spawning.jptw$Latitude,
    popup = spawning.jptw$Site,
    radius = 5,
    fillColor = spawning.jptw$color,
    fillOpacity = 1,
    stroke = F)

genus.tj.day <- ggplot(data = spawning.jptw, aes(x = Genus, y = DoSRtNFM, color = Country)) + 
  geom_boxplot() + 
  ggtitle("Spawning Time Comparison", subtitle = "different genus in Japan and Taiwan") + 
  ylab("DoSRtNFM (Date of Spawning Relative to Nearest Full Moon)") + 
  theme(
    plot.title = element_text(size=20),
    plot.subtitle = element_text(size=12))
genus.tj.day

#check normality----
par(mfrow = c(1, 2))
hist(spawning.jptw$DoSRtNFM)
qqnorm(spawning.jptw$DoSRtNFM, main = 'Normal')
qqline(spawning.jptw$DoSRtNFM)

#check data number----
J.genus <- spawning.jptw %>% 
          filter(Country == "Japan") %>% 
          dplyr::select(Genus)
summary(as.factor(J.genus$Genus))
T.genus <- spawning.jptw %>% 
  filter(Country == "Taiwan") %>% 
  dplyr::select(Genus)
summary(as.factor(T.genus$Genus))

#wilcoxon----
Coelastrea <- spawning.jptw %>%
  filter(Genus %in% "Coelastrea",
         Country %in% c("Taiwan", "Japan"))
wilcox.test(DoSRtNFM ~ Country, data = Coelastrea)

Dipsastraea <- spawning.jptw %>%
  filter(Genus %in% "Dipsastraea",
         Country %in% c("Taiwan", "Japan"))
wilcox.test(DoSRtNFM ~ Country, data = Dipsastraea)

Favites <- spawning.jptw %>%
  filter(Genus %in% "Favites",
         Country %in% c("Taiwan", "Japan"))
wilcox.test(DoSRtNFM ~ Country, data = Favites)

Galaxea <- spawning.jptw %>%
  filter(Genus %in% "Galaxea",
         Country %in% c("Taiwan", "Japan"))
wilcox.test(DoSRtNFM ~ Country, data = Galaxea)

Goniastrea <- spawning.jptw %>%
  filter(Genus %in% "Goniastrea",
         Country %in% c("Taiwan", "Japan"))
wilcox.test(DoSRtNFM ~ Country, data = Goniastrea)

Platygyra <- spawning.jptw %>%
  filter(Genus %in% "Platygyra",
         Country %in% c("Taiwan", "Japan"))
wilcox.test(DoSRtNFM ~ Country, data = Platygyra)

Porites <- spawning.jptw %>%
  filter(Genus %in% "Porites",
         Country %in% c("Taiwan", "Japan"))
wilcox.test(DoSRtNFM ~ Country, data = Porites)

#separate the date----
split.data <- raw.data %>%
  separate(Date, into = c("day", "month", "year"), sep = "/") %>% #先把年月日拆開才能篩選2016
  filter(year == 2016) %>%
  mutate(
    date.x = as.Date(paste(year, month, day, sep = "-"), "%Y-%m-%d"),
    site.y = fct_reorder(Site, Latitude)) %>% # 根據緯度重新排列地點
  group_by(date.x, site.y, Country) %>%  # 處理點點大小：把同天site同country的數據分在同一坨
  summarise(Count = n())# n()功能可以算算同一坨有多少個。接著命名為 Count

split.data2 <- split.data %>%
  left_join(
    raw.data %>%
      dplyr::select(Site, Latitude, Longitude) %>%
      distinct(), by = c("site.y" = "Site"))

i.want.set3 <- brewer.pal(length(unique(split.data$Country)), "Set3")
#set1_colors：指定點點顏色為Set3！
#unique()：獲取該欄位中所有不重複的值，也就是所有不同的國家。
#length()：計算不重複值的數量，就是總共有幾個國家
#brewer.pal(n, "Set3")：從 RColorBrewer 的 Set3 調色板中提取 n 種顏色

ggplot(split.data2, aes(x =date.x, y = site.y, size = Count, color = Country)) +
  geom_point(alpha = 0.4) +  
  scale_size_continuous(name = "Count", range = c(2.5, 8)) +  # 調整點大小
  scale_color_manual(values = i.want.set3) +  
  scale_x_date(date_labels = "%m/%d", date_breaks = "1 month") +
  labs(
    title = "Sampling Distribution by Date and Site in 2016",
    x = "Date (2016)",
    y = "Site (ordered by latitude)",
    color = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size=15.5),
        scale_fill_discrete(name = "Country")) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))

split.data2$Country <- factor(split.data2$Country)

color_palette <- colorFactor(palette = "Set3", domain = levels(split.data2$Country))
leaflet(data = split.data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    popup = ~site.y,
    fillOpacity = 1,
    radius = 6,
    weight = 0.5,
    color = "black",
    fillColor = ~color_palette(Country))




split.data %>% 
  mutate(site.y = fct_reorder(Country, Latitude))
