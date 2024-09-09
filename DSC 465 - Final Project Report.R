# Load libraries
library(ggplot2)
library(dplyr)
library(maps)

# Load dataset
earthquake <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Final Project\\Data File\\earthquake_1995-2023.csv")
earthquake <- na.omit(earthquake)
head(earthquake)

# 2D binning plot displaying a relationship between magnitude and significance
ggplot(earthquake,
       aes(x = magnitude, y = sig)) +
  geom_hex(binwidth = c(0.1, 150)) + 
  stat_smooth(method = "lm",
              se = TRUE,
              color = "#27AD81FF") +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Relationship Between Magnitude and Significance",
       x = "Magnitude",
       y = "Significance") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

# Bar plot displaying the magnitude ratio of earthquakes by continent
earthquake_continent_magnitude <- earthquake %>%
  select(continent, magnitude) %>%
  mutate(magnitude_bin = cut(magnitude,
                             breaks = c(6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5), 
                             right = FALSE, 
                             labels = c("6.5-6.9", "7.0-7.4", "7.5-7.9", "8.0-8.4", "8.5-8.9", "9.0-9.5")))

earthquake_continent_magnitude_percentage <- earthquake_continent_magnitude %>%
  group_by(continent, magnitude_bin) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),
         percentage = (count / total) * 100) %>%
  ungroup()

order_continent <- earthquake_continent_magnitude_percentage %>%
  filter(magnitude_bin == "6.5-6.9") %>%
  arrange(desc(percentage)) %>%
  pull(continent)

earthquake_continent_magnitude_percentage <- earthquake_continent_magnitude_percentage %>%
  mutate(continent = factor(continent, levels = order_continent),
         magnitude_bin = factor(magnitude_bin, levels = rev(levels(magnitude_bin))))

ggplot(earthquake_continent_magnitude_percentage,
       aes(x = continent, y = percentage, fill = magnitude_bin)) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_manual(values = c('#F26419', '#F6AE2D', '#F9F871', '#86BBD8', '#33658A', '#2F4858')) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "The Magnitude Ratio of Earthquakes by Continent",
       x = "Continent",
       y = "Percentage",
       fill = "Magnitude") +
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

# Mosaic plot displaying the number of earthquakes per year by continent
earthquake$date_time2 <- as.POSIXct(earthquake$date_time,
                                    format = "%d-%m-%Y %H:%M")
earthquake$year <- format(earthquake$date_time2, "%Y")

earthquake_table <- table(earthquake$year,
                          earthquake$continent)

mosaicplot(earthquake_table, 
           main = "The Number of Earthquakes per Year by Continent", 
           las = 1, 
           cex = 0.6,
           color = c('#2F4858', '#33658A', '#86BBD8', '#8D99AE', '#F9F871', '#F6AE2D','#F26419'))

# Scatter plot overlaying earthquakes on a world map
top_10_earthquakes <- earthquake[order(earthquake$magnitude, decreasing = TRUE), ][1:10, ]

world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill = "gray",
               color = "white") +
  geom_point(data = earthquake,
             aes(x = longitude, y = latitude, size = magnitude, color = as.factor(tsunami)),
             alpha = 0.5) +
  geom_point(data = top_10_earthquakes,
             aes(x = longitude, y = latitude, size = magnitude, color = "Top 10 Earthquakes"),
             alpha = 0.8) +
  scale_color_manual(values = c("0" = "#33658A", "1" = "#FEC601", "Top 10 Earthquakes" = "#F26419"),
                     name = "Legend",
                     labels = c("0" = "No Tsunami", "1" = "Tsunami Occurred", "Top 10 Earthquakes" = "Top 10 Strongest Earthquakes")) +
  scale_size_continuous(range = c(2, 5),
                        name = "Magnitude") +
  labs(title = "Significant Earthquakes since 1995 to 2023",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))