# Load libraries
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)

# Load dataset
earthquake <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Final Project\\Data File\\earthquake_1995-2023.csv")
head(earthquake)

# Scatter plot of overlaying top earthquakes on a world map
top_earthquakes <- earthquake %>%
  arrange(desc(magnitude)) %>%
  head(50)

world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "white") +
  geom_point(data = top_earthquakes, aes(x = longitude, y = latitude, size = magnitude), color = "red", alpha = 0.5) +
  scale_size_continuous(range = c(1, 5), name = "Magnitude") +
  labs(title = "Top 50 Strongest Earthquakes (1995-2023)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

# Line plot of the number of earthquakes per year
earthquake$Date_Time <- as.POSIXct(earthquake$date_time, format = "%d-%m-%Y %H:%M")
earthquake$Year <- format(earthquake$Date_Time, "%Y")
earthquakes_per_year <- earthquake %>%
  group_by(Year) %>%
  summarise(Count = n())
earthquakes_per_year$Year <- as.numeric(earthquakes_per_year$Year)

ggplot(earthquakes_per_year, aes(x = Year, y = Count)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Earthquakes per Year (1995-2023)",
       x = "Year",
       y = "Number of Earthquakes") +
  theme_minimal()

# Scatter plot of magnitudes by continent
earthquake_continent <- earthquake %>%
  filter(!is.na(continent) & continent != "")

ggplot(earthquake_continent, aes(x = continent, y = magnitude)) +
  geom_boxplot() +
  labs(title = "Earthquake Magnitudes by Continent (1995-2023)",
       x = "Continent",
       y = "Magnitude") +
  theme_minimal()

# Scatter plot of depth by magnitude
ggplot(earthquake, aes(x = magnitude, y = depth)) +
  geom_bin2d(bins = 30) +
  scale_fill_continuous(type = "viridis", name = "Count") +
  labs(title = "Magnitude vs Depth", x = "Magnitude", y = "Depth") +
  theme_minimal()