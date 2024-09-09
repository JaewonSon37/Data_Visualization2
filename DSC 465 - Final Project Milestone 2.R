# Load libraries
library(ggplot2)
library(dplyr)
library(ggcorrplot)

# Load dataset
earthquake <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Final Project\\Data File\\earthquake_1995-2023.csv")
head(earthquake)

# Display the structure
str(earthquake)

# Histogram of earthquake magnitudes
ggplot(earthquake, aes(x = magnitude)) +
  geom_histogram(binwidth = 0.1, color = "black") +
  labs(title = "Distribution of Earthquake Magnitudes", x = "Magnitude", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Scatter plot of magnitude by depth
ggplot(earthquake, aes(x = depth, y = magnitude)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(title = "Magnitude vs Depth", x = "Depth", y = "Magnitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Scatter plot of earthquake occurrences 
ggplot(earthquake, aes(x = longitude, y = latitude)) +
  geom_bin2d(bins = 50) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Earthquake Occurrences across Coordinates", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Correlation matrix
numeric_data <- earthquake %>% 
  select_if(is.numeric) %>%
  select(-tsunami, -latitude, -longitude)
correlation_matrix <- cor(numeric_data)

ggcorrplot(correlation_matrix, type = "lower", lab = TRUE, digits = 2)