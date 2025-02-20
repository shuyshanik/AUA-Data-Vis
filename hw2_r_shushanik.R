library(dplyr)
library(lubridate)
library(ggplot2)
install.packages("viridis")
library(ggthemes)
library(readr)
library(viridis)

# 3.1
lung_data <- read.csv("/Users/shushanikgortsunian/Downloads/lung_cancer_prediction_dataset.csv", sep=",")

#colnames(lung_data)

ggplot(lung_data, aes(y = Annual_Lung_Cancer_Deaths)) +
  geom_boxplot(fill = "blue", color = "grey", outlier.color = "blue", outlier.size = 2) +
  labs(title = "Boxplot of Lung Cancer Deaths Distribution", y = "Annual Lung Cancer Deaths") +
  theme_minimal()


# 3.2
air_data <- read.csv("/Users/shushanikgortsunian/Downloads/global_air_pollution_dataset.csv", sep=",")

ggplot(air_data, aes(x = PM2.5_AQI_Value)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7, color = "grey") +
  labs(title = "Histogram of PM2.5 AQI Values", x = "PM2.5 AQI Value", y = "Frequency") +
  theme_minimal()


# 3.3
ggplot(lung_data, aes(x = Mortality_Rate)) +
  geom_density(fill = "blue", alpha = 0.6, color = "grey") + 
  labs(title = "Density Plot of Lung Cancer Mortality Rate", x = "Lung Cancer Mortality Rate", y = "Density") +
  theme_minimal()


# 3.4
set.seed(42)

points <- data.frame(
  Normal = rnorm(100),
  Logistic = rlogis(100)
)

ggplot(points, aes(x = Normal, y = Logistic)) +
  geom_point(color = "brown", alpha = 0.9) +  theme_minimal() +
  labs(title = "Scatter Plot of Normal vs. Logistic Distribution",x = "Normal Distribution Values", y = "Logistic Distribution Values") +
  theme_solarized(light = FALSE)


#4.2

lung_agg <- lung_data %>%
  group_by(Country) %>%
  summarise(Annual_Lung_Cancer_Deaths = sum(Annual_Lung_Cancer_Deaths, na.rm = TRUE))

air_agg <- air_data %>%
  group_by(Country) %>%
  summarise(PM2.5_AQI = mean(PM2.5_AQI_Value, na.rm = TRUE))

merged <- inner_join(lung_agg, air_agg, by = "Country")

countries <- c("Egypt", "Ethiopia", "France", "Germany", "India", 
                        "Indonesia", "Italy", "Japan", "Mexico", "Myanmar", 
                        "Nigeria", "Pakistan", "Philippines", "South Africa", 
                        "Thailand", "Turkey")

colors <- setNames(
  rainbow(n = nrow(merged)),   
  merged$Country
)

ggplot(merged, aes(x = PM2.5_AQI, y = Annual_Lung_Cancer_Deaths, color = Country)) +
  geom_point(size = 3) +
  geom_text(aes(label = ifelse(Country %in% c("China", "India", "Japan"), Country, "")), 
            size = ifelse(merged$Country == "China", 6, 3), 
            fontface = "bold", color = "black") +  
  labs(title = "PM2.5 AQI vs. Annual Lung Cancer Deaths", 
       x = "PM2.5 AQI Value", 
       y = "Annual Lung Cancer Deaths", 
       color = "Annual_Lung_Cancer_Deaths") +
  scale_color_manual(values = colors, breaks = countries) +  
  theme_classic() +  
  theme(
    plot.title = element_text(face = "bold", color = "darkred", size = 20, hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1, color = "blue"),  
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right",  
    legend.title = element_text(size = 13, margin = margin(t = 10)),  
    legend.text = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(linetype = "dashed", color = "gray")   
  ) + guides(color = guide_legend(title.position = "bottom", title.hjust = 0.5))


# 4.3

lung_data$Cancer_Stage <- as.factor(lung_cancer$Cancer_Stage)

#remove none vals and 0s
cancer_smoke <- lung_data %>%
  filter(Cancer_Stage != "None", Years_of_Smoking>0)

ggplot(cancer_smoke, aes(x = Years_of_Smoking, y = Cancer_Stage, color = Gender, shape = Gender)) +
  geom_jitter(alpha = 0.7, size = 2.0) +  
  scale_color_manual(values = c("Female" = "#d554f1", "Male" = "#5469f1")) +  
  scale_shape_manual(values = c("Female" = 17, "Male" = 16)) +  
  facet_wrap(~Gender) +  
  labs(title = "Lung Cancer Stage vs. Smoking Years",
       subtitle = "Comparison by Gender", x = "Years of Smoking", y = "Cancer Stage", color = "Gender") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12, color = "black", face = "italic"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),  
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )


# 4.4

dist_countries <- c("Brazil", "Germany", "India", 
                    "Italy", "Russian Federation", 
                    "United States of America")

air_data_selected <- air_data %>%
  filter(Country %in% dist_countries)

ggplot(air_data_selected, aes(x = PM2.5_AQI_Value, fill = Country)) +
  geom_histogram(bins = 50, alpha = 0.8, color = "black") +
  scale_fill_viridis_d(option = "plasma") +   
  facet_wrap(~Country, scales = "free_y") +  
  labs(title = "PM2.5 AQI Distribution Across Countries",
       subtitle = "Comparison of Air Pollution Levels", x = "PM2.5 AQI Value",y = "Frequency",fill = "Country") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14), legend.position = "bottom",
    plot.subtitle = element_text(size = 12, color = "gray40", face = "italic"),
    strip.text = element_text(face = "bold", size = 12) 
  )







