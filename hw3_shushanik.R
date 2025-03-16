library(dplyr)
library(ggplot2)
library(tidyr) 
library(viridis)
library(tidyverse)

mobile_data <- read.csv("/Users/shushanikgortsunian/Downloads/mobiles_dataset.csv", sep=",")

#colnames(mobile_data)

# Conversion rates
mobile_data <- mobile_data %>%
  mutate(
    Price_Pakistan_USD = Launched.Price.Pakistan.PKR * 0.0036,
    Price_India_USD = Launched.Price.India.INR * 0.011,
    Price_China_USD = Launched.Price.China.CNY * 0.14,
    Price_Dubai_USD = Launched.Price.Dubai.AED * 0.27,
    Price_USA_USD = Launched.Price.USA.USD  
  )

#part1
#1.1
correlations <- mobile_data %>%
  summarize(
    Pakistan = cor(Battery.Capacity.mAh, Price_Pakistan_USD, use="complete.obs"),
    India = cor(Battery.Capacity.mAh, Price_India_USD, use="complete.obs"),
    China = cor(Battery.Capacity.mAh, Price_China_USD, use="complete.obs"),
    USA = cor(Battery.Capacity.mAh, Price_USA_USD, use="complete.obs"),
    Dubai = cor(Battery.Capacity.mAh, Price_Dubai_USD, use="complete.obs")
  )

print(correlations)

countries <- c("Pakistan" = "Price_Pakistan_USD",
               "India" = "Price_India_USD",
               "China" = "Price_China_USD",
               "USA" = "Price_USA_USD",
               "Dubai" = "Price_Dubai_USD")

ggplot(mobile_data, aes(x = Battery.Capacity.mAh, y = Price_Pakistan_USD)) +
  geom_point(alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Battery Capacity vs. Price in Pakistan",
       x = "Battery Capacity (mAh)",
       y = "Price (USD)")

ggplot(mobile_data, aes(x = Battery.Capacity.mAh, y = Price_India_USD)) +
  geom_point(alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Battery Capacity vs. Price in India",
       x = "Battery Capacity (mAh)",
       y = "Price (USD)")

ggplot(mobile_data, aes(x = Battery.Capacity.mAh, y = Price_China_USD)) +
  geom_point(alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Battery Capacity vs. Price in China",
       x = "Battery Capacity (mAh)",
       y = "Price (USD)")

ggplot(mobile_data, aes(x = Battery.Capacity.mAh, y = Price_USA_USD)) +
  geom_point(alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Battery Capacity vs. Price in USA",
       x = "Battery Capacity (mAh)",
       y = "Price (USD)")

ggplot(mobile_data, aes(x = Battery.Capacity.mAh, y = Price_Dubai_USD)) +
  geom_point(alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Battery Capacity vs. Price in Dubai",
       x = "Battery Capacity (mAh)",
       y = "Price (USD)")

#explanations and answer for the question is provided in python file

#1.2
print(mobile_data$RAM)
ram_value_types <- sapply(mobile_data$RAM, class)
print(ram_value_types)

mobile_data <- mobile_data %>%
  mutate(Numeric_RAM = as.numeric(gsub("GB.*|\\s", "", RAM)))
print(mobile_data$Numeric_RAM)

correlation_pakistan <- cor(mobile_data$Numeric_RAM, mobile_data$Price_Pakistan_USD, use = "complete.obs")
correlation_india <- cor(mobile_data$Numeric_RAM, mobile_data$Price_India_USD, use = "complete.obs")
correlation_china <- cor(mobile_data$Numeric_RAM, mobile_data$Price_China_USD, use = "complete.obs")
correlation_dubai <- cor(mobile_data$Numeric_RAM, mobile_data$Price_Dubai_USD, use = "complete.obs")
correlation_usa <- cor(mobile_data$Numeric_RAM, mobile_data$Price_USA_USD, use = "complete.obs")

cat("Correlation between RAM and Launched Price in Pakistan (USD):", correlation_pakistan, "\n")
cat("Correlation between RAM and Launched Price in India (USD):", correlation_india, "\n")
cat("Correlation between RAM and Launched Price in China (USD):", correlation_china, "\n")
cat("Correlation between RAM and Launched Price in Dubai (USD):", correlation_dubai, "\n")
cat("Correlation between RAM and Launched Price in USA (USD):", correlation_usa, "\n")

mobile_data %>%
  select(starts_with("Price_")) %>%
  pivot_longer(cols = everything(), names_to = "Region", values_to = "Price") %>%
  ggplot(aes(x = Region, y = Price)) +
  geom_boxplot() +
  labs(title = "Boxplot of Prices Across Regions")

par(mfrow = c(2, 2))  

ggplot(mobile_data, aes(x = factor(Numeric_RAM), y = Price_Pakistan_USD)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(x = "RAM (GB)", y = "Price in Pakistan (USD)", 
       title = "Price Distribution in Pakistan vs. RAM") +
  theme_minimal()

ggplot(mobile_data, aes(x = factor(Numeric_RAM), y = Price_India_USD)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(x = "RAM (GB)", y = "Price in India (USD)", 
       title = "Price Distribution in India vs. RAM") +
  theme_minimal()

ggplot(mobile_data, aes(x = factor(Numeric_RAM), y = Price_China_USD)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(x = "RAM (GB)", y = "Price in China (USD)", 
       title = "Price Distribution in China vs. RAM") +
  theme_minimal()

ggplot(mobile_data, aes(x = factor(Numeric_RAM), y = Price_Dubai_USD)) +
  geom_boxplot(fill = "gold", color = "black") +
  labs(x = "RAM (GB)", y = "Price in Dubai (USD)", 
       title = "Price Distribution in Dubai vs. RAM") +
  theme_minimal()

ggplot(mobile_data, aes(x = factor(Numeric_RAM), y = Price_USA_USD)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "RAM (GB)", y = "Price in USA (USD)", 
       title = "Price Distribution in USA vs. RAM") +
  theme_minimal()

# we can see from the graphs that the ram has impact on the price over the countries in most cases, 
#further explanation provided in ipynb file

#1.3
apple_prices <- mobile_data %>%
  filter(grepl("Apple", Company.Name)) %>%
  mutate(
    Price_Pakistan_USD = Launched.Price.Pakistan.PKR * 0.0036,
    Price_India_USD = Launched.Price.India.INR * 0.011,
    Price_China_USD = Launched.Price.China.CNY * 0.14,
    Price_Dubai_USD = Launched.Price.Dubai.AED * 0.27,
    Price_USA_USD = Launched.Price.USA.USD
  ) %>%
  select(Model.Name, Price_Pakistan_USD, Price_India_USD, Price_China_USD, Price_Dubai_USD, Price_USA_USD) %>%
  pivot_longer(cols = starts_with("Price"), names_to = "Country", values_to = "Price")

ggplot(apple_prices, aes(x = Country, y = Price, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Price (USD)", 
       title = "Price of Apple Mobile Phones in Different Countries") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# we can see the difference of apple prices accross the countries, lowest is in pakistan, highest is in India

other_prices <- mobile_data %>%
  filter(!grepl("Apple", Company.Name)) %>%
  mutate(
    Price_Pakistan_USD = Launched.Price.Pakistan.PKR * 0.0036,
    Price_India_USD = Launched.Price.India.INR * 0.011,
    Price_China_USD = Launched.Price.China.CNY * 0.14,
    Price_Dubai_USD = Launched.Price.Dubai.AED * 0.27,
    Price_USA_USD = Launched.Price.USA.USD
  ) %>%
  select(Model.Name, Price_Pakistan_USD, Price_India_USD, Price_China_USD, Price_Dubai_USD, Price_USA_USD) %>%
  pivot_longer(cols = starts_with("Price"), names_to = "Country", values_to = "Price")

ggplot(other_prices, aes(x = Country, y = Price, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Price (USD)", 
       title = "Price of Mobile Phones from Other Companies in Different Countries") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#prices of different models in pakistan
average_prices_pakistan <- mobile_data %>%
  mutate(Price_Pakistan_USD = Launched.Price.Pakistan.PKR * 0.0036) %>%
  group_by(Company.Name) %>%  
  summarise(Average_Price = mean(Price_Pakistan_USD, na.rm = TRUE)) %>%  
  ungroup()  

ggplot(average_prices_pakistan, aes(x = reorder(Company.Name, Average_Price), y = Average_Price)) +
  geom_bar(stat = "identity", fill = "steelblue") +  
  labs(x = "Company Name", y = "Average Price in Pakistan (USD)", 
       title = "Average Price of Mobile Phones by Company in Pakistan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

#in india
average_prices_india <- mobile_data %>%
  mutate(Price_India_USD = Launched.Price.India.INR * 0.011) %>%
  group_by(Company.Name) %>%
  summarise(Average_Price = mean(Price_India_USD, na.rm = TRUE)) %>%
  ungroup()

ggplot(average_prices_india, aes(x = reorder(Company.Name, Average_Price), y = Average_Price)) +
  geom_bar(stat = "identity", fill = "orange") +  
  labs(x = "Company Name", y = "Average Price in India (USD)", 
       title = "Average Price of Mobile Phones by Company in India") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

# in china
average_prices_china <- mobile_data %>%
  mutate(Price_China_USD = Launched.Price.China.CNY * 0.14) %>%
  group_by(Company.Name) %>%
  summarise(Average_Price = mean(Price_China_USD, na.rm = TRUE)) %>%
  ungroup()

ggplot(average_prices_china, aes(x = reorder(Company.Name, Average_Price), y = Average_Price)) +
  geom_bar(stat = "identity", fill = "green") +  
  labs(x = "Company Name", y = "Average Price in China (USD)", 
       title = "Average Price of Mobile Phones by Company in China") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# in USA
average_prices_usa <- mobile_data %>%
  group_by(Company.Name) %>%
  summarise(Average_Price = mean(Launched.Price.USA.USD, na.rm = TRUE)) %>%
  ungroup()

ggplot(average_prices_usa, aes(x = reorder(Company.Name, Average_Price), y = Average_Price)) +
  geom_bar(stat = "identity", fill = "red") +  
  labs(x = "Company Name", y = "Average Price in USA (USD)", 
       title = "Average Price of Mobile Phones by Company in USA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# in dubai
average_prices_dubai <- mobile_data %>%
  mutate(Price_Dubai_USD = Launched.Price.Dubai.AED * 0.27) %>%
  group_by(Company.Name) %>%
  summarise(Average_Price = mean(Price_Dubai_USD, na.rm = TRUE)) %>%
  ungroup()

ggplot(average_prices_dubai, aes(x = reorder(Company.Name, Average_Price), y = Average_Price)) +
  geom_bar(stat = "identity", fill = "pink") +  
  labs(x = "Company Name", y = "Average Price in Dubai (USD)", 
       title = "Average Price of Mobile Phones by Company in Dubai") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#1.4
price_column <- "Price_USA_USD"
mobile_data <- mobile_data %>%
  mutate(
    Segment = case_when(
      !!sym(price_column) < 300 ~ "Budget",
      !!sym(price_column) >= 300 & !!sym(price_column) <= 700 ~ "Mid-range",
      !!sym(price_column) > 700 ~ "Premium",
      TRUE ~ "Unknown"
    )
  )

segment_counts <- mobile_data %>%
  group_by(Company.Name, Segment) %>%
  summarise(Model_Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Segment, values_from = Model_Count, values_fill = 0)
print(segment_counts)

segment_analysis <- segment_counts %>%
  mutate(
    Coverage = case_when(
      Budget > 0 & `Mid-range` > 0 & Premium > 0 ~ "Covers all segments",
      Budget == 0 & `Mid-range` == 0 & Premium > 0 ~ "Only Premium",
      Budget == 0 & `Mid-range` > 0 & Premium == 0 ~ "Only Mid-range",
      TRUE ~ "Others"
    )
  )
print(segment_analysis)
# in segment analysis we can see the table in which we can find which brand covers which segment

ggplot(segment_counts, aes(x = Company.Name)) +
  geom_bar(aes(y = Budget, fill = "Budget"), stat = "identity") +
  geom_bar(aes(y = `Mid-range`, fill = "Mid-range"), stat = "identity") +
  geom_bar(aes(y = Premium, fill = "Premium"), stat = "identity") +
  labs(title = "Number of Models by Price Segment for Each Brand",
       x = "Brand",
       y = "Number of Models") +
  scale_fill_manual(name = "Price Category", 
                    values = c("Budget" = "lightblue", "Mid-range" = "lightgreen", "Premium" = "lightcoral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#1.5
average_prices_by_region <- mobile_data %>%
  mutate(
    Price_Pakistan_USD = Launched.Price.Pakistan.PKR * 0.0036,
    Price_India_USD = Launched.Price.India.INR * 0.011,
    Price_China_USD = Launched.Price.China.CNY * 0.14,
    Price_Dubai_USD = Launched.Price.Dubai.AED * 0.27,
    Price_USA_USD = Launched.Price.USA.USD
  ) %>%
  summarise(
    Avg_Price_Pakistan = mean(Price_Pakistan_USD, na.rm = TRUE),
    Avg_Price_India = mean(Price_India_USD, na.rm = TRUE),
    Avg_Price_China = mean(Price_China_USD, na.rm = TRUE),
    Avg_Price_Dubai = mean(Price_Dubai_USD, na.rm = TRUE),
    Avg_Price_USA = mean(Price_USA_USD, na.rm = TRUE)
  )
print(average_prices_by_region)

average_prices_by_brand_region <- mobile_data %>%
  group_by(Company.Name) %>%
  summarise(
    Avg_Price_Pakistan = mean(Price_Pakistan_USD, na.rm = TRUE),
    Avg_Price_India = mean(Price_India_USD, na.rm = TRUE),
    Avg_Price_China = mean(Price_China_USD, na.rm = TRUE),
    Avg_Price_Dubai = mean(Price_Dubai_USD, na.rm = TRUE),
    Avg_Price_USA = mean(Price_USA_USD, na.rm = TRUE)
  ) %>%
  ungroup()
print(average_prices_by_brand_region)
# brands and their average prices in each region

#print(names(mobile_data))  

price_columns <- c("Price_Pakistan_USD", "Price_India_USD", "Price_China_USD", "Price_USA_USD", "Price_Dubai_USD")
if (!all(price_columns %in% names(mobile_data))) {
  stop("One or more price columns are missing in the dataframe.")
}

avg_price_per_region <- colMeans(mobile_data[, price_columns], na.rm = TRUE)
cheapest_region <- names(avg_price_per_region)[which.min(avg_price_per_region)]

cat("Average smartphone price per region:\n")
print(avg_price_per_region)
cat("\nMost affordable region:", cheapest_region, "\n")

brand_avg_prices <- mobile_data %>%
  group_by(Company.Name) %>%
  summarise(across(all_of(price_columns), ~ mean(., na.rm = TRUE), .names = "avg_{.col}")) %>%
  ungroup() 

brand_price_diff <- brand_avg_prices %>%
  mutate(Price_Diff = apply(across(starts_with("avg_")), 1, max, na.rm = TRUE) - 
           apply(across(starts_with("avg_")), 1, min, na.rm = TRUE)) %>%
  arrange(desc(Price_Diff))
# price difference across regions for each brand

cat("\nBrands with the highest price differences across regions:\n")
print(head(brand_price_diff[, c("Company.Name", "Price_Diff")], 10))

barplot(avg_price_per_region, main = "Average Smartphone Price Per Region", ylab = "Price in USD", col = "lightblue", beside = TRUE)

brand_avg_price <- brand_avg_prices %>%
  summarise(Average_Price = mean(c_across(starts_with("avg_")), na.rm = TRUE), .by = "Company.Name")

brand_avg_price_df <- brand_avg_price %>%
  rename(Brand = Company.Name)

ggplot(brand_avg_price_df, aes(x = reorder(Brand, Average_Price), y = Average_Price)) +
  geom_bar(stat = "identity", aes(fill = Average_Price)) +  
  scale_fill_viridis(option = "D", discrete = FALSE) +  
  labs(x = "Brand", y = "Average Price (USD)", title = "Average Price of Each Brand Across All Countries") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# all further explanations provided in ipynb file

#2.1
# average smartphone price per region
price_columns <- c("Price_Pakistan_USD", "Price_India_USD", "Price_China_USD", "Price_USA_USD", "Price_Dubai_USD")
avg_price_per_region <- colMeans(mobile_data[, price_columns], na.rm = TRUE)

avg_price_df <- data.frame(
  Region = names(avg_price_per_region),
  Average_Price = avg_price_per_region
)

ggplot(avg_price_df, aes(x = reorder(Region, Average_Price), y = Average_Price)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Region", y = "Average Price (USD)", title = "Average Smartphone Price Per Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#2.2
#total number of models for each brand
brand_market_share <- mobile_data %>%
  group_by(Company.Name) %>%
  summarise(Count = n()) %>%
  ungroup()

ggplot(brand_market_share, aes(x = "", y = Count, fill = Company.Name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  
  labs(title = "Market Share of Smartphone Brands") +
  theme_void() +  
  theme(legend.position = "right")  


#3.1
ggplot(mobile_data, aes(x = Company.Name, y = Price_USA_USD, fill = Company.Name)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Avoid double plotting outliers
  geom_jitter(width = 0.2, size = 1, alpha = 0.8) +  # Overlay individual data points
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Price Distribution by Company in USA",
    subtitle = "A boxplot showing how the price varies by company, with individual data points overlaid",
    x = "Company",
    y = "Price in USD",
    fill = "Company Name"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.subtitle = element_text(face = "italic")
  )

#3.2
ggplot(mobile_data, aes(x = Battery.Capacity.mAh, y = Price_USA_USD, color = Company.Name, size = Battery.Capacity.mAh)) +
  geom_point(alpha = 0.8) +
  labs(
    title = "Battery Capacity vs. Price in USA",
    subtitle = "The relationship between battery capacity, price, and screen size across different smartphone brands",
    x = "Battery Capacity",
    y = "Price",
    color = "Brand"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.subtitle = element_text(face = "italic")  
  ) +
  scale_size_continuous(range = c(2, 6)) +
  guides(
    size = "none"  
  )


#3.3
top_5_brands <- c("Apple", "Honor", "Oppo", "Samsung", "Vivo")

filtered_data <- mobile_data %>%
  filter(Company.Name %in% top_5_brands)

shape_map <- c(
  "Apple" = 16,    
  "Honor" = 17,    
  "Oppo" = 18,    
  "Samsung" = 15,  
  "Vivo" = 19       
)

ggplot(filtered_data, aes(
  x = Battery.Capacity.mAh,
  y = Price_USA_USD,
  shape = Company.Name,
  color = Screen.Size.inches,
  size = Battery.Capacity.mAh
)) +
  geom_point(alpha = 0.8) +
  scale_shape_manual(values = shape_map) +
  scale_size_continuous(range = c(1.5, 6)) +
  labs(
    title = "Battery Capacity vs. Price for Top 5 Brands",
    subtitle = "Different Shapes for Each Brand, Color by Screen Size, (USA)",
    x = "Battery Capacity (mAh)",
    y = "Price (USD)",
    shape = "Brand",
    color = "Screen Size (in)",
    size = "Battery Capacity (mAh)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.subtitle = element_text(face = "italic")  
  ) +
  guides(
    color = "none",  
    size = "none",   
    shape = guide_legend(title = "Brand")  
  )


install.packages("rmarkdown")


