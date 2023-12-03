# Clear all previously stored variables
rm(list = ls())

# Install necessary packages
install.packages(c("Hmisc", "ggplot2", "tidyverse", "FactoMineR", "factoextra"))

# Read the dataset
file_path <- "C:/Users/fabio/OneDrive/Documents/CA1-Teste/owid-covid-data.csv"
covid_data <- read.csv(file_path)

# Explore and describe the dataset
library(Hmisc)
describe(covid_data)

# Data Cleaning
library(dplyr)
library(tidyr)

# List of variables to clean
vars_to_clean <- c("total_deaths", "total_vaccinations", "people_fully_vaccinated", 
                   "median_age", "aged_65_older", "aged_70_older")

# Remove NAs from specific variables
covid_data_cleaned <- covid_data %>%
  drop_na(!!vars_to_clean)

# Check if NAs are removed
na_df_cleaned <- covid_data_cleaned %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(variable, missing_count) %>%
  filter(missing_count > 0)

# Print the cleaned dataframe
print(covid_data_cleaned)

# Print the summary of NAs after cleaning
print(na_df_cleaned)


# Statistical Analysis and Visualization
library(ggplot2)

# Filter the dataset for selected dates
selected_dates <- c("2020-03-01", "2020-03-06", "2020-03-09", "2020-03-12",
                    "2021-03-01", "2021-03-06", "2021-03-09", "2021-03-12",
                    "2022-03-01", "2022-03-06", "2022-03-09", "2022-03-12",
                    "2023-03-01", "2023-03-06", "2023-03-09", "2023-11-22")

selected_data <- covid_data %>% 
  filter(date %in% as.Date(selected_dates))

# Function to calculate statistics and create a line plot
calculate_statistics_and_plot <- function(variable, variable_name) {
  # Calculate statistics
  mean_val <- mean(variable, na.rm = TRUE)
  median_val <- median(variable, na.rm = TRUE)
  min_val <- min(variable, na.rm = TRUE)
  max_val <- max(variable, na.rm = TRUE)
  sd_val <- sd(variable, na.rm = TRUE)
  
  # Create a line plot
  ggplot(selected_data, aes(x = date, y = variable)) +
    geom_line() +
    labs(title = paste(variable_name, "Over Time"), x = "Date", y = variable_name) +
    theme_minimal() +
    # Add annotations for statistics
    annotate("text", x = max(selected_data$date), y = max(variable, na.rm = TRUE),
             label = paste("Mean:", round(mean_val, 2), "\nMedian:", round(median_val, 2),
                           "\nMin:", round(min_val, 2), "\nMax:", round(max_val, 2),
                           "\nSD:", round(sd_val, 2)), hjust = 1, vjust = 1)
}

# Calculate statistics and create plots for total_deaths, total_vaccinations, and population
plot_total_deaths <- calculate_statistics_and_plot(selected_data$total_deaths, "Total Deaths")
plot_total_vaccinations <- calculate_statistics_and_plot(selected_data$total_vaccinations, "Total Vaccinations")
plot_population <- calculate_statistics_and_plot(selected_data$population, "Population")

# Display the plots
print(plot_total_deaths)
print(plot_total_vaccinations)
print(plot_population)


# Scatter Plot
# Filter data to remove missing values
filtered_data <- covid_data %>% 
  filter(!is.na(median_age) & !is.na(total_deaths))

# Create a scatter plot
ggplot(filtered_data, aes(x = median_age, y = total_deaths)) +
  geom_point() +
  labs(title = "Impact of the Middle Ages on the Mortality Rate",
       x = "Population Age Average",
       y = "Total of dead") +
  theme_minimal()

# Scatter Plot with Specific Dates
# Filter data to remove missing values and select specific dates
filtered_data <- covid_data %>% 
  filter(!is.na(median_age) & !is.na(total_deaths),
         date %in% as.Date(c("2020-03-01", "2020-03-06", "2020-03-09", "2020-03-12")))

# Create a scatter plot
ggplot(filtered_data, aes(x = median_age, y = total_deaths)) +
  geom_point() +
  labs(title = "Impact of the Middle Ages on the Mortality Rate (Specific Dates)",
       x = "Population Age Average",
       y = "Total of dead") +
  theme_minimal()

# Scatter Plot with Percentage of People Fully Vaccinated and Decrease in New Cases
# Filter data to remove missing values and select specific variables
filtered_data <- covid_data %>% 
  filter(!is.na(people_fully_vaccinated) & !is.na(new_cases),
         date %in% as.Date(c("2020-03-01", "2020-03-06", "2020-03-09", "2020-03-12")))

# Create a scatter plot
ggplot(filtered_data, aes(x = people_fully_vaccinated, y = -new_cases)) +
  geom_point() +
  labs(title = "People Fully Vaccinated and Decrease in New Cases (Specific Dates)",
       x = "Percentage of People Fully Vaccinated",
       y = "Decrease in New Cases (Negative for Visualization)") +
  theme_minimal()

# Heatmap
# Filter data for selected dates and variables
selected_dates <- c("2020-03-01", "2020-03-06", "2020-03-09", "2020-03-12",
                    "2021-03-01", "2021-03-06", "2021-03-09", "2021-03-12",
                    "2022-03-01", "2022-03-06", "2022-03-09", "2022-03-12",
                    "2023-03-01", "2023-03-06", "2023-03-09", "2023-11-22")

filtered_data <- covid_data %>% 
  filter(date %in% as.Date(selected_dates),
         !is.na(median_age) & !is.na(total_deaths))

# Calculate the percentage of people fully vaccinated
selected_data <- filtered_data %>%
  mutate(percentage_vaccinated = people_fully_vaccinated / population * 100)

# Create a heatmap
ggplot(selected_data, aes(x = percentage_vaccinated, y = new_cases)) +
  geom_tile(aes(fill = total_cases), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Relationship between % of Fully Vaccinated People and New Cases",
       x = "Percentage of People Fully Vaccinated",
       y = "New Cases") +
  theme_minimal()


# Comparison of Total Deaths in Asia and Europe
# Filter data for Asia and Europe
asia_data <- covid_data %>% 
  filter(continent == "Asia")

europe_data <- covid_data %>% 
  filter(continent == "Europe")

# Calculate total deaths for each region
total_deaths_asia <- sum(asia_data$total_deaths, na.rm = TRUE)
total_deaths_europe <- sum(europe_data$total_deaths, na.rm = TRUE)

# Create a data frame for visualization
comparison_data <- data.frame(Region = c("Asia", "Europe"),
                              Total_Deaths = c(total_deaths_asia, total_deaths_europe))

# Create a bar plot
ggplot(comparison_data, aes(x = Region, y = Total_Deaths, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of Total Deaths in Asia and Europe",
       x = "Region", y = "Total Deaths") +
  theme_minimal() +
  scale_fill_manual(values = c("Asia" = "skyblue", "Europe" = "salmon"))


# Principal Component Analysis (PCA)
# Install the 'factoextra' package if not installed
install.packages("factoextra")

library(FactoMineR)
library(factoextra)

# Select relevant variables for mortality analysis
mortality_data <- covid_data %>%
  select(location, total_deaths, total_cases_per_million, new_deaths_per_million, population_density,
         median_age, aged_65_older, aged_70_older, gdp_per_capita, cardiovasc_death_rate, diabetes_prevalence,
         female_smokers, male_smokers, life_expectancy)

# Remove rows with missing values
mortality_data <- na.omit(mortality_data)

# Standardize numerical variables
mortality_data_standardized <- scale(mortality_data[, -1])

# Apply PCA
pca_result <- PCA(mortality_data_standardized, graph = FALSE)

# Visualize variable contributions to the first two principal components
fviz_contrib(pca_result, choice = "var", axes = 1:2)

# Visualize observations in the first two principal components
fviz_pca_ind(pca_result, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, habillage = "location")
