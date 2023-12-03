# Clear all previously stored variables
rm(list = ls())

install.packages("Hmisc")



# Read the dataset
file_path <- "C:/Users/fabio/OneDrive/Documents/CA1-Teste/owid-covid-data.csv"
covid_data <- read.csv(file_path)


describe(covid_data)

# Load necessary libraries
library(dplyr)
library(tidyr)

# Assuming covid_data is your dataframe
# Replace this with your actual dataframe name if different


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




library(ggplot2)
library(dplyr)

# Filtre o conjunto de dados para incluir apenas as datas desejadas
selected_dates <- c("2020-03-01", "2020-03-06", "2020-03-09", "2020-03-12",
                    "2021-03-01", "2021-03-06", "2021-03-09", "2021-03-12",
                    "2022-03-01", "2022-03-06", "2022-03-09", "2022-03-12",
                    "2023-03-01", "2023-03-06", "2023-03-09", "2023-11-22")

selected_data <- covid_data %>% 
  filter(date %in% as.Date(selected_dates))

# Função para calcular os parâmetros estatísticos e criar um gráfico de linha
calculate_statistics_and_plot <- function(variable, variable_name) {
  # Calcular estatísticas
  mean_val <- mean(variable, na.rm = TRUE)
  median_val <- median(variable, na.rm = TRUE)
  min_val <- min(variable, na.rm = TRUE)
  max_val <- max(variable, na.rm = TRUE)
  sd_val <- sd(variable, na.rm = TRUE)
  
  # Criar gráfico de linha
  ggplot(selected_data, aes(x = date, y = variable)) +
    geom_line() +
    labs(title = paste(variable_name, "Over Time"), x = "Date", y = variable_name) +
    theme_minimal() +
    # Adicionar anotações para estatísticas
    annotate("text", x = max(selected_data$date), y = max(variable, na.rm = TRUE),
             label = paste("Mean:", round(mean_val, 2), "\nMedian:", round(median_val, 2),
                           "\nMin:", round(min_val, 2), "\nMax:", round(max_val, 2),
                           "\nSD:", round(sd_val, 2)), hjust = 1, vjust = 1)
}

# Calcular estatísticas e criar gráficos para total_deaths, total_vaccinations e population
plot_total_deaths <- calculate_statistics_and_plot(selected_data$total_deaths, "Total Deaths")
plot_total_vaccinations <- calculate_statistics_and_plot(selected_data$total_vaccinations, "Total Vaccinations")
plot_population <- calculate_statistics_and_plot(selected_data$population, "Population")

# Exibir os gráficos
print(plot_total_deaths)
print(plot_total_vaccinations)
print(plot_population)






# Filtrar dados para remover valores ausentes
filtered_data <- covid_data %>% 
  filter(!is.na(median_age) & !is.na(total_deaths))

# Criar o gráfico de dispersão
ggplot(filtered_data, aes(x = median_age, y = total_deaths)) +
  geom_point() +
  labs(title = "Impact of the Middle Ages on the Mortality Rate",
       x = "Population Age Average",
       y = "Total of dead") +
  theme_minimal()













