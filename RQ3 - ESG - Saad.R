# Doing only for comapanies after 2000

install.packages("ggplot2")
library(dplyr)
library(ggplot2)
library(tidyr)
library(cluster)
# Read file named "US_emissions.csv" and store it in a variable called "US_emissions"
US_emissions <- read.csv("US_emissions.csv")

# Column names of the data frame
colnames(US_emissions)

# Check data types of the columns
str(US_emissions)

# Remove Un-necessary columns for the summation of the total emissions of the companies based on year and parent_entity
US_emissions_1 <- US_emissions[, -c(3, 4, 5, 6, 7, 9, 17)]

# Create a new data frame called "US_emissions_2" where all of the columns with data type number are summed based on same year and parent_entity
US_emissions_2 <- US_emissions_1 %>%
  group_by(year, parent_entity) %>%
  summarise_all(sum)

# Give me a table with total number of rows for different years and save in a data frame
table(US_emissions_2$year)

# Distribution Chart for Years
ggplot(US_emissions_2, aes(x = year)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Data by Year",
       x = "Year",
       y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Filter for recent years (2010 onwards)
US_emissions_recent <- US_emissions_2 %>%
  filter(year >= 2000)

# SCOPE 1 Emissions Analysis
# Aggregating data by year to observe overall trend in scope 1 emissions
# Aggregate total emissions by year
total_emissions_by_year_S1 <- US_emissions_recent %>%
  group_by(year) %>%
  summarise(total_operational_emissions_MtCO2e = sum(total_operational_emissions_MtCO2e))


# Scope 1: Total Operational Emissions Over Time
ggplot(total_emissions_by_year_S1, aes(x = year, y = total_operational_emissions_MtCO2e)) +
  geom_line(color = "blue") +
  labs(title = "Total Operational Emissions (MtCO2e) Over Time",
       x = "Year",
       y = "Total Operational Emissions (MtCO2e)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


# Company Level Trends
# Find top 5 highest emitting companies (average over time) Scope 1
top_5_emitting_companies_S1 <- US_emissions_recent %>%
  group_by(parent_entity) %>%
  summarise(total_operational_emissions_MtCO2e = sum(total_operational_emissions_MtCO2e)) %>%
  arrange(desc(total_operational_emissions_MtCO2e)) %>%
  head(5)
# Filter dataset for top 5 companies Scope 1
df_top_companies_S1 <- US_emissions_recent %>% filter(parent_entity %in% top_5_emitting_companies_S1$parent_entity)

# Scope 1: Emissions for Top 5 Companies
ggplot(df_top_companies_S1, aes(x = year, y = total_operational_emissions_MtCO2e, color = parent_entity)) +
  geom_line() +
  labs(title = "Total Scope 1 Emissions Over Time for Top 5 Emitting Companies",
       x = "Year",
       y = "Total Operational Emissions (MtCO2e)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


# Calculating the year-over-year percentage change in total emissions to determine if there’s an annual reduction
# Calculate YoY percentage change in total operational emissions Scope 1
US_emissions_recent <- US_emissions_recent %>%
  arrange(parent_entity, year) %>%
  group_by(parent_entity) %>%
  mutate(yoy_change = (total_operational_emissions_MtCO2e - lag(total_operational_emissions_MtCO2e)) / lag(total_operational_emissions_MtCO2e) * 100)
# Compute average yearly reduction for each company Scope 1
emission_reduction_summary_S1 <- US_emissions_recent %>%
  group_by(parent_entity) %>%
  summarise(avg_yoy_change = mean(yoy_change, na.rm = TRUE)) %>%
  arrange(avg_yoy_change)
print(emission_reduction_summary_S1)


# Define custom colors for specific companies, with highest average emissions
emission_reduction_summary_S1$color <- ifelse(emission_reduction_summary_S1$parent_entity %in% 
                                                c("Chevron", "Peabody Energy", "ConocoPhillips", "Occidental Petroleum", "Arch Resources", "Chesapeake Energy"), "highlight", "normal")


# Scope 1: Average Yearly Change
ggplot(emission_reduction_summary_S1, aes(x = reorder(parent_entity, -avg_yoy_change), y = avg_yoy_change, fill = color)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Yearly Change in Total Scope 1 Emissions (%) by Companies",
       x = "Company",
       y = "Average Yearly Change (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  scale_fill_manual(values = c("highlight" = "red", "normal" = "skyblue"), guide = "none")
# Assessing the Emission Reduction Trend through regression
# Fit a linear regression model
lm_model_S1 <- lm(total_operational_emissions_MtCO2e ~ year, data = total_emissions_by_year_S1)
summary(lm_model_S1)

# Scope 1: Linear Regression
ggplot(total_emissions_by_year_S1, aes(x = year, y = total_operational_emissions_MtCO2e)) +
  geom_line(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Total Scope 1 Emissions Over Time with Linear Regression",
       x = "Year",
       y = "Total Operational Emissions (MtCO2e)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
# According to above, there has been an a gradual decrease total operational emissions (Scope1) over time but there has been a recent spike of increase.

# SCOPE 3 Emissions Analysis

# Aggregating data by year to observe overall trend in scope 3 emissions
# Aggregate total emissions by year
total_emissions_by_year_S3 <- US_emissions_recent %>%
  group_by(year) %>%
  summarise(product_emissions_MtCO2 = sum(product_emissions_MtCO2))

# Scope 3: Total Product Emissions Over Time
ggplot(total_emissions_by_year_S3, aes(x = year, y = product_emissions_MtCO2)) +
  geom_line(color = "blue") +
  labs(title = "Total Product Emissions (MtCO2) Over Time",
       x = "Year",
       y = "Total Product Emissions (MtCO2)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Company Level Trends
# Find top 5 highest emitting companies (average over time) Scope 3
top_5_emitting_companies_S3 <- US_emissions_recent %>%
  group_by(parent_entity) %>%
  summarise(product_emissions_MtCO2 = sum(product_emissions_MtCO2)) %>%
  arrange(desc(product_emissions_MtCO2)) %>%
  head(5)

# Scope 3: Emissions for Top 5 Companies
ggplot(df_top_companies_S3, aes(x = year, y = product_emissions_MtCO2, color = parent_entity)) +
  geom_line() +
  labs(title = "Total Scope 3 Emissions Over Time for Top 5 Emitting Companies",
       x = "Year",
       y = "Total Product Emissions (MtCO2)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
# Calculating the year-over-year percentage change in total emissions to determine if there’s an annual reduction
# Calculate YoY percentage change in total product emissions Scope 3
US_emissions_recent <- US_emissions_recent %>%
  arrange(parent_entity, year) %>%
  group_by(parent_entity) %>%
  mutate(yoy_change = (product_emissions_MtCO2 - lag(product_emissions_MtCO2)) / lag(product_emissions_MtCO2) * 100)

# Compute average yearly reduction for each company Scope 3
emission_reduction_summary_S3 <- US_emissions_recent %>%
  group_by(parent_entity) %>%
  summarise(avg_yoy_change = mean(yoy_change, na.rm = TRUE)) %>%
  arrange(avg_yoy_change)

# Define custom colors for specific companies, with highest average emissions ("Chevron", "Peabody Energy", "ConocoPhillips", "Occidental Petroleum", "Chesapeake Energy")
emission_reduction_summary_S3$color <- ifelse(emission_reduction_summary_S3$parent_entity %in% 
                                                c("Chevron", "Peabody Energy", "ConocoPhillips", "Occidental Petroleum", "Arch Resources", "Chesapeake Energy"), "highlight", "normal")
# Scope 3: Average Yearly Change
ggplot(emission_reduction_summary_S3, aes(x = reorder(parent_entity, -avg_yoy_change), y = avg_yoy_change, fill = color)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Yearly Change in Total Scope 3 Emissions (%) by Companies",
       x = "Company",
       y = "Average Yearly Change (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  scale_fill_manual(values = c("highlight" = "red", "normal" = "skyblue"), guide = "none")

# Assessing the Emission Reduction Trend through regression
# Fit a linear regression model
lm_model_S3 <- lm(product_emissions_MtCO2 ~ year, data = total_emissions_by_year_S3)

# Scope 3: Linear Regression
ggplot(total_emissions_by_year_S3, aes(x = year, y = product_emissions_MtCO2)) +
  geom_line(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Total Scope 3 Emissions Over Time with Linear Regression",
       x = "Year",
       y = "Total Product Emissions (MtCO2)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
# Join both summaries into one dataframe
combined_summary <- merge(emission_reduction_summary_S1, emission_reduction_summary_S3, 
                          by = "parent_entity", suffixes = c("_S1", "_S3"))

# Add classification logic
combined_summary <- combined_summary %>%
  mutate(classification = case_when(
    avg_yoy_change_S1 < 0 & avg_yoy_change_S3 < 0 ~ "Genuine Reducer",
    avg_yoy_change_S1 < 3 & avg_yoy_change_S3 < 3 ~ "Trying to Reduce",
    TRUE ~ "Concerning"
  ))

# View summary
table(combined_summary$classification)

# Visualization: classify companies
ggplot(combined_summary, aes(x = avg_yoy_change_S1, y = avg_yoy_change_S3, color = classification)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  labs(
    title = "Company-Level Scope 1 vs Scope 3 Emission Trends",
    x = "Avg YoY Change in Scope 1 Emissions (%)",
    y = "Avg YoY Change in Scope 3 Emissions (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )



