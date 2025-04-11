
##Linear Regression & K-Means Clustering w/ ANOVA 

library(ggplot2)
library(dplyr)
library(cluster)   # For clustering
library(factoextra) # For visualizing clusters
library(tibble)    # For easier data manipulation

# Step 1: Load your dataset
# ------------------------------------------------------

# load data set
k.means.df <-Emissions_Return

# -------------------------------------------------------

#1. linear model:

#corelation between stock returns and emissions
# Calculate the correlation coefficient
correlation_coefficient <- cor(k.means.df$annual_return, k.means.df$total_operational_emissions_MtCO2e)

linearmodel <- lm(annual_return ~ total_operational_emissions_MtCO2e, data = k.means.df)

summary(linearmodel)
# Create a scatter plot with the regression line
ggplot(k.means.df, aes(x = total_operational_emissions_MtCO2e, y = annual_return)) + 
  geom_point(size = 1) +  # Plot the points without any clustering color
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add the regression line
  labs(title = "Scatter Plot of Annual Return vs Total Operational Emissions with Regression Line", 
       x = "Total Operational Emissions (MtCO2e)", 
       y = "Average Annual Return (%)") + 
  theme_minimal()

# -------------------------------------------------------
#2. K-Means Clustering & ANOVA

# elbow method for clustering 
# Step 1: Determine the optimal number of clusters using the elbow method
#scale annual return data
k.means.df$annual_return <- scale(k.means.df$annual_return)

#finding optimal number of clusters 
fviz_nbclust(data.frame(annual_return = k.means.df$annual_return), kmeans, method = "wss") + labs(title = "Elbow Method")

# optimal number of clusters is 3
# Step 3: Perform K-Means Clustering
# ------------------------------------------------------------
# Perform K-Means clustering with 3 clusters
set.seed(123)  # Set seed for reproducibility
kmeans_result <- kmeans(k.means.df$annual_return, centers = 3, nstart = 25)

# Statistics of each cluster
cluster_stats <- kmeans_result %>%
  group_by(Cluster) %>%
  summarise(
    mean_annual_return = mean(annual_return),
    sd_annual_return = sd(annual_return),
    count = n()
  )
print(cluster_stats)


# ------------------------------------------------------------

# Step 5: Analyze the clusters
#historgram of annual return 
ggplot(k.means.df, aes(x = annual_return)) + 
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.5) + 
  labs(title = "Histogram of Annual Returns with Density Curve", 
       x = "Annual Return", 
       y = "Count") + 
  theme_minimal()
# ------------------------------------------------------------
# Step 6 ANOVA 
# Perform ANOVA to check if the means of the clusters are significantly different

anova_result <- aov(total_operational_emissions_MtCO2e ~ factor(Cluster), data = kmeans_with_clusters)
summary(anova_result)
