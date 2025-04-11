# Re-import required libraries after reset
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Reload the dataset
df = pd.read_excel("US_emissions.xlsx")

# Drop unnecessary columns
df_clean = df.drop(columns=[col for col in df.columns if "Unnamed" in col])

# Calculate emission intensities
df_clean["Scope1_Intensity"] = df_clean["total_operational_emissions_MtCO2e"] / df_clean["production_value"]
df_clean["Scope3_Intensity"] = df_clean["product_emissions_MtCO2"] / df_clean["production_value"]

# Keep relevant columns
df_intensity = df_clean[[
    "year", "parent_entity", "production_value", 
    "total_operational_emissions_MtCO2e", "product_emissions_MtCO2",
    "Scope1_Intensity", "Scope3_Intensity"
]]

# Sort for YoY calculation
df_sorted = df_intensity.sort_values(by=["parent_entity", "year"]).reset_index(drop=True)

# Compute year-over-year changes
df_sorted["Scope1_Δ"] = df_sorted.groupby("parent_entity")["total_operational_emissions_MtCO2e"].diff()
df_sorted["Scope3_Δ"] = df_sorted.groupby("parent_entity")["product_emissions_MtCO2"].diff()
df_sorted["Production_Δ"] = df_sorted.groupby("parent_entity")["production_value"].diff()

# Classification logic
def classify_greenwashing(row):
    if pd.isna(row["Scope1_Δ"]) or pd.isna(row["Scope3_Δ"]) or pd.isna(row["Production_Δ"]):
        return "Insufficient Data"
    if row["Scope1_Δ"] < 0 and row["Scope3_Δ"] < 0:
        return "True Reducer"
    elif row["Scope1_Δ"] >= 0 and row["Scope3_Δ"] < -5:
        return "Offset-Heavy"
    elif row["Production_Δ"] > 0 and row["Scope1_Δ"] >= 0:
        return "Potential Greenwasher"
    else:
        return "Unclassified"

# Filter for 2023 and classify
df_2023 = df_sorted[df_sorted["year"] == 2023].copy()
df_2023["Greenwashing_Risk"] = df_2023.apply(classify_greenwashing, axis=1)
df_2023["Greenwashing_Risk_Cleaned"] = df_2023["Greenwashing_Risk"].replace({
    "Unclassified": "Other",
    "Insufficient Data": "Other"
})

# Select best (preferably non-Other) classification per company
def select_final_classification(group):
    sorted_group = group.sort_values(by="Scope1_Intensity", ascending=False)
    top_row = sorted_group.iloc[0]
    if top_row["Greenwashing_Risk_Cleaned"] == "Other" and len(sorted_group) > 1:
        return sorted_group.iloc[1]
    return top_row

df_2023_final = df_2023.groupby("parent_entity").apply(select_final_classification).reset_index(drop=True)

# Count the final cleaned classification results
risk_counts_2023 = df_2023_final["Greenwashing_Risk_Cleaned"].value_counts()
colors = sns.color_palette('pastel')[0:5]

# Plot pie chart
plt.figure(figsize=(8, 8))
plt.pie(
    risk_counts_2023,
    labels=risk_counts_2023.index,
    autopct='%1.1f%%',
    startangle=90,
    colors=colors,
    wedgeprops={'edgecolor': 'black'}
)
plt.title("Greenwashing Risk Distribution in 2023 (Filtered by Scope1 Intensity)", fontsize=14)
plt.axis('equal')
plt.tight_layout()
plt.show()

# Recalculate greenwashing classification for full dataset
df_sorted["Greenwashing_Risk"] = df_sorted.apply(classify_greenwashing, axis=1)
df_sorted["Greenwashing_Risk_Cleaned"] = df_sorted["Greenwashing_Risk"].replace({
    "Unclassified": "Other",
    "Insufficient Data": "Other"
})

# Filter out rows without classification
df_filtered_cleaned = df_sorted[~df_sorted["Greenwashing_Risk_Cleaned"].isna()]

# Prepare trend data for the last 20 years
risk_trend = df_filtered_cleaned.groupby(["year", "Greenwashing_Risk_Cleaned"])["parent_entity"].nunique().reset_index()
risk_trend.rename(columns={"parent_entity": "Num_Companies"}, inplace=True)
risk_trend_recent = risk_trend[risk_trend["year"] >= 2005]

# Plot the trend
plt.figure(figsize=(12, 6))
sns.lineplot(data=risk_trend_recent, x="year", y="Num_Companies", hue="Greenwashing_Risk_Cleaned", marker="o")
plt.title("Trend of Greenwashing Risk Categories (2005–2023)", fontsize=14)
plt.xlabel("Year")
plt.ylabel("Number of Companies")
plt.legend(title="Risk Category")
plt.tight_layout()
plt.show()
