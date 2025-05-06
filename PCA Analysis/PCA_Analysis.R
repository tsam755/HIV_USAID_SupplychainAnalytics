# Dimensionality Reduction: PCA ----

# Load required libraries. Install if missing.
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

if (!require("stringi")) install.packages("stringi")
library(stringi)

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

# Step 1: Load and Explore the Dataset ----
df_HIVSC_csv <- read.csv("Final_Merged_CSV.csv")
head(df_HIVSC_csv) # Display the first few rows of the dataset
summary(df_HIVSC_csv) # Get a summary of the dataset to understand its structure and stats

unique(df_HIVSC_csv$Location) # Check for unique countries in the "Location" column

# Step 2: Handle Data Issues (Encoding and Outliers) ----

# Fix encoding issues in the "Location" column (e.g., Côte d'Ivoire)
df_HIVSC_csv$Location <- stri_trans_general(df_HIVSC_csv$Location, "Latin-ASCII")

# Replace "Côte d'Ivoire" with "Cote d'Ivoire" in the 'country' column
df_HIVSC_csv$Location <- gsub("C�te d'Ivoire", "Cote d'Ivoire", 
                              df_HIVSC_csv$Location)

# Verify the replacement
unique(df_HIVSC_csv$Location)

# Investigate South Africa as a potential outlier
south_africa_data <- df_HIVSC_csv[df_HIVSC_csv$Location == "South Africa", ]
summary(south_africa_data)

# Compare key metrics for South Africa with other countries
comparison <- df_HIVSC_csv %>%
  group_by(Location) %>%
  summarise(across(c(Deaths, Prevalence, Incidence, Total.Medicine.Shipment.Vol), mean, na.rm = TRUE)) %>%
  arrange(desc(Total.Medicine.Shipment.Vol))

print(comparison)

# Visualize Total Medicine Shipment Volume for all countries
ggplot(comparison, aes(x = reorder(Location, Total.Medicine.Shipment.Vol), 
                       y = Total.Medicine.Shipment.Vol)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Medicine Shipment Volume by Country", x = "Country", 
       y = "Shipment Volume") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Additional Boxplot: Compare Deaths Across Countries
ggplot(df_HIVSC_csv, aes(x = Location, y = Deaths)) +
  geom_boxplot(outlier.color = "red", fill = "lightblue") +
  labs(title = "Distribution of Deaths Across Countries", 
       x = "Country", y = "Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# South Africa exhibits disproportionately high values across key metrics, such
# as deaths, prevalence, and incidence, compared to other countries.
# These extreme values dominate the variance structure, biasing the PCA results
# and reducing its relevance for smaller countries.
# Visualizations and summary statistics confirm this outlier behavior.
# To ensure the PCA reflects the broader dataset trends, South Africa is
# excluded, as it warrants its own dedicated analysis.

# Remove South Africa due to its outlier behavior (skews PCA results)
df_HIVSC_filtered <- df_HIVSC_csv[df_HIVSC_csv$Location != "South Africa", ]

# Verify removal
unique(df_HIVSC_filtered$Location)

# Step 3: Preprocessing - Select Relevant Variables and Standardize ----

# Remove non-numeric columns and irrelevant variables
df_HIVSC <- subset(df_HIVSC_filtered, select = -c(Location, Year, 
                                                  Total.Shipment.Value, 
                                                  Business.impact.of.HIV_AIDS, 
                                                  DeliveryStatus_On.Time,
                                                  DeliveryStatus_Late))


## Pre processing: Standardize the Data
df_HIVSC_std <- as.data.frame(scale(df_HIVSC)) #standardize the data
head(df_HIVSC_std) # Show top 6 records

# Identify columns with missing or infinite values
colSums(is.na(df_HIVSC_std))
colSums(is.infinite(as.matrix(df_HIVSC_std)))

# Step 4: Perform PCA ----

## Perform PCA to transform variables to principal components
pca <- prcomp(df_HIVSC_std, center = TRUE, scale. = TRUE)

## Summary of PCA result
summary(pca)
#sd - std deviation / spread of the data along each principal component
#proportion of variance - % of variance explain by a single PC
#cumulative prop - adds up prop of variance for each PC cumulatively


## Extract proportion of variance explained by each principal component (PC)
explained_variance <- summary(pca)$importance[2,]
barplot(explained_variance, main = "Variance Explained by Each PC")

## We can see that variance spread across the 7 variables is captured mostly by the first two principal components (75%)

# Step 5: Visualize PCA Results ----

## Extract the first two PCs
df_HIVSC_pc_2 <- data.frame(pca$x[, 1:2])
head(df_HIVSC_pc_2) # Show top 6 records

## Scatter plot of PC1 and PC2 (uncolored)
ggplot(df_HIVSC_pc_2, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(title = "Scatter Plot")

## Scatter plot of PC1 and PC2 colored by location
df_HIVSC_pc_2$Location <- df_HIVSC_filtered$Location # Add original label
head(df_HIVSC_pc_2) # Show top 6 records
ggplot(df_HIVSC_pc_2, aes(x = PC1, y = PC2, color = Location)) +
  geom_point() +
  labs(title = "Scatter Plot")

# Step 6: Analyze Variable Contributions ----

## Determine the contribution of each variable to each PC
pca$rotation

## Check this for the first record
df_HIVSC_std[1, ] # Show original data for the first record (before PCA and standardization).
head(df_HIVSC_pc_2)[1, ] # show transformed PCs
rowSums(pca$rotation[, 1] * df_HIVSC_std[1, ]) # Compute PC1
rowSums(pca$rotation[, 2] * df_HIVSC_std[1, ]) # Compute PC2

## Correlation matrix before standardization
print(round(cor(df_HIVSC), 3))
## Correlation matrix after standardization
print(round(cor(df_HIVSC_std), 3))
## Correlation matrix after PCA (should be uncorrelated)
print(round(cor(data.frame(pca$x)), 3))
## We can confirm that PCA achieved zero correlation between PCs!

# Step 7: Generate Biplot for Interpretation ----

## Plotting the PCA for interpretation
biplot(pca)

# Step 8: Save Results for Power BI ----

# Export the first two PCs to a CSV file for use in Power BI
write.csv(df_HIVSC_pc_2, "PCA.csv", row.names = FALSE)
