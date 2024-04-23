setwd("C:/Users/tarig/Desktop/Tariq's/Edinburgh/Masters Courses/VA/SOM")

library(kohonen)
library(tidyverse)
library(cartogram)
library(cartogramR)
library(RColorBrewer)
library(sf)
library(tidyr)
library(readxl)
library(maptools)
library(ggplot2)
library(rgdal)
library(gridExtra)
library(grid)
library(broom) 
library(dplyr) 
library(scales) 



##############

## Data Preperation

##############

# read in the SIMD Data
data <- read_excel("SIMD16 indicator data.xlsx", sheet = 3)

# read in the Edinburgh Data Zones
edinburgh_map <- st_read("Datazones/EDINBURGH.shp")

# check what object types we have
class(data)
class(edinburgh_map)

# what are the column names of the dataframe, and of the spatial points dataframe?
names(data)
names(edinburgh_map)

# how many rows do each of the datasets contain?
nrow(data)
nrow(edinburgh_map)

# Replace '*' with '0' in all attribute values across the dataframe
data <- data %>%
  mutate(across(everything(), ~ifelse(.x == "*", 0, .x)))

# Check if any asterisks '*' are still present in the dataframe 'data'
asterisks_exist <- any(sapply(data, function(column) any(grepl("\\*", column))))

# Print the result
if (asterisks_exist) {
  print("Asterisks '*' are present in the data.")
} else {
  print("No asterisks '*' found in the data.")
}
# Check for any NA values in the dataframe
summary(data)
anyNA(data)

# Replace NA with 0 in all numeric columns
data <- data %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

anyNA(data) #Check for NA's again

# Check for NULL values in each column
sapply(data, is.null)

# Convert Crime Rate and Crime count to numerical
data <- data %>%
  mutate(
    crime_count = as.numeric(gsub("[^0-9.]", "", crime_count)), 
    # Remove non-numeric characters and convert to numeric
    crime_rate = as.numeric(gsub("[^0-9.]", "", crime_rate)),     
    # Remove non-numeric characters and convert to numeric
    Attainment = as.numeric(gsub("[^0-9.]", "", Attainment))     
    # Remove non-numeric characters and convert to numeric
  )

# Check for any NA values in the dataframe
summary(data)

# Filter to the city of Edinburgh
edin_data <- data %>%
  filter(Council_area == "City of Edinburgh")

# Normalize the dataframe only the numerical values
edin_norm <- edin_data %>%
  mutate(across(where(is.numeric), scales::rescale))

#Check the data again
summary(edin_norm)

# Write the dataframe 'edin_norm' to a CSV file in the current working directory
write.csv(edin_norm, "edin_norm.csv", row.names = FALSE)

# Select only the numeric columns from 'edin_norm'
numeric_data <- select_if(edin_norm, is.numeric)

# Calculate the correlation matrix to decide on the variables
correlations <- cor(numeric_data, use = "complete.obs")

# Subset the dataframe to include only the selected variables
subset_data <- edin_norm %>%
 select(Data_Zone, CIF, crime_rate,
        overcrowded_count, Employment_rate, DEPRESS, DRUG, Noquals, 
        SMR, nocentralheat_rate)

# Merge the spatial object with data using the shared column "Data_Zone"
merged_data <- merge(edinburgh_map, subset_data, by.x = "DataZone", by.y = "Data_Zone")

names(merged_data)


##############

## Plotting Regular Maps

##############

# Plot Ranks from data
ggplot(data = merged_data, aes(fill = Rank)) +
  geom_sf() +
  scale_fill_viridis_c() +  # Use the Viridis color scale
  theme_minimal() +
  ggtitle("Spatial Distribution of Deprivation Ranks") +
  theme(panel.grid = element_blank(), # Remove grid lines
        plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank())


# Plot for each attribute and save as an object

plots <- list()

# A vector of attributes to loop through and create plots for
attributes <- c("CIF", "crime_rate",
                "overcrowded_count", "Employment_rate", "DEPRESS", 
                "DRUG","Noquals","SMR","nocentralheat_rate")

# Define color palette
color_palette <- scale_fill_viridis_c(option = "D", direction = 1)

# Loop through attributes and create a plot for each
for (attr in attributes) {
  plots[[attr]] <- ggplot(data = merged_data) +
    geom_sf(aes_string(fill = attr), colour = NA, size = 0.1) +
    color_palette +
    labs(fill = attr, title = paste(attr, "in Edinburgh")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(), 
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          axis.text.x = element_blank(),        # Hide x-axis text
          axis.text.y = element_blank(),        # Hide y-axis text
          axis.ticks = element_blank(),         # Hide axis ticks
          axis.title.x = element_blank(),       # Hide x-axis title
          axis.title.y = element_blank())       # Hide y-axis title
}

# Arrange the plots into a grid
grid_plot <- do.call(grid.arrange, c(plots, ncol = 3))


#### Plotting all the selected variables in a map

# Plot for each attribute and save as an object
plots <- list()

# A vector of attributes to loop through and create plots for
attributes <- c("CIF", "crime_rate", "overcrowded_count", 
                "Employment_rate", "DEPRESS", "DRUG", "Noquals",
                "SMR", "nocentralheat_rate")

# Loop through attributes and create a plot for each, but suppress the legends
for (attr in attributes) {
  plots[[attr]] <- ggplot(data = merged_data) +
    geom_sf(aes_string(fill = attr), colour = NA, size = 0.1) +
    scale_fill_viridis_c(option = "C", direction = 1, guide = FALSE) +
    labs(fill = attr, title = paste(attr, "in Edinburgh")) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.5)  # Centering the title
    )
}

# Create one plot specifically for extracting the legend (using 'CIF' as an example)
legend_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = CIF), color = NA, size = 0.1) +
  scale_fill_viridis_c(option = "C", direction = 1) +
  theme_void() +
  theme(legend.position = "bottom")

# Extract the legend
legend <- ggplotGrob(legend_plot)$grobs[[which(sapply
          (ggplotGrob(legend_plot)$grobs, function(x) x$name) == "guide-box")]]

# Arrange the plots into a grid and add the extracted legend at the bottom with increased separation
grid_plot <- grid.arrange(
  grobs = lapply(plots, ggplotGrob),
  layout_matrix = rbind(
    c(1, 2, 3),
    c(4, 5, 6),
    c(7, 8, 9),
    c(NA, NA, NA)  # Dedicated space for the legend
  ),
  heights = c(1, 1, 1, 0.2)  # Increased the last value for more space for the legend
)

# Draw the grid plot
grid.draw(grid_plot)

# Set the viewport to position the legend further down
pushViewport(viewport(x = 0.5, y = 0.03, width = 0.8, height = 0.1))  # Adjusted 'y' to lower value
grid.draw(legend)
popViewport()




##############

## Self Organising Maps

##############


# Convert to a regular data frame and select specifically the selected vars
data_train <- as.data.frame(merged_data) %>%
  dplyr::select(c("DataZone","CIF", 
  "crime_rate",
  "Employment_rate", "DEPRESS", "DRUG", "overcrowded_count","Noquals","SMR",
  "nocentralheat_rate"))
# Keep DataZone in a separate vector or add as a column in the dataframe but ensure it is not in the matrix used for training
data_train_ids <- data_train$DataZone

# Remove DataZone from the data used for training
data_train_for_som <- data_train %>%
  select(-DataZone)


# Standardize the data and convert to a matrix for SOM training
data_train_matrix <- as.matrix(data_train_for_som)

# keep the column names of data_train as names in our new matrix 
names(data_train_matrix) <- names(data_train)

# define the size and topology of the som grid using Vesanto’s Rule
grid_size <- round(sqrt(5 * sqrt(597)))  #equals 11
som_grid <- somgrid(xdim = grid_size, ydim = grid_size, topo="hexagonal")

# Train the SOM model!
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=1000, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE )

# Plot of the training progress - how the node distances have stabilised over time.
# mean distance to closes codebook vector during training
plot(som_model, type = "changes")

# load custom colour palettes
source('coolBlueHotRed.R')
# Define a new color function using the custom palette suitable for codes SOM plot
paired_pal <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33",
                "#A65628","#F781BF","#999999")

pairedPalette <- function(n, alpha = 1) {
  # Ensure that your palette is long enough by repeating it
  # This part repeats the palette to ensure there are at least 'n' colors
  extended_palette <- rep(paired_pal, ceiling(n / length(paired_pal)))
  # Return the first 'n' colors of the extended palette
  return(extended_palette[1:n])
}

# Set up the layout for a grid of plots
par(mfrow = c(1, 3)) # One row, three columns

# Plot counts within nodes
plot(som_model, type = "counts", main="Node Counts", 
     palette.name=coolBlueHotRed, border="grey", shape="straight")

# Plot map quality
plot(som_model, type = "quality", main="Node Quality/Distance", 
     palette.name=coolBlueHotRed, border="grey", shape="straight")

# Plot neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM Neighbour Distances", 
     palette.name=coolBlueHotRed, border="grey", shape="straight")

# Reset the plotting layout to default
par(mfrow = c(1, 1))


# code spread
plot(som_model, type = "codes", shape = "straight", border = "grey", 
     palette.name=pairedPalette)

# Plot the heatmap for a variable at scaled / normalised values
var <- 1 #define the variable to plot
plot(som_model, type = "property", property = getCodes(som_model)[,var], 
     main=colnames(getCodes(som_model))[var], 
     palette.name=coolBlueHotRed,border="grey",shape="straight")

##plot all the component planes into the same image
par(mfrow = c(3,3)) # 3 x 3 grid
for (i in 1:9) { # loop through all of them and plot
  plot(som_model, type = "property", property = getCodes(som_model)[,i],
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed, 
       shape="straight", border="grey")
}
dev.off()


##############

## Hierarchical Clustering

##############


# show the WCSS metric for kmeans to use it with Elbow-Rule .
# Can be used as a "rough" indicator of the ideal number of clusters
mydata <- getCodes(som_model)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)


plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", 
     main="Within cluster sum of squares (WCSS)")

# Form clusters on grid
# use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 5)

# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
                    '#8c564b', '#e377c2')

# show the same plot with the codes instead of just colours
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster],
     palette.name=pairedPalette, main = "Hierarchical Clusters", 
     shape="straight", border="grey")
add.cluster.boundaries(som_model, som_cluster)
# Get the coordinates for plot labels
som_coords <- som_model$grid$pts
# Adding a legend for the clusters numbers
legend("topright", legend = 1:5, fill = pretty_palette[1:5], title = "Cluster")

# create a label set for our SOM - a random subset of EDNAME
# Get names from the Intermedia column
geog_names <- merged_data$Intermedia

# most EDNAME values are repeated, so we'll remove duplicates, just to get an idea of the spread across Dublin (although you may be interested in how different areas under the same name differ)
geog_names[duplicated(geog_names)] <- NA

# find the index of the names which are not NA
naset <- which(!is.na(geog_names))

# make all but 10 of the placenames in our data NA
naset <- sample(naset, length(naset)-15)
geog_names[naset] <- NA

# Replot our data with labels=geog_names					  
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], 
     main = "Hierarchical Clusters",labels=geog_names, shape="straight", border="grey")
add.cluster.boundaries(som_model, som_cluster)
# Adding a legend for the clusters numbers
legend("right", legend = 1:5, fill = pretty_palette[1:5], title = "Cluster")


## SOM Clustering / using BMUs Best Matching Unit
# Assuming 'som_model' is your trained SOM model
bmus <- som_model$unit.classif  # Extracting the BMU indices
mapped_data <- data.frame(DataZone = data_train_ids, BMU = bmus)
##


##############

## K-Means Clustering

##############


# Extract codebook vectors
codebook_vectors <- getCodes(som_model)

# Perform k-means clustering on the codebook vectors
set.seed(123)  # Set seed for reproducibility
kmeans_result <- kmeans(codebook_vectors, centers = 5)  # Specify the number of clusters
# Save the cluster assignments
kmeans_clusters <- kmeans_result$cluster
# Adjust the color palette to match the number of k-means clusters
color_palette <- brewer.pal(5, "Set3")  # Adjust the number to match 'centers' in k-means
# Plot the SOM grid with k-means cluster assignments
plot(som_model, type = "codes", bgcol = pretty_palette[kmeans_clusters],
     main = "SOM Grid with K-means Clustering", shape = "straight", 
     palette.name=pairedPalette,border = "grey")
# Optionally, add cluster boundaries if needed
add.cluster.boundaries(som_model, kmeans_clusters)
# Adding a legend for the clusters numbers
legend("right", legend = 1:5, fill = pretty_palette[1:5], title = "Cluster")


######
##############

## PCA

##############



###PCA (To explore the variables and examine selections for varience- 
### It was later cancelled)



# Preparing dataset with all the variables (26) for PCA 
pre_pca <- edin_norm %>%
  select(Income_rate,Employment_rate, CIF, ALCOHOL, DRUG,
         SMR, DEPRESS, LBWT, EMERG, Attendance,
         Attainment, Noquals, NEET, HESA,
         drive_petrol, drive_GP, drive_PO,drive_primary,
         drive_retail,drive_secondary, PT_GP, PT_Post,
         PT_retail, crime_rate,overcrowded_rate,
         nocentralheat_rate)


pca_result <- prcomp(pre_pca, center = TRUE, scale. = TRUE)

# Summary of PCA results
summary(pca_result)

# Plot the variance explained by each principal component
plot(pca_result, type = "l", main = "PCA Variance")
# Plotting PCA Scores
plot(pca_result$x[,1], pca_result$x[,2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Scores")
text(pca_result$x[,1], pca_result$x[,2], 
     labels = row.names(pre_pca), pos = 4)

# Plotting Loadings
plot(pca_result$rotation[,1], pca_result$rotation[,2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Loadings")
text(pca_result$rotation[,1], pca_result$rotation[,2], 
     labels = names(pre_pca), pos = 4)

# PCA
pca_scores <- pca_result$x[, 1:2]
set.seed(123)  # Set seed for reproducibility
kmeans_result2 <- kmeans(pca_scores, centers = 5)  # cluster number


# Plot the PCA scores and color points by their cluster assignment
plot(pca_scores[,1], pca_scores[,2], col=pretty_palette[kmeans_result2$cluster], 
     xlab="Principal Component 1", ylab="Principal Component 2", 
     main="PCA Scores Colored by k-means Clustering")
legend("topright", legend=paste("Cluster", unique(kmeans_result2$cluster)), 
       fill=pretty_palette[1:5], title="Cluster")

# adding centroids to the plot
centroids <- kmeans_result2$centers
points(centroids[,1], centroids[,2], pch=8, cex=2, col=1:length(centroids))





###################

## Extracting Clustering details and Joining to dataframes and SFs

##############



### Extract Hierarchical Cluster Details from the model
cluster_details <- data.frame(id = merged_data$DataZone, hier_cluster = 
                                som_cluster[som_model$unit.classif])
# Get the BMU for each data point in the original training set
bmu_indices <- som_model$unit.classif

# Map k-means clusters to each data point based on BMU
data_train_for_som$Cluster_KMeans <- kmeans_clusters[bmu_indices]

# Add DataZone back from 'data_train_ids' if it was separated earlier
data_train_for_som$DataZone <- data_train_ids


# combine the census and cluster data onto our original spatial polygons edinburgh_map
# Merge k-means cluster assignments back to the merged spatial data

edinburgh_map_clust <- merge(merged_data, data_train_for_som[, c("DataZone", 
                            "Cluster_KMeans")], by = "DataZone", all.x = TRUE)
edinburgh_map_clust1 <- merge(edinburgh_map_clust, 
                              mapped_data, by.x="DataZone", by.y="DataZone")
edinburgh_map_clust2 <- merge(edinburgh_map_clust1, cluster_details, 
                              by.x="DataZone", by.y="id")
edinburgh_map_fin <- merge(edinburgh_map_clust2, edin_data[c("Data_Zone",
      "Total_population")],by.x = "DataZone", by.y = "Data_Zone", all.x = TRUE)

# Convert sf object to a regular data frame by dropping geometry
df <- as.data.frame(edinburgh_map_fin)

# Now write the data frame to a CSV file
write.csv(df, 'edinburgh_map_fin.csv', row.names = FALSE)

# Write the merged sf with clusters to a Shapefile
dir.create("Edinburgh_map_clustered")
# Write the sf object to a Shapefile
st_write(obj = edinburgh_map_fin, dsn = "Edinburgh_map_clustered/Edinburgh_map_clustered.shp")



##############

#### Cluster Profiling: Grouping observations by cluster number and mean variables

##############

# Load the data from CSV file
edinburgh_data <- read.csv("edinburgh_map_fin.csv")

# Calculate means of variables by cluster using the updated across() syntax
cluster_means <- edinburgh_data %>%
  group_by(Clst_KM) %>%
  summarise(across(c(CIF, crime_rate, Employment_rate, DEPRESS, DRUG, 
                     overcrowded_count, Noquals, SMR, nocentralheat_rate), 
                   ~ mean(., na.rm = TRUE)))  # Using the anonymous function syntax

# Reshape data for plotting
long_cluster_means <- cluster_means %>%
  pivot_longer(-Clst_KM, names_to = "Variable", values_to = "Mean")

# Plot with centered title
ggplot(long_cluster_means, aes(x = Clst_KM, y = Mean, color = Variable)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Cluster Profiles by Mean Variable Values",
       x = "Cluster Number",
       y = "Normalized Mean Value") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))  # Centering the title


######



##############

## Plotting Maps based on clustering details

##############


# Plot the areas and color by cluster with Viridis color palette
ggplot(data = edinburgh_map_fin) + 
  geom_sf(aes(fill = factor(hier_cluster)), color = "black", size = 0.2) + 
  scale_fill_viridis_d() +  # Use the Viridis color scale
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank())  # Remove axis titles

# Plot for Hierarchical Clustering
p1 <- ggplot(edinburgh_map_fin) +
  geom_sf(aes(fill = as.factor(hier_cluster)), colour = NA) +  # Remove borders
  scale_fill_viridis_d(guide = guide_legend(title = "Hierarchical Clusters")) +
  labs(title = "Hierarchical Clustering") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_rect(fill = "white", colour = NA),  
        # White background without border
        axis.title = element_blank(),  # Remove axis titles
        axis.text = element_blank(),  # Remove axis text
        axis.ticks = element_blank())

# Plot for K-Means Clustering
p2 <- ggplot(edinburgh_map_fin) +
  geom_sf(aes(fill = as.factor(Cluster_KMeans)), colour = NA) +  # Remove borders
  scale_fill_viridis_d(guide = guide_legend(title = "K-Means Clusters")) +
  labs(title = "K-Means Clustering") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(),  # Remove major grid lines
  panel.grid.minor = element_blank(),  # Remove minor grid lines
  panel.background = element_rect(fill = "white", colour = NA),  
  # White background without border
  axis.title = element_blank(),  # Remove axis titles
  axis.text = element_blank(),  # Remove axis text
  axis.ticks = element_blank() )

# Arrange both plots side by side
grid.arrange(p1, p2, ncol = 2)


##############

## Plotting the side-maps (lowest quartile of each used domain Rank):

##############


edinburgh_sf <- st_read("shapefile_plotting/SG_SIMD_2016_EDINBURGH.shp")

# Normalize the attribute Want to plot (now it's HouseRank)
edinburgh_sf$HouseRank_norm <- (edinburgh_sf$HouseRank - 
                                  min(edinburgh_sf$HouseRank, na.rm = TRUE)) /
  (max(edinburgh_sf$HouseRank, na.rm = TRUE) - 
     min(edinburgh_sf$HouseRank, na.rm = TRUE))

# Calculate the threshold for the lowest quartile of HouseRank
emp_rank_quartile <- quantile(edinburgh_sf$HouseRank_norm, 0.25, na.rm = TRUE)

# Create a color column for HouseRank, using grey for the highest quartile
edinburgh_sf$HouseRank_color <- ifelse(edinburgh_sf$HouseRank_norm <= 
                                         emp_rank_quartile, "darkblue", "grey")

# Plot the map for HouseRank with polygon boundaries
ggplot(data = edinburgh_sf) +
  geom_sf(aes(fill = HouseRank_color), color = "black") +  
  # Add black color for polygon boundaries
  scale_fill_identity() +  
  # Use the exact colors specified in the HouseRank_color column
  labs(title = "Housing") +
  theme_void() +  # Remove background, gridlines, and text
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        legend.position = "none")  # Remove the legend





####################
## The beautiful Cartograms <3
####################


# Load the shapefile
edinburgh_map_clustered <- st_read("Edinburgh_map_clustered.shp")

# Joining the Total Population (to size based on it)
edinburgh_map_fin <- merge(edinburgh_map_clustered, 
                           edin_data[c("Data_Zone", "Total_population")],
                           by.x = "DataZon", by.y = "Data_Zone", all.x = TRUE)


# Create cartogram based on "Population"
edinburgh_map_fin_cartogram <- cartogram_cont(edinburgh_map_fin, 
                                              "Total_population", itermax = 10)

# Define the pretty color palette
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd')

# Add cluster information for coloring if not already present
#hier_cartogram$cluster <- edinburgh_map_fin$hr_clst
#hier_cartogram$Cluster_KMeans <- edinburgh_map_fin$Clst_KM

# Plotting the cartogram with centered title and subtitle
ggplot(data = edinburgh_map_fin_cartogram) +  
  # Ensure the correct data object is used here
  geom_sf(aes(fill = factor(hr_clst), geometry = geometry), color = "white", 
          size = 0.1) +
  scale_fill_manual(values = pretty_palette, name = "Cluster Number", 
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3", 
                               "Cluster 4", "Cluster 5")) +
  labs(title = "Edinburgh Deprived Populations Clusters", 
       subtitle = "Size by Population, Color by Cluster Number") +
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid.major = element_blank(),  # Explicitly remove major grid lines
        panel.grid.minor = element_blank(),  # Explicitly remove minor grid lines
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),  # Center the title
        plot.subtitle = element_text(hjust = 0.5))  # Center the subtitle



###########

# Dorling cartograms/ beautiful but proved not efficient (too many obsevations)

###########

# Check if 'edinburgh_map_clust2' has the right structure
if (!inherits(edinburgh_map_clust2, "sf")) {
  edinburgh_map_clust2 <- st_as_sf(edinburgh_map_clust2)
}

# Creating a Dorling cartogram
dorling_cartogram <- cartogram::cartogram_dorling(
  edinburgh_map_clust2, weight = 'Rank', k = 5,
  m_weight = 1, itermax = 1000
)

# Add cluster information for coloring (assuming 'cluster' and 'Cluster_KMeans' are your clustering results)
dorling_cartogram$cluster <- edinburgh_map_clust2$cluster
dorling_cartogram$Cluster_KMeans <- edinburgh_map_clust2$Cluster_KMeans
# Plot Dorling cartogram for Hierarchical Clustering
p1 <- ggplot(dorling_cartogram) +
  geom_sf(aes(fill = as.factor(hier_cluster)), size = 0.1, color = NA) +
  scale_fill_viridis_d() +
  labs(title = "Dorling Cartogram: Hierarchical Clustering") +
  theme_void() +
  theme(legend.position = "bottom")

# Plot Dorling cartogram for K-Means Clustering
p2 <- ggplot(dorling_cartogram) +
  geom_sf(aes(fill = as.factor(Cluster_KMeans)), size = 0.1, color = NA) +
  scale_fill_viridis_d() +
  labs(title = "Dorling Cartogram: K-Means Clustering") +
  theme_void() +
  theme(legend.position = "bottom")

# Arrange both plots side by side
gridExtra::grid.arrange(p1, p2, ncol = 2)

#####



#####
##### The End :)