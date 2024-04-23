# **Edinburgh Census Data Analysis**
This repository contains R scripts developed by Tarig Ali for analyzing Edinburgh census data as part of the final submission for the Visual Analytics course under Professor William Mackaness at the University of Edinburgh, School of Geosciences, Masters of GIS program.

**Repository Structure**
SOM.R: The main script includes data preparation, SOM training, clustering analyses, and generating cartograms and other visualizations.
Datazones/: Shapefiles for the city of Edinburgh, Scotland, with SIMD2016 indicators along with Domain Ranks and Overall Ranks.
Edinburgh_map_clustered/: Shapfiles resulting from the analysis is written here. It has all the information of SIMD, along with BMUs information from SIMD, Clustering information (both K-means and Hierarchical Clustering).
SIMD16 indicator data/: Scotish Index of Multiple Deprivation 2016

**Installation**
Ensure you have R installed on your machine along with the required packages. The script uses several packages, including kohonen for SOM and ggplot2 for plotting.

**Setup**
Copy code
# Clone the repository
git clone https://github.com/tariqabuobeida/SOM.git

# Navigate to the project directory
cd SOM

# Install required R packages
Rscript -e "install.packages(c('kohonen', 'tidyverse', 'cartogram', 'cartogramR', 'RColorBrewer', 'sf', 'tidyr', 'readxl', 'maptools', 'ggplot2', 'rgdal', 'gridExtra', 'grid', 'broom', 'dplyr', 'scales'), dependencies=TRUE)"
Usage
Navigate to the project directory and run the R script.

**Script Overview**
Data Preparation: The script starts by setting the working directory, loading required libraries, and reading input data from Excel and shapefile formats. It processes and normalizes data, handling specific attributes for analysis.

Visualisation: Generates various plots to explore data distributions and relationships. This includes regular maps and attribute-specific visualizations.
Self-Organizing Maps (SOM): Configures and trains an SOM to explore spatial data patterns. Includes visualization of training progression and cluster qualities.

Clustering Analysis: Applies hierarchical and K-means clustering methods to the SOM results. Visualises clusters on the SOM grid.

Cartograms: Generates Dorling and contiguous cartograms to represent data spatially adjusted by population size, enhancing the interpretability of spatial patterns.

Output: The script saves processed data and figures in designated directories and concludes with comprehensive visualizations of the results.

# Contributing
Contributions to this project are welcome. Please fork the repository and submit a pull request with your enhancements.

# License
This project is provided under the MIT License - see the LICENSE file for more details.

# Contact
Tarig Ali - GIS MSc, University of Edinburgh - Email: tarigabuobeida@gmail.com, T.A.A.Ali@sms.ed.ac.uk
