# Title: Exploratory Data Analysis
# Author: Isabela Yepes
# Date: 10/03/2023
# Data Year: 2015
# Location: South Florida land bounded by lat and lon:
#north: 27
#west: -82
#south: 25
#east: -79

#-----------------------------------------------------------------------------------------------------------
#PACKAGES

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.cm import ScalarMappable

#-----------------------------------------------------------------------------------------------------------
#IMPORT DATA AND STORE
# Create an empty dictionary to store dataframes
dataframes = {}

# List of filenames
csvs = ["01.csv", "02.csv", "03.csv", "04.csv", "05.csv",
             "06.csv", "07.csv", "08.csv", "09.csv", "10.csv", "11.csv",
             "12.csv"]

# Import each CSV file into a pandas dataframe
for csv in csvs:
    # Extract the dataframe name from the filename (remove '.csv' extension)
    month = csv.split(".csv")[0]

    # Read the CSV into a pandas dataframe and store it in the dictionary
    dataframes[month] = pd.read_csv("CSVafterClean/" + csv)
    del month
# 12 dataframes stored in a list called 'dataframes'
# Now 'dataframes' contains your dataframes as pandas dataframes
print(dataframes['02'])

#-----------------------------------------------------------------------------------------------------------
#RANGE AND VARIANCE
# Collect information on range and variance for each variable and each month

# Initialize an empty list to store summary data for each dataframe
summary_data = []

# Iterate through each dataframe
for i, df in enumerate(dataframes, start=1):
    # Calculate the range and variance for each column
    column_stats = {
        'Dataframe': f'DF{i}',  # Create a unique identifier for each dataframe
    }
    
    for column in dataframes[df].columns:
        # Check if the column is numeric
        if pd.api.types.is_numeric_dtype(dataframes[df][column]):
            # Calculate range (max - min)
            column_range = dataframes[df][column].max() - dataframes[df][column].min()
        
            # Calculate variance
            column_variance = dataframes[df][column].var()
        
            # Store the results in the dictionary
            column_stats[f'{column}_Range'] = column_range
            column_stats[f'{column}_Variance'] = column_variance
    
    # Append the summary data for this dataframe to the list
    summary_data.append(column_stats)

# Create the summary dataframe
summary_df = pd.DataFrame(summary_data)

# Display or save the summary dataframe
print(summary_df)
# Save to a CSV file
summary_df.to_csv(f'CSVafterClean/summary_data.csv', index=False)

#-----------------------------------------------------------------------------------------------------------
#LAT & LON
#CHECK if LAT AND LON are the same ACROSS MONTHS
unique_lat_lon = {}  # Dictionary to store unique lat and lon values for each dataframe

# Iterate through the dataframes and store unique lat and lon values in the dictionary
for key, df in dataframes.items():
    unique_lat_lon[key] = (df['lat'].unique(), df['lon'].unique())

# Check if all dataframes have the same unique lat and lon values
first_key = list(unique_lat_lon.keys())[0]
same_unique_lat_lon = all(
    all(unique_lat_lon[first_key][0] == unique_lat_lon[key][0]) and
    all(unique_lat_lon[first_key][1] == unique_lat_lon[key][1])
    for key in unique_lat_lon
)

if same_unique_lat_lon:
    print("All dataframes have the same unique lat and lon values.")
else:
    print("Not all dataframes have the same unique lat and lon values.")

#-----------------------------------------------------------------------------------------------------------
#PLOT LAT & LON & PRCP_TOTAL by month
# Create a scatter plot
data = dataframes
month ='01'
df = data[month]
plt.figure(figsize=(8, 6))  # Adjust the figure size as needed
sns.scatterplot(x='lon', y='lat', data=df, marker='o', s=df['prcp_total']*30)  # Customize marker and size as needed
plt.title(f"Scatter Plot Map of Data")
plt.xlabel("Longitude")
plt.ylabel("Latitude")
plt.grid(True)
    
# Save the plot to a file (optional)
plt.savefig(f"CSVafterClean/map_scatter_plot_month{month}.png", dpi=300, bbox_inches='tight')
    
# Show the plot (optional)
plt.show()

#-----------------------------------------------------------------------------------------------------------
#CHECK DATA TYPES by month
data = dataframes
month ='01'
df = data[month]

print(df.dtypes)

#-----------------------------------------------------------------------------------------------------------
# CORRELATION MATRIX by month

# Create a function to generate correlation plots for each dataframe
def generate_correlation_plot(df, month):
    # Compute the correlation matrix
    corr_matrix = df.corr()

    # Create a heatmap
    plt.figure(figsize=(10, 8))
    sns.heatmap(corr_matrix, annot=True, cmap='coolwarm', fmt=".2f")
    plt.title(f"Correlation Plot for Month {month}")
    plt.show()

    # Save the plot to a file (optional)
    plt.savefig(f"CSVafterClean/correlation_plot_month{month}.png", dpi=300, bbox_inches='tight')

# Assuming 'my_dict' is your dictionary of dataframes
data = dataframes
month ='01'
df = data[month]
generate_correlation_plot(df, month)

#for key, df in dataframes.items():
    # Generate a correlation plot for each dataframe
#    generate_correlation_plot(df, f"Correlation Plot for {key}")

#-----------------------------------------------------------------------------------------------------------
#DISTRIBUTION OF DATA AND FEATURES by month

#SPATIAL DISTRIBUTION

# Create a function to generate scatter plots with color-coded points
def generate_spatial_distribution_plot(data, month, feature_name):
    df = data[month]
    plt.figure(figsize=(10, 8))
    sns.scatterplot(x='lon', y='lat', data=df, hue=feature_name, palette='viridis', s=df['prcp_total']*30)
    plt.title(f"Spatial Distribution of {feature_name} in Month {month}")
    plt.xlabel("Longitude")
    plt.ylabel("Latitude")

    # Add a colorbar for the feature
    sm = ScalarMappable(cmap='viridis', norm=plt.Normalize(vmin=df[feature_name].min(), vmax=df[feature_name].max()))
    sm.set_array([])  # You need to set an empty array
    cbar = plt.colorbar(sm, label=feature_name)

    plt.grid(True)
    plt.show()
    
    # Save the plot to a file (optional)
    plt.savefig(f"CSVafterClean/spatial_plot_{feature_name}_month{month}.png", dpi=300, bbox_inches='tight')

# DATA DISTRIBUTION

# Create a function to generate distribution plots
def generate_distribution_plot(data, month, feature_name):
    df = data[month]
    j = df[feature_name].values.astype(float)
    sns.distplot(j, hist=False, kde=True, kde_kws={'shade': True, 'linewidth': 3}).set(title='Distribution of Month ' + month + ' ' + feature_name)
    plt.show()
        
    # Save the plot to a file (optional)
    plt.savefig(f"CSVafterClean/plot_{feature_name}_month{month}.png", dpi=300, bbox_inches='tight')

# Get an idea of the distribution
data = dataframes
month = "01"
# columns_to_exclude = ['lat', 'lon', 'time', 'prcp_total']
# features = ex.columns[~ex.columns.isin(columns_to_exclude)]
features = ["avg_leaf_area_index_high_vegetation"]

for i in features:
    # Specify the feature you want to visualize spatially
    feature_to_visualize = i  # Change this to the desired feature
    # Generate the spatial distribution plot for the specified feature
    generate_spatial_distribution_plot(data, month, feature_to_visualize)
    # Generate the regular distribution plot for the specified feature
    generate_distribution_plot(data, month, feature_to_visualize)

#-----------------------------------------------------------------------------------------------------------