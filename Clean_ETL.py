import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Create an empty dictionary to store dataframes
dataframes = {}

# List of filenames
csv_files = ["2015_01.csv", "2015_02.csv", "2015_03.csv", "2015_04.csv", "2015_05.csv",
             "2015_06.csv", "2015_07.csv", "2015_08.csv", "2015_09.csv", "2015_10.csv", "2015_11.csv",
             "2015_12.csv"]

# Import each CSV file into a pandas dataframe
for csv_file in csv_files:
    # Extract the dataframe name from the filename (remove '.csv' extension)
    month = csv_file.split(".csv")[0].split("_")[1]

    # Read the CSV into a pandas dataframe and store it in the dictionary
    dataframes[month] = pd.read_csv("CSVbeforeClean/" + csv_file)
    # Remove the unnamed column (index column)
    df = dataframes[month]
    df = df.drop(df.columns[0], axis=1)  # The unnamed column is the first column
    # Replace Inf and -Inf with NaN
    df.replace([np.inf, -np.inf], np.nan, inplace=True)
    dataframes[month] = df
    del month

# Now 'dataframes' contains your dataframes as pandas dataframes

#Check Spatial distribution of NaNs
def spNaN(month, variable, data):
    df = data[month]
    # Get the latitude and longitude of NaN values
    nan_lat = df[['lat','lon']][df[variable].isna()]
    # Create a scatter plot
    plt.figure(figsize=(8, 6))  # Adjust the figure size as needed
    sns.scatterplot(x='lon', y='lat', data=nan_lat, marker='o', s=10)  # Customize marker and size as needed
    plt.title(f"Scatter Plot Map of NaN data for Month {month}'s {variable}")
    plt.xlabel("Longitude")
    plt.ylabel("Latitude")
    plt.grid(True)
    # Save the plot to a file (optional)
    plt.savefig(f"CSVafterClean/NaN_{month}_{variable}.png", dpi=300, bbox_inches='tight')
    # Show the plot (optional)
    #return plt.show()
#check
spNaN('01','avg_2m_temperature', dataframes) #11532 NaN total
spNaN('04','avg_10m_v_component_of_wind', dataframes) #11160 NaN total
spNaN('02','avg_surface_pressure', dataframes) #10416 NaN total
spNaN('01','prcp_total', dataframes) # 0 NaN total


#Count NaNs
def countNaN(data,title):
    # Initialize an empty dataframe to store the counts
    nan_counts_df = pd.DataFrame(columns=['Month', 'Variable', 'NaN_Count'])
    # data is the dictionary of dataframes
    for key, df in data.items():
        # Iterate through each column in the dataframe
        for column in df.columns:
            # Count the number of NaN values in the column
            nan_count = df[column].isna().sum()
            # Append the results to the nan_counts_df
            nan_counts_df = nan_counts_df.append({'Month': key, 'Variable': column, 'NaN_Count': nan_count
            }, ignore_index=True)
    # Calculate the sum of NaN counts in the 'NaN_Count' column
    total_nan_count = nan_counts_df['NaN_Count'].sum()
    # Print the total sum of NaN counts
    print(f"{total_nan_count} NaN in total.")
    # Save the nan_counts_df to a CSV file
    nan_counts_df.to_csv(f'CSVafterClean/nan_counts_{title}.csv', index=False)
    #reutrn nan_counts_df for map
    return nan_counts_df

def removeNaN(data):
    # dataframes = dictionary of dataframes
    for key, df in data.items():
        # Replace Inf and -Inf with NaN
        df.replace([np.inf, -np.inf], np.nan, inplace=True)
        # Drop rows where all columns (after the first 3) are NaN
        df.dropna(subset=df.columns[3:], how="all", inplace=True)
        data[key] = df
        # Save the modified dataframe to a CSV file
        df.to_csv(f'CSVafterClean/{key}.csv', index=False)
    # Now 'dataframes' contains your dataframes as pandas dataframes
    return data

#Save CSV of count NaNs before removed and map NaN values
countNaN(dataframes, "pre")

#Count NaNs after removed
dataframes = removeNaN(dataframes)
countNaN(dataframes,"post")
