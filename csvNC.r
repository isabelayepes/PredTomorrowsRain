# AUTHOR: Isabela Yepes
# DATE: 9/09/2023
# PURPOSE: CONVERT NETCDF FILES TO CSV
# CHECKED & REVISED BY: 

start <- Sys.time()
#clear workspace
rm(list=ls())
#install packages
library(tidyverse)
library(ncdf4)
library(reshape2)
library(ggplot2)
library(glue)
#set working directory & location of ncdf file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Data time variables
mnths <- sprintf('%02d', 1:12)
years = 2015

vars <-c('2m_temperature', 'total_precipitation','10m_u_component_of_wind','10m_v_component_of_wind','lake_shape_factor','leaf_area_index_low_vegetation','leaf_area_index_high_vegetation','surface_pressure')

#Creates an empty data frame da
columns<-c("variable","year","month","nrow","nrowAv","nrowNA")
da <-data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(da) = columns
print(da)

#Function purpose: extract df for each variable from each net cdf file
extract_df_from_nc <- function(nc_file, var) {
  nc <- nc_file %>% #this is the pipe operator, ctrl+shift+m
    nc_open()
  
  # this if statement is used to extract variables
  if (var=='2m_temperature') {
    df <- ncvar_get(nc = nc, varid = 't2m')
  } else if (var=='total_precipitation'){
    df <- ncvar_get(nc = nc, varid = 'tp')
  } else if (var == '10m_u_component_of_wind') {
    df <- ncvar_get(nc = nc, varid = 'u10')
  } else if (var == '10m_v_component_of_wind') {
    df <- ncvar_get(nc = nc, varid = 'v10')
  } else if (var == 'lake_shape_factor') {
    df <- ncvar_get(nc = nc, varid = 'lshf')
  } else if (var == 'leaf_area_index_low_vegetation') {
    df <- ncvar_get(nc = nc, varid = 'lai_lv')
  } else if (var == 'leaf_area_index_high_vegetation'){
    df <- ncvar_get(nc = nc, varid = 'lai_hv')
  } else if (var == 'surface_pressure') {
    df <- ncvar_get(nc = nc, varid = 'sp')
  } else {return("error no variable ID")}
  # you can convert matrix to dataframe using melt function from reshape2
  df <- df %>% 
    melt() %>% 
    tibble()  #convert to tibble for better output format in console
  
  ncatt_get(nc,'time')
  time <- tibble(
    time_index = 1:nc$dim$time$len,
    time =   c(as.POSIXct(nc$dim$time$vals*3600, origin = '1900-01-01', tz = 'GMT'))
  )
  
  lat <- tibble(
    lat_index = 1:nc$dim$lat$len,
    lat = c(round(nc$dim$lat$vals,1))
  )
  
  lon <- tibble(
    lon_index = 1:nc$dim$lon$len,
    lon = c(round(nc$dim$lon$vals,1))
  )
  df <- df %>% 
    # give appropriate names to set up for join
    rename(lon_index = Var1, 
           lat_index = Var2,
           time_index = Var3) %>% 
    # join to previously created dataframes
    left_join(time, by = 'time_index') %>% 
    left_join(lat, by = 'lat_index') %>% 
    left_join(lon, by = 'lon_index') %>% 
    # drop index columns
    select(-c(lat_index, lon_index, time_index))
  # always close out nc when done, or else it will clog up space in memory
  nc_close(nc)
  
  return(df)
}

# Creates an empty list to store the dataframes
dtf <- list()

for (year in years) {
  for (mnth in mnths) {
    for (var in vars) {
      nc_file <- glue("DATA/ERA5_{var}_{year}_{mnth}.nc")
      # dtf[glue("{var}_{year}_{mnth}")] <- extract_df_from_nc(nc_file, var)
      # print (glue('iteration for {var} with {mnth} month in {year} successful'))
      
      # Extract df
    
      # if total precip needs a time series conversion because the cumulative is stored as the first entry of the next month
      if(var == 'total_precipitation') {
        df_cur <- extract_df_from_nc(nc_file, var)
        next_dataset <- ym(paste0(year,mnth)) + months(1)
        next_year <- year(next_dataset)
        next_mnth <- sprintf('%02d', month(next_dataset))
        next_nc_file <- glue("DATA/ERA5_{var}_{next_year}_{next_mnth}.nc")
        # check if we have another month of data, if so, grab first timestamp
        if (file.exists(next_nc_file)) {
          df_next <- extract_df_from_nc(next_nc_file, var) %>%
            filter(time == time[1])
          df <- rbind(df_cur, df_next)
          # if we don't have another month, just use current month
        } else {
          df <- df_cur
        }
        # Daily Time Series conversion for precip
        df <- df %>%
          # shift timeseries so 0 hour for next day is included as previous
          mutate(time = as.Date(time - hours(1))) %>%
          # exclude first timestamp (which will be mapped to previous day)
          filter(time != min(time)) %>%
          group_by(lat,lon,time)%>%
          # take max value from cumulative timeseries
          summarize(prcp_total = max(value,na.rm = T)*1000) #Multiplied by 1000 to convert prcp total unit from meters to millimeters
        
        #if temp needs a temp conversion (from Kelvin to Celsius) and hourly time steps converted to daily avg, min, and max
        } else if (var == "2m_temperature") {
        df <- extract_df_from_nc(nc_file, var)
        # 1. Convert temperatures to deg C (see tidy mutate function)
        df <- df %>%
          mutate(cels = value-273.15,
                 time = as.Date(time)) %>%
          group_by(lat,lon,time)%>%
          select(-c(value))
        # 2. Calculate average, max, and min temperature by day and grid cell for the entire year
        # (see tidy functions group_by and summarize, check out as.Date function to convert to Date)
        df <- df %>%
          summarize(
            !!paste("avg", var, sep = "_") := mean(cels, na.rm = TRUE),
            !!paste("max", var, sep = "_") := max(cels, na.rm = TRUE),
            !!paste("min", var, sep = "_") := min(cels, na.rm = TRUE)
          )
        
        #All others still need extraction and hourly time steps converted to daily avg, min, and max
      } else {
        df <- extract_df_from_nc(nc_file, var)
        df <- df %>% mutate(time = as.Date(time)) %>%
          group_by(lat,lon,time) %>% 
          summarize(
            !!paste("avg", var, sep = "_") := mean(value, na.rm = TRUE),
            !!paste("max", var, sep = "_") := max(value, na.rm = TRUE),
            !!paste("min", var, sep = "_") := min(value, na.rm = TRUE)
          )
      }
      
      #saves df to dtf list
      dtf[[paste(var, year, mnth, sep = "_")]] <- df
      
      }
  }
}

Sys.time() - start

start <- Sys.time()

my_data <- list()

for (i in seq_along(dtf)) {
  for (j in 1:12) {
    if (grepl(paste("_",sprintf("%02d", j),sep = ""), names(dtf)[i])) {
      #name variable
      name <- paste("2015",sprintf("%02d", j),sep="_")
    }
  }
  # Check if name already exists in my_data
  if (!name %in% names(my_data)) {
    # If it doesn't exist, appends dtf[[i]]
    my_data[[name]] <- dtf[[i]]
  } else {
    # Left_join the df
    my_data[[name]] <- left_join(my_data[[name]], dtf[[i]], by = c("lat", "lon","time"))
  }
}

#Combine all

c_df <- data.frame()

for (name in names(my_data)) {
  c_df <- bind_rows(c_df, my_data[[name]])
}

Sys.time() - start

#Convert to CSVs

#individual year_month csvs
for (name in names(my_data)) {
  # Export each dataframe to a CSV file
  write.csv(my_data[[name]], file = paste("CSVbeforeClean/",name, ".csv", sep = ""))
}

#Combined csv
write.csv(c_df, file = "CSVbeforeClean/2015_all.csv")



