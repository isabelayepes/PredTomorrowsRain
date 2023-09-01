# AUTHOR: Isabela Yepes
# DATE: 3/16/23
# PURPOSE: PLOT NETCDF OUTPUT
# CHECKED & REVISED BY: JOSH SOPER

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

# indus river basin cmip6 points
cmip6 <- '../CSVin/CMIP6_pts_indus_clip.csv' %>% 
  read_csv() %>% 
  rename(CMIPLat=Lat,
         CMIPLon=Lon)

# era5 cmip6 associations from rachel, inner join to grab only era5 points mapped
# within our watershed
# rename lat/lon to match naming below
era5_pts <- '../CSVin/ERA5_CMIP6_associations.csv' %>% 
  read_csv() %>% 
  inner_join(cmip6, by = c('CMIPLat', 'CMIPLon')) %>% 
  rename(lat = ERA5Lat, lon = ERA5Lon)

# check if distinct lat/lon pairs = 1280 (number of pts in our watershed)
era5_pts %>% 
  select(CMIPLat, CMIPLon) %>% 
  distinct() %>% 
  nrow()

mnths <- sprintf('%02d', 1:12)
years = seq(from = 1989, to = 2014, by = 1)

start <- Sys.time()
vars <-c('2m_temperature', 'total_precipitation')

#Before for loop creates an empty data frame da
columns<-c("variable","year","month","nrow","nrowAv","nrowNA")
da <-data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(da) = columns
print(da)

extract_df_from_nc <- function(nc_file, var) {
  nc <- nc_file %>% #this is the pipe operator, ctrl+shift+m
    nc_open()
  
  # this if statement is used to extract variables
  if (var=='total_precipitation') {
    df <- ncvar_get(nc = nc, varid = 'tp')
  } else {
    df <- ncvar_get(nc = nc, varid = 't2m')
  }
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
    select(-c(lat_index, lon_index, time_index)) %>% 
    inner_join(select(era5_pts, lat, lon), by = c('lat', 'lon'))
  # always close out nc when done, or else it will clog up space in memory
  nc_close(nc)
  
  return(df)
}


for (year in years) {
  for (mnth in mnths) {
    for (var in vars) {
      nc_file <- glue("../DATA/ERA5_{var}_{year}_{mnth}.nc")
      
      # if total precip
      if(var == 'total_precipitation') {
        df_cur <- extract_df_from_nc(nc_file, var)
        
        next_dataset <- ym(paste0(year,mnth)) + months(1)
        next_year <- year(next_dataset)
        next_mnth <- sprintf('%02d', month(next_dataset))
        next_nc_file <- glue("../DATA/ERA5_{var}_{next_year}_{next_mnth}.nc")
        # check if we have another month of data, if so, grab first timestamp
        if (file.exists(next_nc_file)) {
          df_next <- extract_df_from_nc(next_nc_file, var) %>% 
            filter(time == time[1])
          df <- rbind(df_cur, df_next)
        # if we don't have another month, just use current month
        } else {
          df <- df_cur
        }
          
      } else {
        df <- extract_df_from_nc(nc_file, var)
      }
      
      #Daily Time Series conversion
      if (var=='total_precipitation') {
        df_prcp <- df %>% 
          # shift timeseries so 0 hour for next day is included as previous
          mutate(time = as.Date(time - hours(1))) %>%
          # exclude first timestamp (which will be mapped to previous day)
          filter(time != min(time)) %>% 
          group_by(lat,lon,time)%>%
          # take max value from cumulative timeseries
          summarize(total = max(value,na.rm = T)*1000)
      } else {
        # 1. Convert temperatures to deg C (see tidy mutate function)
        df_t <- df %>% 
          mutate(cels = value-273.15,
                 time = as.Date(time)) %>% 
          group_by(lat,lon,time)%>%
          select(-c(value))
        # 2. Calculate average and max temperature by day and grid cell for the entire year 
        # (see tidy functions group_by and summarize, check out as.Date function to convert to Date)
        df_T <- df_t %>% 
          summarize(avg = mean(cels, na.rm = T), max = max(cels, na.rm=T), n=n())    
        df_avgT <- df_t %>% 
          summarize(avg = mean(cels, na.rm = T), n=n())
        df_maxT <- df_t %>% 
          summarize(max = max(cels, na.rm=T), n=n())
        df_minT <- df_t %>% 
          summarize(min = min(cels, na.rm=T), n=n())
        
      }
    }
    
    #during for loop creates db, dc, and dd
    de <- df_maxT
    db<-data.frame("maxT",year,mnth,nrow(de),nrow(na.omit(de)),nrow(de)-nrow(na.omit(de)))
    names(db)<-c("variable","year","month","nrow","nrowAv","nrowNA")
    de <- df_avgT
    dc<-data.frame("avgT",year,mnth,nrow(de),nrow(na.omit(de)),nrow(de)-nrow(na.omit(de)))
    names(dc)<-c("variable","year","month","nrow","nrowAv","nrowNA")
    de <- df_prcp
    dd<-data.frame("totalPrecip",year,mnth,nrow(de),nrow(na.omit(de)),nrow(de)-nrow(na.omit(de)))
    names(dd)<-c("variable","year","month","nrow","nrowAv","nrowNA")
    de <- df_minT
    df<-data.frame("minT",year,mnth,nrow(de),nrow(na.omit(de)),nrow(de)-nrow(na.omit(de)))
    names(df)<-c("variable","year","month","nrow","nrowAv","nrowNA")
    #At the end of every for loop binds new db,dc, and dd created 
    #to the old da and saves this as the new da
    da <- rbind(da, db, dc, dd, df)
    
    #saves CSV files
    write.csv(df_prcp,glue("../CSVout/ERA5_total_precipitation_{year}_{mnth}.csv"), row.names = FALSE)
    write.csv(df_maxT,glue("../CSVout/ERA5_max2m_temp_{year}_{mnth}.csv"), row.names = FALSE)
    write.csv(df_minT,glue("../CSVout/ERA5_min2m_temp_{year}_{mnth}.csv"), row.names = FALSE)
    write.csv(df_avgT,glue("../CSVout/ERA5_avg2m_temp_{year}_{mnth}.csv"), row.names = FALSE)
    print (glue('CSVs for {mnth} month in {year} created Successfully :)'))
  }
}

#After all the code has run should save da as a csv
write.csv(da,glue("../CSVout/NRowsNAcheck.csv"), row.names = FALSE)

Sys.time() - start
#use shapefile long and lat (code to read in shapefile & clip data to the watersheds)
#interjoin to subset the above df