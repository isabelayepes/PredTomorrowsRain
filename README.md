# Predicting Tomorrow's Rain

Extended Summary:

My project goal was to predict tomorrow's rain using the current day's rainfall, temperature, wind surface pressure, and other features. I used machine learning regression and classifier models trained and tested on South Florida's 2015 data. Then, I tested it on New York City and South Florida's 2022 December data. 

I had to download data from an API and convert the hourly data to daily. After standardizing features and removing outliers, I had 87,941 data points to train my models. 

I split the data into the 60th percentile and the 95th percentile for the classifiers. Because I chose these percentiles, it created class imbalance. The last category only represents the 5% maximum of extreme rainfall events. However, I did this because I wanted to see how the models could predict those extreme rainfall events. 

The XGBoost model was the most accurate classifier on the 2015 South Florida test data. I chose all these models without latitude and longitude because the data for New York City had higher latitudes that the model would never have seen. The XGBoost had features like temperature, current day's rainfall, and surface pressures as top importance. 

The South Florida models were then ready to be tested on 2022 data. The neural net regression predicts continuous millimeters of rainfall per day, whereas the categorical predictions group it into small, moderate, or extreme rainfall events. 

In general, the extreme rainfall events were not well predicted by the models, but the small rainfall events for New York and the moderate rainfall events for Florida were better predicted. 

The F1 score is an indicator of accuracy used to compare the classifiers. The XGBoost overall did the best of the classifiers. I preferred the classifier over the regression because it's more important for me to know whether the next day's rainfall is small, moderate, or extreme.

For further work, different percentiles could be tried to avoid class imbalance. Also, the previous day's rainfall could be added as a prediction variable because from the feature importance chart, the current day's rainfall was of high importance. 
