# FloridaRainfallPred
Predict historical rainfall for south Florida (contains the Everglades area and Lake Okeechobee)
using temperature, wind, surface pressure, and lake cover features in a deep learning neural network model.

neuralnet.ipynb neural network model. V1 performance: Mean Absolute Error on the Test Set of 1.13 millimeters of rain per day. Predicted prcp_total, included outliers, did not include lat, lon
machineLearn.ipnyb neural network model. Mean Absolute Error on Test Set: 0.47 millimeters. Predicts next_day_prcp_total, outliers removed, includes lat, lon, & prcp_total
