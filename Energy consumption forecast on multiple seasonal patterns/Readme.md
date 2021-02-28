This project is related to forecast an energy consumption time series with multiple seasonal patterns. In reality, most long-term time series have more than one seasonal pattern, however, most existing models only accommodate simple seasonal patterns.
In that way, we introduce four methods for this problem: MSTL model allows for dynamic seasonal components; Dynamic harmonics regression uses a harmonic to fit patterns; BATS model makes progress on the simple state-space model; TBATS combines BATS model with a trigonometric method.
Four methods are compared by post-sample forecast performance (RMSE) and we can get a short-term forecast basing on our best model.

The original dataset and description of the problem can be found on this website: https://www.kaggle.com/robikscube/hourly-energy-consumption

My analysis only uses the dataset "DUQ_hourly.csv" on this website, which represents the hourly energy consumption from Duquesne Light Company, which generated electricity primarily in the area around Pittsburgh, Pennsylvania.
