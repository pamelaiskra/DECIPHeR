# DECIPHeR
Codes used to process DECIPHeR model outputs

These codes are some of the pieces of the master code to process the outputs from the hydrological modelling done with DECIPHeR v1.0
Processing includes: 
1) Reading of model outputs
2) Storing information in data frames
3) Solving issues such as NaN values or spurious data
4) Determining members of the behvioural ensemble depending on NSE score and RMSE
5) Spread of the 5th and 95th percentiles of the behavioural ensemble defined using the GLUE methodology
6a) Scatter plot of model parameters vs NSE score
6b) Rainfall-runoff plots observed vs simulated
