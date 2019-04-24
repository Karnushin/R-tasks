# R-tasks
Combined ui.R and server.r are for:
Shiny app of type client-server that shows features of option based on Black–Scholes Option Pricing Model (it shows call and  put prices depending on strike)
This app can calculate call price and put price if price of an underlying asset is from [80, 120] for all strike prices from 80 to 120 if volatility is from [0.1, 0.5] and time before expiration time from [0.1, 1.5].
risk-free interest rate is r=0.03, dividend yield is q=0.

Task_0.R defines a distribution of sample given in Task0.csv.

Task_1.R: have 3 samples from Task1.csv, have to evaluate regression model y = a0+a1x1+a2x2+a3x3+noise and delete insignificant variables(if they exist), after that evaluate new regression model; also have to define a distribution of noise and check if they are uncorrelated.

Task_2.R: using data from Task2.csv have to identify ARMA models and evaluate some of them, choose the best one from evaluated ones and also research residuals.
