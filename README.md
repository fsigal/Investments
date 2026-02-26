This project uses descriptive statistics to find out some characteristics and insights about U.S. Stock Prices, to get some information and investment purpose.

File planilla1.R contains a function that imports stock prices from Yahoo Finance, for las 500 working days (~ 2 years) and calculates Confidence Intervals and compares the last price with this interval.
If the last price is under the Interval, it means that the stock price is within the 2.5% lowest prices for the last 500 working days.

File myRSI14.R contains a funcition that calculates approximately the RSI indicator, used to analyze if a stock has been heavily sold or bought in the last days.

File RSI.R applies that function over an important number of stocks and filters those stocks that have a RSI under 30 (stock mainly sold).

This project is still in progress.
