# ==============================================================================
#                       FIN9007 Derivatives 2025
#        Group Project - S&P 500 Futures & Options Analysis
# ==============================================================================
# 1. Install required packages
install.packages(c("tidyverse", "quantmod", "ggplot2", "PerformanceAnalytics", 
                   "TTR", "tseries", "rugarch", "dplyr", "e1071","bizdays","fGarch"))
# 2. Load packages
library(tidyverse); library(quantmod); library(ggplot2); library(PerformanceAnalytics); library(TTR); library(tseries); library(rugarch); library(dplyr); library(e1071); library(bizdays) ; library(fGarch)

# 3. Load Data

underlying <- read.csv("underlyingSampleData.csv")
EminiCross <- read.csv("spxEminiTrade20200110.csv") %>%
  pivot_longer(cols = -TradeDate, names_to = "ExpiryDate", values_to = "Price")
futures_expiry <- read.csv("spxEminiEx20200619.csv")
SpxOptionCross <- read.csv("spxOptionData20200110.csv")
spx_options_expiry <- read.csv("spxOptionDataEx20200619.csv")
SpyOptionCross <- read.csv("spyOptionData20200110.csv")
spy_options_expiry <- read.csv("spyOptionDataEx20200619.csv")
head(spx_options_expiry)

# 4. Convert Date Columns

underlying$TradeDate <- as.Date(underlying$TradeDate, format="%Y-%m-%d")
EminiCross <- EminiCross %>%
  mutate(TradeDate = as.Date(TradeDate, format="%d/%m/%Y"),
         ExpiryDate = as.Date(ExpiryDate, format="%d/%m/%Y"))
futures_expiry <- futures_expiry %>%
  mutate(TradeDate = as.Date(TradeDate, format="%d/%m/%Y"),
         ExpiryDate = as.Date("2020-06-19", format="%Y-%m-%d"))
spx_options_expiry <- spx_options_expiry %>%
  mutate(TradeDate = as.Date(TradeDate, format="%Y%m%d"),
         expiryDate = as.Date(expiryDate, format="%Y%m%d"))
spy_options_expiry <- spy_options_expiry %>%
  mutate(TradeDate = as.Date(TradeDate, format="%Y%m%d"),
         expiryDate = as.Date(expiryDate, format="%Y%m%d"))

# ==============================================================================
# 1.1 Present and comment on the summary statistics of S&P 500 index, SPDR ETF, 
# S&P 500 E-mini futures, SPX options, and SPY options.
# ==============================================================================
generate_summary_and_stats <- function(data, col, label) {
  clean_data <- na.omit(as.numeric(data[[col]]))
  clean_data <- pmax(pmin(clean_data, quantile(clean_data, 0.99, na.rm = TRUE)), quantile(clean_data, 0.01, na.rm = TRUE))
  
  cat("\n---", label, "---\n")
  cat("Mean:", round(mean(clean_data), 4), "\n")
  cat("Median:", round(median(clean_data), 4), "\n")
  cat("Mode:", as.numeric(names(sort(table(clean_data), decreasing = TRUE)[1])), "\n")
  cat("Skewness:", round(skewness(clean_data), 4), "\n")
  cat("Kurtosis:", round(kurtosis(clean_data), 4), "\n")
  cat("Q1:", round(quantile(clean_data, 0.25), 4), "\n")
  cat("Q2 (Median):", round(median(clean_data), 4), "\n")
  cat("Q3:", round(quantile(clean_data, 0.75), 4), "\n")
  cat("Q4 (95th percentile):", round(quantile(clean_data, 0.95), 4), "\n")
  
  if (length(clean_data) > 1) {
    log_returns <- diff(log(clean_data))
    cat("Annualized SD:", round(sd(log_returns) * sqrt(252), 4), "\n")
  } else {
    cat("Annualized SD: NA (Not enough data)\n")
  }
}

# Usage:
generate_summary_and_stats(underlying, "SPX", "S&P 500 Index")
generate_summary_and_stats(underlying, "SPY", "SPDR ETF")
generate_summary_and_stats(futures_expiry, "X19.06.2020", "S&P 500 E-mini Futures")
generate_summary_and_stats(spx_options_expiry, "price", "SPX Options")
generate_summary_and_stats(spy_options_expiry, "price", "SPY Options")

# ==============================================================================
# 1.2 Analyze S&P 500, SPDR ETF, and futures relationship.
# ==============================================================================

# Correlation between SPX and SPY
correlation_result <- cor(underlying$SPX, underlying$SPY, use="complete.obs")
cat("\nCorrelation between SPX and SPY:", correlation_result, "\n")

# Covariance between SPX and SPY
covariance_result <- cov(underlying$SPX, underlying$SPY, use="complete.obs")
cat("\nCovariance between SPX and SPY:", covariance_result, "\n")

# Linear Regression: Beta and R-squared for SPX and SPY
lm_model <- lm(underlying$SPY ~ underlying$SPX)
beta_result <- coef(lm_model)[2]  # Beta (Slope)
r_squared_result <- summary(lm_model)$r.squared  # R-squared
cat("\nBeta (Slope) between SPX and SPY:", beta_result, "\n")
cat("\nR-squared between SPX and SPY:", r_squared_result, "\n")

# Plot SPX vs SPY with Regression Line
plot(underlying$SPX, underlying$SPY, main="SPX vs SPY", xlab="S&P 500 Index", ylab="SPDR ETF", pch=19, col="blue")
abline(lm_model, col="red")  # Regression Line

# Cost of Carry
# Load data and format TradeDate
spEminiCross <- read.csv("spxEminiTrade20200110.csv", header=TRUE, as.is=TRUE, check.names=FALSE)
spEminiCross$TradeDate <- as.Date(spEminiCross$TradeDate, format="%d/%m/%Y")

# Get spot index for the trade date
spotIndex <- underlying[underlying$TradeDate == spEminiCross$TradeDate[1], "SPX"]

# Extract delivery dates from column names
deliveryDate <- as.Date(colnames(spEminiCross)[2:ncol(spEminiCross)], format="%d/%m/%Y")

# Define business calendar
business_calendar <- create.calendar('my_calendar', weekdays=c('saturday', 'sunday'))

# Calculate Time to Maturity (ttm)
ttm <- bizdays(spEminiCross$TradeDate[1], deliveryDate, cal=business_calendar) / 252

# Calculate Cost of Carry
futures_prices <- as.numeric(spEminiCross[1, 2:ncol(spEminiCross)])
carry_cost <- log(futures_prices / spotIndex) / ttm

# Plot Cost of Carry
plot(deliveryDate, carry_cost, type="b", col="purple", pch=19, 
     xlab="Delivery Date", ylab="Cost of Carry", main="Cost of Carry for E-mini Futures")

# Time Series of S&P 500 and E-mini Futures
merged_futures <- merge(underlying, futures_expiry, by="TradeDate")
merged_futures$Basis <- merged_futures$X19.06.2020 - merged_futures$SPX
merged_futures$BasisP <- merged_futures$Basis / merged_futures$SPX

# Linear Regression for SPX and Futures prices
lm_futures <- lm(merged_futures$X19.06.2020 ~ merged_futures$SPX)
beta_futures <- coef(lm_futures)[2]  # Beta for Futures
r_squared_futures <- summary(lm_futures)$r.squared  # R-squared for Futures
cat("\nBeta (Slope) for E-mini Futures:", beta_futures, "\n")
cat("\nR-squared for E-mini Futures:", r_squared_futures, "\n")

# Covariance and Correlation between SPX and E-mini Futures
covariance_futures <- cov(merged_futures$SPX, merged_futures$X19.06.2020, use="complete.obs")
correlation_futures <- cor(merged_futures$SPX, merged_futures$X19.06.2020, use="complete.obs")
cat("\nCovariance between SPX and E-mini Futures:", covariance_futures, "\n")
cat("\nCorrelation between SPX and E-mini Futures:", correlation_futures, "\n")

# Plot S&P 500 vs E-mini Futures with Regression Line
plot(merged_futures$TradeDate, merged_futures$SPX, type="l", col="blue", lwd=2, 
     xlab="Trade Date", ylab="Price", main="S&P 500 vs E-mini Futures")
lines(merged_futures$TradeDate, merged_futures$X19.06.2020, col="red", lwd=2)

# Plot Basis (Futures - Index)
plot(merged_futures$TradeDate, merged_futures$Basis, type="l", col="green", lwd=2, 
     xlab="Trade Date", ylab="Basis", main="Basis (Futures - Index)")

# ==============================================================================
# 2.1 Estimate and compare put-call parity implied price with S&P 500.
# ==============================================================================
# Read raw data
spxOptionCross
spxOptionCross$TradeDate <- as.Date(spxOptionCross$TradeDate, format = "%Y%m%d")
spxOptionCross$expiryDate <- as.Date(spxOptionCross$expiryDate, format = "%Y%m%d")

# 1. Sort unique expiry dates and filter for selected expiry date
sort(unique(spxOptionCross$expiryDate))
selectedExpiry <- "2020-06-19"
oneExpiryOptionData <- spxOptionCross[spxOptionCross$expiryDate == selectedExpiry, ]

# 2. Calculate business days for Time to Maturity (TTM)
business_calendar <- create.calendar('my_calendar', weekdays = c('saturday', 'sunday'))
ttm <- bizdays(oneExpiryOptionData$TradeDate[1], oneExpiryOptionData$expiryDate[1], cal = business_calendar) / 252

# 3. Filter and order call and put option data
callOptionData <- oneExpiryOptionData[oneExpiryOptionData$c_p == 1, ]
callOptionData <- callOptionData[order(callOptionData$strike), ]
putOptionData <- oneExpiryOptionData[oneExpiryOptionData$c_p == 0, ]
putOptionData <- putOptionData[order(putOptionData$strike), ]

# 4. Plot the option prices for call and put options
group <- ifelse(oneExpiryOptionData$c_p == 1, "Call options", "Put options")
plot(oneExpiryOptionData$strike, oneExpiryOptionData$price, pch = 19, col = factor(group), cex = 0.5)

# 5. Pair call and put options by strike price
pairedOptions <- data.frame(strike = sort(unique(oneExpiryOptionData$strike)))
pairedOptions <- merge(x = pairedOptions, y = callOptionData[, c('strike', 'price','TradeDate')], by = "strike", all.x = TRUE)
colnames(pairedOptions)[colnames(pairedOptions) == 'price'] <- "callPrice"
pairedOptions <- merge(x = pairedOptions, y = putOptionData[, c('strike', 'price','TradeDate')], by = "strike", all.x = TRUE)
colnames(pairedOptions)[colnames(pairedOptions) == 'price'] <- "putPrice"

# 6. Calculate the implied SPX using the put-call parity formula
pairedOptions$impliedSPX_Pseudo <- pairedOptions$callPrice + pairedOptions$strike - pairedOptions$putPrice
plot(pairedOptions$strike, pairedOptions$impliedSPX_Pseudo)

# 7. Calculate the implied interest rate using linear regression
sampleImpliedR <- data.frame(strike = pairedOptions$strike, x = (pairedOptions$putPrice - pairedOptions$callPrice))
resIR <- lm(strike ~ x, data = sampleImpliedR)
impliedSPXF <- resIR$coefficients[1]
impliedR <- log(resIR$coefficients[2]) / ttm

# 8. Calculate the implied SPX futures price with implied interest rate
pairedOptions$impliedSPXF <- pairedOptions$callPrice * exp(impliedR * ttm) + pairedOptions$strike - pairedOptions$putPrice * exp(impliedR * ttm)
plot(pairedOptions$strike, pairedOptions$impliedSPXF)

# 9. Read the underlying asset data (S&P 500 index)
underlying <- read.csv("underlyingSampleData.csv", header = TRUE, as.is = TRUE)
underlying$TradeDate <- as.Date(underlying$TradeDate, format = "%Y-%m-%d")
spotUndelrying <- underlying[underlying$TradeDate == "2020-01-10", 'SPX']

# 10. Read the S&P 500 futures price data
spEminiCross <- read.csv("spxEminiTrade20200110.csv", header = TRUE, as.is = TRUE, check.names = FALSE)
spEminiCross$TradeDate <- as.Date(spEminiCross$TradeDate, format = "%d/%m/%Y")
spotSPEmini <- spEminiCross[1, '19/06/2020']

# 11. Calculate the implied dividend yield
impliedDividend <- impliedR - log(impliedSPXF / spotUndelrying) / ttm
print(impliedDividend)

# 12. Calculate the final implied S&P 500 price using the put-call parity formula
pairedOptions$impliedSPX <- (pairedOptions$callPrice + pairedOptions$strike * exp(-impliedR * ttm) - pairedOptions$putPrice) * exp(impliedDividend * ttm)
plot(pairedOptions$strike, pairedOptions$impliedSPX)

# 13. Output the mean implied S&P 500 price
options(digits = 21)
impliedSPX <- mean(pairedOptions$impliedSPX)
print(impliedSPX)

# 14. COREALTION

# IDK ####################################################################################################################

# ==============================================================================
# 2.2 Compare option price bounds and the market option prices
# ==============================================================================
# Calculate the implied dividend yield
impliedDividend <- impliedR - log(impliedSPXF / spotSPEmini) / ttm
print(impliedDividend)

# Calculate the final implied S&P 500 price using the put-call parity formula
pairedOptions$impliedSPX <- (pairedOptions$callPrice + pairedOptions$strike * exp(-impliedR * ttm) - pairedOptions$putPrice) * exp(impliedDividend * ttm)
plot(pairedOptions$strike, pairedOptions$impliedSPX)

# Ensure the impliedSPX column is numeric
pairedOptions$impliedSPX <- as.numeric(pairedOptions$impliedSPX)

# Calculate bounds for call options
callOptionData$upperBound <- spotSPEmini
callOptionData$lowerBound <- impliedSPX * exp(-impliedDividend * ttm) - callOptionData$strike * exp(-impliedR * ttm)
callOptionData$lowerBound[callOptionData$lowerBound < 0] <- 0

# Plot call options with bounds
plot(callOptionData$strike, callOptionData$price, cex = 0.5, ylim = c(0, spotSPEmini * 1.2),
     main = "Call Option Prices vs Theoretical Bounds",
     xlab = "Strike Price", ylab = "Option Price")
lines(callOptionData$strike, callOptionData$upperBound, col = "green")
lines(callOptionData$strike, callOptionData$lowerBound, col = "red")

# Calculate bounds for put options
putOptionData$upperBound <- putOptionData$strike * exp(-impliedR * ttm)
putOptionData$lowerBound <- putOptionData$strike * exp(-impliedR * ttm) - spotSPEmini * exp(-impliedDividend * ttm)
putOptionData$lowerBound[putOptionData$lowerBound < 0] <- 0

# Plot put options with bounds
plot(putOptionData$strike, putOptionData$price, cex = 0.5, ylim = c(0, spotSPEmini * 1.2),
     main = "Put Option Prices vs Theoretical Bounds",
     xlab = "Strike Price", ylab = "Option Price")
lines(putOptionData$strike, putOptionData$upperBound, col = "green")
lines(putOptionData$strike, putOptionData$lowerBound, col = "red")

# Identify arbitrage opportunities for call and put options
call_arbitrage <- callOptionData[callOptionData$price < callOptionData$lowerBound | 
                                   callOptionData$price > callOptionData$upperBound, ]
put_arbitrage <- putOptionData[putOptionData$price < putOptionData$lowerBound | 
                                 putOptionData$price > putOptionData$upperBound, ]

# Function to print arbitrage opportunities
print_arbitrage <- function(option_type, data) {
  cat(paste(option_type, "Arbitrage Opportunities:\n"))
  if (nrow(data) > 0) {
    print(data)
  } else {
    print(paste("No arbitrage opportunities for", tolower(option_type), "options."))
  }
  cat("\n")  
}

# Print call and put option arbitrage opportunities using the function
print_arbitrage("Call Option", call_arbitrage)
print_arbitrage("Put Option", put_arbitrage)

# ==============================================================================
# 3. Binomial tree model, SPX 
# ==============================================================================
# Binomial tree model parameters (CALL OPTION)
nSteps <- 1000
interestRate <- 0.0175     # USA HAD THAT TIME (10-01-2020)
dividendYield <- 0.01784   # SP500 HAD THAT TIME
volatility <- 0.1736       # Calculated above 
ttm <- 6/12
spotUnderlying <- 3265.349
strike <- 800
call_put <- 1 # 1 for call option, -1 for put option
americanOption <- FALSE
displayBT <- FALSE

# Calculate deltaT, uCRR, dCRR, aCRR, and pCRR
deltaT <- ttm / nSteps
uCRR <- exp(volatility * sqrt(deltaT))
dCRR <- exp(-volatility * sqrt(deltaT))
aCRR <- exp((interestRate - dividendYield) * deltaT)
pCRR <- (aCRR - dCRR) / (uCRR - dCRR)

# Calculate option prices using the binomial tree model
calculate_binomial_tree <- function(nSteps, spotUnderlying, strike, call_put, interestRate, dividendYield, volatility, ttm, americanOption) {
  deltaT <- ttm / nSteps
  uCRR <- exp(volatility * sqrt(deltaT))
  dCRR <- exp(-volatility * sqrt(deltaT))
  aCRR <- exp((interestRate - dividendYield) * deltaT)
  pCRR <- (aCRR - dCRR) / (uCRR - dCRR)
  
  # Stock price at expiry
  statesTemp <- rep(NA, nSteps + 1)
  for (j in 1:(nSteps + 1)) {
    statesTemp[j] <- spotUnderlying * uCRR^(j - 1) * dCRR^(nSteps - (j - 1))
  }
  
  # Option price at expiry
  optionTemp <- rep(NA, nSteps + 1)
  for (j in 1:(nSteps + 1)) {
    optionTemp[j] <- max((statesTemp[j] - strike) * call_put, 0)
  }
  
  # Stock price and option price at each node
  for (i in nSteps:1) {
    optionTemp_1 <- rep(NA, i)
    statesTemp <- rep(NA, i)
    for (j in 1:i) {
      statesTemp[j] <- spotUnderlying * uCRR^(j - 1) * dCRR^(i - 1 - (j - 1))
    }
    
    if (americanOption == TRUE) {
      for (j in 1:i) {
        optionTemp_1[j] <- max(((1 - pCRR) * optionTemp[j] + pCRR * optionTemp[j + 1]) * exp(-interestRate * deltaT), (statesTemp[j] - strike) * call_put)
      }
    } else {
      for (j in 1:i) {
        optionTemp_1[j] <- ((1 - pCRR) * optionTemp[j] + pCRR * optionTemp[j + 1]) * exp(-interestRate * deltaT)
      }
    }
    
    optionTemp <- optionTemp_1
  }
  
  return(optionTemp)
}

# Calculate SPX option prices
binomial_tree_prices <- calculate_binomial_tree(nSteps, spotUnderlying, strike, call_put, interestRate, dividendYield, volatility, ttm, americanOption)

# Print the calculated SPX option price
cat("Calculated SPX Option Price is: ", binomial_tree_prices, "\n")

# Calculate binomial tree model prices for each strike price in callOptionData
binomial_tree_prices <- sapply(callOptionData$strike, function(strike) {
  calculate_binomial_tree(nSteps, spotUnderlying, strike, call_put, interestRate, dividendYield, volatility, ttm, americanOption)
})

# Compare with market option prices
market_option_prices <- callOptionData$price

# Print the comparison
comparison <- data.frame(
  Strike = callOptionData$strike,
  MarketPrice = market_option_prices,
  ModelPrice = binomial_tree_prices
)
cat("Comparison of Market and Binomial Tree Model Prices:\n")
print(comparison)

# Plot the comparison
plot(callOptionData$strike, market_option_prices, type = "l", col = "blue", lwd = 2, ylim = range(c(market_option_prices, binomial_tree_prices)), ylab = "Option Price", xlab = "Strike Price", main = "SPX Option Prices: Market vs Binomial Tree Model")
lines(callOptionData$strike, binomial_tree_prices, col = "red", lwd = 2)
legend("topright", legend = c("Market Prices", "Binomial Tree Model Prices"), col = c("blue", "red"), lwd = 2)

# Calculate correlation
correlation <- cor(market_option_prices, binomial_tree_prices)
cat("Correlation between Market Prices and Binomial Tree Model Prices: ", correlation, "\n")

# ==============================================================================
# 4.1 Choose a trading date, use Wiener process
# ==============================================================================




# ==============================================================================
# 5.1 Use the Black-Scholes formula to calculate SPX option prices
# ==============================================================================





# ==============================================================================
# 6. Calculate Implied Volatility of SPX and SPY Options
# ==============================================================================






# ==============================================================================
# 7.1 Estimate and Comment on S&P 500 Historical Volatility
# ==============================================================================
# Sample size
N <- length(underlying$SPX)

# Calculate simple and log returns of SPX
underlying$retSPX <- c(NA, underlying$SPX[2:N] / underlying$SPX[1:(N-1)] - 1)
underlying$logRetSPX <- c(NA, log(underlying$SPX[2:N] / underlying$SPX[1:(N-1)]))

# Plot returns
plot(underlying$TradeDate, underlying$retSPX, type = "l", main = "SPX Returns", xlab = "Date", ylab = "Returns")

# Historical volatility
underlying$var <- NA
m_days <- 60
for (i in ((m_days+1):N)) {
  underlying[i, 'var'] <- 1/m_days * sum(underlying[(i-m_days):(i-1), 'retSPX']^2)
}
underlying$stdAnnual <- sqrt(underlying$var * 252)

# Plot historical volatility
plot(underlying$TradeDate, underlying$stdAnnual, type = "l", main = "Historical Volatility (Annualized)", xlab = "Date", ylab = "Volatility")

# ==============================================================================
# 7.2 Compare GARCH and EWMA Models in Forecasting S&P 500 Conditional Volatility
# ==============================================================================
# EWMA model
# Set the begin date and end date 
B.dates <- "2010-01-01"
E.dates <- "2020-12-31"

objective_EWMA <- function(lambdaEWMA) {
  sampleEWMA <- underlying[underlying$TradeDate >= B.dates & underlying$TradeDate <= E.dates, c('TradeDate', 'SPX', 'retSPX')]
  sampleEWMA$var <- NA
  sampleEWMA[2, 'var'] <- sampleEWMA[2, 'retSPX']^2
  for (i in 3:nrow(sampleEWMA)) {
    sampleEWMA[i, 'var'] <- lambdaEWMA * sampleEWMA[i-1, 'var'] + (1-lambdaEWMA) * sampleEWMA[i-1, 'retSPX']^2
  }
  sampleEWMA$likeli <- -log(sampleEWMA$var) - sampleEWMA$retSPX^2 / sampleEWMA$var
  return(-sum(sampleEWMA$likeli, na.rm = TRUE))
}

result_EWMA <- optimize(objective_EWMA, interval = c(0, 1), tol = 0.0001)
lambdaEWMA <- result_EWMA$minimum

# Calculate EWMA variance
sampleEWMA <- underlying[underlying$TradeDate >= B.dates & underlying$TradeDate <= E.dates, c('TradeDate', 'SPX', 'retSPX')]
sampleEWMA$var <- NA
sampleEWMA[2, 'var'] <- sampleEWMA[2, 'retSPX']^2
for (i in 3:nrow(sampleEWMA)) {
  sampleEWMA[i, 'var'] <- lambdaEWMA * sampleEWMA[i-1, 'var'] + (1-lambdaEWMA) * sampleEWMA[i-1, 'retSPX']^2
}
sampleEWMA$stdAnnual <- sqrt(sampleEWMA$var * 252)

# Plot EWMA volatility
plot(sampleEWMA$TradeDate, sampleEWMA$stdAnnual, type = "l", main = "EWMA Volatility (Annualized)", xlab = "Date", ylab = "Volatility")

# GARCH(1,1) model
objective_GARCH <- function(param) {
  sampleData <- underlying[underlying$TradeDate >= B.dates & underlying$TradeDate <= E.dates, c('TradeDate', 'SPX', 'retSPX')]
  sampleData$var <- NA
  N <- nrow(sampleData)
  sampleData[2, 'var'] <- sampleData[2, 'retSPX']^2
  for (i in 3:N) {
    sampleData[i, 'var'] <- param[1] + param[2] * sampleData[i-1, 'retSPX']^2 + param[3] * sampleData[i-1, 'var']
  }
  sampleData$likeli <- -log(sampleData$var) - sampleData$retSPX^2 / sampleData$var
  return(-sum(sampleData$likeli, na.rm = TRUE))
}

result_GARCH <- constrOptim(c(0.000001, 0.05, 0.9), objective_GARCH, NULL,
                            ui = rbind(c(0, 1, 1), c(0, -1, -1), c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0), c(0, -1, 0), c(0, 0, 1), c(0, 0, -1)),
                            ci = c(0, -1, 0, -1, 0, -1, 0, -1))

param_GARCH <- result_GARCH$par

# Calculate GARCH variance
sampleGARCH <- underlying[underlying$TradeDate >= B.dates & underlying$TradeDate <= E.dates, c('TradeDate', 'SPX', 'retSPX')]
sampleGARCH$var <- NA
sampleGARCH[2, 'var'] <- sampleGARCH[2, 'retSPX']^2
for (i in 3:nrow(sampleGARCH)) {
  sampleGARCH[i, 'var'] <- param_GARCH[1] + param_GARCH[2] * sampleGARCH[i-1, 'retSPX']^2 + param_GARCH[3] * sampleGARCH[i-1, 'var']
}
sampleGARCH$stdAnnual <- sqrt(sampleGARCH$var * 252)

# Plot GARCH volatility
plot(sampleGARCH$TradeDate, sampleGARCH$stdAnnual, type = "l", main = "GARCH Volatility (Annualized)", xlab = "Date", ylab = "Volatility")

# Compare EWMA and GARCH
plot(sampleGARCH$TradeDate, sampleGARCH$stdAnnual, type = "l", main = "EWMA vs GARCH Volatility (Annualized)", xlab = "Date", ylab = "Volatility")
lines(sampleEWMA$TradeDate, sampleEWMA$stdAnnual, col = "red")
lines(underlying$TradeDate, underlying$stdAnnual, col = "blue")
legend("topright", legend = c("GARCH", "EWMA", "Historical"), col = c("black", "red", "blue"), lty = 1)

# ==============================================================================
# 8. Analyze Greek Letters and SPX Option Price Changes
# ==============================================================================




# ==============================================================================  
#                     END OF FIN9007 Derivatives 2025  
#       Group Project: S&P 500 Futures and Options – Analysis and Practice  
# ==============================================================================  
