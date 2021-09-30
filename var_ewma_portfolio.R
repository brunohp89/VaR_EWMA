library(openxlsx)
library(quantmod)
library(lubridate)
source("sigma.R")

options(stringsAsFactors = FALSE)
parameters <- read.csv(paste0(getwd(),"/parameters.csv"))
portfolio_composition <- read.csv(paste0(getwd(),"/porfolio_composition.csv"))

LOC <- parameters[parameters$Parameter == "LOC", "Value"]
holding_period <- parameters[parameters$Parameter == "holding_period", "Value"]
lambda <- parameters[parameters$Parameter == "lambda", "Value"]
backdays <- parameters[parameters$Parameter == "backdays", "Value"]
tickers <- portfolio_composition$Ticker
volume_vector <- portfolio_composition$Volume

for (ticker in tickers) {
  getSymbols(ticker, from = Sys.Date() %m-% days(backdays + 40),
             to = Sys.Date(),warnings = FALSE,
             auto.assign = TRUE)
  
  if (!exists("quotations_xts")) {
    program <- paste0("quotations_xts <- ",ticker,"$",ticker,".Close")
    eval(parse(text = program))
  } else {
    program <- paste0("quotations_xts <- merge(quotations_xts,",ticker,"$",ticker,".Close)")
    eval(parse(text = program))
  }
}

quotations_xts <- quotations_xts[complete.cases(quotations_xts)]
colnames(quotations_xts) <- tickers

if (nrow(quotations_xts) < backdays) {
  stop("Not enough observations to cover the desired backdays")
}

observations_df <- tail(quotations_xts, backdays)


log_returns <- diff(log(observations_df), lag = 1)
log_returns <- log_returns[-c(1),]

sigma_matrix <- sigma_ewma(observations_df, log_returns, lambda)


pl_vector <- coredata(observations_df[nrow(observations_df),] * volume_vector)
pl_vector_transposed <- t(pl_vector)

observations_df2 <- sweep(observations_df, 2, volume_vector, "*")


var_portfolio <- as.numeric((sqrt(holding_period) * qnorm(LOC) * sqrt(pl_vector %*% sigma_matrix %*% pl_vector_transposed))[1,1])

print(paste0("Total Amount Invested: ", as.character(round(sum(mean(observations_df2)),2))))
print(paste0("Portfolio VAR: ", as.character(round(var_portfolio,2))))
