library(quantmod) # get stock prices; useful stock analysis functions
library(rvest)# web scraping
library(tidyverse) # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr) # working with strings
library(forcats)
library(lubridate) # working with dates 
library(plotly) # interactive plots
library(corrplot)
library(dplyr)
library(PerformanceAnalytics) # evaluating the performance and  risk  characteristics  of  financial  assets  or  funds

#Loads the company stock using ticker
getSymbols("MSFT",from="2009-10-01",to="2020-08-30") # Microsoft 
getSymbols("INTC",from="2009-10-01",to="2020-08-30") #Intel
getSymbols("T",from="2009-10-01",to="2020-08-30")   # AT&T
getSymbols("KO",from="2009-10-01",to="2020-08-30")  # Coca Cola
getSymbols("WMT",from="2009-10-01",to="2020-08-30") #Walmart
getSymbols("AAPL",from="2009-10-01",to="2020-08-30") #Apple

#Stock returns in log
MSFT_log_returns<-MSFT%>%Ad()%>%dailyReturn(type='log')
INTC_log_returns<-INTC%>%Ad()%>%dailyReturn(type='log')
T_log_returns<-T%>%Ad()%>%dailyReturn(type='log')
KO_log_returns<-KO%>%Ad()%>%dailyReturn(type='log')
WMT_log_returns<-WMT%>%Ad()%>%dailyReturn(type='log')
AAPL_log_returns<-AAPL%>%Ad()%>%dailyReturn(type='log')

MSFT_log_returns
#Mean of log stock returns 

MSFT_mean_log<-mean(MSFT_log_returns)
INTC_mean_log<-mean(INTC_log_returns)
T_mean_log<-mean(T_log_returns)
KO_mean_log<-mean(KO_log_returns)
WMT_mean_log<-mean(WMT_log_returns)
AAPL_mean_log<-mean(AAPL_log_returns)

#round it to 4 decimal places

mean_log<-c(INTC_mean_log,T_mean_log,KO_mean_log,MSFT_mean_log,WMT_mean_log, AAPL_mean_log)
mean_log<-round(mean_log,4)
mean_log
#standard deviation of log stock returns

MSFT_sd_log<-sd(MSFT_log_returns)
INTC_sd_log<-sd(INTC_log_returns)
T_sd_log<-sd(T_log_returns)
KO_sd_log<-sd(KO_log_returns)
WMT_sd_log<-sd(WMT_log_returns)
AAPL_sd_log<-sd(AAPL_log_returns)

MSFT_sd_log

sd_log<-c(INTC_sd_log,T_sd_log,KO_sd_log,MSFT_sd_log,WMT_sd_log, AAPL_sd_log)
sd_log<-round(sd_log,4)

# Calculating Sharpe Ratio. Useful for comparing investments

# Treasury bonds 5 years - Risk free benchmark rate
getSymbols("^FVX",from="2009-10-01",to="2020-08-30")
FVX <- na.omit(FVX)
rf_log_returns<-FVX%>%Ad()%>%dailyReturn(type='log')
rf_mean_log <- mean(rf_log_returns)

sharpe_ratio_MSFT <- (MSFT_mean_log - rf_mean_log) / MSFT_sd_log
sharpe_ratio_INTC <- (INTC_mean_log - rf_mean_log) / INTC_sd_log
sharpe_ratio_T <- (T_mean_log - rf_mean_log) / T_sd_log
sharpe_ratio_KO <- (KO_mean_log - rf_mean_log) / KO_sd_log
sharpe_ratio_WMT <- (WMT_mean_log - rf_mean_log) / WMT_sd_log
sharpe_ratio_AAPL <- (AAPL_mean_log - rf_mean_log) / AAPL_sd_log

sharpe_ratios <- c(sharpe_ratio_MSFT, sharpe_ratio_INTC, sharpe_ratio_T, sharpe_ratio_KO, sharpe_ratio_WMT, sharpe_ratio_AAPL)

stock_names <- c("MSFT", "INTC", "T", "KO", "WMT", "AAPL")
colors <- c("#1f77b4", "#888888" ,"#ff7f0e", "#2ca02c", "#d62728", "#9467bd")

sharpe_ratios <- data.frame(Stock = stock_names, SharpeRatio = sharpe_ratios)

ggplot(sharpe_ratios, aes(x = Stock, y = SharpeRatio, fill = Stock)) +
  geom_bar(stat = "identity") +  scale_fill_manual(values=colors)+
  labs(title = "Sharpe Ratios of Stocks", x = "Stock", y = "Sharpe Ratio")

#PORTFOLIO: mix of Apple, Coca-cola, Microsoft, AT&T and Walmart 

# calculate the weight of each stock

#Risk parity:
portfolio_data <- data.frame(MSFT_log_returns, T_log_returns, KO_log_returns, WMT_log_returns, AAPL_log_returns)

# covariance matrix
cov_matrix <- cov(portfolio_data)

# risk/volatility of each asset
asset_volatility <- sqrt(diag(cov_matrix))

# risk parity weights
risk_parity_weights <- 1 / asset_volatility / sum(1 / asset_volatility)
risk_parity_weights <- as.vector(risk_parity_weights)

portfolio_names <- c("MSFT", "T", "KO", "WMT", "AAPL")

risk_parity_weights_df <- data.frame(portfolio_names,risk_parity_weights)
colnames(risk_parity_weights_df)

#pie chart
labels <- c(paste("MSFT -", round(risk_parity_weights[1] * 100, 2), "%"),
            paste("T -", round(risk_parity_weights[2] * 100, 2), "%"),
            paste("KO -", round(risk_parity_weights[3] * 100, 2), "%"),
            paste("WMT -", round(risk_parity_weights[4] * 100, 2), "%"),
            paste("AAPL -", round(risk_parity_weights[5] * 100, 2), "%"))

colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")

pie(risk_parity_weights, labels = labels, col = colors, main = "Risk Parity Portfolio Weights")

####################### try color = stocks###################

##################################################

# One simulation of our portfolio - SEPARATELY and by wheigth
portfolio <- c("MSFT", "T", "KO", "WMT", "AAPL")
mean_log
sd_log

price<-data.frame(matrix(NA,nrow=252*4,ncol=5))
colnames(price)<-portfolio

#most recent prices
price[1,1]<-as.numeric(MSFT$MSFT.Adjusted[length(MSFT$MSFT.Adjusted)*risk_parity_weights[1],])
price[1,2]<-as.numeric(T$T.Adjusted[length(T$T.Adjusted)*risk_parity_weights[2],])
price[1,3]<-as.numeric(KO$KO.Adjusted[length(KO$KO.Adjusted)*risk_parity_weights[3],])
price[1,4]<-as.numeric(WMT$WMT.Adjusted[length(WMT$WMT.Adjusted)*risk_parity_weights[4],])
price[1,5]<-as.numeric(AAPL$AAPL.Adjusted[length(AAPL$AAPL.Adjusted)*risk_parity_weights[5],])

#simulating prices
for (day in 2:nrow(price)) {
  for (stock in 1:length(portfolio)) {
    price[day, stock] <- price[day - 1, stock] * exp(rnorm(1, mean_log[stock], sd_log[stock]))
  }
}

portfolio_simulation<-cbind(1:(252*4),price)
colnames(portfolio_simulation)<-c("Day",portfolio)
portfolio_simulation<-as.data.frame(portfolio_simulation)
portfolio_simulation

portfolio_simulation_joint <- portfolio_simulation %>% gather(key = "Stock", value = "Price", -Day)

# Create the plot with lines for all stocks
portfolio_simulation_joint %>%
  ggplot(aes(x = Day, y = Price, color = Stock)) + geom_line() +
  labs(title = "Portfolio Price Simulation for 4 Years") +theme_bw()

######################
## Joining the 5 stocks of the portfolio by weight into a new column ("portfolio") and simulate on that

risk_parity_weights
portfolio_names <- c("MSFT", "T", "KO", "WMT", "AAPL")

portfolio_mean_log<-c(MSFT_mean_log,T_mean_log,KO_mean_log,WMT_mean_log, AAPL_mean_log)
portfolio_sd_log<-c(MSFT_sd_log,T_sd_log,KO_sd_log,WMT_sd_log, AAPL_sd_log)

mu<-portfolio_mean_log*risk_parity_weights
sig<-portfolio_sd_log*risk_parity_weights

portfolio_price<-rep(NA,252*4)

#most recent prices
portfolio_price[1] <- as.numeric(MSFT$MSFT.Adjusted[length(MSFT$MSFT.Adjusted)] * risk_parity_weights[1] +
                         T$T.Adjusted[length(T$T.Adjusted)] * risk_parity_weights[2] +
                         KO$KO.Adjusted[length(KO$KO.Adjusted)] * risk_parity_weights[3] +
                         WMT$WMT.Adjusted[length(WMT$WMT.Adjusted)] * risk_parity_weights[4] +
                         AAPL$AAPL.Adjusted[length(AAPL$AAPL.Adjusted)] * risk_parity_weights[5])

                              
#simulating prices
for(i in 2:length(portfolio_price)){
  portfolio_price[i]<-portfolio_price[i-1]*exp(rnorm(1,mu,sig))
}

portfolio_simulation<-cbind(portfolio_price,1:(252*4))
colnames(portfolio_simulation)<-c("Price","Day")
portfolio_simulation<-as.data.frame(portfolio_simulation)
portfolio_simulation%>%ggplot(aes(Day,Price))+geom_line() +
  labs(title="Portfolio price simulation for 4 years")+theme_bw()


#now monte carlo 500 times
N<-500
mc_matrix<-matrix(nrow=252*4,ncol=N)
mc_matrix[1,1]<-as.numeric(portfolio_price[1])


for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(portfolio_price[1])
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

mean_performance <- apply(mc_matrix, 1, mean)

name<-str_c("Simulation ",seq(1,500))

monte_carlo <- cbind(1:(252 * 4), mc_matrix, mean_performance)
monte_carlo<-as.tibble(monte_carlo)
colnames(monte_carlo) <- c("Day",name, "mean_performance")

# Monte Carlo plot + average
monte_carlo %>%
  gather("Simulation", "Price", 2:(N + 1)) %>%
  ggplot(aes(x = Day, y = Price, color = Simulation)) +
  geom_line(alpha = 0.2) +
  geom_line(data = monte_carlo, aes(x = Day, y = mean_performance, color = "Average"), size = 0.2) +
  scale_color_manual(values = c("Average" = "red")) +
  labs(title = "Portfolio: 500 Monte Carlo Simulations for 4 Years") +
  theme_bw()

mean_performance[length(mean_performance)]

