library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(purrr)
library(yfR) # yahoo finance package

## LOAD DATA

url <- "https://house-stock-watcher-data.s3-us-west-2.amazonaws.com/data/all_transactions.json"
gov_trades_RAW <- fromJSON(url)
write.csv(gov_trades_RAW,
          file = "data/RAW-gov-trades.csv",
          row.names = FALSE)


## REMOVE COLUMNS
gov_trades <- gov_trades_RAW %>% select(-ptr_link)


## CHANGE CLASS 
gov_trades$disclosure_date <- as.Date(gov_trades$disclosure_date, format = "%m/%d/%Y")
gov_trades$transaction_date <- as.Date(gov_trades$transaction_date, format = "%Y-%m-%d")


## REMOVE DATA 

# remove bad date data
gov_trades <- gov_trades %>% filter(transaction_date > "2017-09-04")

# remove "exchange" trade type. I'm having trouble figuring out how to deal with this data and since its only <100 observations, i'll drop it
gov_trades <- gov_trades %>% filter(type != "exchange")

# remove NA Ticker symbols
gov_trades <- gov_trades %>% filter(!is.na(ticker)) %>%
  filter(ticker != "--")


### FORMATTING ###

# value range column ($1,000 - $14,999) to separate columns ($1,000)($14,999)
gov_trades <- gov_trades %>% 
  separate(col = amount,into =c("lower_bound", "upper_bound"), sep = " -")

# All Lower bounds are present, it's the upper bounds that are missing
gov_trades %>% filter(is.na(lower_bound))

# it's only the upper bounds of the $1,000-$15,000. So I will impute the $15,000 for those missing values. 
gov_trades$upper_bound <- replace(gov_trades$upper_bound, gov_trades$upper_bound == "", "15000")


# Remove special characters and convert to number ($1,000 -> 1000)
gov_trades$lower_bound <- as.numeric(str_remove_all(gov_trades$lower_bound, "[$, ]"))
gov_trades$upper_bound <- as.numeric(str_remove_all(gov_trades$upper_bound, "[$, ]"))


## STOCK PRICE HISTORY

# Unique Ticker symbols from gov_trades transactions to pull from yahoo finance
tickers <- gov_trades %>% arrange(ticker) %>% pull(ticker) %>% unique()


## Replace historic ticker symbols with updated ones
changes_2017 <- read.csv("data/ticker-symbol-changes/actions-changes-2017.csv", header = TRUE, sep = ",")
changes_2018 <- read.csv("data/ticker-symbol-changes/actions-changes-2018.csv", header = TRUE, sep = ",")
changes_2019 <- read.csv("data/ticker-symbol-changes/actions-changes-2019.csv", header = TRUE, sep = ",")
changes_2020 <- read.csv("data/ticker-symbol-changes/actions-changes-2020.csv", header = TRUE, sep = ",")
changes_2021 <- read.csv("data/ticker-symbol-changes/actions-changes-2021.csv", header = TRUE, sep = ",")
changes_2022 <- read.csv("data/ticker-symbol-changes/actions-changes-2022.csv", header = TRUE, sep = ",")
changes_2023 <- read.csv("data/ticker-symbol-changes/actions-changes-2023.csv", header = TRUE, sep = ",")

## Change date format in all data frame's from Dec 29, 2017" to "2017-12-29"
# Iterate through the list of dataframes to:
# 1. change to date format of year change .csv's
# 2. Append NEW ticker symbol sequentially by year to gov_trades table

# list of dataframe names
df_list <- c("changes_2017", "changes_2018", "changes_2019", "changes_2020", "changes_2021", "changes_2022", "changes_2023")

# for loop
for(df_name in df_list){
  # Get the dataframe
  df <- get(df_name)
  
  # Change the "Date" column to a date format
  df$Date <- as.Date(df$Date, format = "%b %d, %Y")
  # Assign processing back to dataframe
  assign(df_name, df)
  
  gov_trades <- gov_trades %>% 
    left_join(y=get(df_name), by = c("ticker" = "Old")) %>%
    mutate(ticker=ifelse(!is.na(New), New, ticker)) %>%
    select(-Date,-New,-New.Company.Name)
  
  print(df_name)
}

# Set parameters for yf_get function
first.date <- gov_trades %>% arrange(transaction_date) %>% head(1) %>% pull(transaction_date) # first date in gov_trades dataset
last.date <- Sys.Date() # today
freq.data <- "daily"
ticker_length <- gov_trades %>% arrange(ticker) %>% pull(ticker) %>% unique() %>% length()

# the function stops fetching data after it runs into some weird error and then stops the following records from pulling...so indentify those locations and split into sections
# it might have something to do with API limits: "Using the Public API (without authentication), you are limited to 2,000 requests per hour per IP (or up to a total of 48,000 requests a day)."
tickers1 <- tickers[1:500]
tickers2 <- tickers[501:941]
tickers2a <- tickers[942:1000]
tickers3 <- tickers[1001:1394]
tickers3a <- tickers[1395:1500]
tickers4 <- tickers[1501:1870]
tickers4a <- tickers[1871:2000]
tickers5 <- tickers[2001:2160]
tickers5a <- tickers[2161:ticker_length]




## Get Stock Prices
stocks1 <- yf_get(tickers = tickers1, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = FALSE,
                  thresh_bad_data = 0)
stocks2 <- yf_get(tickers = tickers2, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = FALSE,
                  thresh_bad_data = 0)
stocks2a <- yf_get(tickers = tickers2a, 
                   first_date = first.date,
                   last_date = last.date, 
                   freq_data = freq.data,
                   do_cache = FALSE,
                   thresh_bad_data = 0)
stocks3 <- yf_get(tickers = tickers3, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = FALSE,
                  thresh_bad_data = 0)
stocks3a <- yf_get(tickers = tickers3a, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = FALSE,
                  thresh_bad_data = 0)
stocks4 <- yf_get(tickers = tickers4, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = FALSE,
                  thresh_bad_data = 0)
stocks4a <- yf_get(tickers = tickers4a, 
                   first_date = first.date,
                   last_date = last.date, 
                   freq_data = freq.data,
                   do_cache = FALSE,
                   thresh_bad_data = 0)
stocks5 <- yf_get(tickers = tickers5, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = FALSE,
                  thresh_bad_data = 0)
stocks5a <- yf_get(tickers = tickers5a, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = FALSE,
                  thresh_bad_data = 0)

# BINDING stock tables together for ONE GIANT stocks table
stocks <- bind_rows(stocks1, stocks2, stocks2a, stocks3, stocks4, stocks4a, stocks5)

## APPENDING GOVERNMENT TRADES WITH PRICE DATA FROM THAT DATE ##

gov_trades <- gov_trades %>% 
  left_join(y=stocks, by = c("transaction_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices)

## APPENDING GOVERNMENT TRADES WITH PRICE DATA FROM 30/90/365 days from THAT DATE ##
gov_trades <- gov_trades %>% 
  mutate(price_one_month_date = transaction_date + months(1),
                      price_three_months_date = transaction_date + months(3),
                      price_one_year_date = transaction_date + years(1)) %>%
  # price_one_month later
  left_join(y=stocks, by = c("price_one_month_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -price_one_month_date) %>%
  rename(price_one_month = price_adjusted.y, volume = volume.x, price = price_adjusted.x) %>%
  # price_three_months later
  left_join(y=stocks, by = c("price_three_months_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -price_three_months_date) %>%
  rename(price_three_months = price_adjusted, volume = volume.x) %>%
  # price_one_year later
  left_join(y=stocks, by = c("price_one_year_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -price_one_year_date) %>%
  rename(price_one_year = price_adjusted, volume = volume.x)
 
  

## Append S&P 500 Price that day
VOO <- yf_get(tickers = "VOO", # S&P 500
       first_date = first.date,
       last_date = last.date, 
       freq_data = freq.data,
       do_cache = FALSE,
       thresh_bad_data = 0)

gov_trades <- gov_trades %>%
  left_join(y=VOO, by = c("transaction_date" = "ref_date")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -ticker.y) %>%
  rename(price_voo = price_adjusted, volume = volume.x, ticker = ticker.x)

gov_trades <- gov_trades %>% rename(ticker = ticker.x)

## Reorder Columns

gov_trades <- gov_trades[,c("ticker", "disclosure_year", "disclosure_date", "transaction_date", 
              "lower_bound", "upper_bound", "volume", "price", "price_one_month", 
              "price_three_months" ,"price_one_year" ,"price_voo", "asset_description", 
              "type","owner", "representative", "district", "state", "cap_gains_over_200_usd",
              "industry", "sector", "party")]


## WRITE IT TO.CSV ##
write.csv(gov_trades, 
          file = 'data/CLEAN-gov-trades.csv', 
          row.names = FALSE)

write.csv(stocks, 
          file = "data/CLEAN-stocks-data.csv", 
          row.names = FALSE)






###########################
#FAILED EXPLORATION CODE


# tq_get("AAPL", get = "stock.prices", from = "2015-01-01",adjusted = TRUE)
# 
# gov_temp %>%
#   group_by(ticker) %>%
#   do(data = tq_get(gov_temp$ticker, from = "2020-01-01", to = "2023-01-01", get = "stock.prices", adjusted = TRUE)) %>%
#   ungroup() %>%
#   select(-data) %>% 
#   left_join(gov_temp, by = c("ticker", "transaction_date"))
# 
# 
# 
# gov_temp %>%
#   group_by(ticker) %>%
#   do(data = tq_get(gov_temp$ticker, from = transaction_date, to = transaction_date, get = "stock.prices", adjusted = TRUE)) %>%
#   ungroup() %>%
#   select(-data) %>% 
#   left_join(gov_temp, by = c("ticker", "transaction_date"))
# 
# 
# 
# 
# 
# 
# gov_temp <- gov_trades %>% rename(date=transaction_date)
# BPtemp <- data.frame(BP)
# 
# merge(x=gov_trades[1,], y=BP, left_on = "transaction_date", copy = TRUE)
# 
# 
# merge(gov_temp, BP, by.x = "date", by.y = "index", all.x = TRUE)
# BP$index_col <- "TEST"
# 
# colnames(BP)
# left_join(x=gov_temp, y=BP, by = c("date" = "index"), copy = TRUE)
# 
# gov_trades[1,]
# 
#           
# 
# gov_trades[1,"ticker"]

# Function to get stock price for a given ticker
# TRY1
# get_price <- function(ticker, date) {
#   tq_get(ticker, get = "stock.prices", from = date, to = as.Date(date)+1, adjusted = TRUE, complete_cases = FALSE) %>%
#     pull(adjusted)
# }
# #TRY2
# get_price <- function(ticker, date) {
#   data <- tq_get(ticker, get = "stock.prices", from = date, to = as.Date(date)+1, adjusted = TRUE)
#   if(class(data) == "logical"){
#     return(NA)
#   }
#   else{
#     data %>%
#       pull(adjusted)
#   }
# }
# #TRY3
# get_price <- function(ticker, date) {
#   data <- tq_get(ticker, get = "stock.prices", from = date, to = as.Date(date)+1, adjusted = TRUE)
#   
#   ifelse(class(data) == "logical",NA,data %>%
#            pull(adjusted))
# }
# #TRY4
# get_price <- function(ticker, date) {
#   data <- tq_get(ticker, get = "stock.prices", from = date, to = as.Date(date)+1, adjusted = TRUE)
#   if(class(data) == "logical"){
#     return(NA)
#   }
#   else{
#     first(data %>%
#             pull(adjusted))
#   }
# }
# #TRY5
# get_price <- function(ticker, date) {
#   data <- tq_get(ticker, get = "stock.prices", from = date, to = as.Date(date)+1, adjusted = TRUE)
#   ifelse(nrow(data)==1,data %>%
#            pull(adjusted),NA)
# }
# 
# 
# 
# # test function
# get_price()
# 
# getPrice(BP)
# 
# Ad(symbol = "BP")
# 
# tq_get(c("KPLTW","AAPL", "META"), get = "stock.prices", from = "2021-01-20", to ="2021-01-21", adjusted = TRUE) %>%
#   pull(adjusted)
# 
# 
# # Apply the function to the first 100 rows to test
# gov_temp <- gov_trades[1:50,]
# gov_temp %>%
#   mutate(price = map2_dbl(ticker, transaction_date, get_price))
#           
# # adding column for avg value
# gov_trades$avg_val = (gov_trades$upper_bound+gov_trades$lower_bound)/2



##########################
