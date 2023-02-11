library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(purrr)



## LOAD DATA
#install.packages("jsonlite")
library(jsonlite)
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
#install.packages("yfR")
library(yfR) # yahoo finance package
# Unique Ticker symbols from gov_trades transactions to pull from yahoo finance
tickers <- gov_trades %>% arrange(ticker) %>% pull(ticker) %>% unique()


## Replace historic ticker symbols with updated ones
changes_2017 <- read.csv("data/actions-changes-2017.csv", header = TRUE, sep = ",")
changes_2018 <- read.csv("data/actions-changes-2018.csv", header = TRUE, sep = ",")
changes_2019 <- read.csv("data/actions-changes-2019.csv", header = TRUE, sep = ",")
changes_2020 <- read.csv("data/actions-changes-2020.csv", header = TRUE, sep = ",")
changes_2021 <- read.csv("data/actions-changes-2021.csv", header = TRUE, sep = ",")
changes_2022 <- read.csv("data/actions-changes-2022.csv", header = TRUE, sep = ",")
changes_2023 <- read.csv("data/actions-changes-2023.csv", header = TRUE, sep = ",")

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
tickers2 <- tickers[501:930]
tickers2a <- tickers[931:1000]
tickers3 <- tickers[1001:1394]
tickers3a <- tickers[1395:1500]
tickers4 <- tickers[1501:1722]
tickers4a <- tickers[1723:1870]
tickers4b <- tickers[1871:2000]
tickers5 <- tickers[2001:2160]
tickers5a <- tickers[2161:ticker_length]




## Get Stock Prices
stocks1 <- yf_get(tickers = tickers1, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0)
stocks2 <- yf_get(tickers = tickers2, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0)
stocks2a <- yf_get(tickers = tickers2a, 
                   first_date = first.date,
                   last_date = last.date, 
                   freq_data = freq.data,
                   do_cache = TRUE,
                   thresh_bad_data = 0)
stocks3 <- yf_get(tickers = tickers3, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0)
stocks3a <- yf_get(tickers = tickers3a, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0)
stocks4 <- yf_get(tickers = tickers4, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0)
stocks4a <- yf_get(tickers = tickers4a, 
                   first_date = first.date,
                   last_date = last.date, 
                   freq_data = freq.data,
                   do_cache = TRUE,
                   thresh_bad_data = 0)
stocks4b <- yf_get(tickers = tickers4b, 
                   first_date = first.date,
                   last_date = last.date, 
                   freq_data = freq.data,
                   do_cache = TRUE,
                   thresh_bad_data = 0)
stocks5 <- yf_get(tickers = tickers5, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0)
stocks5a <- yf_get(tickers = tickers5a, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0)

# BINDING stock tables together for ONE GIANT stocks table
stocks <- bind_rows(stocks1, stocks2, stocks2a, stocks3, stocks4, stocks4a, stocks4b, stocks5)

## APPENDING GOVERNMENT TRADES WITH PRICE DATA FROM THAT DATE ##
gov_trades <- gov_trades %>% 
  left_join(y=stocks, by = c("transaction_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, .keep_all = TRUE)
  
  

#### DROP NA VALUES####
gov_trades <- gov_trades %>% filter(!is.na(price_adjusted))

## APPENDING GOVERNMENT TRADES WITH PRICE DATA FROM 30/90/365 days from THAT DATE ##
#install.packages("bizdays")
#install.packages("RQuantLib")

#TESTING IDEA
#install.packages("qlcal")
library(qlcal)

##########
  
  
library(bizdays) # for rounding dates to the nearest business day to get accurate stock  dates
library(RQuantLib)

####SET CALENDAR####
fromD <- as.Date("2017-01-01")
toD <- as.Date("2030-12-31")

myholidays <- getHolidays(fromD, toD)        # US New York Stock Exchange

create.calendar(
  name = "MyNYSE", holidays = myholidays, weekdays = c("sunday", "saturday"),
  adjust.from = adjust.none, adjust.to = adjust.none)
calendars() # LOOK AT CALENDARS IN MY SYSTEM
####################



gov_trades <- gov_trades %>% 
  mutate(price_one_month_date = adjust.previous(transaction_date + months(1),"MyNYSE"),
                      price_three_months_date = adjust.previous(transaction_date + months(3),"MyNYSE"),
                      price_one_year_date = adjust.previous(transaction_date + years(1),"MyNYSE"))
  # price_one_month later
gov_trades <- gov_trades %>%
  left_join(y=stocks, by = c("price_one_month_date" = "ref_date", "ticker" = "ticker"), keep = FALSE) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -price_one_month_date) %>%
  rename(price_one_month = price_adjusted.y, volume = volume.x, price = price_adjusted.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, .keep_all = TRUE) %>%
  # price_three_months later
  left_join(y=stocks, by = c("price_three_months_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -price_three_months_date) %>%
  rename(price_three_months = price_adjusted, volume = volume.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, .keep_all = TRUE) %>%
  # price_one_year later
  left_join(y=stocks, by = c("price_one_year_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -price_one_year_date) %>%
  rename(price_one_year = price_adjusted, volume = volume.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, type, lower_bound, upper_bound,
             representative, district, state, cap_gains_over_200_usd, industry, sector, party, .keep_all = TRUE) 
 
####TESTING####
# colnames(gov_trades)
# colnames(stocks)  
# gov_trades_temp[duplicated(gov_trades[c("transaction_date", "ticker")]),]
# gov_trades_temp %>% nrow()
# gov_trades %>% nrow()  
###############  

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
  rename(voo = price_adjusted, volume = volume.x, ticker = ticker.x)

## APPEND VOO + 30/60/365
gov_trades <- gov_trades %>% 
  mutate(voo_one_month_date = adjust.previous(transaction_date + months(1),"MyNYSE"),
         voo_three_months_date = adjust.previous(transaction_date + months(3),"MyNYSE"),
         voo_one_year_date = adjust.previous(transaction_date + years(1),"MyNYSE")) %>%
  # price_one_month later
  left_join(y=VOO, by = c("voo_one_month_date" = "ref_date")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -voo_one_month_date, -ticker.y) %>%
  rename(voo_one_month = price_adjusted, volume = volume.x, ticker = ticker.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, .keep_all = TRUE) %>%
  # price_three_months later
  left_join(y=VOO, by = c("voo_three_months_date" = "ref_date")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -voo_three_months_date, -ticker.y) %>%
  rename(voo_three_months = price_adjusted, volume = volume.x, ticker = ticker.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, .keep_all = TRUE) %>%
  # price_one_year later
  left_join(y=VOO, by = c("voo_one_year_date" = "ref_date")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -voo_one_year_date, -ticker.y) %>%
  rename(voo_one_year = price_adjusted, volume = volume.x, ticker = ticker.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, type, lower_bound, upper_bound,
         representative, district, state, cap_gains_over_200_usd, industry, sector, party, .keep_all = TRUE)


## Reorder Columns

gov_trades <- gov_trades[,c("ticker", "disclosure_year", "disclosure_date", "transaction_date", 
              "lower_bound", "upper_bound", "volume", "price", "price_one_month", 
              "price_three_months" ,"price_one_year" ,"voo", "voo_one_month", "voo_three_months", "voo_one_year",
              "asset_description", "type","owner", "representative", "district", "state", "cap_gains_over_200_usd",
              "industry", "sector", "party")]


  
## IMPORT COMMITTEE DATA: What Committees each representative was on in each year
committee_2020 <- read.csv("data/RAW-committee_2020.csv", header = TRUE, sep = ",")
committee_2021 <- read.csv("data/RAW-committee_2021.csv", header = TRUE, sep = ",")
committee_2022 <- read.csv("data/RAW-committee_2022.csv", header = TRUE, sep = ",")

## CLEAN COMMITTEE DATA

## Web Scraper pulled duplicate rows for some reason ##
committee_2020 <- committee_2020 %>% distinct(Name, .keep_all = TRUE)
committee_2021 <- committee_2021 %>% distinct(Name, .keep_all = TRUE)
committee_2022 <- committee_2022 %>% distinct(Name, .keep_all = TRUE)

## Split committee names into nested list ("Tech | agriculture" --> "Tech" "Agriculture")
committee_2020$Committee.Assignment_Collapsed <- sapply(str_split(committee_2020$Committee.Assignment, "(?<=[a-z])(?=[A-Z])"), function(x) paste(x, collapse = ":"))
committee_2021$Committee.Assignment_Collapsed <- sapply(str_split(committee_2021$Committee.Assignment, "(?<=[a-z])(?=[A-Z])"), function(x) paste(x, collapse = ":"))
committee_2022$Committee.Assignment_Collapsed <- sapply(str_split(committee_2022$Committee.Assignment, "(?<=[a-z])(?=[A-Z])"), function(x) paste(x, collapse = ":"))

## NOT Dumified version
library(humaniformat)
committee_2020_long <- committee_2020 %>% 
  select(-Committee.Assignment) %>% 
  separate_rows(Committee.Assignment_Collapsed, sep = ":") %>%
  mutate(year = 2020)
committee_2021_long <- committee_2021 %>% 
  select(-Committee.Assignment) %>% 
  separate_rows(Committee.Assignment_Collapsed, sep = ":") %>%
  mutate(year = 2021)
committee_2022_long <- committee_2022 %>% 
  select(-Committee.Assignment) %>% 
  separate_rows(Committee.Assignment_Collapsed, sep = ":") %>%
  mutate(year = 2022)

committees <- bind_rows(committee_2020_long, committee_2021_long, committee_2022_long)
committees$Name <- format_period(committees$Name)
committees$Name <- format_reverse(committees$Name)
committees <- committees %>% mutate(parse_names(committees$Name))
#rename one column
colnames(committees)[colnames(committees)=="Committee.Assignment_Collapsed"] <- "committee" 
#delete NA
committees <- committees %>% filter(!committee == "")

committees %>% filter(grepl("Susan A. Davis", full_name, ignore.case = TRUE))



## DUMIFY Rows into columns binary
#install.packages("fastDummies")
library(fastDummies)

committee_2020 <- committee_2020 %>% 
  rename(c2020 = Committee.Assignment_Collapsed) %>%
  select(-Committee.Assignment) %>%
  dummy_cols(split = ":", select_columns = "c2020", remove_selected_columns = TRUE)

committee_2021 <- committee_2021 %>% 
  rename(c2021 = Committee.Assignment_Collapsed) %>%
  select(-Committee.Assignment) %>%
  dummy_cols(split = ":", select_columns = "c2021", remove_selected_columns = TRUE)

committee_2022 <- committee_2022 %>% 
  rename(c2022 = Committee.Assignment_Collapsed) %>%
  select(-Committee.Assignment) %>%
  dummy_cols(split = ":", select_columns = "c2022", remove_selected_columns = TRUE)


## FORMAT REPRESENTATIVE NAMES and temporarily append first, middle, last, etc columns
#install.packages("humaniformat") # formatting and parsing human names 
#https://cran.r-project.org/web/packages/humaniformat/vignettes/Introduction.html#:~:text=humaniformat%20is%20an%20R%20package,middle%2D%20and%20last%2Dnames.
library(humaniformat)

## Format Names to prepare for merging
committee_2020$Name <- format_period(committee_2020$Name)
committee_2020$Name <- format_reverse(committee_2020$Name)
committee_2020 <- committee_2020 %>% mutate(parse_names(committee_2020$Name)) 
committee_2020 <- committee_2020 %>% select(tail(names(committee_2020),5),everything())
# Remove extraneous columns and reorder
committee_2020 <- committee_2020 %>% select(-Phone, -District, -Name, -Party, -Office.Room, -salutation, -suffix)

committee_2021$Name <- format_period(committee_2021$Name)
committee_2021$Name <- format_reverse(committee_2021$Name)
committee_2021 <- committee_2021 %>% mutate(parse_names(committee_2021$Name))
committee_2021 <- committee_2021 %>% select(tail(names(committee_2021),5),everything())
# Remove extraneous columns and reorder
committee_2021 <- committee_2021 %>% select(-Phone, -District, -Name, -Party, -Office.Room, -salutation, -suffix)

committee_2022$Name <- format_period(committee_2022$Name)
committee_2022$Name <- format_reverse(committee_2022$Name)
committee_2022 <- committee_2022 %>% mutate(parse_names(committee_2022$Name))
committee_2022 <- committee_2022 %>% select(tail(names(committee_2022),5),everything())
# Remove extraneous columns and reorder
committee_2022 <- committee_2022 %>% select(-Phone, -District, -Name, -Party, -Office.Room, -salutation, -suffix)

gov_trades$representative <- format_period(gov_trades$representative)
gov_trades <- gov_trades %>% mutate(parse_names(gov_trades$representative))


#### Unique Legislators and demographic info ####

reps <- gov_trades %>% 
  select(representative, district, state, party, salutation, first_name, middle_name, last_name, suffix, full_name) %>%
  unique()
##########

report_days <- read.csv("data/RAW-economic-indicators-reports-2020-2022.csv", header = TRUE, sep = ",")
report_days$report_date <- as.Date(report_days$report_date, format = "%m/%d/%y")

gov_trades <- gov_trades %>% mutate(type = ifelse(type %in% c("sale_partial", "sale_full", "sale"), "sell", type),
                                    type = ifelse(type=="purchase", "buy", type))

## WRITE IT ALL TO.CSV ##
write.csv(gov_trades, 
          file = 'data/CLEAN-gov-trades.csv', 
          row.names = FALSE)

write.csv(stocks, 
          file = "data/CLEAN-stocks-data.csv", 
          row.names = FALSE)

write.csv(committee_2020, 
          file = "data/CLEAN-committee-2020-data.csv", 
          row.names = FALSE)

write.csv(committee_2021, 
          file = "data/CLEAN-committee-2021-data.csv", 
          row.names = FALSE)

write.csv(committee_2022, 
          file = "data/CLEAN-committee-2022-data.csv", 
          row.names = FALSE)
write.csv(report_days,
          file = "data/CLEAN-gov-report-days.csv",
          row.names = FALSE)
write.csv(reps,
          file = "data/CLEAN-representatives-unique.csv",
          row.names = FALSE)
write.csv(VOO,
          file = 'data/RAW-VOO.csv',
          row.names = FALSE)
write.csv(committees,
          file = "data/CLEAN-committees-all.csv",
          row.names = FALSE)





###########################
#FAILED EXPLORATION CODE

# gov_trades %>%
#   filter((representative == "Virginia Foxx") & (ticker == "BP"))


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
