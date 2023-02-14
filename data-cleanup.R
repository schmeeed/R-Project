library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(purrr)



## LOAD DATA
#install.packages("jsonlite")
library(jsonlite)
# House data
url <- "https://house-stock-watcher-data.s3-us-west-2.amazonaws.com/data/all_transactions.json"
house_trades_RAW <- fromJSON(url)
write.csv(house_trades_RAW,
          file = "data/RAW-house-trades.csv",
          row.names = FALSE)
# Senate
url2 <- "https://senate-stock-watcher-data.s3-us-west-2.amazonaws.com/aggregate/all_transactions.json"
sen_trades_RAW <- fromJSON(url2)
write.csv(sen_trades_RAW,
          file = "data/RAW-senate-trades.csv",
          row.names = FALSE)


## REMOVE COLUMNS
house_trades <- house_trades_RAW %>% select(-ptr_link)
sen_trades <- sen_trades_RAW %>% select(-ptr_link)

## add house and senate labels
house_trades <- house_trades %>% mutate(branch = "house")
sen_trades <- sen_trades%>% mutate(branch = "senate")

## reconcile columns to prepare for rbind
colnames(house_trades)
colnames(sen_trades)

sen_trades <- sen_trades %>% mutate(disclosure_year = format(as.Date(disclosure_date, format = "%m/%d/%Y"), "%Y")) 
house_trades <- house_trades %>% mutate(asset_type = NA)
colnames(sen_trades)[10] <- "representative"
house_trades <- house_trades %>% mutate(comment = NA)
sen_trades <- sen_trades %>% mutate(district = NA)
sen_trades <- sen_trades %>% mutate(cap_gains_over_200_usd = NA)

sen_trades <- sen_trades %>% select(disclosure_year, disclosure_date, transaction_date, owner,
                      ticker, asset_description, asset_type, type, amount,  comment, 
                      representative, district, state, cap_gains_over_200_usd, 
                      industry, sector, party, branch)
house_trades <- house_trades %>% select(disclosure_year, disclosure_date, transaction_date, owner,
                        ticker, asset_description, asset_type, type, amount,  comment, 
                        representative, district, state, cap_gains_over_200_usd, 
                        industry, sector, party, branch)

## CHANGE to date CLASS 
house_trades$disclosure_date <- as.Date(house_trades$disclosure_date, format = "%m/%d/%Y")
house_trades$transaction_date <- as.Date(house_trades$transaction_date, format = "%Y-%m-%d")
sen_trades$disclosure_date <- as.Date(sen_trades$disclosure_date, format = "%m/%d/%Y")
sen_trades$transaction_date <- as.Date(sen_trades$disclosure_date, format = "%m/%d/%Y")
class(sen_trades$disclosure_date)

colnames(house_trades)
colnames(sen_trades)

# rbind together
gov_trades <- rbind(house_trades, sen_trades)

## REMOVE DATA 
# remove bad date data
gov_trades %>% filter(is.na(transaction_date)) # 4 transactions don't  have dates

gov_trades <- gov_trades %>% filter(transaction_date > "2012-01-01") # 7 transactions have misformatted dates 

# remove "exchange" trade type. I'm having trouble figuring out how to deal with this data and since its only 217 observations, I'll drop it
gov_trades <- gov_trades %>% filter(type != "exchange") #139
gov_trades <- gov_trades %>% filter(type != "Exchange") #78
gov_trades <- gov_trades %>% filter(type != "N/A") #793 transactions that have very little information, not ticker symbol, but has rep and date...I'll drop.

gov_trades$type %>% unique()

# remove NA Ticker symbols
gov_trades <- gov_trades %>% filter(!is.na(ticker)) %>%
  filter(ticker != "--") # 1369 transactions without tickers removed


### FORMATTING ###

# value range column ($1,000 - $14,999) to separate columns ($1,000)($14,999)
gov_trades <- gov_trades %>% 
  separate(col = amount,into =c("lower_bound", "upper_bound"), sep = " -")

# All Lower bounds are present, it's the upper bounds that are missing
gov_trades %>% filter(lower_bound == "")
gov_trades %>% filter(upper_bound == "") %>% select(lower_bound) %>% unique()

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

# the function stops fetching data after it runs into some weird error and then stops the following records from pulling...so identify those locations and split into sections
# it might have something to do with API limits: "Using the Public API (without authentication), you are limited to 2,000 requests per hour per IP (or up to a total of 48,000 requests a day)."
tickers1 <- tickers[1:500]
tickers2 <- tickers[501:930]
tickers2a <- tickers[931:1000]
tickers3 <- tickers[1001:1394]
tickers3a <- tickers[1395:1500]
tickers4 <- tickers[1501:1722]
tickers4a <- tickers[1723:1870]
tickers4b <- tickers[1871:2000]
tickers5 <- tickers[2001:2131]
tickers5a <- tickers[2132:2300]
tickers6 <- tickers[2301:2500]
tickers7 <- tickers[2501:2600]
tickers8 <- tickers[2601:2700]
tickers9 <- tickers[2701:ticker_length]




## Get Stock Prices
stocks1 <- yf_get(tickers = tickers1, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0) # 419/500 84% success rate in downloading tickers
stocks2 <- yf_get(tickers = tickers2, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0) # 358/430 83% success rate in downloading tickers
stocks2a <- yf_get(tickers = tickers2a, 
                   first_date = first.date,
                   last_date = last.date, 
                   freq_data = freq.data,
                   do_cache = TRUE,
                   thresh_bad_data = 0) # 61/70 87% success rate in downloading tickers
stocks3 <- yf_get(tickers = tickers3, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0) # 350/394 89% success rate in downloading tickers
stocks3a <- yf_get(tickers = tickers3a, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0) # 89/106 84% success rate in downloading tickers
stocks4 <- yf_get(tickers = tickers4, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0) # 192/222 86% success rate in downloading tickers
stocks4a <- yf_get(tickers = tickers4a, 
                   first_date = first.date,
                   last_date = last.date, 
                   freq_data = freq.data,
                   do_cache = TRUE,
                   thresh_bad_data = 0) # 135/148 91% success rate in downloading tickers
stocks4b <- yf_get(tickers = tickers4b, 
                   first_date = first.date,
                   last_date = last.date, 
                   freq_data = freq.data,
                   do_cache = TRUE,
                   thresh_bad_data = 0) # 115/130 88% success rate in downloading tickers
stocks5 <- yf_get(tickers = tickers5, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0) # 103/131 79% success rate in downloading tickers
stocks5a <- yf_get(tickers = tickers5a, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0) # 155/169 92% success rate in downloading tickers
stocks6 <- yf_get(tickers = tickers6, 
                  first_date = first.date,
                  last_date = last.date, 
                  freq_data = freq.data,
                  do_cache = TRUE,
                  thresh_bad_data = 0) # 171/200 86% success rate in downloading tickers
stocks7<- yf_get(tickers = tickers7, 
                 first_date = first.date,
                 last_date = last.date, 
                 freq_data = freq.data,
                 do_cache = TRUE,
                 thresh_bad_data = 0) # 87/100 87% success rate in downloading tickers
stocks8<- yf_get(tickers = tickers8, 
                 first_date = first.date,
                 last_date = last.date, 
                 freq_data = freq.data,
                 do_cache = TRUE,
                 thresh_bad_data = 0) # 81/100 81% success rate in downloading tickers
stocks9<- yf_get(tickers = tickers9, 
                 first_date = first.date,
                 last_date = last.date, 
                 freq_data = freq.data,
                 do_cache = TRUE,
                 thresh_bad_data = 0) # 43/56 77% success rate in downloading tickers

# BINDING stock tables together for ONE GIANT stocks table
stocks <- bind_rows(stocks1, stocks2, stocks2a, stocks3, stocks4, stocks4a, stocks4b, stocks5, stocks6, stocks7, stocks8, stocks9)

## APPENDING GOVERNMENT TRADES WITH PRICE DATA FROM THAT transaction DATE ##
gov_trades <- gov_trades %>% 
  left_join(y=stocks, by = c("transaction_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, asset_type, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, branch, .keep_all = TRUE)


  

#### DROP NA VALUES####
gov_trades <- gov_trades %>% filter(!is.na(price_adjusted)) # 3899 transactions were unable to match to a ticker on yahoo finance, DROPPING


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
fromD <- as.Date("2012-01-01")
toD <- as.Date("2030-12-31")

myholidays <- getHolidays(fromD, toD)        # US New York Stock Exchange

create.calendar(
  name = "MyNYSE", holidays = myholidays, weekdays = c("sunday", "saturday"),
  adjust.from = adjust.none, adjust.to = adjust.none)
calendars() # LOOK AT CALENDARS IN MY SYSTEM
####################


## Append the dates 5/30/90/365/todate
gov_trades <- gov_trades %>% 
  mutate(price_one_week_date = adjust.previous(transaction_date + weeks(1),"MyNYSE"),
         price_one_month_date = adjust.previous(transaction_date + months(1),"MyNYSE"),
         price_three_months_date = adjust.previous(transaction_date + months(3),"MyNYSE"),
         price_one_year_date = adjust.previous(transaction_date + years(1),"MyNYSE"),
         price_to_date_date = adjust.previous(as.Date(Sys.Date()), "MyNYSE")
         )

  # price_one_month later
gov_trades <- gov_trades %>%
  left_join(y=stocks, by = c("price_one_month_date" = "ref_date", "ticker" = "ticker"), keep = FALSE) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -price_one_month_date) %>%
  rename(price_one_month = price_adjusted.y, volume = volume.x, price = price_adjusted.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, asset_type, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, branch, .keep_all = TRUE) %>%
  # price_three_months later
  left_join(y=stocks, by = c("price_three_months_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -price_three_months_date) %>%
  rename(price_three_months = price_adjusted, volume = volume.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, asset_type, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, branch, .keep_all = TRUE) %>%
  # price_one_year later
  left_join(y=stocks, by = c("price_one_year_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -price_one_year_date) %>%
  rename(price_one_year = price_adjusted, volume = volume.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, asset_type, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, branch, .keep_all = TRUE) %>%
  # price_to_date
  left_join(y=stocks, by = c("price_to_date_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -price_to_date_date) %>%
  rename(price_to_date = price_adjusted, volume = volume.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, asset_type, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, branch, .keep_all = TRUE) %>%
  # price_one_week later
  left_join(y=stocks, by = c("price_one_week_date" = "ref_date", "ticker" = "ticker")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -price_one_week_date) %>%
  rename(price_one_week = price_adjusted, volume = volume.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, asset_type, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, branch, .keep_all = TRUE)
 
colnames(gov_trades)
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

## APPEND VOO + 7/30/60/365/todate
gov_trades <- gov_trades %>% 
  mutate(voo_one_week_date = adjust.previous(transaction_date + weeks(1),"MyNYSE"),
         voo_one_month_date = adjust.previous(transaction_date + months(1),"MyNYSE"),
         voo_three_months_date = adjust.previous(transaction_date + months(3),"MyNYSE"),
         voo_one_year_date = adjust.previous(transaction_date + years(1),"MyNYSE"),
         voo_to_date_date = adjust.previous(as.Date(Sys.Date()), "MyNYSE")) %>%
  
  # price one week later
  left_join(y=VOO, by = c("voo_one_week_date" = "ref_date")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -voo_one_week_date, -ticker.y) %>%
  rename(voo_one_week = price_adjusted, volume = volume.x, ticker = ticker.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, asset_type, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, branch, .keep_all = TRUE) %>%
  # price_one_month later
  left_join(y=VOO, by = c("voo_one_month_date" = "ref_date")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -voo_one_month_date, -ticker.y) %>%
  rename(voo_one_month = price_adjusted, volume = volume.x, ticker = ticker.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, asset_type, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, branch, .keep_all = TRUE) %>%
  # price_three_months later
  left_join(y=VOO, by = c("voo_three_months_date" = "ref_date")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -voo_three_months_date, -ticker.y) %>%
  rename(voo_three_months = price_adjusted, volume = volume.x, ticker = ticker.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, asset_type, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, branch, .keep_all = TRUE) %>%
  # price_one_year later
  left_join(y=VOO, by = c("voo_one_year_date" = "ref_date")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -voo_one_year_date, -ticker.y) %>%
  rename(voo_one_year = price_adjusted, volume = volume.x, ticker = ticker.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, asset_type, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, branch, .keep_all = TRUE) %>%
  # price_to_date
  left_join(y=VOO, by = c("voo_to_date_date" = "ref_date")) %>%
  select(-price_open, -price_high, -price_low, -price_close,-ret_closing_prices,-ret_adjusted_prices, -cumret_adjusted_prices, -volume.y, -voo_to_date_date, -ticker.y) %>%
  rename(voo_to_date = price_adjusted, volume = volume.x, ticker = ticker.x) %>%
  distinct(disclosure_year, disclosure_date, transaction_date, owner, ticker, asset_description, asset_type, type, lower_bound, upper_bound,
           representative, district, state, cap_gains_over_200_usd, industry, sector, party, branch, .keep_all = TRUE) 


## Reorder Columns

gov_trades <- gov_trades[,c("ticker", "disclosure_year", "disclosure_date", "transaction_date", 
              "lower_bound", "upper_bound", "volume", "price", "price_one_week", "price_one_month", 
              "price_three_months" ,"price_one_year" ,  "price_to_date","voo", "voo_one_week", "voo_one_month", "voo_three_months", "voo_one_year","voo_to_date",
              "asset_description","asset_type", "type","owner", "representative", "branch", "district", "state", "cap_gains_over_200_usd",
              "industry", "sector", "party", "comment")]



  
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

#### Stocks Truncated ####

stocks2 <- stocks %>% select(ticker, ref_date, price_adjusted)

## WRITE IT ALL TO.CSV ##
write.csv(gov_trades, 
          file = 'data/CLEAN-gov-trades.csv', 
          row.names = FALSE)

write.csv(stocks, 
          file = "data/CLEAN-stocks-data.csv", 
          row.names = FALSE)
write.csv(stocks2, 
          file = "data/CLEAN-stocks-trunc-data.csv", 
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
