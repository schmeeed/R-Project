library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(forcats)
library(scales)


setwd("/Users/bschmidt/Library/CloudStorage/GoogleDrive-schmidt5364@gmail.com/My\ Drive/#NYC_Data_Science_Academy/Projects/R-Project")
gov_trades <- readRDS(file = "shiny_data/gov_trades.rds")

VOO <- readRDS(file = "shiny_data/RAW-VOO.rds")

stocks <- readRDS("shiny_data/stocks.rds")

committees <- readRDS("shiny_data/CLEAN-committees-all.rds")

report_days <- read.csv("data/CLEAN-gov-report-days.csv", header = TRUE, sep = ",")
committee_2020 <- read.csv("data/CLEAN-committee-2020-data.csv", header = TRUE, sep = ",")
committee_2021 <- read.csv("data/CLEAN-committee-2021-data.csv", header = TRUE, sep = ",")
committee_2022 <- read.csv("data/CLEAN-committee-2022-data.csv", header = TRUE, sep = ",")

gov_trades <- gov_trades %>% mutate(disclosure_date = as.Date(disclosure_date),
                      transaction_date = as.Date(transaction_date))



summary(gov_trades)
# transaction date range from  2014-01-31 through 2023-02-06
# trade value range is from $1,000 through $50,000,000 with an average range of $22,517 - $80,306 and median of $1,000 - $15,000

#### trade count by buy/sell  ####
gov_trades %>% 
  count() %>% 
  ggplot(aes(x = "", y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = 3, color = "white") +
  labs(title = "Government Trades", 
       x = "", 
       y = "Count") +
  theme(legend.background = element_rect(fill = "#ECF0F5"),
        panel.background = element_rect(fill = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
  ggsave(filename = "presentation/images/transaction-count.png")

gov_trades %>% 
  count(type) %>% 
  ggplot(aes(x = "", y = n, fill = factor(type, levels = c("buy", "sell")))) +
  geom_col(position = "stack") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Government Trades by Type", 
       x = "", 
       y = "Count", 
       fill = "Trade Type") +
  theme(legend.background = element_rect(fill = "#ECF0F5"),
        panel.background = element_rect(fill = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
  ggsave(filename = "presentation/images/transaction-count-by-type.png")
  

#### S&P500 vs Congress Transaction Count ####  
daily_count <- gov_trades %>% 
    mutate(week_no = strftime(transaction_date,format = "%V")) %>%
    group_by(week_no,transaction_date) %>%
    count()
daily_count %>% arrange(desc(n))

VOO %>%
  filter(ref_date > min(gov_trades$transaction_date)-30, ref_date < max(gov_trades$transaction_date) +30) %>%
  left_join(y=daily_count, by = c("ref_date" = "transaction_date"), keep = TRUE) %>%
  ggplot() +
  geom_line(aes(x=ref_date, y = price_adjusted)) +
  geom_col(aes(x=ref_date, y =n)) +
  labs(x=NULL,y=NULL)+
  theme(legend.background = element_rect(fill = "#ECF0F5"),
        panel.background = element_rect(fill = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  ggtitle("S&P500 vs Congress Transaction Count")
ggsave(filename = "presentation/images/SP500-vs-Congress-Transaction-Count.png")



#### Cap_gains ####
gov_trades  %>% filter(!is.na(cap_gains_over_200_usd) & (type == "sell"))  %>% group_by(cap_gains_over_200_usd) %>% count() #5598

720/(720+4878) # .128 ~13% sell transactions were sold for a gain over $200


#### committees ####
com_filt_gov_trades <- gov_trades %>%
  filter((state == "CT") & (representative == "Joe Courtney"))

com_filt_gov_trades["transaction_date"] <- com_filt_gov_trades[format(com_filt_gov_trades$transaction_date, "%Y") == 2020,] %>%
  left_join(y=committee_2020, by = c("first_name" = "first_name", "last_name" = "last_name")) %>%
  select(-middle_name.y, - full_name.y)

temp <- gov_trades %>% filter((state == "CA") & (representative == "Nancy Pelosi")) %>% left_join(committee_2020, by = c("first_name" = "first_name", "last_name" = "last_name"))
num1 <- ncol(temp)
col_names1 <- colnames(temp)[34:(num1)]
col_indices1 <- which(colnames(temp) %in% col_names1)
temp %>% filter(format(temp$transaction_date, "%Y")==2020)
temp[format(temp$transaction_date, "%Y") %in% c(2021,2022),34:num1]


temp <- mutate_at(temp, vars(col_indices1), ifelse(format(temp$transaction_date, "%Y")==2020, temp$transaction_date, NA))






  #2021
  left_join(y=committee_2021, by = c("first_name" = "first_name", "last_name" = "last_name")) %>%
  select(-middle_name, - full_name) %>%
  #2022
  left_join(y=committee_2022, by = c("first_name" = "first_name", "last_name" = "last_name")) %>%
  select(-middle_name, - full_name)


  
  
  
#### DELAY BETWEEN REPORTING AND TRADING#####
gov_trades_delay <- gov_trades %>%
  mutate(delay = disclosure_date - transaction_date, limit = 45) %>%
  filter(delay > 0) 
#gov average delay
gov_trades_delay %>%
  summarise(average_delay = mean(delay), max_delay = max(delay), min_delay = min(delay))



# 1883 transactions were over 45 days and 
gov_trades_delay %>% filter(delay > 45) %>% nrow()

# top delayed individuals 
gov_trades_delay %>% filter(delay > 45) %>% 
  group_by(representative,party) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(5) %>%
  ggplot(aes(x=fct_reorder(representative, n), y=n, fill = party)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label =scales::comma(n)), hjust = 1.25, position = "stack", color = "white") +
  scale_fill_manual(values = c("Democrat" = "#629CFF", "Republican" = "#F8756D")) +
  theme(
    legend.background = element_rect(fill = "#ECF0F5"),
    panel.background = element_rect(fill = "#ECF0F5"),
    plot.background = element_rect(fill = "#ECF0F5"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()) +
  labs(x=NULL,y=NULL) +
  ggtitle("# Transactions Reported Late")
ggsave(filename = "presentation/images/DELAY-Transactions-Reported-Late.png")



gov_trades_delay %>% filter(delay > 45) %>% group_by(representative) %>% count() %>% nrow()  
# and NOBODY had a comment or exuse
gov_trades_delay %>% filter(delay > 45) %>% select(representative, comment) %>% filter(!is.na(comment))
# only 375 used the comment field anyway
gov_trades %>% filter(!is.na(comment) & (comment != "--")) %>% select(comment)


####TOP Active Traders count/Volume ####

# count
gov_trades %>% 
  group_by(representative,party) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10) %>%
  ggplot(aes(x=fct_reorder(representative, n), y=n, fill = party)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label =scales::comma(n)), hjust = 1.15, position = "stack", color = "white",  fontface = "bold") +
  scale_fill_manual(values = c("Democrat" = "#629CFF", "Republican" = "#F8756D")) +
  theme(
        legend.background = element_rect(fill = "#ECF0F5"),
        panel.background = element_rect(fill = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x=NULL,y=NULL) +
  ggtitle("Top 10 Traders by Count")
ggsave(filename = "presentation/images/TOP-traders-count.png")


# volume
gov_trades %>% 
  group_by(representative, party) %>% 
  summarise(lower_bound = sum(lower_bound), upper_bound = sum(upper_bound)) %>% 
  arrange(desc(upper_bound)) %>%
  mutate( range = paste0("$", round(lower_bound/1000000,1),"M"," - ", "$", round(upper_bound/1000000,1), "M")) %>%
  head(10) %>%
  ggplot(aes(x=fct_reorder(representative, lower_bound), y=lower_bound, fill = party)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = range), hjust = 1.15, position = "stack", color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Democrat" = "#629CFF", "Republican" = "#F8756D")) +
  theme(
    legend.background = element_rect(fill = "#ECF0F5"),
    panel.background = element_rect(fill = "#ECF0F5"),
    plot.background = element_rect(fill = "#ECF0F5"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()) +
  labs(x=NULL,y=NULL) +
  ggtitle("Top 10 Traders by Volume Traded")
ggsave(filename = "presentation/images/TOP-traders-volume.png")
  





#### Buy performance vs Sell Performance ####

#BUY
gov_trades %>%
  mutate(buy_performance_one_month = ifelse(type == "buy", price_one_month-price, NA),
         buy_performance_three_months = ifelse(type == "buy", price_three_months-price, NA),
         buy_performance_one_year = ifelse(type == "buy", price_one_year-price, NA)) %>%
  select(representative, buy_performance_one_month, buy_performance_three_months, buy_performance_one_year)

gov_trades %>%
  filter(type == "sell") %>%
  mutate(sell_performance_one_month = price_one_month-price,
         sell_performance_three_months = price_three_months-price,
         sell_performance_one_year = price_one_year-price) %>%
  select(representative, sell_performance_one_month, sell_performance_three_months, sell_performance_one_year)


####percentage gain/loss####
install.packages("scales")
library(scales)


## percentage format
voo_summary_buy <- gov_trades %>% 
  mutate(voo_return_one_month = (voo_one_month-voo)/voo,
         voo_return_three_months = (voo_three_months-voo)/voo,
         voo_return_one_year = (voo_one_year-voo)/voo) %>% 
  filter(type == "buy") %>%
  summarise(ticker = "S&P: BUY",
            one_month = percent(mean(voo_return_one_month, na.rm = TRUE), accuracy = .01),
            three_months = percent(mean(voo_return_three_months, na.rm = TRUE), accuracy = .01),
            one_year = percent(mean(voo_return_one_year, na.rm = TRUE), accuracy = .01)
            )
voo_summary_sell <- gov_trades %>% 
  mutate(voo_return_one_month = (voo_one_month-voo)/voo,
         voo_return_three_months = (voo_three_months-voo)/voo,
         voo_return_one_year = (voo_one_year-voo)/voo) %>% 
  filter(type == "sell") %>%
  summarise(ticker = "S&P: SELL",
            one_month = percent(mean(voo_return_one_month, na.rm = TRUE), accuracy = .01),
            three_months = percent(mean(voo_return_three_months, na.rm = TRUE), accuracy = .01),
            one_year = percent(mean(voo_return_one_year, na.rm = TRUE), accuracy = .01)
  )
price_summary_buy <- gov_trades %>% 
  mutate(price_return_one_month = (price_one_month-price)/price,
         price_return_three_months = (price_three_months-price)/price,
         price_return_one_year = (price_one_year-price)/price) %>%
  filter(type == "buy") %>%
  summarise(ticker = "Gov_trades: BUY",
            one_month = percent(mean(price_return_one_month, na.rm = TRUE), accuracy = .01),
            three_months = percent(mean(price_return_three_months, na.rm = TRUE), accuracy = .01),
            one_year = percent(mean(price_return_one_year, na.rm = TRUE), accuracy = .01)
  )
price_summary_sell <- gov_trades %>% 
  mutate(price_return_one_month = (price_one_month-price)/price,
         price_return_three_months = (price_three_months-price)/price,
         price_return_one_year = (price_one_year-price)/price) %>%
  filter(type == "sell") %>%
  summarise(ticker = "Gov_trades: SELL",
            one_month = percent(mean(price_return_one_month, na.rm = TRUE), accuracy = .01),
            three_months = percent(mean(price_return_three_months, na.rm = TRUE), accuracy = .01),
            one_year = percent(mean(price_return_one_year, na.rm = TRUE), accuracy = .01)
  )


combined_summary <- bind_rows(voo_summary_buy, price_summary_buy, voo_summary_sell, price_summary_sell)
combined_summary


5.66-4.85
23.74-19.36


## Numeric Values
voo_summary_num_buy <- gov_trades %>% 
  mutate(voo_return_one_week = (voo_one_week-voo)/voo,
         voo_return_one_month = (voo_one_month-voo)/voo,
         voo_return_three_months = (voo_three_months-voo)/voo,
         voo_return_one_year = (voo_one_year-voo)/voo,
         voo_return_to_date = (voo_to_date-voo)/voo) %>%
  filter(type == "buy") %>%
  summarise(ticker = "S&P 500",
            type="Buy",
            one_week = mean(voo_return_one_week, na.rm = TRUE),
            one_month = mean(voo_return_one_month, na.rm = TRUE),
            three_months = mean(voo_return_three_months, na.rm = TRUE),
            one_year = mean(voo_return_one_year, na.rm = TRUE),
            to_date = mean(voo_return_to_date,na.rm=TRUE)
  )
voo_summary_num_sell <- gov_trades %>% 
  mutate(voo_return_one_week = (voo_one_week-voo)/voo,
         voo_return_one_month = (voo_one_month-voo)/voo,
         voo_return_three_months = (voo_three_months-voo)/voo,
         voo_return_one_year = (voo_one_year-voo)/voo,
         voo_return_to_date = (voo_to_date-voo)/voo) %>%
  filter(type == "sell") %>%
  summarise(ticker = "S&P 500",
            type="Sell",
            one_week = mean(voo_return_one_week, na.rm = TRUE),
            one_month = mean(voo_return_one_month, na.rm = TRUE),
            three_months = mean(voo_return_three_months, na.rm = TRUE),
            one_year = mean(voo_return_one_year, na.rm = TRUE),
            to_date = mean(voo_return_to_date,na.rm=TRUE)
  )
price_summary_num_buy<- gov_trades %>% 
  mutate(price_return_one_week = (price_one_week-price)/price,
         price_return_one_month = (price_one_month-price)/price,
         price_return_three_months = (price_three_months-price)/price,
         price_return_one_year = (price_one_year-price)/price,
         price_return_to_date = (price_to_date-price)/price) %>%
  filter(type == "buy") %>%
  summarise(ticker = "Congress",
            type="Buy",
            one_week = mean(price_return_one_week, na.rm = TRUE),
            one_month = mean(price_return_one_month, na.rm = TRUE),
            three_months = mean(price_return_three_months, na.rm = TRUE),
            one_year = mean(price_return_one_year, na.rm = TRUE),
            to_date = mean(price_return_to_date, na.rm = TRUE)
  )
price_summary_num_sell<- gov_trades %>% 
  mutate(price_return_one_week = (price_one_week-price)/price,
         price_return_one_month = (price_one_month-price)/price,
         price_return_three_months = (price_three_months-price)/price,
         price_return_one_year = (price_one_year-price)/price,
         price_return_to_date = (price_to_date-price)/price) %>%
  filter(type == "sell") %>%
  summarise(ticker = "Congress",
            type="Sell",
            one_week = mean(price_return_one_week, na.rm = TRUE),
            one_month = mean(price_return_one_month, na.rm = TRUE),
            three_months = mean(price_return_three_months, na.rm = TRUE),
            one_year = mean(price_return_one_year, na.rm = TRUE),
            to_date = mean(price_return_to_date, na.rm = TRUE)
  )
combined_summary_num <- bind_rows(voo_summary_num_buy, price_summary_num_buy, voo_summary_num_sell, price_summary_num_sell)
combined_summary_num

#visual

combined_summary_num %>%
  pivot_longer(cols = 3:7, names_to = "period", values_to = "returns") %>%
  ggplot(aes(x=fct_reorder(period,returns, .desc = TRUE), y=returns, fill = ticker)) +
  geom_col(position = "dodge") +
  geom_text(aes(label=paste0(round(returns*100,2),"%"), y=returns, group=ticker),
            position=position_dodge(width=0.9), size=3, vjust=-0.5) +
  facet_grid(rows = vars(type)) +
  scale_fill_manual(values = c("Congress" = "#F8756D", "S&P 500" = "#00BA42")) +
  theme(
    legend.background = element_rect(fill = "#ECF0F5"),
    panel.background = element_rect(fill = "#ECF0F5"),
    plot.background = element_rect(fill = "#ECF0F5"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12)) +
  scale_y_continuous(labels = function(x) paste0(round(x*100,1), "%")) +
  labs(x=NULL,y=NULL) +
  ggtitle("Average Return: Congress vs. S&P 500")
ggsave(filename = "presentation/images/Average-Return-Congress-vs-SP500.png")



################################################################################  

##### of trades per party ####
gov_trades %>% 
  filter(!is.na(party), (party != "Independent"), (party != "Libertarian")) %>%
  group_by(party) %>%
  count() %>%
  arrange(n) %>%
  ggplot(aes(x=fct_reorder(party, -n), y = n)) +
  geom_col(aes(fill = party)) +
  geom_text(aes(label = scales::comma(n), vjust = 2.2), color = "white", size = 12, fontface = "bold") +
  labs(x=NULL,y=NULL) + 
  scale_fill_manual(values = c("Democrat" = "#629CFF", "Republican" = "#F8756D")) +
  theme(legend.position = "none",
      legend.background = element_rect(fill = "#ECF0F5"),
      panel.background = element_rect(fill = "#ECF0F5"),
      plot.background = element_rect(fill = "#ECF0F5"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text = element_text(size = 12))
ggsave(filename = "presentation/images/trades-per-party.png")

##### of trades per sector####
gov_trades %>% 
  group_by(sector, party) %>%
  filter(!is.na(sector), party != "Libertarian", party != "Independent") %>%
  count() %>%
  arrange(party, -n) %>%
  ggplot(aes(x=fct_reorder(sector, n), y=n, fill = party)) +
  geom_col() +
  coord_flip() +
  geom_text(data = . %>% filter(n > 200), aes(label =scales::comma(n)), hjust = 1.15, position = "stack", color = "white") +
  scale_fill_manual(values = c("Democrat" = "#629CFF", "Republican" = "#F8756D")) +
  theme(legend.position = "none",
        legend.background = element_rect(fill = "#ECF0F5"),
        panel.background = element_rect(fill = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x=NULL,y=NULL)
ggsave(filename = "presentation/images/trades-per-sector.png")
  

#### Transactions_grouped_by ####
# group_by Year
gov_trades %>%
  mutate(transaction_year = factor(year(transaction_date))) %>%
  filter(!is.na(sector), party != "Libertarian", party != "Independent") %>%
  group_by(transaction_year, party) %>%
  count() %>%
  ggplot(aes(x=transaction_year,y=n, fill = party)) + 
  geom_col() +
  geom_text(data = . %>% filter(n>180), aes(label = scales::comma(n)), position = "stack", vjust = 1.5, color = "white") +
  ggtitle("Transactions Grouped by Year") +
  scale_fill_manual(values = c("Democrat" = "#629CFF", "Republican" = "#F8756D")) +
  theme(
    legend.background = element_rect(fill = "#ECF0F5"),
    panel.background = element_rect(fill = "#ECF0F5"),
    plot.background = element_rect(fill = "#ECF0F5"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12)) +
  labs(x=NULL,y=NULL)
ggsave(filename = "presentation/images/Transactions-grouped-by-year.png")

# group_by Month
gov_trades %>%
  mutate(transaction_month = factor(month(transaction_date))) %>%
  filter(!is.na(sector), party != "Libertarian", party != "Independent") %>%
  group_by(transaction_month, party) %>%
  count() %>%
  ggplot(aes(x=transaction_month,y=n, fill = party)) + 
  geom_col() +
  geom_text(aes(label = scales::comma(n)), position = "stack", vjust = 1.5, color = "white") +
  ggtitle("Transactions Grouped by Month Name") +
  scale_fill_manual(values = c("Democrat" = "#629CFF", "Republican" = "#F8756D")) +
  theme(
    legend.background = element_rect(fill = "#ECF0F5"),
    panel.background = element_rect(fill = "#ECF0F5"),
    plot.background = element_rect(fill = "#ECF0F5"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12)) +
  labs(x=NULL,y=NULL)
ggsave(filename = "presentation/images/Transactions-grouped-by-month.png")


# group_by day
gov_trades %>%
  mutate(transaction_day = factor(day(transaction_date))) %>%
  filter(!is.na(sector), party != "Libertarian", party != "Independent") %>%
  group_by(transaction_day, party) %>%
  count() %>%
  ggplot(aes(x=transaction_day,y=n, fill = party)) + 
  geom_col() +
  geom_text(aes(label = scales::comma(n)), position = "stack", vjust = 1.5, color = "white", size = 2.5) +
  ggtitle("Transactions Grouped by Day of Month") +
  scale_fill_manual(values = c("Democrat" = "#629CFF", "Republican" = "#F8756D")) +
  theme(
    legend.background = element_rect(fill = "#ECF0F5"),
    panel.background = element_rect(fill = "#ECF0F5"),
    plot.background = element_rect(fill = "#ECF0F5"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12)) +
  labs(x=NULL,y=NULL)
ggsave(filename = "presentation/images/Transactions-grouped-by-day-of-month.png")


#group_by Day of Week
gov_trades %>%
  mutate(transaction_weekday = factor(weekdays(transaction_date))) %>%
  filter(!is.na(sector), party != "Libertarian", party != "Independent") %>%
  group_by(transaction_weekday, party) %>%
  count() %>%
  ggplot(aes(x=transaction_weekday,y=n, fill = party)) + 
  geom_col() +
  geom_text(aes(label = scales::comma(n)), position = "stack", vjust = 1.5, color = "white") +
  ggtitle("Transactions Grouped by Day of Week") +
  scale_fill_manual(values = c("Democrat" = "#629CFF", "Republican" = "#F8756D")) +
  theme(
    legend.background = element_rect(fill = "#ECF0F5"),
    panel.background = element_rect(fill = "#ECF0F5"),
    plot.background = element_rect(fill = "#ECF0F5"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12)) +
  labs(x=NULL,y=NULL)
ggsave(filename = "presentation/images/Transactions-grouped-by-day-of-week.png")

#### SECTOR RSUM ####
gov_trades %>%
  filter(!is.na(sector), party != "Libertarian", party != "Independent") %>%
  group_by(sector) %>%
  arrange(transaction_date) %>%
  mutate(count = 1) %>%
  mutate(rsum=cumsum(count)) %>%
  ggplot(aes(x=transaction_date, y = rsum)) +
  geom_line(aes(color=sector)) +
  ggtitle("Running Count of Transactions by Sector")
ggsave(filename = "presentation/images/Running-Sum-of-Transactions-grouped-by-sector.png")

#### Top Trades ####
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




diff_gov_trades <- gov_trades %>%
  mutate(price_one_week_date = adjust.previous(transaction_date + weeks(1),"MyNYSE"),
         price_one_month_date = adjust.previous(transaction_date + months(1),"MyNYSE"),
         price_three_months_date = adjust.previous(transaction_date + months(3),"MyNYSE"),
         price_one_year_date = adjust.previous(transaction_date + years(1),"MyNYSE"),
         price_to_date_date = adjust.previous(as.Date(Sys.Date()), "MyNYSE")) %>%
  mutate(diff_week = (price_one_week-price)/price,
         diff_month = (price_one_month-price)/price,
         diff_three_months = (price_three_months-price)/price,
         diff_one_year = (price_one_year-price)/price,
         diff_to_date = (price_to_date-price)/price)

my_vector <- c(diff_week, diff_month, diff_three_months, diff_one_year, diff_to_date)

top_performers <- diff_gov_trades %>%
    filter(type == "buy") %>%
    # arrange(-diff_week) %>%
    # arrange(-diff_month) %>%
    arrange(diff_three_months) %>%
    # arrange(diff_one_year) %>%
    # arrange(diff_to_date) %>%
    select(ticker, representative,state, diff_week,diff_month,diff_three_months, transaction_date,diff_one_year,diff_to_date) %>%
    head(10)

# visuals  

for (i in 1:nrow(top_performers)){
  
  rep_filt_trades <- diff_gov_trades %>% filter(representative == top_performers[[i,"representative"]])
  rep_filt_ticker <- top_performers[[i,"ticker"]]
  
  d1 <- stocks %>%
    filter(ticker %in% rep_filt_ticker) %>%
    left_join(y=rep_filt_trades, by = c("ref_date" = "transaction_date", "ticker" = "ticker"), keep = TRUE) %>%
    group_by(ticker.x) %>%
    filter(ref_date >= (min(rep_filt_trades$transaction_date)-30), ref_date <= (max(rep_filt_trades$transaction_date)+30))
  
  d2 <- highlight_key(d1, ~ticker.x)
  
  p <- ggplot(d2, aes(group = ticker.x, color = type, shape = type)) +
    geom_line(aes(x=ref_date, y=price_adjusted)) +
    geom_point(aes(x=transaction_date, y= price_adjusted, size = 12)) +
    theme(legend.background = element_rect(fill = "#ECF0F5"),
          panel.background = element_rect(fill = "#ECF0F5"),
          plot.background = element_rect(fill = "#ECF0F5"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
  
    labs(x=element_blank(), y = element_blank()) +
    ggtitle(paste0(top_performers[[i,"representative"]], ": ", top_performers[[i,"ticker"]], "   ", scales::percent(top_performers[[i,"diff_month"]],4), "     ", top_performers[[i,"sector"]]))
  ggsave(filename = paste0("EDA/buy-month/buy-month",i,".png"), plot = p)

}



#### TOP 5 SHORT/MED/LONG TERM TRADERS ####
colnames(gov_trades)

gov_trades %>% 
  group_by(representative) %>% 
  filter(type == "buy") %>%
  mutate(price_return_one_week = (price_one_week-price)/price,
         price_return_one_month = (price_one_month-price)/price,
         price_return_three_months = (price_three_months-price)/price,
         price_return_one_year = (price_one_year-price)/price,
         voo_return_one_week = (voo_one_week-voo)/voo,
         voo_return_one_month = (voo_one_month-voo)/voo,
         voo_return_three_months = (voo_three_months-voo)/voo,
         voo_return_one_year = (voo_one_year-voo)/voo) %>%
  summarise(avg_one_week = weighted.mean(x=price_return_one_week,w=lower_bound,na.rm=TRUE),
            avg_one_month = weighted.mean(x=price_return_one_month,w=lower_bound, na.rm = TRUE),
            avg_three_months = weighted.mean(price_return_three_months, w=lower_bound, na.rm = TRUE),
            avg_one_year = weighted.mean(price_return_one_year,w=lower_bound, na.rm = TRUE)) %>%
  ## un-comment based on which graph you want
  
  # # one week
  # arrange(desc(avg_one_week)) %>%
  # 
  # # one Month
  # arrange(desc(avg_one_month)) %>%
  # 
  # # three months
  # arrange(desc(avg_three_months)) %>%
  # 
# one year
arrange(desc(avg_one_year)) %>%
  
  pivot_longer(cols = 2:5, names_to = "period", values_to = "returns") %>%
  head(16) %>%
  mutate(period = factor(period, levels = c("avg_one_year", "avg_three_months", "avg_one_month", "avg_one_week"))) %>%
  ggplot(aes(x=fct_reorder(representative, returns), y= returns, fill = period)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percent(returns, accuracy = 1)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  labs(x=NULL,y=NULL)+
  coord_flip()


############################################################

economic_indic <- read.csv("https://brian-ralston-r-project.s3.amazonaws.com/economic-indicators-reports-2020-2022.csv", 
          skip = 9, header = TRUE)


############################################# APP APP APP####
#### Trade Activity - APP ####
VOO <- read.csv(file = "data/RAW-VOO.csv")
library(zoo)
library(scales)


rep_plot <- geom_bar(data = rep_data, aes(x = quarter_year, fill = type)) + 
  scale_fill_manual(values = c("buy" = "#00BA38", "sell" = "#F8766D")) +
  scale_x_discrete(limits = unique(gov_trades$quarter_year)) + 
  scale_y_continuous(sec.axis = sec_axis(~ . * 10, name = "S&P500")) +
  labs(x=element_blank(), y = element_blank()) + 
  ggtitle("Trade Activity") +
  theme(panel.background = element_rect(fill = "white"), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_text(stat='count', aes(label=..count..),position=position_stack(vjust=0.5))

VOO_data <- VOO %>%
  filter(ref_date >= min(rep_data$transaction_date),ref_date <= max(rep_data$transaction_date)) %>%
  mutate( ref_date = as.Date(ref_date),quarter_year = format(as.yearqtr(ref_date, format = "%Y-%m-%d"), "%Y Q%q"))
voo_plot <- ggplot() + geom_line(data = VOO_data, aes(x=ref_date, y = price_adjusted, group = 1)) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y")



gov_trades %>%
  filter((state == "CA") & (representative == "Nancy Pelosi")) %>%
  mutate(transaction_date = as.Date(transaction_date)) %>%
  arrange(transaction_date) %>%
  mutate(quarter = as.yearqtr(transaction_date, format = "%q"),
         month = format(transaction_date, "%m-%Y")) %>%
  group_by(month, type) %>%
  count() %>%
  ggplot() +
  geom_col(aes(x=month, y = n, fill = type)) +
  geom_line(data = VOO_data, aes(x=ref_date, y = price_adjusted, group = 1))

rep_data <- gov_trades %>%
  filter((state == "CA") & (representative == "Nancy Pelosi")) %>%
  mutate(transaction_date = as.Date(transaction_date)) %>%
  arrange(transaction_date) %>%
  mutate(quarter = as.yearqtr(transaction_date, format = "%q"),
         month = format(transaction_date, "%m-%Y"))



VOO_data %>% 
  full_join(y=rep_data, by = c("ref_date" = "transaction_date"),keep = TRUE) %>%
  ggplot() +
  geom_line(aes(x=ref_date, y = price_adjusted, group = 1)) +
  geom_bar(aes(x=transaction_date, color = type)) +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "rep_data"))

rep_data_clean <- rep_data %>% 
  filter(!is.na(rep_data)) %>% 
  filter(!is.infinite(rep_data))

joined_data <- VOO_data %>% 
  full_join(y=rep_data, by = c("ref_date" = "transaction_date"),keep = TRUE) 

joined_data  %>%
  ggplot() +
  geom_line(aes(x=ref_date, y = price_adjusted, group = 1)) +
  geom_point(aes(x=transaction_date, y=price_adjusted, color = type))


  geom_bar(aes(x=transaction_date, fill = type)) +
  scale_y_continuous(name = "price_adjusted", limits = c(min(VOO_data$price_adjusted), max(VOO_data$price_adjusted))) +
  scale_y_continuous(limits = c(0, max(joined_data)), sec.axis = sec_axis(~ ., name = "rep_data"))



  

  ggplot() + 
  geom_bar(aes(x = transaction_date, fill = type)) + 
  scale_fill_manual(values = c("buy" = "#01FF70", "sell" = "#DD4B39")) +
  scale_x_discrete(limits = unique(gov_trades$quarter_year)) + 
  
  ggtitle("Trade Activity") +
  theme(legend.background = element_rect(fill = "#ECF0F5"),
        panel.background = element_rect(fill = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  geom_text(stat='count', aes(label=after_stat(count)),position=position_stack(vjust=0.5))

#### Top 5 Stocks Visual####
  install.packages("gghighlight")
  library(gghighlight) 
  
  million_format <- function(x) {
    ifelse(x > 999999,
           paste0("$", format(round(x/1e6, 1), nsmall = 1), "M"),
           paste0("$", format(x, big.mark = ",")))
  }

  rep_top_tickers <-   gov_trades %>% 
    filter((state == "CA") & (representative == "Nancy Pelosi")) %>%
    group_by(ticker, party) %>% 
    summarise(lower_bound = sum(lower_bound),
              upper_bound = sum(upper_bound),
              count = n()) %>% 
    arrange(desc(upper_bound)) %>%
    mutate(range = paste0(million_format(lower_bound)," - ", million_format(upper_bound), "       (", count,")")) %>%
    head(5)
  
rep_top_tickers %>%
    ggplot(aes(x=fct_reorder(ticker, lower_bound), y=lower_bound, fill = ticker)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = range), hjust = 1.15, position = "stack", color = "white", fontface = "bold") +
    theme(
      legend.background = element_rect(fill = "#ECF0F5"),
      panel.background = element_rect(fill = "#ECF0F5"),
      plot.background = element_rect(fill = "#ECF0F5"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text = element_text(size = 12),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
    labs(x=NULL,y=NULL) +
    scale_y_log10() +
    ggtitle("Top 5 Stocks by Volume")
  

  # rep_top_tickers %>% 
  #   ggplot(aes(x=reorder(ticker, n), y = n, color = ticker, label = paste0(ticker," (",n,")"))) +
  #   geom_segment(aes(xend = ticker, yend = 0)) +
  #   geom_point(size = 4) +
  #   scale_y_continuous(limits = c(0,max(top_5_stocks$n)*1.2)) +
  #   ggtitle("Top 5 Stocks") +
  #   theme(legend.background = element_rect(fill = "#ECF0F5"),
  #         panel.background = element_rect(fill = "#ECF0F5"),
  #         plot.background = element_rect(fill = "#ECF0F5"),
  #         panel.grid.minor = element_blank(), 
  #         panel.grid.major = element_blank(),
  #         axis.text.x=element_blank(),
  #         axis.ticks.x=element_blank(),
  #         axis.text.y=element_blank(),
  #         axis.ticks.y=element_blank()) +
  #   labs(x=element_blank(), y = element_blank()) +
  #   geom_text(hjust = -.5) +
  #   theme(legend.position="none") +
  #   coord_flip()
  
  #### top_5_sectors visual####  
  
  # rep_top_sectors <- gov_trades %>%
  #   filter((state == "CA") & (representative == "Nancy Pelosi")) %>%
  #   mutate(transaction_date = as.Date(transaction_date)) %>%
  #   group_by(sector) %>%
  #   count() %>% 
  #   arrange(desc(n)) %>%
  #   head(5)
  
  rep_top_sectors <- gov_trades %>% 
    filter((state == "CA") & (representative == "Nancy Pelosi"), !is.na(sector)) %>%
    group_by(sector) %>% 
    summarise(lower_bound = sum(lower_bound),
              upper_bound = sum(upper_bound),
              count = n()) %>% 
    arrange(desc(upper_bound)) %>%
    mutate(range = paste0(million_format(lower_bound)," - ", million_format(upper_bound), "       (", count,")")) %>%
    head(5)
  
  rep_top_sectors %>%
    ggplot(aes(x=fct_reorder(sector, lower_bound), y=lower_bound)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = range), hjust = 1.15, position = "stack", color = "white", fontface = "bold") +
    theme(
      legend.background = element_rect(fill = "#ECF0F5"),
      panel.background = element_rect(fill = "#ECF0F5"),
      plot.background = element_rect(fill = "#ECF0F5"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text = element_text(size = 12),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
    labs(x=NULL,y=NULL) +
    scale_y_log10() +
    ggtitle("Top 5 Sectors by Volume")
  
  
  rep_top_sectors %>% 
    ggplot(aes(x=reorder(sector, n), y = n, color = sector, label = paste0(sector," (",n,")"))) +
    geom_segment(aes(xend = sector, yend = 0)) +
    geom_point(size = 4) +
    scale_y_continuous(limits = c(0,max(rep_top_sectors$n)*1.2)) +
    ggtitle("Top 5 Stocks") +
    theme(legend.background = element_rect(fill = "#ECF0F5"),
          panel.background = element_rect(fill = "#ECF0F5"),
          plot.background = element_rect(fill = "#ECF0F5"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    labs(x=element_blank(), y = element_blank()) +
    geom_text(hjust = -.5) +
    theme(legend.position="none") +
    coord_flip()

  #### top stocks visuals ####  
  colnames(d1)
  d1 <- stocks %>%
    filter(ticker %in% c(rep_top_tickers$ticker)) %>%
    left_join(y=rep_data, by = c("ref_date" = "transaction_date", "ticker" = "ticker"), keep = TRUE) %>%
    group_by(ticker.x) %>%
    filter(ref_date >= (min(rep_data$transaction_date)-30), ref_date <= (max(rep_data$transaction_date)+30)) %>%
    mutate(range  = paste0(million_format(lower_bound), "-", million_format(upper_bound)))
  d1$range <- as.character(d1$range)
  

  d2 <- highlight_key(d1, ~ticker.x)
  p <- ggplot(d2, aes(x=ref_date, y=price_adjusted, group = ticker.x, color = ticker.x, tooltip = range)) +
    geom_line() +
    geom_point(aes(x=transaction_date, y=price_adjusted, shape = type, size = volume, color = ticker.x)) +
    theme(legend.background = element_rect(fill = "#ECF0F5"),
          panel.background = element_rect(fill = "#ECF0F5"),
          plot.background = element_rect(fill = "#ECF0F5"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank()) +
    labs(x=element_blank(), y = element_blank())
  cols2 <- toRGB(RColorBrewer::brewer.pal(3, "Dark2"), 0.5)
  gg <- ggplotly(p, tooltip = c("y", "x", "range"), text = c("date", "Price", "Volume")) %>% layout(showlegend = FALSE)
  highlight(gg, on = "plotly_hover", color = cols2, dynamic = TRUE, debounce = 50)
  

  
  
  ##PLOTLY EXAMPLE ##
  
  d <- highlight_key(txhousing, ~city)
  p <- ggplot(d, aes(date, median, group = city)) + geom_line()
  gg <- ggplotly(p, tooltip = "city") 
  highlight(gg, dynamic = TRUE)
  
  # supply custom colors to the brush 
  cols <- toRGB(RColorBrewer::brewer.pal(3, "Dark2"), 0.5)
  highlight(gg, on = "plotly_hover", color = cols, dynamic = TRUE)
  
  # Use attrs_selected() for complete control over the selection appearance
  # note any relevant colors you specify here should override the color argument
  s <- attrs_selected(
    showlegend = TRUE,
    mode = "lines+markers",
    marker = list(symbol = "x")
  )
  
  highlight(layout(gg, showlegend = TRUE), selected = s)
  
#### Trader Performance - APP ####

buy_perf_avg <- gov_trades %>%
  mutate(price_performance_one_month = (price_one_month-price)/price,
         price_performance_three_months = (price_three_months-price)/price,
         price_performance_one_year = (price_one_year-price)/price,
         voo_performance_one_month = (voo_one_month-voo)/price,
         voo_performance_three_months = (voo_three_months-voo)/price,
         voo_performance_one_year = (voo_one_year-voo)/price) %>%
  filter(representative == "Nancy Pelosi", type == "buy") %>%
  summarise(type = "buy",
            gov_short = mean(price_performance_one_month, na.rm = TRUE),
            voo_short = mean(voo_performance_one_month, na.rm = TRUE),
            gov_med = mean(price_performance_three_months, na.rm = TRUE),
            voo_med = mean(voo_performance_three_months, na.rm = TRUE),
            gov_long = mean(price_performance_one_year, na.rm = TRUE),
            voo_long = mean(voo_performance_one_year, na.rm = TRUE)) %>%
  pivot_longer(cols = 2:7, names_to = "trader", values_to = "values") %>% 
  mutate(trader_cat = ifelse(startsWith(trader, "gov"), "Government", "S&P 500"),
         type_cat = case_when(endsWith(trader, "short") ~ "Short",
                              endsWith(trader, "med") ~ "Medium",
                              endsWith(trader, "long") ~ "Long"))

sell_perf_avg <- gov_trades %>%
  mutate(price_performance_one_month = (price_one_month-price)/price,
         price_performance_three_months = (price_three_months-price)/price,
         price_performance_one_year = (price_one_year-price)/price,
         voo_performance_one_month = (voo_one_month-voo)/price,
         voo_performance_three_months = (voo_three_months-voo)/price,
         voo_performance_one_year = (voo_one_year-voo)/price) %>%
  filter(representative == "Nancy Pelosi", type == "sell") %>%
  summarise(type = "sell",
            gov_short = mean(price_performance_one_month, na.rm = TRUE),
            voo_short = mean(voo_performance_one_month, na.rm = TRUE),
            gov_med = mean(price_performance_three_months, na.rm = TRUE),
            voo_med = mean(voo_performance_three_months, na.rm = TRUE),
            gov_long = mean(price_performance_one_year, na.rm = TRUE),
            voo_long = mean(voo_performance_one_year, na.rm = TRUE)) %>%
  pivot_longer(cols = 2:7, names_to = "trader", values_to = "values") %>% 
  mutate(trader_cat = ifelse(startsWith(trader, "gov"), "Government", "S&P 500"),
         type_cat = case_when(endsWith(trader, "short") ~ "Short",
                              endsWith(trader, "med") ~ "Medium",
                              endsWith(trader, "long") ~ "Long"))

buy_sell_perf_avg <- bind_rows(buy_perf_avg, sell_perf_avg)
buy_sell_perf_avg

# visuals
pal <- c("Winner" = "#01FF70",
         "Loser" = "grey")

buy_perf_avg %>% 
  group_by(trader_cat) %>%
  summarise(values = mean(values)) %>%
  mutate(rank = rank(-values),
         winner = ifelse(rank == 1,"Winner", "Loser")) %>%
  ggplot(aes(x=trader_cat, y=values, fill = winner)) + 
  geom_col(aes(size = 4)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x=element_blank(), y = element_blank()) + 
  ggtitle("Buy Timing Performance") +
  geom_text(aes(label = paste0(round(values * 100, 2), "%")), 
            vjust = -1) +
  theme(legend.background = element_rect(fill = "#ECF0F5"),
      panel.background = element_rect(fill = "#ECF0F5"),
      plot.background = element_rect(fill = "#ECF0F5"),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      legend.position = "none") +
  scale_fill_manual(values = pal)

sell_perf_avg %>% 
  group_by(trader_cat) %>%
  summarise(values = mean(values)) %>%
  mutate(rank = rank(values),
         winner = ifelse(rank == 1,"Winner", "Loser")) %>%
  ggplot(aes(x=trader_cat, y=values, fill = winner)) + 
  geom_col(aes(size = 4)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x=element_blank(), y = element_blank()) + 
  ggtitle("Sell Timing Performance") +
  geom_text(aes(label = paste0(round(values * 100, 2), "%")), 
            vjust = -1) +
  theme(legend.background = element_rect(fill = "#ECF0F5"),
        panel.background = element_rect(fill = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = pal)

buy_sell_perf_avg %>% 
  ggplot(aes(x=type, y=values , fill = trader_cat)) +
  geom_col(position = "dodge") +
  facet_grid(cols = vars(type_cat))


gov_trades %>%
  filter(representative == "Nancy Pelosi") %>%
  mutate(transaction_date = as.Date(transaction_date)) %>%
  arrange(transaction_date) %>%
  mutate(quarter_year = format(as.yearqtr(transaction_date, format = "%Y-%m-%d"), "%Y Q%q")) %>%
  ggplot(aes(x = quarter_year, fill = type)) + 
  geom_histogram(aes(fill=type), stat = "count", color = "black") + 
  scale_fill_manual(values = c("buy" = "#00BA38", "sell" = "#F8766D")) +
  scale_x_discrete(limits = unique(gov_trades$quarter_year)) + 
  labs(x=element_blank(), y = element_blank()) + 
  ggtitle("Trade Activity") +
  theme(legend.background = element_rect(fill = "#ECF0F5"),
        panel.background = element_rect(fill = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  geom_text(stat='count', aes(label=..count..),position=position_stack(vjust=0.5))




gov_trades %>%
  filter((state == "CT") & (representative == "Joe Courtney")) %>%
  mutate(price_performance_one_month = (price_one_month-price)/price,
         price_performance_three_months = (price_three_months-price)/price,
         price_performance_one_year = (price_one_year-price)/price,
         voo_performance_one_month = (voo_one_month-voo)/price,
         voo_performance_three_months = (voo_three_months-voo)/price,
         voo_performance_one_year = (voo_one_year-voo)/price) %>%
  summarise(type = "sell",
            gov_short = mean(price_performance_one_month, na.rm = TRUE),
            voo_short = mean(voo_performance_one_month, na.rm = TRUE),
            gov_med = mean(price_performance_three_months, na.rm = TRUE),
            voo_med = mean(voo_performance_three_months, na.rm = TRUE),
            gov_long = mean(price_performance_one_year, na.rm = TRUE),
            voo_long = mean(voo_performance_one_year, na.rm = TRUE)) %>%
  pivot_longer(cols = 2:7, names_to = "trader", values_to = "values") %>% 
  mutate(trader_cat = ifelse(startsWith(trader, "gov"), "Government", "S&P 500"),
         type_cat = case_when(endsWith(trader, "short") ~ "Short",
                              endsWith(trader, "med") ~ "Medium",
                              endsWith(trader, "long") ~ "Long")) %>%
  group_by(trader_cat) %>%
  summarise(values = mean(values)) %>%
  mutate(rank = rank(values),
         winner = ifelse(rank == 1,"Winner", "Loser")) %>%
  ggplot(aes(x=trader_cat, y=values, fill = winner)) + 
  geom_col(aes(linewidth = 4)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x=element_blank(), y = element_blank()) + 
  ggtitle("Sell Timing Performance") +
  geom_text(aes(label = paste0(round(values * 100, 2), "%")), 
            vjust = -1) +
  theme(legend.background = element_rect(fill = "#ECF0F5"),
        panel.background = element_rect(fill = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = pal)

####Treemap####

gov_trades %>%
  filter((state == "CT") & (representative == "Joe Courtney")) %>%
  group_by(sector) %>%
  count() %>%
  ggplot(aes(x=sector, y=n)) +
  geom_col()

library(treemap)

  # group_by(sector) %>%
  # summarise(avg_short_return = mean(price_return_one_month, na.rm = TRUE), 
  #           avg_mid_return = mean(price_return_three_months, na.rm = TRUE),
  #           avg_long_return = mean(price_return_one_year, na.rm = TRUE),
  #           lower_bound_sum = sum(lower_bound),
  #           upper_bound_sum = sum(upper_bound),
  #           count = n())

# Create tree map
gov_trades %>% 
  filter((state == "CT") & (representative == "Joe Courtney") & (type == "buy")) %>%
  mutate(price_return_one_month = (price_one_month-price)/price,
         price_return_three_months = (price_three_months-price)/price,
         price_return_one_year = (price_one_year-price)/price,
         voo_return_one_month = (voo_one_month-voo)/voo,
         voo_return_three_months = (voo_three_months-voo)/voo,
         voo_return_one_year = (voo_one_year-voo)/voo) %>%
  treemap(index= c("sector","ticker"),
        vSize="lower_bound",
        vColor = "price_return_one_month",
        type = "value",
        title="Stocks by Sector",
        fontsize.title=20)
####   ####

pal <- c("Winner" = "#01FF70",
         "Loser" = "grey")

gov_trades %>%
  filter((state == "CA") & (representative == "Zoe Lofgren") & (type == "buy")) %>%
  mutate(price_performance_one_month = (price_one_month-price)/price,
         price_performance_three_months = (price_three_months-price)/price,
         price_performance_one_year = (price_one_year-price)/price,
         voo_performance_three_months = (voo_three_months-voo)/price,
         voo_performance_one_year = (voo_one_year-voo)/price) %>%
  summarise(type = "buy",
            gov_short = mean(price_performance_one_month, na.rm = TRUE),
            voo_short = mean(voo_performance_one_month, na.rm = TRUE),
            gov_med = mean(price_performance_three_months, na.rm = TRUE),
            voo_med = mean(voo_performance_three_months, na.rm = TRUE),
            gov_long = mean(price_performance_one_year, na.rm = TRUE),
            voo_long = mean(voo_performance_one_year, na.rm = TRUE)) %>%
  pivot_longer(cols = 2:7, names_to = "trader", values_to = "values") %>% 
  mutate(trader_cat = ifelse(startsWith(trader, "gov"), "Representative", "S&P 500"),
         type_cat = case_when(endsWith(trader, "short") ~ "Short",
                              endsWith(trader, "med") ~ "Medium",
                              endsWith(trader, "long") ~ "Long")) %>%
  group_by(trader_cat) %>%
  filter(type_cat == "Short") %>%
  summarise(values = mean(values), type = "buy") %>%
  mutate(rank = ifelse(type == "buy", rank(-values), rank(values)),
         winner = ifelse(rank == 1,"Winner", "Loser")) %>%
  ggplot(aes(x=trader_cat, y=values, fill = winner)) + 
  geom_col(aes(linewidth = 4)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x=element_blank(), y = element_blank()) + 
  ggtitle("Buy Timing Performance") +
  geom_text(aes(label = paste0(round(values * 100, 2), "%")), 
            vjust = -1) +
  theme(legend.background = element_rect(fill = "#ECF0F5"),
        panel.background = element_rect(fill = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = pal)









         