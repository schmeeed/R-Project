library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)


gov_trades <- read.csv("data/CLEAN-gov-trades.csv", header = TRUE, sep = ",")
report_days <- read.csv("data/CLEAN-report-days.csv", header = TRUE, sep = ",")
committee_2020 <- read.csv("data/CLEAN-committee_2020.csv", header = TRUE, sep = ",")
committee_2021 <- read.csv("data/CLEAN-committee_2021.csv", header = TRUE, sep = ",")
committee_2022 <- read.csv("data/CLEAN-committee_2022.csv", header = TRUE, sep = ",")

colnames(gov_trades)


gov_trades %>% filter(is.na(type))
####AVERAGE DELAY BETWEEN REPORTING AND TRADING#####
gov_trades %>%
  mutate(delay = disclosure_date - transaction_date) %>%
  summarise(average_delay = mean(delay), max_delay = max(delay), min_delay = min(delay))
#### Buy performance vs Sell Performance ####
unique(gov_trades$type)
gov_trades <- gov_trades %>% mutate(type = ifelse(type %in% c("sale_partial", "sale_full", "sale"), "sell", type),
                                    type = ifelse(type=="purchase", "buy", type))
#BUY
gov_trades %>%
  mutate(buy_performance_one_month = ifelse(type == "buy", price_one_month-price, NA),
         buy_performance_three_months = ifelse(type == "buy", price_three_months-price, NA),
         buy_performance_one_year = ifelse(type == "buy", price_one_year-price, NA),
         buy_performance_avg = mean(c(buy_performance_one_month, buy_performance_three_months, buy_performance_one_year), na.rm = TRUE)) %>%
  select(representative, buy_performance_one_month, buy_performance_three_months, buy_performance_one_year, buy_performance_avg)

gov_trades %>%
  filter(type == "sell") %>%
  mutate(sell_performance_one_month = price_one_month-price,
         sell_performance_three_months = price_three_months-price,
         sell_performance_one_year = price_one_year-price,
         sell_performance_avg = (sell_performance_one_month + sell_performance_three_months + sell_performance_one_year)/3) %>%
  select(representative, sell_performance_one_month, sell_performance_three_months, sell_performance_one_year, sell_performance_avg)


sum(c(3, NA, 5),na.rm = TRUE)
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

## Numeric Values
voo_summary_num_buy <- gov_trades %>% 
  mutate(voo_return_one_month = (voo_one_month-voo)/voo,
         voo_return_three_months = (voo_three_months-voo)/voo,
         voo_return_one_year = (voo_one_year-voo)/voo) %>%
  filter(type == "buy") %>%
  summarise(ticker = "S&P",
            one_month = mean(voo_return_one_month, na.rm = TRUE),
            three_months = mean(voo_return_three_months, na.rm = TRUE),
            one_year = mean(voo_return_one_year, na.rm = TRUE)
  )
voo_summary_num_sell <- gov_trades %>% 
  mutate(voo_return_one_month = (voo_one_month-voo)/voo,
         voo_return_three_months = (voo_three_months-voo)/voo,
         voo_return_one_year = (voo_one_year-voo)/voo) %>%
  filter(type == "sell") %>%
  summarise(ticker = "S&P",
            one_month = mean(voo_return_one_month, na.rm = TRUE),
            three_months = mean(voo_return_three_months, na.rm = TRUE),
            one_year = mean(voo_return_one_year, na.rm = TRUE)
  )
price_summary_num_buy<- gov_trades %>% 
  mutate(price_return_one_month = (price_one_month-price)/price,
         price_return_three_months = (price_three_months-price)/price,
         price_return_one_year = (price_one_year-price)/price) %>%
  filter(type == "buy") %>%
  summarise(ticker = "GOV_TRADES",
            one_month = mean(price_return_one_month, na.rm = TRUE),
            three_months = mean(price_return_three_months, na.rm = TRUE),
            one_year = mean(price_return_one_year, na.rm = TRUE)
  )
price_summary_num_sell<- gov_trades %>% 
  mutate(price_return_one_month = (price_one_month-price)/price,
         price_return_three_months = (price_three_months-price)/price,
         price_return_one_year = (price_one_year-price)/price) %>%
  filter(type == "sell") %>%
  summarise(ticker = "GOV_TRADES",
            one_month = mean(price_return_one_month, na.rm = TRUE),
            three_months = mean(price_return_three_months, na.rm = TRUE),
            one_year = mean(price_return_one_year, na.rm = TRUE)
  )
combined_summary_num <- bind_rows(voo_summary_num_buy, price_summary_num_buy, voo_summary_num_sell, price_summary_num_sell)
combined_summary_num



library(forcats) 

combined_summary_num %>% 
  pivot_longer(cols = 2:4, names_to = "period", values_to = "returns") %>%
  ggplot(aes(x=period, y=returns)) +
  geom_col(aes(fill = ticker), position = "dodge") +
  aes(x = fct_reorder(period,returns, .desc = TRUE)) +
  geom_text(aes(label = percent(returns, accuracy = .01)), position = position_dodge(width = 1))


#########################################################   
#### TOP 10 SHORT TERM TRADERS ####
gov_trades %>% 
  group_by(representative) %>% 
  mutate(price_return_one_month = (price_one_month-price)/price,
         price_return_three_months = (price_three_months-price)/price,
         price_return_one_year = (price_one_year-price)/price,
         voo_return_one_month = (voo_one_month-voo)/voo,
         voo_return_three_months = (voo_three_months-voo)/voo,
         voo_return_one_year = (voo_one_year-voo)/voo) %>%
  summarise(avg_one_month = weighted.mean(x=price_return_one_month,w=lower_bound, na.rm = TRUE),
            avg_three_months = weighted.mean(price_return_three_months, w=lower_bound, na.rm = TRUE),
            avg_one_year = weighted.mean(price_return_one_year,w=lower_bound, na.rm = TRUE)) %>%
  arrange(desc(avg_one_month)) %>%
  pivot_longer(cols = 2:4, names_to = "period", values_to = "returns") %>%
  head(30) %>%
  ggplot(aes(x=representative, y= returns)) +
  geom_col(aes(fill=period), position = "dodge") +
  aes(x = fct_reorder(representative,returns, .desc = TRUE)) +
  geom_text(aes(label = percent(returns, accuracy = .01)), position = position_dodge(width = 1))
  
#### TOP 10 LONG TERM TRADERS ####
gov_trades %>% 
  group_by(representative) %>% 
  mutate(price_return_one_month = (price_one_month-price)/price,
         price_return_three_months = (price_three_months-price)/price,
         price_return_one_year = (price_one_year-price)/price,
         voo_return_one_month = (voo_one_month-voo)/voo,
         voo_return_three_months = (voo_three_months-voo)/voo,
         voo_return_one_year = (voo_one_year-voo)/voo) %>%
  summarise(avg_one_month = weighted.mean(x=price_return_one_month,w=lower_bound, na.rm = TRUE),
            avg_three_months = weighted.mean(price_return_three_months, w=lower_bound, na.rm = TRUE),
            avg_one_year = weighted.mean(price_return_one_year,w=lower_bound, na.rm = TRUE)) %>%
  arrange(desc(avg_one_year)) %>%
  pivot_longer(cols = 2:4, names_to = "period", values_to = "returns") %>%
  head(30) %>%
  ggplot(aes(x=representative, y= returns)) +
  geom_col(aes(fill=period), position = "dodge") +
  aes(x = fct_reorder(representative,returns, .desc = TRUE)) +
  geom_text(aes(label = percent(returns, accuracy = .01)), position = position_dodge(width = 1))

#########################################################   

#install.packages("treemap")
library(treemap)

tree <-  gov_trades %>% filter(representative == "Nancy Pelosi") %>% 
  mutate(price_return_one_month = (price_one_month-price)/price,
         price_return_three_months = (price_three_months-price)/price,
         price_return_one_year = (price_one_year-price)/price,
         voo_return_one_month = (voo_one_month-voo)/voo,
         voo_return_three_months = (voo_three_months-voo)/voo,
         voo_return_one_year = (voo_one_year-voo)/voo) %>% 
  group_by(sector) %>%
  summarise(avg_short_return = mean(price_return_one_month, na.rm = TRUE), count = n())


tree
# Create tree map
treemap(tree,
        index="sector",
        vSize="count",
        vColor = "avg_short_return",
        type = "value",
        title="Stocks by Sector",
        fontsize.title=20)







###################################################

# of trades per party
gov_trades %>% 
  group_by(party) %>%
  ggplot(aes(x=party)) +
  geom_bar(aes(fill = party)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -.2)

# of trades per sector
gov_trades %>% 
  group_by(sector) %>%
  filter(!is.na(sector) & party != "Libertarian") %>%
  ggplot(aes(x=sector, fill = party)) +
  geom_bar(aes(fill = party)) +
  coord_flip() +
  geom_text(aes(label = ..count..), stat = "count", hjust = -.2, position = "stack") 


gov_trades %>%
  mutate(transaction_year = year(transaction_date)) %>%
  filter(!is.na(sector) & party != "Libertarian") %>%
  filter(party != "Libertarian") %>%
  ggplot(aes(transaction_year)) +
  geom_histogram(aes(fill=party), position = "dodge")

gov_trades %>%
  mutate(transaction_month = month(transaction_date)) %>%
  filter(!is.na(sector) & party != "Libertarian") %>%
  ggplot(aes(transaction_month)) + 
  geom_histogram(aes(fill =  party), position = "dodge")

gov_trades %>%
  mutate(transaction_day = day(transaction_date)) %>%
  filter(!is.na(sector) & party != "Libertarian") %>%
  ggplot(aes(transaction_day)) +
  geom_histogram(aes(fill = party))

gov_trades %>%
  mutate(transaction_weekday = weekdays(transaction_date)) %>%
  filter(!is.na(sector) & party != "Libertarian") %>%
  ggplot(aes(transaction_weekday)) +
  geom_bar(aes(fill = party))

gov_trades %>%
  filter(!is.na(sector) & party != "Libertarian") %>%
  group_by(sector,party) %>%
  count(sector)

gov_trades %>%
  filter(!is.na(sector) & party != "Libertarian") %>%
  group_by(sector) %>%
  ggplot(aes(x=transaction_date)) +
  geom_line(stat = "count", aes(color=sector))

as.numeric(as.Date("2022-01-31"))-as.numeric(as.Date("2022-02-01"))

unique(gov_trades$representative)

economic_indic <- read.csv("https://brian-ralston-r-project.s3.amazonaws.com/economic-indicators-reports-2020-2022.csv", 
          skip = 9, header = TRUE)


##########################
library(zoo)
library(scales)

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
  theme(panel.background = element_rect(fill = "white"), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_text(stat='count', aes(label=..count..),position=position_stack(vjust=0.5))















         