library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

getwd()
gov_trades <- read.csv("data/CLEAN-gov-trades.csv", header = TRUE, sep = ",")
report_days <- read.csv("data/CLEAN-gov-report-days.csv", header = TRUE, sep = ",")
committee_2020 <- read.csv("data/CLEAN-committee-2020-data.csv", header = TRUE, sep = ",")
committee_2021 <- read.csv("data/CLEAN-committee-2021-data.csv", header = TRUE, sep = ",")
committee_2022 <- read.csv("data/CLEAN-committee-2022-data.csv", header = TRUE, sep = ",")

gov_trades <- gov_trades %>% mutate(disclosure_date = as.Date(disclosure_date),
                      transaction_date = as.Date(transaction_date))

summary(gov_trades)

####AVERAGE DELAY BETWEEN REPORTING AND TRADING#####
gov_trades %>%
  mutate(delay = disclosure_date - transaction_date) %>%
  filter(delay > 0) %>%
  summarise(average_delay = mean(delay), max_delay = max(delay), min_delay = min(delay))

#### Buy performance vs Sell Performance ####
gov_trades <- gov_trades %>% mutate(type = ifelse(type %in% c("sale_partial", "sale_full", "sale"), "sell", type),
                                    type = ifelse(type=="purchase", "buy", type))
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
  select(representative, sell_performance_one_month, sell_performance_three_months, sell_performance_one_year, sell_performance_avg)


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



#### Trade Activity - APP ####

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










         