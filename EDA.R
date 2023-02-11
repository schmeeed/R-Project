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
format(com_filt_gov_trades$transaction_date, "%Y")

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


  
  
  
####AVERAGE DELAY BETWEEN REPORTING AND TRADING#####
gov_trades %>%
  mutate(delay = disclosure_date - transaction_date) %>%
  filter(delay > 0) %>%
  summarise(average_delay = mean(delay), max_delay = max(delay), min_delay = min(delay))

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

  rep_top_tickers <- gov_trades %>%
  filter((state == "CA") & (representative == "Nancy Pelosi")) %>%
  mutate(transaction_date = as.Date(transaction_date)) %>%
  group_by(ticker) %>%
  count() %>% 
  arrange(desc(n)) %>%
  head(5)
  
  class(rep_top_tickers)

  rep_top_tickers %>% 
    ggplot(aes(x=reorder(ticker, n), y = n, color = ticker, label = paste0(ticker," (",n,")"))) +
    geom_segment(aes(xend = ticker, yend = 0)) +
    geom_point(size = 4) +
    scale_y_continuous(limits = c(0,max(top_5_stocks$n)*1.2)) +
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
  
  #### top_5_sectors visual####  
  
  rep_top_sectors <- gov_trades %>%
    filter((state == "CA") & (representative == "Nancy Pelosi")) %>%
    mutate(transaction_date = as.Date(transaction_date)) %>%
    group_by(sector) %>%
    count() %>% 
    arrange(desc(n)) %>%
    head(5)
  
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
  d1 <- stocks %>%
    filter(ticker %in% c(rep_top_tickers$ticker)) %>%
    left_join(y=rep_data, by = c("ref_date" = "transaction_date", "ticker" = "ticker"), keep = TRUE) %>%
    group_by(ticker.x) %>%
    filter(ref_date >= (min(rep_data$transaction_date)-30), ref_date <= (max(rep_data$transaction_date)+30))
  d2 <- highlight_key(d1, ~ticker.x)
  p <- ggplot(d2, aes(x=ref_date, y=price_adjusted, group = ticker.x, color = ticker.x)) +
    geom_line() +
    geom_point(aes(x=transaction_date, y=price_adjusted, shape = type, size = 12, color = ticker.x)) +
    theme(legend.background = element_rect(fill = "#ECF0F5"),
          panel.background = element_rect(fill = "#ECF0F5"),
          plot.background = element_rect(fill = "#ECF0F5"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank()) +
    labs(x=element_blank(), y = element_blank())
  cols2 <- toRGB(RColorBrewer::brewer.pal(3, "Dark2"), 0.5)
  gg <- ggplotly(p, tooltip = "ticker.x")
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









         