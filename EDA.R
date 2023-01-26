library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

View(gov_trades)
View(committee_2020)
View(report_days)

colnames(gov_trades)

unique(gov_trades$type)
gov_trades %>% mutate(type = ifelse(type %in% c("sale_partial", "sale_full", "sale"), "sell", type),
                      type = ifelse(type=="purchase", "buy", type))

####AVERAGE DELAY BETWEEN REPORTING AND TRADING#####
gov_trades %>% mutate(delay = disclosure_date - transaction_date) %>%
  summarise(average_delay = mean(delay), max_delay = max(delay), min_delay = min(delay))


install.packages("treemap")
library(treemap)


# Create data frame with stocks and their sectors
gov_trades %>% group_by(sector,ticker) %>% count() %>% summarise()


# Create tree map
treemap(gov_trades,
        index=c("ticker", "sector"),
        vSize="MarketCap",
        title="Stocks by Sector",
        fontsize.title=20,
        position.legend ="right"
)







###################################################

gov_trades %>%
  group_by(sector) %>%
  ggplot(aes(x=))

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
  ggplot(aes(x=sector)) +
  geom_bar(aes(fill = party)) +
  coord_flip() +
  geom_text(aes(label = ..count..), stat = "count", hjust = -.2) 


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







         