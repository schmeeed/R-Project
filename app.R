#### Global ####
# loading libraries
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(zoo)
library(scales)
library(treemap)



setwd("/Users/bschmidt/Library/CloudStorage/GoogleDrive-schmidt5364@gmail.com/My\ Drive/#NYC_Data_Science_Academy/Projects/R-Project")

gov_trades <- fread(file = "data/CLEAN-gov-trades.csv")
# group all types of sell and buys into either "buy" or "sell"
gov_trades <- gov_trades %>% mutate(type = ifelse(type %in% c("sale_partial", "sale_full", "sale"), "sell", type),
                                    type = ifelse(type=="purchase", "buy", type))
gov_trades <- gov_trades %>%
  mutate(disclosure_date = as.Date(disclosure_date),
         transaction_date = as.Date(transaction_date))


colnames(gov_trades)

get_representatives <- function(state_choice) {
  reps <- gov_trades[gov_trades$state == state_choice]
  reps2 <- reps %>% arrange(last_name) %>% select(representative) %>% distinct()
  return(reps2)
}

#### UI ####
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = 'Legislator Stock Performance Dashboard'),
  dashboardSidebar(
    sidebarUserPanel("Brian Ralston",
                     image = "https://www.previewsworld.com/SiteImage/MainImage/STL153704.jpg"),
    sidebarMenu(
      menuItem("About", tabName = "About", icon = icon("person")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("database"))
    ),
    selectInput(inputId = "stateinputid",
                label = "State",
                choices = gov_trades %>% arrange(state) %>% select(state) %>% distinct(),
                selected = "GA"),
    selectInput(inputId = "repsinputid",
                label = "Representative",
                choices = gov_trades %>% arrange(last_name) %>% select(representative) %>% distinct(),
                selected = "Richard W. Allen")
  ),
  
  #### Body ####
  dashboardBody(
    tabsetPanel(
      tabPanel("tab panel 1",
               fluidRow(
                 column(width = 4,
                        column(width = 12,
                               valueBoxOutput("trade_count", width = "100%"),
                               column(width = 6,
                                      valueBoxOutput("bought_count", width = "50%")
                               ),
                               column(width = 6,
                                      valueBoxOutput("sold_count", width = "50%"))
                        ),
                        column(width = 12,
                               valueBoxOutput("vol_range", width = "100%"),
                               valueBoxOutput("rep_party", width = "100%")),
                 ),
                 column(width = 8,
                        plotOutput("trade_activity")
                 )
               ),
               fluidRow(
                 column(width = 4, div(style = "height:800px;", plotOutput("timing_perf")),
                        
                 ),
                 column(width = 2,
                        radioButtons("radio_buy_sell", label = h3(""), inline = TRUE,
                                     choices = list("Buy" = "buy", "Sell" = "sell"),
                                     selected = "buy"),
                        radioButtons("radio_short_med_long", label = h3(""), inline = TRUE,
                                     choices = list("30 Days" = "Short", "90 Days" = "Medium", "365 Days" = "Long"),
                                     selected = "Short")
                 ),
                 column(width = 6,
                        plotOutput("treemap"))
               ),
               fluidRow(
                 column(width = 12,
                        dataTableOutput("transaction_table"))
               )
      ),
      tabPanel("tab panel 2")
    )
  )
)



#### SERVER ####
server <- function(input, output, session) {
  # update representative inputs to reflect state selection
  observe({
    updateSelectInput(session,
                      inputId = "repsinputid",
                      choices = get_representatives(input$stateinputid) # custom function: defined in global
    )
  })
  #### DEFINE ####
  filtered_count <- reactive({
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid))# input$stateinputid input$repsinputid
  })
  bought_count <- reactive({
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
      filter(type == "buy")
  })
  
  sold_count <- reactive({
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
      filter(type == "sell")
  })
  rep_state <- reactive({
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
      distinct(state)
  })
  rep_party <- reactive({
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
      distinct(party)
  })
  
  filter_range <- reactive({
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>% # (state == "CA") & (representative == "Nancy Pelosi")
      summarise(lower_bound  = sum(lower_bound),
                upper_bound = sum(upper_bound))
  })
  
  
  pal <- c("Winner" = "#01FF70",
           "Loser" = "grey")
  #### OUTPUTS ####
  #### Value Boxes ####
  output$vol_range <- renderValueBox({
    valueBox(paste0("$", prettyNum(filter_range()[[1]], big.mark = ","), " - ", "$", prettyNum(filter_range()[[2]], big.mark = ",")), # ,scales::dollar(filter_range()[2])
             "Range", icon = icon("bank"), color = "green")
  })
  
  output$trade_count <- renderValueBox({
    valueBox(nrow(filtered_count()), 
             "Trades", icon = icon("exchange"), color = "purple") 
  })
  output$bought_count <- renderValueBox({
    valueBox(nrow(bought_count()), 
             "Bought", icon = icon("cow"), color = "green")
  })
  output$sold_count <- renderValueBox({
    valueBox(nrow(sold_count()), 
             "Sold", icon = icon("paw"), color = "red")
  })
  
  output$rep_state <- renderValueBox({
    valueBox(rep_state(), 
             "State", icon = icon("flag-usa"), color = "yellow")
  })
  output$rep_party <- renderValueBox({
    party <- as.character(rep_party())
    icon <- switch(party,
                   "Republican" = icon("republican"),
                   "Democrat" = icon("democrat"))
    color <- switch(party,
                    "Republican" = "red",
                    "Democrat" = "blue")
    valueBox(party, "Party", icon = icon, color = color)
  })
  output$trade_activity <- renderPlot(
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
      mutate(transaction_date = as.Date(transaction_date)) %>%
      arrange(transaction_date) %>%
      mutate(quarter_year = format(as.yearqtr(transaction_date, format = "%Y-%m-%d"), "%Y Q%q")) %>%
      ggplot(aes(x = quarter_year, fill = type)) + 
      geom_histogram(aes(fill=type), stat = "count", color = "black") + 
      scale_fill_manual(values = c("buy" = "#01FF70", "sell" = "#DD4B39")) +
      scale_x_discrete(limits = unique(gov_trades$quarter_year)) + 
      labs(x=element_blank(), y = element_blank()) + 
      ggtitle("Trade Activity") +
      theme(legend.background = element_rect(fill = "#ECF0F5"),
            panel.background = element_rect(fill = "#ECF0F5"),
            plot.background = element_rect(fill = "#ECF0F5"),
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank()) +
      geom_text(stat='count', aes(label=after_stat(count)),position=position_stack(vjust=0.5))
  )
  #### timing_perf ####
  output$timing_perf <- renderPlot(
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid) & (type == input$radio_buy_sell)) %>%
      mutate(price_performance_one_month = (price_one_month-price)/price,
             price_performance_three_months = (price_three_months-price)/price,
             price_performance_one_year = (price_one_year-price)/price,
             voo_performance_one_month = (voo_one_month-voo)/price,
             voo_performance_three_months = (voo_three_months-voo)/price,
             voo_performance_one_year = (voo_one_year-voo)/price) %>%
      summarise((type = input$radio_buy_sell),
                gov_short = mean(price_performance_one_month, na.rm = TRUE),
                voo_short = mean(voo_performance_one_month, na.rm = TRUE),
                gov_med = mean(price_performance_three_months, na.rm = TRUE),
                voo_med = mean(voo_performance_three_months, na.rm = TRUE),
                gov_long = mean(price_performance_one_year, na.rm = TRUE),
                voo_long = mean(voo_performance_one_year, na.rm = TRUE)) %>%
      pivot_longer(cols = 2:7, names_to = "trader", values_to = "values") %>% 
      mutate(trader_cat = ifelse(startsWith(trader, "gov"), input$repsinputid, "S&P 500"),
             type_cat = case_when(endsWith(trader, "short") ~ "Short",
                                  endsWith(trader, "med") ~ "Medium",
                                  endsWith(trader, "long") ~ "Long")) %>%
      group_by(trader_cat) %>%
      filter(type_cat == input$radio_short_med_long) %>%
      summarise(values = mean(values), type = input$radio_buy_sell) %>%
      mutate(rank = ifelse(type == "buy", rank(-values), rank(values)),
             winner = ifelse(rank == 1,"Winner", "Loser")) %>%
      ggplot(aes(x=trader_cat, y=values, fill = winner)) + 
      geom_col(aes(linewidth = 4)) +
      scale_y_continuous(labels = scales::percent) +
      labs(x=element_blank(), y = element_blank()) + 
      ggtitle("Timing Performance") +
      geom_text(aes(label = paste0(round(values * 100, 2), "%")), 
                vjust = -1) +
      theme(legend.background = element_rect(fill = "#ECF0F5"),
            panel.background = element_rect(fill = "#ECF0F5"),
            plot.background = element_rect(fill = "#ECF0F5"),
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            legend.position = "none") +
      scale_fill_manual(values = pal)
  )
  #### treemap ####
  output$treemap <- renderPlot(
    gov_trades %>% 
      filter((state == input$stateinputid) & (representative == input$repsinputid) & (type == input$radio_buy_sell)) %>%
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
  )
  #### transaction table #### 
  output$transaction_table <- renderDataTable(
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>% # (state == "NC") & (representative == "Virginia Foxx")
      mutate(range = paste0("$",prettyNum(lower_bound, big.mark = ","), " - ", "$", prettyNum(upper_bound, big.mark = ",", scientific = FALSE)),
             price_on_date = paste0("$",round(price,2)),
             volume_on_date = prettyNum(volume, big.mark = ",")) %>%
      select(type, sector, ticker, transaction_date, range, price_on_date, volume_on_date) %>%
      arrange(transaction_date)
  )
  
  
}

# Return a Shiny app object
shinyApp(ui = ui, server = server, options=c(launch.browser = .rs.invokeShinyPaneViewer))

