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
library(gghighlight) 
library(plotly)
library(humaniformat)
library(DT)
library(stringr)
library(rsconnect)


# setwd("/Users/bschmidt/Library/CloudStorage/GoogleDrive-schmidt5364@gmail.com/My\ Drive/#NYC_Data_Science_Academy/Projects/R-Project")

gov_trades <- read.csv(file = "shiny_data/CLEAN-gov-trades.csv")
VOO <- read.csv(file = "shiny_data/RAW-VOO.csv")
# stocks <- read_csv(file = "data/CLEAN-stocks-trunc-data.csv")
# stocks$ref_date <- as.Date(stocks$ref_date)
stocks <- readRDS("shiny_data/stocks-small.rds")
committees <- read_csv(file = "shiny_data/CLEAN-committees-all.csv")

# gov_trades %>% filter(ticker == "0QZI.IL")

# stocks_small <- stocks[1:100000,]
# saveRDS(stocks_small, "stocks-small.rds")

# group all types of sell and buys into either "buy" or "sell"
gov_trades <- gov_trades %>% mutate(type = ifelse(type %in% c("sale_partial", "Sale (Partial)", "sale_full","Sale (Full)", "sale"), "sell", type),
                                    type = ifelse(type=="Purchase", "buy", type))
gov_trades <- gov_trades %>%
  mutate(disclosure_date = as.Date(disclosure_date),
         transaction_date = as.Date(transaction_date))


get_representatives <- function(state_choice) {
  reps <- gov_trades[gov_trades$state == state_choice,]
  reps2 <- reps %>% arrange(last_name) %>% select(representative) %>% distinct()
  return(reps2)
}

# saveRDS(stocks, file = "stocks.rds")



#### UI ####
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = 'Legislator Stock Performance Dashboard'),
  dashboardSidebar(
    sidebarUserPanel("Brian Ralston",
                     image = "https://www.previewsworld.com/SiteImage/MainImage/STL153704.jpg"),
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("person")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Table", tabName = "data", icon = icon("database"))
    ),
    selectInput(inputId = "stateinputid",
                label = "State",
                choices = gov_trades %>% arrange(state) %>% select(state) %>% distinct(),
                selected = "CA"),
    selectInput(inputId = "repsinputid",
                label = "Representative",
                choices = gov_trades %>% arrange(last_name) %>% select(representative) %>% distinct(),
                selected = "Zoe Lofgren")
  ),
  
  #### Body ####
  dashboardBody(
      tabItems(
        tabItem(tabName = "about",
                fluidRow(column(8, align="center", offset = 2,
                                box(htmlOutput('intro_header'), htmlOutput('intro_author'), width = 20, 
                                    background = 'light-blue'),
                                tags$style(type="text/css", "#string { text-align:center }"))),
                fluidRow(column(10, align="center", offset = 1,
                                box(htmlOutput('intro_body1'), div(img(src="david-vives-Nzbkev7SQTg-unsplash.jpg", height=350, width=350)),
                                    htmlOutput('intro_body2'), 
                                    htmlOutput('intro_body3'), 
                                    htmlOutput('intro_body4'), width = 20, background = 'light-blue'),
                                tags$style(type="text/css", "#string { text-align:justified }")))),
        tabItem(tabName = "dashboard",
                fluidRow(column(width = 3, valueBoxOutput("rep_party", width = "100%")),
                         column(width = 2, valueBoxOutput("trade_count", width = "100%")),
                         column(width = 2, valueBoxOutput("bought_sold_count", width = "25%")),
                         column(width = 5, valueBoxOutput("vol_range", width = "100%"))),
                fluidRow(column(width = 8, plotlyOutput("top_stocks_viz", height = "400px")),
                  column(width = 4, height = "400px",
                         fluidRow(
                           column(width = 12,
                                  plotOutput("top_5_stocks", height = "200px")
                           )
                         ),
                         fluidRow(
                           column(width = 12,
                                  plotOutput("top_5_sectors", height = "200px")
                           )
                         )
                  )
                ),
                fluidRow(
                  column(width = 8,
                         plotOutput("trade_activity")
                         ),
                  column(width = 4,
                         dataTableOutput("committee_table")
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
                         )
                )
                ),
        tabItem(tabName = "data",dataTableOutput("transaction_table"))
      )
  )
)



#### SERVER ####
server <- function(input, output, session) {
  # update representative inputs to reflect state selection
  observe({
    updateSelectInput(session,
                      inputId = "repsinputid",
                      choices = get_representatives(input$stateinputid),# custom function: defined in global
                      selected = "Zoe Lofgren" 
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
  rep_branch <- reactive({
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
      distinct(branch)
  })
  
  filter_range <- reactive({
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>% # (state == "CA") & (representative == "Nancy Pelosi")
      summarise(lower_bound  = sum(lower_bound),
                upper_bound = sum(upper_bound))
  })


  rep_top_sectors <- reactive({
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
      mutate(transaction_date = as.Date(transaction_date)) %>%
      group_by(sector) %>%
      count() %>% 
      filter(!is.na(sector)) %>%
      arrange(desc(n)) %>%
      head(5)
  })
  rep_top_tickers <- reactive({
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
      mutate(transaction_date = as.Date(transaction_date)) %>%
      group_by(ticker) %>%
      count() %>% 
      arrange(desc(n)) %>%
      head(5)
  })
  
  rep_top_sectors <- reactive({
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
      mutate(transaction_date = as.Date(transaction_date)) %>%
      group_by(sector) %>%
      count() %>% 
      arrange(desc(n)) %>%
      head(5)
  })

  
   
  pal <- c("Winner" = "#01FF70",
           "Loser" = "grey")
  #### OUTPUTS ####
  
  ### top_stocks_viz ####
  # output$top_stocks_viz <- renderPlot({
  #   stocks %>%
  #     filter(ticker %in% req(rep_top_tickers())$ticker) %>%
  #     left_join(y=filtered_count(), by = c("ref_date" = "transaction_date", "ticker" = "ticker"), keep = TRUE) %>%
  #     group_by(ticker.x) %>%
  #     filter(ref_date >= (min(filtered_count()[["transaction_date"]])-30), ref_date <= (max(filtered_count()[["transaction_date"]])+30)) %>%
  #     ggplot() +
  #     geom_line(aes(x=ref_date, y=price_adjusted, group = ticker.x, color = ticker.x)) +
  #     geom_point(aes(x=transaction_date, y=price_adjusted, shape = type)) +
  #     theme(legend.background = element_rect(fill = "#ECF0F5"),
  #           panel.background = element_rect(fill = "#ECF0F5"),
  #           plot.background = element_rect(fill = "#ECF0F5"),
  #           panel.grid.minor = element_blank(),
  #           panel.grid.major = element_blank()) +
  #     labs(x=element_blank(), y = element_blank())
  # })


  output$top_stocks_viz <- renderPlotly({
    d1 <- stocks %>%
      filter(ticker %in% req(rep_top_tickers())$ticker) %>%
      left_join(y=filtered_count(), by = c("ref_date" = "transaction_date", "ticker" = "ticker"), keep = TRUE) %>%
      group_by(ticker.x) %>%
      filter(ref_date >= (min(filtered_count()[["transaction_date"]])-30), ref_date <= (max(filtered_count()[["transaction_date"]])+30))
    
    d2 <- highlight_key(d1, ~ticker.x)
    
    p <- ggplot(d2, aes(x=ref_date, y=price_adjusted, group = ticker.x, color = ticker.x)) +
      geom_line() +
      geom_point(aes(x=transaction_date, shape = type, size = 12)) +
      theme(legend.background = element_rect(fill = "#ECF0F5"),
            panel.background = element_rect(fill = "#ECF0F5"),
            plot.background = element_rect(fill = "#ECF0F5"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()) +
      labs(x=element_blank(), y = element_blank()) +
      ggtitle("Top 5 Stocks Timing")
    
    gg <- ggplotly(p, tooltip = c("y", "x"), text = c("Price", "Date")) %>% layout(showlegend = FALSE)
    
    
    highlight(gg, on = "plotly_hover", dynamic = FALSE, debounce = 50)
    
    # add_trace(hh, y = d1$price_adjusted)
  })
  
  
  #### top_5_stocks ####
  output$top_5_stocks <-renderPlot(
    rep_top_tickers() %>% 
    ggplot(aes(x=reorder(ticker, n), y = n, color = ticker, label = paste0(ticker," (",n,")"))) +
    geom_segment(aes(xend = ticker, yend = 0)) +
    geom_point(size = 4) +
    scale_y_continuous(limits = c(0,max(rep_top_tickers()$n)*1.6)) +
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
    geom_text(hjust = -.3) +
    theme(legend.position="none") +
    coord_flip()
    )
  

  output$top_5_sectors <- renderPlot(
    rep_top_sectors() %>% 
      ggplot(aes(x=reorder(sector, n), y = n, label = paste0(sector," (",n,")"))) +
      geom_segment(aes(xend = sector, yend = 0)) +
      geom_point(size = 4) +
      scale_y_continuous(limits = c(0,max(rep_top_sectors()$n)*1.7)) +
      ggtitle("Top 5 Sectors") +
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
      geom_text(hjust = -.3) +
      theme(legend.position="none") +
      coord_flip()
  )
  
  
  #### Value Boxes ####
  output$vol_range <- renderValueBox({
    party <- as.character(rep_party())
    color <- switch(party,
                    "Republican" = "red",
                    "Democrat" = "blue")
    valueBox(paste0("$", prettyNum(filter_range()[[1]], big.mark = ","), " - ", "$", prettyNum(filter_range()[[2]], big.mark = ",")), # ,scales::dollar(filter_range()[2])
             "Volume", icon = icon("dollar"), color = color)
  })
  
  output$trade_count <- renderValueBox({
    party <- as.character(rep_party())
    color <- switch(party,
                    "Republican" = "red",
                    "Democrat" = "blue")
    valueBox(nrow(filtered_count()), 
             "Trades", icon = icon("exchange"), color = color) 
  })
  output$bought_sold_count <- renderValueBox({
    party <- as.character(rep_party())
    color <- switch(party,
                    "Republican" = "red",
                    "Democrat" = "blue")
    valueBox(paste0(nrow(bought_count()), " / ", nrow(sold_count())), 
             "Bought/Sold", color = color)
  })

  output$rep_state <- renderValueBox({
    party <- as.character(rep_party())
    color <- switch(party,
                    "Republican" = "red",
                    "Democrat" = "blue")
    valueBox(rep_state(), 
             "State", icon = icon("flag-usa"), color = color)
  })
  output$rep_party <- renderValueBox({
    party <- as.character(rep_party())
    branch <- str_to_title(as.character(rep_branch()))
    icon <- switch(party,
                   "Republican" = icon("republican"),
                   "Democrat" = icon("democrat"))
    color <- switch(party,
                    "Republican" = "red",
                    "Democrat" = "blue")
    valueBox(party, branch, icon = icon, color = color)
  })

  gov_trades
  #### Trade Activity ####
  output$trade_activity <- renderPlot(
    gov_trades %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
      mutate(transaction_date = as.Date(transaction_date)) %>%
      arrange(transaction_date) %>%
      mutate(quarter_year = format(as.yearqtr(transaction_date, format = "%Y-%m-%d"), "%Y Q%q")) %>%
      ggplot(aes(x = quarter_year, fill = type)) + 
      geom_bar() + 
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
  
  #### Committee Table ####
  output$committee_table <- renderDataTable({
    # library(humaniformat)
    parsed_name_input <- parse_names(input$repsinputid)
    
    committees_filtered <- committees %>%
      filter(committees$first_name == parsed_name_input$first_name, committees$last_name == parsed_name_input$last_name) %>%
      select(year, committee)
    
    datatable(committees_filtered, options = list(list(pageLength = 10, 
                                                       autoWidth = TRUE, 
                                                       scrollY = "399px",
                                                       scrollCollapse = TRUE)))
  })
  
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
  
 #### Intro Page Text ####
  output$intro_header <- renderUI({
    h1("House of Representatives Stock Trading Performance App")
  })
  
  output$intro_author <- renderUI({
    h3("coded by Brian Ralston")
  })

  output$intro_body1 <- renderUI({
    p("This app uses data from Financial Disclosure Reports filed by members of 
      the US House of Representatives and housed by the Clerk of the House. 
      Members of Congress fill out at Financial Disclosure Report up to 30 days 
      after they trade stock(s) with a value of $1,000 or more and include 
      information about the source, asset, transaction date, and value range.")
  })
  
  output$intro_body2 <- renderUI({
    p("I have created this app so that the user can summarize the data from 
      these reports per representative and see which house members are good at 
      timing the stock market. ")
  })
  
  output$intro_body3 <- renderUI({
    p("The Dashboard section shows the individual representative’s top 5 stocks, 
      top 5 sectors, and what committees they serve on. There is also an 
      interactive plotly graph that shows a graph of the closing prices of those 
      stocks, and where the congress member bought and sold the stock.")
  })
  
  output$intro_body4 <- renderUI({
    p("The data table section shows details of the transactions that this 
      individual representative has made.")
  })
  
  
  
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)

