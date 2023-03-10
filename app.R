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

#### LOAD DATA ####

gov_trades <- readRDS(file = "shiny_data/gov_trades.rds")

VOO <- readRDS(file = "shiny_data/RAW-VOO.rds")

stocks <- readRDS("shiny_data/stocks.rds")

committees <- readRDS("shiny_data/CLEAN-committees-all.rds")

# Function to get the representatives from the state selected
get_representatives <- function(state_choice) {
  reps <- gov_trades[gov_trades$state == state_choice,]
  reps2 <- reps %>% arrange(last_name) %>% select(representative) %>% distinct()
  return(reps2)
}

get_tickers <- function(rep_choice) {
  ticks <- gov_trades[gov_trades$representative == rep_choice,]
  ticks2 <- ticks %>% arrange(ticker) %>% select(ticker) %>% distinct()
  return(ticks2)
}

#Function to format Dollars in the Millions to "$4.2M"

million_format <- function(x) {
  ifelse(x > 999999,
         paste0("$", format(round(x/1e6, 1), nsmall = 1), "M"),
         paste0("$", format(x, big.mark = ",")))
}



#### UI ####
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = 'Congress Stocks'),
  dashboardSidebar(
    sidebarUserPanel("Brian Ralston",
                     image = "brian-headshot.jpeg"),
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info")),
      menuItem("Top Trades & Trends", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Individual Trades", tabName = "data", icon = icon("person"))
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
                fluidRow(column(4, align="center", offset = 4,
                                box(htmlOutput('intro_header1'), 
                                    htmlOutput('intro_header2'),
                                    htmlOutput('intro_author'), width = 20, background = 'light-blue'),
                                tags$style(type="text/css", "#string { text-align:center }"))),
                fluidRow(column(4, align="center", offset = 4,
                                box(htmlOutput('intro_body1'), div(img(src="david-vives-Nzbkev7SQTg-unsplash.jpg", height=350, width=350)),
                                    htmlOutput('intro_body2'), 
                                    htmlOutput('intro_body3'), 
                                    htmlOutput('intro_body4'),
                                    htmlOutput("intro_footer1"),
                                    htmlOutput("intro_footer2"), width = 20, background = 'light-blue'),
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
                fluidRow(column(width = 2,
                                radioButtons("radio_buy_sell",
                                             label = h3(""),
                                             inline = TRUE,
                                             choices = list("Buy" = "buy", "Sell" = "sell"),
                                             selected = "buy"),
                                radioButtons("radio_short_med_long", 
                                             label = h3(""), 
                                             inline = TRUE,
                                             choices = list("30 Days" = "Short", "90 Days" = "Medium", "365 Days" = "Long"),
                                             selected = "Short")),
                         column(width = 2, div(style = "height:800px;", plotOutput("timing_perf"))),
                         column(width = 4,
                         plotOutput("trade_activity")),
                         column(width = 4,div(style = "height:800px;", dataTableOutput("committee_table")))),
                fluidRow(
                         # column(width = 6, plotOutput("treemap"))
                )),
        tabItem(tabName = "data",
                fluidRow(column(width = 12, selectInput(inputId = "tickerinputid",
                                                label = "Ticker",
                                                choices = gov_trades %>% arrange(ticker) %>% select(ticker) %>% distinct()))),
                fluidRow(column(width = 12, plotlyOutput("select_stock_viz"))),
                fluidRow(column(width = 12, dataTableOutput("transaction_table"))))
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
  
  observe({
    updateSelectInput(session,
                      inputId = "tickerinputid",
                      choices = get_tickers(input$repsinputid)# custom function: defined in global
    )
  })
  
  
  #### DEFINE ####

  
  ticker_filtered <- reactive({
    gov_trades %>%
      filter(representative == input$repsinputid, ticker == input$tickerinputid)# input$stateinputid input$repsinputid
  })
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
      filter((state == input$stateinputid) & (representative == input$repsinputid), !is.na(sector)) %>%
      group_by(sector) %>% 
      summarise(lower_bound = sum(lower_bound),
                upper_bound = sum(upper_bound),
                count = n()) %>% 
      arrange(desc(upper_bound)) %>%
      mutate(range = paste0(million_format(lower_bound)," - ", million_format(upper_bound), "       (", count,")")) %>%
      head(5)
  })
  rep_top_tickers <- reactive({
      gov_trades %>% 
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
      group_by(ticker) %>% 
      summarise(lower_bound = sum(lower_bound),
                upper_bound = sum(upper_bound),
                count = n()) %>% 
      arrange(desc(upper_bound)) %>%
      mutate(range = paste0(million_format(lower_bound)," - ", million_format(upper_bound), "       (", count,")")) %>%
      head(5) 
  })

  
   
  pal <- c("Winner" = "gold",
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
      filter(ref_date >= (min(filtered_count()[["transaction_date"]])-548), ref_date <= (max(filtered_count()[["transaction_date"]])+548)) %>%
      mutate(range  = paste0(million_format(lower_bound), "-", million_format(upper_bound)))
    
    d2 <- highlight_key(d1, ~ticker.x)
    
    p <- ggplot(d2, aes(group = ticker.x, color = ticker.x)) +
      geom_line(aes(x=ref_date, y=price_adjusted)) +
      geom_point(aes(x=transaction_date, y=price_adjusted, fill = ticker.x, shape = type, size = lower_bound, tooltip = range),color = "black") +
      scale_shape_manual(values = c("buy" = 19, "sell" = 17)) +
      theme(legend.background = element_rect(fill = "#ECF0F5"),
            panel.background = element_rect(fill = "#ECF0F5"),
            plot.background = element_rect(fill = "#ECF0F5"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()) +
      labs(x=element_blank(), y = element_blank()) +
      ggtitle("Top 5 Stocks by Volume")
    
    gg <- ggplotly(p, tooltip = c("y", "x", "range", "type")) %>% layout(showlegend = FALSE)
    
    
    highlight(gg, on = "plotly_hover", dynamic = FALSE, debounce = 50)

  })
  
  #### Select_Stock_viz ####
  output$select_stock_viz <- renderPlotly({
    d1 <- stocks %>%
      filter(ticker == input$tickerinputid) %>%
      left_join(y=ticker_filtered(), by = c("ref_date" = "transaction_date", "ticker" = "ticker"), keep = TRUE) %>%
      group_by(ticker.x) %>%
      filter(ref_date >= (min(ticker_filtered()[["transaction_date"]])-548), ref_date <= (max(ticker_filtered()[["transaction_date"]])+548)) %>%
      mutate(range  = paste0(million_format(lower_bound), "-", million_format(upper_bound)))
    
    d2 <- highlight_key(d1, ~ticker.x)
    
    p <- ggplot(d2, aes(group = ticker.x, color = ticker.x)) +
      geom_line(aes(x=ref_date, y=price_adjusted)) +
      geom_point(aes(x=transaction_date, y=price_adjusted, fill = ticker.x, shape = type, size = lower_bound, tooltip = range),color = "black") +
      scale_shape_manual(values = c("buy" = 19, "sell" = 17)) +
      theme(legend.background = element_rect(fill = "#ECF0F5"),
            panel.background = element_rect(fill = "#ECF0F5"),
            plot.background = element_rect(fill = "#ECF0F5"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()) +
      labs(x=element_blank(), y = element_blank()) +
      ggtitle("Top 5 Stocks Timing")
    
    gg <- ggplotly(p, tooltip = c("y", "x", "range", "type")) %>% layout(showlegend = FALSE)
    
    
    highlight(gg, on = "plotly_hover", dynamic = FALSE, debounce = 50)
    
  })
  
  
  #### top_5_stocks ####
  output$top_5_stocks <-renderPlot(
    rep_top_tickers() %>% 
      ggplot(aes(x=fct_reorder(ticker, lower_bound), y=lower_bound, fill = ticker)) +
      geom_col(show.legend = FALSE) +
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
      ggtitle("Stocks")
    )
  

  output$top_5_sectors <- renderPlot(
    rep_top_sectors() %>% 
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
      ggtitle("Sectors")
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
      arrange(transaction_date) %>%
      mutate(quarter_year = format(as.yearqtr(transaction_date, format = "%Y-%m-%d"), "%Y Q%q")) %>%
      ggplot(aes(x = quarter_year, fill = type)) + 
      geom_bar() + 
      scale_fill_manual(values = c("buy" = "#04BE7D", "sell" = "#F8756D")) +
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
  # output$treemap <- renderPlot(
  #   gov_trades %>% 
  #     filter((state == input$stateinputid) & (representative == input$repsinputid) & (type == input$radio_buy_sell)) %>%
  #     mutate(price_return_one_month = (price_one_month-price)/price,
  #            price_return_three_months = (price_three_months-price)/price,
  #            price_return_one_year = (price_one_year-price)/price,
  #            voo_return_one_month = (voo_one_month-voo)/voo,
  #            voo_return_three_months = (voo_three_months-voo)/voo,
  #            voo_return_one_year = (voo_one_year-voo)/voo) %>%
  #     treemap(index= c("sector","ticker"),
  #             vSize="lower_bound",
  #             vColor = "price_return_one_month",
  #             type = "value",
  #             title="Stocks by Sector",
  #             fontsize.title=20)
  # )
  
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
      mutate(return_one_week = percent((price_one_week-price)/price,accuracy = .01),
             return_one_month = percent((price_one_month-price)/price,accuracy = .01),
             return_three_months = percent((price_three_months-price)/price,accuracy = .01),
             return_one_year = percent((price_one_year-price)/price,accuracy = .01),
             return_to_date = percent((price_to_date-price)/price,accuracy = .01)) %>%
      filter((state == input$stateinputid) & (representative == input$repsinputid)) %>% # (state == "NC") & (representative == "Virginia Foxx")
      mutate(range = paste0("$",prettyNum(lower_bound, big.mark = ","), " - ", "$", prettyNum(upper_bound, big.mark = ",", scientific = FALSE)),
             price_on_date = paste0("$",round(price,2)),
             price_one_week = paste0("$",round(price_one_week,2)),
             price_one_month = paste0("$",round(price_one_month,2)),
             price_three_months = paste0("$",round(price_three_months,2)),
             price_one_year = paste0("$",round(price_one_year,2)),
             price_to_date = paste0("$",round(price_to_date,2)),
             volume_on_date = prettyNum(volume, big.mark = ","),
             trans_dt = transaction_date,
             disc_dt = disclosure_date) %>%
      select(type,ticker, sector, trans_dt, disc_dt, range, 
             price_on_date, price_one_week, return_one_week, 
             price_one_month, return_one_month, 
             price_three_months, return_three_months, 
             price_one_year, return_one_year,
             price_to_date, return_to_date) %>%
      arrange(trans_dt)
  )
  
 #### Intro Page Text ####
  output$intro_header1 <- renderUI({
    h1("DC Trades")
  })
  output$intro_header2 <- renderUI({
    h2("Congress Stocks Trading App")
  })
  
  output$intro_author <- renderUI({
    h4("Coded by: Brian Ralston")
  })

  output$intro_body1 <- renderUI({
    p("This app uses data from Financial Disclosure Reports filed by members of US Congress. 
      Members are required to report transactions within 45 days of trading stocks valued at $1,000 or more. 
      The reports include information about the source, asset, transaction date, and value range.")
  })
  
  output$intro_body2 <- renderUI({
    p("I have created this app so that the user can summarize the data from 
      these reports per representative and identify congress members who have 
      a history of successful stock investments. ")
  })
  
  output$intro_body3 <- renderUI({
    p("This Overview section provides insight into the representative's financial interests and potential conflicts of interest. 
    You can see how their investments align with their committee assignments, and how successful they are at buying and selling stocks in the short and long term.
     There is also a great interactive plotly graph to dive into the details of 5 of the larger trades the representatives have made.")
  })
  
  output$intro_body4 <- renderUI({
    p("The data table section shows details of the transactions that this individual 
      representative has made, as well as a way to explore those particular stocks with a plotly graph.")
    
  })
  output$intro_footer1<- renderUI({
    p("Thanks to Timothy Carambat who maintains the api for this dataset on", 
      a("https://housestockwatcher.com/api", href = "https://housestockwatcher.com/api", style = "color: black;", target = "_blank"))
  })
  output$intro_footer2 <- renderUI({
    p("Visit the link for the complete R-Project Shiny app on",
      a("My GitHub", href = "https://github.com/schmeeed/R-Project", style = "color: black;", target = "_blank"),
      " for more information.")
  })
  
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)

