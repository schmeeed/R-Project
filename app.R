#### Global ####
# loading libraries
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(zoo)
library(scales)



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

#### OLD UI ####
# ui <- fluidPage(
#   titlePanel("Legislator Stock Performance"),
# 
#   sidebarLayout(
#     sidebarPanel(
#       selectInput(inputId = "stateinputid",
#                   label = "State",
#                   choices = gov_trades %>% arrange(state) %>% select(state) %>% distinct()),
#       selectInput(inputId = "repsinputid",
#                   label = "Representative",
#                   choices = gov_trades %>% arrange(last_name) %>% select(representative) %>% distinct())
#     ),
#     mainPanel(
#       textOutput("trade_count"),
#       textOutput("bought_count"),
#       textOutput("sold_count")
#               )
#   )
# )

#### DASHBOARD UI ####
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = 'Legislator Stock Performance Dashboard'),
  dashboardSidebar(
    sidebarUserPanel("Brian Ralston",
                     image = "https://www.previewsworld.com/SiteImage/MainImage/STL153704.jpg"),
    sidebarMenu(
      menuItem("About", tabName = "About", icon = icon("person")),
      menuItem("Plots", tabName = "plots", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("database"))
    ),
    selectInput(inputId = "stateinputid",
                label = "State",
                choices = gov_trades %>% arrange(state) %>% select(state) %>% distinct()),
    selectInput(inputId = "repsinputid",
                label = "Representative",
                choices = gov_trades %>% arrange(last_name) %>% select(representative) %>% distinct())
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("tab panel 1",
               fluidRow(
                 column(width = 5,
                        column(width = 12,
                               valueBoxOutput("trade_count"),
                               valueBoxOutput("bought_count"),
                               valueBoxOutput("sold_count")),
                        column(width = 12,
                               valueBoxOutput("rep_state", width = "50%"),
                               valueBoxOutput("rep_party", width = "50%"))
                        ),
                 column(width = 7,
                        plotOutput("trade_activity")
                        )
                 ),
               fluidRow()
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
  # DEEFINE items
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

    

    
    
    # DEFINE output items
    # output$trade_count <- renderText({
    #   paste("Number of trades: ", nrow(filtered_count()))
    # })
    output$trade_count <- renderValueBox({
      valueBox(nrow(filtered_count()), 
               "Trades", icon = icon("exchange"), color = "aqua")
    })
    output$bought_count <- renderValueBox({
      valueBox(nrow(bought_count()), 
               "Bought", icon = icon("cow"), color = "lime")
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
    
    #ifelse(rep_party() == "Republican", as.character(icon("republican")), as.character(icon("democrat")))
    
    
    # output$bought_count <- renderText({
    #   paste("Bought: ", nrow(bought_count()))
    # })
    # output$sold_count <- renderText({
    #   paste("Sold: ", nrow(sold_count()))
    # })
    # output$rep_state <- renderText({
    #   paste("State: ", rep_state())
    # })
    # output$rep_party <- renderText({
    #   paste("Party: ", rep_party())
    # })
    output$trade_activity <- renderPlot(
        gov_trades %>%
          filter((state == input$stateinputid) & (representative == input$repsinputid)) %>%
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
        )
    

}

# Return a Shiny app object
shinyApp(ui = ui, server = server, options=c(launch.browser = .rs.invokeShinyPaneViewer))


