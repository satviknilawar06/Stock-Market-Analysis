# Install packages (run once)
install.packages(c("shiny","quantmod","plotly","TTR","dplyr"))

# Load libraries
library(shiny)
library(quantmod)
library(plotly)
library(TTR)
library(dplyr)

# ---------------- UI ----------------

ui <- fluidPage(
  
  titlePanel("📊 Financial Market Analytics Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("stock",
                  "Select Stock:",
                  choices = c("Apple"="AAPL",
                              "Microsoft"="MSFT",
                              "Google"="GOOG",
                              "Reliance"="RELIANCE.NS")),
      
      dateRangeInput("dates",
                     "Select Date Range:",
                     start = "2023-01-01",
                     end = Sys.Date()),
      
      downloadButton("downloadData","Download Data")
      
    ),
    
    mainPanel(
      
      fluidRow(
        
        column(4,
               wellPanel(
                 h4("Current Price"),
                 textOutput("current_price")
               )),
        
        column(4,
               wellPanel(
                 h4("Day High"),
                 textOutput("day_high")
               )),
        
        column(4,
               wellPanel(
                 h4("Day Low"),
                 textOutput("day_low")
               ))
        
      ),
      
      tabsetPanel(
        
        tabPanel("Candlestick Chart",
                 plotlyOutput("candlestick")),
        
        tabPanel("Moving Average",
                 plotOutput("movingavg")),
        
        tabPanel("RSI Indicator",
                 plotOutput("rsi")),
        
        tabPanel("Volume Analysis",
                 plotOutput("volume_chart")),
        
        tabPanel("Stock Comparison",
                 plotOutput("comparison"))
        
      )
      
    )
    
  )
)

# ---------------- SERVER ----------------

server <- function(input, output) {
  
  stockData <- reactive({
    
    getSymbols(input$stock,
               src="yahoo",
               from=input$dates[1],
               to=input$dates[2],
               auto.assign = FALSE)
    
  })
  
  # KPI Values
  
  output$current_price <- renderText({
    
    stock <- stockData()
    round(as.numeric(last(Cl(stock))),2)
    
  })
  
  output$day_high <- renderText({
    
    stock <- stockData()
    round(max(Hi(stock)),2)
    
  })
  
  output$day_low <- renderText({
    
    stock <- stockData()
    round(min(Lo(stock)),2)
    
  })
  
  # Candlestick Chart
  
  output$candlestick <- renderPlotly({
    
    df <- data.frame(
      Date=index(stockData()),
      Open=as.numeric(Op(stockData())),
      High=as.numeric(Hi(stockData())),
      Low=as.numeric(Lo(stockData())),
      Close=as.numeric(Cl(stockData()))
    )
    
    plot_ly(
      data=df,
      x=~Date,
      type="candlestick",
      open=~Open,
      close=~Close,
      high=~High,
      low=~Low
    ) %>%
      layout(
        title="Candlestick Chart",
        xaxis=list(title="Date"),
        yaxis=list(title="Price")
      )
    
  })
  
  # Moving Average
  
  output$movingavg <- renderPlot({
    
    stock <- stockData()
    
    stock$MA50 <- SMA(Cl(stock),50)
    stock$MA200 <- SMA(Cl(stock),200)
    
    chartSeries(stock,
                type="line",
                name="Moving Average Analysis")
    
    addTA(stock$MA50,col="blue")
    addTA(stock$MA200,col="red")
    
  })
  
  # RSI Indicator
  
  output$rsi <- renderPlot({
    
    stock <- stockData()
    
    rsi <- RSI(Cl(stock),14)
    
    plot(rsi,
         type="l",
         col="purple",
         main="Relative Strength Index (RSI)",
         ylab="RSI Value")
    
    abline(h=70,col="red")
    abline(h=30,col="blue")
    
  })
  
  # Volume Analysis
  
  output$volume_chart <- renderPlot({
    
    stock <- stockData()
    
    barplot(Vo(stock),
            col="darkgreen",
            main="Trading Volume",
            ylab="Volume")
    
  })
  
  # Stock Comparison
  
  output$comparison <- renderPlot({
    
    symbols <- c("AAPL","MSFT","GOOG")
    
    getSymbols(symbols,
               src="yahoo",
               from="2023-01-01")
    
    stocks <- merge(Cl(AAPL),Cl(MSFT),Cl(GOOG))
    
    plot(stocks,
         main="Stock Price Comparison",
         col=c("blue","red","green"))
    
  })
  
  # Download Data
  
  output$downloadData <- downloadHandler(
    
    filename=function(){
      paste("stock_data.csv")
    },
    
    content=function(file){
      
      stock <- stockData()
      
      df <- data.frame(
        Date=index(stock),
        Open=Op(stock),
        High=Hi(stock),
        Low=Lo(stock),
        Close=Cl(stock),
        Volume=Vo(stock)
      )
      
      write.csv(df,file,row.names=FALSE)
      
    }
    
  )
  
}

# ---------------- RUN APP ----------------

shinyApp(ui = ui, server = server)
