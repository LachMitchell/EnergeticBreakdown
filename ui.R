library(shiny)

shinyUI(fluidPage(
  
  # App title ----
  titlePanel("12x25m Test"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(("Data Input"),
               numericInput("E1","25m #1",12),
               numericInput("E2","25m #2",12.5),
               numericInput("E3","25m #3",12.8),
               numericInput("E4","25m #4",13.2),
               numericInput("E5","25m #5",13.8),
               numericInput("E6","25m #6",14.5),
               numericInput("E7","25m #7",14.5),
               numericInput("E8","25m #8",15),
               numericInput("E9","25m #9",15.5),
               numericInput("E10","25m #10",15.62),
               numericInput("E11","25m #11",15.75),
               numericInput("E12","25m #12",15.69),
               
               numericInput("A","First Coefficient Starting Value",1.5),
               numericInput("B","First Coefficient Starting Value",-0.05),
               numericInput("C","First Coefficient Starting Value",1.5),
               
               actionButton("CalcButton1", "Calculate")
               
  ),
  # Main panel for displaying outputs ----
  mainPanel(
    
    h3("12x25m Analysis App"),
    
    plotOutput("plot"),
    
    p("Time (sec)"),
            
            verbatimTextOutput("Time"),
            ("Cumulative Time (sec)"),
            verbatimTextOutput("CTime"),
            ("Velocity (m/s)"),
            verbatimTextOutput("V"),
            ("Model Coefficients"),
            verbatimTextOutput("coef1"),
            verbatimTextOutput("coef2"),
            verbatimTextOutput("coef3"),
    ("Peak Velocity (m/s)"),
    verbatimTextOutput("Peak"),
    ("Critical Speed (m/s)"),
            verbatimTextOutput("CS"),
            ("D'"),
            verbatimTextOutput("D"),
    ("Drop off %"),
    verbatimTextOutput("DropOff")
            )
))



