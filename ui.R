

shinyUI(fluidPage(
  
  # App title ----
  titlePanel("Complete Energetic Breakdown"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(("Data Input"),
               numericInput("Mass","Mass (kg)",70),
               numericInput("FFM","Fat Free Mass (kg)",45),
               numericInput("iLa","Initial [La]",0.8),
               numericInput("fLa","Final [La]",10.0),
               numericInput("Time","Time (sec)",30.0),
               numericInput("rVO2","Resting VO2 (ml/min)",900),
               numericInput("fVO2","Final VO2 Pk",5000),
               numericInput("iPCR","Initial [PCr] (mM/kg)",27.75),
               numericInput("tauPCr","Tau (PCr)",23.3),
               numericInput("tauVO2","Tau (VO2)",22.7),
               numericInput("MassRatio","Body Mass:Active Muscle Mass Ratio",.3),
               numericInput("TD1","Time Delay 1",5.49),
               numericInput("LaC","Lactate:ml O2 Ratio",2.71)
   

               
  ),
  # Main panel for displaying outputs ----
  mainPanel(
    
    h3("Complete Energetic Breakdown Analysis App"),
    ("A1"),
    verbatimTextOutput("A1"),
    ("Total Aerobic Energy Contribution (kJ)"),
    verbatimTextOutput("VO2kJ"),
    ("Glycolytic Energy Contribution (kJ)"),
    verbatimTextOutput("LakJ"),
    ("Phosphocreatine Energy Contribution (kJ)"),
    verbatimTextOutput("PCrkJ"),
    ("Total Energy (kJ)"),
    verbatimTextOutput("TotalkJ"),
    ("Percentages - AER - GLY - PCR"),
    verbatimTextOutput("Percentages")
 
  )))

