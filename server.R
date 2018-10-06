library(cubature)
library(shiny)


shinyServer(
  
  function(input,output){
    
    
    output$A1<-renderPrint({
      
      
      A0<-input$rVO2
      VO2<-c(input$rVO2,input$fVO2)
      TimeM<-c(0,input$Time)
      TD1<-input$TD1
      tauVO2<-input$tauVO2
      
     as.numeric(coef(nls(VO2~A0+A1*(1-exp(-1*((TimeM-TD1)/tauVO2))),start=list(A1=2000))))
      
    })
    
    output$VO2kJ<-renderPrint({
      
      
      A0<-input$rVO2
      VO2<-c(input$rVO2,input$fVO2)
      TimeM<-c(0,input$Time/60)
      TD1<-input$TD1/60
      tauVO2<-input$tauVO2/60
      
      A1<-as.numeric(coef(nls(VO2~A0+A1*(1-exp(-1*((TimeM-TD1)/tauVO2))),start=list(A1=2000))))
      
      modelVO2<-function(x){A0+A1*(1-exp(-1*((x-TD1)/tauVO2)))}
      
      
     c((as.numeric(adaptIntegrate(modelVO2,upperLimit = input$Time/60,lowerLimit = TD1/60)[1])/1000)*20.9-((input$Time/60)*input$rVO2)/1000)
      
      
    })
    
    
    
    output$LakJ<-renderPrint({
      
      LaNet<-input$fLa-input$iLa
      LaO2<-LaNet*input$Mass*input$LaC
      (LaO2/1000)*20.9
      
    })
    
    output$PCrkJ<-renderPrint({
      
      
      PCr<-input$iPCR
      tauPCr<-input$tauPCr
      M<-input$Mass*input$MassRatio
      
      modelPCr<-function(x){PCr*(1-exp(-1*(x/tauPCr)))*M}
      
      (modelPCr(input$Time)/6.25)*.468
      
    })
    
   output$TotalkJ<-renderPrint({
     
     
     A0<-input$rVO2
     VO2<-c(input$rVO2,input$fVO2)
     TimeM<-c(0,input$Time/60)
     TD1<-input$TD1/60
     tauVO2<-input$tauVO2/60
     
     A1<-as.numeric(coef(nls(VO2~A0+A1*(1-exp(-1*((TimeM-TD1)/tauVO2))),start=list(A1=2000))))
     
     modelVO2<-function(x){A0+A1*(1-exp(-1*((x-TD1)/tauVO2)))}
     
     
     AER<-c((as.numeric(adaptIntegrate(modelVO2,upperLimit = input$Time/60,lowerLimit = TD1/60)[1])/1000)*20.9-((input$Time/60)*input$rVO2)/1000)
     
     LaNet<-input$fLa-input$iLa
     LaO2<-LaNet*input$Mass*input$LaC
     GLYC<-(LaO2/1000)*20.9
     
     PCr<-input$iPCR
     tauPCr<-input$tauPCr
     M<-input$Mass*input$MassRatio
     
     modelPCr<-function(x){PCr*(1-exp(-1*(x/tauPCr)))*M}
     
     PCR<-(modelPCr(input$Time)/6.25)*.468
     
     AER+GLYC+PCR
     
   })
    
   output$Percentages<-renderPrint({
     

       
       
       A0<-input$rVO2
       VO2<-c(input$rVO2,input$fVO2)
       TimeM<-c(0,input$Time/60)
       TD1<-input$TD1/60
       tauVO2<-input$tauVO2/60
       
       A1<-as.numeric(coef(nls(VO2~A0+A1*(1-exp(-1*((TimeM-TD1)/tauVO2))),start=list(A1=2000))))
       
       modelVO2<-function(x){A0+A1*(1-exp(-1*((x-TD1)/tauVO2)))}
       
       
       AER<-c((as.numeric(adaptIntegrate(modelVO2,upperLimit = input$Time/60,lowerLimit = TD1/60)[1])/1000)*20.9-((input$Time/60)*input$rVO2)/1000)
       
       LaNet<-input$fLa-input$iLa
       LaO2<-LaNet*input$Mass*input$LaC
       GLYC<-(LaO2/1000)*20.9
       
       PCr<-input$iPCR
       tauPCr<-input$tauPCr
       M<-input$Mass*input$MassRatio
       
       modelPCr<-function(x){PCr*(1-exp(-1*(x/tauPCr)))*M}
       
       PCR<-(modelPCr(input$Time)/6.25)*.468
       
       Tot<-AER+GLYC+PCR
       
       c((AER/Tot)*100,(GLYC/Tot)*100,(PCR/Tot)*100)

     
     
   })
   
   
  })


