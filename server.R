library(shiny)
library(mosaic)
library(cubature)

shinyServer(
  
  function(input,output){
    
    output$Time<-reactive({c(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11,input$E12)
    })

    
    output$CTime<-reactive({c(sum(input$E1),sum(input$E1,input$E2),sum(input$E1,input$E2,input$E3),sum(input$E1,input$E2,input$E3,input$E4),sum(input$E1,input$E2,input$E3,input$E4,input$E5),sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6),sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7),sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8),sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9),sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10),sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11),sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11,input$E12))})
    output$V<-reactive({c(round(25/input$E1,2),round(25/input$E2,2),round(25/input$E3,2),round(25/input$E4,2),round(25/input$E5,2),round(25/input$E6,2),round(25/input$E7,2),round(25/input$E8,2),round(25/input$E9,2),round(25/input$E10,2),round(25/input$E11,2),round(25/input$E12,2))})
    
#    output$model<-renderPrint({
      
#      input$CalcButton1
      
#      isolate(nls(c(round(25/input$E1,2),round(25/input$E2,2),round(25/input$E3,2),round(25/input$E4,2),
#                   round(25/input$E5,2),round(25/input$E6,2),round(25/input$E7,2),round(25/input$E8,2),
#                   round(25/input$E9,2),round(25/input$E10,2),round(25/input$E11,2),round(25/input$E12,2))
#                 ~(a*exp(b*c(
#                   sum(input$E1),sum(input$E1,input$E2),sum(input$E1,input$E2,input$E3),
#                   sum(input$E1,input$E2,input$E3,input$E4),sum(input$E1,input$E2,input$E3,input$E4,input$E5),
#                   sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6),
#                   sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7),
#                   sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8),
#                   sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9),
#                   sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10),
#                   sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11),
#                   sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11,input$E12))
#                 )+c),start=list(a=1.5,b=-0.05,c=1.5)  )  
#              )
#                            })
    
    
    output$coef1<-renderPrint({
      
      input$CalcButton1
      
      c.time<-c(sum(input$E1),sum(input$E1,input$E2),sum(input$E1,input$E2,input$E3),
      sum(input$E1,input$E2,input$E3,input$E4),sum(input$E1,input$E2,input$E3,input$E4,input$E5),
      sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6),
      sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7),
      sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8),
      sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9),
      sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10),
      sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11),
      sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11,input$E12))
      
      vel<-c(round(25/input$E1,2),round(25/input$E2,2),round(25/input$E3,2),round(25/input$E4,2),
             round(25/input$E5,2),round(25/input$E6,2),round(25/input$E7,2),round(25/input$E8,2),
             round(25/input$E9,2),round(25/input$E10,2),round(25/input$E11,2),round(25/input$E12,2))
      
      isolate(coef(
      nls(vel~(a*exp(b*c.time)+c),start=list(a=input$A,b=input$B,c=input$C)  )  )[1] )
    
      
      
      
      })
    output$coef2<-renderPrint({
      
      input$CalcButton1
      
      c.time<-c(sum(input$E1),sum(input$E1,input$E2),sum(input$E1,input$E2,input$E3),
                sum(input$E1,input$E2,input$E3,input$E4),sum(input$E1,input$E2,input$E3,input$E4,input$E5),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11,input$E12))
      
      vel<-c(round(25/input$E1,2),round(25/input$E2,2),round(25/input$E3,2),round(25/input$E4,2),
             round(25/input$E5,2),round(25/input$E6,2),round(25/input$E7,2),round(25/input$E8,2),
             round(25/input$E9,2),round(25/input$E10,2),round(25/input$E11,2),round(25/input$E12,2))
      
      isolate(coef(
        nls(vel~(a*exp(b*c.time)+c),start=list(a=input$A,b=input$B,c=input$C)  )  )[2] )
      
      
      
      
    })
    output$coef3<-renderPrint({
      
      input$CalcButton1
      
      c.time<-c(sum(input$E1),sum(input$E1,input$E2),sum(input$E1,input$E2,input$E3),
                sum(input$E1,input$E2,input$E3,input$E4),sum(input$E1,input$E2,input$E3,input$E4,input$E5),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11,input$E12))
      
      vel<-c(25/input$E1,25/input$E2,25/input$E3,25/input$E4,25/input$E5,25/input$E6,
             25/input$E7,25/input$E8,25/input$E9,25/input$E10,25/input$E11,25/input$E12)
      
      isolate(coef(
        nls(vel~(a*exp(b*c.time)+c),start=list(a=input$A,b=input$B,c=input$C)  )  )[3] )
      
      
      
      
    })
    
    output$CS<-reactive({
      
      round((sort(c(25/input$E1,25/input$E2,25/input$E3,25/input$E4,25/input$E5,25/input$E6,
             25/input$E7,25/input$E8,25/input$E9,25/input$E10,25/input$E11,25/input$E12),decreasing = TRUE)[11]
      +sort(c(25/input$E1,25/input$E2,25/input$E3,25/input$E4,25/input$E5,25/input$E6,
              25/input$E7,25/input$E8,25/input$E9,25/input$E10,25/input$E11,25/input$E12),decreasing = TRUE)[12])/2,2)
            
      
      
    })
    
    output$D<-renderPrint({
      
      input$CalcButton1
      
      c.time<-c(sum(input$E1),sum(input$E1,input$E2),sum(input$E1,input$E2,input$E3),
                sum(input$E1,input$E2,input$E3,input$E4),sum(input$E1,input$E2,input$E3,input$E4,input$E5),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11,input$E12))
      
      vel<-c(25/input$E1,25/input$E2,25/input$E3,25/input$E4,25/input$E5,25/input$E6,
             25/input$E7,25/input$E8,25/input$E9,25/input$E10,25/input$E11,25/input$E12)
      
      upper<-sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11,input$E12)
      lower<-input$E1
    
      a<-coef(nls(vel~(a*exp(b*c.time)+c),start=list(a=input$A,b=input$B,c=input$C)  )  )[1] 
      b<-coef(nls(vel~(a*exp(b*c.time)+c),start=list(a=input$A,b=input$B,c=input$C)  )  )[2]
      c<-coef(nls(vel~(a*exp(b*c.time)+c),start=list(a=input$A,b=input$B,c=input$C)  )  )[3]
      
      CS<-(sort(c(25/input$E1,25/input$E2,25/input$E3,25/input$E4,25/input$E5,25/input$E6,
              25/input$E7,25/input$E8,25/input$E9,25/input$E10,25/input$E11,25/input$E12),decreasing = TRUE)[11]
        +sort(c(25/input$E1,25/input$E2,25/input$E3,25/input$E4,25/input$E5,25/input$E6,
                25/input$E7,25/input$E8,25/input$E9,25/input$E10,25/input$E11,25/input$E12),decreasing = TRUE)[12])/2
      
      
      model<-function(x){a*exp(b*x)+c}
      
      as.numeric(adaptIntegrate(model,upperLimit = upper,lowerLimit = lower)[1])-
        (CS*(upper-lower))+(25-(CS*lower))
      
     
           
    })
    
    
    
      output$plot<-renderPlot({
      
      
      c.time<-c(sum(input$E1),sum(input$E1,input$E2),sum(input$E1,input$E2,input$E3),
                sum(input$E1,input$E2,input$E3,input$E4),sum(input$E1,input$E2,input$E3,input$E4,input$E5),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11),
                sum(input$E1,input$E2,input$E3,input$E4,input$E5,input$E6,input$E7,input$E8,input$E9,input$E10,input$E11,input$E12))
      
      vel<-c(25/input$E1,25/input$E2,25/input$E3,25/input$E4,25/input$E5,25/input$E6,
             25/input$E7,25/input$E8,25/input$E9,25/input$E10,25/input$E11,25/input$E12)
      
      a<-coef(nls(vel~(a*exp(b*c.time)+c),start=list(a=input$A,b=input$B,c=input$C)  )  )[1] 
      b<-coef(nls(vel~(a*exp(b*c.time)+c),start=list(a=input$A,b=input$B,c=input$C)  )  )[2]
      c<-coef(nls(vel~(a*exp(b*c.time)+c),start=list(a=input$A,b=input$B,c=input$C)  )  )[3]
      
      CS<-(sort(c(25/input$E1,25/input$E2,25/input$E3,25/input$E4,25/input$E5,25/input$E6,
                  25/input$E7,25/input$E8,25/input$E9,25/input$E10,25/input$E11,25/input$E12),decreasing = TRUE)[11]
           +sort(c(25/input$E1,25/input$E2,25/input$E3,25/input$E4,25/input$E5,25/input$E6,
                   25/input$E7,25/input$E8,25/input$E9,25/input$E10,25/input$E11,25/input$E12),decreasing = TRUE)[12])/2
      
      
      model<-function(x){a*exp(b*x)+c}
      
      plot(c.time,vel,pch=20,bty="l",ylab="Velocity (m/s)",xlab="Time (sec)")
      lines(c.time,model(c.time),lty=2,lwd=1.5)
      lines(c.time,rep(CS,length(c.time)),lwd=2,lty=2,col="Orange2")
      legend("topright",bty="n",legend = c("Velocity","Model Fit","Critical Speed"),pch=c(20,NA,NA),lty=c(NA,2,2),lwd=c(NA,1.5,2),col=c("black","black","orange2"))
      
      
    })
    
      output$DropOff<-renderPrint({
        
        input$CalcButton1
        
 
        vel<-c(25/input$E1)
        
      
        CS<-(sort(c(25/input$E1,25/input$E2,25/input$E3,25/input$E4,25/input$E5,25/input$E6,
                    25/input$E7,25/input$E8,25/input$E9,25/input$E10,25/input$E11,25/input$E12),decreasing = TRUE)[11]
             +sort(c(25/input$E1,25/input$E2,25/input$E3,25/input$E4,25/input$E5,25/input$E6,
                     25/input$E7,25/input$E8,25/input$E9,25/input$E10,25/input$E11,25/input$E12),decreasing = TRUE)[12])/2
        
        round(((vel-CS)/vel)*100,1)
        
        
        
      })
      
      output$Peak<-renderPrint({
        
        input$CalcButton1
        
        
        round(25/input$E1,2)
        
      
        
        
        
      })
    
    })