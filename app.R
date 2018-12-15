#11 Nov 2018
#Tansley Insight
#Mario Vallejo-Marin

#Deployed: https://plant-evolution.shinyapps.io/Buzz-pollination-vibrations/

#Load libraries
library(shiny)
library(shinydashboard)
library(reshape2)

#Set colour palette:
pal<-c("#56B4E9",  "#E69F00", "#000000")

#Define variables
buzzvariables<-c("Velocity", "Acceleration",
                 "Displacement")

#User interface:
ui <- dashboardPage(skin="blue",
                    dashboardHeader(title="Buzz Pollination"),
                    sidebar<- dashboardSidebar(
                      collapsed = TRUE,
                      sidebarMenu(
                        menuItem("Plots", tabName = "plots", icon = icon("bar-chart-o") 
                        ),
                        menuItem("ReadMe", tabName = "readme", icon = icon("readme"))
                      )
                    ),
                    
                    body <-  dashboardBody(
                      tabItems(
                        tabItem(tabName = "plots",
                                h4("Plots")
                        ),
                        tabItem(tabName = "readme",
                                h4("Read Me")
                        )
                      ),
                      fluidPage(
                        titlePanel("Buzz pollination: Vibration properties"),
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("frequency",
                                        "Select frequency value (Hz):",
                                        min = 100,
                                        max = 1000,
                                        value = 300),
                            box(      width=18,  
                                      radioButtons(inputId="varchosen", label="Select one additional variable:", 
                                                   choices  = buzzvariables,
                                                   selected = list("Acceleration")),
                                      conditionalPanel(
                                        condition = "input.varchosen.includes('Acceleration')",           
                                        sliderInput("max.accel",
                                                    "Peak acceleration (m/s^2):",
                                                    min = 0,
                                                    max = 500,
                                                    value = 10)
                                      ),
                                      conditionalPanel(
                                        condition = "input.varchosen.includes('Velocity')", 
                                        sliderInput("max.vel",
                                                    "Peak velocity (m/s):",
                                                    min = 0,
                                                    max = 1,
                                                    step= 0.01,
                                                    value = 0.05)
                                      ),
                                      conditionalPanel(
                                        condition = "input.varchosen.includes('Displacement')", 
                                        sliderInput("max.disp",
                                                    "Peak displacement (m):",
                                                    min = 0,
                                                    max = 50e-6,
                                                    step = 1e-6,
                                                    value = 4e-6)
                                      )
                            )
                          ),
                          # Show a plot of the generated sinewave
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("All", plotOutput("iallPlot"),
                                                 p(),
                                                 p(strong("Figure S1."), 
                                                   "Instantaneous displacement, velocity and acceleration vs. time",
                                                   style = "font-size: 14pt")
                                        ),
                                        tabPanel("Displacement", plotOutput("idisplacementPlot"),
                                                 p(),
                                                 p(strong("Figure S2."), 
                                                   "Instantaneous displacement vs. time",
                                                   style = "font-size: 14pt"),
                                                 p("DA = Peak Displacement Amplitude; 
                                                   VA = Peak Velocity Amplitude;
                                                   AA = Peak Acceleration Amplitude")
                                                 ),
                                        tabPanel("Velocity", plotOutput("ivelPlot"),
                                                 p(),
                                                 p(strong("Figure S3."), "Instantaneous velocity vs. time",
                                                   style = "font-size: 14pt"),
                                                 p("DA = Peak Displacement Amplitude; 
                                                   VA = Peak Velocity Amplitude;
                                                   AA = Peak Acceleration Amplitude")
                                                 ),
                                        tabPanel("Acceleration", plotOutput("iaccelPlot"),
                                                 p(),
                                                 p(strong("Figure S4."), "Instantaneous acceleration vs. time",
                                                   style = "font-size: 14pt"),
                                                 p("DA = Peak Displacement Amplitude; 
                                                   VA = Peak Velocity Amplitude;
                                                   AA = Peak Acceleration Amplitude")
                                                 ),
                                        tabPanel("Constant Displacement", plotOutput("ifreqvelaccPlot"),
                                                 p(),
                                                 p(strong("Figure S5."), 
                                                   "Change in peak acceleration and peak velocity as a function of frequency, 
                                                   assuming a constant peak displacement.",
                                                   style = "font-size: 14pt"),
                                                 p("Change in either peak amplitude (left-hand side panel) or 
                                                   peak velocity (right-hand side panel) as a function of frequency can
                                                   be calculated for a given maximum displacement (peak displacement amplitude, DA)."),
                                                 p("The red dashed line shows the relationship calculated for half the input DA")
                                                 ),
                                        tabPanel("Constant Acceleration", plotOutput("ifreqdispPlot"),
                                                 p(),
                                                 p(strong("Figure S6."), 
                                                   "Change in peak displacement and peak velocity as a function of frequency, 
                                                   assuming a constant peak acceleration",
                                                   style = "font-size: 14pt"),
                                                 p("Change in either peak displacement (left-hand side panel) or 
                                                   peak velocity (right-hand side panel) as a function of frequency can
                                                   be calculated for a constant value of maximum acceleration (peak acceleration amplitude, AA)."),
                                                 p("The purple dashed line shows the relationship calculated for half the input AA")
                                                 )
                                        ),
                            textOutput("frequency")
                            #textOutput("velocity")
                                        )
                                                 )
                                                 )
                      ),
                    
                    dashboardPage(
                      dashboardHeader(title = "Buzz pollination App"),
                      sidebar,
                      body
                    )
                    
                    )

# Define server logic 
server <- function(input, output) {
  ttime<-seq(0,.01,length.out=1000)
  output$ivelPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    if(!("Acceleration" %in% input$varchosen)){
      if("Displacement" %in% input$varchosen){
        max.accel <- (2*pi*input$frequency)^2 * input$max.disp
      } else {
        max.accel<-(2*pi*input$frequency) * input$max.vel
      }
    }  else {max.accel<-input$max.accel}
    
    if("Displacement" %in% input$varchosen){
      max.disp<-input$max.disp
    } else {max.disp <- max.accel/((2*pi*input$frequency)^2)}
    
    max.vel<- max.disp * (2 * pi * input$frequency )
    velt<- max.disp * (2 * pi * input$frequency ) * cos(2 * pi * input$frequency * ttime)
    plot(velt*1000~ttime, type="l", col=pal[2], lwd=2,
         ylim=c(min(velt)*2e3, max(velt)*2e3),
         main="Velocity", ylab="Velocity (mm/s)", xlab="Time (s)")
    abline(h=0, lty=2, col="lightgray")
    abline(v=0, lty=2, col="lightgray")
    
    legend("topright", cex=0.8, legend=c(paste("Frequency = ", input$frequency, "Hz"),
                                         paste("DA = ", round(max.disp*1e6,2), "um"),
                                         paste("VA = ", round(max.vel*1000,2), "mm/s"),
                                         paste("AA = ", round(max.accel,2), "m/s^2")),
           inset=0.02, box.col="white")
    
  })
  
  output$iaccelPlot <- renderPlot({
    if(!("Acceleration" %in% input$varchosen)){
      if("Displacement" %in% input$varchosen){
        max.accel <- (2*pi*input$frequency)^2 * input$max.disp
      } else {
        max.accel<-(2*pi*input$frequency) * input$max.vel
      }
    }  else {max.accel<-input$max.accel}
    
    if("Displacement" %in% input$varchosen){
      max.disp<-input$max.disp
    } else {max.disp <- max.accel/((2*pi*input$frequency)^2)}
    accelt<- -1 * max.disp*(2*pi*input$frequency)^2 * sin(2 * pi * input$frequency * ttime)
    max.vel<- max.disp * (2 * pi * input$frequency )
    
    pax<- plot(accelt~ttime, type="l", col=pal[3], lwd=2,
               ylim=c(min(accelt)*1.8, max(accelt)*1.8),
               main="Acceleration", ylab="Acceleration (m/s^2)", xlab="Time (s)")
    abline(h=0, lty=2, col="lightgray")
    abline(v=0, lty=2, col="lightgray")
    
    legend("topright", cex=0.8, legend=c(paste("Frequency = ", input$frequency, "Hz"),
                                         paste("DA = ", round(max.disp*1e6,2), "um"),
                                         paste("VA = ", round(max.vel*1000,2), "mm/s"),
                                         paste("AA = ", round(max.accel,2), "m/s^2")),
           inset=0.02, box.col="white")
    
  })
  
  output$idisplacementPlot <- renderPlot({
    if(!("Acceleration" %in% input$varchosen)){
      if("Displacement" %in% input$varchosen){
        max.accel <- (2*pi*input$frequency)^2 * input$max.disp
      } else {
        max.accel<-(2*pi*input$frequency) * input$max.vel
      }
    }  else {max.accel<-input$max.accel}
    
    if("Displacement" %in% input$varchosen){
      max.disp<-input$max.disp
    } else {max.disp <- max.accel/((2*pi*input$frequency)^2)}
    
    displacet<- max.disp*sin(2 * pi * input$frequency * ttime)
    max.vel<- max.disp * (2 * pi * input$frequency )
    
    plot(displacet*1e6~ttime, type="l", col=pal[1], lwd=2,
         ylim=c(min(displacet)*2e6, max(displacet)*2e6),
         main="Displacement", ylab="Displacement (um)", xlab="Time (s)")
    abline(h=0, lty=2, col="lightgray")
    abline(v=0, lty=2, col="lightgray")
    
    legend("topright", cex=0.8, legend=c(paste("Frequency = ", input$frequency, "Hz"),
                                         paste("DA = ", round(max.disp*1e6,2), "um"),
                                         paste("VA = ", round(max.vel*1000,2), "mm/s"),
                                         paste("AA = ", round(max.accel,2), "m/s^2")),
           inset=0.02, box.col="white")
  })
  
  output$ifreqvelaccPlot <- renderPlot({  
    vel.acc.fun<-function (Dmax, frequency){
      accel<- Dmax*(2*pi*frequency)^2 #* sin(2 * pi * frequency * ttime)
      vel<- Dmax*(2* pi * frequency ) #* cos(2* pi * frequency * ttime)
      return(data.frame(frequency=frequency,acceleration=accel,velocity=vel))
    }
    
    if(!("Acceleration" %in% input$varchosen)){
      if("Displacement" %in% input$varchosen){
        max.accel <- (2*pi*input$frequency)^2 * input$max.disp
      } else {
        max.accel<-(2*pi*input$frequency) * input$max.vel
      }
    }  else {max.accel<-input$max.accel}
    
    if("Displacement" %in% input$varchosen){
      max.disp<-input$max.disp
    } else {max.disp <- max.accel/((2*pi*input$frequency)^2)}
    
    freq.range<-seq(50,1000,length.out=1000)
    
    dat1<-vel.acc.fun(Dmax=max.disp,frequency=freq.range)
    dat2<-vel.acc.fun(Dmax=max.disp*0.5,frequency=freq.range)
    #Plot:   
    par(las=1, par(mfrow=c(1,2))) 
    par(mar=c(5.1,5.1,2.1,1.1))
    
    plot(dat1$acceleration~dat1$frequency, type="l",  lwd=2, col="black", 
         xlab="Frequency (Hz)", ylab="Acceleration (m/s^2)",mgp=c(3.3,1,0))
    abline(v=input$frequency, col="gray", lty=3)
    abline(h=max.accel, col="gray", lty=3)
    lines(dat2$acceleration~dat2$frequency, lwd=2,  lty=2, col="darkred")
    
    legend("topleft", lty=c(1,2), lwd=2, cex=0.6, legend=c(paste("DA =", round(max.disp*1e6,2), "um"),
                                                           paste("DA =",round(max.disp*0.5e6,2), "um")), 
           inset=.05, box.col="white", col=c("black", "darkred"))
    
    max.vel<- max.disp * (2 * pi * input$frequency )
    plot(dat1$velocity~dat1$frequency, type="l", 
         lwd=2, col="black", 
         xlab="Frequency (Hz)", ylab="Velocity (m/s)",mgp=c(3.3,1,0))
    abline(v=input$frequency, col="gray", lty=3)
    abline(h=max.vel, col="gray", lty=3)
    lines(dat2$velocity~dat2$frequency, lwd=2, lty=2, col="darkred")
    legend("topleft", lty=c(1,2), lwd=2, cex=0.6, legend=c(paste("DA =", round(max.disp*1e6,2), "um"),
                                                           paste("DA =",round(max.disp*0.5e6,2), "um")), 
           inset=.05, box.col="white", col=c("black", "darkred"))
    
  })
  
  output$ifreqdispPlot <- renderPlot({  
    vel.acc.fun<-function (Dmax, frequency){
      accel<- Dmax*(2*pi*frequency)^2 #* sin(2 * pi * frequency * ttime)
      vel<- Dmax*(2* pi * frequency ) #* cos(2* pi * frequency * ttime)
      return(data.frame(frequency=frequency,acceleration=accel,velocity=vel))
    }
    
    if(!("Acceleration" %in% input$varchosen)){
      if("Displacement" %in% input$varchosen){
        max.accel <- (2*pi*input$frequency)^2 * input$max.disp
      } else {
        max.accel<-(2*pi*input$frequency) * input$max.vel
      }
    }  else {max.accel<-input$max.accel}
    
    if("Displacement" %in% input$varchosen){
      max.disp<-input$max.disp
    } else {max.disp <- max.accel/((2*pi*input$frequency)^2)}
    
    freq.range<-seq(50,1000,length.out=1000)
    
    max.disp.fun <- function(max.accel, frequency){
      disp<- max.accel/((2*pi*frequency)^2)
      accel<- disp*(2*pi*frequency)^2 #* sin(2 * pi * frequency * ttime)
      vel<- disp*(2* pi * frequency ) #* cos(2* pi * frequency * ttime)
      return(data.frame(frequency=frequency,acceleration=accel,velocity=vel, 
                        displacement=disp/1e-6))
    }
    
    dat1<-max.disp.fun(max.accel=max.accel,frequency=freq.range)
    dat2<-max.disp.fun(max.accel=max.accel*0.5,frequency=freq.range)
    
    par(las=1, par(mfrow=c(1,2))) 
    par(mar=c(5.1,5.1,2.1,1.1))
    plot(dat1$displacement~dat1$frequency, log="y", type="l", 
         #ylim=c(0,0.02), 
         lwd=2, col="black", 
         xlab="Frequency (Hz)", ylab="Displacement (um)",mgp=c(3.3,1,0), 
         ylim=c(min(c(dat1$displacement,dat2$displacement)),
                max(c(dat1$displacement,dat2$displacement))))
    abline(v=input$frequency, col="gray", lty=3)
    abline(h=max.disp*1e6, col="gray", lty=3)
    lines(dat2$displacement~dat2$frequency, lwd=2, lty=2, col="purple")
    legend("topright", lty=c(1,2), lwd=2, cex=0.6, legend=c(paste("AA =",round(max.accel,2), "m/s^2"),
                                                            paste("AA =",round(max.accel*0.5,2), "m/s^2")), 
           inset=.05, box.col="white", col=c("black", "purple"))
    max.vel<- max.disp * (2 * pi * input$frequency )
    
    plot(dat1$velocity~dat1$frequency, log="y", type="l", 
         #ylim=c(0,0.02), 
         lwd=2, col="black", 
         xlab="Frequency (Hz)", ylab="Velocity (m/s)",mgp=c(3.3,1,0),
         ylim=c(min(c(dat1$velocity,dat2$velocity)),
                max(c(dat1$velocity,dat2$velocity))))
    abline(v=input$frequency, col="gray", lty=3)
    abline(h=max.vel, col="gray", lty=3)
    
    lines(dat2$velocity~dat2$frequency, lwd=2, lty=2, col="purple")
    
    legend("topright", lty=c(1,2), lwd=2, cex=0.6, legend=c(paste("AA =",round(max.accel,2), "m/s^2"),
                                                            paste("AA =",round(max.accel*0.5,2), "m/s^2")), 
           inset=.05, box.col="white", col=c("black", "purple"))
  })
  
  output$iallPlot <- renderPlot({
    if(!("Acceleration" %in% input$varchosen)){
      if("Displacement" %in% input$varchosen){
        max.accel <- (2*pi*input$frequency)^2 * input$max.disp
      } else {
        max.accel<-(2*pi*input$frequency) * input$max.vel
      }
    }  else {max.accel<-input$max.accel}
    
    if("Displacement" %in% input$varchosen){
      max.disp<-input$max.disp
    } else {max.disp <- max.accel/((2*pi*input$frequency)^2)}
    
    displacet<- max.disp*sin(2 * pi * input$frequency * ttime)
    
    par(mfrow=c(1,3), las=1)
    
    pdis<- plot(displacet*1e6~ttime, type="l", col=pal[1], lwd=2,
                ylim=c(min(displacet)*3e6, max(displacet)*3e6),
                main="Displacement", ylab="Displacement (um)", xlab="Time (s)")
    abline(h=0, lty=2, col="lightgray")
    abline(v=0, lty=2, col="lightgray")
    
    velt<- max.disp * (2 * pi * input$frequency ) * cos(2 * pi * input$frequency * ttime)
    pvel<- plot(velt*1000~ttime, type="l", col=pal[2], lwd=2,
                ylim=c(min(velt)*2e3, max(velt)*2e3),
                main="Velocity", ylab="Velocity (mm/s)", xlab="Time (s)")
    abline(h=0, lty=2, col="lightgray")
    abline(v=0, lty=2, col="lightgray")
    
    accelt<- -1 * max.disp*(2*pi*input$frequency)^2 * sin(2 * pi * input$frequency * ttime)
    
    pax<- plot(accelt~ttime, type="l", col=pal[3], lwd=2,
               ylim=c(min(accelt)*1.2, max(accelt)*1.2),
               main="Acceleration", ylab="Acceleration (m/s^2)", xlab="Time (s)")
    abline(h=0, lty=2, col="lightgray")
    abline(v=0, lty=2, col="lightgray")
    
  })
  
  output$frequency <- renderText({ 
    paste("Frequency: ", input$frequency, "Hz")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)