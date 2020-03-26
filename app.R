library(deSolve)
library(shiny)


#Set/change working directory
#setwd("~/Wits/PhD/Proposal/Covid19")


# Define UI for application that draws a histogram
ui <-  fluidPage(
  
  tabsetPanel(
    id="panels",
    tabPanel(title = strong("Simple Covid-19 Model"),
             titlePanel(" Parameters"),
             
             sidebarPanel(            
               numericInput(inputId="N", label = "population size", value=100000, min=1000, max=60000000, step=100),
               sliderInput(inputId="beta", label = "average contact rate per day", value = 0.55, min=0.01, max=1,step=0.01),
               sliderInput(inputId="rec", label = "Average natural recovery period (days)", value = 10, min=1, max=20,step=1),
               sliderInput(inputId="s", label = "Social Distancing parameter", value = 0.5, min=0.01, max=1,step=0.01),
               sliderInput(inputId="I", label = "Number infected", value = 100, min=1, max=1000,step=1),
               sliderInput(inputId="d", label = "Death rate", value = 0.02, min=0.01, max=1,step=0.01)
               
             ),
             # Show a plot model output
             mainPanel(
               plotOutput(outputId = "Plot") 
             ))
  ))

# Set the start and end time for the model simulation
times <- seq(0, 365, 1)

# set up a function to solve the equations
# Simple SIRS model for Covid19
sircovid19 <- function(t, x, parms)  {
  with(as.list(c(parms, x)), {
    N=S+I+R+D
    dS= - beta*I/N*S*s
    dI=beta*I/N*S*s-r*I -d*I
    dR=r*I  
    dD=d*I
    output <- c(dS, dI, dR, dD)
    list(output)
  })
}



# Define server logic required to run model and plot outputs
server <- function(input, output) {
  #Create parameters vector - reactive nature
  parametersR <- reactive(c(beta=input$beta,    
                            r=1/input$rec,              
                            s=input$s,
                            N=input$N,
                            d=input$d 
                        
  ))
  
  #Define intital values  
  initS<-reactive(input$N)# 
  initR<-0 # 
  initD<-0 # 
  initI<-400

  #Create reactive plot function
  output$Plot <- renderPlot({
    
    #initS=initN()-initI-initR # Susceptible (non-immune)
    state <- c(S = initS, I=initI, R = initR, D=initD)
    
    #Run model
    outR <- reactive(ode(times=times, y= state, func=sircovid19, parms= parametersR() ))
    #outR <- reactive(ode(y = state, times = times, func = sirsv, parms = parametersR()))  #reactive object
    out<-outR()
    
    # some more model outputs
    
    #Plots
    plot(out[,2], col="red", ylim=c(0,initS), type="l", main="SIR Model of Covid 19")
    lines(out[,3], col="blue")
    lines(out[,4], col="green")
    lines(out[,5], col="orange")
    lines(out[,2]+out[,3]+out[,4]+out[,5])
    legend("topright",legend=c("S", "I", "R","D", "N"), col=c("red", "blue", "green","orange", "black"), lty=c(1,1,1,1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

