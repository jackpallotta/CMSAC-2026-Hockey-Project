#trying the shiny app
require(mgcv)
require(nhlscraper)
require(shiny)
require(dplyr)
require(ggplot2)
require(arrow)


#getting started with shiny
events_after_faceoff2 = readRDS("events_after_faceoff2.rds")
events_model = readRDS("events_model.rds")
train_data = readRDS("train_data.rds")
nhl.mod = readRDS("nhl_mod.RDS")


ui = fluidPage(
  titlePanel("NHL Faceoff Shot Probability"),
  h4("App Description"),
  p("This app is designed to allow the user to create a sequence of events directly 
    following a faceoff and calculate the probability of a future shot attempt 
    after that sequence of events. The User is able to click on the rink to 
    determine the positions of each event. However, the first event will always 
    be a faceoff and the user's click on the rink will be automatically 'snapped' 
    to the nearest faceoff dot. The user does have to manually change the seconds 
    elapsed in the game for each event."),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("isEmptyNetFor", "Net is Empty for Event Team", FALSE),
      checkboxInput("isEmptyNetAgainst", "Net is Empty for Team Against Event", FALSE),
      numericInput("secondsElapsedInGame","Seconds Elapsed in Game",min = 0,max = 3600,value = 1200),
      selectInput("strengthState","Strength State",choices = levels(events_model$strengthState)),
      selectInput("scoreState","Score State",choices = levels(events_model$scoreState)),
      selectInput("eventTypeDescKey", "Event Type" , choices = c("blocked-shot","giveaway","hit","missed-shot",
                                                                 "penalty", "shot-on-goal","takeaway")),
      actionButton("predict", "Calculate Probability"),
      actionButton("reset", "Reset Sequence")),
    mainPanel(
      plotOutput("rink",
                 click = "rink_click",
                 height = "600px"),
      h3("Event Sequence"),
      tableOutput("events"),
      h2(textOutput("probability")))))

server = function(input, output) {
  #stores the selected faceoff location
  sequence = reactiveValues(
    events = data.frame(eventTypeDescKey= character(), xCoord = numeric(), yCoord = numeric(),
                        secondsElapsedInGame = numeric()))
  
  faceoff_dots = data.frame(
    x = c(-69, -20, 20, 69, -69, -20, 20, 69, 0),
    y = c(22, 22, 22, 22, -22, -22, -22, -22, 0 ))
  
  #updates coordinates when user clicks
  observeEvent(input$rink_click, {
    click_x = input$rink_click$x
    click_y = input$rink_click$y
    
    if(nrow(sequence$events) == 0){
      dist = sqrt((faceoff_dots$x-click_x)^2 + (faceoff_dots$y-click_y)^2)
      
      closest = which.min(dist)
      
      sequence$events = data.frame(
        eventTypeDescKey = "faceoff", #Event 
        xCoord=faceoff_dots$x[closest],
        yCoord=faceoff_dots$y[closest],
        secondsElapsedInGame = input$secondsElapsedInGame)
    }
    else {
      sequence$events = rbind(sequence$events, data.frame(
        eventTypeDescKey = input$eventTypeDescKey, #Event
        xCoord=click_x, yCoord=click_y,
        secondsElapsedInGame = input$secondsElapsedInGame))
    }
    
  })
  
  output$events = renderTable({
    sequence$events})
  
  #draws the rink
  output$rink = renderPlot({
    draw_NHL_rink()
    coord_fixed(xlim = c(-100, 100),ylim = c(-43, 43))
    
    axis(side = 1, at = c(-100,0, 100), labels = c("-100", "0", "100"))
    axis(side = 4, at = c(-43, 0 , 43), labels = c("-43", "0", "43"))
    
    box(which = "outer")
    
    if (nrow(sequence$events) >0) {
      graphics::points(x = sequence$events$xCoord, y = sequence$events$yCoord, 
                       col = "black", pch = 19, cex = 2)
      
      graphics::lines(sequence$events$xCoord, sequence$events$yCoord, col="green2", lwd=2)}
  })
  
  #calculates probability
  observeEvent(input$predict, {
    req(nrow(sequence$events)>0)
    current = tail(sequence$events, 1)
    distance = sqrt((89 - current$xCoord)^2 + current$yCoord^2)
    angle = atan2(abs(current$yCoord), 89 - current$xCoord) * 180 / pi
    new_data = data.frame(
      xCoord = current$xCoord,
      yCoord = current$yCoord,
      distance = distance,
      angle = angle,
      secondsElapsedInGame = input$secondsElapsedInGame,
      
      strengthState = factor(input$strengthState,
                             levels = levels(train_data$strengthState)),
      
      scoreState = factor(input$scoreState,
                          levels = levels(train_data$scoreState)),
      
      isEmptyNetFor = factor(input$isEmptyNetFor,
                             levels = levels(train_data$isEmptyNetFor)),
      
      isEmptyNetAgainst = factor(input$isEmptyNetAgainst,
                                 levels = levels(train_data$isEmptyNetAgainst)),
      
      eventTypeDescKey = factor(current$eventTypeDescKey, 
                                levels = levels(train_data$eventTypeDescKey)))
    
    prob = predict(
      nhl.mod, newdata = new_data,
      type = "response")
    
    output$probability = renderText({
      paste0(round(prob * 100, 1), "% probability of a future shot attempt following 
             the sequence of events after the faceoff shown")})
  })
  
  observeEvent(input$reset,{
    sequence$events = data.frame(
      eventTypeDescKey=character(), xCoord=numeric(), yCoord=numeric(), #EVent
      secondsElapsedInGame = numeric())
    
  })
  
}

shinyApp(ui, server)


