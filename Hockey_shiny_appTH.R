#trying the shiny app
require(mgcv)
require(nhlscraper)
require(shiny)
require(tidyverse)
pbp_cleaned = readRDS("pbp_cleaned.rds")

#data set of events within five-seconds after a face-off from the full 
#play by play data pulled from the pbp.R file
pbp_faceoffs = pbp_cleaned |>
  mutate(row_id = row_number()) |>
  filter(eventTypeDescKey == "faceoff") |>
  select(faceoff_row = row_id,
         gameId,                 
         periodNumber,                 
         faceoff_time = secondsElapsedInGame) |>
  mutate(faceoff_end = faceoff_time + 5)

events_after_faceoff2 = pbp_faceoffs |>
  inner_join(
    pbp_cleaned,
    by = join_by(
      gameId == gameId,
      periodNumber == periodNumber,
      faceoff_time < secondsElapsedInGame,   
      faceoff_end >= secondsElapsedInGame)) |>
  mutate(fo_success = as.factor(ifelse(eventTypeDescKey == "shot-on-goal" | eventTypeDescKey =="goal", 1, 0))) |>
  mutate(is_shot_atmpt = as.numeric(eventTypeDescKey == "shot-on-goal" | eventTypeDescKey == "missed-shot" | eventTypeDescKey == "goal" )) |>
  mutate(faceoffDotCategory = case_when(xCoord == -69 & yCoord == 22 ~ '1',
                                        xCoord == -20 & yCoord == 22 ~ '2',
                                        xCoord == 20 & yCoord == 22 ~ '3',
                                        xCoord == 69 & yCoord == 22 ~ '4',
                                        xCoord == -69 & yCoord == -22 ~ '5',
                                        xCoord == -20 & yCoord == -22 ~ '6',
                                        xCoord == 20 & yCoord == -22 ~ '7',
                                        xCoord == 69 & yCoord == -22 ~ '8',
                                        xCoord == 0 & yCoord == 0 ~ '0')) |>
  mutate(scoreState = as.factor(case_when(goalDifferential >= 4 | goalDifferential <= -4 ~ '+/- 4 or greater',
                                          goalDifferential %in% c(-3,3) ~ '+/- 3',
                                          goalDifferential %in% c(-2,2) ~ '+/- 2',
                                          goalDifferential %in% c(-1,1) ~ '+/- 1',
                                          goalDifferential == 0 ~ '0'))) |>
  mutate(leftRight = as.factor(case_when(homeTeamDefendingSide == 'left' & eventTeamVenue == 'home'~
                                           if_else(faceoffDotCategory %in% c('1','2','3','4'),'L','R'),
                                         homeTeamDefendingSide == 'left' & eventTeamVenue == 'away'~
                                           if_else(faceoffDotCategory %in% c('5','6','7','8'),'L','R'),
                                         homeTeamDefendingSide == 'right' & eventTeamVenue == 'home'~
                                           if_else(faceoffDotCategory %in% c('5','6','7','8'),'L','R'),
                                         homeTeamDefendingSide == 'right' & eventTeamVenue == 'away'~
                                           if_else(faceoffDotCategory %in% c('1','2','3','4'),'L','R')))) |>
  mutate(zoneCode = as.factor(if_else(zoneCode == 'N' & faceoffDotCategory != 'C',
                                      case_when(homeTeamDefendingSide == 'left' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('2','6') ~ 
                                                  'C-NZ',
                                                homeTeamDefendingSide == 'left' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('3','7') ~
                                                  'F-NZ',
                                                homeTeamDefendingSide == 'left' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('2','6') ~
                                                  'F-NZ',
                                                homeTeamDefendingSide == 'left' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('3','7') ~
                                                  'C-NZ',
                                                homeTeamDefendingSide == 'right' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('2','6') ~
                                                  'F-NZ',
                                                homeTeamDefendingSide == 'right' & eventTeamVenue == 'home' & faceoffDotCategory %in% c('3','7') ~
                                                  'C-NZ',
                                                homeTeamDefendingSide == 'right' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('2','6') ~
                                                  'C-NZ',
                                                homeTeamDefendingSide == 'right' & eventTeamVenue == 'away' & faceoffDotCategory %in% c('3','7') ~
                                                  'F-NZ'),zoneCode))) |>
  mutate(zoneCode = as.factor(if_else(zoneCode == 'N','C', zoneCode)))|>
  mutate(situationDescriptor = as.factor(case_when(is.na(zoneCode) | is.na(leftRight) ~ NA_character_,
                                                   TRUE ~ paste(zoneCode, leftRight, sep = ' ')))) |>
  mutate(situationLinkage = as.factor(case_when(situationDescriptor %in% c('D L', 'O R') ~
                                                  'A',
                                                situationDescriptor %in% c('D R', 'O L') ~
                                                  'B',
                                                situationDescriptor %in% c('F-NZ R', 'C-NZ L') ~
                                                  'C',
                                                situationDescriptor %in% c('F-NZ L', 'C-NZ R') ~
                                                  'D',
                                                situationDescriptor == 'C R' ~
                                                  'E'))) |>
  mutate(periodNumber = as.factor(periodNumber)) |>
  mutate(isEmptyNetFor = as.factor(isEmptyNetFor)) |>
  mutate(isEmptyNetAgainst = as.factor(isEmptyNetAgainst)) |>
  mutate(strengthState = as.factor(strengthState)) |>
  mutate(eventTypeDescKey = as.factor(eventTypeDescKey)) |>
  filter(periodType == "REG")


events_after_faceoff2 = events_after_faceoff2 |>
  group_by(faceoff_row) |>
  mutate(future_shot = as.numeric(any(eventTypeDescKey %in%
        c("shot-on-goal","missed-shot","goal")))) |>
  ungroup()
events_model = events_after_faceoff2 |> #training model
  filter(!eventTypeDescKey %in% c("shot-on-goal","missed-shot","goal"))


#testing and training new model
set.seed(071526)

faceoff_ids = unique(events_model$faceoff_row)

train_ids = sample(
  faceoff_ids,
  size = 0.75 * length(faceoff_ids))

train_data = events_model |>
  filter(faceoff_row %in% train_ids)

test_data = events_model |>
  filter(!faceoff_row %in% train_ids)


test.mod = bam(future_shot~ eventTypeDescKey + s(xCoord,yCoord, k = 30) + 
                 s(distance, k = 20) + s(secondsElapsedInGame) + strengthState*scoreState +
                 isEmptyNetFor + isEmptyNetAgainst + s(angle, k = 15) , 
               data = train_data, family = binomial(link = logit), method = "fREML", discrete = TRUE)
summary(test.mod)

test_data$pred_prob = predict(test.mod, newdata = test_data, type = "response")

test_data$pred_class = ifelse(test_data$pred_prob > 0.5, 1, 0)

table(Actual = test_data$future_shot, Predicted = test_data$pred_class)


#Modeling probability of a shot attempt following a face-off
#renaming test.mod
nhl.mod =  bam(future_shot~ eventTypeDescKey + s(xCoord,yCoord, k = 30) + 
                 s(distance, k = 20) + s(secondsElapsedInGame) + strengthState*scoreState +
                 isEmptyNetFor + isEmptyNetAgainst + s(angle, k = 15) , 
               data = train_data, family = binomial(link = logit), method = "fREML", discrete = TRUE)
summary(nhl.mod)
saveRDS(nhl.mod, "nhl_mod.rds")

#for every obs the pred prob of a shot attempt
nhl.mod_pred_prob = predict(nhl.mod, type = "response") 
nhl.mod_pred_class = ifelse(nhl.mod_pred_prob > 0.5, "Win", "Loss") 
nhl.mod_pred_binary = ifelse(nhl.mod_pred_prob > 0.5, 1, 0)

#pretty solid BRIER Score of 0.086
mean((nhl.mod_pred_binary - nhl.mod_pred_prob)^2)


#checking the accuracy of the predictions of the shot probabilities
#creating a new variable shot_prob of the probability of shots following a face-off
shot_results = train_data |>
  select(future_shot, eventTypeDescKey, xCoord, yCoord,distance, scoreState, strengthState,
         secondsElapsedInGame, angle, isEmptyNetFor, isEmptyNetAgainst) |>
  drop_na() |>
  mutate(shot_prob = predict(nhl.mod, type = "response"),
         pred_decile2 = ntile(shot_prob, 10))

#checks each level of predictions withe the actual results
shot_calibration_check =  shot_results |>
  group_by(pred_decile2) |>
  summarize(
    predicted = mean(shot_prob),
    actual = mean(future_shot),
    n = n(),
    .groups = "drop")
shot_calibration_check #only slightly off

roc_obj2 = roc(
  response = shot_results$future_shot,
  predictor = shot_results$shot_prob,
  quiet = TRUE)

auc(roc_obj2) #AUC is 0.858

#creating an ROC curve
#library(pROC)

shot_roc = tibble(threshold = c(roc_obj2$thresholds),
                  specificity = roc_obj2$specificities,
                  sensitivity = roc_obj2$sensitivities)

shot_roc |> 
  ggplot(aes(x = 1 - specificity , y = sensitivity)) + #1-specificity (false pos. rate)
  geom_path() +
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed")

#getting started with shiny

nhl.mod = readRDS("nhl_mod.rds")

ui = fluidPage(
  titlePanel("NHL Faceoff Shot Probability"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("isEmptyNetFor", "Net is Empty for Event Team", FALSE),
      checkboxInput("isEmptyNetAgainst", "Net is Empty for Team Against Event", FALSE),
      numericInput("secondsElapsedInGame","Seconds Elapsed in Game",min = 0,max = 3600,value = 1200),
      selectInput("strengthState","Strength State",choices = levels(events_model$strengthState)),
      selectInput("scoreState","Score State",choices = levels(events_model$scoreState)),
      selectInput("eventTypeDescKey", "Event Type" , choices = levels(events_model$eventTypeDescKey)),
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
    events = data.frame(eventTypeDescKey = character(), xCoord = numeric(), yCoord = numeric(),
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
        eventTypeDescKey = "faceoff",
        xCoord=faceoff_dots$x[closest],
        yCoord=faceoff_dots$y[closest])
    }
    else {
      sequence$events = rbind(sequence$events, data.frame(
        eventTypeDescKey = input$eventTypeDescKey,
        xCoord=click_x, yCoord=click_y))
    }
    
  })
  
  output$events = renderTable({
    sequence$events})
  
  #draws the rink
  output$rink = renderPlot({
     draw_NHL_rink() +
       coord_fixed(xlim = c(-100, 100),ylim = c(-43, 43))
    
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
      paste0(round(prob * 100, 1), "% probability of a shot attempt")})
  })
  
  observeEvent(input$reset,{
    sequence$events = data.frame(
      eventTypeDescKey=character(), xCoord=numeric(), yCoord=numeric(),
      secondsElapsedInGame = numeric())
  })
}

shinyApp(ui, server)


