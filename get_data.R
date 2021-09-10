library(tidyverse)
library(readxl)

participant_data <- dir("./data", full.names = T) %>% map_df(
  ~read_csv(., col_types = cols_only(
    participantId = col_double(),
    condition = col_double(),
    trialNumber = col_double(),
    trialStart = col_double(),
    trialEnd = col_double(),
    expectedAngle = col_double(),
    drawnAngle = col_double(),
    wristRotation = col_double()
  )
  )
)

# participantIDs for walking
list <- c(11, 21, 31, 41, 52, 62, 72, 82, 91, 101, 111, 121, 132, 142, 152, 162, 171, 181, 192, 202)
walking_first <- c(1, 2, 3, 4, 9, 10, 11, 12, 17, 18)
jigsaw_first <- c(5, 6, 7, 8, 13, 14, 15, 16, 19, 20)

participant_data <- participant_data %>% mutate(task = case_when(
  participantId %in% list ~ 0, # gehen
  TRUE ~ 1 # puzzeln
))

participant_data <- participant_data %>% mutate(trialDuration = trialEnd - trialStart)
participant_data <- participant_data %>% filter(trialDuration < 10000 & trialDuration > 100)
participant_data <- participant_data %>% mutate(expectedAngle = (expectedAngle + 180) %% 360)
participant_data <- participant_data %>% mutate(drawnAngle = (drawnAngle + 180) %% 360)
participant_data <- participant_data %>% mutate(angleDelta = abs(((abs(expectedAngle - drawnAngle) + 180) %% 360) - 180))
participant_data <- participant_data %>% mutate(group = task * 2 + condition)

mental_data_walking <- read_excel("./Frageboegen/Gehen.xlsx")
mental_data_jigsaw <- read_excel("./Frageboegen/Puzzlen.xlsx")

mental_data_walking <- mental_data_walking %>% mutate(firstTask = case_when(
  participantId %in% walking_first ~ TRUE,
  TRUE ~ FALSE
))

mental_data_jigsaw <- mental_data_jigsaw %>% mutate(firstTask = case_when(
  participantId %in% jigsaw_first ~ TRUE,
  TRUE ~ FALSE
))

participant_ids <- c()
task <- c() # 0: gehen, 1: puzzlen
isFirstTask <- c() # 1: true, 0: false
mentalDemand <- c()
physicalDemand <- c()

for (i in 1:20) {
  mental_walking <- mental_data_walking[i,]
  mental_jigsaw <- mental_data_jigsaw[i,]

  participant_ids <- append(participant_ids, c(i, i))
  if (mental_walking$firstTask) {
    isFirstTask <- append(isFirstTask , c(1, 0))
  } else {
    isFirstTask <- append(isFirstTask , c(0, 1))
  }
  task <- append(task, c(0, 1))
  mentalDemand <- append(mentalDemand, c(mental_walking$`Mental Demand`, mental_jigsaw$`Mental Demand`))
  physicalDemand <- append(physicalDemand, c(mental_walking$`Physical Demand`, mental_jigsaw$`Physical Demand`))
}

mental_demand_data <- data.frame(
  participantId = participant_ids,
  mentalDemand = mentalDemand,
  physicalDemand = physicalDemand,
  task = task,
  isFirstTask = isFirstTask
)
