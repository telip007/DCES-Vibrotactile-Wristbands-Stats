library(tidyverse)

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
