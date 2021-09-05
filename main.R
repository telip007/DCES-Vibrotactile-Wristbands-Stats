source("get_data.R")

# Hypothese 1
describeBy(participant_data$angleDelta, participant_data$condition)
wilcox.test(participant_data$angleDelta, participant_data$condition, 
            paired = TRUE, exact = FALSE, correct = FALSE, conf.int = TRUE, 
            alternative = "two.sided")

# Hypothese 2
describeBy(participant_data$trialDuration, participant_data$condition)
wilcox.test(participant_data$trialDuration, participant_data$condition, 
            paired = TRUE, exact = FALSE, correct = FALSE, conf.in = TRUE, 
            alternative =  "two.sided")

# Hypothese 3
describeBy(participant_data$trialDuration, participant_data$task)
wilcox.test(participant_data$trialDuration, participant_data$task,
            paired = TRUE, exact = FALSE, correct = FALSE, conf.in = TRUE,
            alternative = "two.sided")

# Hypothese 4
describeBy(participant_data$angleDelta, participant_data$task)
wilcox.test(participant_data$angleDelta, participant_data$task, 
            paired = TRUE, exact = FALSE, correct = FALSE, conf.int = TRUE, 
            alternative = "two.sided")

# Hypothese 5
angleDelta_reg <- lm(participant_data$angleDelta~participant_data$trialNumber)
plot(participant_data$trialNumber,participant_data$angleDelta,col="blue", ylim=c(30,40))
abline(angleDelta_reg, col="red")
summary(angleDelta_reg)

# Hypothese 6
reactionTime_reg <- lm(participant_data$trialDuration~participant_data$trialNumber)
plot(participant_data$trialNumber,participant_data$trialDuration,col="blue", ylim=c(200,4000))
abline(reactionTime_reg, col="red")
summary(reactionTime_reg)

# Hypothese 7

