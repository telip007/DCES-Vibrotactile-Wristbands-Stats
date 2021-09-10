source("get_data.R")
library(psych)
library(car)

# Normalverteilungtest (angleDelta & trialDuration)
shapiro.test(participant_data$angleDelta)
shapiro.test(participant_data$trialDuration)
shapiro.test(mental_demand_data$mentalDemand)
shapiro.test(mental_demand_data$physicalDemand)

# Hypothese 1
# Signifikant
# 0 < 1 (ego besser)
describeBy(participant_data$angleDelta, participant_data$condition)
wilcox.test(participant_data$angleDelta, participant_data$condition, 
            paired = TRUE, exact = FALSE, correct = FALSE, conf.int = TRUE, 
            alternative = "two.sided")

# Hypothese 2
# Signifikant
# 0 < 1 (ego besser)
describeBy(participant_data$trialDuration, participant_data$condition)
wilcox.test(participant_data$trialDuration, participant_data$condition, 
            paired = TRUE, exact = FALSE, correct = FALSE, conf.in = TRUE, 
            alternative =  "two.sided")

# Hypothese 3
# Signifikant
# 0 < 1 (gehen besser)
describeBy(participant_data$trialDuration, participant_data$task)
wilcox.test(participant_data$trialDuration, participant_data$task,
            paired = TRUE, exact = FALSE, correct = FALSE, conf.in = TRUE,
            alternative = "two.sided")

# Hypothese 4
# Signifikant
# 1 < 0 (puzzlen besser)
describeBy(participant_data$angleDelta, participant_data$task)
wilcox.test(participant_data$angleDelta, participant_data$task, 
            paired = TRUE, exact = FALSE, correct = FALSE, conf.int = TRUE, 
            alternative = "two.sided")

# Hypothese 5 - AngleDelta ~ Task + Condition
# Ego im gehen gut, und allo beim spielen
# groups: 0: gehen + ego, 1: gehen + allo, 2: spiel + ego, 3: spiel + allo
# Anova: Nein, da keine normalverteilung und varianzen nicht homogen
# Kruskal: Signifikant
# Post-hoc (wilcox): 0:1, 1:2, 1:3
describeBy(participant_data$angleDelta, participant_data$group)
leveneTest(participant_data$angleDelta, participant_data$group)
shapiro.test(participant_data$angleDelta)
kruskal.test(formula(participant_data$angleDelta ~ participant_data$group), data=participant_data)
pairwise.wilcox.test(participant_data$angleDelta, participant_data$group, p.adjust="bonferroni")

# Hypothese 6 - TrialDuration ~ Task + Condition
# Ego im gehen gut, und allo beim spielen
# groups: 0: gehen + ego, 1: gehen + allo, 2: spiel + ego, 3: spiel + allo
# Anova: nein, da keine normalverteilung, varianzen aber homogon
# Kruskal: Signifikant
# Post-hoc (wilcox): 0:2, 0:3, 1:2, 1:3, 2:3
describeBy(participant_data$trialDuration, participant_data$group)
leveneTest(participant_data$trialDuration, participant_data$group)
shapiro.test(participant_data$trialDuration)
kruskal.test(formula(participant_data$trialDuration ~ participant_data$group), data=participant_data)
pairwise.wilcox.test(participant_data$trialDuration, participant_data$group, p.adjust="bonferroni")

egocentricParticipants <- participant_data %>% filter(condition == 0)
allocentricParticipants <- participant_data %>% filter(condition == 1)

# Hypothese 7
# Nicht signifikant
angleDelta_reg <- lm(egocentricParticipants$angleDelta~egocentricParticipants$trialNumber)
angleDelta_regAllo <- lm(allocentricParticipants$angleDelta~allocentricParticipants$trialNumber)
plot(participant_data$trialNumber,participant_data$angleDelta, col="transparent", ylim = c(28,40))
abline(angleDelta_reg, col="red")
abline(angleDelta_regAllo, col="blue")
summary(angleDelta_reg)
summary(angleDelta_regAllo)

# Hypothese 8
# Signifikant
reactionTimeRegEgo <- lm(egocentricParticipants$trialDuration~egocentricParticipants$trialNumber)
reactionTimeRegAllo <- lm(allocentricParticipants$trialDuration~allocentricParticipants$trialNumber)
plot(participant_data$trialNumber,participant_data$trialDuration,col="transparent", ylim=c(2000,2500))
abline(reactionTimeRegEgo, col="red")
abline(reactionTimeRegAllo, col="blue")
summary(reactionTimeRegEgo)
summary(reactionTimeRegAllo)

# Hypothese 9
# 2. task sogar noch mehr demanding
describeBy(mental_demand_data$mentalDemand, mental_demand_data$isFirstTask)
wilcox.test(mental_demand_data$mentalDemand, mental_demand_data$isFirstTask, 
            paired = TRUE, exact = FALSE, correct = FALSE, conf.int = TRUE, 
            alternative = "two.sided")

# Hypothese 10
# Gehen weniger mental demanding als puzzlen
describeBy(mental_demand_data$mentalDemand, mental_demand_data$task)
wilcox.test(mental_demand_data$mentalDemand, mental_demand_data$task, 
            paired = TRUE, exact = FALSE, correct = FALSE, conf.int = TRUE, 
            alternative = "two.sided")

# Hypothese 11
# Gehen mehr physical demand
describeBy(mental_demand_data$physicalDemand, mental_demand_data$task)
wilcox.test(mental_demand_data$physicalDemand, mental_demand_data$task, 
            paired = TRUE, exact = FALSE, correct = FALSE, conf.int = TRUE, 
            alternative = "two.sided")
