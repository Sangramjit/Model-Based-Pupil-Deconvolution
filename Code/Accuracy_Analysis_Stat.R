################################################################################
# PACKAGES
################################################################################

library(lme4)
library(car)
library(emmeans)

options(contrasts = c("contr.sum", "contr.poly"))

################################################################################
# SETTINGS
################################################################################

DATA_PATH <- file.path(
  "E:",
  "Research",
  "Paper_Writing",
  "Face_AB_Modelling",
  "Data",
  "trialwise_accuracy.csv"
)

EXCLUDE_PARTICIPANTS <- c(
  "P36",
  "P6",
  "P24",
  "P25",
  "P35"
)

################################################################################
# LOAD DATA
################################################################################

df <- read.csv(
  DATA_PATH,
  stringsAsFactors = FALSE
)

df <- subset(
  df,
  !(participant %in% EXCLUDE_PARTICIPANTS)
)

df$correct <- as.numeric(df$correct)

cat(
  "\nParticipants:",
  length(unique(df$participant)),
  "\n"
)

################################################################################
# TWO TARGET DATA
################################################################################

df_two <- subset(
  df,
  T2 != ""
)

df_two$participant <- factor(
  df_two$participant
)

df_two$pair <- factor(
  paste0(df_two$T1, df_two$T2)
)

df_two$lag <- factor(
  df_two$lag
)

################################################################################
# GLMM
################################################################################

cat("\n=============================================\n")
cat("TWO TARGET GLMM\n")
cat("=============================================\n")

model_two <- glmer(
  correct ~ pair * lag +
    (1 + lag | participant),
  
  data = df_two,
  
  family = binomial,
  
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

################################################################################
# TYPE III TESTS
################################################################################

cat("\n=============================================\n")
cat("TYPE III WALD TESTS\n")
cat("=============================================\n")

anova_two <- Anova(
  model_two,
  type = 3
)

print(anova_two)

################################################################################
# EMOTION PAIR MAIN EFFECT
################################################################################

if(anova_two["pair","Pr(>Chisq)"] < .05){
  
  cat("\n=============================================\n")
  cat("POSTHOC: EMOTION PAIRS\n")
  cat("=============================================\n")
  
  pair_emm <- emmeans(
    model_two,
    ~ pair
  )
  
  pair_posthoc <- pairs(
    pair_emm,
    adjust = "tukey"
  )
  
  print(summary(pair_posthoc))
}

################################################################################
# LAG MAIN EFFECT
################################################################################

if(anova_two["lag","Pr(>Chisq)"] < .05){
  
  cat("\n=============================================\n")
  cat("POSTHOC: LAGS\n")
  cat("=============================================\n")
  
  lag_emm <- emmeans(
    model_two,
    ~ lag
  )
  
  lag_posthoc <- pairs(
    lag_emm,
    adjust = "tukey"
  )
  
  print(summary(lag_posthoc))
}

################################################################################
# INTERACTION
################################################################################

if(anova_two["pair:lag","Pr(>Chisq)"] < .05){
  
  cat("\n=============================================\n")
  cat("INTERACTION FOLLOW-UP\n")
  cat("LAG COMPARISONS WITHIN EACH PAIR\n")
  cat("=============================================\n")
  
  interaction_posthoc <- pairs(
    emmeans(
      model_two,
      ~ lag | pair
    ),
    adjust = "tukey"
  )
  
  print(
    summary(interaction_posthoc)
  )
}

################################################################################
# ONE TARGET
################################################################################

df_one <- subset(
  df,
  T2 == ""
)

df_one$participant <- factor(
  df_one$participant
)

df_one$T1 <- factor(
  df_one$T1
)

################################################################################
# GLMM
################################################################################

cat("\n=============================================\n")
cat("ONE TARGET GLMM\n")
cat("=============================================\n")

model_one <- glmer(
  correct ~ T1 +
    (1 | participant),
  
  data = df_one,
  
  family = binomial,
  
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

################################################################################
# TYPE III TEST
################################################################################

anova_one <- Anova(
  model_one,
  type = 3
)

print(anova_one)

################################################################################
# HAPPY VS ANGRY
################################################################################

cat("\n=============================================\n")
cat("HAPPY VS ANGRY\n")
cat("=============================================\n")

one_emm <- emmeans(
  model_one,
  ~ T1
)

one_posthoc <- pairs(
  one_emm,
  adjust = "tukey"
)

print(
  summary(one_posthoc)
)