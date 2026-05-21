library(lavaan)
library(blavaan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(corrplot)
library(semTools)

Data01 <- read.csv("C:/Users/vucin/OneDrive/Desktop/Thesis/D2 - SEM/Data01.csv")

# Individual CFAs

# Inconsistency
sub <- Data01[, c("rebounds_per100Std", "astTovStd", "plusMinusPoints_per100Std")]
model <- 'inc =~ rebounds_per100Std + astTovStd + plusMinusPoints_per100Std'
fit <- cfa(model, data=sub)
summary(fit, standardized=TRUE)

# Leadership
sub <- Data01[, c("win_pct", "plusMinusPoints_per100", "AST.")]
model <- 'lead =~ win_pct + plusMinusPoints_per100 + AST.'
fit <- cfa(model, data=sub)
summary(fit, standardized=TRUE)

# Decision Making
sub <- Data01[, c("fieldGoalsMade_per100", "turnovers_per100", "freeThrowsMade_per100")]
model <- 'dec =~ fieldGoalsMade_per100 + turnovers_per100 + freeThrowsMade_per100'
fit <- cfa(model, data=sub)
summary(fit, standardized=TRUE)

# Adaptability
sub <- Data01[, c("height", "eFG", "stocks_per100")]
model <- 'ada =~ height + eFG + stocks_per100'
fit <- cfa(model, data=sub)
summary(fit, standardized=TRUE)

# Work Engagement
sub <- Data01[, c("steals_per100", "rebounds_per100", "MPG")]
model <- 'we =~ steals_per100 + rebounds_per100 + MPG'
fit <- cfa(model, data=sub)
summary(fit, standardized=TRUE)

# CFA1
sub <- Data01[, c("rebounds_per100Std", "astTovStd", "plusMinusPoints_per100Std", 
                  "win_pct", "plusMinusPoints_per100", "AST.", "fieldGoalsMade_per100", 
                  "turnovers_per100", "freeThrowsMade_per100", "height", "eFG", 
                  "stocks_per100", "steals_per100", "rebounds_per100", "MPG")]
# Reverse code rebounds and turnovers for reliability measures
sub$turnovers_per100 <- -sub$turnovers_per100
sub$rebounds_per100 <- -sub$rebounds_per100
model_cfa1 <- 'inc =~ rebounds_per100Std + astTovStd + plusMinusPoints_per100Std
          lead =~ win_pct + plusMinusPoints_per100 + AST.
          dec =~ fieldGoalsMade_per100 + turnovers_per100 + freeThrowsMade_per100
          ada =~ height + eFG + stocks_per100
          we =~ steals_per100 + rebounds_per100 + MPG'
cfa1 <- bsem(model_cfa1, data=sub)
summary(cfa1, standardized=TRUE)
standardizedSolution(cfa1) %>% filter(op == "=~") %>% group_by(lhs) %>% summarise (AVE = sum(est.std^2) / n())
round(apply({m <- inspect(cfa1, "cor.lv")^2; diag(m) <- NA; m}, 1, max, na.rm = TRUE), 3)
# CR
load <- standardizedSolution(cfa1)[standardizedSolution(cfa1)$op == "=~", ]
load %>% group_by(lhs) %>% summarise(lambda = list(est.std), CR = {
  lam <- est.std
  theta <- 1 - lam^2
  (sum(lam)^2) / ((sum(lam)^2) + sum(theta))})

# CFA2
model_cfa2 <- 'inc =~ rebounds_per100Std + astTovStd + plusMinusPoints_per100Std
          dec =~ fieldGoalsMade_per100 + turnovers_per100 + freeThrowsMade_per100
          ada =~ height + eFG + stocks_per100
          we =~ steals_per100 + rebounds_per100 + MPG'
cfa2 <- bsem(model_cfa2, data=sub)
summary(cfa2, standardized=TRUE)
standardizedSolution(cfa2) %>% filter(op == "=~") %>% group_by(lhs) %>% summarise (AVE = sum(est.std^2) / n())
round(apply({m <- inspect(cfa2, "cor.lv")^2; diag(m) <- NA; m}, 1, max, na.rm = TRUE), 3)
# CR
load <- standardizedSolution(cfa2)[standardizedSolution(cfa2)$op == "=~", ]
load %>% group_by(lhs) %>% summarise(lambda = list(est.std), CR = {
  lam <- est.std
  theta <- 1 - lam^2
  (sum(lam)^2) / ((sum(lam)^2) + sum(theta))})

# CFA3
model_cfa3 <- 'inc =~ rebounds_per100Std + astTovStd + plusMinusPoints_per100Std
          dec =~ fieldGoalsMade_per100 + turnovers_per100 + freeThrowsMade_per100'
cfa3 <- bsem(model_cfa3, data=sub)
summary(cfa3, standardized=TRUE)
standardizedSolution(cfa3) %>% filter(op == "=~") %>% group_by(lhs) %>% summarise (AVE = sum(est.std^2) / n())
round(apply({m <- inspect(cfa3, "cor.lv")^2; diag(m) <- NA; m}, 1, max, na.rm = TRUE), 3)
# CR
load <- standardizedSolution(cfa3)[standardizedSolution(cfa3)$op == "=~", ]
load %>% group_by(lhs) %>% summarise(lambda = list(est.std), CR = {
  lam <- est.std
  theta <- 1 - lam^2
  (sum(lam)^2) / ((sum(lam)^2) + sum(theta))})

# Model (Non-Bayes)
sub <- Data01[, c("playerteamName", "rebounds_per100Std", "astTovStd", "plusMinusPoints_per100Std", "turnovers_per100", "freeThrowsMade_per100", "fieldGoalsMade_per100", "salary_adj")]
model <- 'inc =~ rebounds_per100Std + astTovStd + plusMinusPoints_per100Std
          dec =~ fieldGoalsMade_per100 + turnovers_per100 + freeThrowsMade_per100 + plusMinusPoints_per100Std
          astTovStd ~~ turnovers_per100
          salary_adj ~ inc + dec'
fit <- cfa(model, data=sub)
summary(fit, standardized=TRUE)
fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))

# Bayesian + Multilevel approach
sub <- Data01[, c("playerteamName", "rebounds_per100Std", "astTovStd", "plusMinusPoints_per100Std", "turnovers_per100", "freeThrowsMade_per100", "fieldGoalsMade_per100", "salary_adj")]
model_sem <- 'inc =~ rebounds_per100Std + astTovStd + plusMinusPoints_per100Std
          dec =~ fieldGoalsMade_per100 + turnovers_per100 + freeThrowsMade_per100 + plusMinusPoints_per100Std
          astTovStd ~~ turnovers_per100
          salary_adj ~ inc + dec'

model_sem_ml <- 'level: 1
             inc =~ rebounds_per100Std + astTovStd + plusMinusPoints_per100Std
             dec =~ fieldGoalsMade_per100 + turnovers_per100 + freeThrowsMade_per100 + plusMinusPoints_per100Std
             astTovStd ~~ turnovers_per100
             salary_adj ~ inc + dec

             level: 2
             inc =~ rebounds_per100Std + astTovStd + plusMinusPoints_per100Std
             dec =~ fieldGoalsMade_per100 + turnovers_per100 + freeThrowsMade_per100 + plusMinusPoints_per100Std
             salary_adj ~ inc + dec'
sem1 <- bsem(model_sem_ml, data=sub, cluster="playerteamName")
summary(sem1, standardized=TRUE)

# Multilevel exploration
pe <- parameterEstimates(sem1, standardized = TRUE)
within  <- subset(pe, level==1)
between <- subset(pe, level==2)

# Final Bayesian model
sem2 <- bsem(model_sem, data=sub)
summary(sem2, standardized=TRUE)
