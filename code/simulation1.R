#mean and 95% CI lm with quadratic effect
rm(list=ls())
library(mice, warn.conflicts = FALSE)
library(miceadds)
library(MASS)
library(magrittr)
library(ggplot2)
library(dplyr)


set.seed(123)
missingness <- 0.3
nominal <- 0.75
x <- runif(1000, -3, 3)
y <- x + x^2 + rnorm(1000, 0, 1)
complete.data <- data.frame(x = x, xsq = x^2, y = y)
#incomplete.data <- ampute(data = complete.data, prop = missingness, mech = "MCAR", patterns = c(1, 1, 0))$amp
incomplete.data <- ampute(data=complete.data, prop = missingness, mech = "MAR", type = "RIGHT", patterns = c(1, 1, 0), weights = c(1, 0, 0))$amp
imp.1 <- mice(incomplete.data, method = c("", "", "norm"), print = FALSE)
est.1 <- with(imp.1, lm(y ~ x + xsq)) %>% pool()
pred <- imp.1$pred
pred["y", "xsq"] <- 0
imp.2 <- mice(incomplete.data, method = c("", "", "norm"), pred = pred, print = FALSE)
est.2 <- with(imp.2, lm(y ~ x + xsq)) %>% pool()
rep.data <- obs.data <- incomplete.data[is.na(incomplete.data$y) == 0, ]
rep.data[, "y"]<- NA
concatenated.data <- rbind(obs.data, rep.data)
#with predictor xsq
imp.ppc.1 <- mice(concatenated.data, method = c("", "", "norm"), print = FALSE)
densityplot(imp.ppc.1)
xyplot(imp.ppc.1, y ~ x | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))

imp.ppc.11 <- mice(concatenated.data, m = 50, method = c("", "", "norm"), print = FALSE)
result.mean <- as.vector(t(apply(imp.ppc.11$imp$y, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.11$imp$y, 1, sd)))
result.1 <- data.frame(
                       Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
                       Mean = result.mean,
                       Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
                       Obs = obs.data$y
)
result.1 <- result.1 %>%  
  mutate(Covered = Lower < Obs & Obs < Upper)
result.1 <- result.1[order(result.1$Mean), ]
limits <- aes(ymax = result.1$Upper, ymin = result.1$Lower)
ggplot(result.1, aes(y=Mean, x = 1: nrow(obs.data), colour = Covered)) + 
  geom_pointrange(limits)+
  geom_point(aes(y = Obs, x = 1: nrow(obs.data)), colour = "dark grey")+
  xlab("Simulation counts") +
  ylab("Means and Coverage") 
(sum(result.1$Covered) / nrow(obs.data)) %>% round(., 2)
(sum(abs(result.1$Mean - result.1$Obs)) / nrow(obs.data)) %>% round(., 2)
(sum(result.1$Upper - result.1$Lower) / nrow(obs.data)) %>% round(., 2)


#without predictor xsq
imp.ppc.2 <- mice(concatenated.data, method = c("", "", "norm"), pred = pred, print = FALSE)
densityplot(imp.ppc.2)
xyplot(imp.ppc.2, y ~ x | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))

imp.ppc.22 <- mice(concatenated.data, m = 50, method = c("", "", "norm"), pred = pred, print = FALSE)
result.mean <- as.vector(t(apply(imp.ppc.22$imp$y, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.22$imp$y, 1, sd)))
result.2 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$y
)
result.2 <- result.2 %>%  
  mutate(Covered = Lower < Obs & Obs < Upper)
result.2 <- result.2[order(result.2$Mean), ]
limits <- aes(ymax = result.2$Upper, ymin = result.2$Lower)
ggplot(result.2, aes(y=Mean, x = 1: nrow(obs.data), colour = Covered)) + 
  geom_pointrange(limits)+
  geom_point(aes(y = Obs, x = 1: nrow(obs.data)), colour = "dark grey")+
  xlab("Simulation counts") +
  ylab("Means and Coverage") 
(sum(result.2$Covered) / nrow(obs.data)) %>% round(., 2)
(sum(abs(result.2$Mean - result.2$Obs)) / nrow(obs.data)) %>% round(., 2)
(sum(result.2$Upper - result.2$Lower) / nrow(obs.data)) %>% round(., 2)
