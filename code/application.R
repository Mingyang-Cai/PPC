#BMI data
rm(list=ls())
library(mice, warn.conflicts = FALSE)
library(miceadds)
library(MASS)
library(magrittr)
library(ggplot2)
library(dplyr)

nominal <- 0.95
data <- selfreport[, c("age", "sex", "hm", "hr", "wm", "wr")]
md.pattern(data, plot = FALSE)
rep.data <- obs.data <- data[is.na(data$hm) == 0, ]
rep.data[, c("hm", "wm")]<- NA
concatenated.data <- rbind(obs.data, rep.data)
#initial model selection
fit.hm <- lm(hm~., data = obs.data)
stepAIC(fit.hm, direction = "both", data = obs.data) #leave hr out
step(fit.hm, scope = . ~ .^2, direction = "both")
fit.wm <- lm(wm~., data = obs.data)
stepAIC(fit.wm, direction = "both", data = obs.data) #leave wr out
step(fit.wm, scope = . ~ .^2, direction = "both")
ini.imp <- mice(concatenated.data, print = FALSE)
meth <- c("", "", "norm", "", "norm", "")
#hr in wr in
imp.ppc.1 <- mice(concatenated.data, method = meth, print = FALSE)
##density plot
densityplot(imp.ppc.1)
##scatter plot
xyplot(imp.ppc.1, hm ~ hr | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))
xyplot(imp.ppc.1, wm ~ wr | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))

imp.ppc.11 <- mice(concatenated.data, m = 50, method = meth, print = FALSE)
##plot of mean and CI for hm
result.mean <- as.vector(t(apply(imp.ppc.11$imp$hm, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.11$imp$hm, 1, sd)))
result.1 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$hm
)
result.1 <- result.1 %>%  
  mutate(Covered = Lower < Obs & Obs < Upper)
result.1 <- result.1[order(result.1$Mean), ]
limits <- aes(ymax = result.1$Upper, ymin = result.1$Lower)
ggplot(result.1, aes(y=Mean, x = 1: nrow(obs.data), colour = Covered)) + 
  geom_pointrange(limits)+
  geom_point(aes(y = Obs, x = 1: nrow(obs.data)), colour = "dark grey")+
  xlab("Missing cases counts") +
  ylab("Means and Coverage") 
(sum(result.1$Covered) / nrow(obs.data)) %>% round(., 2)
(sum(abs(result.1$Mean - result.1$Obs)) / nrow(obs.data)) %>% round(., 2)
(sum(result.1$Upper - result.1$Lower) / nrow(obs.data)) %>% round(., 2)
##plot of mean and CI for wm
result.mean <- as.vector(t(apply(imp.ppc.11$imp$wm, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.11$imp$wm, 1, sd)))
result.1 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$wm
)
result.1 <- result.1 %>%  
  mutate(Covered = Lower < Obs & Obs < Upper)
result.1 <- result.1[order(result.1$Mean), ]
limits <- aes(ymax = result.1$Upper, ymin = result.1$Lower)
ggplot(result.1, aes(y=Mean, x = 1: nrow(obs.data), colour = Covered)) + 
  geom_pointrange(limits)+
  geom_point(aes(y = Obs, x = 1: nrow(obs.data)), colour = "dark grey")+
  xlab("Missing cases counts") +
  ylab("Means and Coverage") 
(sum(result.1$Covered) / nrow(obs.data)) %>% round(., 2)
(sum(abs(result.1$Mean - result.1$Obs)) / nrow(obs.data)) %>% round(., 2)
(sum(result.1$Upper - result.1$Lower) / nrow(obs.data)) %>% round(., 2)
#hr in wr out
pred.2 <- ini.imp$pred
pred.2["wm", "wr"] <- 0
imp.ppc.2 <- mice(concatenated.data, method = meth, pred = pred.2, print = FALSE)
##density plot
densityplot(imp.ppc.2)
##scatter plot
xyplot(imp.ppc.2, hm ~ hr | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))
xyplot(imp.ppc.2, wm ~ wr | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))

imp.ppc.22 <- mice(concatenated.data, m = 50, method = meth, pred = pred.2, print = FALSE)
##plot of mean and CI for hm
result.mean <- as.vector(t(apply(imp.ppc.22$imp$hm, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.22$imp$hm, 1, sd)))
result.2 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$hm
)
result.2 <- result.2 %>%  
  mutate(Covered = Lower < Obs & Obs < Upper)
result.2 <- result.2[order(result.2$Mean), ]
limits <- aes(ymax = result.2$Upper, ymin = result.2$Lower)
ggplot(result.2, aes(y=Mean, x = 1: nrow(obs.data), colour = Covered)) + 
  geom_pointrange(limits)+
  geom_point(aes(y = Obs, x = 1: nrow(obs.data)), colour = "dark grey")+
  xlab("Missing cases counts") +
  ylab("Means and Coverage") 
(sum(result.2$Covered) / nrow(obs.data)) %>% round(., 2)
(sum(abs(result.2$Mean - result.2$Obs)) / nrow(obs.data)) %>% round(., 2)
(sum(result.2$Upper - result.2$Lower) / nrow(obs.data)) %>% round(., 2)
##plot of mean and CI for wm
result.mean <- as.vector(t(apply(imp.ppc.22$imp$wm, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.22$imp$wm, 1, sd)))
result.2 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$wm
)
result.2 <- result.2 %>%  
  mutate(Covered = Lower < Obs & Obs < Upper)
result.2 <- result.2[order(result.2$Mean), ]
limits <- aes(ymax = result.2$Upper, ymin = result.2$Lower)
ggplot(result.2, aes(y=Mean, x = 1: nrow(obs.data), colour = Covered)) + 
  geom_pointrange(limits)+
  geom_point(aes(y = Obs, x = 1: nrow(obs.data)), colour = "dark grey")+
  xlab("Missing cases counts") +
  ylab("Means and Coverage") 
(sum(result.2$Covered) / nrow(obs.data)) %>% round(., 2)
(sum(abs(result.2$Mean - result.2$Obs)) / nrow(obs.data)) %>% round(., 2)
(sum(result.2$Upper - result.2$Lower) / nrow(obs.data)) %>% round(., 2)
#hr out wr in
pred.3 <- ini.imp$pred
pred.3["hm", "hr"] <- 0
imp.ppc.3 <- mice(concatenated.data, method = meth, pred = pred.3, print = FALSE)
##density plot
densityplot(imp.ppc.3)
##scatter plot
xyplot(imp.ppc.3, hm ~ hr | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))
xyplot(imp.ppc.3, wm ~ wr | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))

imp.ppc.33 <- mice(concatenated.data, m = 50, method = meth, pred = pred.3, print = FALSE)
##plot of mean and CI for hm
result.mean <- as.vector(t(apply(imp.ppc.33$imp$hm, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.33$imp$hm, 1, sd)))
result.3 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$hm
)
result.3 <- result.3 %>%  
  mutate(Covered = Lower < Obs & Obs < Upper)
result.3 <- result.3[order(result.3$Mean), ]
limits <- aes(ymax = result.3$Upper, ymin = result.3$Lower)
ggplot(result.3, aes(y=Mean, x = 1: nrow(obs.data), colour = Covered)) + 
  geom_pointrange(limits)+
  geom_point(aes(y = Obs, x = 1: nrow(obs.data)), colour = "dark grey")+
  xlab("Missing cases counts") +
  ylab("Means and Coverage") 
(sum(result.3$Covered) / nrow(obs.data)) %>% round(., 2)
(sum(abs(result.3$Mean - result.3$Obs)) / nrow(obs.data)) %>% round(., 2)
(sum(result.3$Upper - result.3$Lower) / nrow(obs.data)) %>% round(., 2)
##plot of mean and CI for wm
result.mean <- as.vector(t(apply(imp.ppc.33$imp$wm, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.33$imp$wm, 1, sd)))
result.3 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$wm
)
result.3 <- result.3 %>%  
  mutate(Covered = Lower < Obs & Obs < Upper)
result.3 <- result.3[order(result.3$Mean), ]
limits <- aes(ymax = result.3$Upper, ymin = result.3$Lower)
ggplot(result.3, aes(y=Mean, x = 1: nrow(obs.data), colour = Covered)) + 
  geom_pointrange(limits)+
  geom_point(aes(y = Obs, x = 1: nrow(obs.data)), colour = "dark grey")+
  xlab("Missing cases counts") +
  ylab("Means and Coverage") 
(sum(result.3$Covered) / nrow(obs.data)) %>% round(., 2)
(sum(abs(result.3$Mean - result.3$Obs)) / nrow(obs.data)) %>% round(., 2)
(sum(result.3$Upper - result.3$Lower) / nrow(obs.data)) %>% round(., 2)

#hr out wr out
pred.4 <- ini.imp$pred
pred.4["wm", "wr"] <- 0
pred.4["hm", "hr"] <- 0
imp.ppc.4 <- mice(concatenated.data, method = meth, pred = pred.4, print = FALSE)
##density plot
densityplot(imp.ppc.4)
##scatter plot
xyplot(imp.ppc.4, hm ~ hr | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))
xyplot(imp.ppc.4, wm ~ wr | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))

imp.ppc.44 <- mice(concatenated.data, m = 50, method = meth, pred = pred.4, print = FALSE)
##plot of mean and CI for hm
result.mean <- as.vector(t(apply(imp.ppc.44$imp$hm, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.44$imp$hm, 1, sd)))
result.4 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$hm
)
result.4 <- result.4 %>%  
  mutate(Covered = Lower < Obs & Obs < Upper)
result.4 <- result.4[order(result.4$Mean), ]
limits <- aes(ymax = result.4$Upper, ymin = result.4$Lower)
ggplot(result.4, aes(y=Mean, x = 1: nrow(obs.data), colour = Covered)) + 
  geom_pointrange(limits)+
  geom_point(aes(y = Obs, x = 1: nrow(obs.data)), colour = "dark grey")+
  xlab("Missing cases counts") +
  ylab("Means and Coverage") 
(sum(result.4$Covered) / nrow(obs.data)) %>% round(., 2)
(sum(abs(result.4$Mean - result.4$Obs)) / nrow(obs.data)) %>% round(., 2)
(sum(result.4$Upper - result.4$Lower) / nrow(obs.data)) %>% round(., 2)
##plot of mean and CI for wm
result.mean <- as.vector(t(apply(imp.ppc.44$imp$wm, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.44$imp$wm, 1, sd)))
result.4 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$wm
)
result.4 <- result.4 %>%  
  mutate(Covered = Lower < Obs & Obs < Upper)
result.4 <- result.4[order(result.4$Mean), ]
limits <- aes(ymax = result.4$Upper, ymin = result.4$Lower)
ggplot(result.4, aes(y=Mean, x = 1: nrow(obs.data), colour = Covered)) + 
  geom_pointrange(limits)+
  geom_point(aes(y = Obs, x = 1: nrow(obs.data)), colour = "dark grey")+
  xlab("Missing cases counts") +
  ylab("Means and Coverage") 
(sum(result.4$Covered) / nrow(obs.data)) %>% round(., 2)
(sum(abs(result.4$Mean - result.4$Obs)) / nrow(obs.data)) %>% round(., 2)
(sum(result.4$Upper - result.4$Lower) / nrow(obs.data)) %>% round(., 2)












