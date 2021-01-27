#binary outcome 2
rm(list=ls())
library(mice, warn.conflicts = FALSE)
library(miceadds)
library(MASS)
library(magrittr)
library(ggplot2)
library(dplyr)

set.seed(123)
sample.size <- 1000
missingness <- 0.5
nimp <- 50

x <- runif(sample.size, -3, 3)
z <- rnorm(sample.size, 1, 1)
pp <- exp(x + z) / (1 + exp(x + z))
y <- rbinom(sample.size, 1, pp)
complete.data <- data.frame(x = x, z = z, y = y)
#incomplete.data <-ampute(data=complete.data, prop = missingness, mech = "MCAR", patterns = c(1, 1, 0))$amp
incomplete.data <- ampute(data=complete.data, prop=missingness, mech = "MAR", type = 'RIGHT', patterns = c(1, 1, 0),
                          weights = c(1, 1, 0))$amp
rep.data <- obs.data <- incomplete.data[is.na(incomplete.data$y) == 0, ]
rep.data[, "y"]<- NA
concatenated.data <- rbind(obs.data, rep.data)
#with predictor x
imp.1 <- mice(incomplete.data, m =5, method = c("", "", "logreg"), print = FALSE)
est.1 <- with(imp.1, glm(y ~ x + z, family = binomial(link = logit))) %>% pool()

imp.ppc.1 <- mice(concatenated.data, method = c("", "", "logreg"), print = FALSE)
densityplot(imp.ppc.1)
#xyplot(imp.ppc.1, y ~ x | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))

pi.hat <- matrix(0, nrow(obs.data), nimp)
for (i in 1 : nimp) {
  imp.ppc.11 <- mice(concatenated.data, m = 1, method = c("", "", "logreg"), print = FALSE)
  pi.hat[, i] <- p
  }
s <- obs.data$y * 2 - 1
dev.resi <- s * sqrt(-2 * (obs.data$y * log(pi.hat[, 1]) + (1 - obs.data$y) * log(1 - pi.hat[, 1])))
dev.resi <- apply(pi.hat, 2, function(x) s * sqrt(-2 * (obs.data$y * log(x) + (1 - obs.data$y) * log(1 - x))))

summary.mean <- apply(dev.resi, 1, mean)
summay.max <- apply(dev.resi, 1, max)
summary.min <- apply(dev.resi, 1, min)

summary <- data.frame(mean = summary.mean, max = summay.max, min = summary.min)
summary <- summary[order(summary$mean), ]
limits <- aes(ymax = summary$max, ymin = summary$min)
ggplot(summary, aes(y=mean, x = 1: nrow(obs.data))) + 
  geom_pointrange(limits, colour = "dark grey")+
  xlab("Simulation") +
  ylab("deviance residuals") 
(sum(summary$mean^2) / nrow(obs.data))%>%round(., 2)


#without predictor x
pred <- imp.1$pred
pred["y", "x"] <- 0
imp.2 <- mice(incomplete.data, method = c("", "", "logreg"), pred = pred, print = FALSE)
est.2 <- with(imp.1, glm(y ~ x + z, family = binomial(link = logit))) %>% pool()

imp.ppc.2 <- mice(concatenated.data, method = c("", "", "logreg"), pred = pred, print = FALSE)
densityplot(imp.ppc.2)
#xyplot(imp.ppc.2, y ~ x | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))


pi.hat <- matrix(0, nrow(obs.data), nimp)
for (i in 1 : nimp) {
  imp.ppc.22 <- mice(concatenated.data, m = 1, method = c("", "", "logreg"), pred = pred, print = FALSE)
  pi.hat[, i] <- p
}
s <- obs.data$y * 2 - 1
dev.resi <- s * sqrt(-2 * (obs.data$y * log(pi.hat[, 1]) + (1 - obs.data$y) * log(1 - pi.hat[, 1])))
dev.resi <- apply(pi.hat, 2, function(x) s * sqrt(-2 * (obs.data$y * log(x) + (1 - obs.data$y) * log(1 - x))))

summary.mean <- apply(dev.resi, 1, mean)
summay.max <- apply(dev.resi, 1, max)
summary.min <- apply(dev.resi, 1, min)

summary <- data.frame(mean = summary.mean, max = summay.max, min = summary.min)
summary <- summary[order(summary$mean), ]
limits <- aes(ymax = summary$max, ymin = summary$min)
ggplot(summary, aes(y=mean, x = 1: nrow(obs.data))) + 
  geom_pointrange(limits, colour = "dark grey")+
  xlab("Simulation") +
  ylab("deviance residuals") 
(sum(summary$mean^2) / nrow(obs.data))%>%round(., 2)
