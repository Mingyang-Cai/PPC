#mean and 95% CI   covariate with quadratic effect 
rm(list=ls())
library(mice, warn.conflicts = FALSE)
library(miceadds)
library(MASS)
library(magrittr)
library(ggplot2)
library(dplyr)
library(smcfcs)


set.seed(123)
sample.size <- 1000
missingness <- 0.3
nominal <- 0.75
x <- rnorm(sample.size, mean = 0, sd = 1)
y <- x + x^2  + rnorm(sample.size, 0, 1)
complete.data <- data.frame(x = x, xsq = x^2, y = y)
#incomplete.data <- ampute(data = complete.data, prop = missingness, mech = "MCAR", patterns = c(0, 0, 1))$amp
incomplete.data <- ampute(data=complete.data, prop=missingness, mech = "MAR", type = 'RIGHT', patterns = c(0, 0, 1),
                          weights = c(0, 0, 1))$amp

rep.data <- obs.data <- incomplete.data[is.na(incomplete.data$x) == 0, ]
rep.data[, c("x", "xsq")]<- NA
concatenated.data <- rbind(obs.data, rep.data)
#pmm.pc
ini <- mice(incomplete.data, maxit = 0)
meth <- c("quadratic", "~I(x^2)", "")
pred <- ini$pred
pred[, "xsq"] <- 0
imp.1 <- mice(incomplete.data, meth = meth, pred = pred, quad.outcome = "y", print = FALSE)
est.1 <- with(imp.1, lm(y ~ x + xsq)) %>% pool()
xyplot(imp.1, x ~ y | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))

imp.ppc.1 <- mice(concatenated.data, m = 5, meth = meth, pred = pred, quad.outcome = "y", print = FALSE)
densityplot(imp.ppc.1)
xyplot(imp.ppc.1, x ~ y | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))

imp.ppc.11 <- mice(concatenated.data, m = 50, meth = meth, pred = pred, quad.outcome = "y", print = FALSE)
result.mean <- as.vector(t(apply(imp.ppc.11$imp$x, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.11$imp$x, 1, sd)))
result.1 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$x
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



#smc-fcs
imp.ppc.2 <- smcfcs(concatenated.data, smtype = "lm", smformula = "y~x+xsq", method = c( "norm", "x^2", ""), rjlimit= 10000)
smc.result <- data.frame( c(rep(0, 1450), rep(1, 1450), rep(2, 1450), rep(3, 1450), rep(4, 1450), rep(5, 1450)),  
                                 rep(c(1:1450), 6))
colnames(smc.result)<- c(".imp", ".id")
smc.res <- cbind(smc.result, rbind(concatenated.data, do.call(rbind, imp.ppc.2$impDatasets)))
smc.res <- as.mids(smc.res)
densityplot(smc.res)
xyplot(smc.res, x ~ y | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))


imp.ppc.22 <- smcfcs(concatenated.data, smtype = "lm", smformula = "y~x+xsq", method = c( "norm", "x^2", ""), m = 50, rjlimit= 10000)
result.222 <- lapply(imp.ppc.22$impDatasets, function(x) x[(nrow(obs.data)+1) : nrow(concatenated.data), "x"])
result.22 <- matrix(unlist(result.222), nrow = 50, ncol = nrow(rep.data), byrow = TRUE)
result.mean <- as.vector(t(apply(result.22, 2, mean)))
result.sd <- as.vector(t(apply(result.22, 2, sd)))
result.2 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$x
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

#pmm
imp.3 <- mice(incomplete.data, print = FALSE)
est.3 <- with(imp.3, lm(y ~ x + xsq)) %>% pool()
xyplot(imp.3, x ~ y | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))


imp.ppc.3 <- mice(concatenated.data, m = 5, print = FALSE)
densityplot(imp.ppc.3)
xyplot(imp.ppc.3, x ~ y | as.factor(.imp), pch = c(1, 19), col = mdc(1:2))

imp.ppc.33 <- mice(concatenated.data, m = 50, print = FALSE)
result.mean <- as.vector(t(apply(imp.ppc.33$imp$x, 1, mean)))
result.sd <- as.vector(t(apply(imp.ppc.33$imp$x, 1, sd)))
result.3 <- data.frame(
  Lower = result.mean + qnorm((1 - nominal) / 2) * result.sd,
  Mean = result.mean,
  Upper = result.mean - qnorm((1 - nominal) / 2) * result.sd,
  Obs = obs.data$x
)
result.3 <- result.3 %>%  
  mutate(Covered = Lower < Obs & Obs < Upper)
result.3 <- result.3[order(result.3$Mean), ]
limits <- aes(ymax = result.3$Upper, ymin = result.3$Lower)
ggplot(result.3, aes(y=Mean, x = 1: nrow(obs.data), colour = Covered)) + 
  geom_pointrange(limits)+
  geom_point(aes(y = Obs, x = 1: nrow(obs.data)), colour = "dark grey")+
  xlab("Simulation counts") +
  ylab("Means and Coverage") 
(sum(result.3$Covered) / nrow(obs.data)) %>% round(., 2)
(sum(abs(result.3$Mean - result.3$Obs)) / nrow(obs.data)) %>% round(., 2)
(sum(result.3$Upper - result.3$Lower) / nrow(obs.data)) %>% round(., 2)

