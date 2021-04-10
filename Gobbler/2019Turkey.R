setwd(choose.dir('Gobbler'))
SG2019<-read.csv('2019SGRaw.csv')
head(SG2019)

#Model for just Gobblers
fit<-glm(Gcalled ~ Temp + Region,
         family = poisson,
         data = SG2019)
summary(fit)

summary(fit)[['coefficients']]['Region', 'z value']

Rnd <- data.frame(
  Temp = seq(min(SG2019$Temp), max(SG2019$Temp)),
  Region = 1)
prd <- predict.glm(object = fit, newdata = Rnd, type = 'link', se.fit = T)

low <- exp(prd$fit - qnorm(0.975) * prd$se.fit)
high <- exp(prd$fit + qnorm(0.975) * prd$se.fit)

plot(y = exp(prd$fit), x = Rnd$Temp, xlab = 'Temperature', ylab = 'Expected Count',
     cex.axis = 1.5, cex.lab = 1.5, 
     ylim = c(min(low), max(high)), type = 'l')
lines(x=Rnd$Temp, y = high, lty = 2)
lines(x=Rnd$Temp, y = low, lty = 2)

#With Hours offset
houroff <- log(SG2019$Hours)
fitoff <- glm(Gcalled ~ Temp + Region,
                   family = poisson,
                   data = SG2019,
              offset = houroff)
summary(fitoff)

summary(fitoff)[['coefficients']]['Region', 'z value']

Ond <- data.frame(Temp = seq(min(SG2019$Temp),
                             max(SG2019$Temp)),
                  Region = 1,
                  houroff = rep(log(1)))
oprd <- predict.glm(object = fitoff, newdata = Ond, type = 'link', se.fit = T)

olow <- exp(oprd$fit - qnorm(0.975) * oprd$se.fit)
ohigh <- exp(oprd$fit + qnorm(0.975) * oprd$se.fit)

plot(y = exp(oprd$fit), x = Ond$Temp, xlab = 'Temperature', ylab = 'Expected Count', 
     cex.axis = 1.5, cex.lab = 1.5, ylim = c(min(olow), max(ohigh)), type = 'l')
lines(x=Ond$Temp, y = ohigh, lty = 2)
lines(x=Ond$Temp, y = olow, lty = 2)

#Model for Total Turkeys
fit3<-glm(TotCalled ~ Temp + Region,
         family = poisson,
         data = SG2019)
summary(fit3)

summary(fit3)[['coefficients']]['Region', 'z value']

Tnd <- data.frame(
  Temp = seq(min(SG2019$Temp), max(SG2019$Temp)),
  Region = 1)
trd <- predict.glm(object = fit3, newdata = Tnd, type = 'link', se.fit = T)

tlow <- exp(trd$fit - qnorm(0.975) * trd$se.fit)
thigh <- exp(trd$fit + qnorm(0.975) * trd$se.fit)

plot(y = exp(trd$fit), x = Tnd$Temp, xlab = 'Temperature', ylab = 'Expected Count',
     cex.axis = 1.5, cex.lab = 1.5, 
     ylim = c(min(tlow), max(thigh)), type = 'l')
lines(x=Tnd$Temp, y = thigh, lty = 2)
lines(x=Tnd$Temp, y = tlow, lty = 2)

#MOdel for total turkeys with offset
thouroff <- log(SG2019$Hours)
SG2019$Temp <- factor(SG2019$Temp, levels = c('1', '2', '3', '4'))
SG2019$Region <- factor(SG2019$Region, levels = c('1', '2', '3', '4', '5', '6'))
tfitoff <- glm(TotCalled ~ Temp + Region,
              family = poisson,
              data = SG2019,
              offset = thouroff)
summary(tfitoff)

summary(tfitoff)[['coefficients']]['Region2', 'z value']

TOnd <- data.frame(
  Temp = factor(c('1'), levels = c('1', '2', '3', '4')), #seq(min(SG2019$Temp), max(SG2019$Temp)),
  Region = factor(SG2019$Region),
  thouroff = rep(log(1)))
toprd <- predict.glm(object = tfitoff, newdata = TOnd, type = 'link', se.fit = T)
summary(toprd)

tolow <- exp(toprd$fit - qnorm(0.975) * toprd$se.fit)
tohigh <- exp(toprd$fit + qnorm(0.975) * toprd$se.fit)
View(tolow)
  
plot(y = exp(toprd$fit), x = TOnd$Region, xlab = 'Region', ylab = 'Expected Count', 
       cex.axis = 1.5, cex.lab = 1.5, ylim = c(min(tolow), max(tohigh)),
       type = 'l')
lines(aes(ymax = tohigh, ymin = tolow), width = .1)

lines(x=TOnd$Region, y = tohigh, lty = 2)
lines(x=TOnd$Region, y = tolow, lty = 2)

#Occupancy Model
library(unmarked)
y <- read.csv('wt2019.csv')
wt_mat <- as.matrix(y)

det_covs <- list(temperature = data.frame(Temp = factor(SG2019$Temp)),
                 prec = data.frame(Precip = factor(SG2019$Precip)))
site_covs <- factor(SG2019$Region, levels = c('1', '2', '3', '4', '5', '6'))

occu_data <- unmarkedFrameOccu(y = wt_mat,
                               siteCovs = SG2019$Region,
                               obsCov = det_covs)
occufit <- occu(~ temperature + prec ~ 1, data = occu_data)
summary(occufit)
coef(occufit)





k <- 5
k_time <- 10

gam_formula <- gam(TotCalled ~ Temp + Region,
                   family = poisson,
                   data = SG2019,
                   offset = thouroff)
summary(gam_formula)

time_knots <- list(time = seq(0,24, length.out = 12))
m_nb <- gam(list(gam_formula),
                data = SG2019$TotCalled,
                family = "nb",
                knots = time_knots)
  
fit2 <- glm(Gcalled ~ Hours + Region,
            family = poisson,
            data = SG2019)
summary(fit2)

Hnd <- data.frame(
  Hours = seq(min(SG2019$Hours), max(SG2019$Hours)),
  Region = 1)
prd2 <- predict.glm(object = fit2, newdata = Hnd, type = 'link', se.fit = T)
summary(prd2)

low2 <- exp(prd2$fit - qnorm(0.975) * prd2$se.fit)
high2 <- exp(prd2$fit + qnorm(0.975) * prd2$se.fit)

plot(y = exp(prd2$fit), x = Hnd$Hours, xlab = 'Hours', ylab = 'Expected Count',
     cex.axis = 1.5, cex.lab = 1.5, 
     ylim = c(min(low2), max(high2)), type = 'l')
lines(x=)

library(unmarked)
g_mat <- as.matrix(Gcalled)
g_data <- unmarkedFramePCount(y = g_mat)
fit3 <- pcount(~ 1 ~ 1, data = g_data, K = 100)
summary(fit3)
