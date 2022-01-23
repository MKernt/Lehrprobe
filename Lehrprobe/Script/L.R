a <- rnorm(10)
b <- rnorm(10)
c <- lm(a~b)
plot(c)

x <- c(1,2,3)
y <- c(2,4,6)
z <- lm(a1 ~ b1)

plot(x, y, xlim=c(0,4), ylim=c(0,6), pch = 19, col = "blue", main="Einfache lineare Regression")
abline(lm(y ~ x), col = "red")

x1 <- runif(8, 2, 8)
y1 <- runif(8, 2, 8)

plot(x1, y1, xlim=c(0,10), ylim=c(0,10), pch = 19, col = "blue", main="Einfache lineare Regression", xlab = "x", ylab = "y")

abline(lm(y1 ~ x1), col = "red")


lm.res=lm(y1~x1)
lm.res
yhat=3.3238-0.5536*x1
resid=y1-yhat
resid
cbind(x1,y1,yhat,resid)
for (i in 1:8) {
  lines(c(x1[i],x1[i]),c(y1[i],yhat[i]),lwd=2,col="green")
}


hist(y1)





#---- Logit
y2 <- c(1,1,1,1,0,1,0,0,1,0)
x2 <- runif(10, 2, 8)
plot(x2,y2)

model <- glm(y2 ~ x2, family=binomial)

newdata <- data.frame(x2=seq(min(x2), max(y2)))

#use fitted model to predict values of vs
newdata$y2 = predict(model, newdata, type="response")

#plot logistic regression curve
plot(y2 ~ x2, col="steelblue")
lines(y2 ~ x2, newdata, lwd=1)



fit = glm(vs ~ hp, data=mtcars, family=binomial)
newdat <- data.frame(hp=seq(min(mtcars$hp), max(mtcars$hp),len=100))
newdat$vs = predict(fit, newdata=newdat, type="response")
plot(vs~hp, data=mtcars, col="blue", main="Logistische Regression", xlab = "x", ylab = "y")
lines(vs ~ hp, newdat, col="red", lwd=2)


fit = glm(y2 ~ x2, family=binomial)
newdat <- data.frame(x2=seq(min(x2), max(y2),len=10))
newdat$y2 = predict(fit, newdata=newdat, type="response")
plot(y2~x2, col="red4", main="Logistische Regression", xlab = "x", ylab = "y")
lines(y2 ~ x2, newdat, col="green4", lwd=2)

plot(y1 ~ x1)
lm.y1x1 <- lm(y1 ~ x1)
plot(lm.y1x1)



# World Data ------------------------------------------
world_data <- read.csv("~/Documents/Latex/Lehrprobe/Data/world_data.csv", stringsAsFactors=FALSE)

world_data$Frauenrechte_2012 = 
  as.factor(world_data$Frauenrechte_2012)

head(world_data)

head(world_data)
str(world_data)
summary(world_data)

## H1 ----- 
model_H1 = lm(Geburtenrate_2012 ~ Bildung_2012, 
              data=world_data)
summary(model_H1)
install.packages("stargazer")
library(stargazer)
stargazer(model_H1)

plot(y = world_data$Geburtenrate_2012, 
     x = world_data$Bildung_2012, 
     pch = 19, 
     col = "blue", 
     main="Plot H1", 
     xlab = "Bildung in Jahren", 
     ylab = "Geburtenrate")

abline(lm(world_data$Geburtenrate_2012 ~ world_data$Bildung_2012), col = "red")

## H2 ----- 
model_H2 = lm(Geburtenrate_2012 ~ 
                Frauenrechte_2012, 
              data=world_data)
summary(model_H2)

plot(y = world_data$Geburtenrate_2012, 
     x = world_data$Frauenrechte_2012, 
     pch = 19, 
     col = "blue", 
     main="Plot H2", 
     xlab = "Level Frauenrechte", 
     ylab = "Geburtenrate")

abline(lm(world_data$Geburtenrate_2012 ~ 
            world_data$Frauenrechte_2012), 
       col = "red")
## Nice Plotting ----
install.packages("tidyverse")
library(tidyverse)

ggplot(world_data, aes(x=Bildung_2012,y=Geburtenrate_2012)) + geom_point() +
  geom_smooth(method="lm") + theme_classic() +
  ylab("Anzahl Kinder / number of children") +
  xlab("obligatorische Schulzeit / mandatory school")
## Multi -------
model_multi <- lm(Geburtenrate_2012 ~ Bildung_2012 + Frauenrechte_2012, data = world_data)
summary(model_multi)

install.packages("relaimpo")
library(relaimpo)
calc.relimp(model3, type="lmg",rela=T,rank=T)