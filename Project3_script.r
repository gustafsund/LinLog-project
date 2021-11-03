library(ggplot2)

countries <- read.table(file = "clipboard", 
                           sep = "\t", header = TRUE)

head(countries)
nrow(countries)

model0 <- lm(Life.expectancy ~ 1, countries)

model_test <- lm(Life.expectancy ~ gini_coef + pop_total
                 + babies_per_woman + gni_capita + gov_health_spending, 
                 countries)
confint(model_test)

ggplot(countries, aes(babies_per_woman, Life.expectancy)) + 
  geom_point()
#Babies per woman seems to show some linearity
ggplot(countries, aes(pop_total, Life.expectancy)) + 
  geom_point()
ggplot(countries, aes(log(pop_total), Life.expectancy)) + 
  geom_point()
#pop total does not seem to show any kind of relationship
ggplot(countries, aes(gini_coef, Life.expectancy)) + 
  geom_point()
ggplot(countries, aes(log(gini_coef), Life.expectancy)) + 
  geom_point()
#gini seems somewhat linear, log does not make it look better. 
#confint shows significant variable in linear correlation. 
ggplot(countries, aes(gov_health_spending, Life.expectancy)) + 
  geom_point()
ggplot(countries, aes(log(gov_health_spending), Life.expectancy)) + 
  geom_point()
#log of gov health spending might be better..? 

ggplot(countries, aes(gni_capita, Life.expectancy)) + 
  geom_point()
ggplot(countries, aes(log(gni_capita), Life.expectancy)) + 
  geom_point()
#log of gni seems to give way better fit. 

#let's see if gni and spending look correlated: 
ggplot(countries, aes(gni_capita, gov_health_spending)) + 
  geom_point()
#They seem very correlated.

model_transformed <- lm(Life.expectancy ~ babies_per_woman*gini_coef*log(gov_health_spending)*log(gni_capita)*log(pop_total), 
                        countries)
summary(model_transformed)

step(model0, 
     scope = list(upper = model_transformed), 
     direction = "forward")#gives modelAIC by using AIC as criterion
#model.for.aic <- lm(formula = Life.expectancy ~ babies_per_woman + log(gni_capita) + 
#                      gini_coef + log(pop_total) + babies_per_woman:gini_coef + 
 #                     babies_per_woman:log(gni_capita) + log(gni_capita):gini_coef + 
  #                    babies_per_woman:log(gni_capita):gini_coef, data = countries)

step(model0, 
     scope = list(upper = model_transformed), 
     direction = "forward", 
     k = log(nrow(countries)))#gives modelBIC by using BIC as criterion
model.for.bic <- lm(formula = Life.expectancy ~ babies_per_woman + log(gni_capita) + 
                      gini_coef + babies_per_woman:gini_coef + babies_per_woman:log(gni_capita) + 
                      log(gni_capita):gini_coef, data = countries)

step(model_transformed) #gives back model_transformed
step(model_transformed, k = log(nrow(countries)))#also gives back model_transformed

model_transformed2 <- lm(Life.expectancy ~ gini_coef*log(gov_health_spending)*log(gni_capita)*babies_per_woman, 
                         countries)
step(model0, 
     scope = list(upper = model_transformed2), 
     direction = "forward")
#model.for.aic2 <- lm(formula = Life.expectancy ~ babies_per_woman + log(gni_capita) + 
 #                      gini_coef + babies_per_woman:gini_coef + babies_per_woman:log(gni_capita) + 
  #                     log(gni_capita):gini_coef + babies_per_woman:log(gni_capita):gini_coef, 
   #                  data = countries)

step(model0, 
     scope = list(upper = model_transformed2), 
     direction = "forward", 
     k = log(nrow(countries)))
model.for.bic2 <- lm(formula = Life.expectancy ~ babies_per_woman + log(gni_capita) + 
                       gini_coef + babies_per_woman:gini_coef + babies_per_woman:log(gni_capita) + 
                       log(gni_capita):gini_coef, data = countries)

step(model_transformed2)
#model.back.aic2 <- lm(formula = Life.expectancy ~ gini_coef + log(gov_health_spending) + 
 #                       log(gni_capita) + babies_per_woman + gini_coef:log(gov_health_spending) + 
  #                      gini_coef:log(gni_capita) + log(gov_health_spending):log(gni_capita) + 
   #                     gini_coef:babies_per_woman + log(gov_health_spending):babies_per_woman + 
    #                    log(gni_capita):babies_per_woman + gini_coef:log(gov_health_spending):log(gni_capita) + 
     #                   gini_coef:log(gov_health_spending):babies_per_woman, data = countries)
step(model_transformed2, k = log(nrow(countries)))
model.back.bic2 <- lm(formula = Life.expectancy ~ gini_coef + log(gov_health_spending) + 
                        log(gni_capita) + babies_per_woman + gini_coef:log(gov_health_spending) + 
                        gini_coef:log(gni_capita) + log(gov_health_spending):log(gni_capita) + 
                        gini_coef:babies_per_woman + log(gov_health_spending):babies_per_woman + 
                        gini_coef:log(gov_health_spending):log(gni_capita) + gini_coef:log(gov_health_spending):babies_per_woman, 
                      data = countries)
model.simple.inter <- lm(Life.expectancy ~ babies_per_woman*log(gni_capita), countries)
#F-stat = 682.4
model.simple <- lm(Life.expectancy ~ babies_per_woman + log(gni_capita), countries)
#F-stat = 1012

anova(model.simple, model.simple.inter)
#ANOVA shows model.simple significantly better. 

#playing around a bit: 
model.simple2 <- lm(Life.expectancy ~babies_per_woman + 
                      log(gni_capita)+ gini_coef, 
                    data = countries)#F-stat = 697

#denna kör vi på. Bakgrunden till den är att vi tog bort insignifikanta
#variabler från model.for.bic2. 
model.for.bic.red <- lm(formula = Life.expectancy ~log(gni_capita) + 
                          gini_coef + babies_per_woman:gini_coef + babies_per_woman:log(gni_capita) + 
                          log(gni_capita):gini_coef, data = countries)


ggplot(countries, aes(gini_coef, log(babies_per_woman))) + 
  geom_point()

collect_AIC <- data.frame(
  nr = seq(1, 7),
  model = c("model.null", "model.simple", "model.full", "model.forward.bic", 
            "model.full.nopop", "model.backward.bic.nopop","model.forward.bic.red"), 
  AIC = AIC(model0, model.simple, model_transformed, model.for.bic,
            model_transformed2, model.back.bic2, model.for.bic.red),
  BIC = BIC(model0, model.simple, model_transformed, model.for.bic, 
      model_transformed2, model.back.bic2, model.for.bic.red), 
  R2 = c(summary(model0)$r.squared, summary(model.simple)$r.squared, 
         summary(model_transformed)$r.squared, summary(model.for.bic)$r.squared, 
         summary(model_transformed2)$r.squared, summary(model.back.bic2)$r.squared, 
         summary(model.for.bic.red)$r.squared), 
  R2adj = c(summary(model0)$adj.r.squared, summary(model.simple)$adj.r.squared, 
            summary(model_transformed)$adj.r.squared, summary(model.for.bic)$adj.r.squared, 
            summary(model_transformed2)$adj.r.squared, summary(model.back.bic2)$adj.r.squared, 
            summary(model.for.bic.red)$adj.r.squared)
)

ggplot(data = collect_AIC, aes(nr, R2)) + 
  geom_point(aes(y = R2), color = "green", size = 7) + 
  geom_point(aes(y = R2adj), color = "red", size = 3) + 
  geom_line(aes(x = nr, y = R2), size = 1)+ 
  geom_line(aes(x = nr, y = R2adj), color = "red", 
            size = 1, linetype = "dashed") + 
  labs(caption = "R2 (black and green) and R2adj (red and dashed)") + 
  ylab(label = "R2 and R2adj")

ggplot(data = collect_AIC, aes(nr, AIC.AIC)) + 
  geom_point(aes(y = AIC.AIC), color = "green", size = 5) +
  geom_point(aes(y = BIC.BIC), color = "red", size = 2) + 
  geom_line(aes(x = nr, y = AIC.AIC), size = 1)+ 
  geom_line(aes(x = nr, y = BIC.BIC), color = "red", 
            size = 1, linetype = "dashed") + 
  labs(caption = "AIC (black and green) and BIC (red and dashed)") + 
  ylab(label = "AIC and BIC")


countries.pred <- cbind(countries, 
                        pred = predict(model.for.bic.red, interval = "prediction"), 
                        conf = predict(model.for.bic.red, interval = "confidence"), 
                        res = residuals(model.for.bic.red), 
                        rstud = rstudent(model.for.bic.red), 
                        v = influence(model.for.bic.red)$hat, 
                        D = cooks.distance(model.for.bic.red)
)
countries.pred$fit <- countries.pred$conf.fit
countries.pred$pred.fit <- countries.pred$conf.fit <- NULL
head(countries.pred)


ggplot(countries.pred, aes(sample = res)) + 
  geom_qq(data = countries.pred, aes(sample = res)) + 
  geom_qq_line() 

ggplot(countries.pred, aes(fit, rstud)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = c(-4,-2,2,4), color = "red")

I_extremeres <- which(countries.pred$rstud <= -4)
I_highleverage <- which(countries.pred$v > 0.05)


ggplot(countries.pred, aes(log(gni_capita), v)) + 
  geom_point() + 
  geom_point(data = countries.pred[I_extremeres, ], 
             color = "red", shape = 24) + 
  geom_hline(yintercept = c(1/nrow(countries), 14/nrow(countries)), 
             color = "red") + 
  geom_point(data = countries.pred[I_highleverage, ], 
             color = "red", shape = 25)


ggplot(countries.pred, aes(gini_coef, v)) + 
  geom_point() + 
  geom_point(data = countries.pred[I_extremeres, ], 
             color = "red", shape = 24) + 
  geom_hline(yintercept = c(1/nrow(countries), 14/nrow(countries)), 
             color = "red") + 
  geom_point(data = countries.pred[I_highleverage, ], 
             color = "red", shape = 25)

ggplot(countries.pred, aes(log(gni_capita), D)) + 
  geom_point() + 
  geom_hline(yintercept = 4/nrow(countries), color = "red") +
  geom_point(data = countries.pred[I_extremeres, ], 
             color = "red", shape = 24) + 
  geom_point(data = countries.pred[I_highleverage, ], 
             color = "red", shape = 25)
#The observations with extreme residuals were also very high cooks D, 
#thus we remove all of the extreme residuals. Other obs had high 
#D, but were not otherwise identified as being problematic. 
countries.new <- countries[-I_extremeres, ]

model.new <- lm(formula = Life.expectancy ~log(gni_capita) + 
                  gini_coef + babies_per_woman:gini_coef + babies_per_woman:log(gni_capita) + 
                  log(gni_capita):gini_coef, data = countries.new)


countries.pred.new <- cbind(countries.new, 
                        pred = predict(model.new, interval = "prediction"), 
                        conf = predict(model.new, interval = "confidence"), 
                        res = residuals(model.new), 
                        rstud = rstudent(model.new), 
                        v = influence(model.new)$hat, 
                        D = cooks.distance(model.new)
)

head(countries.pred.new)

ggplot(data = countries.pred.new, aes(conf.fit, rstud)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = c(-4,-2,2,4), color = "red")

ggplot(data = countries.pred.new, aes(sample = res)) +
  geom_qq(aes(sample = res)) + 
  geom_qq_line()

collect_AIC2 <- data.frame(
  nr = seq(1, 8),
  model = c("model.null", "model.simple", "model.full", "model.forward.bic", 
            "model.full.nopop", "model.backward.bic.nopop","model.forward.bic.red", "model.new"), 
  AIC = AIC(model0, model.simple, model_transformed, model.for.bic,
            model_transformed2, model.back.bic2, model.for.bic.red, model.new),
  BIC = BIC(model0, model.simple, model_transformed, model.for.bic, 
            model_transformed2, model.back.bic2, model.for.bic.red, model.new), 
  R2 = c(summary(model0)$r.squared, summary(model.simple)$r.squared, 
         summary(model_transformed)$r.squared, summary(model.for.bic)$r.squared, 
         summary(model_transformed2)$r.squared, summary(model.back.bic2)$r.squared, 
         summary(model.for.bic.red)$r.squared, summary(model.new)$r.squared), 
  R2adj = c(summary(model0)$adj.r.squared, summary(model.simple)$adj.r.squared, 
            summary(model_transformed)$adj.r.squared, summary(model.for.bic)$adj.r.squared, 
            summary(model_transformed2)$adj.r.squared, summary(model.back.bic2)$adj.r.squared, 
            summary(model.for.bic.red)$adj.r.squared, summary(model.new)$adj.r.squared)
)
collect_AIC2
#Även om vi använt oss av annan data för model.new användes AIC, BIC, R2 och R2adj för 
#att vägleda i valet av model. model.new verkar bättre än den tidigare föredragna
#model.for.bic.red, men detta är riktvärden och inte statistiskt signifikanta tester. 

#sweden 2010
predict(model.new, newdata = data.frame(babies_per_woman = 1.99, 
                                        gni_capita = 54600, 
                                        gini_coef = 27.7), 
        interval = "prediction")
#niger 2010
predict(model.new, newdata = data.frame(babies_per_woman = 7.49, 
                                        gni_capita = 350, 
                                        gini_coef = 34), 
        interval = "prediction")

#sweden one extra baby
predict(model.new, newdata = data.frame(babies_per_woman = 2.99, 
                                        gni_capita = 54600, 
                                        gini_coef = 27.7), 
        interval = "prediction")

#niger one extra baby
predict(model.new, newdata = data.frame(babies_per_woman = 8.49, 
                                        gni_capita = 350, 
                                        gini_coef = 34), 
        interval = "prediction")

#sweden 100 extra gni
predict(model.new, newdata = data.frame(babies_per_woman = 1.99, 
                                        gni_capita = 54700, 
                                        gini_coef = 27.7), 
        interval = "prediction")
#niger 100 extra gni
predict(model.new, newdata = data.frame(babies_per_woman = 7.49, 
                                        gni_capita = 450, 
                                        gini_coef = 34), 
        interval = "prediction")

#sweden 1 higher gini coef
predict(model.new, newdata = data.frame(babies_per_woman = 1.99, 
                                        gni_capita = 54600, 
                                        gini_coef = 28.7), 
        interval = "prediction")

#niger 1 higher gini coef
predict(model.new, newdata = data.frame(babies_per_woman = 7.49, 
                                        gni_capita = 350, 
                                        gini_coef = 35), 
        interval = "prediction")
mean(countries$Life.expectancy)
min(countries.pred.new$conf.fit)
