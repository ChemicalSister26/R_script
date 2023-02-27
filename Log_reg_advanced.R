# Intercept only model##############################################################

tit <- read.csv('titanic.csv')
no_pred <- glm(Survived ~ 1, tit, family='binomial')
coef(no_pred)
# (Intercept) 
# -0.4732877 
summary(no_pred)
table(tit$Survived)
# 0   1 
# 549 342
odds <-  342/549
# 0.6229508
log(odds)
# -0.4732877 == Intercept

exp(log(odds))
# 0.6229508
exp(coef(no_pred))
# 0.6229508 
# this is a clear ODDS odds to survive without additional
# predictors - for brand new passenger

# модель возвращает в таблице коэффициентов ЛОГАРИФМ от шансов,
# можно сразу же возвести экспоненту в степень этого коэфф и получить шансы


# logistic regression with one nominative predictor###############################

one_nom <- glm(Survived ~ Sex, tit, family=binomial)
coefficients((one_nom))

# (Intercept)     Sexmale 
# 1.056589        -2.513710
summary(one_nom)
table(tit$Survived, tit$Sex)
#    female male
# 0     81  468
# 1    233  109

odds_male <- 109/468 # 0.232906

odds_female <- 233/81  # 2.876543

log(odds_female) # 1.056589
log(odds_male) # -1.45712

odds_ratio <- odds_male/odds_female
log(odds_ratio)

anova(no_pred, one_nom, test="Chisq")
#       Resid.  Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1       890     1186.7                          
# 2       889      917.8  1   268.85 < 2.2e-16 ***

# residuals in second model much lower - therefore predictor is important

# logistic regression with two nominative predictors ############################
tit$Pclass <- as.factor(tit$Pclass)
two_nom <-  glm(Survived ~ Sex*Pclass, tit, family=binomial)
summary(two_nom)

tit_table <- table(tit$Survived, tit$Pclass, tit$Sex)

# where from the intercept?
female_class1_odds <- 91/3
log(female_class1_odds)
# 3.412247 - Intercept in summary

# where from the sexmale?
male_class1_odds <- 45/77
log(male_class1_odds/female_class1_odds)
# -3.94939 SexMale it is a logarithm of odds for male in 1 class divided to 
# odds for female if 1 class

mosaicplot(~ Sex + Pclass + Survived, data = tit, color = TRUE)


# Pclass2
odds_female_class2 <- 70/6 # 11.66667
female_class1_odds # 30.33333

log(odds_female_class2/female_class1_odds)
# -0.9555

# Pclass3
odds_female_class3 <- 72/72
log(odds_female_class3/female_class1_odds)
# -3.4122

# Sexmale:Pclass2
male_class2_odds <- 17/91
log(male_class2_odds/odds_female_class2) - 
  log(male_class1_odds/female_class1_odds) # -0.1849918

# Sexmale:Pclass3
male_class3_odds <- 47/300
log(male_class3_odds/odds_female_class3) -
  log(male_class1_odds/female_class1_odds)
# 2.095755


# comparing two models by quality #################################
anova(one_nom, two_nom, test="Chisq")
anova(two_nom, test="Chisq")
# residuals much lower with two variables - it is better to use two predictors

# model wil different types of predictors - factors and continuous #################
############################################################################

dif_types <- glm(Survived ~ Sex + Age + Pclass, data=tit, family=binomial)
summary(dif_types)
anova(dif_types, test='Chisq')
str(tit)
