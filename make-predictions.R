
# Load packages

library(boot)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)
library(gridExtra)
library(pander)
library(tidyverse)

# Load and prepare data

turbulence = read.csv("data-train.csv")

turbulence$Sv = turbulence$St / turbulence$Fr

turbulence$Re <- as.factor(turbulence$Re)
levels(turbulence$Re) <- c("Low", "Med", "High")

turbulence$Fr <- as.factor(turbulence$Fr)
levels(turbulence$Fr) <- c("Low", "Inf", "Med") 

turbulence$Sv_2 = turbulence$Sv^2
turbulence$St_2 = turbulence$St^2

test = read.csv("data-test.csv")
test$Sv = test$St / test$Fr

test$Re <- as.factor(test$Re)
levels(test$Re) <- c("Low", "Med", "High")

test$Fr <- as.factor(test$Fr)
levels(test$Fr) <- c("Low", "Med", "Inf") 

test$St_2 = test$St^2
test$Sv_2 = test$Sv^2

# Moment 1

moment.1.mod <- lm(R_moment_1 ~ Re + St + St * Re + Sv + Sv * Re + Sv_2 + Sv_2 * Re, data=turbulence)
moment.1.mod.glm <- glm(R_moment_1 ~ Re + St + St*Re + Sv + Sv*Re + Sv_2 + Sv_2*Re, data=turbulence)
pander(moment.1.mod, caption="Moment 1")

m1.preds.raw = data.frame(predict(moment.1.mod, newdata=test, interval="prediction", level=0.95))
m1.preds = m1.preds.raw %>% mutate(lwr = case_when(lwr <= 0 ~ 0,
                                                   TRUE ~ lwr))
full_preds_df = cbind(test, m1.preds)

full_preds_df = full_preds_df %>% dplyr::select(c("Re", "St", "Sv", "fit", "lwr", "upr"))

colnames(full_preds_df) = c("Re", "St", "Sv", "pred", "95% low pred", "95% high pred")

preds_of_interest = full_preds_df[c(3, 10, 14, 15),]
rownames(preds_of_interest) = NULL


# Moment 2

og.mod.2 <- lm(log(R_moment_2) ~ Re*Fr + Re*St + poly(St, 2), data = turbulence) # This is a rank deficient fit so the estimates of the coefficients are unreliable
moment.2.mod.glm <- glm(log(R_moment_2) ~ Re + Sv + St + St_2 + Sv*Re, data = turbulence)
moment.2.mod <- lm(log(R_moment_2) ~ Re + Sv + St + St_2 + Sv*Re, data = turbulence)
pander(moment.2.mod, caption="Moment 2")

m2.preds.raw = data.frame(predict(moment.2.mod, newdata=test, interval="prediction", level=0.95))
m2_full_preds_df = cbind(test, m2.preds.raw)
m2_full_preds_df = m2_full_preds_df %>% dplyr::select(c("Re", "St", "Sv", "Sv_2", "fit", "lwr", "upr"))
colnames(m2_full_preds_df) = c("Re", "Sv", "St","St_2", "pred", "95% low pred", "95% high pred")
m2_preds_of_interest = m2_full_preds_df[c(3, 10, 14, 15),]
rownames(m2_preds_of_interest) = NULL


# Moment 3

og.moment.3.mod <- glm(log(R_moment_3) ~ Fr + St + Re + Fr*St + Fr*Re + St*Re, data=turbulence) # also rank deficient fit
moment.3.mod.glm <- glm(log(R_moment_3) ~ Sv_2 + Sv + Re, data=turbulence)
moment.3.mod <- lm(log(R_moment_3) ~ Sv_2 + Sv + Re, data=turbulence)
pander(moment.3.mod, caption="Moment 3")

m3.preds.raw = data.frame(predict(moment.3.mod, newdata=test, interval="prediction", level=0.95))
m3_full_preds_df = cbind(test, m3.preds.raw)
m3_full_preds_df = m3_full_preds_df %>% dplyr::select(c("Re", "Sv", "Sv_2", "fit", "lwr", "upr"))
colnames(m3_full_preds_df) = c("Re", "Sv", "Sv_2", "pred", "95% low pred", "95% high pred")
m3_preds_of_interest = m3_full_preds_df[c(3, 10, 14, 15),]
rownames(m3_preds_of_interest) = NULL


# Moment 4

moment.4.mod <- lm(log(R_moment_4) ~ Re + Sv + St + Re*Sv + Re*St + Sv*St, data=turbulence)
moment.4.mod.glm <- glm(log(R_moment_4) ~ Re + Sv + St + Re*Sv + Re*St + Sv*St, data=turbulence)
pander(moment.4.mod, caption="Moment 4")

m4.preds.raw = data.frame(predict(moment.4.mod, newdata=test, interval="prediction", level=0.95))
m4_full_preds_df = cbind(test, m4.preds.raw)
m4_full_preds_df = m4_full_preds_df %>% dplyr::select(c("Re", "St", "Sv", "fit", "lwr", "upr"))
colnames(m4_full_preds_df) = c("Re", "St", "Sv", "pred", "95% low pred", "95% high pred")
m4_preds_of_interest = m4_full_preds_df[c(6, 21, 4, 14),]
rownames(m4_preds_of_interest) = NULL
pander(m4_preds_of_interest, caption="Selected Predictions for Moment 4")


# Make predictions file

colnames(m1.preds) = c("moment1_pred", "moment1_95%_low", "moment1_95%_high")
colnames(m2.preds.raw) = c("moment2_pred", "moment2_95%_low", "moment2_95%_high")
colnames(m3.preds.raw) = c("moment3_pred", "moment3_95%_low", "moment3_95%_high")
colnames(m4.preds.raw) = c("moment4_pred", "moment4_95%_low", "moment4_95%_high")

predictions_df = cbind(m1.preds, m2.preds.raw, m3.preds.raw, m4.preds.raw)

final_predictions_all = predictions_df %>% select(c("moment1_pred", "moment2_pred", "moment3_pred", "moment4_pred"))

preds_with_predictors = cbind(test, final_predictions_all)

write.csv(preds_with_predictors, "test_predictions.csv", row.names=F)

