library(caret)
library(car)
library(olsrr)
options(scipen=999)

df <- readRDS("data_regresja.rds")

# One hot encoding
dummy <- dummyVars(" ~ .", data=df)
newdata <- data.frame(predict(dummy, newdata = df)) 

newdata$BKG_SALES_DATE_MONTH <- as.factor(newdata$BKG_SALES_DATE_MONTH)
newdata$BKG_SALES_DATE_WEEKDAY<- as.factor(newdata$BKG_SALES_DATE_WEEKDAY)
newdata$FCP_FLIGHT_DATE_QUARTER <- as.factor(newdata$FCP_FLIGHT_DATE_QUARTER)
newdata$FCP_FLIGHT_DATE_WEEKDAY <- as.factor(newdata$FCP_FLIGHT_DATE_WEEKDAY)

# Simple Linear Regression
model_reg <- lm(flg_min_upg ~., data = newdata)
summary(model_reg)

# Check multicollinearity
alias(model_reg)
ld.vars <- attributes(alias(model_reg)$Complete)$dimnames[[1]]

# Remove ambigious variables
newdata_no_vif <- subset(newdata, select=-c(BKG_STAY_LENGTH_GROUPEDShort, 
                    TKT_CURRENCY_finUSD, 
                    TKT_PAX_TYPEINFANT, 
                    FCP_FLIGHT_TIME22.04, 
                    FCP_FLIGHT_RANGESHORT.HAUL, 
                    BKG_TRIP_TYPEONE.WAY, 
                    BKG_TRIP_TYPEROUND.TRIP, 
                    FCP_BOOKED_CABINPremium, 
                    FCP_VABSTANDARD
))

# Run new model and check vif
fit.new <-lm(flg_min_upg ~., data = newdata_no_vif)

df_vif <- vif(fit.new) > 10
df_vif

# Remove vif's > 10

newdata <- newdata_no_vif[, -c(17,18)]

# Prepare new regression for automated backward elimination

fit.new <-lm(flg_min_upg ~., data = newdata)

ols_step_backward_p(fit.new,
                    # p-value for removing
                    # (default p = 0.3)
                    prem = 0.05,
                    # show progress
                    progress = TRUE) -> model_linear_backward

# Checking the final model details

summary(model_linear_backward$model)

# Save final dataset for linear regression

newdata_final_for_lr <- subset(newdata, select=c(BKG_BOOKING_WINDOW_D, 
                                                  BKG_STAY_LENGTH_GROUPEDLong, 
                                                  BKG_STAY_LENGTH_GROUPEDMedium, 
                                                  BKG_STAY_LENGTH_GROUPEDOne.Way, 
                                                  BKG_SALES_CHANNEL_grouped, 
                                                  TKT_TOTAL_PRICE_PLN, 
                                                  EMD_BAGGAGE, 
                                                  FCP_FLIGHT_DATE_QUARTER, 
                                                  FCP_FLIGHT_DATE_WEEKDAY,
                                                  FCP_FLIGHT_DISTANCE,
                                                  FCP_FLIGHT_RANGEDOMESTIC,
                                                  FCP_BOOKED_CABINEconomy,
                                                  FCP_VABBASIC,
                                                  flg_min_upg
))

write.csv2(newdata_final_for_lr, "linear_final_dataset.csv")




                                                    
  
