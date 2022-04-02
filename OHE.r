#OHE framework, used for several datasets
library(dplyr)
df <- test

df$BKG_SALES_DATE_MONTH <- as.factor(df$BKG_SALES_DATE_MONTH)
df$BKG_SALES_DATE_WEEKDAY <- as.factor(df$BKG_SALES_DATE_WEEKDAY)
df$BKG_STAY_LENGTH_GROUPED <- as.factor(df$BKG_STAY_LENGTH_GROUPED)
df$BKG_SALES_CHANNEL_grouped <- as.factor(df$BKG_SALES_CHANNEL_grouped)
df$TKT_CURRENCY_fin <- as.factor(df$TKT_CURRENCY_fin)
df$TKT_PAX_TYPE <- as.factor(df$TKT_PAX_TYPE)
df$TKT_Child_flg <- as.factor(df$TKT_Child_flg)
df$TKT_CORPORATE_CONTRACT_FLG <- as.factor(df$TKT_CORPORATE_CONTRACT_FLG)
df$TKT_LOYAL_CUSTOMER <- as.factor(df$TKT_LOYAL_CUSTOMER)
df$EMD_TOTAL_PRICE_PLN <- as.numeric(df$EMD_TOTAL_PRICE_PLN)
df$EMD_BAGGAGE <- as.factor(df$EMD_BAGGAGE)
df$EMD_SPECIAL_EQUIPMENT <- as.factor(df$EMD_SPECIAL_EQUIPMENT)
df$EMD_SEAT_SELECTION <- as.factor(df$EMD_SEAT_SELECTION)
df$EMD_PET <- as.factor(df$EMD_PET)
df$EMD_SERVICE <- as.factor(df$EMD_SERVICE)
df$EMD_MEAL <- as.factor(df$EMD_MEAL)
df$EMD_FEE <- as.factor(df$EMD_FEE)
df$EMD_OTHER <- as.factor(df$EMD_OTHER)
df$FCP_FLIGHT_DATE_QUARTER <- as.factor(df$FCP_FLIGHT_DATE_LOCAL_QUARTER)
df$FCP_FLIGHT_DATE_WEEKDAY <- as.factor(df$FCP_FLIGHT_DATE_LOCAL_WEEKDAY)
df$FCP_FLIGHT_TIME <- as.factor(df$FCP_FLIGHT_TIME)
df$FCP_FLIGHT_RANGE <- as.factor(df$FCP_FLIGHT_RANGE)
df$BKG_SALES_DATE_MONTH <- as.factor(df$BKG_SALES_DATE_MONTH)
df$BKG_TRIP_TYPE <- as.factor(df$BKG_TRIP_TYPE)
df$FCP_BOOKED_CABIN <- as.factor(df$FCP_BOOKED_CABIN)
df$FCP_VAB <- as.factor(df$FCP_VAB)
df$FCP_UPGRADED_FLAG <- as.factor(df$FCP_UPGRADED_FLAG)


dummy <- dummyVars(" ~ .", data=df)
test_ohe <- data.frame(predict(dummy, newdata = df))
df_ohe <- na.omit(df_ohe)
saveRDS(test_ohe1, 'test_ohe.Rds')


