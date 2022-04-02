options(scipen = 999)
head(bkg)

library(DescTools)
library(lubridate)
library(tidyverse)
library(sqldf)
library(ggcorrplot)
library(psych)

bkg <- read.csv2('BKG_test.csv')
tkt <- read.csv2('TKT_test.csv')
fcp <- read.csv2('FCP_test.csv')
emd <- read.csv2('EMD_test.csv')


unique(bkg$UPGRADE_TYPE)
table(bkg$UPGRADE_TYPE,bkg$UPGRADED_FLAG)

Desc(bkg1[,0:9], plotit = F)

summary(bkg[,13:ncol(bkg)])

Desc(tkt, plotit = F)

head(as.numeric(tkt$TOTAL_PRICE_PLN))

ecdf(as.numeric(tkt$TOTAL_PRICE_PLN))

Desc(fcp, plotit = F)

Desc(emd, plotit =  F)

###########
### BKG ###
###########
    
bkg1 <- bkg

# variable SALES_DATE

# change format into date
bkg1$SALES_DATE1 <- ymd(bkg1$SALES_DATE)
bkg1$SALES_DATE_WEEKDAY <- ifelse(as.POSIXlt(bkg1$SALES_DATE1)$wday == 0, 7,as.POSIXlt(bkg1$SALES_DATE1)$wday) 
bkg1$SALES_DATE_MONTH <- month(ymd(bkg1$SALES_DATE1))

# variable sales_channel 
bkg1$SALES_CHANNEL_grouped <- ifelse(bkg1$SALES_CHANNEL %in% c('LOT.COM', 'LOT TRAVEL', 'CALL CENTER'), 1 , 0)
bkg1$SALES_CHANNEL_LOT_OR_NOT <- bkg1$SALES_CHANNEL_grouped

# winsorize outliers
bkg1$BOOKING_WINDOW_D <- ifelse(bkg1$BOOKING_WINDOW_D > 138, 138, bkg1$BOOKING_WINDOW_D)

# stay_length 
bkg1 <- bkg1 %>%
  mutate(STAY_LENGTH_GROUPED = case_when(STAY_LENGTH_D == -9999 ~ 'One-Way',
                                         STAY_LENGTH_D > 0 & STAY_LENGTH_D <= 5 ~ 'Short', 
                                         STAY_LENGTH_D > 5 & STAY_LENGTH_D <= 14 ~ 'Medium',
                                         STAY_LENGTH_D > 14 ~ 'Long'))
# FLIGHT_COUPONS
plot(density(bkg1$FLIGHT_COUPONS))
dim(bkg1)[1]-dim(bkg2)[1]

# SEGMENTS 
table(bkg1$SEGMENTS)

# BKG DONE 
bkg_done <- bkg1 %>% 
  select(BOOKING_ID, SALES_DATE1 ,SALES_DATE_MONTH, SALES_DATE_WEEKDAY,
         SALES_CHANNEL_grouped,TRIP_TYPE, BOOKING_WINDOW_D, STAY_LENGTH_GROUPED,
         FLIGHT_COUPONS)

###########
### EMD ###
########### 

emd1 <- emd %>% 
  select(SALES_DATE, REFERENCE_TICKET_NUMBER, SALES_CHANNEL, ORIGIN_AIRPORT_CODE, CATEGORY,TOTAL_PRICE_PLN )

###########
### TKT ###
########### 

currs <- c("PLN", "EUR", "USD", "GBP", "SEK")
tkt$CURRENCY_fin <- ifelse(tkt$CURRENCY %in% currs, tkt$CURRENCY, "Other")

tkt <- tkt %>%
  group_by(BOOKING_ID) %>%
  mutate(Child_flg = if_else(max(PAX_TYPE) == "ADULT", 0, 1))

tkt_done <- select(tkt, c("BOOKING_ID", "TICKET_NUMBER", "ORIGINAL_TICKET_NUMBER", "FORM_OF_PAYMENT",
                                     "CURRENCY_fin", "TOTAL_PRICE_PLN", "PAX_TYPE", "Child_flg", "CORPORATE_CONTRACT_FLG",
                                     "LOYAL_CUSTOMER"))

tkt_done$TOTAL_PRICE_PLN <- as.numeric(tkt_done$TOTAL_PRICE_PLN)
tkt_done$CORPORATE_CONTRACT_FLG <- ifelse(tkt_done$CORPORATE_CONTRACT_FLG == 'N', 0 , 1)
tkt_done$LOYAL_CUSTOMER <- ifelse(tkt_done$LOYAL_CUSTOMER == 'N', 0 , 1 )

###########
### FCP ###
###########

# FLIGHT_DATE_LOCAL_LOCAL

# change format into date
fcp$FLIGHT_DATE_LOCAL <- ymd(fcp$FLIGHT_DATE_LOCAL)
fcp$FLIGHT_DATE_LOCAL_WEEKDAY <- ifelse(as.POSIXlt(fcp$FLIGHT_DATE_LOCAL)$wday == 0, 7,as.POSIXlt(fcp$FLIGHT_DATE_LOCAL)$wday) 
fcp$FLIGHT_DATE_LOCAL_QUARTER <- quarter(ymd(fcp$FLIGHT_DATE_LOCAL))

fcp <- fcp[!(is.na(fcp$BOOKED_CABIN) | fcp$BOOKED_CABIN==""), ]
fcp <- fcp %>% 
  mutate(FLIGHT_TIME = case_when((TIME_DEPARTURE_LOCAL_TIME >= '22:00:00' & TIME_DEPARTURE_LOCAL_TIME <= '23:59:59') | (TIME_DEPARTURE_LOCAL_TIME >= '00:00:00' & TIME_DEPARTURE_LOCAL_TIME <= '03:59:59') ~  '22-04',
                                 TIME_DEPARTURE_LOCAL_TIME >= '04:00:00' & TIME_DEPARTURE_LOCAL_TIME <= '10:00:00' ~ '04-10',
                                 TIME_DEPARTURE_LOCAL_TIME >= '10:00:01' & TIME_DEPARTURE_LOCAL_TIME <= '16:00:00' ~ '10-16',
                                 TIME_DEPARTURE_LOCAL_TIME >= '16:00:01' & TIME_DEPARTURE_LOCAL_TIME <= '21:59:59' ~ '16-22',))

fcp_done <- fcp %>% 
  select(TICKET_NUMBER, COUPON_NUMBER, ORIGIN_AIRPORT_CODE, FLIGHT_DATE_LOCAL, FLIGHT_DATE_LOCAL_WEEKDAY,
         FLIGHT_DATE_LOCAL_QUARTER, FLIGHT_TIME, FLIGHT_DISTANCE,FLIGHT_RANGE, 
         BOOKED_CABIN, VAB)

colnames(bkg_done) <- paste("BKG", colnames(bkg_done), sep = "_")
colnames(emd1) <- paste("EMD", colnames(emd1), sep = "_")
colnames(fcp_done) <- paste("FCP", colnames(fcp_done), sep = "_")
colnames(tkt_done) <- paste("TKT", colnames(tkt_done), sep = "_")

emd1$EMD_BAGGAGE <- if_else(emd1$EMD_CATEGORY == "BAGGAGE", 1, 0)
emd1$EMD_SPECIAL_EQUIPMENT <- if_else(emd1$EMD_CATEGORY == "SPECIAL EQUIPMENT", 1, 0)
emd1$EMD_SEAT_SELECTION <- if_else(emd1$EMD_CATEGORY == "SEAT SELECTION", 1, 0)
emd1$EMD_PET <- if_else(emd1$EMD_CATEGORY == "PET", 1, 0)
emd1$EMD_SERVICE <- if_else(emd1$EMD_CATEGORY == "SERVICE", 1, 0)
emd1$EMD_MEAL <- if_else(emd1$EMD_CATEGORY == "MEAL", 1, 0)
emd1$EMD_FEE <- if_else(emd1$EMD_CATEGORY == "FEE", 1, 0)
emd1$EMD_OTHER <- if_else(emd1$EMD_CATEGORY == "OTHER", 1, 0)


# Merging datasets
merge_tkt_bgk <- sqldf('select bkg.*, tkt.* 
                            from tkt_done tkt
                            left join bkg_done bkg on tkt.tkt_booking_id = bkg.bkg_booking_id') 

laczone2_emd <- sqldf('select lac.*, emd.* 
                            from merge_tkt_bgk lac
                            left join emd1 emd on lac.tkt_ticket_number = emd.emd_reference_ticket_number') 

laczone2_emd1 <- sqldf('select a.*
                       from laczone2_emd a left join bkg b on
                       a.tkt_booking_id = b.booking_id')

laczone2_emd <- dplyr::select(laczone2_emd1, -c("EMD_CATEGORY", "EMD_SALES_CHANNEL", "EMD_ORIGIN_AIRPORT_CODE", "EMD_REFERENCE_TICKET_NUMBER"))

laczone2_emd_no_dates <- dplyr::select(laczone2_emd, -c("EMD_SALES_DATE"))

laczone2_emd_no_dates$EMD_TOTAL_PRICE_PLN <- as.numeric(laczone2_emd_no_dates$EMD_TOTAL_PRICE_PLN)

sums_by_no_tickets <- aggregate(laczone2_emd_no_dates[21:28], by = list(laczone2_emd_no_dates$TKT_TICKET_NUMBER), FUN = sum, na.rm=T)

laczone2_emd_no_dates2 <- laczone2_emd_no_dates[,1:20]

laczone2_emd_no_dates2 <- laczone2_emd_no_dates2 %>% 
                            group_by(TKT_TICKET_NUMBER)

colnames(sums_by_no_tickets)[1] <- c('TKT_TICKET_NUMBER')

laczone2_emd_aggr <- sqldf('select a.*, b.*
                       from laczone2_emd_no_dates2 a left join sums_by_no_tickets b on
                       a.TKT_TICKET_NUMBER = b.TKT_TICKET_NUMBER')

colnames(laczone2_emd_aggr)[11] <- c('TKT_TICKET_NUMBER1')

laczone3_fcp <- sqldf('select distinct a.*, b.*
                       from laczone2_emd_aggr a left join fcp_done b on
                       a.TKT_TICKET_NUMBER = b.FCP_TICKET_NUMBER')

laczone3_fcp$coupon_id <- paste(laczone3_fcp$TKT_TICKET_NUMBER, laczone3_fcp$FCP_COUPON_NUMBER) 
laczone3_fcp <- distinct(laczone3_fcp, coupon_id, .keep_all = T)

laczone3_fcp$EMD_TOTAL_PRICE_PLN <- ifelse(is.na(laczone3_fcp$EMD_TOTAL_PRICE_PLN), 0, laczone3_fcp$EMD_TOTAL_PRICE_PLN)

laczone3_fcp %>% 
  mutate(ratio_EMD_TKT_price = EMD_TOTAL_PRICE_PLN/TKT_TOTAL_PRICE_PLN) -> laczone3_fcp 

laczone3_fcp$EMD_BAGGAGE <- ifelse(laczone3_fcp$EMD_BAGGAGE >=1, 1, 0)
laczone3_fcp$EMD_SPECIAL_EQUIPMENT<- ifelse(laczone3_fcp$EMD_SPECIAL_EQUIPMENT >=1, 1, 0)
laczone3_fcp$EMD_SEAT_SELECTION <- ifelse(laczone3_fcp$EMD_SEAT_SELECTION >=1, 1, 0)
laczone3_fcp$EMD_PET<- ifelse(laczone3_fcp$EMD_PET>=1, 1, 0)
laczone3_fcp$EMD_SERVICE <- ifelse(laczone3_fcp$EMD_SERVICE >=1, 1, 0)
laczone3_fcp$EMD_MEAL<- ifelse(laczone3_fcp$EMD_MEAL >=1, 1, 0)
laczone3_fcp$EMD_FEE <- ifelse(laczone3_fcp$EMD_FEE >=1, 1, 0)
laczone3_fcp$EMD_OTHER <- ifelse(laczone3_fcp$EMD_OTHER >=1, 1, 0)
  
final_filters <- laczone3_fcp 

final_filters<- final_filters %>% 
  filter(UPGRADED_FLAG == 'Y')

final_filters$FLIGHT_DATE_LOCAL <- as.Date(final_filters$FLIGHT_DATE_LOCAL)
final_filters$UPGRADE_SALES_DATE <- as.Date(final_filters$UPGRADE_SALES_DATE)
final_filters$UPGRADE_SALES_DATE <- as.numeric(final_filters$UPGRADE_SALES_DATE)
final_filters$FLIGHT_DATE_LOCAL <- as.numeric(final_filters$FLIGHT_DATE_LOCAL)
final_filters$flg_min_upg <- final_filters$FLIGHT_DATE_LOCAL - final_filters$UPGRADE_SALES_DATE 

final_filters %>% 
  mutate(flight_minus_upgrade =  FLIGHT_DATE_LOCAL - UPGRADE_SALES_DATE)
 
final_filters$final_flag <- ifelse(final_filters$flg_min_upg < 0, 'false', 'correct')

into_reg <- final_filters %>% 
  filter(final_flag == 'correct') %>% 
  select(BKG_BOOKING_ID, TKT_TICKET_NUMBER1, )

final_reg <- into_reg %>% 
  select(BKG_BOOKING_ID, TKT_TICKET_NUMBER1, BKG_SALES_DATE_MONTH, BKG_SALES_DATE_WEEKDAY,
         BKG_BOOKING_WINDOW_D, BKG_STAY_LENGTH_GROUPED,BKG_FLIGHT_COUPONS,BKG_SALES_CHANNEL_grouped,
         TKT_CURRENCY_fin, TKT_TOTAL_PRICE_PLN,TKT_PAX_TYPE,TKT_Child_flg,TKT_CORPORATE_CONTRACT_FLG,
         TKT_LOYAL_CUSTOMER,EMD_TOTAL_PRICE_PLN,EMD_BAGGAGE, EMD_SPECIAL_EQUIPMENT, EMD_SEAT_SELECTION,
         EMD_PET,EMD_SERVICE, EMD_MEAL,EMD_FEE,EMD_OTHER,
         FCP_FLIGHT_DATE_QUARTER, FCP_FLIGHT_DATE_WEEKDAY, FCP_FLIGHT_TIME, FCP_FLIGHT_DISTANCE,
         FCP_FLIGHT_RANGE, BKG_TRIP_TYPE, FCP_BOOKED_CABIN, FCP_VAB, ratio_EMD_TKT_price, flg_min_upg)

laczone3_fcp <- laczone3_fcp %>% 
  select(BKG_BOOKING_ID, TKT_TICKET_NUMBER1, FCP_COUPON_NUMBER, BKG_SALES_DATE_MONTH, BKG_SALES_DATE_WEEKDAY,
         BKG_BOOKING_WINDOW_D, BKG_STAY_LENGTH_GROUPED,BKG_FLIGHT_COUPONS,BKG_SALES_CHANNEL_grouped,
         TKT_CURRENCY_fin, TKT_TOTAL_PRICE_PLN,TKT_PAX_TYPE,TKT_Child_flg,TKT_CORPORATE_CONTRACT_FLG,
         TKT_LOYAL_CUSTOMER,EMD_TOTAL_PRICE_PLN,EMD_BAGGAGE, EMD_SPECIAL_EQUIPMENT, EMD_SEAT_SELECTION,
         EMD_PET,EMD_SERVICE, EMD_MEAL,EMD_FEE,EMD_OTHER,
         FCP_FLIGHT_DATE_LOCAL_QUARTER, FCP_FLIGHT_DATE_LOCAL_WEEKDAY, FCP_FLIGHT_TIME, FCP_FLIGHT_DISTANCE,
         FCP_FLIGHT_RANGE, BKG_TRIP_TYPE, FCP_BOOKED_CABIN, FCP_VAB, ratio_EMD_TKT_price )

unique(train$FCP_BOOKED_CABIN)

train <- distinct(train)
test_fin <- na.omit(test)

saveRDS(do_klasyfikacji, file = "backup2250_class.rds")

set.seed(997)
data_upgs <- do_klasyfikacji[do_klasyfikacji$FCP_UPGRADED_FLAG == "Y",]
data_non <- do_klasyfikacji[do_klasyfikacji$FCP_UPGRADED_FLAG == "N",]

data_non_samp <- sample_n(data_non, nrow(data_non)*0.05) 

dim(data_upgs)
dim(data_non_samp)

data_samp <- rbind(data_upgs, data_non_samp)

data_samp1 <- na.omit(data_samp)

do_regresji <- na.omit(final_reg)

saveRDS(data_samp1, file = "data_classification.rds")
saveRDS(do_regresji, file = "data_regresja.rds")
saveRDS(laczone3_fcp, file = "test.rds")

train <- readRDS('data_classification.rds')
test <- readRDS('test.rds')
tr <- na.omit(train)

############
trn <- dplyr::select_if(tr, is.numeric)
corr <- round(cor(trn), 1)
ggcorrplot(corr, method = "circle")
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

trn <- trn  %>% 
  select(BKG_BOOKING_WINDOW_D,TKT_TOTAL_PRICE_PLN,EMD_TOTAL_PRICE_PLN,FCP_FLIGHT_DISTANCE)
pairs.panels(trn,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "kendall", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

ids <- read.csv2('submit_ids.csv', sep = ',')

ids$ticket_number <- as.numeric(ids$TKT_TICKET_NUMBER1)
ids$coupon_number <- as.numeric(ids$COUPON_NUMBER)

results <- read.csv2('submit_solutions.csv', sep = ',')

results$predictions <- as.numeric(results$X1)

tocsv <- cbind(ids[,c(4,5)], results[,c(5)])

colnames(tocsv$`results[, c(5)]`)[1] <- c('prediction')

tocsv$predictions <- tocsv$`results[, c(5)]`

tocsv <- tocsv[,-c(3)]
tocsv$ticket_number <- as.character(tocsv$ticket_number)
saveRDS('tst_results.rds')
write.csv2(tocsv, 'tst_results.csv')



