# load packages ------------------------------------------------------------
library(tidyverse)
library(zoo)
library(xts)
library(lubridate)
library(urca)
library(forecast)
library(tseries)
library(ggfortify)
source("./make_list.R")
source("./plot_figs.R")
source("./get_temp.R")

# Load data ------------------------------------------------------------
stacdata <- read_csv("/Users/ahayashi/Dropbox/Imported/Stock/Timeseries_TW_Engraulis-japonicus.csv") %>%
  rename(year = Year,
         biomass = `B_10^3ton`) %>%
  dplyr::select(year, biomass)

sstdata <- get_tw_sst("../../data/漁海況データ（浮魚資源Gへ）.xlsx", 2018)

data <- readRDS("../output/西海ブロック漁獲量2018.rds")
db   <- make_database(data) %>%
  mutate(day = 1,
         date = make_date(year, month, day))
db2 <- left_join(db, sstdata, key = c(year, month))
db3 <- left_join(db2, stacdata, key = year)

blank_data <- data_frame(year = 2019,
                         month = 1:3,
                         tw_degc = NA,
                         biomass = NA)

seikai_org <- db3 %>%
  bind_rows(blank_data) %>%
  group_by(year, month, tw_degc, biomass) %>%
  summarize(logcatch = log(sum(catch_ton, na.rm = TRUE))) %>%
  as.data.frame()

seikai <- db3 %>%
  filter(year < 2018) %>%
  group_by(year, month, tw_degc, biomass) %>%
  summarize(logcatch = log(sum(catch_ton, na.rm = TRUE))) %>%
  as.data.frame()

seikai_year <- seikai %>%
  group_by(year) %>%
  summarize(sum = sum(logcatch))

seikai_ts <- seikai %>%
  dplyr::select(logcatch) %>%
  unlist() %>%
  ts(start = min(db$year), frequency = 12)

seikai_sst <- seikai %>%
  dplyr::select(tw_degc) %>%
  unlist() %>%
  ts(start = min(db$year), frequency = 12)

seikai_b <- seikai %>%
  dplyr::select(biomass) %>%
  unlist() %>%
  ts(start = min(db$year), frequency = 12)
xvars <- cbind(seikai_sst, seikai_b)

# autoplot(seikai_ts)
# autoplot(seikai_sst)

# analysis ------------------------------------------------------------

ndiffs(seikai_ts) # No lag is needed
ggtsdisplay(seikai_ts)

ggsubseriesplot(seikai_ts)

seikai_ts_seas <- diff(seikai_ts, lag = frequency(seikai_ts))
ggtsdisplay(seikai_ts_seas)
acf(seikai_ts_seas, plot = FALSE, lag.max = 12)


train       <- window(seikai_ts,  end = c(2016, 12))
xvars_train <- window(xvars, end = c(2016, 12))
test        <- window(seikai_ts,  start = c(2017, 1), end = c(2017, 12))
xvars_test  <- window(xvars, start = c(2017, 1), end = c(2017, 12))

# Build model ----------------------------------------------------------------------
model_sarimax <- auto.arima(y = seikai_ts,
#                            y = train,
#                            xreg = xvars_train,
                            ic = "aicc",
                            max.order = 8,
                            stepwise = FALSE,
                            approximation = FALSE,
                            parallel = TRUE,
                            num.cores = 4)
model_sarimax

abs(polyroot(c(1, -coef(model_sarimax)[c("ar1")]))) # ok
abs(polyroot(c(1,  coef(model_sarimax)[c("ma1")]))) # ok
checkresiduals(model_sarimax)
jarque.bera.test(resid(model_sarimax)) # Not good. Residuals do not distribute normally (p < 0.001).

# Validation of the model ------------------------------------------------------------

model_valid <- forecast(model_sarimax,
#                           xreg = xvars_test,
                           h = 24,
                           level = c(95, 70))
model_valid
mean(model_valid$mean[11:15])
mean(model_valid$lower[11:15])
mean(model_valid$upper[11:15])
p <- autoplot(model_valid, predict.color = 1)
ggsave("../../figs/tsplot.png", family = "Helvetica")

xvar_mean <- data.frame(tw_degc = rep(mean(xvars_train[,"seikai_sst"], na.rm = TRUE), 12),
                        biomass = rep(mean(xvars_train[,"seikai_b"])))
fc_mean  <- forecast(model_sarimax,
                     xreg = xvar_mean,
                     h = 12,
                     level = c(95, 70))
autoplot(fc_mean, predict.color = 1)

xvar_tail <- data.frame(tw_degc = rep(tail(xvars_train[,"seikai_sst"], n = 1), 12),
                        biomass = rep(tail(xvars_train[,"seikai_b"], n = 1), 12))
fc_tail  <- forecast(model_sarimax,
                     xreg = xvar_tail,
                     h = 24,
                     level = c(95, 70))
autoplot(fc_tail, predict.color = 1)

g <- autoplot(model_valid)
real2017 <- data.frame(date = seq.Date(as.Date("2017-01-01"), as.Date("2017-12-01"), by = "month"), logcatch = test)
g <- g + geom_point(data = real2017, aes(date, logcatch))
g

gm <- autoplot(fc_mean)
real2017 <- data.frame(date = seq.Date(as.Date("2017-01-01"), as.Date("2017-12-01"), by = "month"), logcatch = test)
gm <- gm + geom_point(data = real2017, aes(date, logcatch))
gm

gt <- autoplot(fc_tail)
real2017 <- data.frame(date = seq.Date(as.Date("2017-01-01"), as.Date("2017-12-01"), by = "month"), logcatch = test)
gt <- gt + geom_point(data = real2017, aes(date, logcatch))
gt

naive <- meanf(train, h = 12)

rmse_valid <- sqrt(sum((model_valid$mean - test)^2) / length(model_valid$mean))
rmse_valid
accuracy(naive, x = test)["Test set", "RMSE"]
accuracy(model_valid, x = test)["Test set", "RMSE"] # better than naive model
accuracy(fc_mean, x = test)["Test set", "RMSE"]
accuracy(fc_tail, x = test)["Test set", "RMSE"] # best

model_valid$mean
str(model_valid)


# Plot by year ------------------------------------------------------------
res_valid <- data.frame(year = rep(2017, 12),
                          month = month.abb,
                          logcatch = model_valid$mean,
                          lwr = model_valid$lower,
                          upr = model_valid$upper) %>%
  group_by(year) %>%
  summarize(sum = sum(logcatch),
            upr = sum(upr.95.),
            lwr = sum(lwr.95.))

years <- seikai %>%
  filter(year < 2017) %>%
  select(year) %>%
  unlist()
years
length(unique(years))
nrow(seikai)

res_train   <- data.frame(year = years,
                          month = rep(month.abb, length(unique(years))),
                          logcatch = as.numeric(model_sarimax$fitted),
                          upper = model_sarimax$fitted + 1.96 * sqrt(model_sarimax$sigma2),
                          lower = model_sarimax$fitted - 1.96 * sqrt(model_sarimax$sigma2)) %>%
  group_by(year) %>%
  summarize(sum = sum(logcatch),
            upr = sum(upper),
            lwr = sum(lower))

res <- bind_rows(res_train, res_valid)

p <- res %>%
  ggplot(aes(year, sum)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = hsv(200/360, 0.8, 0.8, 0.4)) +
  geom_line(color = hsv(200/360, 0.8, 0.8))
p <- p + geom_point(data = seikai_year, aes(year, sum))
p

dat.forecast <- data.frame(biomass = rep(tail(stacdata$biomass, 1), 24),
                           tw_degc = rep(filter(sstdata, year==2017)$tw_degc, 2)) %>%
  ts(start = 2016, frequency = 12)
dat.forecast

ts.plot(model_sarimax$fit, train, col = c(3, 1))
