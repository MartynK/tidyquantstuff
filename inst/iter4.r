# Attempting implementation different investment strategies (not according to tidyquant)

require("tidyquant")
require("tidyverse")
require("ggplot2")
require("lubridate")
library(splines)
library(dplyr)
library(nlme)
library(pROC)
library(glmnet)
library(nlme)


# get stonks, its kinda slow so dont do it during reruns
if (exists("stonks") == FALSE) {

  stonks <- tq_get(
    c(
      "SPY"
      #"MSFT"
      #,"AAPL"
      #,"NVDA"
      #,"AMZN"
      # ,"META"
      #,"GOOGL"
      # #,"BRK.B"
      # #,"GOOG"
      #,"LLY"
      # ,"AVGO"
      # ,"JPM"
      # ,"TSLA"
      # ,"V"
      # ,"UNH"
    ),
    from = "2000-09-01", to = lubridate::today())

}

# turn above into function
get_columns_for_model <- function(df) {
  out <- df %>%
    mutate(symbol=factor(symbol),
           closelagperc = lag(close,1)/lag(close,2),
           openlag = lag(open),
           highlagperc = lag(high,1)/lag(high,2),
           openchperc = lag(open,1)/lag(open,2),
           volumelag = lag(log(volume)),
           volumelogdiff = exp(lag(log(volume),1) - lag(log(volume),2)),
           diff = lag(high,1) - lag(low,1),
           close2wk = close - lag(close,14),
           close2wklog = sign(close2wk) - log(close2wk),
           close2wkperc = lag(close,14)/close,
           date_yday = yday(date))
}

dat <- get_columns_for_model(stonks)

fit_model_at_date <- function(dat,
                              date_limit = as.Date("2020-07-01"),
                              return_just_predictions = TRUE) {

  if ( weekdays(date_limit) %in% c("Saturday", "Sunday")) {
    # its on a weekend, no trading
    return(-Inf)
  }

  dat_act <- dat %>%
    filter(date <= date_limit) %>%
    mutate(close2wkperc = lead(close,14)/close)

  mod <-
    lm(close2wkperc ~
         (
           ns(openchperc,2) +
             ns(closelagperc,df=2) +
             ns(highlagperc,df=1) +
             ns(diff,df=1) *
             ns(date_yday,df=2) +
             ns(volumelag,df=2)
         )
       ,weights= 1/ ((as.numeric(dat_act$date[nrow(dat_act)] - dat_act$date)))^2
       , na.action=na.omit,
       dat_act)

  dat_act$pred <- predict(mod, newdata = dat_act)

  if (return_just_predictions) {
    return(dat_act$pred[nrow(dat_act)])
  } else {
    return( list(mod = mod, dat_act = dat_act))
  }

}


lst <- fit_model_at_date(dat, as.Date("2020-07-01"))


CUTOFF <- 0.975

out <- data.frame(
  date = seq.Date(as.Date(dat$date[91]), lubridate::today(), by = 1),
  no_owned_std = 0,
  no_owned_cut = 0,
  pred = NA
  )
budget_std <- 0
budget_cut <- 0

pb <- txtProgressBar(max = nrow(out), style = 3)
for (i in 1:nrow(out)) {
  act_date <- out$date[i]

  if (mday(act_date) == 1) {
    # pay day
    budget_std <- budget_std + 250
    budget_cut <- budget_cut + 250
  }

  if(act_date %in% dat$date == FALSE) {
    # no trading/data on this day
    setTxtProgressBar(pb, i)
    next
  }

  if ( budget_cut > 0 ) {
    act_pred <- fit_model_at_date(dat, act_date)
    out$pred[i] <- act_pred
    if (act_pred > CUTOFF) {
      out$no_owned_cut[i:nrow(out)] <- out$no_owned_cut[i] + budget_cut / dat$low[dat$date == act_date]
      budget_cut <- 0
    }
  }

  if ( budget_std > 0) {
    out$no_owned_std[i:nrow(out)] <- out$no_owned_std[i] + budget_std / dat$low[dat$date == act_date]
    budget_std <- 0
  }

  setTxtProgressBar(pb, i)

}

close(pb)

# buy the last round if have budget leftover
if (budget_cut > 0) {
  out$no_owned_cut[nrow(out)] <- out$no_owned_cut[nrow(out)] + budget_cut / dat$low[nrow(dat)]
}

# plot out
out %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=no_owned_std, color = "std")) +
  geom_line(aes(y=no_owned_cut, color = "cut")) +
  labs(title = "Investment strategies",
       y = "Value",
       x = "Date") +
  scale_color_manual(values = c("std" = "blue", "cut" = "red")) +
  theme_minimal()


# plot out
out %>%
  ggplot(aes(x=date, y = no_owned_cut - no_owned_std)) +
  geom_line() +
  labs(title = "Investment strategies",
       y = "Value",
       x = "Date") +
  theme_minimal()
