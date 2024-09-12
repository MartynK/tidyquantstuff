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


# turn above into function
get_columns_for_model <- function(df) {
  out <- df %>%
    mutate(symbol=factor(symbol),
           closelagperc = lag(close,1)/lag(close,2),
           openlag = lag(open,1),
           highlagperc = lag(high,1)/lag(high,2),
           openchperc = lag(open,1)/lag(open,2),
           opench1    = open / lag(open,1),
           volumelag = lag(log(volume)),
           volumelogdiff = exp(lag(log(volume),1) - lag(log(volume),2)),
           diff = lag(high,1) - lag(low,1),
           close2wk = lag(close,1) - lag(close,14),
           close2wklog = sign(close2wk) - log(close2wk),
           close2wkperc = lag(close,14)/lag(close,1),
           date_yday = yday(date))
}

check_if_lull <- function(dat, date_act = as.Date("2020-07-01"),
                          dip_perc = 0) {

  indx <- which(dat$date == date_act)

  if (indx > nrow(dat) - 6) {
    return(NA)
  }

  act <- dat[indx:(indx+5),]

  if (min(act$close[2:6]) < act$close[1]*(1-dip_perc)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
Check_if_lull <- Vectorize(check_if_lull, vectorize.args = "date_act")

#############

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


# TODAY's open, Get it manually!!
# https://finance.yahoo.com/quote/SPY/
OPEN <- 550.89

dat <-
  stonks %>%
  # generate the other cols
  get_columns_for_model()


dat$lull <- Check_if_lull(dat,dat$date)


add_open <- function(dat, open) {

  if ( weekdays(last(dat$date)) %in% c("Saturday")) {
    # its on a weekend, no trading
    today_corr = today() + days(2)
  } else if (weekdays(last(dat$date)) %in% c("Sunday")) {
    today_corr = today() + days(1)
  } else {
    today_corr = today()
  }

  # add last day with OPEN
  new_day <-
    data.frame(date = today_corr,
               symbol = "SPY", # doesnt realy matter
           open = open,
           high = NA,
           low = NA,
           close = NA,
           adjusted = NA,
           volume = NA) %>%
      # generate the other cols
      get_columns_for_model()

  dat <- bind_rows(dat,new_day) %>%
    # redo the derived columns so the lagged stuff is good for the last day also
    get_columns_for_model()

  return(dat)
}

# plot lull as smoothers
dat %>%
  ggplot(aes(x=date, y = as.numeric(lull))) +
  geom_smooth() +
  geom_smooth(method="loess",span = .02, color = "salmon4",se=FALSE) +
  labs(title = "Lull detection",
       y = "P(Lull)",
       x = "Date") +
  theme_minimal()

fit_model_at_date <- function(dat,
                              expre = as.formula("lull ~
                                                  (
                                                      ns(closelagperc,df=2) +
                                                      ns(openchperc,df=1) +
                                                      ns(highlagperc,df=1) +
                                                      ns(diff,df=1) *
                                                      ns(date_yday,df=2) +
                                                      ns(volumelag,df=1)
                                                  )"),
                              date_limit = as.Date("2020-07-01"),
                              return_just_predictions = TRUE,
                              open = NA,
                              exponent = 0.8) {

  if ( weekdays(date_limit) %in% c("Saturday", "Sunday")) {
    # its on a weekend, no trading
    return(-Inf)
  }

  if (is.na(open) == FALSE) {
    dat <- add_open(dat, open)
  }

  dat_act <- dat %>%
    filter(date <= date_limit) %>%
    mutate(close2wkperc = lead(close,14)/close,
           weights = 5/(as.numeric(date_limit - date + 1))^exponent)

  # Normalize dat_act$weights to a 0-1 scale
  weights_normalized <- (dat_act$weights - min(dat_act$weights)) /
                          (max(dat_act$weights) - min(dat_act$weights))

  # Scale to 0-100
  dat_act$weights_scaled <- {weights_normalized * 100} %>% round()

  # # Display the scaled values
  # hist(dat_act$weights_scaled,breaks = 100)

  mod <-
    glm(expre
       , na.action=na.omit
       , family = binomial(link = "logit")
       , weights = dat_act$weights_scaled
       , dat_act)

  dat_act$pred <- predict(mod, newdata = dat_act, type = "response")

  # # plot
  # dat_act %>%
  #   ggplot(aes(x=pred, y = as.numeric(lull))) +
  #   geom_point() +
  #   geom_smooth() +
  #   labs(title = "Predictions",
  #        y = "P(Lull)",
  #        x = "Pred") +
  #   theme_minimal()

  if (return_just_predictions) {
    return(dat_act$pred[nrow(dat_act)])
  } else {
    return( list(mod = mod, dat_act = dat_act))
  }

}


# make predictions for current day
fit_model_at_date(dat, date_limit = today(), open = 1000)

open_mat <-
  data.frame(
    open = seq(last(dat$open)*0.8,
               last(dat$open)*1.2,length.out = 10),
    pred = NA
  )


pb <- txtProgressBar(max = nrow(open_mat), style = 3)
for (i in 1:nrow(open_mat)) {
  open_mat$pred[i] <- fit_model_at_date(dat,
                                        date_limit = today()+1,
                                        open = open_mat$open[i])
  setTxtProgressBar(pb, i)
}
close(pb)

#######

test_strategy <- function( cutoff = 0.45,
                           exponent = 0.5) {
  out <- data.frame(
    date = seq.Date(as.Date(dat$date[91]),
                    as.Date("2022-06-30")
                    #lubridate::today()
                    , by = 1),
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
      act_pred <- fit_model_at_date(dat, date_limit =  act_date,
                                    exponent = exponent)
      out$pred[i] <- act_pred
      if (act_pred > cutoff) {
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
    out$no_owned_cut[nrow(out)] <- out$no_owned_cut[nrow(out)] +
                                      budget_cut / dat$low[nrow(dat)]
  }
  return(out)
}

out <- test_strategy(cutoff = 0.45, exponent = 0.5)
#
# out_mat_dip <- data.frame(
#   dip = seq(-0.00025,-0.00025,length.out = 2),
#   ratio = NA
# )
#
# for (i in 1:nrow(out_mat_dip)) {
#
#   dat$lull <- Check_if_lull(dat,dat$date, dip_perc = out_mat_dip$dip[i])
#   out <- test_strategy(0.4, exponent = 0.5)
#   out_mat_dip$ratio[i] <- out$no_owned_cut[nrow(out)] / out$no_owned_std[nrow(out)]
# }
#
# save(out_mat_dip, file = here::here("data", "out_mat_dip.rda"))
#
# # plot out_mat
# out_mat_dip %>%
#   ggplot(aes(x=dip, y = ratio)) +
#   geom_line() +
#   geom_point() +
#   labs(title = "Investment strategies",
#        y = "Value",
#        x = "Cutoff") +
#   theme_minimal()

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

out$no_owned_cut[nrow(out)] / out$no_owned_std[nrow(out)]
