# Attempting implementation of rolling forecasts

require("tidyquant")
require("tidyverse")
require("ggplot2")
require("lubridate")
library(splines)
library(dplyr)
library(nlme)
library(pROC)
library(glmnet)



if (exists("AAPL") == FALSE) {


  #AAPL <- tq_get("NVDA", get = "stock.prices", from = "2000-09-01", to = "2024-03-01")

  # Get S&P 500 data
  sp500 <- tq_get(
    "^GSPC",
    #"NVDA",
    #"MSFT",
    from = "2000-09-01", to = "2024-03-01")

  AAPL <- sp500
}


end <- today()

dat <- AAPL %>%
  mutate(closelagperc = lag(close,1)/lag(close,2),
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

PERIODS <- 14 #days
mindate <- min(dat$date)
maxdate <- max(dat$date)

segments <- seq( mindate, maxdate - PERIODS, by = PERIODS)

results <- data.frame(
    date = seq(mindate,maxdate,by=1),
    pr = NA
  ) %>%
  left_join(y=dat %>% select(date, close2wkperc), by = "date")


pb <- txtProgressBar(max = length(segments), style = 3)
for ( i in 20:length(segments)) {

  act_segment <- segments[i]
  act_data_full <- dat %>%
    filter(date < act_segment)

  act_data    <- act_data_full %>%
    mutate(close2wkperc = ifelse( date < act_segment - PERIODS, close2wkperc, NA))


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
      ,
      weights= 1/ ((as.numeric(segments[i] - act_data$date)))^2,
      na.action=na.omit,
      #control=glsControl(maxIter = 200),
      act_data)

  act_data_future <-
    act_data_full %>%
    filter( date >= act_segment - PERIODS)



  results$pr[results$date %in% act_data_future$date] <- predict(mod,
                                                                newdata = act_data_future %>%
                                                                  filter(date >= act_segment -
                                                                           PERIODS))

  setTxtProgressBar(pb, i)
}
close(pb)

# summary(mod)
# anova(mod)
# plot(effects::predictorEffects(mod))


results %>%
  mutate(pr = ifelse(pr>2,2,pr)) %>%
  mutate(pr = ifelse(pr<0,0,pr)) %>%
  ggplot(aes(y=close2wkperc,
             x=pr,
             color = date)) +
    theme_tq() +
   geom_hline(yintercept=1,color = "grey40") +
   geom_vline(xintercept=1,color = "grey40") +
    geom_point(alpha=.2, size = .8) +
    geom_smooth(se=FALSE) +
    geom_smooth(se=FALSE, method = "lm") +
    scale_y_continuous(limits=c(0.66,1.5))




cor(results$close2wkperc, results$pr, use = "pairwise.complete.obs",
    method = "kendall")


results %>%
  ggplot(aes(x=close2wkperc)) +
    geom_histogram(fill="blue",color="black",bins = 100) +
    geom_histogram(fill="red",color="black",bins = 100,
                   data = results %>% filter(pr > 1)) +
    theme_tq()



# Create a binary outcome variable based on close2wkperc
results$above_1 <- ifelse(results$close2wkperc > 1, 1, 0)

# Create an ROC curve
roc_curve <- roc(results$above_1, results$pr)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue")

# Calculate the area under the ROC curve (AUC)
auc(roc_curve)

get_gainz <- function( treshold =1 ) {
  gainz <- 0
  for (i in 1:nrow(results)) {
    if ( is.na(results$pr[i]) == FALSE) {
      if (results$pr[i] > treshold) {
        gainz <- gainz + results$close2wkperc[i]
      } else {
        gainz <- gainz + 1
      }
    }
  }
  gainz
}
Get_gainz <- Vectorize(get_gainz)


length(na.omit(results$pr)) # No. of days you have a prediction or money if did nothing

#Money if you bought shares for 2wks only every single day
results$close2wkperc[results %>% select(pr,close2wkperc) %>% complete.cases] %>% sum



d <- data.frame(t=seq(0,2,length.out = 100)) %>%
  mutate(gainz = Get_gainz(t))

d %>%
  ggplot(aes(x=t,y=gainz)) +
    theme_tq() +
    geom_line() +
    geom_hline(yintercept=results$close2wkperc[results %>% select(pr,close2wkperc) %>% complete.cases] %>% sum)

# Money if you bought shares for 2 weeks only when predicted 1+, otherwise did nothing with it
max(d$gainz)

dat_month <-
  results %>%
    mutate(month_value = format(date, "%Y-%m"),
           day = format(date, "%d"))




get_endofmonth <- function( month, treshold, dat_month. = dat_month) {

  act_dat <- dat_month. %>% filter( month_value == month,
                                    is.na(pr) == FALSE)
  balance <- 1

  if (nrow(act_dat) == 0) {
    return(1)
  }

  # assume dat_month. is arranged by date
  for (i in 1:nrow(act_dat)) {
    if(  act_dat$pr[i] > treshold) {
      balance <- act_dat$close2wkperc[i]
      break
    }
  }
  return(balance)
}



months <- expand.grid( dat_month = dat_month$month_value %>% unique(),
                       treshold  = seq(1,1.1,length.out=20),
                       balance = NA)

for (i in 1:nrow(months)) {
  months$balance[i] <- get_endofmonth(month = months$dat_month[i],
                                      treshold = months$treshold[i])
}

months <-
  months %>%
  group_by(treshold) %>%
  mutate(mean_balance = mean(balance))

months %>%
  ggplot(aes(x=dat_month,y = balance, color = treshold)) +
  geom_point()


months %>%
  ggplot(aes(x=treshold,y = mean_balance, color = treshold)) +
  geom_point()

