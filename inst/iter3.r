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
library(nlme)



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

table(stonks$symbol)

end <- today()

dat <- stonks %>%
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

PERIODS <- 14 #days
mindate <- min(dat$date)
maxdate <- max(dat$date)

segments <- seq( mindate, maxdate - PERIODS, by = PERIODS)

symbols <- as.character(unique(dat$symbol))
for (symbol in 1:length(symbols)) {
  act_symbol <- symbols[symbol]

  dat_symbol <- dat %>% filter(symbol == act_symbol)

  results <- expand.grid(
    date = seq(mindate,maxdate,by=1),
    pr = NA
  ) %>%
    left_join(y = dat_symbol %>%
                filter(symbol == act_symbol) %>%
                select(date,symbol, close2wkperc),
              by = c("date"))


  pb <- txtProgressBar(max = length(segments), style = 3)
  for ( i in 20:length(segments)) {

    act_segment <- segments[i]


    act_data_full <- dat_symbol %>%
      filter(date < act_segment) %>%
      mutate(date_diff=date-act_segment)

    act_data    <- act_data_full %>%
      mutate(close2wkperc = ifelse( date < act_segment - PERIODS, close2wkperc, NA))

    act_data_future <-
      act_data_full %>%
      filter( date >= act_segment - PERIODS)

    good_symbols <- act_data %>%
      filter(is.na(close2wkperc) == FALSE) %>%
      .$symbol %>%
      as.character %>%
      unique

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
         act_data)

    results$pr[results$date %in% act_data_future$date] <-
      predict(mod,
              newdata = act_data_future )

    setTxtProgressBar(pb, i)
  }
  close(pb)

  if (symbol == 1) {
    results_out <- results %>%
      mutate(symbol = symbols[symbol])
  } else {
    results_out <- results %>%
      mutate(symbol = symbols[symbol]) %>%
      bind_rows(.,results_out)
  }
}




# summary(mod)
# anova(mod)
# plot(effects::predictorEffects(mod))


results_out %>%
  filter(is.na(close2wkperc)==FALSE) %>%
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
    scale_y_continuous(limits=c(0.66,1.5)) +
    facet_grid("symbol")



cor(results_out$close2wkperc, results_out$pr, use = "pairwise.complete.obs",
    method = "kendall")


results_out %>%
  mutate(close2wkperc = ifelse(close2wkperc>2,2,close2wkperc)) %>%
  ggplot(aes(x=close2wkperc)) +
    geom_histogram(fill="blue",color="black",bins = 100) +
    geom_histogram(fill="red",color="black",bins = 100,
                   data = results_out %>% filter(pr > 1)) +
    theme_tq()



# Create a binary outcome variable based on close2wkperc
results_out$above_1 <- ifelse(results_out$close2wkperc > 1, 1, 0)

# Create an ROC curve
roc_curve <- roc(results_out$above_1, results_out$pr)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue")

# Calculate the area under the ROC curve (AUC)
auc(roc_curve)

get_gainz <- function( treshold =1 ) {
  gainz <- 0
  for (i in 1:nrow(results_out)) {
    if ( is.na(results_out$pr[i]) == FALSE) {
      if (results_out$pr[i] > treshold) {
        gainz <- gainz + results_out$close2wkperc[i]
      } else {
        gainz <- gainz + 1
      }
    }
  }
  gainz
}
Get_gainz <- Vectorize(get_gainz)


length(na.omit(results_out$pr)) # No. of days you have a prediction or money if did nothing

#Money if you bought shares for 2wks only every single day
results_out$close2wkperc[results_out %>% select(pr,close2wkperc) %>% complete.cases] %>% sum



d <- data.frame(t=seq(0,2,length.out = 100)) %>%
  mutate(gainz = Get_gainz(t))

d %>%
  ggplot(aes(x=t,y=gainz)) +
    theme_tq() +
    geom_line() +
    geom_hline(yintercept=results_out$close2wkperc[results_out %>% select(pr,close2wkperc) %>% complete.cases] %>% sum)

# Money if you bought shares for 2 weeks only when predicted 1+, otherwise did nothing with it
max(d$gainz)

dat_month <-
  results_out %>%
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

tail(results_out)
