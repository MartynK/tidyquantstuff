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

# setting up a knawrly sequence for penalty param. lambda
n <- 2 # Number of points
xs <- seq(0.01, pi/1.075, length.out = n)
spacing <- 1/(sin(xs) * 15 + 10) # Sine wave adjusted to have the right range
final_sequence <- cumsum(spacing) # Cumulative sum to create the sequence
final_sequence <- final_sequence - final_sequence[1] # Shift to start at 0
final_sequence <- (final_sequence / max(final_sequence)) * 0 - 100 # Scale to the range -15 to -5
# Print the result
plot(final_sequence)
final_sequence <- final_sequence[1]

if (exists("AAPL") == FALSE) {
  AAPL <- tq_get("AAPL", get = "stock.prices", from = "2000-09-01", to = "2024-03-01")
  sp500 <- tq_index("SP500")
}


end <- today()

dat <- AAPL %>%
  mutate(closelag = lag(close),
         openlag = lag(open),
         highlag = lag(high),
         openchperc = lag(open,1)/lag(open,2),
         volumelag = lag(log(volume)),
         volumelogdiff = lag(log(volume),1) - lag(log(volume),2),
         close2wk = close - lag(close,14),
         close2wklog = sign(close2wk) - log(close2wk),
         close2wkperc = lag(close,14)/close)

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

  form <- as.formula(close2wkperc ~
                       (
                         ns(openchperc,2) +
                           ns(closelag,df=2) +
                           ns(highlag,df=2) +
                           ns(volumelogdiff,df=2) +
                           ns(volumelag,df=1) +
                           ns(yday(date),df=2)
                       ))
  modelmat <- model.matrix(form, act_data)[,-1]

  cv_lasso <- glmnet( modelmat,
                         na.omit(act_data$close2wkperc),
                         weights=
                           1/ ((as.numeric(segments[i] -
                                             act_data$date[
                                               complete.cases(act_data$close2wkperc)
                                                          ]))*0.5)^.6,
                         relax=FALSE,
                         lambda = exp(final_sequence))

  act_data_future <-
    act_data_full %>%
    filter( date >= act_segment - PERIODS)

  form2 <- as.formula(close2wkperc ~
                       (
                         ns(openchperc,2) +
                           ns(closelag,df=2) +
                           ns(highlag,df=2) +
                           ns(volumelogdiff,df=2) +
                           ns(volumelag,df=1) +
                           ns(yday(date),df=2)
                       ) + date)

  predmat <- model.matrix(form, act_data_future)[,-1]
  pr <- predict(cv_lasso,newx=predmat,s="lambda.min")
  results$pr[results$date %in% act_data_future$date] <- pr


  setTxtProgressBar(pb, i)
}
close(pb)


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
    geom_smooth(se=FALSE, method = "lm")


gc()
Sys.sleep(2)

cor(results$close2wkperc, results$pr, use = "pairwise.complete.obs",
    method = "kendall")


# Create a binary outcome variable based on close2wkperc
results$above_1 <- ifelse(results$close2wkperc > 1, 1, 0)

# # Create an ROC curve
# roc_curve <- roc(results$above_1, results$pr)
#
# # Plot the ROC curve
# plot(roc_curve, main = "ROC Curve", col = "blue")
#
# # Calculate the area under the ROC curve (AUC)
# auc(roc_curve)
