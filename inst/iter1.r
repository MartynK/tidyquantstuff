require("tidyquant")
require("tidyverse")
require("ggplot2")
library(splines)
library(dplyr)
library(nlme)

AAPL <- tq_get("AAPL", get = "stock.prices", from = "2010-09-01", to = "2024-03-01")
AMZN <- tq_get("AMZN", get = "stock.prices", from = "2000-01-01", to = "2024-03-01")
sp500 <- tq_index("SP500")

end <- today()

AAPL <- AAPL %>%
  mutate(closelag = lag(close),
         openlag = lag(open),
         openchperc = lag(open,1)/lag(open,2),
         volumelag = lag(log(volume)),
         volumelogdiff = lag(log(volume),1) - lag(log(volume),2),
         close2wk = close - lag(close,14),
         close2wklog = sign(close2wk) - log(close2wk),
         close2wkperc = lag(close,14)/close)


mod <- lm(close2wkperc ~
            (
              ns(openchperc,3) +
                ns(closelag,df=3) +
                ns((volumelogdiff),df=3) +
                ns((volumelag),df=3)
            )^2 +
            ns(yday(date),df=4)
          ,
          #weights=varExp(form=~fitted(.)),
          na.action=na.omit,
          #control=glsControl(maxIter = 200),
          AAPL)

summary(mod)
anova(mod)

mod %>% effects::predictorEffects(
  partial.residuals=TRUE
) %>% plot


AAPL %>%
  ggplot(aes(x = date, y = close, open = open,
             high = high, low = low, close = close)) +
  geom_candlestick() +
  geom_bbands(ma_fun = SMA, sd = 2, n = 20) +
  #scale_y_log10() + #BBands screw this up
  labs(title = "AAPL Candlestick Chart",
       subtitle = "BBands with SMA Applied",
       y = "Closing Price", x = "") +
  # coord_x_date(xlim = c(end - weeks(24), end),
  #              ylim = c(aapl_range_60_tbl$min_low * 0.85,
  #                       aapl_range_60_tbl$max_high) * 1.05) +
  theme_tq()



