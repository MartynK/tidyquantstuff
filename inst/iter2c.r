

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
  ggplot(aes(x=treshold,y = mean_balance, color = treshold)) +
    geom_point()

