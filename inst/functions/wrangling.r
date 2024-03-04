# Data wrangling

descriptor <-
  here::here("inst","extdata","description.xlsx") %>%
  file.path() %>%
  readxl::read_excel(skip = 0)

# make a list to be used as labels for 'labelled::'
labs <-
  # take a 1xN matrix
  matrix( descriptor$description,
          ncol = length(descriptor$description)
  ) %>%
  as.list() %>%
  # add column names as 'names'
  `names<-`(descriptor$name_new)


data <- fil |>
  readxl::read_xls() |> # or read_xlsx() as appropriate
  mutate( across( .cols = which( descriptor$trf == "factor"),
                    .fns = as.factor
    ),
    across( .cols = which( descriptor$trf == "numeric"),
            .fns = as.numeric # removing potential '?', 'NA', '.' etc.
    ),
    across( .cols = which( descriptor$trf == "date"),
            .fns = lubridate::as_datetime
    )) %>%
  `colnames<-`( descriptor$name_new) %>%
  labelled::`var_label<-`(   labs  ) 




