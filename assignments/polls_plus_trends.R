# Packages ####
library(tidyverse)
library(rvest)
library(broom)

# POLLS ####
## Poll URL ###
polls_url <- "https://cdn-dev.economistdatateam.com/jobs/pds/code-test/index.html"

## Poll disclaimers #### 
polls_disclaimers <- polls_url |>
  rvest::read_html() |>
  rvest::html_elements("li") |>
  rvest::html_text() |>
  stringr::str_squish() |>
  tibble::as_tibble() |>
  dplyr::mutate(disclaimer = value,
                value = case_when(str_detect(disclaimer,"Excludes") ~ "*",
                                  str_detect(disclaimer,"Includes") ~ "**",
                                  TRUE ~ NA))

## Latest update ####
polls_date_latest <- polls_url |>
  rvest::read_html() |>
  rvest::html_element("h1") |>
  rvest::html_text() |>
  stringr::str_sub(start = 17L) |>
  lubridate::mdy()

# Vector of candidates ####
candidates <- polls_url |>
  rvest::read_html() |>
  rvest::html_elements("th") |>
  rvest::html_text() |>
  stringr::str_squish() |>
  tibble::as_tibble() |>
  dplyr::filter(!value %in% c("Date","Pollster","Sample")) |>
  dplyr::pull(value) |>
  as.character()

# Number of candidates currently polled
polls_n_candidates <- polls_url |>
  rvest::read_html() |>
  rvest::html_elements("th") |>
  rvest::html_text() |>
  stringr::str_squish() |>
  tibble::as_tibble() |>
  dplyr::filter(!value %in% c("Date","Pollster","Sample")) |>
  dplyr::tally() |>
  as.numeric()

## Poll dataframe ####
polls_raw <- polls_url |>
  rvest::read_html() |>
  # Scrape all variables in the table 
  rvest::html_elements("td , th") |>
  # Read text within the element
  rvest::html_text() |>
  tibble::as_tibble() |>
  # Pivot and rename variables
  dplyr::mutate(var = rep(c("date","pollster","sample",candidates),times=n()/(3+polls_n_candidates))) |>
  tidyr::pivot_wider(names_from = var, values_from = value, values_fn = list) |>
  tidyr::unnest(cols = everything()) |>
  # Drop first row with name of columns
  dplyr::slice(-1)

## Cleaning poll data ####
polls_clean <- polls_raw |>
  # Clean and format variables
  dplyr::mutate(date = lubridate::mdy(date),
                pollster = pollster |> str_trim("both") |> factor()) |>
  dplyr::mutate(n_disc = case_when(str_detect(sample,"\\*") ~ "Excludes overseas territories",
                                   str_detect(sample,"\\*\\*") ~ "Included in alternate question",
                                   TRUE ~ NA)) |>
  dplyr::mutate(n = sample |> str_remove_all("[:punct:]") |> as.integer()) |>
  # Clean voting intentions 
  dplyr::mutate(across(all_of(candidates),
                       ~str_remove(.,"%") |> as.numeric())) |>
  # Validation: across adds to 100
  dplyr::rowwise() |>
  dplyr::mutate(total_polls = sum(c_across(all_of(candidates)),na.rm = TRUE)) |>
  # Errors in the data
  dplyr::mutate(error = case_when(total_polls>101 ~ "Poll error: sum of proportions larger than 100%",
                                  total_polls %in% c(99,101) ~ "Poll error: Decimal point errors, sum of proportions 99% or 101%",
                                  TRUE ~ NA)) |>
  dplyr::ungroup()

## Data frame with corrected error ####
polls_corrections <- polls_clean |>
  # Fix poll with registration error
  dplyr::mutate(Bulstrode = ifelse(date=="2023-11-18" & pollster=="Policy Voice Polling",Bulstrode-(total_polls-100), Bulstrode),
                error = ifelse(date=="2023-11-18" & pollster=="Policy Voice Polling",paste(error,"\n. Correction: Substract the difference between all candidates and 100%"),error)) |>
  dplyr::select(date,pollster,n,n_disc,all_of(candidates),error)

## Export polls ####
polls_export <- polls_corrections |>
  dplyr::mutate(across(all_of(candidates),~./100)) |>
  dplyr::select(date,pollster,n,all_of(candidates), error)

write_csv(polls_export,paste0(today(),"_polls.csv"))

# TRENDS ####

## Estimating LOESS for each candidate
dataland_loess <- polls_corrections |>
  dplyr::mutate(id = row_number(),
                min_date = min(date),
                interval = interval(min_date,date)) |>
  dplyr::mutate(duration = as.duration(interval)/ddays(1)) |>
  dplyr::select(id, date, all_of(candidates), duration, n, pollster) |>
  tidyr::pivot_longer(cols = candidates, names_to = "candidate",values_to = "vote_intention") |>
  dplyr::group_by(candidate) |>
  tidyr::nest() |>
  dplyr::rowwise() |>
  dplyr::mutate(
    model_loess = list(
      loess(vote_intention ~ duration, span = 0.75, data = data, na.action=na.exclude)
    ),
    predicted = list(
      broom::augment(model_loess, data)
    ) 
  ) |>
  dplyr::select(candidate, predicted) |>
  tidyr::unnest(predicted) |>
  dplyr::ungroup()


## Export trends ####
trends_export <- dataland_loess |>
  dplyr::select(candidate, id, date, trends=.fitted) |>
  tidyr::pivot_wider(names_from = candidate, values_from = trends) |>
  dplyr::mutate(across(all_of(candidates),~./100))

write_csv(trends_export,paste0(today(),"_trends.csv"))