
library(COVID19)
library(dplyr)

get_PL_new_tests <- function() {
  read.csv('data/pl_tests_new.csv', encoding='UTF-8')
}

#'
#'
get_tests <- function(country, date.min) {
  # daily data
  covid_data <- covid19(country, level = 1)
  if(country %in% c('PL','POL','Poland','SE','SWE','Sweden')) {
    # Poland
    if(country %in% c('PL','POL','Poland')) {
      # parse tests
      url <- 'https://raw.githubusercontent.com/martinbenes1996/covid19poland/master/data/tests.csv'
      tests <- read.csv(url) %>%
        dplyr::mutate(Date = as.Date(date), Tests = tests)
      tests.total <- tests %>%
        dplyr::filter(region == '') %>%
        dplyr::arrange(Date)
      tests.group <- tests %>%
        dplyr::filter(region != '') %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(tests = sum(tests), .groups = 'drop')
      tests <- tests.total %>%
        dplyr::full_join(tests.group, by = 'Date') %>%
        dplyr::transmute(Date, tests = ifelse(is.na(tests.x), tests.y, tests.x)) %>%
        dplyr::arrange(Date) %>%
        dplyr::transmute(Date, Tests = c(0,diff(tests))) %>%
        dplyr::mutate(Tests = ifelse(Tests > 0, Tests, 0))
      tests.weekly <- tests %>%
        dplyr::filter(Date >= as.Date('2020-09-01')) %>% #'2020-09-07')) %>%
        dplyr::filter(Tests > 0) %>%
        dplyr::filter(Date < as.Date('2020-12-28'))
      tests.new <- get_PL_new_tests()
      colnames(tests.new) <- c('X', 'year', 'week', 'Region', 'Tests', 'Date')
      tests.new <- tests.new %>%
        dplyr::transmute(Date = as.Date(Date, '%Y-%m-%d'),Region, Tests) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(Tests = sum(Tests), .groups='drop')
      tests.weekly <- rbind(tests.weekly, tests.new) %>%
        dplyr::arrange(Date)
      # Sweden
    } else {
      # parse tests
      tests <- read.csv('data/se_tests.csv')
      colnames(tests) <- c('Year','Week','Dates','Region','Tests','Performed','URL')
      tests <- tests %>%
        dplyr::mutate(Week = ifelse(Year == 2021, Week + 1, Week)) %>%
        dplyr::mutate(Date = sprintf('%d-%02d-1', Year,Week-1)) %>%
        dplyr::mutate(Date = as.Date(Date, '%Y-%W-%w'))
      tests.weekly <- tests %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(Tests = sum(Tests, na.rm = T), .groups='drop')
    }
    # fill empty days
    max.date <- max(tests.weekly$Date) + 7
    tests.weekly <- tests.weekly %>%
      dplyr::mutate(days = diff(c(Date, max.date))) %>%
      dplyr::mutate(days = as.integer(days)) %>%
      dplyr::filter(Date > date.min, Tests > 0)
    for(i in 1:nrow(tests.weekly)) {
      row <- tests.weekly[i,]
      end.day <- row$Date + row$days - 1
      week <- rmultinom(1, row$Tests, rep(1/row$days, row$days))
      for(j in 0:(row$days - 1)) {
        dt <- row$Date + j
        if(nrow(tests[which(tests$Date == dt),]) == 0) {
          tests <- rbind(tests, data.frame(Date = dt, Tests = NA))
        }
      }
      tests <- tests %>% dplyr::arrange(Date)
      tests[which(tests$Date >= row$Date & tests$Date <= end.day),]$Tests <- week
    }
    # Covid 19 Data-Hub
  } else {
    tests <- covid_data %>%
      dplyr::ungroup() %>%
      dplyr::filter(date >= date.min, !is.na(tests)) %>%
      dplyr::mutate(Tests = c(0,diff(tests)), Date = date) %>%
      dplyr::filter(Date > date.min, Tests > 0)
  }
  # to view structure
  tests <- tests %>%
    dplyr::transmute(Date, Tests)
  # fill missing dates
  missing_days <- c()
  for(day_offset in 0:as.numeric(max(tests$Date)-min(tests$Date))) {
    day <- min(tests$Date) + day_offset
    if(!(day %in% tests$Date)) {
      missing_days <- c(missing_days, strftime(day))
    }
  }
  tests <- tests %>%
    dplyr::mutate(Date = strftime(Date))
  if(length(missing_days) > 0) {
    tests <- rbind(tests, data.frame(Date = missing_days, Tests = 0))
  }
  # to view structure
  tests <- tests %>%
    dplyr::arrange(Date) %>%
    dplyr::transmute(Date = as.Date(Date), Tests, Variable = 'Tests')
  return(tests)
}

get_incidence <- function(country, date.min = '2020-01-01', date.max = NA) {
  covid_data <- covid19(country, level = 1)
  covid_stats <- covid_data %>%
    dplyr::ungroup() %>%
    dplyr::transmute(dates = date, I = c(0,diff(confirmed))) %>%
    dplyr::mutate(I = ifelse(is.na(I), 0, I)) %>%
    dplyr::mutate(I = as.integer(I)) %>%
    dplyr::mutate(I = ifelse(I < 0, 0, I)) # remove corrections
  covid_stats <- covid_stats %>%
    dplyr::filter(dates >= as.Date(date.min))
  if(!is.na(date.max)) 
    covid_stats <- covid_stats %>%
      dplyr::filter(dates <= as.Date(date.max))
  return(covid_stats)
}

# tests
#tests <- get_tests('CZE', '2020-08-01')

