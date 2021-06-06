
# import
library(EpiEstim)
library(plotly)
source("src.R")

get.serial <- function(lim.max = 20, serial.pars = c(1.901,0.41781)) {
  y.discrete <- pgamma(1:(lim.max+1),serial.pars[1],serial.pars[2]) -
    pgamma(0:lim.max,serial.pars[1],serial.pars[2])
  y.discrete[1] <- 0
  y.discrete <- y.discrete / sum(y.discrete)
  return(y.discrete)
}

estimate_reproduction <- function(incid, date.min) {
  res <- estimate_R(incid = incid, method = "non_parametric_si",
                    config = make_config(list(si_distr = get.serial())))
  R <- data.frame(
    Date = res$dates[res$R$`t_start`],
    R0 = res$R$`Mean(R)`,
    lower95 = res$R$`Quantile.0.025(R)`,
    upper95 = res$R$`Quantile.0.975(R)`) %>%
    dplyr::filter(Date >= date.min) %>%
    dplyr::mutate(Variable = 'R0')
}

I_R0 <- function(country, date.min, date.max=NA) {
  # get data
  covid_stats <- get_incidence(country, date.min, date.max)
  # estimate R0
  R <- estimate_reproduction(covid_stats, date.min)
  R
}

plot.I_R0 <- function(country, date.min, date.max=NA) {
  # get data
  tests <- get_tests(country, date.min)
  R <- I_R0(country, date.min)
  # plot
  dt.min <- min(R$Date)
  dt.max <- max(R$Date)
  plot_ly() %>%
    add_lines(x = ~Date, y = ~R0, color = ~Variable, data = R,
              opacity = 1) %>%
    add_lines(x = ~Date, y = ~Tests, color = ~Variable, data = tests,
              yaxis = "y2", opacity = .25) %>%
    add_segments(x = ~Date, xend = ~Date, y = ~lower95, yend = ~upper95,
                 data = R, opacity = .4, name = '95% CI') %>%
    add_segments(x = dt.min, xend = dt.max, y = 1, yend = 1,
                 opacity = .4, name = 'R0 = 1') %>%
    layout(yaxis2 = list(overlaying = "y", side = "right"))
}

plot.I_R0('CZE', '2020-03-10')
plot.I_R0('POL', '2020-03-10')
#plot.I_R0('SWE', '2020-03-10') # not working fully yet :(
plot.I_R0('ITA', '2020-03-10')
