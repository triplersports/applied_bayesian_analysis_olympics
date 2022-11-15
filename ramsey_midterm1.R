packages <- c("tidyverse", "gt", "xlsx", "ggimage", "countrycode")

install.packages(setdiff(packages, rownames(installed.packages())))

library(tidyverse)
library(gt)
library(xlsx)
library(ggimage)
library(countrycode)

setwd("")

olympics_data <- read.csv(file = "Medals.csv")
colnames(olympics_data) <- tolower(colnames(olympics_data))

# add flags unicode
#olympics_data$flag_unicode <- countrycode(olympics_data$host.country, "country.name", "unicode.symbol")

########################################################################################################################
# summarizing data
########################################################################################################################

olympics_data %>%
  gt(rowname_col = "host.country") %>%
  fmt_number(columns = contains("year"), decimals = 0, use_seps = FALSE) %>%
  fmt_number(columns = contains(c("during")), decimals = 0) %>%
  grand_summary_rows(columns = contains("during"), fns = list(Aggregate = "sum"), missing_text = "", formatter = fmt_number, decimals = 0) %>%
  cols_move(columns = c(medals.won.during.host.year, participating.athletes.during.host.year), after = participating.athletes.during.previous.olympics) %>%
  tab_spanner(label = "Previous Olympics", columns = contains("previous")) %>%
  tab_spanner(label = "Host Year", columns = contains("host.year")) %>%
  cols_label(year = "Year",
             medals.won.during.previous.olympics = "Medals Won",
             medals.won.during.host.year = "Medals Won",
             participating.athletes.during.previous.olympics = "Participating Athletes",
             participating.athletes.during.host.year = "Participating Athletes") %>%
  # add outer border to table (gray)
  opt_table_outline(style = "solid", width = px(3)) %>%
  # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("right"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_body(columns = c(year, participating.athletes.during.previous.olympics)))

########################################################################################################################
# part 1
########################################################################################################################

set.seed(2024)

lambda_values <- seq(0, 1, by = 0.001)

# total medals won and participants for host countries in host year
y1 <- sum(olympics_data$medals.won.during.host.year)
n1 <- sum(olympics_data$participating.athletes.during.host.year)

# assume a likelihood based on number of medals won during host year and number of participants
likelihood1 <- (y1 / n1)

# use uninformative Gamma prior of a = b = 0.1
prior_a <- 0.1
prior_b <- prior_a

a1 <- (prior_a + y1)
b1 <- (prior_b + n1)

# total medals won and participants for host countries in previous year
y0 <- sum(olympics_data$medals.won.during.previous.olympics)
n0 <- sum(olympics_data$participating.athletes.during.previous.olympics)

a0 <- (prior_a + y0)
b0 <- (prior_b + n0)

# poisson-gamma model posterior
posterior_df <- data.frame(lambda_values, dgamma(lambda_values, shape = a1, rate = b1),
                           dgamma(lambda_values, shape = a0, rate = b0))
colnames(posterior_df) <- c("lambda", "host_year", "previous_year")

# plot both posterior distributions
posterior_df %>%
  pivot_longer(cols = c("host_year", "previous_year")) %>%
  ggplot(aes(x = lambda, y = value)) +
  geom_line(aes(color = name), size = 2) +
  scale_x_continuous(limits = c(0, 0.2)) +
  scale_color_manual(labels = c("Host Year Olympics", "Previous Olympics"), values = c("red", "blue")) +
  labs(x = "Estimated Medals Won per Participant",
       y = "Posterior Density",
       title = "Host Country Advantage in the Olympics?",
       caption = "Note the x-axis was truncated to range from 0 to 0.2",
       color = "Olympics Year") +
  theme(axis.title = element_text(size = 24),
        plot.title = element_text(size = 24),
        axis.text = element_text(size = 18),
        plot.subtitle = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18))

# credible intervals for medal rates when hosting
posterior_1_2.5 <- qgamma(0.025, shape = a1, rate = b1)
posterior_1_97.5 <- qgamma(0.975, shape = a1, rate = b1)

# credible intervals for medal rates in Olympics prior to hosting
posterior_0_2.5 <- qgamma(0.025, shape = a0, rate = b0)
posterior_0_97.5 <- qgamma(0.975, shape = a0, rate = b0)

########################################################################################################################
# part 2 - hypothesis test
########################################################################################################################

# test probability that host posterior > 97.5th CI percentile in previous posterior?
1 - pgamma(q = posterior_0_97.5, shape = a1, rate = b1)

# test probability that host posterior < 2.5th CI percentile in previous posterior?
pgamma(q = posterior_0_2.5, shape = a1, rate = b1)

# test probability that previous posterior > 97.5th CI percentile in host posterior?
1 - pgamma(q = posterior_1_97.5, shape = a0, rate = b0)

# monte carlo sampling from posterior distributions of medal per participant rates
set.seed(2024)

n_samples <- 100000

host_mc <- rgamma(n = n_samples, shape = a1, rate = b1)
previous_mc <- rgamma(n = n_samples, shape = a0, rate = b0)

# take means from monte carlo samples
mean(host_mc > previous_mc)

# are the results sensitive to the prior?
prior_a_list <- c(0.1, 0.5, seq(1, 5, by = 1), 10)
prior_b_list <- prior_a_list

mc_prior_results <- c()

for(a in 1:length(prior_a_list)) {
  a1_temp <- (prior_a_list[a] + y1)
  b1_temp <- (prior_b_list[a] + n1)
  
  a0_temp <- (prior_a_list[a] + y0)
  b0_temp <- (prior_b_list[a] + n0)
  
  host_mc_temp <- rgamma(n = n_samples, shape = a1_temp, rate = b1_temp)
  previous_mc_temp <- rgamma(n = n_samples, shape = a0_temp, rate = b0_temp)
  
  # take means from monte carlo samples
  mc_mean_temp <- mean(host_mc_temp > previous_mc_temp)
  
  # add to data frame storing test results
  mc_prior_results <- rbind(mc_prior_results, c(prior_a_list[a], prior_b_list[a], mc_mean_temp))
}

mc_prior_results <- as.data.frame(mc_prior_results)
colnames(mc_prior_results) <- c("prior_a", "prior_b", "mc_mean")
mc_prior_results <- mc_prior_results %>% mutate(prior_dist_character = paste0("Gamma(", prior_a, ", ", prior_b, ")"))

# table of priors and hypothesis test results
mc_prior_results %>%
  select(-c("prior_a", "prior_b")) %>%
  gt(rowname_col = "prior_dist_character")

########################################################################################################################
# part 3 - prediction
########################################################################################################################

olympics_data_comp <- olympics_data %>%
  mutate(host_year_previous_year_participants_ratio = participating.athletes.during.host.year / participating.athletes.during.previous.olympics) %>%
  mutate(host_year_previous_year_participants_difference = (participating.athletes.during.host.year - participating.athletes.during.previous.olympics)) %>%
  as.data.frame()

olympics_data_comp %>% ggplot(aes(x = host.country, y = participating.athletes.during.host.year, label = participating.athletes.during.host.year)) +
  geom_point(stat = "identity", size = 10, fill = "blue") +
  geom_segment(aes(y = participating.athletes.during.previous.olympics, x = host.country, yend = participating.athletes.during.host.year, xend = host.country),
               color = "blue") +
  geom_text(color = "white", size = 4) +
  labs(x = "Host Country",
       y = "Participants Difference",
       title = "Host Year Participants Compared to Previous Olympics Participants") +
  theme(axis.title = element_text(size = 24),
        plot.title = element_text(size = 24),
        axis.text = element_text(size = 18),
        plot.subtitle = element_text(size = 18)) +
  coord_flip()

#http://www.olympedia.org/countries/FRA
#france_olympic_data <- read.xlsx(file = "olympedia_france_data.xlsx", sheetIndex = 1)
# select only summer olympics data
#france_olympic_data <- france_olympic_data[grepl("Summer", france_olympic_data$Edition),]

# average host year to previous year participants ratio of countries similarly well-represented at Olympics as France
mean_host_year_previous_olympics_participant_ratio <- mean(olympics_data_comp$host_year_previous_year_participants_ratio[which(olympics_data_comp$participating.athletes.during.previous.olympics >= 200)])

france_est_participants_2024 <- 398 * mean_host_year_previous_olympics_participant_ratio

# simulate using random medals per participant rate generated from host year posterior distribution
set.seed(2024)

n_samples_france_mc <- 100000

france_mc <- rgamma(n = n_samples_france_mc, shape = a1, rate = b1)

# create a data frame from random samples
france_forecast_df <- as.data.frame(france_mc)
colnames(france_forecast_df) <- c("estimated_medals_per_participant_rate")
france_forecast_df$forecasted_medals_won <- france_forecast_df$estimated_medals_per_participant_rate * france_est_participants_2024

# plot distribution of forecasted number of medals won by France in 2024
france_forecast_df %>%
  ggplot(aes(x = forecasted_medals_won)) +
  #geom_bar() +
  #scale_x_binned() +
  geom_histogram(binwidth = 2) +
  geom_vline(xintercept = mean(france_forecast_df$forecasted_medals_won), color = "red", linetype = "dashed", size = 2) +
  labs(x = "Forecasted Medals Won", 
       y = "Number of Simulations",
       title = "Forecasted Number of Medals Won by France in 2024 Summer Olympics") +
  theme(axis.title = element_text(size = 24),
        plot.title = element_text(size = 24),
        axis.text = element_text(size = 18),
        plot.subtitle = element_text(size = 18))

# quantiles of estimated medals won for France in 2024
quantile(france_forecast_df$forecasted_medals_won, c(0.025, 0.975))

########################################################################################################################
# part 4 - country-specific analysis
########################################################################################################################

# combine data for countries that have hosted twice - Australia, Japan, and USA
olympics_data_countries_combined <- olympics_data %>%
  #filter(year < 2020) %>%
  group_by(host.country) %>%
  select(-c(year)) %>%
  summarise_all(sum) %>%
  as.data.frame()

# use uninformative Gamma prior of a = b = 0.1
prior_a <- 0.1
prior_b <- prior_a

# data frame for countries posterior summaries
country_posterior_summary <- c()

# number of monte carlo samples
set.seed(2024)
n_samples <- 100000

# estimate prior and posterior distributions for each country
for(i in 1:nrow(olympics_data_countries_combined)) {
  host_country_temp <- olympics_data_countries_combined$host.country[i]
  
  # total medals won and participants for host countries in host year
  y1_temp <- olympics_data_countries_combined$medals.won.during.host.year[i]
  n1_temp <- olympics_data_countries_combined$participating.athletes.during.host.year[i]
  
  a1_temp <- (prior_a + y1_temp)
  b1_temp <- (prior_b + n1_temp)
  
  # total medals won and participants for host countries in previous year
  y0_temp <- olympics_data_countries_combined$medals.won.during.previous.olympics[i]
  n0_temp <- olympics_data_countries_combined$participating.athletes.during.previous.olympics[i]
  
  a0_temp <- (prior_a + y0_temp)
  b0_temp <- (prior_b + n0_temp)
  
  # poisson-gamma model posterior
  posterior_df_temp <- data.frame(lambda_values, dgamma(lambda_values, shape = a1_temp, rate = b1_temp),
                             dgamma(lambda_values, shape = a0_temp, rate = b0_temp))
  colnames(posterior_df_temp) <- c("lambda", "host_year", "previous_year")
  
  #plot both posterior distributions
  p_temp <- posterior_df_temp %>%
    pivot_longer(cols = c("host_year", "previous_year")) %>%
    ggplot(aes(x = lambda, y = value)) +
    geom_line(aes(color = name), size = 2) +
    scale_x_continuous(limits = c(0, 0.6)) +
    scale_color_manual(values = c("red", "blue")) +
    labs(x = "Estimated Medals Won per Participant",
         y = "Posterior Density",
         title = paste0("Host Country Advantage in the Olympics - ", host_country_temp),
         subtitle = "Plot by Rich Ramsey") +
    theme(axis.title = element_text(size = 24),
          plot.title = element_text(size = 24),
          axis.text = element_text(size = 18),
          plot.subtitle = element_text(size = 18))

  plot(p_temp)
  
  # credible intervals for medal rates when hosting
  posterior_1_2.5_temp <- qgamma(0.025, shape = a1_temp, rate = b1_temp)
  posterior_1_97.5_temp <- qgamma(0.975, shape = a1_temp, rate = b1_temp)
  
  # credible intervals for medal rates in Olympics prior to hosting
  posterior_0_2.5_temp <- qgamma(0.025, shape = a0_temp, rate = b0_temp)
  posterior_0_97.5_temp <- qgamma(0.975, shape = a0_temp, rate = b0_temp)
  
  # hypothesis test for whether host year posterior estimate is less than previous year posterior estimate
  host_year_p_gt_previous_year_temp <- (1 - pgamma(q = posterior_0_97.5_temp, shape = a1_temp, rate = b1_temp))
  previous_year_p_gt_host_year_temp <- (1 - pgamma(q = posterior_1_97.5_temp, shape = a0_temp, rate = b0_temp))
  
  # monte carlo samples from host year and previous year posterior distributions
  host_mc_temp <- rgamma(n = n_samples, shape = a1_temp, rate = b1_temp)
  previous_mc_temp <- rgamma(n = n_samples, shape = a0_temp, rate = b0_temp)
  
  # take means from monte carlo samples
  mc_pct_host_gt_previous_temp <- mean(host_mc_temp > previous_mc_temp)
  mc_host_previous_estimated_ratio_temp <- mean(host_mc_temp / previous_mc_temp)
  
  country_posterior_summary <- rbind(country_posterior_summary, 
                                     c(host_country_temp, posterior_1_2.5_temp, posterior_1_97.5_temp,
                                       posterior_0_2.5_temp, posterior_0_97.5_temp,
                                       host_year_p_gt_previous_year_temp, previous_year_p_gt_host_year_temp,
                                       mc_pct_host_gt_previous_temp, mc_host_previous_estimated_ratio_temp))
}

# put country posterior summary into data frame and set column names
country_posterior_summary <- as.data.frame(country_posterior_summary)
country_posterior_summary <- country_posterior_summary %>% mutate_at(c(2:ncol(country_posterior_summary)), as.numeric)
colnames(country_posterior_summary) <- c("country", "host_year_posterior_ci_2.5", "host_year_posterior_ci_97.5",
                                         "previous_year_posterior_ci_2.5", "previous_year_posterior_ci_97.5",
                                         "prob_host_year_gt_previous_year", "prob_previous_year_gt_host_year",
                                         "mc_pct_host_year_gt_previous_year", "mc_host_year_previous_year_estimated_ratio")

# display summary of country posterior distributions as gt object
country_posterior_summary %>%
  select(-c("prob_host_year_gt_previous_year", "prob_previous_year_gt_host_year")) %>%
  gt(rowname_col = "country") %>%
  tab_spanner(label = "Host Year Olympics", columns = contains("host_year_posterior")) %>%
  tab_spanner(label = "Previous Olympics", columns = contains("previous_year_posterior")) %>%
  tab_spanner(label = "Comparison - Monte Carlo Samples From Country-Specific Posteriors", columns = contains("mc_")) %>%
  fmt_percent(columns = contains("pct"), decimals = 1) %>%
  fmt_number(columns = !contains("pct"), decimals = 2) %>%
  cols_label(host_year_posterior_ci_2.5 = "2.5th Percentile",
             host_year_posterior_ci_97.5 = "97.5th Percentile",
             previous_year_posterior_ci_2.5 = "2.5th Percentile",
             previous_year_posterior_ci_97.5 = "97.5th Percentile",
             mc_pct_host_year_gt_previous_year = "% Samples Host Year > Previous Olympics", 
             mc_host_year_previous_year_estimated_ratio = "Mean Ratio of Host Year to Previous Olympics") %>%
  #add outer border to table (gray)
  opt_table_outline(style = "solid", width = px(3)) %>%
  # # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("right"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_body(columns = c(host_year_posterior_ci_97.5, previous_year_posterior_ci_97.5))) %>%
  # highlight host countries where at least 90% of samples from host year posterior were greater than samples from previous Olympics posterior
  tab_style(
    style = list(cell_fill(color = "yellow")),
    locations = cells_body(
      columns = c(mc_host_year_previous_year_estimated_ratio, mc_pct_host_year_gt_previous_year),
      rows = mc_pct_host_year_gt_previous_year > 0.9
    )
  )

########################################################################################################################
# part 5 - conclusions
########################################################################################################################