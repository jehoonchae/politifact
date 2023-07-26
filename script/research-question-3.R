pacman:::p_load(tidyverse, ggpubr)



# Fig 6: Lorenz curve
# RQ3: User characteristic

total_merged |> 
  distinct(retweeter_id, .keep_all = TRUE) |> 
  filter(theta >= 0.5) |> 
  nrow()

total_merged |> 
  distinct(retweeter_id, .keep_all = TRUE) |> 
  filter(theta <= -0.5) |> 
  nrow()


total_merged |> 
  filter(created_at >= as_date("2016-01-01") & created_at <= as_date("2021-12-31")) |> 
  filter(theta >= 0.5 | theta <= -0.5) |> 
  filter(is_political != "Not political") |>
  mutate(month = lubridate::floor_date(created_at, "1 month")) |> 
  mutate(ideology = if_else(theta >= 0.5, "Conservative", "Liberal"))

total_retweet_num_conservative <- total_merged |> 
  filter(created_at >= as_date("2016-01-01") & created_at <= as_date("2021-12-31")) |> 
  filter(theta >= 0.5 | theta <= -0.5) |> 
  filter(is_political != "Not political") |>
  mutate(month = lubridate::floor_date(created_at, "1 month")) |> 
  mutate(ideology = if_else(theta >= 0.5, "Conservative", "Liberal")) |> 
  filter(ideology == "Conservative") |> 
  distinct(retweeter_id) |> 
  nrow()

total_retweet_num_liberal <- total_merged |> 
  filter(created_at >= as_date("2016-01-01") & created_at <= as_date("2021-12-31")) |> 
  filter(theta >= 0.5 | theta <= -0.5) |> 
  filter(is_political != "Not political") |>
  mutate(month = lubridate::floor_date(created_at, "1 month")) |> 
  mutate(ideology = if_else(theta >= 0.5, "Conservative", "Liberal")) |> 
  filter(ideology == "Liberal") |> 
  distinct(retweeter_id) |> 
  nrow()

lorenz_data_conservative <- total_merged |> 
  filter(created_at >= as_date("2016-01-01") & created_at <= as_date("2021-12-31")) |> 
  filter(theta >= 0.5 | theta <= -0.5) |> 
  filter(is_political != "Not political") |>
  mutate(month = lubridate::floor_date(created_at, "1 month")) |> 
  mutate(ideology = if_else(theta >= 0.5, "Conservative", "Liberal")) |> 
  filter(ideology == "Conservative") |> 
  group_by(retweeter_id) |> 
  count() |>
  ungroup() |> 
  arrange(n) |>
  mutate(cum_retweet = cumsum(n),
         total_retweet = sum(n),
         cum_retweet_pct = cum_retweet / total_retweet,
         cum_retweeter_pct = (row_number() - 1) / (total_retweet_num_conservative - 1))

lorenz_data_liberal <- total_merged |> 
  filter(created_at >= as_date("2016-01-01") & created_at <= as_date("2021-12-31")) |> 
  filter(theta >= 0.5 | theta <= -0.5) |> 
  filter(is_political != "Not political") |>
  mutate(month = lubridate::floor_date(created_at, "1 month")) |> 
  mutate(ideology = if_else(theta >= 0.5, "Conservative", "Liberal")) |> 
  filter(ideology == "Liberal") |> 
  group_by(retweeter_id) |> 
  count() |>
  ungroup() |> 
  arrange(n) |>
  mutate(cum_retweet = cumsum(n),
         total_retweet = sum(n),
         cum_retweet_pct = cum_retweet / total_retweet,
         cum_retweeter_pct = (row_number() - 1) / (total_retweet_num_liberal - 1))

tab <- bind_rows(
  bind_cols(ideology = "Liberal", lorenz_data_liberal),
  bind_cols(ideology = "Conservative", lorenz_data_conservative)
)

fig6 <- tab |> 
  ggplot(aes(x = cum_retweeter_pct, y = cum_retweet_pct, group = ideology)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_line(aes(colour = ideology), size = 3) +
  annotate("text", label = "Conservative leaners", x = 0, y = 0.95, size = 6,
           colour = "#FF0000", hjust = 0, family = "Roboto Condensed", fontface = "bold") +
  annotate("text", label = "Liberal leaners", x = 0, y = 0.85, size = 6,
           colour = "#0015BC", hjust = 0, family = "Roboto Condensed", fontface = "bold") +
  labs(x = "Cumulative percentage of users",
       y = "Cumulative percentage of retweeting") +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(scale = 100)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(scale = 100)) +
  scale_colour_manual(values = c("#FF0000", "#0015BC")) +
  theme_bw(base_size = 20, base_family = "Roboto Condensed") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = .3),
        axis.ticks = element_line(colour = "black", size = .3),
        axis.line = element_line(colour = "black", linewidth = .3),
        axis.text = element_text(colour = 'black', size = rel(.8)),
        axis.title = element_text(colour = 'black', size = rel(.8)))

ggsave(plot = fig6, device = cairo_pdf, filename = "./fig/fig6.pdf", 
       width = 6, height = 6)

# Assuming you have a dataframe 'df' with a numeric variable 'income' and a factor 'group'


# Function to calculate Gini coefficient
gini_fn <- function(data, indices) {
  d <- data[indices, ] # allows boot to select sample
  return(ineq::Gini(d$n))
}


# Bootstrapping for each group
bootstrap_group1 <- boot::boot(data = tab[tab$ideology == "Liberal", ], statistic = gini_fn, R = 1000)
bootstrap_group2 <- boot::boot(data = tab[tab$ideology == "Conservative", ], statistic = gini_fn, R = 1000)

# Get 95% confidence intervals
ci_group1 <- boot::boot.ci(bootstrap_group1, type = "perc")
ci_group2 <- boot::boot.ci(bootstrap_group2, type = "perc")

ineq::Gini(tab[tab$ideology == "Liberal", ]$n)
print(ci_group1)
ineq::Gini(tab[tab$ideology == "Conservative", ]$n)
print(ci_group2)

# Fig 7: partisan selective sharing
total_merged <- total_merged |> 
  mutate(congruency = case_when(
    political_affiliation == "Liberal/Democrat" & adjudication == "true" ~ "Liberal/Democrat congruent",
    political_affiliation == "Liberal/Democrat" & adjudication == "mostly-true" ~ "Liberal/Democrat congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "false" ~ "Liberal/Democrat congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "pants-fire" ~ "Liberal/Democrat congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "true" ~ "Conservative/Republican congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "mostly-true" ~ "Conservative/Republican congruent",
    political_affiliation == "Liberal/Democrat" & adjudication == "false" ~ "Conservative/Republican congruent",
    political_affiliation == "Liberal/Democrat" & adjudication == "pants-fire" ~ "Conservative/Republican congruent"
  )) |> 
  mutate(inparty_congruency = case_when(
    congruency == "Liberal/Democrat congruent" & theta < 0 ~ 1,
    congruency == "Conservative/Republican congruent" & theta > 0 ~ 1,
    TRUE ~ 0
  ))

# total_merged |> 
#   filter(inparty_congruency == 1) |> 
#   group_by(retweeter_id) |> 
#   count() |> 
#   left_join(tmp2, by = "retweeter_id") |> 
#   ggplot(aes(theta, n)) +
#   geom_jitter(alpha = .1) +
#   coord_trans(y = 'log10') +
#   labs(x = "Ideal point of user", y = "Number of retweet") +
#   theme_bw(base_size = 20, base_family = "Roboto Condensed") +
#   theme(legend.position = "none",
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_line(size = .3),
#         axis.ticks = element_line(colour = "black", size = .3),
#         axis.line = element_line(colour = "black", linewidth = .3),
#         axis.text = element_text(colour = 'black', size = rel(.8)),
#         axis.title = element_text(colour = 'black', size = rel(.8)))

tmp1 <- total_merged |> 
  group_by(retweeter_id) |> 
  count() |> 
  ungroup()

tmp2 <- total_merged |> 
  select(retweeter_id, theta) |> 
  distinct(retweeter_id, .keep_all = TRUE)

tmp3 <- tmp1 |> left_join(tmp2, by = "retweeter_id")

# tmp3 |> 
#   filter(n > 3) |> 
#   ggplot(aes(theta, n)) +
#   geom_jitter(alpha = .1) +
#   coord_trans(y = 'log10') +
#   labs(x = "Ideal point of user", y = "Number of retweet") +
#   theme_bw(base_size = 20, base_family = "Roboto Condensed") +
#   theme(legend.position = "none",
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_line(size = .3),
#         axis.ticks = element_line(colour = "black", size = .3),
#         axis.line = element_line(colour = "black", linewidth = .3),
#         axis.text = element_text(colour = 'black', size = rel(.8)),
#         axis.title = element_text(colour = 'black', size = rel(.8)))

tab_congruency1 <- total_merged |> 
  group_by(retweeter_id) |> 
  summarise(total_share = n())

tab_congruency2 <- total_merged |> 
  filter(inparty_congruency == 1) |> 
  group_by(retweeter_id) |> 
  summarise(congruent_share = n())

tab_congruency3 <- tab_congruency1 |> 
  left_join(tab_congruency2, by = "retweeter_id") |> 
  mutate(congruent_share = replace_na(congruent_share, 0)) |> 
  mutate(selective_share = congruent_share/total_share) |> 
  left_join(tmp2, by = "retweeter_id") |> 
  mutate(theta_abs = abs(theta)) |> 
  mutate(ideology = case_when(theta <= -.5 ~ "Liberal leaners",
                              theta > -.5 & theta < .5 ~ "Moderate",
                              theta >= .5 ~ "Conservative leaners"))

fig7 <- tab_congruency3 |>
  filter(ideology != "Moderate") |> 
  ggplot(aes(theta_abs, selective_share)) +
  geom_jitter(aes(group = ideology, fill = "grey", alpha = ideology,
                  size = log10(total_share))) +
  geom_smooth(aes(group = ideology, colour = ideology, weight = log10(total_share)), 
              method = 'lm', size = 3) +
  geom_smooth(aes(group = ideology, colour = ideology, weight = log10(total_share)), 
              method = 'loess', size = 3, se = FALSE) +
  scale_colour_manual(values = c("#FF0000", "#0015BC")) +
  scale_alpha_manual(values = c(0.1, 0.01)) +
  labs(x = TeX("Ideology strength of user, $|\\hat{\\theta}|$"),
       y = "Proportion of in-party congruent sharing") +
  facet_grid(.~ ideology, scales = "free_x") +
  theme_bw(base_size = 20, base_family = "Roboto Condensed") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = .3),
        axis.ticks = element_line(colour = "black", size = .3),
        axis.line = element_line(colour = "black", linewidth = .3),
        axis.text = element_text(colour = 'black', size = rel(.8)),
        axis.title = element_text(colour = 'black', size = rel(.8)))

ggsave(plot = fig7, device = cairo_pdf, filename = "./fig/fig7.pdf", 
       width = 10, height = 6)

tab_congruency3 |>
  filter(ideology == 'Conservative leaners') |> 
  lm(formula = selective_share ~ theta_abs) |> 
  broom::tidy(conf.int = TRUE)

tab_congruency3 |>
  filter(ideology == 'Liberal leaners') |> 
  lm(formula = selective_share ~ theta_abs) |> 
  broom::tidy(conf.int = TRUE)


# merged_fig2 <- ((fig3 + fig6) / fig4 / fig5) + plot_annotation(tag_levels = 'A')
# ggsave(plot = merged_fig2, device = cairo_pdf, filename = "./fig/merged_fig2.pdf", 
#        width = 10, height = 14)
# 
# merged_fig2_horizontal <- (fig3 + fig4 + fig6 + fig5) + 
#   plot_annotation(tag_levels = 'A') +
#   plot_layout(widths = c(1, 2.5), ncol = 2)

# merged_fig2_horizontal <- ggarrange(fig3, fig4, fig5,
#                                     labels = c("A", "B", "C"),
#                                     ncol = 2, nrow = 2, 
#                                     font.label = list(color = "black", size = 20), 
#                                     widths = c(1, 2.5))
# 
# ggsave(plot = merged_fig2_horizontal, 
#        device = cairo_pdf, filename = "./fig/merged_fig2_horizontal.pdf", 
#        width = 15, height = 8.5)

merged_fig3 <- ggarrange(fig6, fig7, labels = c("A", "B"), 
                         nrow = 2, font.label = list(color = "black", size = 20))

ggsave(plot = merged_fig3, 
       device = cairo_pdf, filename = "./fig/merged_fig3.pdf", 
       width = 8, height = 9)
