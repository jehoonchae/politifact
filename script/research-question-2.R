pacman::p_load(tidyverse, here, ggdist, latex2exp, ggridges, coin, rio, ineq, boot)
source("./script/helper.R")
# Import Barbera score

# RQ2
# Figure 3
barbera_score1 <- here("data", "barbera", "user-ideal-points-202008_000000000000.csv") |> 
  read_csv()
barbera_score2 <- here("data", "barbera", "user-ideal-points-202008_000000000001.csv") |> 
  read_csv()
barbera_score3 <- here("data", "barbera", "user-ideal-points-202008_000000000002.csv") |> 
  read_csv()
barbera_score4 <- here("data", "barbera", "user-ideal-points-202008_000000000003.csv") |> 
  read_csv()
barbera_score5 <- here("data", "barbera", "user-ideal-points-202008_000000000004.csv") |> 
  read_csv()
barbera_score6 <- here("data", "barbera", "user-ideal-points-202008_000000000005.csv") |> 
  read_csv()
barbera_score7 <- here("data", "barbera", "user-ideal-points-202008_000000000006.csv") |> 
  read_csv()

barbera_score <- bind_rows(
  barbera_score1, barbera_score2, barbera_score3,
  barbera_score4, barbera_score5, barbera_score6, barbera_score7
)

rm(barbera_score1, barbera_score2, barbera_score3, barbera_score4,
   barbera_score5, barbera_score6, barbera_score7)

# barbera_score$theta |> sd()
# Import the retweeter list of each politifact tweet post
# and left join it with barbera score 
politifact_retweet_merged <- read_csv("./data/data-processed/retweeter_unique.csv") |> 
  distinct(retweeter_id, .keep_all = TRUE) |> 
  left_join(barbera_score, by = c("retweeter_id" = "id_str")) |> 
  drop_na(theta)

# politifact_retweet_merged |> 
#   summarise(m = mean(theta), sd = sd(theta), n = n())

# ttest_tab2 <- barbera_score |> sample_n(100000)
# t.test(politifact_retweet_merged$theta, barbera_score$theta, var.equal = FALSE)

# Figure: Distribution of general Twitter users 
# and Politifact retweets' ideology scores
fig3 <- barbera_score |> 
  sample_n(100000) |> 
  ggplot(aes(theta)) +
  scale_x_continuous(limits = c(-3, 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.2)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", linewidth = .3) +
  annotate("text", x = 0.2, y = 1.1, size = 5, hjust = 0, family = "Roboto Condensed",
           label = "More conservative", colour = "black") +
  geom_segment(aes(x = 0.2, y = 1, xend = 2, yend = 1),
               arrow = arrow(length = unit(0.2, "cm"), type = 'closed')) +
  annotate("text", x = -0.2, y = 1.1, size = 5, hjust = 1, family = "Roboto Condensed",
           label = "More liberal", colour = "black") +
  geom_segment(aes(x = -0.2, y = 1, xend = -2, yend = 1),
               arrow = arrow(length = unit(0.2, "cm"), type = 'closed')) +
  stat_slab(fill = "#1DA1F2", alpha = .8) +
  # geom_rug(colour = "#1DA1F2", alpha = .01) +
  stat_slab(data = politifact_retweet_merged |> drop_na(theta), 
            aes(theta), fill = "#ff5311", alpha = .8) +
  # geom_rug(data = politifact_retweet_merged |> drop_na(theta), 
  #          aes(theta), colour = "#ff5311", alpha = .01) +
  annotate("text", x = .4, y = 0.6, size = 5, hjust = 0, family = "Roboto Condensed",
           label = "General Twitter users", colour = "#1DA1F2") +
  annotate("text", x = .4, y = 0.5, size = 5, hjust = 0, family = "Roboto Condensed",
           label = "Users shared fact-check", colour = "#ff5311") +
  labs(x = TeX("Estimated ideal points of users $\\hat{\\theta}$"), y = NULL) +
  theme_classic(base_size = 20, base_family = "Fira Sans Condensed") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = .3), 
        axis.line.y.left = element_blank(),
        axis.line.x.bottom = element_line(colour = "black", linewidth = .3),
        axis.text = element_text(colour = 'black', size = rel(.8)),
        axis.title = element_text(colour = 'black', size = rel(.8))
  )

ggsave(plot = fig3, device = cairo_pdf, filename = "./fig/fig3.pdf", 
       width = 8, height = 4)

# Import all the politifact teets
politifact_tweet <- import("./data/data-processed/politifact_tweet_combined.csv", setclass = "tibble") |> 
  janitor::clean_names() |> 
  filter(created_at >= as_date("2016-01-01") & created_at <= as_date("2022-02-01")) |> 
  mutate(id = as.character(id), author_id = as.character(author_id))

politifact_merged <- politifact_tweet |> 
  filter(has_url == 1) |> 
  drop_na(first_url_decoded) |> 
  left_join(politifact_article,
            by = c("first_url_decoded" = "url"))

politifact_merged_filtered <- politifact_merged |> 
  filter(first_url_decoded %in% politifact_article$url) |> 
  filter(is_political != "Not political") |> 
  arrange(created_at) |> 
  rename(tweet_id = id)


tweet_retweet <- import("./data/data-processed/tweet_retweet_merged.csv", setclass = "tibble")

total_merged <- tweet_retweet |> 
  mutate(tweet_id = as.character(tweet_id), retweeter_id = as.character(retweeter_id)) |> 
  mutate(tweet_id = as.double(tweet_id), retweeter_id = as.double(retweeter_id)) |> 
  left_join(politifact_merged_filtered |> mutate(tweet_id = as.double(tweet_id)), by = "tweet_id") |> 
  # mutate(retweeter_id = as.double(retweeter_id)) |> 
  left_join(barbera_score |> distinct(id_str, .keep_all = TRUE), by = c("retweeter_id" = "id_str")) |> 
  drop_na(created_at, theta) 

tab <- total_merged |> 
  group_by(tweet_id) |> 
  summarise(mean_ideology = mean(theta))

# tab |> filter(mean_ideology > 0.5)
# tab |> 
#   ggplot(aes(mean_ideology)) +
#   geom_histogram() +
#   geom_vline(xintercept = 0)

# saveRDS(tab, file = "./data/data-processed/average_ideology_score_by_tweet.rds")

tab_fig4 <- politifact_merged_filtered |> 
  mutate(tweet_id = as.double(tweet_id)) |> 
  left_join(tab, by = "tweet_id") |> 
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
  drop_na(mean_ideology) |> 
  filter(political_affiliation != "Affiliation unknown") |> 
  mutate(adjudication = case_when(
    adjudication == "pants-fire" ~ "Pants on Fire",
    adjudication == "false" ~ "False",
    adjudication == "barely-true" ~ "Mostly False",
    adjudication == "half-true" ~ "Half True",
    adjudication == "mostly-true" ~ "Mostly True",
    adjudication == "true" ~ "True"
  )) |> 
  mutate(adjudication = factor(adjudication, 
                               levels = c("True", 
                                          "Mostly True", 
                                          "Half True",
                                          "Mostly False", 
                                          "False",
                                          "Pants on Fire"))) |> 
  mutate(political_affiliation = case_when(political_affiliation == "Conservative/Republican" ~ "Target: Conservative/Republican",
                                           political_affiliation == "Liberal/Democrat" ~ "Target: Liberal/Democrat"))

fig4 <- tab_fig4 |> 
  drop_na(congruency) |> 
  ggplot(aes(mean_ideology, group = congruency)) +
  # stat_halfeye(aes(fill = congruency), alpha = .8) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", linewidth = .5) +
  annotate("text", x = 0.2, y = 1.05, size = 5, hjust = 0, family = "Roboto Condensed",
           label = "More conservative", colour = "black") +
  geom_segment(aes(x = 0.2, y = .95, xend = 1.5, yend = .95),
               arrow = arrow(length = unit(0.2, "cm"), type = 'closed')) +
  annotate("text", x = -0.2, y = 1.05, size = 5, hjust = 1, family = "Roboto Condensed",
           label = "More liberal", colour = "black") +
  geom_segment(aes(x = -0.2, y = .95, xend = -1.5, yend = .95),
               arrow = arrow(length = unit(0.2, "cm"), type = 'closed')) +
  annotate("text", x = .4, y = 0.3, size = 5, hjust = 0, family = "Roboto Condensed",
           label = "Liberal/Democrat congruent", colour = "#0015BC") +
  annotate("text", x = .4, y = 0.2, size = 5, hjust = 0, family = "Roboto Condensed",
           label = "Conservative/Republican congruent", colour = "#FF0000") +
  stat_slab(aes(fill = congruency), alpha = .8) +
  scale_fill_manual(values = c("#FF0000", "#0015BC")) +
  scale_x_continuous(limits = c(-1.5, 3.5)) + 
  scale_y_continuous(expand = c(0, 0), limit = c(0, 1.1)) +
  labs(x = "Mean ideology distribution of sharers", y = NULL) +
  theme_classic(base_size = 20, base_family = "Roboto Condensed") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = .3), 
        axis.line.y.left = element_blank(),
        axis.line.x.bottom = element_line(colour = "black", linewidth = .3),
        axis.text = element_text(colour = 'black', size = rel(.8)),
        axis.title = element_text(colour = 'black', size = rel(.8)),
        legend.position = "none"
  )

ggsave(plot = fig4, device = cairo_pdf, filename = "./fig/fig4.pdf", 
       width = 8, height = 4)



mean_diff_tab <- tab_fig4 |> drop_na(congruency)
mean_diff_tab |> 
  group_by(congruency) |> 
  summarise(mean = mean(mean_ideology),
            sd = sd(mean_ideology))
# t.test(mean_diff_tab$mean_ideology[mean_diff_tab$congruency == 'Conservative/Republican congruent'],
#        mean_diff_tab$mean_ideology[mean_diff_tab$congruency == 'Liberal/Democrat congruent'])
with(mean_diff_tab,
     oneway_test(
       mean_ideology ~ as.factor(congruency),
       distribution = approximate(nresample = 9999)
     ))

aov(mean_ideology ~ as.factor(adjudication),
      tab_fig4 |> filter(political_affiliation == "Target: Conservative/Republican")) |>
  summary()

aov(mean_ideology ~ as.factor(adjudication),
    tab_fig4 |> filter(political_affiliation == "Target: Liberal/Democrat")) |>
  summary()

tab_fig4 |> filter(political_affiliation == "Target: Conservative/Republican")
####

tmp <- politifact_retweet_merged |> drop_na(theta)
cons_sample <- tmp |> filter(theta >= 0.5)
lib_sample <- tmp |> filter(theta <= -0.5)

post_sample <- total_merged |> 
  filter(created_at >= as_date("2016-01-01") & created_at <= as_date("2021-12-31")) |> 
  filter(theta >= 0.5 | theta <= -0.5) |> 
  filter(is_political != "Not political") |>
  mutate(month = lubridate::floor_date(created_at, "1 month")) |> 
  mutate(ideology = if_else(theta >= 0.5, "Conservative", "Liberal")) |> 
  mutate(congruency = case_when(
    political_affiliation == "Liberal/Democrat" & adjudication == "true" ~ "Liberal/Democrat congruent",
    political_affiliation == "Liberal/Democrat" & adjudication == "mostly-true" ~ "Liberal/Democrat congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "false" ~ "Liberal/Democrat congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "pants-fire" ~ "Liberal/Democrat congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "true" ~ "Conservative/Republican congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "mostly-true" ~ "Conservative/Republican congruent",
    political_affiliation == "Liberal/Democrat" & adjudication == "false" ~ "Conservative/Republican congruent",
    political_affiliation == "Liberal/Democrat" & adjudication == "pants-fire" ~ "Conservative/Republican congruent"
  ))

# post_sample |> 
#   group_by(month, ideology) |> 
#   count()

tab <- post_sample |> 
  drop_na(congruency) |> 
  mutate(month = as_date(month)) |> 
  group_by(month, ideology, congruency) |> 
  count() |> 
  filter((ideology == "Conservative" & congruency == "Conservative/Republican congruent")|
           (ideology == "Liberal" & congruency == "Liberal/Democrat congruent"))

post_sample |> 
  drop_na(congruency) |> 
  mutate(month = as_date(month)) |> 
  group_by(congruency) |> 
  count()

vec_conservative <- tab |> 
  filter(congruency == "Conservative/Republican congruent") |> 
  arrange(desc(n)) |> 
  pull(n)

vec_liberal <- tab |> 
  filter(congruency == "Liberal/Democrat congruent") |> 
  arrange(desc(n)) |> 
  pull(n)

fig5 <- tab |> 
  ungroup() |> 
  mutate(normalising_const = if_else(ideology == "Conservative", vec_conservative[1], vec_liberal[1])) |> 
  mutate(normalised_n = n / normalising_const) |> 
  ggplot(aes(month, normalised_n)) +
  geom_hline(yintercept = 0, linewidth = .3, colour = "gray30") +
  # geom_point(aes(group = congruency, colour = congruency), size = 2) +
  geom_line(aes(group = congruency, colour = congruency), size = 1) +
  geom_vline(xintercept = as.numeric(as_date("2016-11-08")), size = .5, linetype = 2,
             colour = 'gray30', alpha = .5, linewidth = .35) +
  annotate("text", x = as_date("2016-11-08"), y = .8, family = 'Roboto Condensed', size = 5, alpha = .7,
           hjust = -.05, vjust = 0, label = "2016\nPresidential\nElection", fontface = "bold",
           colour = 'black') +
  geom_vline(xintercept = as.numeric(as_date("2018-11-06")), size = .5, linetype = 2,
             colour = 'gray30', alpha = .5, linewidth = .35) +
  annotate("text", x = as_date("2018-11-06"), y = .8, family = 'Roboto Condensed', size = 5, alpha = .7,
           hjust = -.05, vjust = 0, label = "2018\nMidterm\nElection", fontface = "bold",
           colour = 'black') +
  geom_vline(xintercept = as.numeric(as_date("2020-11-03")), size = .5, linetype = 2,
             colour = 'gray30', alpha = .5, linewidth = .35) +
  annotate("text", x = as_date("2020-11-03"), y = .8, family = 'Roboto Condensed', size = 5, alpha = .7,
           hjust = -.05, vjust = 0, label = "2020\nPresidential\nElection", fontface = "bold",
           colour = 'black') +
  # geom_vline(xintercept = as.numeric(as_date("2022-11-08")), size = .5, linetype = 2,
  #            colour = 'gray30', alpha = .5, linewidth = .35) +
  # annotate("text", x = as_date("2022-11-08"), y = .8, family = 'Roboto Condensed', size = 5, alpha = .7,
  #          hjust = -.05, vjust = 0, label = "2022\nMidterm\nElection", fontface = "bold",
  #          colour = 'black') +
  scale_x_date(date_breaks = '1 years', date_labels = '%Y', expand = c(0, 0)) +
  labs(x = "Time (by month)", y = "Partisan cheerleading share\n(Normalized with the maximum count month)",
       colour = NULL) +
  scale_colour_manual(breaks = c("Liberal/Democrat congruent", "Conservative/Republican congruent"),
                      values = c("#0015BC", "#FF0000")) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 20, base_family = "Roboto Condensed") +
  theme(axis.ticks = element_line(colour = "black", size = .3),
        # axis.line.y = element_blank(),
        legend.position = "top",
        axis.line = element_line(colour = "black", linewidth = .3),
        axis.text = element_text(colour = 'black', size = rel(.8)),
        axis.title = element_text(colour = 'black', size = rel(.8)),
        strip.text = element_text(colour = 'black', size = rel(.8)))

ggsave(plot = fig5, device = cairo_pdf, filename = "./fig/fig5.pdf", 
       width = 10, height = 6)


merged_fig2 <- ggarrange(ggarrange(fig3, fig4, labels = c("A", "B"), 
                                   ncol = 2, font.label = list(color = "black", size = 20)),
                         fig5, nrow = 2, labels = c(NA, "C"), 
                         font.label = list(color = "black", size = 20))

ggsave(plot = merged_fig2, 
       device = cairo_pdf, filename = "./fig/merged_fig2.pdf", 
       width = 10, height = 9)


