pacman::p_load(rio, tidyverse, ggpubr)
source("./script/helper.R")

politifact_article <- import('./data/data-raw/politifact/politifact.csv', setclass = "tibble") |> 
  janitor::clean_names() |> 
  filter(!(adjudication %in% c("full-flop", "half-flip", "no-flip"))) |> 
  filter(date >= as_date("2016-01-01") & date <= as_date("2021-12-31"))

politifact_article <- politifact_article |> 
  mutate(domain_target = str_extract(url, "(?<=\\/factchecks\\/\\d{4}\\/\\w{3}\\/\\d{1,2}\\/)[^\\/]+")) |> 
  mutate(domain_target = str_replace_all(domain_target, "-", " "))

# Import manual coded entity affiliation 
entities_manual_coding <- bind_rows(import('./data/entities_manual_coding_before2019.csv'),
                                    import("./data/entities_manual_coding.csv"))

# Left join to the politifact fact-checking news
politifact_article <- politifact_article |> 
  left_join(entities_manual_coding, by = "domain_target")

# How many fact-checking news for anlaysis? By political affiliation
politifact_article |> 
  filter(is_political != "Not political") |> 
  group_by(political_affiliation) |> 
  count()

# How many fact-checking news for anlaysis? By adjudication
politifact_article |> 
  filter(is_political != "Not political") |> 
  group_by(adjudication) |> 
  count()

# Figure: proportion of party leaning of source of claim by adjudication
fig1 <- politifact_article |> 
  mutate(adjudication = case_when(
    adjudication == "pants-fire" ~ "Pants on Fire\n(n = 851)",
    adjudication == "false" ~ "False\n(n = 1,530)",
    adjudication == "barely-true" ~ "Mostly False\n(n = 1,238)",
    adjudication == "half-true" ~ "Half True\n(n = 1,142)",
    adjudication == "mostly-true" ~ "Mostly True\n(n = 1,077)",
    adjudication == "true" ~ "True\n(n = 620)"
  )) |> 
  mutate(adjudication = factor(adjudication, 
                               levels = c("True\n(n = 620)", 
                                          "Mostly True\n(n = 1,077)", 
                                          "Half True\n(n = 1,142)",
                                          "Mostly False\n(n = 1,238)", 
                                          "False\n(n = 1,530)",
                                          "Pants on Fire\n(n = 851)"))) |> 
  filter(is_political != "Not political") |> 
  group_by(political_affiliation, adjudication) |> 
  count() |> 
  ggplot(aes(adjudication, n)) +
  geom_col(aes(fill = political_affiliation), linewidth = .35, colour = "black", position = "fill") +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_manual(breaks = c("Liberal/Democrat", "Conservative/Republican", "Affiliation unknown"),
                    values = c("#0015BC", "#FF0000", "grey50")) +
  scale_y_continuous(labels = scales::percent_format(scale = 100),
                     expand = c(.01, .01)) +
  coord_flip() +
  theme_jhchae(fontsize = 20, font = "Roboto Condensed")

ggsave(plot = fig1, device = cairo_pdf, filename = "./fig/fig1.pdf", 
       width = 10, height = 6)


fig2 <- politifact_article |> 
  filter(is_political != "Not political") |>
  mutate(congruency = case_when(
    political_affiliation == "Liberal/Democrat" & adjudication == "true" ~ "Liberal/Democrat congruent",
    political_affiliation == "Liberal/Democrat" & adjudication == "mostly-true" ~ "Liberal/Democrat congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "barely-true" ~ "Liberal/Democrat congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "false" ~ "Liberal/Democrat congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "pants-fire" ~ "Liberal/Democrat congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "true" ~ "Conservative/Republican congruent",
    political_affiliation == "Conservative/Republican" & adjudication == "mostly-true" ~ "Conservative/Republican congruent",
    political_affiliation == "Liberal/Democrat" & adjudication == "barely-true" ~ "Conservative/Republican congruent",
    political_affiliation == "Liberal/Democrat" & adjudication == "false" ~ "Conservative/Republican congruent",
    political_affiliation == "Liberal/Democrat" & adjudication == "pants-fire" ~ "Conservative/Republican congruent"
  )) |> 
  drop_na(congruency) |> 
  mutate(month = lubridate::floor_date(date, "1 month")) |> 
  group_by(congruency, month) |> 
  count() |> 
  ggplot(aes(month, n)) +
  geom_line(size = 1, aes(group = congruency,
                          colour = congruency)) +
  # scale_y_continuous(limits = c(0, 100)) +
  geom_hline(yintercept = 0, linewidth = .3, colour = "gray30") +
  # annotate("text", x = as_date("2016-03-01"), y = 95, hjust = 0, size = 7,
  #          family = 'Roboto Condensed',
  #          label = "Liberal/Democrat congruent fact-checking", 
  #          colour = "#0015BC", fontface = "bold") +
  # annotate("text", x = as_date("2016-03-01"), y = 88, hjust = 0, size = 7,
  #          family = 'Roboto Condensed',
  #          label = "Conservative/Republican congruent fact-checking", 
  #          colour = "#FF0000", fontface = "bold") +
  geom_vline(xintercept = as.numeric(as_date("2016-11-08")), size = .5, linetype = 2, 
             colour = 'gray30', alpha = .5, linewidth = .35) +
  annotate("text", x = as_date("2016-11-08"), y = 80, family = 'Roboto Condensed', size = 5, alpha = .7,
           hjust = -.05, vjust = 0, label = "2016\nPresidential\nElection", fontface = "bold",
           colour = 'black') +
  geom_vline(xintercept = as.numeric(as_date("2018-11-06")), size = .5, linetype = 2,
             colour = 'gray30', alpha = .5, linewidth = .35) +
  annotate("text", x = as_date("2018-11-06"), y = 80, family = 'Roboto Condensed', size = 5, alpha = .7,
           hjust = -.05, vjust = 0, label = "2018\nMidterm\nElection", fontface = "bold",
           colour = 'black') +
  geom_vline(xintercept = as.numeric(as_date("2020-11-03")), size = .5, linetype = 2,
             colour = 'gray30', alpha = .5, linewidth = .35) +
  annotate("text", x = as_date("2020-11-03"), y = 80, family = 'Roboto Condensed', size = 5, alpha = .7,
           hjust = -.05, vjust = 0, label = "2020\nPresidential\nElection", fontface = "bold",
           colour = 'black') +
  scale_x_date(date_breaks = '1 years', date_labels = '%Y', expand = c(0, 0)) +
  labs(x = "Time (by month)", y = "Number of fact-checking cases", 
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

ggsave(plot = fig2, device = cairo_pdf, filename = "./fig/fig2.pdf", 
       width = 10, height = 6)

# library(patchwork)
# merged_fig1 <- fig1 / fig2 + plot_annotation(tag_levels = 'A')
merged_fig1 <- ggarrange(fig1, fig2,
                         labels = c("A", "B"),
                         ncol = 1, nrow = 2, 
                         font.label = list(color = "black", size = 20))
ggsave(plot = merged_fig1, device = cairo_pdf, filename = "./fig/merged_fig1.pdf", 
       width = 10, height = 11)
