# Load packages ---------------------------
library("tidyverse")
library("scales")
library("ggforce")
library("extrafont")
library("ggtext")

# Read data ---------------------------
passwords <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# Wrangle data ---------------------------
passwords <- passwords %>%
  filter(!is.na(password)) %>%
  mutate(
    chars = str_length(password),
    composition = case_when(
      str_count(password, "[[:alpha:]]") == chars ~ "alphabetic",
      str_count(password, "[[:digit:]]") == chars ~ "numeric",
      TRUE ~ "mixed"
    )
  )

# Plot data ---------------------------
passwords %>%
  ggplot(aes(x = factor(chars), y = offline_crack_sec, colour = composition)) +
  geom_sina(aes(group = factor(chars)), alpha = 0.5, scale = "count") +
  scale_y_continuous(name = "time to crack (seconds)",
                     trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", scales::math_format(10^.x))) +
  xlab("password length") +
  scale_colour_brewer(name="password type",
                      palette="Dark2",guide=F) +
  labs(title = "Analysis of top 500 passwords",
       subtitle = "Longer passwords are more difficult to crack; **<span style='color:#D95F02'>mixing </span><span style='color:#1B9E77'>letters</span>** and **<span style='color:#7570B3'>numbers</span>** makes it even harder",
       caption = "Source: Knowledge is Beautiful\nVisualisation: Bethany Dearlove (@bethany.dearlove)") +
  ggdark::dark_theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    text = element_text(family = "Gill Sans MT"),
    plot.title = element_text(margin = margin(b = 4), face = "bold", size = 14),
    plot.subtitle = element_markdown(margin = margin(b = 4), size = 10),
    plot.caption = element_text(margin = margin(t = 15), size = 8),
    axis.title.x = element_text(margin = margin(t = 5)),
    axis.title.y = element_text(margin = margin(r = 5))
  ) +
  NULL

# Save plot
ggsave("2020_week3_passwords.png", height=4, width=6.5, dpi=300, unit="in")
