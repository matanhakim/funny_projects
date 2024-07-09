library(tidyverse)
library(rtlr)
library(extrafont)
show_te

df_raw <- read_csv("data/uricovagerev.csv")

df <- df_raw |> 
  select(
    date = 1,
    river = 3
  ) |> 
  mutate(
    date = dmy(date),
    river = fct_lump_n(river, 4, other_level = "אחר")
  ) |> 
  arrange(date) |> 
  count(date, river) |> 
  mutate(
    .by = river,
    n = cumsum(n)
  )

p <- df |> 
  mutate(
    river = fct_reorder2(river, date, n)
  ) |> 
  ggplot(aes(date, n, color = river, shape = river, label = river)) +
  geom_line() +
  geom_point() +
  geom_text(
    data = df |> slice_max(n, by = river),
    family = "Assistant SemiBold",
    vjust = -1
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  theme_minimal() +
  theme(
    text = element_text(family = "Assistant SemiBold"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    title = "שכיחות הנהרות של אוריכובעגרב לאורך זמן",
    x = "שנה",
    y = "שכיחות",
    caption = str_rtl("נתונים: X.com     ניתוח: מתן חכים")
  )

p