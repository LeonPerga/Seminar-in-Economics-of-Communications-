library(ggplot2)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(ISOweek)
library(lfe)
library(stargazer)
data = read.csv("seminar_data_with_wordcount.csv")
data$date = as.Date(data$date)



plot_data <- all_weeks %>%
  left_join(weekly_counts, by = "iso.week") %>%
  mutate(
    Arab = replace_na(Arab, 0),
    Jewish = replace_na(Jewish, 0)
  ) %>%
  arrange(iso.week) %>%
  pivot_longer(
    cols = c(Arab, Jewish),
    names_to = "victim.identity",
    values_to = "count"
  )
plot_data = plot_data[-c(1:28),]
ggplot(plot_data, aes(x = factor(iso.week),
                      y = count,
                      fill = victim.identity),
                      ) +
  geom_col() +
  labs(
    x = "שבוע",
    y = "מספר הכתבות",
    fill = "זהות הקורבן",
    title = "מספר הכתבות על מקרי אלימות באיוש לפי שבוע - 2024",
  ) +
  scale_fill_manual(labels = c("ערבי","יהודי"),values = c("#D55E00","#0072B2" )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
######################
plot_df <- data %>%
  mutate(
    victim.identity = factor(victim.identity, levels = c("Arab", "Jewish")),
    site = factor(site, levels = c("Israel Hayom", "YNET")),
    both = factor(
      was.reported.in.both.sites,
      levels = c(0, 1),
      labels = c("דווח באתר אחד", "דווח בשני האתרים")
    )
  ) %>%
  count(victim.identity, site, both)
ggplot(plot_df,
       aes(x = victim.identity, y = n, fill = both)) +
  geom_col(position = "stack") +
  facet_wrap(~ site) +
  scale_fill_manual(
    values = c(
      "דווח באתר אחד"  = "grey70",
      "דווח בשני האתרים" = "black"
    )
  ) +
  labs(
    x = "זהות הקורבן",
    y = "מספר כתבות",
    fill = "",
    title = "סיקור מקרי אלימות על בסיס לאומני ביהודה ושומרון"
  ) +
  theme_minimal(base_size = 14)
######regression######
reg_data = data
reg_data$victim.identity[reg_data$victim.identity == "Jewish"] = 0
reg_data$victim.identity[reg_data$victim.identity == "Arab"] = 1

reg_data$site[reg_data$site == "YNET"] = 1
reg_data$site[reg_data$site == "Israel Hayom"] = 0

length(reg_data$victim.identity[reg_data$victim.identity == 1])
length(reg_data$victim.identity[reg_data$victim.identity == 0])
length(reg_data$was.reported.in.both.sites[reg_data$was.reported.in.both.sites == 1])

model1 = felm(formula = site ~ victim.identity | month , data = reg_data)
summary(model1)
confint(model1)
data$victim.identity <- factor(data$victim.identity, levels = c(0, 1),
                               labels = c("Jewish", "Arab"))
stargazer(model1,covariate.labels = c("זהות הקורבן"),dep.var.labels = c("אתר האינטרנט"),ci = TRUE, out = "out.html")

model2 = felm(formula = word.count ~ victim.identity*site | month , data = reg_data)
summary(model2)
