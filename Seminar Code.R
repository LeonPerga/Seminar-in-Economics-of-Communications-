library(ggplot2)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(ISOweek)
library(lfe)
library(stargazer)
data = read.csv("seminar_data_with_wordcount.csv")
data$X <- NULL
data$X.1 <- NULL
data$X.2 <- NULL

data$date = as.Date(data$date, format = "%d/%m/%Y")


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

length(reg_data$was.reported.in.both.sites == 0 )
length(reg_data$victim.identity[reg_data$victim.identity == 1 & reg_data$was.reported.in.both.sites == 0 ])
length(reg_data$victim.identity[reg_data$victim.identity == 0 & reg_data$was.reported.in.both.sites == 0 ])
length(reg_data$victim.identity[reg_data$site == 1 & reg_data$was.reported.in.both.sites == 0 ])
length(reg_data$victim.identity[reg_data$site == 0 & reg_data$was.reported.in.both.sites == 0 ])
length(reg_data$victim.identity[reg_data$victim.identity == 1 & reg_data$site == 1 & reg_data$was.reported.in.both.sites == 0 ])
length(reg_data$victim.identity[reg_data$victim.identity == 0 & reg_data$site == 0 & reg_data$was.reported.in.both.sites == 0 ])

model0 = lm(formula = site ~ victim.identity, data = reg_data)
model1 = felm(formula = site ~ victim.identity | month , data = reg_data)
cor(reg_data$site,reg_data$victim.identity)
summary(model1)
confint(model1)
data$victim.identity <- factor(data$victim.identity, levels = c(0, 1),
                               labels = c("Jewish", "Arab"))
stargazer(model1,covariate.labels = c("זהות הקורבן"),dep.var.labels = c("אתר האינטרנט"),ci = TRUE, out = "out.html")
stargazer(model0, model1, omit = '[i][n][d]', 
          type='text',
          title = "Table: Effect of Victim Identity on Probability of Reporting Site" ,
          report = "vc*ps", 
          dep.var.labels = "Site (YNET = 1)",
          covariate.labels  = "Victim Identity (Arab = 1)",
          out="out2.html")
stargazer(
  model1,
  type              = "html",
  report            = "vc*ps",
  out               = "model1_table.html",
  title             = "Table: Effect of Victim Identity on Probability of Reporting Site",
  dep.var.labels    = "Site (YNET = 1)",
  covariate.labels  = c(
    "Victim Identity (Arab = 1)"
  ),
  ci                = FALSE,
  ci.level          = 0.95,
  digits            = 4,
  notes             = c(
    "* p&lt;0.1; ** p&lt;0.05; *** p&lt;0.01"
  ),
  notes.append      = FALSE
)

not_reported = reg_data %>%
  filter(was.reported.in.both.sites == 0) %>% 
  mutate(word.count = 0 ) %>%
  mutate(site = 1-as.numeric(site)) %>%
  mutate(title = "כניסה מדומה לייצג כתבה שלא דווחה(נחשבת כאפס מילים)")
  
reg_data2 = rbind(reg_data,not_reported)

model2 = felm(formula = word.count ~ victim.identity*site | month , data = reg_data2)
model2_1 = felm(formula = word.count ~ victim.identity*site | month , data = reg_data)

summary(model2)

stargazer(model2, model2_1, omit = '[i][n][d]',
          column.labels = c("non reported counted as 0 |  ", "   non reported not counted"),
          type='text',
          title = "Table: Effect of Victim Identity, Site and Interaction on Length of Article" ,
          report = "vc*ps",
          dep.var.labels  = c(
            "Word Count"
          ),
          covariate.labels  = c(
            "Victim Identity (Arab = 1)",
            "Site (YNET = 1)",
            "Victim Identity × Site"
          ),
          out="out3.html")




