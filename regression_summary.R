# ─────────────────────────────────────────────────────────────────
# Regression Results: Charts & Tables
# Model: site ~ victim.identity | month  (felm, month FE)
# site: YNET = 1, Israel Hayom = 0
# victim.identity: Arab = 1, Jewish = 0
# ─────────────────────────────────────────────────────────────────

library(tidyverse)
library(lfe)
library(broom)
library(knitr)
library(kableExtra)

# ── 1. Load & prepare data (mirrors Seminar_Code.R) ───────────────
data <- read.csv("seminar_data_with_wordcount.csv")
data$date <- as.Date(data$date, format = "%d-%m-%y")

reg_data <- data
reg_data$victim.identity <- ifelse(reg_data$victim.identity == "Arab", 1, 0)
reg_data$site            <- ifelse(reg_data$site == "YNET", 1, 0)

# ── 2. Run the model ──────────────────────────────────────────────
model1 <- felm(site ~ victim.identity | month, data = reg_data)
s      <- summary(model1)
ci     <- confint(model1)

# ── 3. Tidy coefficient table ──────────────────────────────────────
coef_df <- data.frame(
  term      = "Victim Identity\n(Arab = 1, Jewish = 0)",
  estimate  = coef(model1)[["victim.identity"]],
  ci_lo     = ci["victim.identity", 1],
  ci_hi     = ci["victim.identity", 2],
  std_error = s$coefficients["victim.identity", "Std. Error"],
  t_value   = s$coefficients["victim.identity", "t value"],
  p_value   = s$coefficients["victim.identity", "Pr(>|t|)"]
)

# significance stars
coef_df$stars <- case_when(
  coef_df$p_value < 0.001 ~ "***",
  coef_df$p_value < 0.01  ~ "**",
  coef_df$p_value < 0.05  ~ "*",
  coef_df$p_value < 0.1   ~ ".",
  TRUE                     ~ ""
)

# ── 4. Shared theme ───────────────────────────────────────────────
base_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "grey40"),
    plot.caption     = element_text(size = 9,  color = "grey50"),
    axis.title       = element_text(size = 11),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ══════════════════════════════════════════════════════════════════
# CHART 1 — Coefficient plot (with 95% CI)
# ══════════════════════════════════════════════════════════════════
p_coef <- ggplot(coef_df, aes(x = estimate, y = term)) +
  # zero-effect reference line
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.7) +
  # CI band
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.15, colour = "#2166AC", linewidth = 1) +
  # point estimate
  geom_point(size = 4, colour = "#2166AC") +
  # annotate coefficient value
  geom_text(aes(label = sprintf("β = %.3f%s", estimate, stars)),
            vjust = -1.2, size = 4, colour = "#2166AC", fontface = "bold") +
  labs(
    title    = "Effect of Victim Identity on Site of Coverage",
    subtitle = "Linear probability model with month fixed effects\nDependent variable: YNET (1) vs Israel Hayom (0)",
    x        = "Coefficient estimate (95% CI)",
    y        = NULL,
    caption  = "* p<0.05  ** p<0.01  *** p<0.001"
  ) +
  base_theme +
  theme(
    axis.text.y  = element_text(size = 11),
    panel.grid.major.y = element_blank()
  )

ggsave("chart1_coefficient_plot.png",
       plot = p_coef, width = 8, height = 4, dpi = 180, bg = "white")

# ══════════════════════════════════════════════════════════════════
# CHART 2 — Predicted probability by victim identity
# (de-meaned fitted values averaged over month FE)
# ══════════════════════════════════════════════════════════════════
pred_df <- data.frame(
  victim_identity = c("Jewish", "Arab"),
  # predicted P(YNET) at the mean of month FE
  prob = c(
    mean(fitted(model1)[reg_data$victim.identity == 0]),
    mean(fitted(model1)[reg_data$victim.identity == 1])
  )
)
pred_df$victim_identity <- factor(pred_df$victim_identity,
                                   levels = c("Jewish", "Arab"))

p_pred <- ggplot(pred_df, aes(x = victim_identity, y = prob,
                               fill = victim_identity)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f", prob)),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("Jewish" = "#2166AC", "Arab" = "#D6604D")) +
  scale_y_continuous(limits = c(0, 1),
                     labels  = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Predicted Probability of Coverage by YNET",
    subtitle = "Average fitted values by victim identity (month FE absorbed)",
    x        = "Victim Identity",
    y        = "P(Site = YNET)",
    caption  = "Based on felm model with month fixed effects"
  ) +
  base_theme

ggsave("chart2_predicted_probabilities.png",
       plot = p_pred, width = 6, height = 5, dpi = 180, bg = "white")

# ══════════════════════════════════════════════════════════════════
# CHART 3 — Observed coverage rates (raw data, for comparison)
# ══════════════════════════════════════════════════════════════════
obs_df <- reg_data %>%
  mutate(victim_label = ifelse(victim.identity == 1, "Arab", "Jewish")) %>%
  group_by(victim_label) %>%
  summarise(
    n_total = n(),
    n_ynet  = sum(site == 1),
    pct_ynet = n_ynet / n_total,
    .groups = "drop"
  )

p_obs <- ggplot(obs_df, aes(x = victim_label, y = pct_ynet,
                             fill = victim_label)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", pct_ynet * 100, n_total)),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Jewish" = "#2166AC", "Arab" = "#D6604D")) +
  scale_y_continuous(limits = c(0, 1),
                     labels  = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Observed Rate of Coverage by YNET",
    subtitle = "Raw proportions (before controlling for month FE)",
    x        = "Victim Identity",
    y        = "Share of articles on YNET",
    caption  = "Raw data from seminar_data_with_wordcount.csv"
  ) +
  base_theme

ggsave("chart3_observed_rates.png",
       plot = p_obs, width = 6, height = 5, dpi = 180, bg = "white")

# ══════════════════════════════════════════════════════════════════
# TABLE 1 — Main regression results (HTML + console)
# ══════════════════════════════════════════════════════════════════
table1_df <- data.frame(
  ` `            = "Victim Identity (Arab)",
  `Estimate`     = sprintf("%.4f", coef_df$estimate),
  `Std. Error`   = sprintf("%.4f", coef_df$std_error),
  `t value`      = sprintf("%.3f", coef_df$t_value),
  `p value`      = sprintf("%.4f", coef_df$p_value),
  `Sig.`         = coef_df$stars,
  `95% CI`       = sprintf("[%.4f, %.4f]", coef_df$ci_lo, coef_df$ci_hi),
  check.names    = FALSE
)

# Model-level stats
n_obs   <- nobs(model1)
r2      <- s$r2
adj_r2  <- s$adj.r.squared

cat("\n══════════════════════════════════════════════════\n")
cat(" TABLE 1 — Regression: site ~ victim.identity | month\n")
cat("══════════════════════════════════════════════════\n")
print(knitr::kable(table1_df, align = "lrrrrrr"), quote = FALSE)
cat(sprintf("\nN = %d  |  R² = %.4f  |  Adj. R² = %.4f\n", n_obs, r2, adj_r2))
cat("Month fixed effects: YES\n")
cat("Significance: . p<0.1  * p<0.05  ** p<0.01  *** p<0.001\n")
cat("══════════════════════════════════════════════════\n\n")

# Save as HTML
knitr::kable(
  table1_df,
  format  = "html",
  caption = paste0(
    "Table 1: OLS with Month Fixed Effects — Dependent variable: Site (YNET=1, Israel Hayom=0)<br>",
    sprintf("N = %d  |  R² = %.4f  |  Adj. R² = %.4f  |  Month FE: YES", n_obs, r2, adj_r2)
  ),
  align   = "lrrrrrr"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "bordered"),
    full_width        = FALSE,
    font_size         = 14
  ) %>%
  row_spec(0, bold = TRUE, background = "#2166AC", color = "white") %>%
  save_kable("table1_regression_results.html")

# ══════════════════════════════════════════════════════════════════
# TABLE 2 — Descriptive summary by victim identity
# ══════════════════════════════════════════════════════════════════
desc_df <- reg_data %>%
  mutate(victim_label = ifelse(victim.identity == 1, "Arab", "Jewish")) %>%
  group_by(`Victim Identity` = victim_label) %>%
  summarise(
    `N articles`           = n(),
    `On YNET`              = sum(site == 1),
    `On Israel Hayom`      = sum(site == 0),
    `% YNET`               = sprintf("%.1f%%", mean(site == 1) * 100),
    `Reported by both`     = sum(data$was.reported.in.both.sites[
                                   data$victim.identity %in%
                                   c("Arab","Jewish")
                                 ][seq_len(n())] == 1),
    .groups = "drop"
  )

# simpler & safer version
desc_df2 <- data %>%
  group_by(`Victim Identity` = victim.identity) %>%
  summarise(
    `N articles`       = n(),
    `On YNET`          = sum(site == "YNET"),
    `On Israel Hayom`  = sum(site == "Israel Hayom"),
    `% YNET`           = sprintf("%.1f%%", mean(site == "YNET") * 100),
    `Reported by both` = sum(was.reported.in.both.sites == 1),
    .groups = "drop"
  )

cat(" TABLE 2 — Descriptive statistics\n")
print(knitr::kable(desc_df2, align = "lrrrrr"), quote = FALSE)

knitr::kable(
  desc_df2,
  format  = "html",
  caption = "Table 2: Article counts by victim identity",
  align   = "lrrrrr"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "bordered"),
    full_width        = FALSE,
    font_size         = 14
  ) %>%
  row_spec(0, bold = TRUE, background = "#555555", color = "white") %>%
  save_kable("table2_descriptive_stats.html")

message("\n✓ All outputs saved:")
message("  Charts : chart1_coefficient_plot.png")
message("           chart2_predicted_probabilities.png")
message("           chart3_observed_rates.png")
message("  Tables : table1_regression_results.html")
message("           table2_descriptive_stats.html")
