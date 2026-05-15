# ─────────────────────────────────────────────────────────────
#  Coefficient plot: Effect on Article Word Count
#  Reproduces model2_chart1_coefplot.png
#  Model: OLS with month fixed effects (felm from lfe package)
# ─────────────────────────────────────────────────────────────

library(ggplot2)
library(dplyr)
library(lfe)        # for felm() – install with: install.packages("lfe")

# ── 1. Load & clean data ──────────────────────────────────────
data <- read.csv("seminar_data_with_wordcount.csv", fileEncoding = "UTF-8-BOM")

# Drop any unnamed trailing columns
data <- data[, !grepl("^X(\\.\\d+)?$", names(data))]

# Standardise column names (handle spaces vs dots from CSV export)
names(data) <- gsub(" ", ".", names(data))
# Expected columns after rename:
#   ID, date, title, url, month, victim.identity, site,
#   was.reported.in.both.sites, word.count

# ── 2. Encode predictors as numeric ──────────────────────────
reg_data <- data %>%
  mutate(
    victim.identity = ifelse(victim.identity == "Arab", 1, 0),
    site            = ifelse(site == "YNET",            1, 0)
  )

# ── 3. Build model2 dataset ───────────────────────────────────
# Articles not reported on a site are added as zero-word-count rows
# for that site (i.e. the "missing" coverage is counted as 0 words).
not_reported <- reg_data %>%
  filter(was.reported.in.both.sites == 0) %>%
  mutate(
    word.count = 0,
    site       = 1 - site,          # flip to the site that didn't cover it
    title      = "placeholder_not_reported"
  )

reg_data2 <- bind_rows(reg_data, not_reported)

# ── 4. Fit model ──────────────────────────────────────────────
# word.count ~ victim.identity + site + victim.identity:site | month FE
model2 <- felm(word.count ~ victim.identity * site | month, data = reg_data2)

s  <- summary(model2)
ci <- confint(model2)   # 95 % CIs by default

# ── 5. Build tidy coefficient data frame ─────────────────────
# Print coefficient names so you can verify / debug if needed:
message("Coefficient names: ", paste(names(coef(model2)), collapse = ", "))

# Auto-detect the interaction term name (felm may use "victim.identity:site"
# or "victim.identity1:site1" depending on whether inputs are numeric/factor)
interaction_term <- grep(
  "victim\\.identity.*site|site.*victim\\.identity",
  names(coef(model2)), value = TRUE
)

term_map <- c(
  "victim.identity" = "Victim Identity\n(Arab vs Jewish)",
  "site"            = "Site\n(YNET vs Israel Hayom)"
)
term_map[interaction_term] <- "Interaction\n(Arab \u00d7 YNET)"

coef_df <- data.frame(
  term     = names(term_map),
  label    = unname(term_map),
  estimate = coef(model2)[names(term_map)],
  ci_lo    = ci[names(term_map), 1],
  ci_hi    = ci[names(term_map), 2],
  p_value  = s$coefficients[names(term_map), "Pr(>|t|)"],
  stringsAsFactors = FALSE,
  row.names = NULL
) %>%
  mutate(
    stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    # Ordered factor so plot rows match the chart (top → bottom)
    label = factor(label, levels = rev(unname(term_map)))
  )

# ── 6. Plot ───────────────────────────────────────────────────
# ── 6. Plot ───────────────────────────────────────────────────
p <- ggplot(coef_df, aes(x = estimate, y = label)) +
  
  # vertical reference lines at -100 and +100 (light grey)
  geom_vline(xintercept = c(-100, 100),
             colour = "grey80", linewidth = 0.5) +
  
  # zero-effect dashed reference
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "grey50", linewidth = 0.7) +
  
  # 95 % confidence interval bars
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.12, colour = "#1F5FA6", linewidth = 1.1) +
  
  # point estimates
  geom_point(size = 4, colour = "#1F5FA6") +
  
  # coefficient labels (above each point)
  geom_text(
    aes(label = paste0("β = ", round(estimate, 1), stars)),
    vjust = -1.1, size = 3.8, colour = "#1F5FA6", fontface = "bold"
  ) +
  
  scale_x_continuous(breaks = c(-200,-100, 0, 100,200)) +
  
  # coord_cartesian clips the *view* without dropping data — this ensures
  # error bars that extend beyond the window are still drawn (not silently removed)
  coord_cartesian(xlim = c(-220, 290)) +
  
  labs(
    title    = "Effect on Article Word Count",
    subtitle = "OLS with month fixed effects · 95% confidence intervals",
    x        = "Coefficient (words)",
    y        = NULL,
    caption  = "p<0.1  * p<0.05  ** p<0.01  *** p<0.001"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle    = element_text(size = 11, hjust = 0.5, colour = "grey40"),
    plot.caption     = element_text(size = 9,  colour = "grey50", hjust = 1),
    axis.title.x     = element_text(size = 11),
    axis.text.y      = element_text(size = 11, lineheight = 1.2),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin      = margin(15, 20, 10, 15)
  )

# ── 7. Save ───────────────────────────────────────────────────
ggsave("model2_chart1_coefplot_reproduced.png",
       plot = p, width = 8, height = 5, dpi = 180, bg = "white")

message("Chart saved to model2_chart1_coefplot_reproduced.png")

