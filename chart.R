# ─────────────────────────────────────────────────────────────────
# Timeline: Number of Events Reported by Victim Identity
# Sources: Ynet & Israel Hayom
# ─────────────────────────────────────────────────────────────────

library(tidyverse)
library(lubridate)

# ── 1. Load data ──────────────────────────────────────────────────
df <- read_csv("seminar_data_with_wordcount.csv")

# ── 2. Parse dates & build a proper calendar month label ──────────
df <- df %>%
  mutate(
    date_parsed = dmy(date),
    # floor to first of calendar month for grouping
    cal_month   = floor_date(date_parsed, "month")
  )

# ── 3. Count events per calendar month × victim identity × site ───
counts <- df %>%
  count(site, cal_month, `victim identity`, name = "n_events")

# ── 4. Shared theme & colour palette ─────────────────────────────
victim_colours <- c("Jewish" = "#2166AC",   # blue
                    "Arab"   = "#D6604D")   # red-orange

base_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "grey40"),
    plot.caption     = element_text(size = 9, color = "grey50", hjust = 1),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    axis.title       = element_text(size = 11),
    legend.position  = "top",
    legend.title     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# ── 5. Plot function ───────────────────────────────────────────────
make_timeline <- function(site_name, data) {
  site_data <- data %>% filter(site == site_name)
  
  ggplot(site_data, aes(x = cal_month, y = n_events,
                        colour = `victim identity`,
                        group  = `victim identity`)) +
    
    # shaded area under each line
    geom_area(aes(fill = `victim identity`),
              alpha = 0.12, position = "identity") +
    
    # line
    geom_line(linewidth = 1.1) +
    
    # points at each observation
    geom_point(size = 2.8) +
    
    # label the peaks to aid reading
    geom_text(
      data = site_data %>%
        group_by(`victim identity`) %>%
        filter(n_events == max(n_events)) %>%
        slice_head(n = 1),          # one label per identity if tied
      aes(label = n_events),
      vjust = -1, size = 3.5, fontface = "bold", show.legend = FALSE
    ) +
    
    scale_colour_manual(values = victim_colours, name = "Victim identity") +
    scale_fill_manual(  values = victim_colours, name = "Victim identity") +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b\n%Y",
      expand = expansion(mult = 0.03)
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 5),
      expand = expansion(mult = c(0, 0.18))
    ) +
    labs(
      title    = paste("Events Reported by", site_name),
      subtitle = "Categorised by victim identity",
      x        = NULL,
      y        = "Number of events",
    ) +
    base_theme
}

# ── 6. Generate the two plots ──────────────────────────────────────
p_ynet  <- make_timeline("YNET",         counts)
p_hayom <- make_timeline("Israel Hayom", counts)

# ── 7. Save to files ───────────────────────────────────────────────
ggsave("ynet_timeline.png",
       plot   = p_ynet,
       width  = 10, height = 5.5, dpi = 180, bg = "white")

ggsave("israel_hayom_timeline.png",
       plot   = p_hayom,
       width  = 10, height = 5.5, dpi = 180, bg = "white")

message("✓ Plots saved: ynet_timeline.png  &  israel_hayom_timeline.png")

# ── 8. (Optional) side-by-side combined view ──────────────────────
# Uncomment the lines below if you have the 'patchwork' package installed:
#
# library(patchwork)
# combined <- p_ynet / p_hayom +
#   plot_annotation(
#     title   = "News Coverage of Violence Events by Victim Identity",
#     caption = "Ynet (top)  ·  Israel Hayom (bottom)"
#   )
# ggsave("combined_timeline.png",
#        plot = combined, width = 10, height = 11, dpi = 180, bg = "white")