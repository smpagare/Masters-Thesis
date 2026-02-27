# 08_occupation_dotplot.R - Occupation dot plot for presentation
# Shows top occupations in Treatment vs Control by sample size

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
SOURCED_BY_CHILD <- TRUE
source(here::here("code", "00_master.R"))

# Load DID sample
did <- readRDS(file.path(DATA_CLEAN, "did_sample.rds"))

# Load Dingel-Neiman for occupation titles
dn <- fread(file.path(DN_DIR, "onet_teleworkable_blscodes.csv"))
setnames(dn, c("OCC_CODE", "OES_TITLE", "teleworkable"),
         c("soc_6digit", "occ_title", "tw"))

# Aggregate: count observations per occupation × treatment status
occ_counts <- did[, .(
  N         = .N,
  mean_wage = weighted.mean(real_hourly_wage, EARNWT, na.rm = TRUE)
), by = .(soc_6digit, treated)]

# Merge occupation titles
occ_counts <- merge(occ_counts, dn[, .(soc_6digit, occ_title)],
                    by = "soc_6digit", all.x = TRUE)

# Fallback titles for SOC codes that don't match DN file exactly
# (CPS uses broader aggregations than the BLS SOC used in DN)
soc_fallback <- data.table(
  soc_6digit = c("25-3000", "25-2010", "25-2020", "25-2030", "25-2050",
                  "43-9199", "21-1020", "21-1010", "21-2099", "25-1000",
                  "25-4010", "11-2020", "11-9030", "13-1021", "13-1023",
                  "13-1030", "13-1070", "13-2070", "15-2090", "17-1010",
                  "17-1020", "17-2070", "17-2110", "19-1010", "19-1020",
                  "19-1030", "19-1040", "19-2010", "19-2030", "19-2040",
                  "19-3030", "19-3090", "27-1010", "27-1020", "27-3010",
                  "27-3020", "27-3090", "27-4010", "27-4030", "29-1020",
                  "29-1060", "29-1129", "29-9000", "31-2010", "31-2020",
                  "33-1099", "33-2020", "33-3010", "39-9030", "39-9099",
                  "53-1000"),
  fallback   = c("Other Teachers & Instructors", "Preschool & Kindergarten Teachers",
                  "Elementary & Middle School Teachers", "Secondary School Teachers",
                  "Special Education Teachers",
                  "Office Support Workers, All Other", "Social Workers",
                  "Counselors", "Religious Workers", "Postsecondary Teachers",
                  "Librarians", "Marketing Managers", "Education Administrators",
                  "Purchasing Agents", "Purchasing Agents (Exc. Wholesale/Retail)",
                  "Claims Adjusters & Examiners", "Human Resources Specialists",
                  "Financial Examiners", "Mathematical Scientists",
                  "Architects", "Surveyors", "Industrial Engineers",
                  "Environmental Engineers", "Agricultural Scientists",
                  "Biological Scientists", "Conservation Scientists",
                  "Medical Scientists", "Physicists & Astronomers",
                  "Environmental Scientists", "Chemists & Materials Scientists",
                  "Psychologists", "Social Scientists, All Other",
                  "Artists & Related Workers", "Designers",
                  "Reporters & Correspondents", "Editors",
                  "Writers & Authors", "Broadcast Technicians",
                  "Sound Engineering Technicians", "Dentists",
                  "Physicians & Surgeons", "Nurse Practitioners",
                  "Health Technologists, All Other", "Occupational Therapy Assistants",
                  "Physical Therapy Assistants",
                  "First-Line Supervisors (Protective)", "Fire Inspectors",
                  "Bailiffs & Correctional Officers", "Residential Advisors",
                  "Personal Care Workers, All Other",
                  "Transportation Supervisors")
)
occ_counts <- merge(occ_counts, soc_fallback, by = "soc_6digit", all.x = TRUE)
occ_counts[is.na(occ_title) & !is.na(fallback), occ_title := fallback]
occ_counts[, fallback := NULL]

# Clean up titles: shorten long names for readability
occ_counts[, occ_label := str_to_title(occ_title)]
occ_counts[nchar(occ_label) > 38, occ_label := paste0(substr(occ_label, 1, 36), "\u2026")]

# Flag missing titles (shouldn't happen now)
occ_counts[is.na(occ_label), occ_label := paste0("SOC ", soc_6digit)]

# Create group labels
occ_counts[, group := factor(treated,
  levels = c(1, 0),
  labels = c("Treatment: Teleworkable", "Control: Non-Teleworkable")
)]

# Top 8 per group by sample size
top_treat  <- occ_counts[treated == 1][order(-N)][1:8]
top_ctrl   <- occ_counts[treated == 0][order(-N)][1:8]
plot_data  <- rbind(top_treat, top_ctrl)

# Order within each facet by N
plot_data[, occ_label := reorder(occ_label, N)]

# Reuse thesis theme from 03_figures.R, tweak for horizontal dot plot
theme_occ <- theme_minimal(base_size = 13, base_family = "serif") +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle    = element_text(size = 11, color = "grey40", hjust = 0),
    plot.caption     = element_text(size = 8, color = "grey50", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position  = "none",
    strip.text       = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = NA),
    plot.margin      = margin(10, 15, 10, 10)
  )

group_colors <- c("Treatment: Teleworkable" = "#0072B2",
                  "Control: Non-Teleworkable" = "#D55E00")

# Create lollipop/dot plot
fig_occ <- ggplot(plot_data, aes(x = N, y = occ_label, color = group)) +
  geom_segment(aes(x = 0, xend = N, y = occ_label, yend = occ_label),
               linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 3.5) +
  geom_text(aes(label = scales::comma(N)), hjust = -0.3, size = 3.2,
            family = "serif", show.legend = FALSE) +
  facet_wrap(~ group, scales = "free_y", ncol = 2) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "Top Occupations by Sample Size: Treatment vs. Control",
    subtitle = "DID estimation sample (high-abstract occupations), CPS-ORG 2017\u20132023",
    x        = "Number of Observations",
    y        = NULL,
    caption  = "Occupations classified by Dingel-Neiman (2020) telework feasibility within top-tercile abstract-task intensity."
  ) +
  theme_occ

# Save
ggsave(file.path(FIG_DIR, "fig_occ_dotplot.pdf"),
       fig_occ, width = 12, height = 5.5, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig_occ_dotplot.png"),
       fig_occ, width = 12, height = 5.5, dpi = 300)

# Also save to presentation figures directory
pres_fig_dir <- file.path(BASE_DIR, "presentation", "figures")
if (dir.exists(pres_fig_dir)) {
  file.copy(file.path(FIG_DIR, "fig_occ_dotplot.pdf"),
            file.path(pres_fig_dir, "fig_occ_dotplot.pdf"),
            overwrite = TRUE)
  file.copy(file.path(FIG_DIR, "fig_occ_dotplot.png"),
            file.path(pres_fig_dir, "fig_occ_dotplot.png"),
            overwrite = TRUE)
}

file.path(FIG_DIR, "fig_occ_dotplot.pdf") # check output path
