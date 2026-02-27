# 06_strip_titles.R - Regenerate all figures without ggplot titles
# LaTeX captions handle titles, so ggplot titles create duplicates

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
SOURCED_BY_CHILD <- TRUE
source(here::here("code", "00_master.R"))

library(HonestDiD)

analytical <- readRDS(file.path(DATA_CLEAN, "analytical_sample.rds"))
did        <- readRDS(file.path(DATA_CLEAN, "did_sample.rds"))
es_data    <- readRDS(file.path(TAB_DIR, "event_study_coefs.rds"))
ct_es_data <- readRDS(file.path(TAB_DIR, "continuous_event_study_coefs.rds"))
het_models <- readRDS(file.path(TAB_DIR, "heterogeneity_models.rds"))
robust_models <- readRDS(file.path(TAB_DIR, "robustness_models.rds"))
did_models <- readRDS(file.path(TAB_DIR, "did_baseline_models.rds"))

theme_pub <- theme_minimal(base_size = 13, base_family = "serif") +
  theme(
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    plot.caption     = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 10),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 10),
    strip.text       = element_text(size = 11, face = "bold"),
    plot.margin      = margin(10, 15, 10, 10)
  )

quad_colors <- c(
  "HighAbstract_Teleworkable"     = "#0072B2",
  "HighAbstract_NonTeleworkable"  = "#D55E00",
  "LowAbstract_Teleworkable"     = "#56B4E9",
  "LowAbstract_NonTeleworkable"  = "#999999"
)
quad_labels <- c(
  "HighAbstract_Teleworkable"     = "High Abstract, Teleworkable (Treatment)",
  "HighAbstract_NonTeleworkable"  = "High Abstract, Non-Teleworkable (Control)",
  "LowAbstract_Teleworkable"     = "Low Abstract, Teleworkable",
  "LowAbstract_NonTeleworkable"  = "Low Abstract, Non-Teleworkable"
)

extract_coef <- function(model, label) {
  ct <- coeftable(model)
  idx <- grep("treated_x_post", rownames(ct))
  if (length(idx) == 0) return(NULL)
  data.table(group = label, estimate = ct[idx[1], "Estimate"],
             se = ct[idx[1], "Std. Error"],
             ci_lo = ct[idx[1], "Estimate"] - 1.96 * ct[idx[1], "Std. Error"],
             ci_hi = ct[idx[1], "Estimate"] + 1.96 * ct[idx[1], "Std. Error"])
}

extract_robust <- function(model, label, coef_name = "treated_x_post") {
  ct <- coeftable(model)
  idx <- grep(coef_name, rownames(ct))
  if (length(idx) == 0) return(NULL)
  data.table(spec = label, estimate = ct[idx[1], "Estimate"],
             se = ct[idx[1], "Std. Error"],
             ci_lo = ct[idx[1], "Estimate"] - 1.96 * ct[idx[1], "Std. Error"],
             ci_hi = ct[idx[1], "Estimate"] + 1.96 * ct[idx[1], "Std. Error"])
}


# Fig 1: Wage trends

wage_trends <- analytical[, .(
  mean_wage = weighted.mean(real_hourly_wage, EARNWT), N = .N
), by = .(quadrant, year_quarter, YEAR, quarter)]
wage_trends[, date := YEAR + (quarter - 1) / 4]

fig1 <- ggplot(wage_trends, aes(x = date, y = mean_wage, color = quadrant, group = quadrant)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  annotate("text", x = 2020.1, y = max(wage_trends$mean_wage) * 0.98,
           label = "COVID-19", hjust = 0, size = 3, color = "grey40", family = "serif") +
  scale_color_manual(values = quad_colors, labels = quad_labels) +
  scale_x_continuous(breaks = 2017:2025) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = NULL, y = "Mean Real Hourly Wage ($2019)") +
  theme_pub + guides(color = guide_legend(nrow = 2))

ggsave(file.path(FIG_DIR, "fig1_wage_trends_quadrant.pdf"), fig1, width = 10, height = 6, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig1_wage_trends_quadrant.png"), fig1, width = 10, height = 6, dpi = 300)

# Fig 1b: DID trends

did_trends <- did[, .(mean_log_wage = weighted.mean(log_wage, EARNWT), N = .N),
                  by = .(treated, year_quarter, YEAR, quarter)]
did_trends[, date := YEAR + (quarter - 1) / 4]
did_trends[, group := factor(treated, labels = c("Control (Non-Teleworkable)", "Treatment (Teleworkable)"))]

fig1b <- ggplot(did_trends, aes(x = date, y = mean_log_wage, color = group, group = group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  annotate("text", x = 2020.1, y = max(did_trends$mean_log_wage) * 0.998,
           label = "COVID-19", hjust = 0, size = 3, color = "grey40", family = "serif") +
  scale_color_manual(values = c("Treatment (Teleworkable)" = "#0072B2",
                                 "Control (Non-Teleworkable)" = "#D55E00")) +
  scale_x_continuous(breaks = 2017:2025) +
  labs(x = NULL, y = "Mean Log Real Hourly Wage") +
  theme_pub

ggsave(file.path(FIG_DIR, "fig1b_did_trends.pdf"), fig1b, width = 10, height = 6, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig1b_did_trends.png"), fig1b, width = 10, height = 6, dpi = 300)

# Fig 2: Event study

fig2 <- ggplot(es_data, aes(x = event_time, y = estimate)) +
  annotate("rect", xmin = -0.5, xmax = max(es_data$event_time) + 0.5,
           ymin = -Inf, ymax = Inf, fill = "#0072B2", alpha = 0.05) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "#0072B2", alpha = 0.15) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.3, color = "#0072B2", alpha = 0.5) +
  geom_point(size = 2.2, color = "#0072B2") +
  geom_line(linewidth = 0.6, color = "#0072B2", alpha = 0.7) +
  geom_point(data = es_data[event_time == -1], size = 3.5,
             shape = 21, fill = "white", color = "#0072B2", stroke = 1.2) +
  scale_x_continuous(breaks = seq(-12, 12, by = 2),
                     labels = function(x) {
                       qt <- ((x %% 4) + 4) %% 4 + 1
                       yr_adj <- 2020 + (x - (qt - 1)) %/% 4
                       paste0(yr_adj, "\nQ", qt)
                     }) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Quarter Relative to 2020Q1", y = "Log Wage Differential (Treatment - Control)") +
  theme_pub + theme(axis.text.x = element_text(size = 7, lineheight = 0.9))

ggsave(file.path(FIG_DIR, "fig2_event_study.pdf"), fig2, width = 11, height = 6, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig2_event_study.png"), fig2, width = 11, height = 6, dpi = 300)

# Fig 3: Continuous event study

fig3 <- ggplot(ct_es_data, aes(x = event_time, y = estimate)) +
  annotate("rect", xmin = -0.5, xmax = max(ct_es_data$event_time) + 0.5,
           ymin = -Inf, ymax = Inf, fill = "#009E73", alpha = 0.05) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "#009E73", alpha = 0.15) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.3, color = "#009E73", alpha = 0.5) +
  geom_point(size = 2.2, color = "#009E73") +
  geom_line(linewidth = 0.6, color = "#009E73", alpha = 0.7) +
  geom_point(data = ct_es_data[event_time == -1], size = 3.5,
             shape = 21, fill = "white", color = "#009E73", stroke = 1.2) +
  scale_x_continuous(breaks = seq(-12, 12, by = 2),
                     labels = function(x) {
                       qt <- ((x %% 4) + 4) %% 4 + 1
                       yr_adj <- 2020 + (x - (qt - 1)) %/% 4
                       paste0(yr_adj, "\nQ", qt)
                     }) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Quarter Relative to 2020Q1", y = "Marginal Effect of Telework Feasibility") +
  theme_pub + theme(axis.text.x = element_text(size = 7, lineheight = 0.9))

ggsave(file.path(FIG_DIR, "fig3_continuous_event_study.pdf"), fig3, width = 11, height = 6, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig3_continuous_event_study.png"), fig3, width = 11, height = 6, dpi = 300)

# Fig 4: First stage

first_stage <- analytical[YEAR >= 2022 & !is.na(TELWRKPAY) & TELWRKPAY > 0, .(
  pct_telework = weighted.mean(TELWRKPAY == 1, EARNWT), N = .N
), by = .(quadrant)]
first_stage[, quadrant_label := quad_labels[quadrant]]

fig4 <- ggplot(first_stage, aes(x = reorder(quadrant_label, -pct_telework),
                                 y = pct_telework, fill = quadrant)) +
  geom_col(width = 0.65, alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%", pct_telework * 100)),
            vjust = -0.5, size = 3.5, family = "serif") +
  scale_fill_manual(values = quad_colors, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 22)) +
  labs(x = NULL, y = "Share Teleworking for Pay") +
  theme_pub + theme(axis.text.x = element_text(size = 9))

ggsave(file.path(FIG_DIR, "fig4_first_stage.pdf"), fig4, width = 9, height = 5.5, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig4_first_stage.png"), fig4, width = 9, height = 5.5, dpi = 300)

# Fig 5: Heterogeneity

het_fig_list <- list(
  extract_coef(het_models$male, "Male"), extract_coef(het_models$female, "Female"),
  extract_coef(het_models$metro_only, "Metro"), extract_coef(het_models$nonmetro_only, "Non-Metro"),
  extract_coef(het_models$ba_plus, "BA+"), extract_coef(het_models$no_ba, "No BA"),
  extract_coef(het_models$union, "Union"), extract_coef(het_models$nonunion, "Non-Union")
)
if (!is.null(het_models$age_under40)) {
  het_fig_list <- c(het_fig_list, list(
    extract_coef(het_models$age_under40, "Under 40"),
    extract_coef(het_models$age_40plus, "40+")
  ))
}
het_coefs <- rbindlist(het_fig_list)
grp_levels <- c("Male","Female","Metro","Non-Metro","BA+","No BA","Union","Non-Union")
if (!is.null(het_models$age_under40)) grp_levels <- c(grp_levels, "Under 40", "40+")
het_coefs[, group := factor(group, levels = rev(grp_levels))]
het_coefs[, dimension := fcase(
  group %in% c("Male","Female"), "Gender",
  group %in% c("Metro","Non-Metro"), "Geography",
  group %in% c("BA+","No BA"), "Education",
  group %in% c("Union","Non-Union"), "Union Status",
  group %in% c("Under 40","40+"), "Age"
)]

fig5 <- ggplot(het_coefs, aes(x = estimate, y = group, color = dimension)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.2, linewidth = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Gender"="#0072B2","Geography"="#D55E00","Education"="#009E73","Union Status"="#CC79A7","Age"="#E69F00")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Estimated Effect on Log Wages", y = NULL) +
  theme_pub + theme(legend.position = "right", legend.title = element_text(face = "bold", size = 10))

ggsave(file.path(FIG_DIR, "fig5_heterogeneity.pdf"), fig5, width = 9, height = 5.5, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig5_heterogeneity.png"), fig5, width = 9, height = 5.5, dpi = 300)

# Fig 6: Robustness

fig6_list <- list(
  extract_robust(did_models$m5, "Baseline (preferred)"),
  extract_robust(robust_models$ind_x_yq, "Ind x YQ FE"),
  extract_robust(robust_models$no_peak, "Excl. 2020Q2-Q4"),
  extract_robust(robust_models$post21, "Pre-2020 vs Post-2021", "treat_x_post21"),
  extract_robust(robust_models$q75, "Top quartile"),
  extract_robust(robust_models$p60, "Top 40%"),
  extract_robust(robust_models$dbl_cluster, "Double-clustered SE"),
  extract_robust(robust_models$unweighted, "Unweighted"),
  extract_robust(did_models$m6, "RPP-adjusted wages"),
  extract_robust(robust_models$placebo, "Placebo (2018Q1)", "placebo_treat_x_post")
)
if (!is.null(robust_models$no_healthcare)) {
  fig6_list <- append(fig6_list, list(
    extract_robust(robust_models$no_healthcare, "Excl. healthcare (SOC 29)")
  ), after = 9)
}
if (!is.null(robust_models$prime_age)) {
  fig6_list <- append(fig6_list, list(
    extract_robust(robust_models$prime_age, "Prime-age (25-54)")
  ))
}
if (!is.null(robust_models$above_med_abstract)) {
  fig6_list <- append(fig6_list, list(
    extract_robust(robust_models$above_med_abstract, "Above-median abstract")
  ))
}
robust_coefs <- rbindlist(fig6_list)
robust_coefs[, spec := factor(spec, levels = rev(spec))]
robust_coefs[, is_baseline := spec == "Baseline (preferred)"]

fig6 <- ggplot(robust_coefs, aes(x = estimate, y = spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  annotate("rect",
           xmin = robust_coefs[spec == "Baseline (preferred)", ci_lo],
           xmax = robust_coefs[spec == "Baseline (preferred)", ci_hi],
           ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.2, linewidth = 0.6, color = "#0072B2") +
  geom_point(aes(shape = is_baseline), size = 3, color = "#0072B2") +
  scale_shape_manual(values = c("TRUE" = 18, "FALSE" = 16), guide = "none") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Estimated Effect on Log Wages", y = NULL) +
  theme_pub

ggsave(file.path(FIG_DIR, "fig6_robustness.pdf"), fig6, width = 9, height = 7, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig6_robustness.png"), fig6, width = 9, height = 7, dpi = 300)

# Fig 7: Wage density

did[, period_label := fifelse(post == 0, "Pre-2020", "Post-2020")]
did[, treat_label := fifelse(treated == 1, "Teleworkable (Treatment)", "Non-Teleworkable (Control)")]

fig7 <- ggplot(did, aes(x = log_wage, fill = period_label, weight = EARNWT)) +
  geom_density(alpha = 0.4, color = NA) +
  facet_wrap(~ treat_label, ncol = 2) +
  scale_fill_manual(values = c("Pre-2020" = "#56B4E9", "Post-2020" = "#D55E00")) +
  labs(x = "Log Real Hourly Wage", y = "Density") +
  theme_pub + theme(legend.position = "bottom")

ggsave(file.path(FIG_DIR, "fig7_wage_density.pdf"), fig7, width = 10, height = 5.5, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig7_wage_density.png"), fig7, width = 10, height = 5.5, dpi = 300)

# Fig 8: Progressive controls

prog_coefs <- rbindlist(lapply(seq_along(did_models), function(i) {
  m <- did_models[[i]]
  ct <- coeftable(m)
  idx <- grep("treated_x_post", rownames(ct))
  if (length(idx) == 0) return(NULL)
  data.table(spec = names(did_models)[i],
             estimate = ct[idx, "Estimate"], se = ct[idx, "Std. Error"],
             ci_lo = ct[idx, "Estimate"] - 1.96 * ct[idx, "Std. Error"],
             ci_hi = ct[idx, "Estimate"] + 1.96 * ct[idx, "Std. Error"])
}))
spec_labels <- c("m1"="(1) OCC + YQ FE","m2"="(2) + Demographics","m3"="(3) + Educ + Race FE",
                 "m4"="(4) + Industry FE","m5"="(5) + State FE","m6"="(6) RPP-Adjusted","m7"="(7) Ind x YQ FE")
prog_coefs[, spec_label := factor(spec_labels[spec], levels = spec_labels)]

fig8 <- ggplot(prog_coefs, aes(x = spec_label, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2, color = "#0072B2", linewidth = 0.6) +
  geom_point(size = 3, color = "#0072B2") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = NULL, y = "Estimated Effect on Log Wages") +
  theme_pub + theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9))

ggsave(file.path(FIG_DIR, "fig8_progressive_controls.pdf"), fig8, width = 9, height = 5.5, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig8_progressive_controls.png"), fig8, width = 9, height = 5.5, dpi = 300)

# Fig 9: Overlaid event studies

es_indyq_data <- readRDS(file.path(TAB_DIR, "event_study_indyq_coefs.rds"))
es_base_copy <- copy(es_data[, .(event_time, estimate, se, ci_lo, ci_hi)])
es_ind_copy  <- copy(es_indyq_data[, .(event_time, estimate, se, ci_lo, ci_hi)])
es_base_copy[, spec := "Baseline (Occ + YQ + Educ + Race + Ind + State FE)"]
es_ind_copy[, spec := "Saturated (+ Industry x Year-Quarter FE)"]
es_combined <- rbind(es_base_copy, es_ind_copy)

fig9 <- ggplot(es_combined, aes(x = event_time, y = estimate, color = spec, fill = spec)) +
  annotate("rect", xmin = -0.5, xmax = max(es_combined$event_time) + 0.5,
           ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.1, color = NA) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(values = c(
    "Baseline (Occ + YQ + Educ + Race + Ind + State FE)" = "#0072B2",
    "Saturated (+ Industry x Year-Quarter FE)" = "#D55E00")) +
  scale_fill_manual(values = c(
    "Baseline (Occ + YQ + Educ + Race + Ind + State FE)" = "#0072B2",
    "Saturated (+ Industry x Year-Quarter FE)" = "#D55E00")) +
  scale_x_continuous(breaks = seq(-12, 12, by = 2),
                     labels = function(x) {
                       qt <- ((x %% 4) + 4) %% 4 + 1
                       yr_adj <- 2020 + (x - (qt - 1)) %/% 4
                       paste0(yr_adj, "\nQ", qt)
                     }) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Quarter Relative to 2020Q1", y = "Log Wage Differential (Treatment - Control)") +
  theme_pub +
  theme(axis.text.x = element_text(size = 7, lineheight = 0.9), legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))

ggsave(file.path(FIG_DIR, "fig9_event_study_overlay.pdf"), fig9, width = 11, height = 6.5, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig9_event_study_overlay.png"), fig9, width = 11, height = 6.5, dpi = 300)

# Fig 10: Honest DID

honest_rm <- readRDS(file.path(TAB_DIR, "honest_did_rm.rds"))
honest_sd <- readRDS(file.path(TAB_DIR, "honest_did_sd.rds"))

rm_dt <- as.data.table(honest_rm)
fig10a <- ggplot(rm_dt, aes(x = Mbar, y = (lb + ub)/2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = "#0072B2", alpha = 0.2) +
  geom_line(aes(y = lb), color = "#0072B2", linewidth = 0.6) +
  geom_line(aes(y = ub), color = "#0072B2", linewidth = 0.6) +
  geom_point(aes(y = lb), color = "#0072B2", size = 1.5) +
  geom_point(aes(y = ub), color = "#0072B2", size = 1.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = expression(bar(M) ~ "(bound on post/pre violation ratio)"),
       y = "Confidence Interval for Treatment Effect") +
  theme_pub

ggsave(file.path(FIG_DIR, "fig10a_honest_did_rm.pdf"), fig10a, width = 8, height = 5.5, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig10a_honest_did_rm.png"), fig10a, width = 8, height = 5.5, dpi = 300)

sd_dt <- as.data.table(honest_sd)
fig10b <- ggplot(sd_dt, aes(x = M, y = (lb + ub)/2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = "#009E73", alpha = 0.2) +
  geom_line(aes(y = lb), color = "#009E73", linewidth = 0.6) +
  geom_line(aes(y = ub), color = "#009E73", linewidth = 0.6) +
  geom_point(aes(y = lb), color = "#009E73", size = 1.5) +
  geom_point(aes(y = ub), color = "#009E73", size = 1.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "M (bound on change in slope of violation)",
       y = "Confidence Interval for Treatment Effect") +
  theme_pub

ggsave(file.path(FIG_DIR, "fig10b_honest_did_sd.pdf"), fig10b, width = 8, height = 5.5, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "fig10b_honest_did_sd.png"), fig10b, width = 8, height = 5.5, dpi = 300)

# Fig A1: Metro vs Non-metro Event Studies (Appendix)

metro_es_path <- file.path(TAB_DIR, "metro_nonmetro_es_coefs.rds")
if (file.exists(metro_es_path)) {
  metro_es <- readRDS(metro_es_path)
  metro_coefs    <- metro_es$metro
  nonmetro_coefs <- metro_es$nonmetro
  metro_coefs[, subsample := "Metro"]
  nonmetro_coefs[, subsample := "Non-metro"]
  es_metro_combined <- rbind(metro_coefs, nonmetro_coefs)

  figA1 <- ggplot(es_metro_combined, aes(x = event_time, y = estimate,
                                          color = subsample, fill = subsample)) +
    annotate("rect", xmin = -0.5, xmax = max(es_metro_combined$event_time) + 0.5,
             ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "solid", color = "grey50", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.1, color = NA) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 2, alpha = 0.8) +
    scale_color_manual(values = c("Metro" = "#0072B2", "Non-metro" = "#D55E00")) +
    scale_fill_manual(values = c("Metro" = "#0072B2", "Non-metro" = "#D55E00")) +
    scale_x_continuous(breaks = seq(-12, 12, by = 2),
                       labels = function(x) {
                         qt <- ((x %% 4) + 4) %% 4 + 1
                         yr_adj <- 2020 + (x - (qt - 1)) %/% 4
                         paste0(yr_adj, "\nQ", qt)
                       }) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Quarter Relative to 2020Q1",
         y = "Log Wage Differential (Treatment - Control)") +
    theme_pub +
    theme(axis.text.x = element_text(size = 7, lineheight = 0.9),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

  ggsave(file.path(FIG_DIR, "fig_A1_metro_nonmetro_es.pdf"),
         figA1, width = 11, height = 6, device = cairo_pdf)
  ggsave(file.path(FIG_DIR, "fig_A1_metro_nonmetro_es.png"),
         figA1, width = 11, height = 6, dpi = 300)
}
