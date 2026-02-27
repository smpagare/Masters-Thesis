# 05_reformat_tables.R - Regenerate tables 3-5 as standard tabular LaTeX
# modelsummary's talltblr format overflows page width with 1-inch margins

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
SOURCED_BY_CHILD <- TRUE
source(here::here("code", "00_master.R"))

did_models    <- readRDS(file.path(TAB_DIR, "did_baseline_models.rds"))
robust_models <- readRDS(file.path(TAB_DIR, "robustness_models.rds"))

extract_coef <- function(model, coef_name = "treated_x_post") {
  ct <- coeftable(model)
  if (coef_name %in% rownames(ct)) {
    est <- ct[coef_name, "Estimate"]
    se  <- ct[coef_name, "Std. Error"]
    pv  <- ct[coef_name, "Pr(>|t|)"]
    stars <- ifelse(pv < 0.01, "^{***}", ifelse(pv < 0.05, "^{**}", ifelse(pv < 0.1, "^{*}", "")))
    return(list(est = est, se = se, pv = pv, stars = stars))
  }
  NULL
}


# Table 3: Baseline DID (7 specs)

models <- did_models
specs <- c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)")

coefs <- lapply(models, function(m) extract_coef(m, "treated_x_post"))
nobs <- sapply(models, function(m) format(m$nobs, big.mark = ","))
r2   <- sapply(models, function(m) sprintf("%.3f", fitstat(m, "r2")[[1]]))

fe_rows <- list(
  "Occupation FE"     = rep("X", 7),
  "Year-Quarter FE"   = rep("X", 7),
  "Demographics"      = c("", "X", "X", "X", "X", "X", "X"),
  "Education, Race FE"= c("", "", "X", "X", "X", "X", "X"),
  "Industry FE"       = c("", "", "", "X", "X", "X", ""),
  "State FE"          = c("", "", "", "", "X", "X", "X"),
  "Ind $\\times$ YQ FE" = c("", "", "", "", "", "", "X")
)

tab3_tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Baseline Difference-in-Differences: Effect of Telework Access on Abstract-Task Wages}",
  "\\label{tab:baseline}",
  "\\footnotesize",
  "\\begin{tabular}{lccccccc}",
  "\\toprule",
  paste0(" & ", paste(specs, collapse = " & "), " \\\\"),
  "\\midrule"
)

coef_vals <- sapply(coefs, function(c) {
  if (is.null(c)) return("")
  sprintf("$%.3f%s$", c$est, c$stars)
})
tab3_tex <- c(tab3_tex,
  paste0("Treated $\\times$ Post & ", paste(coef_vals, collapse = " & "), " \\\\")
)

se_vals <- sapply(coefs, function(c) {
  if (is.null(c)) return("")
  sprintf("(%.3f)", c$se)
})
tab3_tex <- c(tab3_tex,
  paste0(" & ", paste(se_vals, collapse = " & "), " \\\\[6pt]")
)

for (fe_name in names(fe_rows)) {
  vals <- fe_rows[[fe_name]]
  tab3_tex <- c(tab3_tex,
    paste0(fe_name, " & ", paste(vals, collapse = " & "), " \\\\")
  )
}

tab3_tex <- c(tab3_tex,
  "\\midrule",
  paste0("Num.\\ Obs. & ", paste(nobs, collapse = " & "), " \\\\"),
  paste0("$R^2$ & ", paste(r2, collapse = " & "), " \\\\"),
  "\\bottomrule",
  "\\multicolumn{8}{l}{\\footnotesize $^{***}p < 0.01$; $^{**}p < 0.05$; $^{*}p < 0.1$}",
  "\\end{tabular}",
  "",
  "\\bigskip",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\small\\textit{Notes:} All regressions use CPS-ORG data, 2017Q1--2023Q1. Sample restricted to high-abstract occupations (top tercile of O*NET abstract-task index). The dependent variable is log real hourly wages (CPI-U-RS adjusted to 2019 dollars). Standard errors clustered at the occupation level (185 clusters) in parentheses. Column (5) is the preferred specification. Column (6) uses RPP-adjusted wages. Column (7) replaces industry FE with industry $\\times$ year-quarter FE to absorb sector-specific pandemic shocks.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(tab3_tex, file.path(TAB_DIR, "tab3_baseline_did.tex"))


# Table 6: Robustness (8 specs)

rob_specs <- c("Preferred", "Placebo", "No 2020", "Top 25\\%", "Top 40\\%",
               "Dbl.\\ Clust.", "Unwgt.", "Ind$\\times$YQ", "No HC", "Prime",
               "High Abs.")

m5 <- did_models$m5

rob_coefs <- list(
  extract_coef(m5, "treated_x_post"),
  extract_coef(robust_models$placebo, "placebo_treat_x_post"),
  extract_coef(robust_models$no_peak, "treated_x_post"),
  extract_coef(robust_models$q75, "treated_x_post"),
  extract_coef(robust_models$p60, "treated_x_post"),
  extract_coef(robust_models$dbl_cluster, "treated_x_post"),
  extract_coef(robust_models$unweighted, "treated_x_post"),
  extract_coef(robust_models$ind_x_yq, "treated_x_post"),
  if (!is.null(robust_models$no_healthcare)) extract_coef(robust_models$no_healthcare, "treated_x_post") else NULL,
  if (!is.null(robust_models$prime_age)) extract_coef(robust_models$prime_age, "treated_x_post") else NULL,
  if (!is.null(robust_models$above_med_abstract)) extract_coef(robust_models$above_med_abstract, "treated_x_post") else NULL
)

ncols <- length(rob_specs)

rob_nobs <- c(
  format(m5$nobs, big.mark = ","),
  format(robust_models$placebo$nobs, big.mark = ","),
  format(robust_models$no_peak$nobs, big.mark = ","),
  format(robust_models$q75$nobs, big.mark = ","),
  format(robust_models$p60$nobs, big.mark = ","),
  format(robust_models$dbl_cluster$nobs, big.mark = ","),
  format(robust_models$unweighted$nobs, big.mark = ","),
  format(robust_models$ind_x_yq$nobs, big.mark = ","),
  if (!is.null(robust_models$no_healthcare)) format(robust_models$no_healthcare$nobs, big.mark = ",") else "",
  if (!is.null(robust_models$prime_age)) format(robust_models$prime_age$nobs, big.mark = ",") else "",
  if (!is.null(robust_models$above_med_abstract)) format(robust_models$above_med_abstract$nobs, big.mark = ",") else ""
)

rob_r2 <- c(
  sprintf("%.3f", fitstat(m5, "r2")[[1]]),
  sprintf("%.3f", fitstat(robust_models$placebo, "r2")[[1]]),
  sprintf("%.3f", fitstat(robust_models$no_peak, "r2")[[1]]),
  sprintf("%.3f", fitstat(robust_models$q75, "r2")[[1]]),
  sprintf("%.3f", fitstat(robust_models$p60, "r2")[[1]]),
  sprintf("%.3f", fitstat(robust_models$dbl_cluster, "r2")[[1]]),
  sprintf("%.3f", fitstat(robust_models$unweighted, "r2")[[1]]),
  sprintf("%.3f", fitstat(robust_models$ind_x_yq, "r2")[[1]]),
  if (!is.null(robust_models$no_healthcare)) sprintf("%.3f", fitstat(robust_models$no_healthcare, "r2")[[1]]) else "",
  if (!is.null(robust_models$prime_age)) sprintf("%.3f", fitstat(robust_models$prime_age, "r2")[[1]]) else "",
  if (!is.null(robust_models$above_med_abstract)) sprintf("%.3f", fitstat(robust_models$above_med_abstract, "r2")[[1]]) else ""
)

col_nums <- paste(sprintf("(%d)", seq_len(ncols)), collapse = " & ")

tab6_tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Robustness Checks}",
  "\\label{tab:robust}",
  "\\scriptsize",
  "\\resizebox{\\textwidth}{!}{%",
  sprintf("\\begin{tabular}{l%s}", paste(rep("c", ncols), collapse = "")),
  "\\toprule",
  paste0(" & ", paste(rob_specs, collapse = " & "), " \\\\"),
  paste0(" & ", col_nums, " \\\\"),
  "\\midrule"
)

coef_vals_r <- sapply(rob_coefs, function(c) {
  if (is.null(c)) return("")
  sprintf("$%.3f%s$", c$est, c$stars)
})
se_vals_r <- sapply(rob_coefs, function(c) {
  if (is.null(c)) return("")
  sprintf("(%.3f)", c$se)
})

# Row 1: Treated x Post (blank for placebo column)
coef_vals_r1 <- coef_vals_r
coef_vals_r1[2] <- ""
se_vals_r1 <- se_vals_r
se_vals_r1[2] <- ""

tab6_tex <- c(tab6_tex,
  paste0("Treated $\\times$ Post & ", paste(coef_vals_r1, collapse = " & "), " \\\\"),
  paste0(" & ", paste(se_vals_r1, collapse = " & "), " \\\\[6pt]")
)

# Row 2: Placebo x Post
placebo_row <- rep("", ncols)
placebo_se  <- rep("", ncols)
placebo_row[2] <- coef_vals_r[2]
placebo_se[2]  <- se_vals_r[2]

tab6_tex <- c(tab6_tex,
  paste0("Placebo $\\times$ Post & ", paste(placebo_row, collapse = " & "), " \\\\"),
  paste0(" & ", paste(placebo_se, collapse = " & "), " \\\\[6pt]")
)

tab6_tex <- c(tab6_tex,
  "\\midrule",
  paste0("Num.\\ Obs. & ", paste(rob_nobs, collapse = " & "), " \\\\"),
  paste0("$R^2$ & ", paste(rob_r2, collapse = " & "), " \\\\"),
  "\\bottomrule",
  sprintf("\\multicolumn{%d}{l}{\\footnotesize $^{***}p < 0.01$; $^{**}p < 0.05$; $^{*}p < 0.1$}", ncols + 1),
  "\\end{tabular}%",
  "}",
  "",
  "\\bigskip",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\small\\textit{Notes:} All regressions use CPS-ORG data. Standard errors clustered at the occupation level in parentheses. Column (1) reproduces the preferred specification from Table~\\ref{tab:baseline}. Column (2) uses pre-pandemic data only (2017--2019) with a placebo treatment at 2018Q1. Column (3) excludes 2020Q2--Q4. Columns (4)--(5) use alternative abstract-task thresholds. Column (6) double-clusters by occupation and state. Column (7) is unweighted. Column (8) includes industry $\\times$ year-quarter FE. Column (9) excludes healthcare practitioners (SOC major group 29) from the control group. Column (10) restricts to prime-age workers (25--54). Column (11) restricts to occupations above the median abstract-task score within the top tercile.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(tab6_tex, file.path(TAB_DIR, "tab5_robustness.tex"))


# Table 5: Heterogeneity as standard tabular

het_models <- readRDS(file.path(TAB_DIR, "heterogeneity_models.rds"))

het_list <- list(
  het_models$male, het_models$female,
  het_models$metro_only, het_models$nonmetro_only,
  het_models$ba_plus, het_models$no_ba,
  het_models$union, het_models$nonunion,
  het_models$age_under40, het_models$age_40plus
)
het_specs <- c("Male", "Female", "Metro", "Non-Metro", "BA+", "No BA", "Union", "Non-Union", "$<$40", "$\\geq$40")

het_coefs <- lapply(het_list, extract_coef, coef_name = "treated_x_post")
het_nobs <- sapply(het_list, function(m) format(m$nobs, big.mark = ","))
het_r2   <- sapply(het_list, function(m) sprintf("%.3f", fitstat(m, "r2")[[1]]))

tab5_tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Heterogeneity in the Telework-Wage Effect}",
  "\\label{tab:het}",
  "\\scriptsize",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Gender} & \\multicolumn{2}{c}{Geography} & \\multicolumn{2}{c}{Education} & \\multicolumn{2}{c}{Union Status} & \\multicolumn{2}{c}{Age} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9} \\cmidrule(lr){10-11}",
  paste0(" & ", paste(het_specs, collapse = " & "), " \\\\"),
  paste0(" & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10) \\\\"),
  "\\midrule"
)

coef_vals_h <- sapply(het_coefs, function(c) {
  if (is.null(c)) return("")
  sprintf("$%.3f%s$", c$est, c$stars)
})
se_vals_h <- sapply(het_coefs, function(c) {
  if (is.null(c)) return("")
  sprintf("(%.3f)", c$se)
})

tab5_tex <- c(tab5_tex,
  paste0("Treated $\\times$ Post & ", paste(coef_vals_h, collapse = " & "), " \\\\"),
  paste0(" & ", paste(se_vals_h, collapse = " & "), " \\\\[6pt]"),
  "\\midrule",
  paste0("Num.\\ Obs. & ", paste(het_nobs, collapse = " & "), " \\\\"),
  paste0("$R^2$ & ", paste(het_r2, collapse = " & "), " \\\\"),
  "\\bottomrule",
  "\\multicolumn{11}{l}{\\footnotesize $^{***}p < 0.01$; $^{**}p < 0.05$; $^{*}p < 0.1$}",
  "\\end{tabular}%",
  "}",
  "",
  "\\bigskip",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\small\\textit{Notes:} All regressions use CPS-ORG data, 2017Q1--2023Q1. Each column reports the coefficient on Treated $\\times$ Post from a separate regression on the indicated subsample. All specifications include occupation, year-quarter, education (where applicable), race, industry, and state fixed effects plus individual demographic controls. Standard errors clustered at the occupation level in parentheses.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(tab5_tex, file.path(TAB_DIR, "tab4_heterogeneity.tex"))
