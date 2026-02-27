# 07_paper_stats.R - Generate paper_stats.tex with \newcommand definitions
# Manuscript uses \input{paper_stats.tex} so all numbers update automatically

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
SOURCED_BY_CHILD <- TRUE
source(here::here("code", "00_master.R"))

library(fixest)

# Load saved objects
did_models   <- readRDS(file.path(TAB_DIR, "did_baseline_models.rds"))
es_model     <- readRDS(file.path(TAB_DIR, "event_study_model.rds"))
het_models   <- readRDS(file.path(TAB_DIR, "heterogeneity_models.rds"))
rob_models   <- readRDS(file.path(TAB_DIR, "robustness_models.rds"))
honest_rm    <- readRDS(file.path(TAB_DIR, "honest_did_rm.rds"))
honest_sd    <- readRDS(file.path(TAB_DIR, "honest_did_sd.rds"))
honest_rm_avg <- readRDS(file.path(TAB_DIR, "honest_did_rm_avg.rds"))
cont_models  <- readRDS(file.path(TAB_DIR, "continuous_did_models.rds"))
het_tests_path <- file.path(TAB_DIR, "heterogeneity_equality_tests.rds")
het_tests <- if (file.exists(het_tests_path)) readRDS(het_tests_path) else NULL

es_indyq_path <- file.path(TAB_DIR, "event_study_indyq_model.rds")
es_indyq <- if (file.exists(es_indyq_path)) readRDS(es_indyq_path) else NULL

did <- readRDS(file.path(DATA_CLEAN, "did_sample.rds"))
analytical <- readRDS(file.path(DATA_CLEAN, "analytical_sample.rds"))

fmt_coef <- function(x, digits = 3) formatC(round(x, digits), digits, format = "f")
fmt_pct  <- function(x) formatC(round(x * 100, 1), 1, format = "f")
fmt_int  <- function(x) format(as.integer(x), big.mark = ",")
fmt_p    <- function(p) {
  if (p < 0.01) return("p < 0.01")
  if (p < 0.05) return("p < 0.05")
  if (p < 0.10) return("p < 0.10")
  return(sprintf("p = %.2f", p))
}


stats <- list()

# Main DID (m5 = preferred)
m5 <- did_models$m5
stats$mainCoef     <- fmt_coef(coef(m5)["treated_x_post"])
stats$mainSE       <- fmt_coef(sqrt(vcov(m5)["treated_x_post", "treated_x_post"]))
stats$mainN        <- fmt_int(m5$nobs)
stats$mainPval     <- fmt_p(fixest::pvalue(m5)["treated_x_post"])
stats$mainCoefPct  <- fmt_coef(coef(m5)["treated_x_post"] * 100, 1)

# Specification range
all_coefs <- sapply(did_models, function(m) coef(m)["treated_x_post"])
stats$specMinCoef  <- fmt_coef(min(all_coefs))
stats$specMaxCoef  <- fmt_coef(max(all_coefs))
stats$specMinPct   <- fmt_coef(min(all_coefs) * 100, 1)
stats$specMaxPct   <- fmt_coef(max(all_coefs) * 100, 1)

stats$mOneCoef     <- fmt_coef(coef(did_models$m1)["treated_x_post"])
stats$mSixCoef     <- fmt_coef(coef(did_models$m6)["treated_x_post"])
stats$mSevenCoef   <- fmt_coef(coef(did_models$m7)["treated_x_post"])
stats$mSevenPct    <- fmt_coef(coef(did_models$m7)["treated_x_post"] * 100, 1)
stats$mSevenPval   <- fmt_p(fixest::pvalue(did_models$m7)["treated_x_post"])

# R-squared range
all_r2 <- sapply(did_models, function(m) fitstat(m, "r2")$r2)
stats$rSqMin <- fmt_coef(min(all_r2))
stats$rSqMax <- fmt_coef(max(all_r2))

# Event study pre-trend F-test
pre_names <- names(coef(es_model))[grepl("event_time_f::-[2-9]|event_time_f::-1[0-9]",
                                          names(coef(es_model)))]
if (length(pre_names) > 0) {
  pre_test <- wald(es_model, keep = pre_names)
  stats$preTrendP <- sprintf("%.3f", pre_test$p)
}

# Saturated event study F-test (computed from saved model if available)
if (!is.null(es_indyq)) {
  pre_names_indyq <- names(coef(es_indyq))[grepl("event_time_f::-[2-9]|event_time_f::-1[0-9]",
                                                    names(coef(es_indyq)))]
  if (length(pre_names_indyq) > 0) {
    pre_test_indyq <- wald(es_indyq, keep = pre_names_indyq)
    stats$satPreTrendP <- sprintf("%.3f", pre_test_indyq$p)
  }
} else {
  stop("Saturated event study model (es_indyq) not found. ",
       "Run the full pipeline (02_analysis.R) first to generate this model.")
}

# Cluster counts
stats$nOccClusters   <- as.character(length(unique(did$OCC2010)))
stats$nStateClusters <- as.character(length(unique(did$STATEFIP)))

# Sample sizes
stats$fullN     <- fmt_int(nrow(analytical))
stats$didN      <- fmt_int(nrow(did))
stats$treatN    <- fmt_int(sum(did$treated == 1))
stats$controlN  <- fmt_int(sum(did$treated == 0))
stats$treatPct  <- fmt_pct(mean(did$treated == 1))
stats$controlPct <- fmt_pct(mean(did$treated == 0))

# First stage (telework adoption rates)
fs <- did[YEAR >= 2022 & !is.na(TELWRKPAY) & TELWRKPAY > 0]
if (nrow(fs) > 0) {
  fs_rates <- fs[, .(pct = weighted.mean(TELWRKPAY == 1, EARNWT)), by = treated]
  stats$fsTreatRate   <- fmt_coef(fs_rates[treated == 1, pct] * 100, 1)
  stats$fsControlRate <- fmt_coef(fs_rates[treated == 0, pct] * 100, 1)
  stats$fsGap         <- fmt_coef((fs_rates[treated == 1, pct] - fs_rates[treated == 0, pct]) * 100, 0)
}

# Heterogeneity
het_coef <- function(nm) {
  mod <- het_models[[nm]]
  fmt_coef(coef(mod)["treated_x_post"] * 100, 1)
}
het_pval <- function(nm) {
  mod <- het_models[[nm]]
  fmt_p(fixest::pvalue(mod)["treated_x_post"])
}

stats$metroCoefPct    <- het_coef("metro_only")
stats$metroPval       <- het_pval("metro_only")
stats$nonmetroCoefPct <- het_coef("nonmetro_only")
stats$nonmetroPval    <- het_pval("nonmetro_only")
stats$maleCoefPct     <- het_coef("male")
stats$malePval        <- het_pval("male")
stats$femaleCoefPct   <- het_coef("female")
stats$femalePval      <- het_pval("female")
stats$baCoefPct       <- het_coef("ba_plus")
stats$baPval          <- het_pval("ba_plus")
stats$nobaCoefPct     <- het_coef("no_ba")
stats$nobaPval        <- het_pval("no_ba")
stats$unionCoefPct    <- het_coef("union")
stats$unionPval       <- het_pval("union")
stats$nonunionCoefPct <- het_coef("nonunion")
stats$nonunionPval    <- het_pval("nonunion")

# Equality tests
if (!is.null(het_tests)) {
  metro_test  <- het_tests[group1 == "Metro"]
  gender_test <- het_tests[group1 == "Male"]
  educ_test   <- het_tests[group1 == "BA+"]
  union_test  <- het_tests[group1 == "Union"]

  stats$metroEqZ  <- sprintf("%.2f", metro_test$z)
  stats$metroEqP  <- sprintf("%.3f", metro_test$p)
  stats$genderEqZ <- sprintf("%.2f", gender_test$z)
  stats$genderEqP <- sprintf("%.3f", gender_test$p)
  stats$educEqZ   <- sprintf("%.2f", educ_test$z)
  stats$educEqP   <- sprintf("%.3f", educ_test$p)
  stats$unionEqZ  <- sprintf("%.2f", union_test$z)
  stats$unionEqP  <- sprintf("%.3f", union_test$p)
}

# Robustness
stats$placeboCoef  <- fmt_coef(coef(rob_models$placebo)["placebo_treat_x_post"])
stats$placeboPval  <- sprintf("%.2f", fixest::pvalue(rob_models$placebo)["placebo_treat_x_post"])
stats$noPeakCoef   <- fmt_coef(coef(rob_models$no_peak)["treated_x_post"])
stats$noPeakPval   <- fmt_p(fixest::pvalue(rob_models$no_peak)["treated_x_post"])
stats$topQuartCoef <- fmt_coef(coef(rob_models$q75)["treated_x_post"])
stats$topQuartPval <- fmt_p(fixest::pvalue(rob_models$q75)["treated_x_post"])
stats$topFortyCoef <- fmt_coef(coef(rob_models$p60)["treated_x_post"])
stats$topFortyPval <- fmt_p(fixest::pvalue(rob_models$p60)["treated_x_post"])
stats$unwgtCoef    <- fmt_coef(coef(rob_models$unweighted)["treated_x_post"])
stats$unwgtPval    <- fmt_p(fixest::pvalue(rob_models$unweighted)["treated_x_post"])

# Continuous treatment
cont_main <- cont_models[[1]]
tw_coef_name <- grep("telework_feasible", names(coef(cont_main)), value = TRUE)[1]
if (!is.na(tw_coef_name)) {
  stats$contCoef    <- fmt_coef(coef(cont_main)[tw_coef_name])
  stats$contCoefPct <- fmt_coef(coef(cont_main)[tw_coef_name] * 100, 1)
  stats$contSE      <- fmt_coef(sqrt(vcov(cont_main)[tw_coef_name, tw_coef_name]))
  stats$contPval    <- fmt_p(fixest::pvalue(cont_main)[tw_coef_name])
  cont_p <- fixest::pvalue(cont_main)[tw_coef_name]
  stats$contStars   <- if (cont_p < 0.01) "^{***}" else if (cont_p < 0.05) "^{**}" else if (cont_p < 0.10) "^{*}" else ""
}

# Triple-difference
cont_triple <- cont_models[[2]]
triple_names <- grep("telework_feasible.*abstract|abstract.*telework_feasible",
                     names(coef(cont_triple)), value = TRUE)
if (length(triple_names) > 0) {
  triple_name <- triple_names[1]
  stats$tripleCoef  <- fmt_coef(coef(cont_triple)[triple_name])
  stats$tripleSE    <- fmt_coef(sqrt(vcov(cont_triple)[triple_name, triple_name]))
  triple_p <- fixest::pvalue(cont_triple)[triple_name]
  stats$triplePval  <- fmt_p(triple_p)
  stats$tripleStars <- if (triple_p < 0.01) "^{***}" else if (triple_p < 0.05) "^{**}" else if (triple_p < 0.10) "^{*}" else ""
  stats$tripleN     <- fmt_int(cont_triple$nobs)
}

# Honest DID
rm_df <- as.data.frame(honest_rm)
stats$honestRmLbZero <- fmt_coef(rm_df$lb[rm_df$Mbar == 0])
stats$honestRmUbZero <- fmt_coef(rm_df$ub[rm_df$Mbar == 0])

sd_df <- as.data.frame(honest_sd)
stats$honestSdLbZero <- fmt_coef(sd_df$lb[sd_df$M == 0])
stats$honestSdUbZero <- fmt_coef(sd_df$ub[sd_df$M == 0])

rma_df <- as.data.frame(honest_rm_avg)
stats$honestRmAvgLb <- fmt_coef(rma_df$lb[rma_df$Mbar == 0])
stats$honestRmAvgUb <- fmt_coef(rma_df$ub[rma_df$Mbar == 0])

# Dollar calculations
# M2 fix: renamed from median_wage (was misleading; computes weighted mean, not median)
mean_wage <- weighted.mean(did$real_hourly_wage, did$EARNWT)
stats$meanWage <- fmt_int(round(mean_wage))
# M1 fix: use exact semi-elasticity transformation (exp(beta) - 1) for log-wage coefficients
# Previously used linear approximation beta * mean_wage, which understates by ~1.2%
prem_lo <- (exp(coef(did_models$m7)["treated_x_post"]) - 1) * mean_wage
prem_hi <- (exp(coef(did_models$m5)["treated_x_post"]) - 1) * mean_wage
stats$premHourLo   <- fmt_coef(prem_lo, 2)
stats$premHourHi   <- fmt_coef(prem_hi, 2)
stats$premAnnualLo <- fmt_int(round(prem_lo * 2080))
stats$premAnnualHi <- fmt_int(round(prem_hi * 2080))

# Descriptive stats
ctrl_wage <- weighted.mean(analytical[quadrant == "HighAbstract_NonTeleworkable", real_hourly_wage],
                            analytical[quadrant == "HighAbstract_NonTeleworkable", EARNWT])
treat_wage <- weighted.mean(analytical[quadrant == "HighAbstract_Teleworkable", real_hourly_wage],
                             analytical[quadrant == "HighAbstract_Teleworkable", EARNWT])
stats$ctrlMeanWage  <- fmt_coef(ctrl_wage, 2)
stats$treatMeanWage <- fmt_coef(treat_wage, 2)


# Additional macros (revision round)

# Distinct occupation counts per group
stats$treatOccN   <- as.character(data.table::uniqueN(did[treated == 1, OCC2010]))
stats$controlOccN <- as.character(data.table::uniqueN(did[treated == 0, OCC2010]))

# Average post-treatment event study effect
es_coefs_data <- readRDS(file.path(TAB_DIR, "event_study_coefs.rds"))
post_coefs <- es_coefs_data[event_time >= 0]
stats$avgPostES     <- fmt_coef(mean(post_coefs$estimate))
stats$avgPostESPct  <- fmt_coef(mean(post_coefs$estimate) * 100, 1)

# Oster (2019) coefficient stability - delta calculation
beta_restricted <- coef(did_models$m1)["treated_x_post"]
r2_restricted   <- fitstat(did_models$m1, "r2")$r2
beta_full       <- coef(did_models$m5)["treated_x_post"]
r2_full         <- fitstat(did_models$m5, "r2")$r2
r_max <- 1.3 * r2_full

stats$rSqRestricted <- fmt_coef(r2_restricted, 3)
stats$rSqFull       <- fmt_coef(r2_full, 3)
stats$betaRestricted <- fmt_coef(beta_restricted)
stats$betaFull       <- fmt_coef(beta_full)
stats$osterRmax      <- fmt_coef(r_max, 3)

if (abs(beta_restricted - beta_full) < 1e-4) {
  stats$osterDelta <- "$\\infty$"
} else {
  oster_delta <- (beta_full * (r_max - r2_full)) /
                 ((beta_restricted - beta_full) * (r2_full - r2_restricted))
  stats$osterDelta <- fmt_coef(oster_delta, 2)
}

# Union and non-union subsample Ns
stats$unionN    <- fmt_int(het_models$union$nobs)
stats$nonunionN <- fmt_int(het_models$nonunion$nobs)

# ITT to Wald (implied IV) rescaling
fs_did <- did[YEAR >= 2022 & !is.na(TELWRKPAY) & TELWRKPAY > 0]
if (nrow(fs_did) > 0) {
  fs_rates_raw <- fs_did[, .(pct = weighted.mean(TELWRKPAY == 1, EARNWT)), by = treated]
  fs_gap_raw <- fs_rates_raw[treated == 1, pct] - fs_rates_raw[treated == 0, pct]
  wald_est <- coef(did_models$m5)["treated_x_post"] / fs_gap_raw
  stats$waldEstimate    <- fmt_coef(wald_est)
  stats$waldEstimatePct <- fmt_coef(wald_est * 100, 1)
  stats$fsGapRaw        <- fmt_coef(fs_gap_raw * 100, 1)
}

# Healthcare exclusion robustness
if (!is.null(rob_models$no_healthcare)) {
  stats$noHealthCoef <- fmt_coef(coef(rob_models$no_healthcare)["treated_x_post"])
  nh_p <- fixest::pvalue(rob_models$no_healthcare)["treated_x_post"]
  stats$noHealthPval <- fmt_p(nh_p)
  stats$noHealthN    <- fmt_int(rob_models$no_healthcare$nobs)
}

# Tercile DN specification
tercile_path <- file.path(TAB_DIR, "tercile_dn_model.rds")
if (file.exists(tercile_path)) {
  tercile_model <- readRDS(tercile_path)
  tc <- coeftable(tercile_model)
  if ("tw_mid_x_post" %in% rownames(tc)) {
    stats$tercileMidCoef  <- fmt_coef(tc["tw_mid_x_post", "Estimate"])
    stats$tercileMidSE    <- fmt_coef(tc["tw_mid_x_post", "Std. Error"])
    mid_p <- tc["tw_mid_x_post", "Pr(>|t|)"]
    stats$tercileMidStars <- ifelse(mid_p < 0.01, "^{***}", ifelse(mid_p < 0.05, "^{**}", ifelse(mid_p < 0.1, "^{*}", "")))
  }
  if ("tw_high_x_post" %in% rownames(tc)) {
    stats$tercileHighCoef <- fmt_coef(tc["tw_high_x_post", "Estimate"])
    stats$tercileHighSE   <- fmt_coef(tc["tw_high_x_post", "Std. Error"])
    hi_p <- tc["tw_high_x_post", "Pr(>|t|)"]
    stats$tercileHighStars <- ifelse(hi_p < 0.01, "^{***}", ifelse(hi_p < 0.05, "^{**}", ifelse(hi_p < 0.1, "^{*}", "")))
  }
  stats$tercileN   <- fmt_int(tercile_model$nobs)
  stats$tercileRsq <- sprintf("%.3f", fitstat(tercile_model, "r2")[[1]])
}

# Bias-adjusted beta (Oster 2019, delta = 1)
# beta_adj = beta_full - 1 * (beta_restricted - beta_full) * (Rmax - R_full) / (R_full - R_restricted)
if (abs(r2_full - r2_restricted) > 1e-6) {
  bias_adj_beta <- beta_full - 1 * (beta_restricted - beta_full) *
                   (r_max - r2_full) / (r2_full - r2_restricted)
  stats$biasAdjBeta    <- fmt_coef(bias_adj_beta)
  stats$biasAdjBetaPct <- fmt_coef(bias_adj_beta * 100, 1)
} else {
  stats$biasAdjBeta    <- stats$betaFull
  stats$biasAdjBetaPct <- stats$mainCoefPct
}

# Age heterogeneity
het_age_u40 <- het_models[["age_under40"]]
het_age_40p <- het_models[["age_40plus"]]
if (!is.null(het_age_u40)) {
  stats$ageUnderFortyCoef <- fmt_coef(coef(het_age_u40)["treated_x_post"] * 100, 1)
  stats$ageUnderFortyPval <- fmt_p(fixest::pvalue(het_age_u40)["treated_x_post"])
}
if (!is.null(het_age_40p)) {
  stats$ageFortyplusCoef <- fmt_coef(coef(het_age_40p)["treated_x_post"] * 100, 1)
  stats$ageFortyplusPval <- fmt_p(fixest::pvalue(het_age_40p)["treated_x_post"])
}

# Age equality test
het_tests_data <- readRDS(file.path(TAB_DIR, "heterogeneity_equality_tests.rds"))
age_test <- het_tests_data[group1 == "Under 40"]
if (nrow(age_test) > 0) {
  stats$ageEqZ <- sprintf("%.2f", age_test$z[1])
  stats$ageEqP <- sprintf("%.3f", age_test$p[1])
}

# Prime-age robustness
if (!is.null(rob_models$prime_age)) {
  stats$primeAgeCoef <- fmt_coef(coef(rob_models$prime_age)["treated_x_post"])
  stats$primeAgeCoefPct <- fmt_coef(coef(rob_models$prime_age)["treated_x_post"] * 100, 1)
  pa_p <- fixest::pvalue(rob_models$prime_age)["treated_x_post"]
  stats$primeAgePval <- fmt_p(pa_p)
  stats$primeAgeN    <- fmt_int(rob_models$prime_age$nobs)
}

# Above-median abstract-task robustness
if (!is.null(rob_models$above_med_abstract)) {
  stats$aboveMedAbsCoef <- fmt_coef(coef(rob_models$above_med_abstract)["treated_x_post"])
  stats$aboveMedAbsCoefPct <- fmt_coef(coef(rob_models$above_med_abstract)["treated_x_post"] * 100, 1)
  ama_p <- fixest::pvalue(rob_models$above_med_abstract)["treated_x_post"]
  stats$aboveMedAbsPval <- fmt_p(ama_p)
  stats$aboveMedAbsN    <- fmt_int(rob_models$above_med_abstract$nobs)
}

# Control group telework rate (for LATE caveat)
fs_ctrl <- did[YEAR >= 2022 & !is.na(TELWRKPAY) & TELWRKPAY > 0 & treated == 0]
if (nrow(fs_ctrl) > 0) {
  ctrl_tw_rate <- weighted.mean(fs_ctrl$TELWRKPAY == 1, fs_ctrl$EARNWT)
  stats$fsControlTelework <- fmt_coef(ctrl_tw_rate * 100, 1)
}

# Top 5 occupations by N in treatment and control
dn_names <- fread(file.path(BASE_DIR, "data", "raw", "dingel_neiman",
                             "onet_teleworkable_blscodes.csv"))
setnames(dn_names, c("OCC_CODE", "OES_TITLE"), c("soc_6digit", "occ_title"))
occ_counts <- did[, .(N = .N), by = .(OCC2010, soc_6digit, treated)]
occ_counts <- merge(occ_counts, dn_names[, .(soc_6digit, occ_title)],
                    by = "soc_6digit", all.x = TRUE)
occ_counts[is.na(occ_title), occ_title := soc_6digit]

top5_treat <- occ_counts[treated == 1][order(-N)][1:5]
top5_ctrl  <- occ_counts[treated == 0][order(-N)][1:5]

# Store as single-line macro for footnote
treat_list <- paste(sprintf("%s ($N$ = %s)", top5_treat$occ_title,
                            format(top5_treat$N, big.mark = ",")), collapse = "; ")
ctrl_list  <- paste(sprintf("%s ($N$ = %s)", top5_ctrl$occ_title,
                            format(top5_ctrl$N, big.mark = ",")), collapse = "; ")
stats$topFiveTreat <- treat_list
stats$topFiveCtrl  <- ctrl_list


# Write paper_stats.tex

lines <- c(
  "% paper_stats.tex - Auto-generated by code/07_paper_stats.R",
  paste0("% Generated: ", Sys.time()),
  "% DO NOT EDIT MANUALLY - re-run 07_paper_stats.R to update"
)

for (nm in sort(names(stats))) {
  lines <- c(lines, sprintf("\\newcommand{\\%s}{%s}", nm, stats[[nm]]))
}

out_path <- file.path(TAB_DIR, "paper_stats.tex")
writeLines(lines, out_path)

# Sync all output to paper/ directory for Overleaf
# (Addresses Referee 2 Round 3 minor concern: previously only paper_stats.tex was synced)
paper_tab_dir <- file.path(BASE_DIR, "paper", "tables")
paper_fig_dir <- file.path(BASE_DIR, "paper", "figures")

if (dir.exists(paper_tab_dir)) {
  tex_files <- list.files(TAB_DIR, pattern = "\\.tex$", full.names = TRUE)
  n_synced <- sum(sapply(tex_files, function(f) {
    file.copy(f, file.path(paper_tab_dir, basename(f)), overwrite = TRUE)
  }))
  print(sprintf("Synced %d .tex files to paper/tables/", n_synced))
}

if (dir.exists(paper_fig_dir)) {
  fig_files <- list.files(FIG_DIR, pattern = "\\.pdf$", full.names = TRUE)
  n_synced <- sum(sapply(fig_files, function(f) {
    file.copy(f, file.path(paper_fig_dir, basename(f)), overwrite = TRUE)
  }))
  print(sprintf("Synced %d .pdf files to paper/figures/", n_synced))
}

# Session info
si_path <- file.path(OUTPUT_DIR, "session_info.txt")
writeLines(capture.output(sessionInfo()), si_path)
