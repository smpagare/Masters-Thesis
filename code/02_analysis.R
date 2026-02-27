# 02_analysis.R - DID, event study, heterogeneity, robustness
# Siddhant Pagare, King's College London

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
SOURCED_BY_CHILD <- TRUE
source(here::here("code", "00_master.R"))

analytical <- readRDS(file.path(DATA_CLEAN, "analytical_sample.rds"))
did        <- readRDS(file.path(DATA_CLEAN, "did_sample.rds"))

nrow(analytical)
nrow(did)

# Factor encoding for fixest
did[, `:=`(
  occ_fe       = factor(OCC2010),
  yq_fe        = factor(year_quarter),
  state_fe     = factor(STATEFIP),
  ind_fe       = factor(industry),
  educ_fe      = factor(educ_cat),
  race_fe      = factor(race_eth),
  event_time_f = factor(event_time)
)]

analytical[, `:=`(
  occ_fe   = factor(OCC2010),
  yq_fe    = factor(year_quarter),
  state_fe = factor(STATEFIP),
  ind_fe   = factor(industry),
  educ_fe  = factor(educ_cat),
  race_fe  = factor(race_eth)
)]

# Reference period for event study: 2019Q4 (event_time = -1)
did[, event_time_f := relevel(event_time_f, ref = "-1")]


# Summary statistics

balance_vars <- c("real_hourly_wage", "log_wage", "AGE", "female", "married",
                  "has_children", "exper", "abstract", "routine", "manual",
                  "rti", "telework_feasible")

# Weighted means by treatment x period
tab1 <- did[, {
  out <- lapply(.SD, function(x) weighted.mean(x, w = EARNWT, na.rm = TRUE))
  c(out, list(N = .N))
}, by = .(treated, post), .SDcols = balance_vars]

print(tab1)

# Education distribution
tab1_educ <- did[, .(
  pct_less_hs      = weighted.mean(educ_cat == "1_less_hs", EARNWT),
  pct_hs           = weighted.mean(educ_cat == "2_hs_diploma", EARNWT),
  pct_some_college = weighted.mean(educ_cat == "3_some_college", EARNWT),
  pct_ba_plus      = weighted.mean(educ_cat == "4_bachelors_plus", EARNWT)
), by = .(treated, post)]
print(tab1_educ)

# Race/ethnicity
tab1_race <- did[, .(
  pct_white_nh  = weighted.mean(race_eth == "white_nh", EARNWT),
  pct_black_nh  = weighted.mean(race_eth == "black_nh", EARNWT),
  pct_hispanic  = weighted.mean(race_eth == "hispanic", EARNWT),
  pct_other_nh  = weighted.mean(race_eth == "other_nh", EARNWT)
), by = .(treated, post)]
print(tab1_race)

# Industry distribution
tab1_ind <- did[, .(N = .N, pct = .N / nrow(did)), by = .(treated, industry)][
  order(treated, -pct)]
print(tab1_ind[pct > 0.02])

# Wage distribution by quadrant
tab2 <- analytical[, .(
  N          = .N,
  mean_wage  = weighted.mean(real_hourly_wage, EARNWT),
  p10        = quantile(real_hourly_wage, 0.10),
  p25        = quantile(real_hourly_wage, 0.25),
  p50        = quantile(real_hourly_wage, 0.50),
  p75        = quantile(real_hourly_wage, 0.75),
  p90        = quantile(real_hourly_wage, 0.90),
  mean_abstract   = mean(abstract),
  mean_telework   = mean(telework_feasible)
), by = .(quadrant, post)][order(quadrant, post)]
print(tab2)

# First stage: actual telework adoption (CPS TELWRKPAY, Oct 2022+)
first_stage <- analytical[YEAR >= 2022 & !is.na(TELWRKPAY) & TELWRKPAY > 0, .(
  pct_telework_actual = weighted.mean(TELWRKPAY == 1, EARNWT),
  N = .N
), by = .(quadrant)]
print(first_stage)

first_stage_did <- did[YEAR >= 2022 & !is.na(TELWRKPAY) & TELWRKPAY > 0, .(
  pct_telework_actual = weighted.mean(TELWRKPAY == 1, EARNWT),
  N = .N
), by = .(treated)]
print(first_stage_did)

saveRDS(tab1, file.path(TAB_DIR, "tab1_balance.rds"))
saveRDS(tab2, file.path(TAB_DIR, "tab2_wages_by_quadrant.rds"))


# Baseline DID (7 specifications, progressive controls)

# OCC + YQ FE only
m1 <- feols(log_wage ~ treated_x_post | occ_fe + yq_fe,
            data = did, weights = ~EARNWT, cluster = ~OCC2010)

# + individual controls
m2 <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
              married + has_children |
              occ_fe + yq_fe,
            data = did, weights = ~EARNWT, cluster = ~OCC2010)

# + education and race FE
m3 <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
              married + has_children |
              occ_fe + yq_fe + educ_fe + race_fe,
            data = did, weights = ~EARNWT, cluster = ~OCC2010)

# + industry FE
m4 <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
              married + has_children |
              occ_fe + yq_fe + educ_fe + race_fe + ind_fe,
            data = did, weights = ~EARNWT, cluster = ~OCC2010)

# + state FE (PREFERRED)
m5 <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
              married + has_children |
              occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
            data = did, weights = ~EARNWT, cluster = ~OCC2010)

# RPP-adjusted wages
m6 <- feols(log_wage_rpp ~ treated_x_post + female + exper + exper2 +
              married + has_children |
              occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
            data = did[!is.na(log_wage_rpp)],
            weights = ~EARNWT, cluster = ~OCC2010)

# Industry x Year-Quarter FE (absorbs sector-specific pandemic shocks)
did[, ind_x_yq := interaction(industry, year_quarter, drop = TRUE)]
m7 <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
              married + has_children |
              occ_fe + yq_fe + educ_fe + race_fe + ind_x_yq + state_fe,
            data = did, weights = ~EARNWT, cluster = ~OCC2010)

etable(m1, m2, m3, m4, m5, m6, m7,
       keep = "%treated_x_post",
       headers = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6) RPP", "(7) Ind x YQ"),
       dict = c(treated_x_post = "Treated x Post"),
       se.below = TRUE, fitstat = ~ n + r2 + wr2)

did_models <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5, m6 = m6, m7 = m7)
saveRDS(did_models, file.path(TAB_DIR, "did_baseline_models.rds"))

modelsummary(
  list("(1)" = m1, "(2)" = m2, "(3)" = m3, "(4)" = m4,
       "(5)" = m5, "(6) RPP" = m6, "(7) Ind$\\times$YQ" = m7),
  coef_map = c("treated_x_post" = "Treated $\\times$ Post"),
  gof_map = c("nobs", "r.squared"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  output = file.path(TAB_DIR, "tab3_baseline_did.tex"),
  title = "Baseline Difference-in-Differences: Effect of Telework Access on Abstract-Task Wages",
  notes = c("Standard errors clustered at occupation level in parentheses.",
            "All specifications include occupation and year-quarter fixed effects.",
            "Sample: high-abstract occupations (top tercile), CPS-ORG 2017Q1-2023Q1.",
            "Column (7) includes industry $\\times$ year-quarter FE to absorb sector-specific pandemic shocks.")
)


# Event study

es <- feols(log_wage ~ i(event_time_f, treated, ref = "-1") +
              female + exper + exper2 + married + has_children |
              occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
            data = did, weights = ~EARNWT, cluster = ~OCC2010)

print(summary(es))

es_coefs <- as.data.table(coeftable(es))
es_coefs[, term := rownames(coeftable(es))]

es_plot_data <- es_coefs[grepl("event_time_f", term)]
es_plot_data[, event_time := as.integer(gsub("event_time_f::(-?\\d+):treated", "\\1", term))]
es_plot_data[, `:=`(
  estimate = Estimate,
  se       = `Std. Error`,
  ci_lo    = Estimate - 1.96 * `Std. Error`,
  ci_hi    = Estimate + 1.96 * `Std. Error`
)]

# Reference period (0 by construction)
ref_row <- data.table(event_time = -1L, estimate = 0, se = 0, ci_lo = 0, ci_hi = 0)
es_plot_data <- rbind(
  es_plot_data[, .(event_time, estimate, se, ci_lo, ci_hi)],
  ref_row
)[order(event_time)]

saveRDS(es_plot_data, file.path(TAB_DIR, "event_study_coefs.rds"))
saveRDS(es, file.path(TAB_DIR, "event_study_model.rds"))

# Joint F-test for pre-trends
pre_terms <- grep("event_time_f::-[2-9]|event_time_f::-1[0-9]", names(coef(es)), value = TRUE)
if (length(pre_terms) > 0) {
  pre_test <- wald(es, keep = pre_terms)
  print(pre_test)
}


# Continuous treatment DID

# Continuous telework feasibility x post (within high-abstract)
ct1 <- feols(log_wage ~ telework_feasible:post + female + exper + exper2 +
               married + has_children |
               occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
             data = did, weights = ~EARNWT, cluster = ~OCC2010)
print(summary(ct1))

# Triple-difference on full sample: abstract x telework x post
ct2 <- feols(log_wage ~ telework_feasible:post + abstract:post +
               telework_feasible:abstract:post +
               female + exper + exper2 + married + has_children |
               occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
             data = analytical, weights = ~EARNWT, cluster = ~OCC2010)
print(summary(ct2))

# Continuous event study
ct_es <- feols(log_wage ~ i(event_time_f, telework_feasible, ref = "-1") +
                 female + exper + exper2 + married + has_children |
                 occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
               data = did, weights = ~EARNWT, cluster = ~OCC2010)

ct_es_coefs <- as.data.table(coeftable(ct_es))
ct_es_coefs[, term := rownames(coeftable(ct_es))]
ct_es_coefs <- ct_es_coefs[grepl("event_time_f", term)]
ct_es_coefs[, event_time := as.integer(gsub("event_time_f::(-?\\d+):telework_feasible", "\\1", term))]
ct_es_coefs[, `:=`(
  estimate = Estimate, se = `Std. Error`,
  ci_lo = Estimate - 1.96 * `Std. Error`,
  ci_hi = Estimate + 1.96 * `Std. Error`
)]

ref_row_ct <- data.table(event_time = -1L, estimate = 0, se = 0, ci_lo = 0, ci_hi = 0)
ct_es_data <- rbind(
  ct_es_coefs[, .(event_time, estimate, se, ci_lo, ci_hi)],
  ref_row_ct
)[order(event_time)]

saveRDS(ct_es_data, file.path(TAB_DIR, "continuous_event_study_coefs.rds"))
saveRDS(list(ct1 = ct1, ct2 = ct2, ct_es = ct_es),
        file.path(TAB_DIR, "continuous_did_models.rds"))


# Heterogeneity

# Gender interaction
het_gender <- feols(log_wage ~ treated_x_post * female +
                      exper + exper2 + married + has_children |
                      occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                    data = did, weights = ~EARNWT, cluster = ~OCC2010)

# Split sample: gender
m_male <- feols(log_wage ~ treated_x_post + exper + exper2 +
                   married + has_children |
                   occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                 data = did[female == 0],
                 weights = ~EARNWT, cluster = ~OCC2010)

m_female <- feols(log_wage ~ treated_x_post + exper + exper2 +
                     married + has_children |
                     occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                   data = did[female == 1],
                   weights = ~EARNWT, cluster = ~OCC2010)

etable(m_male, m_female, keep = "%treated_x_post",
       headers = c("Male", "Female"), se.below = TRUE)

# Parental status
het_parent <- feols(log_wage ~ treated_x_post * has_children +
                      female + exper + exper2 + married |
                      occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                    data = did, weights = ~EARNWT, cluster = ~OCC2010)

# Metro vs non-metro
# NOTE: The interaction spec (treated_x_post * metro) with state FE
# produces near-collinearity because metro is highly correlated with state.
# Split-sample estimates are more reliable here.
het_metro <- feols(log_wage ~ treated_x_post * i(metro) +
                     female + exper + exper2 + married + has_children |
                     occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                   data = did[!is.na(metro)],
                   weights = ~EARNWT, cluster = ~OCC2010)

m_metro <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                   married + has_children |
                   occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                 data = did[metro == 1],
                 weights = ~EARNWT, cluster = ~OCC2010)

m_nonmetro <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                      married + has_children |
                      occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                    data = did[metro == 0],
                    weights = ~EARNWT, cluster = ~OCC2010)

etable(m_metro, m_nonmetro, keep = "%treated_x_post",
       headers = c("Metro", "Non-Metro"), se.below = TRUE)

# Education
m_ba <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                married + has_children |
                occ_fe + yq_fe + race_fe + ind_fe + state_fe,
              data = did[educ_cat == "4_bachelors_plus"],
              weights = ~EARNWT, cluster = ~OCC2010)

m_no_ba <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                   married + has_children |
                   occ_fe + yq_fe + race_fe + ind_fe + state_fe,
                 data = did[educ_cat != "4_bachelors_plus"],
                 weights = ~EARNWT, cluster = ~OCC2010)

etable(m_ba, m_no_ba, keep = "%treated_x_post",
       headers = c("BA+", "No BA"), se.below = TRUE)

# Union status
m_union <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                   married + has_children |
                   occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                 data = did[UNION == 2],  # 2 = member
                 weights = ~EARNWT, cluster = ~OCC2010)

m_nonunion <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                      married + has_children |
                      occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                    data = did[UNION == 1],  # 1 = not member
                    weights = ~EARNWT, cluster = ~OCC2010)

etable(m_union, m_nonunion, keep = "%treated_x_post",
       headers = c("Union", "Non-Union"), se.below = TRUE)

het_models <- list(
  gender = het_gender, parent = het_parent, metro = het_metro,
  male = m_male, female = m_female,
  metro_only = m_metro, nonmetro_only = m_nonmetro,
  ba_plus = m_ba, no_ba = m_no_ba,
  union = m_union, nonunion = m_nonunion
)
saveRDS(het_models, file.path(TAB_DIR, "heterogeneity_models.rds"))

modelsummary(
  list("Male" = m_male, "Female" = m_female,
       "Metro" = m_metro, "Non-Metro" = m_nonmetro,
       "BA+" = m_ba, "No BA" = m_no_ba),
  coef_map = c("treated_x_post" = "Treated $\\times$ Post"),
  gof_map = c("nobs", "r.squared"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  output = file.path(TAB_DIR, "tab4_heterogeneity.tex"),
  title = "Heterogeneity in the Telework-Wage Effect",
  notes = "Standard errors clustered at occupation level in parentheses."
)

# Formal equality tests across subgroups (Wald-type, conservative)
wald_equality_test <- function(mod1, mod2, label1, label2, coef_name = "treated_x_post") {
  b1  <- coef(mod1)[coef_name]
  b2  <- coef(mod2)[coef_name]
  se1 <- sqrt(vcov(mod1)[coef_name, coef_name])
  se2 <- sqrt(vcov(mod2)[coef_name, coef_name])
  diff <- b1 - b2
  se_diff <- sqrt(se1^2 + se2^2)
  z <- diff / se_diff
  p <- 2 * (1 - pnorm(abs(z)))
  data.table(group1 = label1, group2 = label2, diff = diff,
             se_diff = se_diff, z = z, p = p)
}

equality_tests <- rbind(
  wald_equality_test(m_metro, m_nonmetro, "Metro", "Non-metro"),
  wald_equality_test(m_male, m_female, "Male", "Female"),
  wald_equality_test(m_ba, m_no_ba, "BA+", "No BA"),
  wald_equality_test(m_union, m_nonunion, "Union", "Non-union")
)
print(equality_tests)
saveRDS(equality_tests, file.path(TAB_DIR, "heterogeneity_equality_tests.rds"))


# Robustness checks

# Placebo: fake treatment at 2018Q1 using pre-period only
pre_data <- did[post == 0]
pre_data[, placebo_post := as.integer(YEAR >= 2018)]
pre_data[, placebo_treat_x_post := treated * placebo_post]

placebo <- feols(log_wage ~ placebo_treat_x_post + female + exper + exper2 +
                   married + has_children |
                   occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                 data = pre_data, weights = ~EARNWT, cluster = ~OCC2010)
print(summary(placebo))

# Exclude pandemic peak (2020Q2-Q4)
did_nopeak <- did[!(YEAR == 2020 & quarter %in% c(2, 3, 4))]
m_nopeak <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                    married + has_children |
                    occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                  data = did_nopeak, weights = ~EARNWT, cluster = ~OCC2010)

etable(m5, m_nopeak, keep = "%treated_x_post",
       headers = c("Full", "No Peak"), se.below = TRUE)

# Alternative abstract thresholds
abstract_p75 <- quantile(analytical$abstract, 0.75, na.rm = TRUE)
did_q75 <- analytical[abstract >= abstract_p75]
did_q75[, `:=`(occ_fe = factor(OCC2010), yq_fe = factor(year_quarter),
               state_fe = factor(STATEFIP), ind_fe = factor(industry),
               educ_fe = factor(educ_cat), race_fe = factor(race_eth))]

m_q75 <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                 married + has_children |
                 occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
               data = did_q75, weights = ~EARNWT, cluster = ~OCC2010)

abstract_p60 <- quantile(analytical$abstract, 0.60, na.rm = TRUE)
did_p60 <- analytical[abstract >= abstract_p60]
did_p60[, `:=`(occ_fe = factor(OCC2010), yq_fe = factor(year_quarter),
               state_fe = factor(STATEFIP), ind_fe = factor(industry),
               educ_fe = factor(educ_cat), race_fe = factor(race_eth))]

m_p60 <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                 married + has_children |
                 occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
               data = did_p60, weights = ~EARNWT, cluster = ~OCC2010)

etable(m5, m_q75, m_p60, keep = "%treated_x_post",
       headers = c("Top Tercile", "Top Quartile", "Top 40%"),
       se.below = TRUE, fitstat = ~ n + r2 + wr2)

# Double clustering (occupation + state)
m_dblclust <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                      married + has_children |
                      occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                    data = did, weights = ~EARNWT,
                    cluster = ~OCC2010 + STATEFIP)

# Unweighted
m_unweight <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                      married + has_children |
                      occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                    data = did, cluster = ~OCC2010)

# Post-2021 only (structural telework, not emergency WFH)
did_post21 <- did[YEAR <= 2019 | YEAR >= 2021]
did_post21[, post21 := as.integer(YEAR >= 2021)]
did_post21[, treat_x_post21 := treated * post21]

m_post21 <- feols(log_wage ~ treat_x_post21 + female + exper + exper2 +
                    married + has_children |
                    occ_fe + factor(year_quarter) + educ_fe + race_fe + ind_fe + state_fe,
                  data = did_post21, weights = ~EARNWT, cluster = ~OCC2010)

robust_models <- list(
  placebo = placebo, no_peak = m_nopeak,
  q75 = m_q75, p60 = m_p60,
  dbl_cluster = m_dblclust, unweighted = m_unweight,
  post21 = m_post21, ind_x_yq = m7
)
saveRDS(robust_models, file.path(TAB_DIR, "robustness_models.rds"))

modelsummary(
  list("Baseline" = m5, "Placebo" = placebo,
       "No 2020 Peak" = m_nopeak, "Top Quartile" = m_q75,
       "Top 40\\%" = m_p60, "Double Cluster" = m_dblclust,
       "Unweighted" = m_unweight, "Ind$\\times$YQ FE" = m7),
  coef_map = c("treated_x_post" = "Treated $\\times$ Post",
               "placebo_treat_x_post" = "Placebo $\\times$ Post"),
  gof_map = c("nobs", "r.squared"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  output = file.path(TAB_DIR, "tab5_robustness.tex"),
  title = "Robustness Checks",
  notes = c("Standard errors clustered at occupation level in parentheses.",
            "Column (1) reproduces the preferred specification from Table 3.")
)


# Goodman-Bacon decomposition at occupation-quarter level

occ_panel <- did[, .(
  mean_log_wage = weighted.mean(log_wage, EARNWT),
  treated       = first(treated),
  post          = first(post),
  N             = .N
), by = .(OCC2010, year_quarter, YEAR, quarter)]
occ_panel[, treated_x_post := treated * post]

m_occ <- feols(mean_log_wage ~ treated_x_post | OCC2010 + year_quarter,
               data = occ_panel, weights = ~N)
print(summary(m_occ))


# Event study with Industry x YQ FE (saturated spec)

es_indyq <- feols(log_wage ~ i(event_time_f, treated, ref = "-1") +
                    female + exper + exper2 + married + has_children |
                    occ_fe + yq_fe + educ_fe + race_fe + ind_x_yq + state_fe,
                  data = did, weights = ~EARNWT, cluster = ~OCC2010)

es_indyq_coefs <- as.data.table(coeftable(es_indyq))
es_indyq_coefs[, term := rownames(coeftable(es_indyq))]
es_indyq_coefs <- es_indyq_coefs[grepl("event_time_f", term)]
es_indyq_coefs[, event_time := as.integer(gsub("event_time_f::(-?\\d+):treated", "\\1", term))]
es_indyq_coefs[, `:=`(
  estimate = Estimate, se = `Std. Error`,
  ci_lo = Estimate - 1.96 * `Std. Error`,
  ci_hi = Estimate + 1.96 * `Std. Error`
)]

ref_row_indyq <- data.table(event_time = -1L, estimate = 0, se = 0, ci_lo = 0, ci_hi = 0)
es_indyq_data <- rbind(
  es_indyq_coefs[, .(event_time, estimate, se, ci_lo, ci_hi)],
  ref_row_indyq
)[order(event_time)]

saveRDS(es_indyq_data, file.path(TAB_DIR, "event_study_indyq_coefs.rds"))
saveRDS(es_indyq, file.path(TAB_DIR, "event_study_indyq_model.rds"))

# Pre-trend test for saturated spec
pre_terms_indyq <- grep("event_time_f::-[2-9]|event_time_f::-1[0-9]",
                         names(coef(es_indyq)), value = TRUE)
if (length(pre_terms_indyq) > 0) {
  pre_test_indyq <- wald(es_indyq, keep = pre_terms_indyq)
  print(pre_test_indyq)
  saveRDS(pre_test_indyq, file.path(TAB_DIR, "pre_test_indyq.rds"))
}


# Honest DID - Rambachan & Roth (2023) sensitivity

library(HonestDiD)

es_model <- es

all_coef_names <- names(coef(es_model))
es_coef_names <- all_coef_names[grepl("event_time_f::", all_coef_names)]
pre_coef_names  <- es_coef_names[grepl("::-", es_coef_names)]
post_coef_names <- es_coef_names[!grepl("::-", es_coef_names)]

n_pre  <- length(pre_coef_names)
n_post <- length(post_coef_names)

all_beta <- coef(es_model)
all_vcov <- vcov(es_model)
es_idx <- which(all_coef_names %in% es_coef_names)

betahat <- all_beta[es_idx]
sigma   <- all_vcov[es_idx, es_idx]

es_event_times <- as.integer(gsub("event_time_f::(-?\\d+):treated", "\\1",
                                   names(betahat)))
ord <- order(es_event_times)
betahat <- betahat[ord]
sigma   <- sigma[ord, ord]
es_event_times_sorted <- es_event_times[ord]

pre_idx_in_vec  <- which(es_event_times_sorted < 0)
post_idx_in_vec <- which(es_event_times_sorted >= 0)
l_pre  <- length(pre_idx_in_vec)
l_post <- length(post_idx_in_vec)

# Relative magnitudes approach
honest_rm <- createSensitivityResults_relativeMagnitudes(
  betahat = betahat, sigma = sigma,
  numPrePeriods = l_pre, numPostPeriods = l_post,
  Mbarvec = seq(0, 2, by = 0.5),
  l_vec = basisVector(1, l_post)
)
print(honest_rm)
saveRDS(honest_rm, file.path(TAB_DIR, "honest_did_rm.rds"))

# Smoothness restriction (Delta^SD)
honest_sd <- createSensitivityResults(
  betahat = betahat, sigma = sigma,
  numPrePeriods = l_pre, numPostPeriods = l_post,
  Mvec = seq(0, 0.04, by = 0.01),
  l_vec = basisVector(1, l_post)
)
print(honest_sd)
saveRDS(honest_sd, file.path(TAB_DIR, "honest_did_sd.rds"))

# Average post-treatment effect
avg_l_vec <- rep(1/l_post, l_post)

honest_rm_avg <- createSensitivityResults_relativeMagnitudes(
  betahat = betahat, sigma = sigma,
  numPrePeriods = l_pre, numPostPeriods = l_post,
  Mbarvec = seq(0, 2, by = 0.5),
  l_vec = avg_l_vec
)
print(honest_rm_avg)
saveRDS(honest_rm_avg, file.path(TAB_DIR, "honest_did_rm_avg.rds"))

honest_sd_avg <- createSensitivityResults(
  betahat = betahat, sigma = sigma,
  numPrePeriods = l_pre, numPostPeriods = l_post,
  Mvec = seq(0, 0.04, by = 0.01),
  l_vec = avg_l_vec
)
print(honest_sd_avg)
saveRDS(honest_sd_avg, file.path(TAB_DIR, "honest_did_sd_avg.rds"))


# Additional analyses (revision round)

# --- Binned tercile DN specification (nonlinearity check) ---
# Use ntile() because telework_feasible has mass points at 0 and 1
# Pre-compute interactions to avoid collinearity with post absorbed by yq_fe
did[, tw_tercile := dplyr::ntile(telework_feasible, 3)]
did[, tw_mid := as.integer(tw_tercile == 2)]
did[, tw_high := as.integer(tw_tercile == 3)]
did[, tw_mid_x_post := tw_mid * post]
did[, tw_high_x_post := tw_high * post]

ct_tercile <- feols(log_wage ~ tw_mid_x_post + tw_high_x_post +
                      female + exper + exper2 + married + has_children |
                      occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                    data = did, weights = ~EARNWT, cluster = ~OCC2010)
print(summary(ct_tercile))
saveRDS(ct_tercile, file.path(TAB_DIR, "tercile_dn_model.rds"))


# --- Metro / non-metro separate event studies ---
extract_es_coefs <- function(model) {
  ct <- as.data.table(coeftable(model))
  ct[, term := rownames(coeftable(model))]
  ct <- ct[grepl("event_time_f", term)]
  ct[, event_time := as.integer(gsub("event_time_f::(-?\\d+):treated", "\\1", term))]
  ct[, .(event_time, estimate = Estimate, se = `Std. Error`,
         ci_lo = Estimate - 1.96 * `Std. Error`,
         ci_hi = Estimate + 1.96 * `Std. Error`)]
}

es_metro <- feols(log_wage ~ i(event_time_f, treated, ref = "-1") +
                    female + exper + exper2 + married + has_children |
                    occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                  data = did[metro == 1],
                  weights = ~EARNWT, cluster = ~OCC2010)

es_nonmetro <- feols(log_wage ~ i(event_time_f, treated, ref = "-1") +
                       female + exper + exper2 + married + has_children |
                       occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                     data = did[metro == 0],
                     weights = ~EARNWT, cluster = ~OCC2010)

es_metro_coefs <- rbind(
  extract_es_coefs(es_metro),
  data.table(event_time = -1L, estimate = 0, se = 0, ci_lo = 0, ci_hi = 0)
)[order(event_time)]

es_nonmetro_coefs <- rbind(
  extract_es_coefs(es_nonmetro),
  data.table(event_time = -1L, estimate = 0, se = 0, ci_lo = 0, ci_hi = 0)
)[order(event_time)]

saveRDS(list(metro = es_metro_coefs, nonmetro = es_nonmetro_coefs),
        file.path(TAB_DIR, "metro_nonmetro_es_coefs.rds"))
print("Metro event study:")
print(summary(es_metro))
print("Non-metro event study:")
print(summary(es_nonmetro))


# --- Healthcare SOC exclusion robustness ---
did[, soc_major := substr(soc_6digit, 1, 2)]
n_hc_ctrl <- did[treated == 0 & soc_major == "29", .N]
print(sprintf("Healthcare practitioners in control group: %d observations", n_hc_ctrl))

did_no_health <- did[!(treated == 0 & soc_major == "29")]

m_no_health <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                       married + has_children |
                       occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                     data = did_no_health, weights = ~EARNWT, cluster = ~OCC2010)
print(summary(m_no_health))

# Append to robustness models
robust_models$no_healthcare <- m_no_health


# --- Age heterogeneity (under 40 vs 40+) ---
m_age_under40 <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                          married + has_children |
                          occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                        data = did[AGE < 40],
                        weights = ~EARNWT, cluster = ~OCC2010)

m_age_40plus <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                        married + has_children |
                        occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                      data = did[AGE >= 40],
                      weights = ~EARNWT, cluster = ~OCC2010)

print("Age heterogeneity:")
etable(m_age_under40, m_age_40plus, keep = "%treated_x_post",
       headers = c("Under 40", "40+"), se.below = TRUE)

het_models$age_under40 <- m_age_under40
het_models$age_40plus  <- m_age_40plus
saveRDS(het_models, file.path(TAB_DIR, "heterogeneity_models.rds"))

# Age equality test
age_eq <- wald_equality_test(m_age_under40, m_age_40plus, "Under 40", "40+")
print(age_eq)
equality_tests <- rbind(equality_tests, age_eq)
saveRDS(equality_tests, file.path(TAB_DIR, "heterogeneity_equality_tests.rds"))


# --- Prime-age robustness (25-54) ---
did_prime <- did[AGE >= 25 & AGE <= 54]
print(sprintf("Prime-age sample: %d observations", nrow(did_prime)))

m_prime_age <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                       married + has_children |
                       occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                     data = did_prime, weights = ~EARNWT, cluster = ~OCC2010)
print(summary(m_prime_age))

robust_models$prime_age <- m_prime_age

# --- Above-median abstract-task robustness (Option A from v3 review) ---
# Restrict to occupations above the median abstract-task score within the
# top-tercile sample, addressing concerns about borderline occupations
med_abstract <- median(did$abstract, na.rm = TRUE)
did_high_abs <- did[abstract > med_abstract]
print(sprintf("Above-median abstract sample: %d observations (median = %.3f)",
              nrow(did_high_abs), med_abstract))

m_above_med_abs <- feols(log_wage ~ treated_x_post + female + exper + exper2 +
                           married + has_children |
                           occ_fe + yq_fe + educ_fe + race_fe + ind_fe + state_fe,
                         data = did_high_abs, weights = ~EARNWT, cluster = ~OCC2010)
print(summary(m_above_med_abs))

robust_models$above_med_abstract <- m_above_med_abs
saveRDS(robust_models, file.path(TAB_DIR, "robustness_models.rds"))
