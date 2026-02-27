# 01_clean_data.R - Build analytical sample from raw data
# Pipeline: O*NET task indices -> Dingel-Neiman -> crosswalks -> RPP -> CPS -> merge

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
SOURCED_BY_CHILD <- TRUE
source(here::here("code", "00_master.R"))

# Track sample attrition at each stage
attrition <- list()


# Part A: O*NET task indices

work_act <- fread(file.path(ONET_DIR, "Work Activities.txt"))
setnames(work_act, c("O*NET-SOC Code", "Element ID", "Element Name",
                      "Scale ID", "Data Value", "Recommend Suppress"),
         c("onet_soc", "element_id", "element_name",
           "scale_id", "value", "suppress"))

work_ctx <- fread(file.path(ONET_DIR, "Work Context.txt"))
setnames(work_ctx, c("O*NET-SOC Code", "Element ID", "Element Name",
                      "Scale ID", "Data Value", "Recommend Suppress"),
         c("onet_soc", "element_id", "element_name",
           "scale_id", "value", "suppress"))

# Abstract index (Work Activities, Importance scale)
# Following Autor, Levy, Murnane (2003)
abstract_elements <- c(
  "4.A.2.a.4",  # Analyzing Data or Information
  "4.A.2.b.2",  # Thinking Creatively
  "4.A.4.a.1",  # Interpreting Meaning of Information
  "4.A.4.a.4"   # Establishing Interpersonal Relationships
)

abstract_raw <- work_act[element_id %in% abstract_elements &
                           scale_id == "IM" &
                           (is.na(suppress) | suppress != "Y"),
                         .(abstract_raw = mean(value, na.rm = TRUE)),
                         by = onet_soc]

# Routine index (Work Context, CX scale)
routine_elements <- c(
  "4.C.3.b.7",  # Importance of Repeating Same Tasks
  "4.C.3.b.4",  # Importance of Being Exact or Accurate
  "4.C.3.b.8",  # Structured vs Unstructured Work
  "4.C.3.d.3"   # Pace Determined by Speed of Equipment
)

routine_raw <- work_ctx[element_id %in% routine_elements &
                          scale_id == "CX" &
                          (is.na(suppress) | suppress != "Y"),
                        .(routine_raw = mean(value, na.rm = TRUE)),
                        by = onet_soc]

# Manual index (WA Importance + WC Context)
manual_wa <- work_act[element_id == "4.A.3.a.4" &
                        scale_id == "IM" &
                        (is.na(suppress) | suppress != "Y"),
                      .(onet_soc, value)]

manual_wc <- work_ctx[element_id == "4.C.2.d.1.g" &
                        scale_id == "CX" &
                        (is.na(suppress) | suppress != "Y"),
                      .(onet_soc, value)]

manual_raw <- rbind(manual_wa, manual_wc)[,
  .(manual_raw = mean(value, na.rm = TRUE)), by = onet_soc]

# Merge and aggregate to 6-digit SOC
task_onet <- merge(abstract_raw, routine_raw, by = "onet_soc", all = TRUE)
task_onet <- merge(task_onet, manual_raw, by = "onet_soc", all = TRUE)
task_onet[, soc_6digit := str_extract(onet_soc, "^\\d{2}-\\d{4}")]

task_by_soc <- task_onet[,
  .(abstract_raw = mean(abstract_raw, na.rm = TRUE),
    routine_raw  = mean(routine_raw, na.rm = TRUE),
    manual_raw   = mean(manual_raw, na.rm = TRUE),
    n_onet_codes = .N),
  by = soc_6digit]

# Standardize
task_by_soc[, abstract := as.numeric(scale(abstract_raw))]
task_by_soc[, routine  := as.numeric(scale(routine_raw))]
task_by_soc[, manual   := as.numeric(scale(manual_raw))]
task_by_soc[, rti      := log(routine_raw) - log(abstract_raw) - log(manual_raw)]

nrow(task_by_soc) # should be ~800 SOCs


# Part B: Dingel-Neiman telework feasibility

dn <- fread(file.path(DN_DIR, "onet_teleworkable_blscodes.csv"))
setnames(dn, c("OCC_CODE", "OES_TITLE", "teleworkable"),
         c("soc_6digit", "dn_title", "telework_feasible"))

# Merge task indices with DN telework scores
n_before_dn <- nrow(task_by_soc)
task_tw <- merge(task_by_soc, dn[, .(soc_6digit, telework_feasible)],
                 by = "soc_6digit", all.x = TRUE)
n_exact <- sum(!is.na(task_tw$telework_feasible))
print(sprintf("DN exact merge: %d / %d matched (%.1f%%)",
              n_exact, nrow(task_tw), 100 * n_exact / nrow(task_tw)))

# Broad SOC fallback for unmatched
task_tw[, soc_broad := substr(soc_6digit, 1, 6)]
dn_broad <- dn[, .(telework_broad = mean(telework_feasible, na.rm = TRUE)),
               by = .(soc_broad = substr(soc_6digit, 1, 6))]
task_tw <- merge(task_tw, dn_broad, by = "soc_broad", all.x = TRUE)
n_fallback <- sum(is.na(task_tw$telework_feasible) & !is.na(task_tw$telework_broad))
task_tw[is.na(telework_feasible), telework_feasible := telework_broad]
task_tw[, c("soc_broad", "telework_broad") := NULL]
print(sprintf("DN broad fallback: %d SOCs filled", n_fallback))

task_tw[, telework_binary := as.integer(telework_feasible >= 0.5)]


# Part C: Census OCC -> SOC crosswalks

cw_2010 <- as.data.table(read_xls(file.path(CW_DIR, "census2010_occ_to_2010soc_crosswalk.xls"),
                                    skip = 4))
soc_col <- names(cw_2010)[grep("[Ss][Oo][Cc]", names(cw_2010))][1]
occ_col <- names(cw_2010)[grep("[Cc]ensus.*[Cc]ode", names(cw_2010))][1]
cw_2010 <- cw_2010[, .SD, .SDcols = c(occ_col, soc_col)]
setnames(cw_2010, c("census_occ_2010", "soc_2010"))
cw_2010 <- cw_2010[!is.na(census_occ_2010) & !is.na(soc_2010)]
cw_2010[, census_occ_2010 := str_pad(as.character(census_occ_2010), 4, pad = "0")]
cw_2010[, soc_6digit := str_extract(as.character(soc_2010), "\\d{2}-\\d{4}")]
cw_2010 <- cw_2010[!is.na(soc_6digit)]
cw_2010 <- unique(cw_2010[, .(census_occ_2010, soc_6digit)])
cw_2010 <- cw_2010[, .SD[1], by = census_occ_2010]  # keep first match

nrow(cw_2010) # crosswalk mappings


# Part D: Regional price parities

rpp_raw <- fread(file.path(RPP_DIR, "SARPP_STATE_2008_2024.csv"), fill = TRUE)
rpp_raw <- rpp_raw[LineCode == 1]  # "All items" RPP

year_cols <- names(rpp_raw)[grep("^\\d{4}$", names(rpp_raw))]
rpp <- melt(rpp_raw, id.vars = c("GeoFIPS", "GeoName"),
            measure.vars = year_cols,
            variable.name = "year", value.name = "rpp_index")
rpp[, year := as.integer(as.character(year))]
rpp[, statefip := as.integer(substr(str_extract(GeoFIPS, "\\d+"), 1, 2))]
rpp[, rpp_index := as.numeric(rpp_index)]
rpp <- rpp[year >= 2017 & statefip > 0, .(statefip, year, rpp_index)]

# Extrapolate 2025 from 2024
if (max(rpp$year) < 2025) {
  rpp_ext <- rpp[year == max(year)]
  rpp_ext[, year := 2025L]
  rpp <- rbind(rpp, rpp_ext)
}


# Part E: Clean CPS-MORG

cps <- fread(file.path(CPS_DIR, "cps_00031.csv"), showProgress = TRUE)
attrition$raw_cps <- nrow(cps)

# ORG sample: MIS 4 or 8, age 18-64, private/govt wage workers, employed
cps <- cps[MISH %in% c(4L, 8L) &
             AGE >= 18L & AGE <= 64L &
             CLASSWKR >= 20L & CLASSWKR <= 28L &
             EARNWT > 0 &
             EMPSTAT %in% c(10L, 12L)]
attrition$org_filter <- nrow(cps)

# Drop allocated wages (Hirsch-Schumacher correction)
cps <- cps[QHOURWAG == 0 & QEARNWEE == 0]
attrition$drop_allocated <- nrow(cps)

# Construct hourly wage
cps[, hourly_wage := fcase(
  PAIDHOUR == 2L & HOURWAGE > 0 & HOURWAGE < 999, as.double(HOURWAGE),
  PAIDHOUR == 1L & EARNWEEK > 0 & EARNWEEK < 9999 &
    UHRSWORKORG > 0 & UHRSWORKORG < 999,
  as.double(EARNWEEK) / as.double(UHRSWORKORG),
  default = NA_real_
)]
cps <- cps[!is.na(hourly_wage)]
attrition$wage_constructed <- nrow(cps)

# Top-code correction: 1.5x multiplier for top-coded weekly earnings
# CPS replaces top-coded EARNWEEK with the mean above the threshold
cps[, topcode_val := max(EARNWEEK[EARNWEEK < 9999], na.rm = TRUE), by = YEAR]
cps[, is_topcode := (PAIDHOUR == 1L & EARNWEEK >= topcode_val)]
cps[is_topcode == TRUE, hourly_wage := hourly_wage * 1.5]
cps[, c("topcode_val", "is_topcode") := NULL]

# Trim extreme wages
cps <- cps[hourly_wage >= 3.00 & hourly_wage <= 300.00]
attrition$wage_trimmed <- nrow(cps)

# Deflate to real 2019 dollars
cps <- merge(cps, cpi_u_rs, by = "YEAR", all.x = TRUE)
cps[, real_hourly_wage := hourly_wage * 100 / cpi_2019]
cps[, log_wage := log(real_hourly_wage)]

# Demographics
cps[, `:=`(
  educ_cat = fcase(
    EDUC <= 72L,                   "1_less_hs",
    EDUC == 73L,                   "2_hs_diploma",
    EDUC %in% c(81L, 91L, 92L),   "3_some_college",
    EDUC >= 111L,                  "4_bachelors_plus",
    default = NA_character_
  ),
  female = as.integer(SEX == 2L),
  race_eth = fcase(
    HISPAN > 0L & HISPAN < 900L,  "hispanic",
    RACE == 100L,                  "white_nh",
    RACE == 200L,                  "black_nh",
    default = "other_nh"
  ),
  married = as.integer(MARST %in% c(1L, 2L)),
  metro = fcase(
    METRO %in% c(2L, 3L, 4L), 1L,
    METRO == 1L,               0L,
    default = NA_integer_
  ),
  has_children = as.integer(NCHILD > 0L)
)]

# Potential experience (Mincer)
cps[, years_educ := fcase(
  EDUC <= 60L,  10L, EDUC == 71L,  11L, EDUC == 73L,  12L,
  EDUC == 81L,  13L, EDUC %in% c(91L, 92L), 14L,
  EDUC == 111L, 16L, EDUC == 123L, 18L,
  EDUC == 124L, 20L, EDUC == 125L, 21L,
  default = 12L
)]
cps[, exper  := pmax(AGE - years_educ - 6L, 0L)]
cps[, exper2 := exper^2 / 100]

# Industry (13 sectors)
cps[, industry := fcase(
  IND >= 170L  & IND <= 290L,  "agriculture",
  IND >= 370L  & IND <= 490L,  "mining",
  IND == 770L,                  "construction",
  IND >= 1070L & IND <= 3990L, "manufacturing",
  IND >= 4070L & IND <= 4590L, "wholesale_trade",
  IND >= 4670L & IND <= 5790L, "retail_trade",
  IND >= 6070L & IND <= 6390L, "transport_utilities",
  IND >= 6470L & IND <= 6780L, "information",
  IND >= 6870L & IND <= 7190L, "finance_insurance_re",
  IND >= 7270L & IND <= 7790L, "prof_business_svc",
  IND >= 7860L & IND <= 8470L, "educ_health",
  IND >= 8560L & IND <= 8690L, "leisure_hospitality",
  IND >= 8770L & IND <= 9590L, "other_services_govt",
  default = "other"
)]

# Time variables
cps[, quarter := ceiling(MONTH / 3L)]
cps[, year_quarter := paste0(YEAR, "Q", quarter)]
cps[, event_time := (YEAR - 2020L) * 4L + (quarter - 1L)]
cps[, post := as.integer(YEAR >= 2020L)]


# Part F: Merge CPS with task indices + telework + RPP

cps[, occ2010_padded := str_pad(as.character(OCC2010), 4, pad = "0")]

n_pre_soc <- nrow(cps)
cps <- merge(cps, cw_2010, by.x = "occ2010_padded", by.y = "census_occ_2010",
             all.x = TRUE)
print(sprintf("CPS -> SOC: %d / %d matched (%.1f%%)",
              sum(!is.na(cps$soc_6digit)), nrow(cps),
              100 * mean(!is.na(cps$soc_6digit))))

# Merge task indices + telework
cps <- merge(cps, task_tw[, .(soc_6digit, abstract, routine, manual, rti,
                               abstract_raw, routine_raw, manual_raw,
                               telework_feasible, telework_binary)],
             by = "soc_6digit", all.x = TRUE)

# Broad SOC fallback for workers whose 6-digit SOC didn't match
# Uses substr(1, 6) = 5-digit SOC minor group (e.g., "XX-XXX"), consistent with
# the DN telework fallback in Part B (line 105). Previously used substr(1, 5) = 4-digit
# broad occupation, creating an inconsistency (M3 fix from Referee 2 Round 3).
task_broad <- task_tw[, .(
  abstract_b = mean(abstract, na.rm = TRUE),
  routine_b  = mean(routine, na.rm = TRUE),
  manual_b   = mean(manual, na.rm = TRUE),
  rti_b      = mean(rti, na.rm = TRUE),
  abstract_raw_b = mean(abstract_raw, na.rm = TRUE),
  routine_raw_b  = mean(routine_raw, na.rm = TRUE),
  manual_raw_b   = mean(manual_raw, na.rm = TRUE),
  tw_b           = mean(telework_feasible, na.rm = TRUE),
  tw_bin_b       = as.integer(mean(telework_feasible, na.rm = TRUE) >= 0.5)
), by = .(soc_broad = substr(soc_6digit, 1, 6))]

cps[, soc_broad := substr(soc_6digit, 1, 6)]
n_need_fallback <- sum(is.na(cps$abstract))
cps <- merge(cps, task_broad, by = "soc_broad", all.x = TRUE)

n_filled <- sum(is.na(cps$abstract) & !is.na(cps$abstract_b))
print(sprintf("Task broad fallback: %d obs needed, %d filled", n_need_fallback, n_filled))

cps[is.na(abstract),          abstract := abstract_b]
cps[is.na(routine),           routine  := routine_b]
cps[is.na(manual),            manual   := manual_b]
cps[is.na(rti),               rti      := rti_b]
cps[is.na(abstract_raw),      abstract_raw := abstract_raw_b]
cps[is.na(routine_raw),       routine_raw  := routine_raw_b]
cps[is.na(manual_raw),        manual_raw   := manual_raw_b]
cps[is.na(telework_feasible), telework_feasible := tw_b]
cps[is.na(telework_binary),   telework_binary   := tw_bin_b]

cps[, c("soc_broad", "abstract_b", "routine_b", "manual_b", "rti_b",
        "abstract_raw_b", "routine_raw_b", "manual_raw_b",
        "tw_b", "tw_bin_b") := NULL]

attrition$after_task_merge <- sum(!is.na(cps$abstract))

# RPP merge
cps <- merge(cps, rpp, by.x = c("STATEFIP", "YEAR"),
             by.y = c("statefip", "year"), all.x = TRUE)
cps[!is.na(rpp_index), `:=`(
  real_wage_rpp = real_hourly_wage * 100 / rpp_index,
  log_wage_rpp  = log(real_hourly_wage * 100 / rpp_index)
)]


# Treatment/control classification

# Abstract-task tercile threshold
abstract_p67 <- quantile(cps$abstract, 2/3, na.rm = TRUE)
cps[, high_abstract := as.integer(abstract >= abstract_p67)]

# Within high-abstract: treatment = teleworkable, control = non-teleworkable
cps[, treated := as.integer(high_abstract == 1L & telework_binary == 1L)]
cps[, treated_x_post := treated * post]

# Four quadrants for descriptive analysis
cps[, quadrant := fcase(
  high_abstract == 1L & telework_binary == 1L, "HighAbstract_Teleworkable",
  high_abstract == 1L & telework_binary == 0L, "HighAbstract_NonTeleworkable",
  high_abstract == 0L & telework_binary == 1L, "LowAbstract_Teleworkable",
  high_abstract == 0L & telework_binary == 0L, "LowAbstract_NonTeleworkable",
  default = NA_character_
)]


# Final sample + save

n_before <- nrow(cps)
analytical <- cps[!is.na(log_wage) &
                    !is.na(abstract) &
                    !is.na(telework_feasible) &
                    !is.na(educ_cat) &
                    !is.na(race_eth) &
                    !is.na(exper) &
                    !is.na(industry)]
attrition$analytical <- nrow(analytical)

fwrite(analytical, file.path(DATA_CLEAN, "analytical_sample.csv"))
saveRDS(analytical, file.path(DATA_CLEAN, "analytical_sample.rds"))

# DID estimation sample (high-abstract occupations only)
did_sample <- analytical[high_abstract == 1L]
attrition$did_sample <- nrow(did_sample)
attrition$did_treatment <- sum(did_sample$treated == 1)
attrition$did_control   <- sum(did_sample$treated == 0)
attrition$n_occ_clusters   <- uniqueN(did_sample$OCC2010)
attrition$n_state_clusters <- uniqueN(did_sample$STATEFIP)

fwrite(did_sample, file.path(DATA_CLEAN, "did_sample.csv"))
saveRDS(did_sample, file.path(DATA_CLEAN, "did_sample.rds"))

# Save attrition table for paper_stats and replication
attrition_dt <- data.table(
  stage = c("Raw CPS extract",
            "ORG + employment filter",
            "Drop allocated wages",
            "Wage constructed (non-missing)",
            "Trim extreme wages ($3-$300)",
            "Task index matched (after fallback)",
            "Final analytical sample",
            "DID sample (high-abstract)",
            "  Treatment (teleworkable)",
            "  Control (non-teleworkable)"),
  N = c(attrition$raw_cps,
        attrition$org_filter,
        attrition$drop_allocated,
        attrition$wage_constructed,
        attrition$wage_trimmed,
        attrition$after_task_merge,
        attrition$analytical,
        attrition$did_sample,
        attrition$did_treatment,
        attrition$did_control)
)
saveRDS(attrition_dt, file.path(TAB_DIR, "sample_attrition.rds"))

print("--- Sample attrition ---")
print(attrition_dt)

print("--- Quadrant distribution ---")
print(analytical[, .(.N, mean_wage = weighted.mean(real_hourly_wage, EARNWT)),
                 by = quadrant][order(quadrant)])

print("--- DID sample balance ---")
print(did_sample[, .(
  N = .N,
  mean_log_wage = weighted.mean(log_wage, EARNWT),
  mean_abstract = mean(abstract),
  pct_female    = weighted.mean(female, EARNWT),
  pct_ba_plus   = weighted.mean(educ_cat == "4_bachelors_plus", EARNWT)
), by = .(treated, post)])
