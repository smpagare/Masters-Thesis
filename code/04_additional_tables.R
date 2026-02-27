# 04_additional_tables.R - Summary statistics and wage distribution tables

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
SOURCED_BY_CHILD <- TRUE
source(here::here("code", "00_master.R"))

library(kableExtra)

did <- readRDS(file.path(DATA_CLEAN, "did_sample.rds"))
analytical <- readRDS(file.path(DATA_CLEAN, "analytical_sample.rds"))
tab1_data <- readRDS(file.path(TAB_DIR, "tab1_balance.rds"))
tab2_data <- readRDS(file.path(TAB_DIR, "tab2_wages_by_quadrant.rds"))


# Table 1: Summary statistics by treatment/control and period

summary_stats <- did[, .(
  `Hourly Wage (\\$)`       = weighted.mean(real_hourly_wage, EARNWT),
  `Log Hourly Wage`         = weighted.mean(log_wage, EARNWT),
  `Age`                     = weighted.mean(AGE, EARNWT),
  `Female`                  = weighted.mean(female, EARNWT),
  `Married`                 = weighted.mean(married, EARNWT),
  `Has Children`            = weighted.mean(has_children, EARNWT),
  `Experience (years)`      = weighted.mean(exper, EARNWT),
  `Bachelor's Degree+`      = weighted.mean(educ_cat == "4_bachelors_plus", EARNWT),
  `Abstract Task Index`     = weighted.mean(abstract, EARNWT),
  `Telework Feasibility`    = weighted.mean(telework_feasible, EARNWT),
  `N`                       = .N
), by = .(treated, post)]

summary_stats[, group := fifelse(treated == 1,
  fifelse(post == 0, "Treat.\\ Pre", "Treat.\\ Post"),
  fifelse(post == 0, "Control Pre", "Control Post")
)]

stats_long <- melt(summary_stats,
  id.vars = c("treated", "post", "group"),
  variable.name = "Variable", value.name = "Value"
)
stats_wide <- dcast(stats_long, Variable ~ group, value.var = "Value")

col_order <- c("Variable", "Treat.\\ Pre", "Treat.\\ Post", "Control Pre", "Control Post")
stats_wide <- stats_wide[, ..col_order]

format_val <- function(x, var) {
  ifelse(var == "N", format(round(x), big.mark = ","),
    ifelse(var == "Hourly Wage (\\$)", sprintf("%.2f", x),
      ifelse(var %in% c("Female", "Married", "Has Children", "Bachelor's Degree+", "Telework Feasibility"),
        sprintf("%.3f", x),
        sprintf("%.2f", x)
      )
    )
  )
}

var_order <- c("Hourly Wage (\\$)", "Log Hourly Wage", "Age", "Experience (years)",
               "Female", "Married", "Has Children", "Bachelor's Degree+",
               "Abstract Task Index", "Telework Feasibility", "N")

stats_wide[, Variable := factor(Variable, levels = var_order)]
stats_wide <- stats_wide[order(Variable)]

for (col in col_order[-1]) {
  stats_wide[, (col) := sapply(seq_len(.N), function(i) {
    format_val(get(col)[i], as.character(Variable[i]))
  })]
}

tab1_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Summary Statistics: Treatment and Control Groups}",
  "\\label{tab:summary}",
  "\\small",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{\\textbf{Treatment (Teleworkable)}} & \\multicolumn{2}{c}{\\textbf{Control (Non-Teleworkable)}} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "\\textbf{Variable} & Pre-2020 & Post-2020 & Pre-2020 & Post-2020 \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(stats_wide))) {
  row_vals <- paste(
    stats_wide[i, Variable],
    stats_wide[i, `Treat.\\ Pre`],
    stats_wide[i, `Treat.\\ Post`],
    stats_wide[i, `Control Pre`],
    stats_wide[i, `Control Post`],
    sep = " & "
  )
  tab1_tex <- c(tab1_tex, paste0(row_vals, " \\\\"))
  if (as.character(stats_wide[i, Variable]) == "Telework Feasibility") {
    tab1_tex <- c(tab1_tex, "\\midrule")
  }
}

tab1_tex <- c(tab1_tex,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.92\\textwidth}",
  "\\vspace{0.3cm}",
  "\\small \\textit{Notes:} CPS-ORG, 2017Q1--2023Q1. Sample restricted to high-abstract occupations (top tercile of O*NET abstract-task index). Treatment: occupations classified as teleworkable by \\citet{dingel2020}. All statistics weighted by CPS earnings weight. Hourly wages in real 2019 dollars (CPI-U-RS adjusted).",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(tab1_tex, file.path(TAB_DIR, "tab1_summary_stats.tex"))


# Table 2: Wage distribution by task-telework quadrant

quad_stats <- analytical[, .(
  N    = .N,
  Mean = weighted.mean(real_hourly_wage, EARNWT),
  SD   = sqrt(weighted.mean((real_hourly_wage - weighted.mean(real_hourly_wage, EARNWT))^2, EARNWT)),
  P10  = quantile(real_hourly_wage, 0.10),
  P25  = quantile(real_hourly_wage, 0.25),
  P50  = quantile(real_hourly_wage, 0.50),
  P75  = quantile(real_hourly_wage, 0.75),
  P90  = quantile(real_hourly_wage, 0.90)
), by = .(quadrant)]

quad_labels_short <- c(
  "HighAbstract_Teleworkable"     = "High Abstract, Teleworkable",
  "HighAbstract_NonTeleworkable"  = "High Abstract, Non-Teleworkable",
  "LowAbstract_Teleworkable"     = "Low Abstract, Teleworkable",
  "LowAbstract_NonTeleworkable"  = "Low Abstract, Non-Teleworkable"
)
quad_stats[, Label := quad_labels_short[quadrant]]
quad_stats <- quad_stats[order(-Mean)]

tab2_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Wage Distribution by Task-Telework Quadrant}",
  "\\label{tab:wagequad}",
  "\\small",
  "\\begin{tabular}{lccccccc}",
  "\\toprule",
  "\\textbf{Quadrant} & \\textbf{N} & \\textbf{Mean} & \\textbf{SD} & \\textbf{P10} & \\textbf{P25} & \\textbf{P50} & \\textbf{P75} \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(quad_stats))) {
  row_vals <- paste(
    quad_stats[i, Label],
    format(quad_stats[i, N], big.mark = ","),
    sprintf("%.2f", quad_stats[i, Mean]),
    sprintf("%.2f", quad_stats[i, SD]),
    sprintf("%.2f", quad_stats[i, P10]),
    sprintf("%.2f", quad_stats[i, P25]),
    sprintf("%.2f", quad_stats[i, P50]),
    sprintf("%.2f", quad_stats[i, P75]),
    sep = " & "
  )
  tab2_tex <- c(tab2_tex, paste0(row_vals, " \\\\"))
}

tab2_tex <- c(tab2_tex,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.92\\textwidth}",
  "\\vspace{0.3cm}",
  "\\small \\textit{Notes:} CPS-ORG, 2017Q1--2023Q1. All wages in real 2019 dollars. Quadrant assignment based on O*NET abstract-task index (top tercile cutoff) and Dingel-Neiman telework feasibility classification.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(tab2_tex, file.path(TAB_DIR, "tab2_wage_quantiles.tex"))


# Table A1: Full occupation listing (Appendix)

dn <- fread(file.path(BASE_DIR, "data", "raw", "dingel_neiman",
                       "onet_teleworkable_blscodes.csv"))
setnames(dn, c("OCC_CODE", "OES_TITLE", "teleworkable"),
         c("soc_6digit", "occ_title", "dn_score"))

occ_table <- did[, .(
  N = .N,
  treated = first(treated),
  mean_abstract = round(mean(abstract), 2),
  mean_tw = round(mean(telework_feasible), 2)
), by = .(OCC2010, soc_6digit)]

occ_table <- merge(occ_table, dn[, .(soc_6digit, occ_title)],
                   by = "soc_6digit", all.x = TRUE)

# BLS SOC title lookup for codes not in Dingel-Neiman
soc_titles <- c(
  "25-3000" = "Other Teachers and Instructors",
  "25-2010" = "Preschool and Kindergarten Teachers",
  "25-2020" = "Elementary and Middle School Teachers",
  "43-9199" = "Other Office/Admin Support Workers",
  "27-1020" = "Designers",
  "13-1070" = "Human Resources Workers",
  "39-9030" = "Residential Advisors",
  "25-1000" = "Postsecondary Teachers",
  "11-2020" = "Marketing and Sales Managers",
  "13-2070" = "Tax Preparers/Financial Specialists",
  "53-1000" = "Supervisors, Transportation Workers",
  "13-1030" = "Claims Adjusters and Examiners",
  "13-1023" = "Purchasing Agents",
  "25-2030" = "Secondary School Teachers",
  "25-2050" = "Special Education Teachers",
  "29-9000" = "Other Healthcare Practitioners",
  "39-9099" = "Other Personal Care Workers",
  "17-2070" = "Electrical/Electronics Engineers",
  "27-4010" = "Broadcast/Sound Engineers",
  "27-3090" = "Other Media and Comm. Workers",
  "19-1020" = "Biological Scientists",
  "15-2090" = "Misc. Mathematical Scientists",
  "27-3020" = "News Analysts and Reporters",
  "27-1010" = "Artists and Related Workers",
  "17-1010" = "Architects",
  "19-2040" = "Environmental Scientists",
  "19-1040" = "Medical Scientists",
  "19-2030" = "Chemists and Materials Scientists",
  "17-1020" = "Surveyors and Cartographers",
  "19-3090" = "Misc. Social Scientists",
  "25-4010" = "Librarians",
  "27-4030" = "Misc. Media/Comm. Equipment Workers",
  "21-2099" = "Other Religious Workers",
  "19-3030" = "Psychologists",
  "19-1030" = "Conservation Scientists/Foresters",
  "27-3010" = "Announcers",
  "19-1010" = "Agricultural and Food Scientists",
  "13-1021" = "Buyers and Purchasing Agents",
  "19-2010" = "Astronomers and Physicists",
  "21-1020" = "Social Workers",
  "21-1010" = "Counselors",
  "33-3010" = "Correctional Officers and Jailers",
  "11-9030" = "Education Administrators",
  "31-2020" = "Physical Therapist Assistants",
  "29-1129" = "Misc. Physicians",
  "29-1060" = "Physicians and Surgeons",
  "17-2110" = "Industrial Engineers",
  "33-1099" = "First-Line Supvrs, Protective Svc",
  "31-2010" = "Occupational Therapy Assistants",
  "33-2020" = "Fire Inspectors and Prevention",
  "29-1020" = "Dentists"
)

# Fill missing titles from manual lookup, then remaining as SOC code
occ_table[is.na(occ_title) & soc_6digit %in% names(soc_titles),
          occ_title := soc_titles[soc_6digit]]
occ_table[is.na(occ_title), occ_title := paste0("SOC ", soc_6digit)]

# Truncate long titles
occ_table[, occ_title := substr(occ_title, 1, 40)]

# Sort: treatment first (descending N), then control (descending N)
occ_table <- occ_table[order(-treated, -N)]
occ_table[, group := fifelse(treated == 1, "Treatment", "Control")]

# Build LaTeX longtable
tabA1_tex <- c(
  "\\footnotesize",
  "\\begin{longtable}{p{5.5cm}lrllr}",
  "\\caption{Occupations in the Estimation Sample} \\label{tab:occlist} \\\\",
  "\\toprule",
  "Occupation & SOC & N & Group & Abstract & TW Score \\\\",
  "\\midrule",
  "\\endfirsthead",
  "\\multicolumn{6}{l}{\\textit{Table~\\ref{tab:occlist} continued}} \\\\",
  "\\toprule",
  "Occupation & SOC & N & Group & Abstract & TW Score \\\\",
  "\\midrule",
  "\\endhead",
  "\\midrule",
  "\\multicolumn{6}{r}{\\textit{Continued on next page}} \\\\",
  "\\endfoot",
  "\\bottomrule",
  "\\endlastfoot"
)

for (i in seq_len(nrow(occ_table))) {
  # Escape special LaTeX chars in title
  title_clean <- gsub("&", "\\\\&", occ_table[i, occ_title])
  row_str <- sprintf("%s & %s & %s & %s & %.2f & %.2f \\\\",
    title_clean,
    occ_table[i, soc_6digit],
    format(occ_table[i, N], big.mark = ","),
    occ_table[i, group],
    occ_table[i, mean_abstract],
    occ_table[i, mean_tw]
  )
  tabA1_tex <- c(tabA1_tex, row_str)
}

tabA1_tex <- c(tabA1_tex,
  "\\end{longtable}",
  "",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\small\\textit{Notes:} Occupations sorted by sample size within each group. Abstract is the standardized O*NET abstract-task index. TW Score is the Dingel-Neiman telework feasibility score. Entries labeled ``SOC XX-XXXX'' represent aggregated occupation categories in the IPUMS CPS coding that lack a direct match to detailed O*NET occupation titles.",
  "\\end{minipage}"
)

writeLines(tabA1_tex, file.path(TAB_DIR, "tab_A1_occupation_list.tex"))
