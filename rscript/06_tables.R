# Table S1 requires an authorized private dictionary at data_raw/questionnaire_dictionary.csv.
# This input is intentionally not distributed with the public code.
# 正文及补充Excel表：读取已完成评价和训练记录，不重训或重新抽样。 ----
# 运行设置集中于report_config.R；支持从项目根目录或其子目录运行。
project_dir <- normalizePath(getwd(), mustWork = TRUE)
while (!file.exists(file.path(project_dir, "rscript", "report_config.R"))) {
  parent_dir <- dirname(project_dir)
  if (identical(parent_dir, project_dir)) {
    stop("请在IDE中打开hemophilia项目根目录或其子目录后运行。")
  }
  project_dir <- parent_dir
}
renv::load(project_dir)
source(file.path(project_dir, "rscript", "report_config.R"))
report_settings <- report_config()
run_id <- report_settings$run_id
plot_format <- report_settings$plot_format
stopifnot(plot_format %in% c("png", "pdf", "tiff"))

library(openxlsx)
source(file.path(project_dir, "rscript", "tool_functions.R"))
run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%OS6")
table_dir <- file.path(project_dir, "output", "tables", "evaluation", run_id)
output_dir <- file.path(project_dir, "output", "tables", "main")
input_paths <- c(
  baseline = file.path(project_dir, "output/tables/eda/table1_selected.csv"),
  performance = file.path(table_dir, "performance_original_full_summary.csv"),
  calibration = file.path(table_dir, "calibration_table_original_full_summary.csv"),
  sampling = file.path(table_dir, "sampling_full_comparison.csv"),
  predictor = file.path(table_dir, "predictor_comparison.csv")
)
prediction_file <- file.path(project_dir, "output/tables/training", run_id, "outer_predictions.csv")
metadata_files <- file.path(table_dir, c(
  "performance_metadata.rds", "calibration_table_metadata.rds",
  "sampling_full_comparison_metadata.rds"
))
for (file in metadata_files) {
  metadata <- readRDS(file)
  stopifnot(identical(
    unname(metadata$input_md5), unname(tools::md5sum(prediction_file))
  ))
}
restricted_prediction_file <- file.path(
  project_dir, "output/tables/training", report_settings$restricted_run_id,
  "outer_predictions.csv"
)
predictor_metadata_file <- file.path(table_dir, "predictor_comparison_metadata.rds")
predictor_metadata <- readRDS(predictor_metadata_file)
stopifnot(identical(
  unname(predictor_metadata$input_md5),
  unname(tools::md5sum(c(prediction_file, restricted_prediction_file)))
))

supplement_inputs <- c(
  file.path(project_dir, "data_raw/questionnaire_dictionary.csv"),
  file.path(project_dir, "data_raw/hemophilia_data.xlsx"),
  file.path(project_dir, "data_interim/recode_data.RData"),
  file.path(project_dir, "output/tables/training", run_id, "variable_registry.csv"),
  file.path(project_dir, "output/logs/training", run_id, "config.rds"),
  file.path(project_dir, "output/lrns", run_id,
    paste0("original_full_", c(paste0("repeat1_fold", 1:5), "final"), ".rds"))
)
report <- start_report(
  project_dir, run_id, "tables",
  c(input_paths, prediction_file, restricted_prediction_file,
    metadata_files, predictor_metadata_file, supplement_inputs), run_stamp
)
tables <- lapply(input_paths, readr::read_csv, show_col_types = FALSE)
labels <- c(
  logistic = "LR", logistic_spline = "LR with splines", tree = "DT",
  rf = "RF", svm = "SVM", xgboost = "XGBoost", knn = "KNN", naive_bayes = "NB"
)
interval_text <- function(estimate, lower, upper) {
  stopifnot(all(is.finite(c(estimate, lower, upper))), lower <= upper)
  values <- c(estimate, lower, upper)
  values[abs(values) < 0.0005] <- 0
  sprintf("%.3f (%.3f, %.3f)", values[1], values[2], values[3])
}
lookup_metric <- function(data, algorithm, metric) {
  row <- data[data$algorithm == algorithm & data$metric == metric, ]
  stopifnot(nrow(row) == 1L)
  row
}
export_table <- function(number, title, header, rows, notes, filename,
                         sections = integer(), numeric_source = NULL) {
  stopifnot(ncol(rows) == length(header), nrow(rows) > 0)
  wb <- createWorkbook()
  sheet <- paste("Table", number)
  addWorksheet(wb, sheet, gridLines = FALSE)
  modifyBaseFont(wb, fontName = "Arial", fontSize = 11)
  nc <- length(header)
  writeData(wb, sheet, title, startRow = 2, colNames = FALSE)
  mergeCells(wb, sheet, cols = seq_len(nc), rows = 2)
  addStyle(wb, sheet, createStyle(fontSize = 14, textDecoration = "bold"), 2, 1)
  writeData(wb, sheet, t(header), startRow = 4, colNames = FALSE)
  addStyle(wb, sheet, createStyle(
    textDecoration = "bold", halign = "center",
    valign = "center", wrapText = TRUE, border = c("top", "bottom")
  ),
  rows = 4, cols = seq_len(nc), gridExpand = TRUE
  )
  writeData(wb, sheet, rows, startRow = 5, colNames = FALSE, keepNA = FALSE)
  last <- 4 + nrow(rows)
  addStyle(wb, sheet, createStyle(wrapText = TRUE, valign = "center"),
    rows = 5:last, cols = seq_len(nc), gridExpand = TRUE
  )
  addStyle(wb, sheet, createStyle(halign = "center"),
    rows = 5:last,
    cols = 2:nc, gridExpand = TRUE, stack = TRUE
  )
  setColWidths(wb, sheet, cols = 1, widths = if (number == 1) 57 else 36)
  setColWidths(wb, sheet, cols = 2:nc, widths = if (number == 2) 24 else 30)
  setRowHeights(wb, sheet, rows = 4:last, heights = 34)
  for (r in sections + 4L) {
    addStyle(wb, sheet, createStyle(textDecoration = "bold", fgFill = "#EEF2F5"),
      rows = r, cols = seq_len(nc), gridExpand = TRUE, stack = TRUE
    )
  }
  addStyle(wb, sheet, createStyle(border = "bottom"),
    rows = last,
    cols = seq_len(nc), gridExpand = TRUE, stack = TRUE
  )
  for (i in seq_along(notes)) {
    r <- last + 1L + i
    writeData(wb, sheet, notes[i], startRow = r, colNames = FALSE)
    mergeCells(wb, sheet, cols = seq_len(nc), rows = r)
    addStyle(wb, sheet, createStyle(fontSize = 10, wrapText = TRUE, valign = "center"), r, 1)
    setRowHeights(wb, sheet, rows = r, heights = 36)
  }
  pageSetup(wb, sheet, orientation = "landscape", fitToWidth = 1, fitToHeight = 0)
  if (!is.null(numeric_source)) {
    addWorksheet(wb, "Estimates", gridLines = FALSE)
    writeData(wb, "Estimates", numeric_source)
    setColWidths(wb, "Estimates", cols = seq_len(ncol(numeric_source)), widths = 20)
  }
  file <- file.path(output_dir, filename)
  ensure_parent_dir(file)
  snapshot <- backup_path(file, run_stamp)
  saveWorkbook(wb, file, overwrite = TRUE)
  stopifnot(file.copy(file, snapshot, overwrite = FALSE))
  saved <- read.xlsx(file,
    sheet = sheet, rows = 5:last, colNames = FALSE,
    skipEmptyRows = FALSE, skipEmptyCols = FALSE
  )
  stopifnot(identical(dim(saved), dim(rows)))
  expected <- as.matrix(rows)
  actual <- as.matrix(saved)
  populated <- !is.na(expected) & nzchar(expected)
  stopifnot(identical(unname(actual[populated]), unname(expected[populated])))
  invisible(file)
}

# Table 1：英文标签按源CSV变量及类别顺序映射，并验证每组行数。
baseline_labels <- list(
  v7 = c("Marital status", "Never married", "Married", "Divorced", "Widowed"),
  v8 = c(
    "Educational attainment", "No formal schooling", "Primary school",
    "Junior secondary school", "Senior secondary / vocational school",
    "Junior college", "Bachelor's degree", "Master's degree or higher"
  ),
  v9 = c("Household registration", "Rural", "Urban"),
  v17 = c(
    "Basic medical insurance", "Employee basic medical insurance",
    "Urban-rural resident basic medical insurance", "Government-funded medical care", "None"
  ),
  v32 = c("Hemophilia type", "A", "B"),
  v33 = c("Hemophilia severity", "Mild", "Moderate", "Severe"),
  v34 = "Age at diagnosis, years",
  v37 = c("History of joint surgery", "No", "Yes"),
  v38 = c("Hemophilia-related disability", "None", "Physical disability", "Intellectual or mental disability"),
  v39 = c("Family history of hemophilia", "No", "Yes"),
  v43 = c("Treatment regimen", "Regular prophylaxis", "On-demand treatment"),
  v47 = c("History of factor inhibitors (current or previous)", "No", "Yes", "Uncertain")
)
baseline <- tables$baseline
stopifnot(identical(unique(baseline$variable), names(baseline_labels)))
baseline_rows <- list()
sections <- integer()
for (v in names(baseline_labels)) {
  part <- baseline[baseline$variable == v, ]
  text <- baseline_labels[[v]]
  stopifnot(nrow(part) == if (v == "v34") 1L else length(text) - 1L)
  if (v != "v34") {
    sections <- c(sections, length(baseline_rows) + 1L)
    baseline_rows[[length(baseline_rows) + 1L]] <- c(text[1], "", "", "")
  }
  for (i in seq_len(nrow(part))) {
    baseline_rows[[length(baseline_rows) + 1L]] <- c(
      if (v == "v34") text[1] else paste0("    ", text[i + 1L]),
      part$Overall[i], part$High[i], part$Low[i]
    )
  }
}
# 组别人数从同批冻结外层预测提取，避免表头沿用旧样本量。
prediction_file <- file.path(project_dir, "output/tables/training", run_id, "outer_predictions.csv")
predictions <- readr::read_csv(prediction_file, show_col_types = FALSE)
subjects <- predictions[predictions$scenario == "original_full" & predictions$algorithm == "xgboost", ]
stopifnot(!anyDuplicated(subjects$patient_id), nrow(subjects) > 0)
export_table(
  1, "Table 1. Characteristics of the study participants",
  c(
    "Characteristic", sprintf("Overall (N = %s)", format(nrow(subjects), big.mark = ",")),
    sprintf("High ABR (n = %s)", format(sum(subjects$y_high == 1), big.mark = ",")),
    sprintf("Low ABR (n = %s)", format(sum(subjects$y_high == 0), big.mark = ","))
  ),
  do.call(rbind, baseline_rows),
  c(
    "Values are n (%) or median [25th, 75th percentiles]. Percentages use the total in each column.",
    "High ABR: >1; Low ABR: <=1. All participants were adult men. Uncertain inhibitor history is reported separately."
  ),
  "table1_participant_characteristics.xlsx", sections
)

common_notes <- c(
  "Original distribution, full candidate predictor set; High ABR (>1) is the positive class. Values are equal-weight means of five outer test folds.",
  "95% intervals are conditional on fixed predictions, using 2,000 patient bootstrap samples within folds. They exclude retraining variability and overlapping training-set dependence; they are not full-procedure nested-CV intervals."
)
performance_metrics_labels <- c(
  roc_auc = "ROC-AUC", pr_auc = "PR-AUC",
  sensitivity = "Sensitivity", specificity = "Specificity", ppv = "PPV", npv = "NPV",
  balanced_accuracy = "Balanced accuracy", mcc = "MCC"
)
performance_rows <- lapply(names(labels), function(a) {
  c(labels[a], vapply(names(performance_metrics_labels), function(m) {
    r <- lookup_metric(tables$performance, a, m)
    interval_text(r$estimate, r$lower, r$upper)
  }, character(1)))
})
export_table(2, "Table 2. Model performance under nested cross-validation",
  c("Model", performance_metrics_labels), do.call(rbind, performance_rows),
  c(
    common_notes, "Classification thresholds were selected within outer training data. PR-AUC uses trapezoidal integration, not average precision.",
    "PPV, positive predictive value; NPV, negative predictive value; MCC, Matthews correlation coefficient."
  ),
  "table2_model_performance.xlsx",
  numeric_source = tables$performance
)

calibration_labels <- c(
  brier = "Brier score", citl = "Calibration intercept (CITL)",
  slope = "Calibration slope", eavg = "Eavg", emax = "Emax"
)
calibration_models <- c(labels, prevalence_only = "Prevalence-only reference")
calibration_rows <- lapply(names(calibration_models), function(a) {
  c(calibration_models[a], vapply(names(calibration_labels), function(m) {
    if (a == "prevalence_only" && m != "brier") {
      return("Not reported")
    }
    r <- lookup_metric(tables$calibration, a, m)
    interval_text(r$estimate, r$lower, r$upper)
  }, character(1)))
})
export_table(3, "Table 3. Probabilistic accuracy and calibration",
  c("Model", calibration_labels), do.call(rbind, calibration_rows),
  c(
    common_notes, "CITL fixes slope at 1; calibration slope has a free intercept. Ideal CITL = 0 and slope = 1. Logit diagnostics bound probabilities to [0.000001, 0.999999].",
    "Eavg/Emax use LOWESS (span 2/3, no robust iterations), refitted in every resample and evaluated at its observed probabilities, following rms::val.prob. No resamples were discarded. Figure curves use a separate spline smoother.",
    "Percentile intervals for absolute errors may be shifted above the point estimate. Source: https://github.com/harrelfe/rms/blob/master/R/val.prob.s. These are approximate conditional intervals.",
    "The reference uses original training-fold prevalence. Lower Brier, Eavg and Emax values are better. No predictions were recalibrated."
  ),
  "table3_model_calibration.xlsx",
  numeric_source = tables$calibration
)

# Table 4: both sensitivity analyses share the same main-analysis reference.
sensitivity_rows <- list()
sensitivity_labels <- c(
  logistic = "LR", logistic_spline = "LR with splines", tree = "DT",
  rf = "RF", svm = "SVM", xgboost = "XGBoost", knn = "KNN",
  naive_bayes = "NB"
)
sections <- integer()
for (m in c("roc_auc", "pr_auc", "brier")) {
  sections <- c(sections, length(sensitivity_rows) + 1L)
  sensitivity_rows[[length(sensitivity_rows) + 1L]] <- c(
    c(roc_auc = "ROC-AUC", pr_auc = "PR-AUC", brier = "Brier score")[m],
    rep("", 5)
  )
  for (a in names(labels)) {
    r <- lookup_metric(tables$sampling, a, m)
    p <- lookup_metric(tables$predictor, a, m)
    stopifnot(identical(
      unlist(r[c("original", "original_lower", "original_upper")]),
      unlist(p[c("original", "original_lower", "original_upper")])
    ))
    sensitivity_rows[[length(sensitivity_rows) + 1L]] <- c(
      sensitivity_labels[a], interval_text(r$original, r$original_lower, r$original_upper),
      interval_text(r$smotenc, r$smotenc_lower, r$smotenc_upper),
      interval_text(r$delta, r$delta_lower, r$delta_upper),
      interval_text(p$restricted, p$restricted_lower, p$restricted_upper),
      interval_text(p$delta, p$delta_lower, p$delta_upper)
    )
  }
}
sensitivity_numeric <- rbind(
  data.frame(comparison = "SMOTENC versus main", tables$sampling),
  data.frame(comparison = "Restricted versus main",
    setNames(tables$predictor, names(tables$sampling)))
)
names(sensitivity_numeric)[names(sensitivity_numeric) == "smotenc"] <- "sensitivity"
names(sensitivity_numeric) <- sub("^smotenc_", "sensitivity_", names(sensitivity_numeric))
export_table(
  4, "Table 4. Sensitivity analyses",
  c("Model", "Main analysis (95% CI)", "SMOTENC (95% CI)",
    "SMOTENC - main (95% CI)", "Restricted predictors (95% CI)",
    "Restricted - main (95% CI)"),
  do.call(rbind, sensitivity_rows),
  c(
    "Main: no oversampling, 89 candidate predictors. SMOTENC: oversampling, 89 candidates. Restricted: no oversampling, 71 candidates. High ABR (>1) is the positive class.",
    "Values are equal-weight means across five outer test folds. Positive AUC differences and negative Brier differences favor the sensitivity scenario.",
    "95% CIs use 2,000 paired patient bootstrap samples within folds. Fixed predictions; intervals exclude retraining variability and overlapping training-set dependence. No multiplicity adjustment.",
    "Excluded: osteoarthritis, arthritis, any comorbidity/complication, joint surgery, hemophilia-related disability, symptom change as a reason for injection frequency, home rehabilitation/physical therapy, and 11 target-joint indicators.",
    "Feature selection and tuning are performed separately in each scenario. PR-AUC uses trapezoidal integration, not average precision. No recalibration. Differences use unrounded estimates and paired bootstrap replicates."
  ),
  "table4_sensitivity_analyses.xlsx", sections, sensitivity_numeric
)
# Supplementary tables: source definitions, fitted configurations and counts. ----
registry_file <- file.path(project_dir, "output/tables/training", run_id, "variable_registry.csv")
raw_file <- file.path(project_dir, "data_raw/hemophilia_data.xlsx")
encoded_file <- file.path(project_dir, "data_interim/recode_data.RData")
training_manifest <- readRDS(file.path(
  project_dir, "output/logs/training", run_id, "manifest_finished.rds"
))
stopifnot(identical(
  unname(tools::md5sum(encoded_file)),
  unname(training_manifest$files["data_interim/recode_data.RData"])
))
registry <- readr::read_csv(registry_file, show_col_types = FALSE)
raw <- readxl::read_excel(raw_file, sheet = "成人")
encoded_env <- new.env()
load(encoded_file, envir = encoded_env)
encoded <- encoded_env$recode_data
stopifnot(nrow(raw) == nrow(encoded), identical(as.character(raw[[1]]), as.character(encoded$id)))
stopifnot(setequal(as.character(encoded$id), as.character(subjects$patient_id)))
questionnaire_dictionary <- readr::read_csv(
  file.path(project_dir, "data_raw/questionnaire_dictionary.csv"),
  show_col_types = FALSE
)
stopifnot(
  nrow(questionnaire_dictionary) == 54L,
  identical(questionnaire_dictionary$source_field, paste0("v", 1:54)),
  !anyNA(questionnaire_dictionary)
)
source_labels <- setNames(
  questionnaire_dictionary$variable,
  questionnaire_dictionary$source_field
)
stopifnot(
  all((encoded$v36 == "yes") ==
    (rowSums(encoded[grepl("^v35_", names(encoded))] == "yes") > 0L)),
  all((encoded$v46 == "yes") ==
    (rowSums(encoded[c("v45_self", "v45_family", "v45_hired_caregiver")] ==
      "yes") > 0L))
)
suffix_labels <- c(
  code_rare_disease_center = "CORD rare disease center", pain_challenge_foundation = "Pain Challenge Foundation",
  rare_disease_alliance = "Rare disease alliance", patient_organization = "Local hemophilia patient organization",
  no_contact = "No organization contacted", no_option = "No support option selected (including skipped)",
  not_sure = "Treatment product unknown", pcc = "Prothrombin complex concentrate",
  hiv = "HIV", plasma_factor = "Plasma-derived factor", recombinant_factor = "Recombinant factor",
  emicizumab = "Emicizumab", mental = "Intellectual or mental disability"
)
variable_label <- function(v) {
  if (v == "id") return("Participant identifier")
  parent <- sub("_.*$", "", v)
  stopifnot(parent %in% names(source_labels))
  if (v == parent) return(unname(source_labels[parent]))
  suffix <- sub("^[^_]+_", "", v)
  term <- if (suffix %in% names(suffix_labels)) suffix_labels[[suffix]] else gsub("_", " ", suffix)
  paste0(source_labels[parent], ": ", term)
}
# Present each source field once; multi-response indicators share one row.
definition_rows <- list()
for (i in seq_len(nrow(questionnaire_dictionary))) {
  q <- questionnaire_dictionary[i, ]
  parent <- q$source_field
  r <- registry[registry$source_variable == parent, ]
  stopifnot(nrow(r) > 0L)
  source_index <- as.integer(sub("v", "", parent)) + 1L
  stopifnot(all(names(raw)[source_index] == r$source_question))
  values <- trimws(as.character(raw[[source_index]]))
  blank <- is.na(values) | values == ""
  skipped <- !blank & values == "(跳过)"
  uncertain <- !blank & grepl("不记得|不清楚|不确定", values)
  included <- r[r$include_full, ]
  excluded_restricted <- included$variable[!included$include_restricted]
  role <- if (parent == "v50") {
    "Outcome source; excluded from predictors"
  } else if (nrow(included) == 0L) {
    switch(parent,
      v1 = "Excluded: fixed population stratum",
      v2 = "Excluded: fixed respondent identity",
      v3 = "Excluded: fixed sex in this dataset",
      v15 = "Excluded: duplicate of v9",
      v21 = "Excluded: bleeding-related healthcare utilization",
      v22 = "Excluded: bleeding-related healthcare utilization",
      v23 = "Excluded: bleeding-related healthcare utilization",
      v24 = "Excluded: bleeding-related healthcare utilization",
      v41 = "Excluded: incompatible dose units",
      v48 = "Excluded: bleeding outcome-proximal item",
      v49 = "Excluded: bleeding outcome-proximal item",
      v51 = "Excluded: joint bleeding outcome",
      stop("Unmapped source-field exclusion: ", parent)
    )
  } else {
    base <- sprintf("Full: %d; restricted: %d predictor(s).",
      nrow(included), sum(included$include_restricted))
    if (length(excluded_restricted) == nrow(included)) {
      paste0(base, " Excluded from the restricted set.")
    } else if (length(excluded_restricted) > 0L) {
      paste0(base, " Restricted excludes: ",
        paste(excluded_restricted, collapse = ", "), ".")
    } else base
  }
  references <- unique(r$reference_level[
    !is.na(r$reference_level) & nzchar(r$reference_level)
  ])
  reference <- if (length(references) == 0L) {
    "Not applicable"
  } else {
    paste(references, collapse = "; ")
  }
  if (length(references) == 1L && references == "no" && nrow(r) > 1L) {
    reference <- "no (0), for each binary indicator"
  }
  source_counts <- sprintf(
    "Blank: %d (%.1f%%); skipped: %d (%.1f%%); uncertain: %d (%.1f%%)",
    sum(blank), 100 * mean(blank), sum(skipped), 100 * mean(skipped),
    sum(uncertain), 100 * mean(uncertain)
  )
  definition_rows[[i]] <- data.frame(
    Variable = q$variable,
    Questionnaire_item = paste0(q$questionnaire_item, " [", parent, "]"),
    Definition = q$definition,
    Coding_and_reference = paste0(q$coding, " Reference: ", reference, "."),
    Model_inclusion = role,
    Source_response_status = source_counts,
    check.names = FALSE
  )
}
definitions <- do.call(rbind, definition_rows)
stopifnot(
  nrow(definitions) == 54L,
  nrow(registry) == ncol(encoded),
  sum(registry$include_full) == 89L,
  sum(registry$include_restricted) == 71L,
  !anyNA(encoded)
)

sheet_spec <- function(title, data, notes, widths) {
  stopifnot(length(widths) == ncol(data), !anyNA(data))
  list(title = title, data = data, notes = notes, widths = widths)
}
supplement_specs <- list()
supplement_specs$table_s1_variables_missingness <- list(
  "Table S1" = sheet_spec(
    "Table S1. Definitions and coding information for candidate predictors",
    definitions[c("Variable", "Definition", "Coding_and_reference", "Model_inclusion")],
    c(
      "Notes: Each of the 54 supplied source fields is listed once. Multiple-response items generate separate binary predictors. The encoded dataset contains 113 fields, including the record identifier; 89 predictors are eligible in the full set and 71 in the restricted set. The identifier is excluded from all models.",
      "Coding describes the dataset actually analyzed; differences between the questionnaire and supplied categories are stated explicitly. All measures are respondent-reported, including reports of diagnoses.",
      "References describe treatment coding for factor-based contrasts; binary no/yes indicators use 0/1 before model-specific preprocessing. Naive Bayes retains factors. Continuous variables have no categorical reference.",
      "No encoded fields contain NA in the 1,765 supplied adult records. Questionnaire skips and explicit uncertain responses are handled as described for the relevant variables. This does not establish absence of missingness before data delivery.",
      "ABR and AJBR here are the reported 12-month spontaneous bleeding counts (all sites and joint sites, respectively). ABR >1 defines the positive outcome. No annualization multiplier was applied.",
      "Source: Patient-version Chinese Hemophilia Health Literacy Survey, questions 4-50, PDF pp. 2-7; supplied adult dataset; 01_recode.R; frozen training variable registry."
    ),
    c(32, 58, 70, 34)
  )
)

config_file <- file.path(project_dir, "output/logs/training", run_id, "config.rds")
frozen_config <- readRDS(config_file)
stopifnot(frozen_config$schema_version == 4L, frozen_config$outer_folds == 5L)
search_rows <- list()
for (a in names(labels)) {
  space <- frozen_config$search_spaces[[a]]
  if (length(space) == 0L) {
    search_rows[[length(search_rows) + 1L]] <- data.frame(Model = labels[[a]], Parameter = "None tuned",
      Search = "Fixed configuration; feature selector still compared", Values = "Not applicable")
  }
  for (parameter in names(space)) {
    s <- space[[parameter]]
    values <- if (a == "rf" && parameter == "num.trees") {
      stopifnot(identical(s$values, seq(50L, 2000L, by = 10L)))
      "50, 60, ..., 1990 (increments of 10)"
    } else if (s$type == "choice") {
      paste(s$values, collapse = ", ")
    } else {
      paste(s$lower, s$upper, sep = " to ")
    }
    search_rows[[length(search_rows) + 1L]] <- data.frame(Model = labels[[a]], Parameter = parameter,
      Search = paste(frozen_config$search_method[[a]], s$type, sep = ": "), Values = values)
  }
}
search_table <- do.call(rbind, search_rows)
model_files <- file.path(project_dir, "output/lrns", run_id,
  paste0("original_full_", c(paste0("repeat1_fold", 1:5), "final"), ".rds"))
stopifnot(all(file.exists(model_files)))
selected_rows <- list()
final_features <- data.frame(Field = registry$variable[registry$include_full])
final_features$Variable <- vapply(final_features$Field, variable_label, character(1))
for (j in seq_along(model_files)) {
  bundle <- readRDS(model_files[j])
  for (a in names(labels)) {
    m <- bundle$models[[a]]
    parameters <- if (length(m$params) == 0L) "No tuned learner parameters" else paste(
      vapply(names(m$params), function(k) paste0(k, "=", paste(format(m$params[[k]], digits = 9, trim = TRUE), collapse = ",")), character(1)),
      collapse = "; "
    )
    selected_rows[[length(selected_rows) + 1L]] <- data.frame(
      Model = labels[[a]], Fit = if (j <= 5L) paste("Outer training fold", j) else "Full-data final fit",
      Selector = m$selection$selector, Selected_variables = length(m$selection$selected),
      Parameters = parameters,
      Threshold = bundle$thresholds$threshold[match(a, bundle$thresholds$algorithm)]
    )
    if (j == 6L) final_features[[labels[[a]]]] <- as.integer(final_features$Field %in% m$selection$selected)
  }
}
selected_parameters <- do.call(rbind, selected_rows)
methods <- data.frame(
  Component = c("Analysis scope", "Resampling", "Feature selectors", "LASSO", "Boruta", "Encoding and scaling",
    "Spline logistic comparator", "Naive Bayes", "SMOTENC scenarios", "Tuning", "Selection rule", "Threshold", "Randomness and environment"),
  Specification = c(
    "Original distribution, full candidate set. Selected configurations in Table S4 and final features in Table S5 describe this main analysis only.",
    "Stratified outer 5-fold CV; stratified inner 5-fold CV. Outer test patients are excluded from all training, selection, tuning and threshold steps.",
    "LASSO and Boruta are compared inside the inner loop; each is fitted on real training patients before oversampling. Selected features are refitted on the full outer training set.",
    "Binomial cv.glmnet, alpha=1, standardize=TRUE, internal 5-fold CV, lambda.1se, maxit=1,000,000. Any selected contrast retains its entire raw variable.",
    "Ranger scaled permutation importance, 500 trees, maxRuns=100, pValue=0.01, multiplicity adjustment enabled. Tentative variables are excluded; non-finite importance stops the run.",
    "Treatment contrasts with prespecified factor levels. For all learners except Naive Bayes, encoded nonconstant columns are centered and scaled using real training data; these values are frozen for validation. Logistic fits also remove redundant columns using training QR rank.",
    "Natural splines for numeric predictors, maximum df=3, with training-derived knots/boundaries and reduced basis when necessary. Logistic maxit=100.",
    "Factors remain categorical; numeric predictors remain numeric. e1071 mixed-input Naive Bayes, categorical Laplace smoothing fixed at 1; invalid within-class numeric SD causes an explicit error.",
    "Not used in the main analysis. Sensitivity scenarios use k=5 and minority/majority ratio=1 within training folds only, in the eligible mixed-type candidate space before final learner encoding.",
    "Tree and KNN use full grids; RF uses 100, SVM 150 and XGBoost 150 random parameter configurations, shared by both selectors. There are 1,318 selector-learner candidates per search, evaluated on five inner folds.",
    "Highest mean inner ROC-AUC; ties favor fewer mean selected features, then fixed candidate order. Each algorithm has its own winner. All algorithm results are retained.",
    "Youden index from inner out-of-fold probabilities of the winning configuration. Ties use the larger threshold. High if p >= threshold; outer-test outcomes never choose the threshold.",
    "Master seed 123 with deterministic context-specific seeds. Package and R versions are recorded in renv.lock and the frozen run manifest. No additional probability recalibration."
  )
)
methods <- rbind(methods, data.frame(
  Component = paste("Learner:", unname(labels)),
  Specification = c(
    "mlr3learners::classif.log_reg, binomial logistic regression; treatment coding, training centering/scaling, QR rank selection; maxit=100.",
    "mlr3learners::classif.log_reg with training-derived natural-spline numeric terms (up to df=3); treatment coding, centering/scaling and QR rank selection; maxit=100.",
    "mlr3learners::classif.rpart; treatment coding and training centering/scaling; internal rpart xval=0 because tuning uses the common inner folds.",
    "mlr3learners::classif.ranger; treatment coding and training centering/scaling; num.threads=1, context-specific seed. Tuned parameters are in the search and selected-configuration sheets.",
    "mlr3learners::classif.svm (e1071/LIBSVM); C-classification, radial kernel, scale=FALSE because scaling is fitted upstream; native probability estimation within each fitted training model.",
    "mlr3learners::classif.xgboost; treatment coding and training centering/scaling; binary probability learner, nthread=1, context-specific seed, verbosity=0.",
    "mlr3learners::classif.kknn; treatment coding and training centering/scaling; scale=FALSE because scaling is fitted upstream.",
    "mlr3learners::classif.naive_bayes (e1071); retains categorical factors and unscaled numeric variables; Laplace smoothing=1. Numeric zero-SD checks are mandatory."
  )
))
supplement_specs$table_s2_model_development <- list(
  "Table S2" = sheet_spec("Table S2. Preprocessing and model development", methods,
    "Source: frozen training configuration, model bundles and implementation, schema 4.", c(32, 130))
)
supplement_specs$table_s3_hyperparameter_search <- list(
  "Table S3" = sheet_spec("Table S3. Hyperparameter search spaces and tuning strategies", search_table,
    c("Continuous uniform/log-uniform ranges are sampled, not discretized into a small preset grid. Integer ranges include both endpoints.",
      "RF num.trees spans 50 to 2,000 in increments of 10, ending at 1,990 as generated by seq(50,2000,by=10). RF mtry is derived from the encoded feature count and mtry.ratio."), c(38, 28, 38, 95))
)
supplement_specs$table_s4_selected_configurations <- list(
  "Table S4" = sheet_spec("Table S4. Selected configurations in the main analysis", selected_parameters,
    c("Five outer-training fits and one full-data final fit per algorithm. These are fitted configurations, not separate test samples.",
      "Thresholds use training-internal predictions; the final-fit threshold is not used to recompute outer-test performance. Fixed implementation parameters appear in Table S2; displayed numeric parameters are rounded to nine significant digits."), c(38, 28, 16, 20, 100, 20))
)
supplement_specs$table_s5_final_features <- list(
  "Table S5" = sheet_spec("Table S5. Features retained in full-data final models", final_features,
    "1 = retained raw variable; 0 = not retained. Final-fit selections are descriptive; outer folds repeat selection independently. Field definitions are in Table S1.",
    c(20, 48, rep(22, length(labels))))
)

count_rows <- list()
pooled_rows <- list()
for (a in c(names(labels), "all_high")) {
  x <- if (a == "all_high") subjects else predictions[predictions$scenario == "original_full" & predictions$algorithm == a, ]
  stopifnot(nrow(x) == nrow(subjects), !anyDuplicated(x$patient_id))
  classified <- if (a == "all_high") rep(1L, nrow(x)) else as.integer(x$p_high_raw >= x$threshold_raw)
  if (a != "all_high") stopifnot(identical(classified, as.integer(x$class_raw)))
  model <- if (a == "all_high") "Classify everyone as High" else labels[[a]]
  for (f in c(0L, 1:5)) {
    keep <- if (f == 0L) rep(TRUE, nrow(x)) else x$outer_fold == f
    y <- x$y_high[keep]; cl <- classified[keep]
    tp <- sum(y == 1 & cl == 1); fp <- sum(y == 0 & cl == 1)
    tn <- sum(y == 0 & cl == 0); fn <- sum(y == 1 & cl == 0)
    stopifnot(tp + fp + tn + fn == sum(keep))
    row <- data.frame(Model = model, Fold = if (f == 0L) "Pooled outer tests" else as.character(f),
      Threshold = if (a == "all_high") "Not applicable" else if (f == 0L) "Fold-specific" else {
        t <- unique(x$threshold_raw[keep]); stopifnot(length(t) == 1L); format(t, digits = 9)
      }, N = length(y), TP = tp, FP = fp, TN = tn, FN = fn,
      Accuracy = (tp + tn) / length(y), F1 = 2 * tp / (2 * tp + fp + fn))
    if (f == 0L) pooled_rows[[length(pooled_rows) + 1L]] <- row else count_rows[[length(count_rows) + 1L]] <- row
  }
}
pooled_counts <- do.call(rbind, pooled_rows)
fold_counts <- do.call(rbind, count_rows)
fold_counts$Fold <- as.integer(fold_counts$Fold)
for (a in unique(pooled_counts$Model)) {
  stopifnot(identical(as.numeric(pooled_counts[pooled_counts$Model == a, c("TP", "FP", "TN", "FN")]),
    as.numeric(colSums(fold_counts[fold_counts$Model == a, c("TP", "FP", "TN", "FN")]))))
}
s3_counts <- do.call(rbind, lapply(unique(pooled_counts$Model), function(a) {
  rbind(
    fold_counts[fold_counts$Model == a, ],
    pooled_counts[pooled_counts$Model == a, ]
  )
}))
stopifnot(nrow(s3_counts) == 54L)
s3_counts$Fold[s3_counts$Fold == "Pooled outer tests"] <- "Pooled"
supplement_specs$table_s6_thresholds_confusion <- list(
  "Table S6" = sheet_spec(
    "Table S6. Fold-specific thresholds and confusion matrices",
    s3_counts,
    c("Notes: High ABR (>1) is positive. Thresholds maximize Youden's J using inner cross-validation predictions from the corresponding outer training set. High is assigned when p >= threshold.",
      "Pooled rows sum the five outer-test confusion matrices, retaining each fold's threshold. Pooled accuracy and F1 are calculated from these counts, not averaged across folds as in Table 2. The all-High rule is a reference strategy.",
      "Abbreviations: ABR, annualized bleeding rate; LR, logistic regression; DT, decision tree; RF, random forest; SVM, support vector machine; XGBoost, extreme gradient boosting; KNN, k-nearest neighbors; NB, naive Bayes; TP, true positives; FP, false positives; TN, true negatives; FN, false negatives."),
    c(30, 12, 24, rep(10, 5), 13, 13))
)

# Keep data and formatting specifications together for reproducible R exports.
supplement_dir <- file.path(project_dir, "output/tables/supplementary")
for (name in names(supplement_specs)) {
  specs <- supplement_specs[[name]]
  wb <- createWorkbook()
  modifyBaseFont(wb, fontName = "Arial", fontSize = 11)
  for (sheet in names(specs)) {
    s <- specs[[sheet]]; d <- s$data; nc <- ncol(d); nr <- nrow(d)
    addWorksheet(wb, sheet, gridLines = FALSE)
    writeData(wb, sheet, s$title, startRow = 2, colNames = FALSE)
    mergeCells(wb, sheet, rows = 2, cols = seq_len(nc))
    addStyle(wb, sheet, createStyle(fontSize = 14, textDecoration = "bold"), rows = 2, cols = 1)
    writeData(wb, sheet, d, startRow = 4)
    addStyle(wb, sheet, createStyle(textDecoration = "bold", wrapText = TRUE,
      valign = "center", halign = "center", border = c("top", "bottom")),
      rows = 4, cols = seq_len(nc), gridExpand = TRUE)
    addStyle(wb, sheet, createStyle(wrapText = TRUE, valign = "top"),
      rows = 5:(nr + 4), cols = seq_len(nc), gridExpand = TRUE)
    if (name == "table_s6_thresholds_confusion") {
      addStyle(wb, sheet,
        createStyle(textDecoration = "bold", border = "bottom"),
        rows = which(d$Fold == "Pooled") + 4L,
        cols = seq_len(nc), gridExpand = TRUE, stack = TRUE)
    }
    for (k in seq_len(nc)) {
      setColWidths(wb, sheet, k, widths = s$widths[k])
      if (is.numeric(d[[k]])) addStyle(wb, sheet, createStyle(numFmt = if (grepl("percent", names(d)[k])) "0.0" else if (names(d)[k] %in% c("Threshold", "Accuracy", "F1")) "0.000" else "0"),
        rows = 5:(nr + 4), cols = k, stack = TRUE)
    }
    heights <- vapply(seq_len(nr), function(i) {
      max(28, 16 * max(ceiling(nchar(as.character(d[i, ])) / (s$widths * 0.85))) + 12)
    }, numeric(1))
    setRowHeights(wb, sheet, rows = 5:(nr + 4), heights = heights)
    setRowHeights(wb, sheet, rows = 4, heights = 42)
    freezePane(wb, sheet, firstActiveRow = 5, firstActiveCol = 2)
    for (i in seq_along(s$notes)) {
      r <- nr + 6 + i
      writeData(wb, sheet, s$notes[i], startRow = r, colNames = FALSE)
      mergeCells(wb, sheet, rows = r, cols = seq_len(nc))
      addStyle(wb, sheet, createStyle(wrapText = TRUE, fontSize = 10), rows = r, cols = 1)
      setRowHeights(wb, sheet, rows = r, heights = 42)
    }
    pageSetup(wb, sheet, orientation = "landscape", fitToWidth = 1, fitToHeight = 0)
  }
  file <- file.path(supplement_dir, paste0(name, ".xlsx"))
  ensure_parent_dir(file)
  saveWorkbook(wb, file, overwrite = TRUE)
  stopifnot(file.copy(file, backup_path(file, run_stamp), overwrite = FALSE))
  for (sheet in names(specs)) {
    expected <- specs[[sheet]]$data
    actual <- read.xlsx(file, sheet = sheet, rows = 4:(4 + nrow(expected)), check.names = FALSE)
    stopifnot(isTRUE(all.equal(actual, as.data.frame(expected), check.attributes = FALSE)))
  }
}

finish_report(report)
