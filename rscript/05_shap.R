# 最终 XGBoost 的描述性 TreeSHAP；在项目根目录运行。 ----
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
library(ggplot2)
library(mlr3learners)
source(file.path(
  project_dir,
  "rscript",
  "tool_functions.R"
))
source(file.path(
  project_dir,
  "rscript",
  "model_functions.R"
))
run_stamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%OS6"
)
model_file <- file.path(
  project_dir,
  "output",
  "lrns",
  run_id,
  "original_full_final.rds"
)
manifest <- readRDS(file.path(
  project_dir,
  "output/logs/training",
  run_id,
  "manifest_finished.rds"
))
config <- readRDS(file.path(
  project_dir,
  "output/logs/training",
  run_id,
  "config.rds"
))
stopifnot(
  manifest$status == "complete",
  config$schema_version == 4L
)
input_files <- c(
  "data_interim/recode_data.RData",
  "data_interim/code_book.csv"
)
stopifnot(identical(
  unname(tools::md5sum(file.path(project_dir, input_files))),
  unname(manifest$files[input_files])
))
report <- start_report(
  project_dir, run_id, "shap",
  c(
    model_file, file.path(project_dir, input_files),
    file.path(project_dir, "output/logs/training", run_id, "manifest_finished.rds"),
    file.path(project_dir, "output/logs/training", run_id, "config.rds")
  ),
  run_stamp
)
bundle <- readRDS(model_file)
model <- bundle$models$xgboost
loaded <- load_analysis_data(
  project_dir,
  config
)
d <- loaded$data
stopifnot(
  model$type == "learner",
  model$sampling == "original",
  identical(
    model$training_ids,
    d$patient_id
  ),
  model$algorithm == "xgboost"
)
selected <- model$selection$selected
feature_labels <- c(
  v10 = "Employment status",
  v13 = "Annual household income",
  v16_osteoarthritis = "Osteoarthritis",
  v19 = "Receipt of medical charitable aid",
  v29 = "Years of regular multidisciplinary care",
  v31_charity_donation = "Charitable support sought from patient organizations",
  v32 = "Hemophilia type",
  v33 = "Hemophilia severity",
  v34 = "Age at diagnosis",
  v35_arthritis = "Arthritis",
  v38 = "Hemophilia-related disability",
  v44_doctor_advice = "Injection frequency: physician advice",
  v44_economic_price = "Injection frequency: cost considerations",
  v52_left_ankle = "Target joint: left ankle",
  v52_left_elbow = "Target joint: left elbow",
  v52_left_knee = "Target joint: left knee",
  v52_right_ankle = "Target joint: right ankle",
  v52_right_knee = "Target joint: right knee"
)
stopifnot(setequal(
  selected,
  names(feature_labels)
))
encoded <- apply_design(
  model$design,
  d[selected]
)
encoded <- encoded[, model$learner$state$feature_names, drop = FALSE]
booster <- model$learner$model
native_probability <- predict(
  booster,
  encoded,
  validate_features = TRUE
)
margin <- predict(
  booster,
  encoded,
  outputmargin = TRUE,
  validate_features = TRUE
)
contributions <- predict(
  booster,
  encoded,
  predcontrib = TRUE,
  approxcontrib = FALSE,
  validate_features = TRUE
)
probability <- predict_base(
  model,
  d
)
stopifnot(
  is.matrix(contributions),
  nrow(contributions) == nrow(d),
  ncol(contributions) == ncol(encoded) + 1L,
  all(is.finite(contributions)),
  max(abs(probability - native_probability)) < 1e-6,
  max(abs(plogis(margin) - probability)) < 1e-6,
  max(abs(rowSums(contributions) - margin)) < 1e-4
)
# 编码列按实际训练映射相加；这不是对重新分组博弈另算的SHAP。
encoded_shap <- contributions[, seq_len(ncol(encoded)), drop = FALSE]
colnames(encoded_shap) <- colnames(encoded)
bias <- contributions[, ncol(contributions)]
mapping <- model$design$mapping
mapping <- mapping[match(
  colnames(encoded),
  mapping$column
), ]
stopifnot(
  !anyNA(mapping),
  setequal(
    mapping$variable,
    selected
  )
)
grouped <- vapply(
  selected,
  function(v) {
    rowSums(
      encoded_shap[,
        mapping$variable == v,
        drop = FALSE
      ]
    )
  },
  numeric(nrow(d))
)
stopifnot(max(abs(rowSums(grouped) + bias - margin)) < 1e-4)
importance <- data.frame(
  variable = selected,
  label = unname(feature_labels[selected]),
  mean_abs_shap = colMeans(abs(grouped))
)
importance <- importance[order(
  importance$mean_abs_shap,
  decreasing = TRUE
), ]
feature_order <- rev(importance$variable)
long <- do.call(
  rbind,
  lapply(
    selected,
    function(v) {
      value <- d[[v]]
      color_value <- rep(
        NA_real_,
        nrow(d)
      )
      color_type <- "Nominal category"
      if (is.numeric(value)) {
        stopifnot(diff(range(value)) > 0)
        color_value <- (value - min(value)) / diff(range(value))
        color_type <- "Numeric (within-feature min-max)"
      } else if (identical(
        levels(value),
        c(
          "no",
          "yes"
        )
      )) {
        color_value <- as.numeric(value == "yes")
        color_type <- "No = 0; yes = 1"
      }
      shap <- grouped[
        ,
        v
      ]
      data.frame(
        patient_id = d$patient_id,
        variable = v,
        label = unname(feature_labels[v]),
        value = as.character(value),
        shap = shap,
        color_value = color_value,
        color_type = color_type
      )
    }
  )
)
table_dir <- file.path(
  project_dir,
  "output",
  "tables",
  "shap",
  run_id
)
plot_dir <- file.path(
  project_dir,
  "output",
  "plots",
  "main"
)
save_table(
  importance,
  file.path(
    table_dir,
    "xgboost_importance.csv"
  ),
  run_stamp
)
save_table(
  long,
  file.path(
    table_dir,
    "xgboost_shap_long.csv"
  ),
  run_stamp
)
save_table(
  data.frame(
    patient_id = d$patient_id,
    bias = bias,
    margin = margin,
    p_high = probability,
    grouped
  ),
  file.path(
    table_dir,
    "xgboost_shap_values.csv"
  ),
  run_stamp
)
save_rds(
  list(
    model_file = model_file,
    model_md5 = tools::md5sum(model_file),
    data_md5 = tools::md5sum(file.path(project_dir, input_files)),
    selector = model$selection$selector,
    selected = selected,
    params = model$params,
    explanation_population = "all 1765 original patients used for final model fitting; descriptive, not held-out performance",
    event = "High (ABR > 1)",
    scale = "raw margin (log-odds of High)",
    method = "exact native XGBoost TreeSHAP, approxcontrib=FALSE",
    background = "tree-path-dependent reference from training Hessian cover stored in booster; no external background sample",
    aggregation = "sum encoded-column SHAP contributions within each original variable",
    color = "numeric min-max and no/yes only; nominal categories grey; zero/skip semantics retain source coding",
    max_additivity_error = max(abs(rowSums(grouped) + bias - margin)),
    max_probability_error = max(abs(probability - native_probability)),
    mapping = mapping,
    session = sessionInfo()
  ),
  file.path(
    table_dir,
    "xgboost_shap_metadata.rds"
  ),
  run_stamp
)
base_theme <- theme_minimal(base_size = 12) + theme(
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_line(
    color = "#E8ECEF",
    linewidth = 0.3
  ),
  axis.text.y = element_text(color = "#222222"),
  plot.margin = margin(
    12,
    18,
    12,
    12
  )
)
# 无序类别用常量占位，构图后明确设为灰色，不赋予类别高低含义。
plot_values <- as.data.frame(lapply(selected, function(v) {
  values <- long$color_value[long$variable == v]
  if (all(is.na(values))) {
    return(rep(0, nrow(d)))
  }
  stopifnot(!anyNA(values))
  values
}))
names(plot_values) <- unname(feature_labels[selected])
plot_shap <- grouped
colnames(plot_shap) <- names(plot_values)
shap_object <- shapviz::shapviz(
  plot_shap,
  X = plot_values,
  baseline = bias[1]
)
summary_plot <- shapviz::sv_importance(
  shap_object,
  kind = "beeswarm",
  max_display = length(selected),
  viridis_args = list(begin = 0.25, end = 0.85, option = "inferno"),
  size = 0.65,
  alpha = 0.6
)
nominal_labels <- unique(long$label[long$color_type == "Nominal category"])
summary_plot$data$color[
  summary_plot$data$feature %in% nominal_labels
] <- NA_real_
summary_plot <- summary_plot +
  labs(x = "SHAP contribution to High ABR (log-odds)") +
  base_theme
save_plot(
  file.path(
    plot_dir,
    paste0(
      "Figure_4B_xgboost_shap_summary.",
      plot_format
    )
  ),
  summary_plot,
  width = 12,
  height = 9,
  dpi = 300,
  run_stamp = run_stamp
)
importance_plot <- shapviz::sv_importance(
  shap_object,
  kind = "bar",
  fill = unname(report_settings$model_colors["xgboost"]),
  max_display = length(selected),
  show_numbers = TRUE
) +
  labs(x = "Mean absolute SHAP contribution (log-odds)") +
  base_theme
save_plot(
  file.path(
    plot_dir,
    paste0(
      "Figure_4A_xgboost_shap_importance.",
      plot_format
    )
  ),
  importance_plot,
  width = 11,
  height = 8.5,
  dpi = 300,
  run_stamp = run_stamp
)
message(
  "SHAP complete: ",
  length(selected),
  " original variables; ",
  nrow(d),
  " patients."
)
print(
  importance[c(
    "label",
    "mean_abs_shap"
  )],
  row.names = FALSE
)

# Figure 4：两面板共享变量顺序，右侧不重复长变量名。
figure_4 <- patchwork::wrap_plots(
  importance_plot,
  summary_plot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  ncol = 2,
  widths = c(1, 1.25)
) + patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))
save_plot(
  file.path(plot_dir, paste0("Figure_4_xgboost_shap.", plot_format)),
  figure_4,
  width = 16, height = 9, dpi = 300, run_stamp = run_stamp
)

# Figure S5 / S6：原始取值用于解释，不能使用着色占位值。
explanation_values <- d[selected]
for (v in selected) {
  if (is.factor(explanation_values[[v]])) {
    levels(explanation_values[[v]]) <- stringr::str_to_sentence(
      gsub("_", " ", levels(explanation_values[[v]]))
    )
  }
}
levels(explanation_values$v38) <- c("None", "Physical", "Intellectual / mental")
names(explanation_values) <- unname(feature_labels[selected])
stopifnot(max(abs(bias - bias[1])) < 1e-6)
explanation_object <- shapviz::shapviz(
  plot_shap,
  X = explanation_values,
  baseline = bias[1]
)
top_variables <- importance$variable[1:6]
dependence_plots <- lapply(top_variables, function(v) {
  shapviz::sv_dependence(
    explanation_object,
    v = unname(feature_labels[v]),
    color_var = NULL,
    color = unname(report_settings$model_colors["xgboost"]),
    jitter_width = if (is.factor(d[[v]])) 0.16 else 0,
    seed = 123,
    alpha = 0.25,
    size = 0.85
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#999999") +
    labs(y = "SHAP contribution (log-odds)") +
    base_theme +
    theme(axis.text.x = element_text(size = 10))
})
supplementary_dir <- file.path(project_dir, "output", "plots", "supplementary")
figure_s5 <- patchwork::wrap_plots(dependence_plots, ncol = 3) +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))
save_plot(
  file.path(supplementary_dir, paste0("Figure_S5_shap_dependence.", plot_format)),
  figure_s5,
  width = 16, height = 9, dpi = 300, run_stamp = run_stamp
)
example_rows <- vapply(c(1L, 0L), function(group) {
  idx <- which(d$y_high == group)
  group_median <- median(probability[idx])
  idx[order(abs(probability[idx] - group_median), d$patient_id[idx])[1]]
}, integer(1))
example_table <- data.frame(
  panel = c("A", "B"),
  patient_id = d$patient_id[example_rows],
  observed = ifelse(d$y_high[example_rows] == 1, "High", "Low"),
  predicted_probability_high = probability[example_rows],
  margin = margin[example_rows],
  baseline = bias[example_rows],
  selection = "closest to within-observed-group median final-model probability; ties ordered by patient_id"
)
save_table(example_table, file.path(table_dir, "shap_example_patients.csv"), run_stamp)
waterfall_plots <- lapply(seq_along(example_rows), function(i) {
  shapviz::sv_waterfall(
    explanation_object,
    row_id = example_rows[i],
    max_display = 9L,
    fill_colors = c("#E64B35", "#3C5488"),
    annotation_size = 3.5
  ) +
    labs(
      x = "Model output (log-odds of High ABR)",
      title = sprintf(
        "Observed %s; P(High) = %.3f",
        example_table$observed[i], probability[example_rows[i]]
      )
    ) +
    theme(plot.title = element_text(size = 12), axis.text.y = element_text(size = 10))
})
figure_s6 <- patchwork::wrap_plots(waterfall_plots, ncol = 1) +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))
save_plot(
  file.path(supplementary_dir, paste0("Figure_S6_shap_waterfall.", plot_format)),
  figure_s6,
  width = 13, height = 12, dpi = 300, run_stamp = run_stamp
)

finish_report(report)
