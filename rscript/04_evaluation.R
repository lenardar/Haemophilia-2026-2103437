# 冻结外层预测的校准评价；在项目根目录运行整个文件。 ----
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
logit_epsilon <- 1e-6 # 仅用于logit校准诊断，不修改原始概率。
library(ggplot2)
source(file.path(
  project_dir,
  "rscript",
  "tool_functions.R"
))
run_stamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%OS6"
)
table_dir <- file.path(
  project_dir,
  "output",
  "tables",
  "evaluation",
  run_id
)
plot_dir <- file.path(
  project_dir,
  "output",
  "plots",
  "evaluation",
  run_id
)
prediction_file <- file.path(
  project_dir,
  "output",
  "tables",
  "training",
  run_id,
  "outer_predictions.csv"
)
data <- readr::read_csv(
  prediction_file,
  show_col_types = FALSE
)
manifest <- readRDS(file.path(
  project_dir,
  "output",
  "logs",
  "training",
  run_id,
  "manifest_finished.rds"
))
stopifnot(
  manifest$status == "complete",
  nrow(data) == manifest$prediction_rows,
  !anyNA(data[c(
    "patient_id",
    "y_high",
    "p_high_raw"
  )]),
  all(data$p_high_raw >= 0 & data$p_high_raw <= 1),
  all(data$y_high %in% 0:1),
  !anyDuplicated(data[c(
    "scenario",
    "algorithm",
    "repeat_id",
    "patient_id"
  )]),
  all(table(
    data$scenario,
    data$algorithm
  ) == manifest$n)
)


# 两个冻结批次通过患者、结局和外层折匹配，禁止按行位置拼接。
restricted_prediction_file <- file.path(
  project_dir, "output/tables/training", report_settings$restricted_run_id,
  "outer_predictions.csv"
)
restricted_inputs <- c(
  restricted_prediction_file,
  file.path(project_dir, "output/logs/training",
    report_settings$restricted_run_id, "manifest_finished.rds"),
  file.path(project_dir, "output/logs/training",
    c(run_id, report_settings$restricted_run_id), "config.rds"),
  file.path(project_dir, "output/tables/training",
    c(run_id, report_settings$restricted_run_id), "variable_registry.csv")
)

report <- start_report(
  project_dir, run_id, "evaluation",
  c(prediction_file, restricted_inputs, file.path(
    project_dir, "output/logs/training", run_id,
    "manifest_finished.rds"
  )), run_stamp
)

calibration_fit <- function(
  x,
  epsilon
) {
  # 此边界处理仅定义 logit 诊断，不写回患者概率。
  p <- pmin(
    pmax(
      x$p_high_raw,
      epsilon
    ),
    1 - epsilon
  )
  z <- qlogis(p)
  y <- x$y_high
  stopifnot(length(unique(z)) > 1L)
  # 截距模型的似然得分单调；直接求根避免极端 offset 下 IRLS 假收敛。
  score <- function(a) sum(y - plogis(a + z))
  bound <- max(abs(z)) + 40
  intercept <- uniroot(
    score,
    interval = c(
      -bound,
      bound
    ),
    tol = 1e-10
  )$root
  slope <- glm(
    y ~ z,
    family = binomial()
  )
  stopifnot(
    slope$converged,
    is.finite(intercept),
    abs(score(intercept)) < 1e-6,
    all(is.finite(coef(slope)))
  )
  c(
    citl = intercept,
    joint_intercept = unname(coef(slope)[1]),
    slope = unname(coef(slope)[2]),
    n_boundary_adjusted = sum(p != x$p_high_raw)
  )
}

metric_rows <- list()
sensitivity_rows <- list()
curve_rows <- list()
bin_rows <- list()
groups <- split(
  data,
  interaction(
    data$scenario,
    data$algorithm,
    data$repeat_id,
    data$outer_fold,
    drop = TRUE
  )
)
for (i in seq_along(groups)) {
  x <- groups[[i]]
  key <- as.data.frame(
    x[
      1,
      c(
        "scenario",
        "algorithm",
        "repeat_id",
        "outer_fold"
      )
    ]
  )
  p <- x$p_high_raw
  y <- x$y_high
  stopifnot(length(unique(x$training_prevalence)) == 1L)
  brier <- mean((p - y)^2)
  baseline <- mean((x$training_prevalence - y)^2)
  cal <- calibration_fit(
    x,
    logit_epsilon
  )
  for (epsilon in c(
    1e-4,
    1e-6,
    1e-8
  )) {
    sensitivity_rows[[length(sensitivity_rows) + 1L]] <- cbind(
      key,
      epsilon = epsilon,
      as.data.frame(as.list(calibration_fit(
        x,
        epsilon
      )))
    )
  }
  # 固定复杂度的逻辑样条仅描绘测试集校准关系，不应用到模型输出。
  bounds <- range(p)
  knots <- unique(quantile(
    p,
    c(
      1 / 3,
      2 / 3
    ),
    names = FALSE
  ))
  knots <- knots[knots > bounds[1] & knots < bounds[2]]
  basis <- splines::ns(
    p,
    knots = knots,
    Boundary.knots = bounds
  )
  smooth_df <- ncol(basis)
  fit <- glm(
    y ~ .,
    data = data.frame(
      y = y,
      basis
    ),
    family = binomial(),
    control = glm.control(maxit = 100)
  )
  stopifnot(
    fit$converged,
    all(is.finite(coef(fit)))
  )
  grid <- sort(unique(c(
    seq(
      min(p),
      max(p),
      length.out = 101
    ),
    p
  )))
  predicted <- predict(
    fit,
    newdata = data.frame(predict(
      basis,
      newx = grid
    )),
    se.fit = TRUE
  )
  curve <- plogis(predicted$fit)
  stopifnot(
    all(is.finite(curve)),
    all(is.finite(predicted$se.fit))
  )
  curve_rows[[i]] <- cbind(
    key,
    predicted = grid,
    observed_smooth = curve,
    lower = plogis(predicted$fit - qnorm(0.975) * predicted$se.fit),
    upper = plogis(predicted$fit + qnorm(0.975) * predicted$se.fit)
  )
  fitted_calibration <- fitted(fit)
  metric_rows[[i]] <- cbind(
    key,
    n = length(y),
    prevalence = mean(y),
    mean_probability = mean(p),
    brier = brier,
    baseline_brier = baseline,
    brier_skill = 1 - brier / baseline,
    as.data.frame(as.list(cal)),
    n_zero = sum(p == 0),
    n_one = sum(p == 1),
    eavg = mean(abs(fitted_calibration - p)),
    emax_observed_range = max(abs(curve - grid)),
    smoother_df = smooth_df
  )
  # 相同预测概率始终位于同一组；分位数重合时减少组数。
  cuts <- unique(quantile(
    p,
    seq(
      0,
      1,
      0.1
    ),
    names = FALSE
  ))
  bin <- cut(
    p,
    breaks = cuts,
    include.lowest = TRUE
  )
  stopifnot(!anyNA(bin))
  bin_rows[[i]] <- do.call(
    rbind,
    lapply(
      split(
        seq_along(p),
        bin
      ),
      function(j) {
        stopifnot(length(j) > 0L)
        n <- length(j)
        observed <- mean(y[j])
        z <- qnorm(0.975)
        center <- (observed + z^2 / (2 * n)) / (1 + z^2 / n)
        half <- z * sqrt(observed * (1 - observed) / n + z^2 / (4 * n^2)) /
          (1 + z^2 / n)
        cbind(
          key,
          predicted = mean(p[j]),
          observed = observed,
          n = n,
          lower = center - half,
          upper = center + half
        )
      }
    )
  )
}
metrics <- do.call(
  rbind,
  metric_rows
)
sensitivity <- do.call(
  rbind,
  sensitivity_rows
)
curves <- do.call(
  rbind,
  curve_rows
)
bins <- do.call(
  rbind,
  bin_rows
)
columns <- c(
  "prevalence",
  "mean_probability",
  "brier",
  "baseline_brier",
  "brier_skill",
  "citl",
  "slope",
  "eavg",
  "emax_observed_range"
)
summary_mean <- aggregate(
  metrics[columns],
  metrics[c(
    "scenario",
    "algorithm"
  )],
  mean
)
summary_sd <- aggregate(
  metrics[columns],
  metrics[c(
    "scenario",
    "algorithm"
  )],
  sd
)
names(summary_sd)[-(1:2)] <- paste0(
  columns,
  "_fold_sd"
)
summary <- merge(
  summary_mean,
  summary_sd,
  by = c(
    "scenario",
    "algorithm"
  )
)
for (name in c(
  "metrics",
  "sensitivity",
  "curves",
  "bins",
  "summary"
)) {
  save_table(
    get(name),
    file.path(
      table_dir,
      paste0(
        "calibration_",
        name,
        ".csv"
      )
    ),
    run_stamp
  )
}
for (scenario_name in unique(data$scenario)) {
  c <- curves[curves$scenario == scenario_name, ]
  b <- bins[bins$scenario == scenario_name, ]
  raw <- data[data$scenario == scenario_name, ]
  plot <- ggplot(
    c,
    aes(
      predicted,
      observed_smooth
    )
  ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = 2,
      color = "grey45"
    ) +
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper
      ),
      fill = "#2878A5",
      alpha = 0.18
    ) +
    geom_line(
      color = "#2878A5",
      linewidth = 0.5
    ) +
    geom_point(
      data = b,
      aes(
        predicted,
        observed
      ),
      size = 0.8
    ) +
    geom_rug(
      data = raw,
      aes(x = p_high_raw),
      inherit.aes = FALSE,
      sides = "b",
      alpha = 0.12
    ) +
    facet_grid(
      algorithm ~ outer_fold,
      labeller = label_both
    ) +
    coord_cartesian(
      xlim = c(
        0,
        1
      ),
      ylim = c(
        0,
        1
      )
    ) +
    labs(
      title = scenario_name,
      subtitle = "Outer test folds; High is the event; original probabilities",
      x = "Predicted probability of High",
      y = "Observed probability of High",
      caption = paste(
        "Blue: logistic spline (up to 3 df), pointwise 95% model-based Wald intervals.",
        "Dots: quantile-bin observed rates; rugs: predictions. Dashed: ideal calibration.",
        "Diagnostic intervals conditional on fixed predictions; no development-process uncertainty.",
        sep = "\n"
      )
    ) +
    theme_bw(base_size = 9) +
    theme(
      strip.text.y = element_text(angle = 0),
      panel.grid.minor = element_blank()
    )
  save_plot(
    file.path(
      plot_dir,
      paste0(
        "calibration_",
        scenario_name,
        ".",
        plot_format
      )
    ),
    plot,
    width = 15,
    height = 19,
    dpi = 150,
    run_stamp = run_stamp
  )
}
metadata <- list(
  run_id = run_id,
  input_md5 = tools::md5sum(prediction_file),
  logit_epsilon = logit_epsilon,
  sensitivity_epsilon = c(
    1e-4,
    1e-6,
    1e-8
  ),
  smoother = paste(
    "binomial GLM natural spline of raw probability; unique tertile knots",
    "strictly inside observed range; up to 3 df"
  ),
  curve_interval = "pointwise model-based Wald 95%; not bootstrap",
  stage = "fold-level spline diagnostics; manuscript table uses LOWESS Eavg/Emax with bootstrap intervals",
  session = sessionInfo()
)
save_rds(
  metadata,
  file.path(
    table_dir,
    "calibration_metadata.rds"
  ),
  run_stamp
)
print(
  summary[c(
    "scenario",
    "algorithm",
    columns
  )],
  row.names = FALSE
)
message(
  "Calibration evaluation complete: ",
  run_id
)

# DCA：阈值表达决策代价，不使用 Youden 分类阈值。 ----
dca_thresholds <- seq(
  0,
  0.99,
  by = 0.01
)
dca_bootstrap <- 2000L
dca_seed <- 20261006L
set.seed(dca_seed)
dca_rows <- list()
stopifnot(length(unique(data$repeat_id)) == 1L)
dca_boot_means <- list()
dca_boot_all_means <- list()
dca_fold_counts <- table(unique(data[c(
  "scenario",
  "outer_fold"
)])$scenario)
fold_groups <- split(
  data,
  interaction(
    data$scenario,
    data$repeat_id,
    data$outer_fold,
    drop = TRUE
  )
)
for (fold_data in fold_groups) {
  ids <- sort(unique(fold_data$patient_id))
  n <- length(ids)
  weights <- rmultinom(
    dca_bootstrap,
    n,
    rep(
      1 / n,
      n
    )
  )
  for (algorithm_name in c(
    unique(fold_data$algorithm),
    "prevalence_only"
  )) {
    source_algorithm <- if (algorithm_name == "prevalence_only") {
      fold_data$algorithm[1]
    } else {
      algorithm_name
    }
    x <- fold_data[fold_data$algorithm == source_algorithm, ]
    x <- x[match(
      ids,
      x$patient_id
    ), ]
    stopifnot(
      nrow(x) == n,
      !anyNA(x$patient_id)
    )
    y <- x$y_high
    stopifnot(length(unique(x$training_prevalence)) == 1L)
    p <- if (algorithm_name == "prevalence_only") {
      x$training_prevalence
    } else {
      x$p_high_raw
    }
    prevalence <- mean(y)
    odds <- dca_thresholds / (1 - dca_thresholds)
    tp <- vapply(
      dca_thresholds,
      function(t) sum(y[p >= t] == 1),
      numeric(1)
    )
    fp <- vapply(
      dca_thresholds,
      function(t) sum(y[p >= t] == 0),
      numeric(1)
    )
    nb <- (tp - fp * odds) / n
    all_nb <- prevalence - (1 - prevalence) * odds
    stopifnot(
      abs(nb[1] - prevalence) < 1e-12,
      abs(all_nb[1] - prevalence) < 1e-12
    )
    # 多项分布权重等价于普通患者有放回抽样；同折所有算法共用权重。
    order_p <- order(
      p,
      decreasing = TRUE
    )
    k <- vapply(
      dca_thresholds,
      function(t) sum(p >= t),
      integer(1)
    )
    cumulative_tp <- rbind(
      0,
      apply(
        weights[order_p, ] * y[order_p],
        2,
        cumsum
      )
    )
    cumulative_fp <- rbind(
      0,
      apply(
        weights[order_p, ] * (1 - y[order_p]),
        2,
        cumsum
      )
    )
    boot_nb <- (
      cumulative_tp[k + 1L, ] - cumulative_fp[k + 1L, ] * odds
    ) / n
    boot_prevalence <- colSums(weights * y) / n
    boot_all <- outer(
      1 + odds,
      boot_prevalence
    ) - odds
    boot_delta <- boot_nb - pmax(
      boot_all,
      0
    )
    stopifnot(all(is.finite(boot_nb)))
    mean_key <- paste(
      x$scenario[1],
      algorithm_name,
      sep = "/"
    )
    fold_count <- unname(dca_fold_counts[x$scenario[1]])
    if (!mean_key %in% names(dca_boot_means)) {
      dca_boot_means[[mean_key]] <- boot_nb / fold_count
      dca_boot_all_means[[mean_key]] <- boot_all / fold_count
    } else {
      dca_boot_means[[mean_key]] <- dca_boot_means[[mean_key]] + boot_nb / fold_count
      dca_boot_all_means[[mean_key]] <-
        dca_boot_all_means[[mean_key]] + boot_all / fold_count
    }

    ci <- t(apply(
      boot_nb,
      1,
      quantile,
      probs = c(
        0.025,
        0.975
      )
    ))
    delta_ci <- t(apply(
      boot_delta,
      1,
      quantile,
      probs = c(
        0.025,
        0.975
      )
    ))
    dca_rows[[length(dca_rows) + 1L]] <- data.frame(
      scenario = x$scenario[1],
      algorithm = algorithm_name,
      repeat_id = x$repeat_id[1],
      outer_fold = x$outer_fold[1],
      threshold = dca_thresholds,
      n = n,
      training_prevalence = x$training_prevalence[1],
      tp = tp,
      fp = fp,
      net_benefit = nb,
      treat_all = all_nb,
      treat_none = 0,
      delta_best_default = nb - pmax(
        all_nb,
        0
      ),
      lower = ci[
        ,
        1
      ],
      upper = ci[
        ,
        2
      ],
      delta_lower = delta_ci[
        ,
        1
      ],
      delta_upper = delta_ci[
        ,
        2
      ]
    )
  }
  message(
    "DCA completed: ",
    x$scenario[1],
    " / fold ",
    x$outer_fold[1]
  )
}
dca <- do.call(
  rbind,
  dca_rows
)
dca_mean <- aggregate(
  dca[c(
    "net_benefit",
    "treat_all",
    "treat_none",
    "delta_best_default"
  )],
  dca[c(
    "scenario",
    "algorithm",
    "threshold"
  )],
  mean
)
# 对每次重抽样先求折均值，再取分位数；不平均逐折区间端点。
dca_mean$lower <- NA_real_
dca_mean$upper <- NA_real_
dca_mean$delta_mean_best_default <- dca_mean$net_benefit -
  pmax(
    dca_mean$treat_all,
    0
  )
dca_mean$delta_mean_lower <- NA_real_
dca_mean$delta_mean_upper <- NA_real_
for (i in seq_len(nrow(dca_mean))) {
  row <- dca_mean[i, ]
  key <- paste(
    row$scenario,
    row$algorithm,
    sep = "/"
  )
  t_index <- match(
    row$threshold,
    dca_thresholds
  )
  boot_mean <- dca_boot_means[[key]][t_index, ]
  boot_difference <- boot_mean - pmax(
    dca_boot_all_means[[key]][t_index, ],
    0
  )
  interval <- quantile(
    boot_mean,
    c(
      0.025,
      0.975
    )
  )
  difference_interval <- quantile(
    boot_difference,
    c(
      0.025,
      0.975
    )
  )
  dca_mean$lower[i] <- interval[1]
  dca_mean$upper[i] <- interval[2]
  dca_mean$delta_mean_lower[i] <- difference_interval[1]
  dca_mean$delta_mean_upper[i] <- difference_interval[2]
}
stopifnot(
  !anyNA(dca_mean),
  all(dca_mean$lower <= dca_mean$upper)
)
for (name in c(
  "dca",
  "dca_mean"
)) {
  save_table(
    get(name),
    file.path(
      table_dir,
      paste0(
        name,
        ".csv"
      )
    ),
    run_stamp
  )
}
baseline <- unique(dca_mean[c(
  "scenario",
  "threshold",
  "treat_all"
)])
reference <- rbind(
  transform(
    baseline,
    net_benefit = treat_all,
    strategy = "Treat all"
  ),
  transform(
    baseline,
    net_benefit = 0,
    strategy = "Treat none"
  ),
  transform(
    dca_mean[
      dca_mean$algorithm == "prevalence_only",
      c(
        "scenario",
        "threshold",
        "treat_all",
        "net_benefit"
      )
    ],
    strategy = "Prevalence only"
  )
)
model_labels <- c(report_settings$model_labels, selection = "Inner-CV selection")
stopifnot(all(setdiff(
  unique(dca_mean$algorithm),
  "prevalence_only"
) %in%
  names(model_labels)))
model_colors <- c(report_settings$model_colors, selection = "#333333")
scenario_labels <- c(
  original_restricted = "Original distribution\nRestricted predictors",
  original_full = "Original distribution\nFull predictors",
  smotenc_full = "SMOTENC\nFull predictors",
  smotenc_restricted = "SMOTENC\nRestricted predictors"
)
plot <- ggplot(
  dca_mean[dca_mean$algorithm != "prevalence_only", ],
  aes(
    threshold,
    net_benefit,
    color = algorithm
  )
) +
  geom_line(
    linewidth = 0.65,
    alpha = 0.9
  ) +
  geom_line(
    data = reference,
    aes(
      threshold,
      net_benefit,
      linetype = strategy
    ),
    inherit.aes = FALSE,
    color = "#646464",
    linewidth = 0.75
  ) +
  facet_wrap(
    ~scenario,
    nrow = 1,
    labeller = as_labeller(scenario_labels)
  ) +
  coord_cartesian(ylim = c(
    -0.05,
    0.95
  )) +
  scale_color_manual(
    values = model_colors,
    breaks = names(model_labels),
    labels = model_labels
  ) +
  scale_linetype_manual(
    values = c(
      "Treat all" = "dashed",
      "Treat none" = "dotted",
      "Prevalence only" = "dotdash"
    ),
    breaks = c(
      "Treat all",
      "Treat none",
      "Prevalence only"
    )
  ) +
  scale_x_continuous(breaks = seq(
    0,
    1,
    0.2
  )) +
  scale_y_continuous(breaks = seq(
    0,
    0.9,
    0.1
  )) +
  labs(
    title = "Decision curve analysis",
    subtitle = "High as the event | Mean net benefit across five outer test folds",
    x = "Threshold probability",
    y = "Net benefit",
    color = "Model",
    linetype = "Reference",
    caption = paste(
      "Prevalence only uses the original training-fold event proportion; it overlaps Treat all below that probability and Treat none above it.",
      "Thresholds 0.00–0.99 are exploratory. Values below −0.05 are outside the display; full values are retained in the tables.",
      "Per-fold intervals from 2,000 paired patient resamples are in dca.csv; no confidence interval for the fold mean is shown.",
      sep = "\n"
    )
  ) +
  guides(
    color = guide_legend(
      order = 1,
      nrow = 2,
      byrow = TRUE
    ),
    linetype = guide_legend(
      order = 2,
      nrow = 1
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "#E9EDF1",
      linewidth = 0.35
    ),
    strip.text = element_text(
      face = "bold",
      size = 12,
      margin = margin(b = 12)
    ),
    panel.spacing = grid::unit(
      1.3,
      "lines"
    ),
    plot.title = element_text(
      face = "bold",
      size = 19
    ),
    plot.subtitle = element_text(
      color = "#555555",
      margin = margin(b = 16)
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      lineheight = 1.2,
      color = "#555555",
      margin = margin(t = 12)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.box = "vertical",
    legend.key.width = grid::unit(
      1.1,
      "cm"
    ),
    legend.title = element_text(face = "bold"),
    plot.margin = margin(
      16,
      20,
      14,
      16
    )
  )
save_plot(
  file.path(
    plot_dir,
    paste0(
      "dca_mean.",
      plot_format
    )
  ),
  plot,
  width = 15,
  height = 7,
  dpi = 150,
  run_stamp = run_stamp
)

save_rds(
  list(
    seed = dca_seed,
    bootstrap = dca_bootstrap,
    thresholds = dca_thresholds,
    event = "High",
    decision = "p_high_raw >= threshold",
    prevalence_only = "original training-fold event proportion >= threshold",
    uncertainty = "paired ordinary patient bootstrap within each test fold; fixed models",
    mean_interval = "pointwise percentile interval of equal-weight fold-mean net benefit; 2000 resamples; excludes retraining variability and overlapping training-set dependence; not full-procedure CV or simultaneous coverage",
    mean_difference = "mean model net benefit minus max(mean treat-all net benefit, 0), recomputed in each paired resample",
    input_md5 = tools::md5sum(prediction_file),
    session = sessionInfo()
  ),
  file.path(
    table_dir,
    "dca_metadata.rds"
  ),
  run_stamp
)
message(
  "DCA complete: ",
  run_id
)

# 主分析性能表：固定外层预测的条件 bootstrap 区间。 ----
performance_metrics <- function(
  x,
  w = rep(
    1L,
    nrow(x)
  )
) {
  y <- x$y_high
  pred <- x$class_raw
  tp <- sum(w * (y == 1 & pred == 1))
  fp <- sum(w * (y == 0 & pred == 1))
  tn <- sum(w * (y == 0 & pred == 0))
  fn <- sum(w * (y == 1 & pred == 0))
  stopifnot(
    tp + fn > 0,
    tn + fp > 0,
    tp + fp > 0,
    tn + fn > 0
  )
  # 同一概率的观测作为一个阈值点，避免任意排列 ties。
  g <- match(
    x$p_high_raw,
    sort(
      unique(x$p_high_raw),
      decreasing = TRUE
    )
  )
  pos <- as.numeric(rowsum(
    w * y,
    g,
    reorder = TRUE
  ))
  neg <- as.numeric(rowsum(
    w * (1 - y),
    g,
    reorder = TRUE
  ))
  keep <- pos + neg > 0
  pos <- pos[keep]
  neg <- neg[keep]
  recall <- c(
    0,
    cumsum(pos) / sum(pos)
  )
  fpr <- c(
    0,
    cumsum(neg) / sum(neg)
  )
  precision <- c(
    1,
    cumsum(pos) / cumsum(pos + neg)
  )
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  c(
    roc_auc = sum(diff(fpr) * (head(
      recall,
      -1
    ) + tail(
      recall,
      -1
    )) / 2),
    pr_auc = sum(diff(recall) * (head(
      precision,
      -1
    ) + tail(
      precision,
      -1
    )) / 2),
    sensitivity = sensitivity,
    specificity = specificity,
    ppv = tp / (tp + fp),
    npv = tn / (tn + fn),
    balanced_accuracy = (sensitivity + specificity) / 2,
    mcc = (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  )
}
if ("original_full" %in% data$scenario) {
  performance_seed <- 20261007L
  performance_bootstrap <- 2000L
  main <- data[data$scenario == "original_full", ]
  stopifnot(
    length(unique(main$repeat_id)) == 1L,
    all(main$class_raw == as.integer(main$p_high_raw >= main$threshold_raw))
  )
  algorithms <- c(
    "logistic",
    "logistic_spline",
    "tree",
    "rf",
    "svm",
    "xgboost",
    "knn",
    "naive_bayes",
    "selection"
  )
  stopifnot(setequal(
    unique(main$algorithm),
    algorithms
  ))
  set.seed(performance_seed)
  point_rows <- list()
  bootstrap_means <- setNames(
    vector(
      "list",
      length(algorithms)
    ),
    algorithms
  )
  folds <- sort(unique(main$outer_fold))
  stopifnot(length(folds) == 5L)
  for (f in folds) {
    fd <- main[main$outer_fold == f, ]
    ids <- sort(unique(fd$patient_id))
    n <- length(ids)
    weights <- rmultinom(
      performance_bootstrap,
      n,
      rep(
        1 / n,
        n
      )
    )
    for (a in algorithms) {
      x <- fd[fd$algorithm == a, ]
      x <- x[match(
        ids,
        x$patient_id
      ), ]
      stopifnot(
        nrow(x) == n,
        !anyNA(x$patient_id)
      )
      estimate <- performance_metrics(x)
      boot <- vapply(
        seq_len(performance_bootstrap),
        function(b) {
          performance_metrics(
            x,
            weights[
              ,
              b
            ]
          )
        },
        numeric(length(estimate))
      )
      stopifnot(all(is.finite(boot)))
      point_rows[[length(point_rows) + 1L]] <- data.frame(
        algorithm = a,
        outer_fold = f,
        metric = names(estimate),
        estimate = estimate
      )
      if (f == folds[1]) {
        bootstrap_means[[a]] <- boot / length(folds)
      } else {
        bootstrap_means[[a]] <- bootstrap_means[[a]] + boot / length(folds)
      }
    }
    message(
      "Performance bootstrap completed: fold ",
      f
    )
  }
  performance_by_fold <- do.call(
    rbind,
    point_rows
  )
  performance_summary <- aggregate(
    estimate ~ algorithm + metric,
    performance_by_fold,
    mean
  )
  performance_summary$lower <- NA_real_
  performance_summary$upper <- NA_real_
  metric_names <- names(estimate)
  for (i in seq_len(nrow(performance_summary))) {
    r <- performance_summary[i, ]
    ci <- quantile(
      bootstrap_means[[r$algorithm]][match(
        r$metric,
        metric_names
      ), ],
      c(
        0.025,
        0.975
      )
    )
    performance_summary$lower[i] <- ci[1]
    performance_summary$upper[i] <- ci[2]
  }
  stopifnot(!anyNA(performance_summary))
  save_table(
    performance_by_fold,
    file.path(
      table_dir,
      "performance_original_full_by_fold.csv"
    ),
    run_stamp
  )
  save_table(
    performance_summary,
    file.path(
      table_dir,
      "performance_original_full_summary.csv"
    ),
    run_stamp
  )
  save_rds(
    list(
      seed = performance_seed,
      bootstrap = performance_bootstrap,
      point_estimate = "equal-weight mean of five outer test fold metrics",
      interval = "2.5th and 97.5th percentiles of fold-mean metrics; ordinary patient bootstrap independently within folds, paired across algorithms; predictions and thresholds fixed; excludes retraining variability and does not establish full CV coverage",
      pr_auc = "trapezoidal area; tied scores grouped; precision at recall zero is 1; High is positive; not average precision",
      input_md5 = tools::md5sum(prediction_file)
    ),
    file.path(
      table_dir,
      "performance_metadata.rds"
    ),
    run_stamp
  )
}

# 正文校准表：固定预测的患者 bootstrap；LOWESS误差每次重新估计。 ----
if ("original_full" %in% data$scenario) {
  cal_table_seed <- 20261008L
  cal_table_bootstrap <- 2000L
  cal_algorithms <- c(
    "logistic",
    "logistic_spline",
    "tree",
    "rf",
    "svm",
    "xgboost",
    "knn",
    "naive_bayes"
  )
  cal_main <- data[data$scenario == "original_full", ]
  stopifnot(length(unique(cal_main$repeat_id)) == 1L)
  cal_folds <- sort(unique(cal_main$outer_fold))
  stopifnot(length(cal_folds) == 5L)
  # Brier draws are shared with the sensitivity comparisons.
  cal_brier_seed <- 20261007L
  set.seed(cal_brier_seed)
  cal_brier_weights <- lapply(cal_folds, function(f) {
    n <- length(unique(cal_main$patient_id[cal_main$outer_fold == f]))
    rmultinom(cal_table_bootstrap, n, rep(1 / n, n))
  })
  set.seed(cal_table_seed)
  cal_boot_means <- setNames(
    vector(
      "list",
      length(cal_algorithms)
    ),
    cal_algorithms
  )
  cal_point_rows <- list()
  cal_warning_rows <- list()
  cal_reference_boot <- numeric(cal_table_bootstrap)
  cal_reference_point <- numeric(length(cal_folds))
  for (f in cal_folds) {
    fd <- cal_main[cal_main$outer_fold == f, ]
    ids <- sort(unique(fd$patient_id))
    n <- length(ids)
    weights <- rmultinom(
      cal_table_bootstrap,
      n,
      rep(
        1 / n,
        n
      )
    )
    for (a in cal_algorithms) {
      x <- fd[fd$algorithm == a, ]
      x <- x[match(
        ids,
        x$patient_id
      ), ]
      stopifnot(
        nrow(x) == n,
        !anyNA(x$patient_id)
      )
      y <- x$y_high
      p <- x$p_high_raw
      z <- qlogis(pmin(
        pmax(
          p,
          logit_epsilon
        ),
        1 - logit_epsilon
      ))
      slope_design <- cbind(1, z)
      bound <- max(abs(z)) + 40
      evaluate <- function(w,
                           include_smooth = TRUE,
                           bootstrap_id = 0L) {
        stopifnot(
          sum(w * y) > 0,
          sum(w * (1 - y)) > 0
        )
        score <- function(intercept) sum(w * (y - plogis(intercept + z)))
        citl <- uniroot(
          score,
          c(
            -bound,
            bound
          ),
          tol = 1e-10
        )$root
        slope_fit <- withCallingHandlers(
          glm.fit(
            slope_design,
            y,
            weights = w,
            family = binomial(),
            control = glm.control(maxit = 100)
          ),
          warning = function(warning) {
            cal_warning_rows[[length(cal_warning_rows) + 1L]] <<- data.frame(
              algorithm = a, outer_fold = f, bootstrap_id = bootstrap_id,
              component = "calibration_slope", message = conditionMessage(warning)
            )
          }
        )
        stopifnot(
          slope_fit$converged,
          all(is.finite(slope_fit$coefficients)),
          abs(score(citl)) < 1e-6
        )
        base <- c(
          brier = sum(w * (p - y)^2) / sum(w),
          citl = citl,
          slope = unname(slope_fit$coefficients[2])
        )
        if (!include_smooth) {
          return(base)
        }
        # 与rms::val.prob的Eavg/Emax定义一致；计数权重展开为患者重抽样。
        # LOWESS仅估计校准误差，不写回任何患者预测。
        stopifnot(all(w >= 0), all(w == as.integer(w)))
        sampled <- rep(seq_along(p), times = w)
        smooth <- stats::lowess(
          p[sampled],
          y[sampled],
          f = 2 / 3,
          iter = 0,
          delta = 0.01 * diff(range(p[sampled]))
        )
        calibrated <- stats::approx(
          smooth,
          xout = p[sampled],
          ties = mean
        )$y
        stopifnot(all(is.finite(calibrated)))
        errors <- abs(calibrated - p[sampled])
        c(base, eavg = mean(errors), emax = max(errors))
      }
      point <- evaluate(
        rep(
          1L,
          n
        ),
        include_smooth = TRUE
      )
      boot <- vapply(
        seq_len(cal_table_bootstrap),
        function(b) {
          evaluate(
            weights[
              ,
              b
            ],
            bootstrap_id = b
          )
        },
        numeric(5)
      )
      boot["brier", ] <- colSums(
        cal_brier_weights[[match(f, cal_folds)]] * (p - y)^2
      ) / n
      stopifnot(all(is.finite(boot)))
      cal_point_rows[[length(cal_point_rows) + 1L]] <- data.frame(
        algorithm = a,
        outer_fold = f,
        metric = names(point),
        estimate = point
      )
      if (f == cal_folds[1]) {
        cal_boot_means[[a]] <- boot / length(cal_folds)
      } else {
        cal_boot_means[[a]] <- cal_boot_means[[a]] + boot / length(cal_folds)
      }
      message(
        "Calibration bootstrap completed: ",
        a,
        " / fold ",
        f
      )
    }
    loss <- (x$training_prevalence - y)^2
    cal_reference_point[f] <- mean(loss)
    cal_reference_boot <- cal_reference_boot +
      colSums(cal_brier_weights[[match(f, cal_folds)]] * loss) /
        n / length(cal_folds)
  }
  calibration_table_by_fold <- do.call(
    rbind,
    cal_point_rows
  )
  calibration_table_summary <- aggregate(
    estimate ~ algorithm + metric,
    calibration_table_by_fold,
    mean
  )
  calibration_table_summary$lower <- NA_real_
  calibration_table_summary$upper <- NA_real_
  for (i in seq_len(nrow(calibration_table_summary))) {
    r <- calibration_table_summary[i, ]
    ci <- quantile(
      cal_boot_means[[r$algorithm]][match(
        r$metric,
        names(point)
      ), ],
      c(
        0.025,
        0.975
      )
    )
    calibration_table_summary$lower[i] <- ci[1]
    calibration_table_summary$upper[i] <- ci[2]
  }
  ref_ci <- quantile(
    cal_reference_boot,
    c(
      0.025,
      0.975
    )
  )
  calibration_table_summary <- rbind(
    calibration_table_summary,
    data.frame(
      algorithm = "prevalence_only",
      metric = "brier",
      estimate = mean(cal_reference_point),
      lower = ref_ci[1],
      upper = ref_ci[2]
    )
  )
  calibration_table_summary$interval_status <-
    "Conditional percentile bootstrap interval"
  stopifnot(
    all(is.finite(calibration_table_summary$estimate)),
    all(is.finite(calibration_table_summary$lower)),
    all(is.finite(calibration_table_summary$upper))
  )
  save_table(
    calibration_table_by_fold,
    file.path(
      table_dir,
      "calibration_table_original_full_by_fold.csv"
    ),
    run_stamp
  )
  save_table(
    calibration_table_summary,
    file.path(
      table_dir,
      "calibration_table_original_full_summary.csv"
    ),
    run_stamp
  )
  warning_audit <- if (length(cal_warning_rows) > 0L) {
    do.call(rbind, cal_warning_rows)
  } else {
    data.frame(algorithm = character(), outer_fold = integer(),
      bootstrap_id = integer(), component = character(), message = character())
  }
  save_table(warning_audit,
    file.path(table_dir, "calibration_table_warning_audit.csv"), run_stamp)
  save_rds(
    list(
      seed = cal_table_seed,
      brier_seed = cal_brier_seed,
      brier_draws = "Shared with sensitivity comparisons; includes prevalence-only reference",
      bootstrap = cal_table_bootstrap,
      epsilon = logit_epsilon,
      complete = TRUE,
      warning_count = nrow(warning_audit),
      warning_policy = "Warnings retained in audit; all slope fits must converge with finite coefficients; no resamples dropped",
      intercept = "calibration-in-the-large: slope fixed at 1",
      slope = "logistic calibration with freely estimated intercept",
      smoother = "LOWESS (stats::lowess, f=2/3, iter=0, delta=1% sample probability range); no clipping; refitted in each resample",
      source = "https://github.com/harrelfe/rms/blob/master/R/val.prob.s",
      eavg = "mean absolute LOWESS error at observed patient probabilities",
      emax = "maximum absolute LOWESS error at observed patient probabilities; evaluated on each resample support",
      interval = "percentile interval of fold-mean metrics, paired patient resampling within folds; excludes retraining variability; not full-procedure CV confidence interval",
      input_md5 = tools::md5sum(prediction_file)
    ),
    file.path(
      table_dir,
      "calibration_table_metadata.rds"
    ),
    run_stamp
  )
}

# 正文校准图：合并外层测试预测，每患者每算法一条记录。 ----
if ("original_full" %in% data$scenario) {
  main_labels <- report_settings$model_labels
  main_predictions <- data[
    data$scenario == "original_full" & data$algorithm %in% names(main_labels),
  ]
  stopifnot(
    length(unique(main_predictions$repeat_id)) == 1L,
    !anyDuplicated(main_predictions[c(
      "patient_id",
      "algorithm"
    )]),
    !anyNA(main_predictions[c(
      "y_high",
      "p_high_raw"
    )])
  )
  main_curves <- list()
  for (a in names(main_labels)) {
    x <- main_predictions[main_predictions$algorithm == a, ]
    p <- x$p_high_raw
    y <- x$y_high
    stopifnot(
      nrow(x) == manifest$n,
      length(unique(p)) > 1L
    )
    bounds <- range(p)
    knots <- unique(quantile(
      p,
      c(
        1 / 3,
        2 / 3
      ),
      names = FALSE
    ))
    knots <- knots[knots > bounds[1] & knots < bounds[2]]
    basis <- splines::ns(
      p,
      knots = knots,
      Boundary.knots = bounds
    )
    fit <- glm(
      y ~ .,
      data = data.frame(
        y = y,
        basis
      ),
      family = binomial(),
      control = glm.control(maxit = 100)
    )
    stopifnot(
      fit$converged,
      all(is.finite(coef(fit)))
    )
    grid <- sort(unique(c(
      seq(
        bounds[1],
        bounds[2],
        length.out = 301
      ),
      p
    )))
    estimated <- predict(
      fit,
      newdata = data.frame(predict(
        basis,
        newx = grid
      )),
      se.fit = TRUE
    )
    stopifnot(
      all(is.finite(estimated$fit)),
      all(is.finite(estimated$se.fit))
    )
    main_curves[[a]] <- data.frame(
      algorithm = a,
      predicted = grid,
      observed = plogis(estimated$fit),
      lower = plogis(estimated$fit - qnorm(0.975) * estimated$se.fit),
      upper = plogis(estimated$fit + qnorm(0.975) * estimated$se.fit)
    )
  }
  main_calibration_curves <- do.call(
    rbind,
    main_curves
  )
  main_calibration_curves$algorithm <- factor(
    main_calibration_curves$algorithm,
    levels = names(main_labels)
  )
  main_predictions$algorithm <- factor(
    main_predictions$algorithm,
    levels = names(main_labels)
  )
  save_table(
    main_calibration_curves,
    file.path(
      table_dir,
      "calibration_main_pooled_curves.csv"
    ),
    run_stamp
  )
  main_plot <- ggplot(
    main_calibration_curves,
    aes(
      predicted,
      observed,
      color = algorithm,
      fill = algorithm
    )
  ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "#8B9298",
      linewidth = 0.55
    ) +
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper
      ),
      color = NA,
      alpha = 0.16
    ) +
    geom_line(
      linewidth = 0.8
    ) +
    geom_rug(
      data = main_predictions,
      aes(x = p_high_raw, color = algorithm),
      inherit.aes = FALSE,
      sides = "b",
      alpha = 0.10,
      linewidth = 0.25
    ) +
    facet_wrap(
      ~algorithm,
      ncol = 4,
      labeller = as_labeller(main_labels)
    ) +
    coord_fixed(
      ratio = 1,
      xlim = c(
        0,
        1
      ),
      ylim = c(
        0,
        1
      ),
      expand = FALSE
    ) +
    scale_x_continuous(
      breaks = seq(
        0,
        1,
        0.25
      ),
      labels = c(
        "0",
        "0.25",
        "0.50",
        "0.75",
        "1"
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        0,
        1,
        0.25
      ),
      labels = c(
        "0",
        "0.25",
        "0.50",
        "0.75",
        "1"
      )
    ) +
    labs(
      x = "Predicted probability of High ABR (>1)",
      y = "Estimated observed proportion of High ABR (>1)"
    ) +
    scale_color_manual(values = report_settings$model_colors, guide = "none") +
    scale_fill_manual(values = report_settings$model_colors, guide = "none") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "#E8ECEF",
        linewidth = 0.3
      ),
      strip.text = element_text(
        face = "bold",
        size = 10
      ),
      panel.spacing = grid::unit(
        1.2,
        "lines"
      ),
      plot.margin = margin(
        14,
        18,
        14,
        14
      )
    )
  save_plot(
    file.path(
      project_dir,
      "output",
      "plots",
      "supplementary",
      paste0(
        "Figure_S3_calibration_models.",
        plot_format
      )
    ),
    main_plot,
    width = 14,
    height = 8,
    dpi = 300,
    run_stamp = run_stamp
  )
}

# 正文 DCA：原始分布、完整变量集，五折净获益等权平均。 ----
if ("original_full" %in% dca_mean$scenario) {
  dca_main_labels <- report_settings$model_labels
  # ggsci::pal_npg("nrc")(8)，固定色值避免修改训练环境依赖。
  dca_main_colors <- report_settings$model_colors
  dca_main <- dca_mean[dca_mean$scenario == "original_full", ]
  dca_main_defaults <- unique(dca_main[c(
    "threshold",
    "treat_all"
  )])
  dca_main_reference <- rbind(
    data.frame(
      threshold = dca_main_defaults$threshold,
      net_benefit = dca_main_defaults$treat_all,
      strategy = "Treat all"
    ),
    data.frame(
      threshold = dca_main_defaults$threshold,
      net_benefit = 0,
      strategy = "Treat none"
    )
  )
  stopifnot(nrow(dca_main_reference) == 200L)
  dca_main$algorithm <- factor(
    dca_main$algorithm,
    levels = c(
      names(dca_main_labels),
      "selection",
      "prevalence_only"
    )
  )
  dca_main_plot <- ggplot(
    dca_main[dca_main$algorithm %in% names(dca_main_labels), ],
    aes(
      threshold,
      net_benefit,
      color = algorithm
    )
  ) +
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper,
        fill = algorithm
      ),
      alpha = 0.18,
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(linewidth = 1.0) +
    geom_line(
      data = dca_main_reference,
      aes(
        threshold,
        net_benefit,
        linetype = strategy
      ),
      inherit.aes = FALSE,
      color = "#666666",
      linewidth = 0.65
    ) +
    scale_fill_manual(
      values = dca_main_colors,
      guide = "none"
    ) +
    scale_color_manual(
      values = dca_main_colors,
      breaks = names(dca_main_labels),
      labels = dca_main_labels
    ) +
    scale_linetype_manual(
      values = c(
        "Treat all" = "dashed",
        "Treat none" = "dotted"
      ),
      breaks = c(
        "Treat all",
        "Treat none"
      )
    ) +
    facet_wrap(
      ~algorithm,
      ncol = 4,
      labeller = as_labeller(dca_main_labels)
    ) +
    scale_x_continuous(
      breaks = seq(
        0,
        1,
        0.25
      ),
      labels = c(
        "0",
        "0.25",
        "0.50",
        "0.75",
        "1"
      )
    ) +
    scale_y_continuous(breaks = seq(
      0,
      0.8,
      0.2
    )) +
    coord_cartesian(
      xlim = c(
        0,
        1
      ),
      ylim = c(
        -0.05,
        0.95
      ),
      expand = FALSE
    ) +
    labs(
      x = "Threshold probability",
      y = "Net benefit",
      color = NULL,
      linetype = NULL
    ) +
    guides(
      color = "none",
      linetype = guide_legend(
        order = 2,
        nrow = 1
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(
        face = "bold",
        size = 11,
        margin = margin(b = 10)
      ),
      panel.spacing = grid::unit(
        1.6,
        "lines"
      ),
      panel.grid.major = element_line(
        color = "#E8ECEF",
        linewidth = 0.3
      ),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = 10),
      legend.key.width = grid::unit(
        1.1,
        "cm"
      ),
      plot.margin = margin(
        12,
        16,
        10,
        12
      )
    )
  save_plot(
    file.path(
      project_dir,
      "output",
      "plots",
      "supplementary",
      paste0(
        "Figure_S4_dca_models.",
        plot_format
      )
    ),
    dca_main_plot,
    width = 14,
    height = 8.5,
    dpi = 300,
    run_stamp = run_stamp
  )
}

# 正文 ROC / PR：逐折曲线等权平均，固定预测的逐点 bootstrap 区间。 ----
if ("original_full" %in% data$scenario) {
  curve_seed <- 20261007L
  curve_bootstrap <- 2000L
  curve_grid <- seq(0, 1, length.out = 1001L)
  curve_labels <- report_settings$model_labels
  curve_colors <- report_settings$model_colors
  curve_data <- data[data$scenario == "original_full", ]
  stopifnot(length(unique(curve_data$repeat_id)) == 1L)
  curve_folds <- sort(unique(curve_data$outer_fold))
  stopifnot(length(curve_folds) == 5L)
  # 保留重复横坐标表示的垂直线段：精确落点取最后一点，
  # 区间内连接左侧最后一点与右侧第一点，不先合并坐标。
  interpolate_curve <- function(x, y) {
    stopifnot(all(diff(x) >= 0), x[1] == 0, tail(x, 1) == 1)
    i <- findInterval(curve_grid, x)
    j <- pmin(i + 1L, length(x))
    result <- y[i]
    between <- x[j] > x[i]
    result[between] <- y[i[between]] +
      (curve_grid[between] - x[i[between]]) /
        (x[j[between]] - x[i[between]]) *
        (y[j[between]] - y[i[between]])
    result
  }
  curve_points <- function(y, group, w) {
    positive <- as.numeric(rowsum(w * y, group, reorder = TRUE))
    negative <- as.numeric(rowsum(w * (1 - y), group, reorder = TRUE))
    keep <- positive + negative > 0
    positive <- positive[keep]
    negative <- negative[keep]
    stopifnot(sum(positive) > 0, sum(negative) > 0)
    recall <- c(0, cumsum(positive) / sum(positive))
    fpr <- c(0, cumsum(negative) / sum(negative))
    precision <- c(1, cumsum(positive) / cumsum(positive + negative))
    list(fpr = fpr, recall = recall, precision = precision)
  }
  curve_means <- curve_boot_means <- setNames(
    vector("list", length(curve_labels)), names(curve_labels)
  )
  curve_auc_rows <- list()
  curve_prevalence <- numeric(length(curve_folds))
  set.seed(curve_seed)
  for (f in curve_folds) {
    fd <- curve_data[curve_data$outer_fold == f, ]
    ids <- sort(unique(fd$patient_id))
    n <- length(ids)
    weights <- rmultinom(curve_bootstrap, n, rep(1 / n, n))
    reference_y <- fd$y_high[match(ids, fd$patient_id)]
    curve_prevalence[f] <- mean(reference_y)
    for (a in names(curve_labels)) {
      x <- fd[fd$algorithm == a, ]
      x <- x[match(ids, x$patient_id), ]
      stopifnot(
        nrow(x) == n, identical(x$patient_id, ids),
        identical(x$y_high, reference_y)
      )
      group <- match(x$p_high_raw, sort(unique(x$p_high_raw), decreasing = TRUE))
      point <- curve_points(x$y_high, group, rep(1, n))
      point_grid <- c(
        interpolate_curve(point$fpr, point$recall),
        interpolate_curve(point$recall, point$precision)
      )
      curve_auc_rows[[length(curve_auc_rows) + 1L]] <- data.frame(
        algorithm = a, outer_fold = f,
        roc_auc = sum(diff(point$fpr) *
          (head(point$recall, -1) + tail(point$recall, -1)) / 2),
        pr_auc = sum(diff(point$recall) *
          (head(point$precision, -1) + tail(point$precision, -1)) / 2)
      )
      boot <- vapply(seq_len(curve_bootstrap), function(b) {
        p <- curve_points(x$y_high, group, weights[, b])
        c(
          interpolate_curve(p$fpr, p$recall),
          interpolate_curve(p$recall, p$precision)
        )
      }, numeric(2L * length(curve_grid)))
      stopifnot(all(is.finite(boot)))
      if (f == curve_folds[1]) {
        curve_means[[a]] <- point_grid / length(curve_folds)
        curve_boot_means[[a]] <- boot / length(curve_folds)
      } else {
        curve_means[[a]] <- curve_means[[a]] + point_grid / length(curve_folds)
        curve_boot_means[[a]] <- curve_boot_means[[a]] + boot / length(curve_folds)
      }
    }
    message("ROC / PR bootstrap completed: fold ", f)
  }
  curve_results <- do.call(rbind, lapply(names(curve_labels), function(a) {
    limits <- apply(curve_boot_means[[a]], 1, quantile, probs = c(0.025, 0.975))
    data.frame(
      algorithm = a, curve = rep(c("ROC", "PR"), each = length(curve_grid)),
      x = rep(curve_grid, 2), estimate = curve_means[[a]],
      lower = limits[1, ], upper = limits[2, ]
    )
  }))
  stopifnot(
    all(is.finite(as.matrix(curve_results[c("estimate", "lower", "upper")]))),
    all(curve_results$lower <= curve_results$upper),
    all(curve_results$lower >= 0), all(curve_results$upper <= 1)
  )
  save_table(curve_results, file.path(table_dir, "roc_pr_original_full_curves.csv"), run_stamp)
  save_table(
    do.call(rbind, curve_auc_rows),
    file.path(table_dir, "roc_pr_original_full_auc_by_fold.csv"), run_stamp
  )
  save_rds(
    list(
      seed = curve_seed, bootstrap = curve_bootstrap,
      grid = curve_grid, scenario = "original_full", event = "High (ABR > 1)",
      aggregation = "equal-weight average of five fold curves on common grids; linear interpolation preserving vertical segments; final point at duplicate abscissae",
      interval = "95% pointwise percentile interval; ordinary patient bootstrap independently within each fold; shared draws across models; fixed predictions; excludes retraining and overlapping training-set dependence; not simultaneous or full nested-CV coverage",
      pr_convention = "ties grouped by score; initial precision 1; trapezoidal PR-AUC as in performance table; no precision-envelope smoothing",
      prevalence_reference = mean(curve_prevalence), input_md5 = tools::md5sum(prediction_file)
    ),
    file.path(table_dir, "roc_pr_metadata.rds"), run_stamp
  )
  curve_panel <- function(kind, supplementary = FALSE) {
    z <- curve_results[curve_results$curve == kind, ]
    p <- ggplot(z, aes(x, estimate, color = algorithm, fill = algorithm)) +
      geom_line(linewidth = 0.75) +
      scale_color_manual(
        values = curve_colors, breaks = names(curve_labels),
        labels = curve_labels, name = NULL
      ) +
      scale_fill_manual(values = curve_colors, guide = "none") +
      scale_x_continuous(breaks = seq(0, 1, 0.25), expand = expansion(mult = 0)) +
      scale_y_continuous(breaks = seq(0, 1, 0.25), expand = expansion(mult = 0)) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#E8ECEF", linewidth = 0.3),
        legend.position = "bottom", plot.margin = margin(12, 14, 12, 12)
      ) +
      guides(color = guide_legend(nrow = 2, byrow = TRUE))
    if (kind == "ROC") {
      p <- p + geom_abline(
        slope = 1, intercept = 0, linetype = "dashed",
        color = "#777777", linewidth = 0.5
      ) +
        labs(x = "False positive rate (1 - specificity)", y = "Sensitivity")
    } else {
      p <- p + geom_hline(
        yintercept = mean(curve_prevalence),
        linetype = "dashed", color = "#777777", linewidth = 0.5
      ) +
        labs(x = "Recall (sensitivity)", y = "Precision (positive predictive value)")
    }
    if (supplementary) {
      p <- p +
        geom_ribbon(
          aes(ymin = lower, ymax = upper),
          alpha = 0.18,
          color = NA,
          show.legend = FALSE
        ) +
        geom_line(linewidth = 0.75) +
        facet_wrap(
          ~ factor(algorithm, levels = names(curve_labels)),
          ncol = 4,
          labeller = labeller(.default = as_labeller(curve_labels))
        ) +
        theme(
          legend.position = "none",
          panel.spacing.x = grid::unit(1, "cm"),
          panel.spacing.y = grid::unit(0.6, "cm")
        )
    }
    p
  }
  roc_pr_plot <- patchwork::wrap_plots(curve_panel("ROC"), curve_panel("PR"),
    ncol = 2, guides = "collect"
  ) +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(legend.position = "bottom", plot.tag = element_text(face = "bold"))
  save_plot(file.path(project_dir, "output", "plots", "main", paste0("Figure_2_roc_pr_models.", plot_format)),
    roc_pr_plot,
    width = 13, height = 6.5, dpi = 300, run_stamp = run_stamp
  )
  for (kind in c("ROC", "PR")) {
    save_plot(
      file.path(
        project_dir,
        "output", "plots", "supplementary",
        paste0(
          if (kind == "ROC") "Figure_S1_" else "Figure_S2_",
          tolower(kind), "_models_ci.", plot_format
        )
      ),
      curve_panel(kind, supplementary = TRUE),
      width = 14,
      height = 8,
      dpi = 300,
      run_stamp = run_stamp
    )
  }
}

# Figure 3：正文仅展示XGBoost；完整模型比较见Figure S3、S4。 ----
if ("original_full" %in% data$scenario) {
  xgb_calibration_plot <- main_plot
  xgb_calibration_plot$data <- main_calibration_curves[
    main_calibration_curves$algorithm == "xgboost",
  ]
  for (i in seq_along(xgb_calibration_plot$layers)) {
    layer_data <- xgb_calibration_plot$layers[[i]]$data
    if (is.data.frame(layer_data) && "algorithm" %in% names(layer_data)) {
      xgb_calibration_plot$layers[[i]]$data <- layer_data[
        layer_data$algorithm == "xgboost",
      ]
    }
  }
  xgb_calibration_plot <- xgb_calibration_plot + facet_null()
  xgb_dca_plot <- dca_main_plot
  xgb_dca_plot$data <- dca_main[dca_main$algorithm == "xgboost", ]
  xgb_dca_plot <- xgb_dca_plot + facet_null()
  figure_3 <- patchwork::wrap_plots(
    xgb_calibration_plot, xgb_dca_plot,
    ncol = 2
  ) + patchwork::plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(face = "bold"))
  save_plot(
    file.path(
      project_dir,
      "output", "plots", "main",
      paste0("Figure_3_xgboost_calibration_dca.", plot_format)
    ),
    figure_3,
    width = 13, height = 6.5, dpi = 300, run_stamp = run_stamp
  )
}

# Table 4：全部候选变量下，SMOTENC相对原始分布的配对比较。 ----
if (all(c("original_full", "smotenc_full") %in% data$scenario)) {
  sampling_seed <- 20261007L
  sampling_bootstrap <- 2000L
  sampling_algorithms <- c(
    "logistic", "logistic_spline", "tree", "rf",
    "svm", "xgboost", "knn", "naive_bayes"
  )
  sampling_data <- data[data$scenario %in% c("original_full", "smotenc_full"), ]
  stopifnot(length(unique(sampling_data$repeat_id)) == 1L)
  sampling_folds <- sort(unique(sampling_data$outer_fold))
  stopifnot(length(sampling_folds) == 5L)
  sampling_point <- sampling_boot <- list()
  set.seed(sampling_seed)
  for (f in sampling_folds) {
    fd <- sampling_data[sampling_data$outer_fold == f, ]
    ids <- sort(unique(fd$patient_id))
    n <- length(ids)
    w <- rmultinom(sampling_bootstrap, n, rep(1 / n, n))
    reference_y <- fd$y_high[match(ids, fd$patient_id)]
    for (a in sampling_algorithms) {
      for (s in c("original_full", "smotenc_full")) {
        x <- fd[fd$algorithm == a & fd$scenario == s, ]
        stopifnot(nrow(x) == n, !anyDuplicated(x$patient_id))
        x <- x[match(ids, x$patient_id), ]
        stopifnot(identical(x$patient_id, ids), identical(x$y_high, reference_y))
        evaluate_sampling <- function(weights) {
          c(performance_metrics(x, weights)[c("roc_auc", "pr_auc")],
            brier = sum(weights * (x$p_high_raw - x$y_high)^2) / sum(weights)
          )
        }
        point <- evaluate_sampling(rep(1L, n))
        boot <- vapply(seq_len(sampling_bootstrap), function(b) {
          evaluate_sampling(w[, b])
        }, numeric(3))
        key <- paste(a, s, sep = ":")
        if (f == sampling_folds[1]) {
          sampling_point[[key]] <- point / length(sampling_folds)
          sampling_boot[[key]] <- boot / length(sampling_folds)
        } else {
          sampling_point[[key]] <- sampling_point[[key]] + point / length(sampling_folds)
          sampling_boot[[key]] <- sampling_boot[[key]] + boot / length(sampling_folds)
        }
      }
    }
    message("Sampling comparison completed: fold ", f)
  }
  sampling_rows <- list()
  for (a in sampling_algorithms) {
    original <- paste(a, "original_full", sep = ":")
    smote <- paste(a, "smotenc_full", sep = ":")
    for (m in c("roc_auc", "pr_auc", "brier")) {
      ci_original <- quantile(sampling_boot[[original]][m, ], c(0.025, 0.975))
      ci_smote <- quantile(sampling_boot[[smote]][m, ], c(0.025, 0.975))
      ci_delta <- quantile(
        sampling_boot[[smote]][m, ] - sampling_boot[[original]][m, ],
        c(0.025, 0.975)
      )
      sampling_rows[[length(sampling_rows) + 1L]] <- data.frame(
        algorithm = a, metric = m,
        original = unname(sampling_point[[original]][m]),
        original_lower = ci_original[1], original_upper = ci_original[2],
        smotenc = unname(sampling_point[[smote]][m]),
        smotenc_lower = ci_smote[1], smotenc_upper = ci_smote[2],
        delta = unname(sampling_point[[smote]][m] - sampling_point[[original]][m]),
        delta_lower = ci_delta[1], delta_upper = ci_delta[2]
      )
    }
  }
  sampling_comparison <- do.call(rbind, sampling_rows)
  stopifnot(
    nrow(sampling_comparison) == 24L,
    all(is.finite(as.matrix(sampling_comparison[, 3:11])))
  )
  save_table(
    sampling_comparison,
    file.path(table_dir, "sampling_full_comparison.csv"), run_stamp
  )
  save_rds(
    list(
      seed = sampling_seed, bootstrap = sampling_bootstrap,
      delta = "SMOTENC minus original distribution; equal-weight five-fold means",
      interval = "paired ordinary patient bootstrap within folds, shared across scenarios and algorithms; fixed predictions; excludes retraining and overlapping training-set dependence; conditional 95% percentile intervals, no multiplicity adjustment",
      scope = "same full candidate pool; feature selection and tuning performed separately in each scenario; comparison of complete pipelines, not identical fixed feature sets",
      input_md5 = tools::md5sum(prediction_file)
    ),
    file.path(table_dir, "sampling_full_comparison_metadata.rds"), run_stamp
  )
}

# Table S4：不采样条件下，预先排除相关变量的配对比较。 ----
{
  predictor_seed <- 20261007L
  predictor_bootstrap <- 2000L
  predictor_algorithms <- c(
    "logistic", "logistic_spline", "tree", "rf",
    "svm", "xgboost", "knn", "naive_bayes"
  )
  restricted_data <- readr::read_csv(restricted_prediction_file, show_col_types = FALSE)
  restricted_manifest <- readRDS(restricted_inputs[2])
  main_config <- readRDS(restricted_inputs[3])
  restricted_config <- readRDS(restricted_inputs[4])
  stopifnot(
    restricted_manifest$status == "complete",
    nrow(restricted_data) == restricted_manifest$prediction_rows,
    identical(manifest$files[grepl("^data_interim/", names(manifest$files))],
      restricted_manifest$files[grepl("^data_interim/", names(restricted_manifest$files))]),
    identical(names(main_config), names(restricted_config))
  )
  for (key in setdiff(names(main_config), c("checkpoint_dir", "scenarios"))) {
    stopifnot(identical(main_config[[key]], restricted_config[[key]]))
  }
  main_registry <- readr::read_csv(restricted_inputs[5], show_col_types = FALSE)
  restricted_registry <- readr::read_csv(restricted_inputs[6], show_col_types = FALSE)
  stopifnot(identical(names(main_registry), names(restricted_registry)),
    all(vapply(names(main_registry), function(v) {
      identical(main_registry[[v]], restricted_registry[[v]])
    }, logical(1))))
  predictor_data <- rbind(
    data[data$scenario == "original_full", ],
    restricted_data[restricted_data$scenario == "original_restricted", ]
  )
  predictor_data <- predictor_data[predictor_data$algorithm %in% predictor_algorithms, ]
  stopifnot(
    !anyNA(predictor_data[c("patient_id", "y_high", "p_high_raw", "outer_fold", "repeat_id")]),
    all(is.finite(predictor_data$p_high_raw)),
    all(predictor_data$p_high_raw >= 0 & predictor_data$p_high_raw <= 1),
    all(predictor_data$y_high %in% 0:1),
    !anyDuplicated(predictor_data[c("scenario", "algorithm", "repeat_id", "patient_id")]),
    all(table(predictor_data$scenario, predictor_data$algorithm) == manifest$n)
  )
  stopifnot(length(unique(predictor_data$repeat_id)) == 1L)
  predictor_folds <- sort(unique(predictor_data$outer_fold))
  stopifnot(length(predictor_folds) == 5L)
  predictor_point <- predictor_boot <- list()
  set.seed(predictor_seed)
  for (f in predictor_folds) {
    fd <- predictor_data[predictor_data$outer_fold == f, ]
    ids <- sort(unique(fd$patient_id))
    n <- length(ids)
    w <- rmultinom(predictor_bootstrap, n, rep(1 / n, n))
    reference_y <- fd$y_high[match(ids, fd$patient_id)]
    for (a in predictor_algorithms) {
      for (s in c("original_full", "original_restricted")) {
        x <- fd[fd$algorithm == a & fd$scenario == s, ]
        stopifnot(nrow(x) == n, !anyDuplicated(x$patient_id))
        x <- x[match(ids, x$patient_id), ]
        stopifnot(identical(x$patient_id, ids), identical(x$y_high, reference_y))
        evaluate_predictor <- function(weights) {
          g <- match(x$p_high_raw, sort(unique(x$p_high_raw), decreasing = TRUE))
          pos <- as.numeric(rowsum(weights * x$y_high, g, reorder = TRUE))
          neg <- as.numeric(rowsum(weights * (1 - x$y_high), g, reorder = TRUE))
          keep <- pos + neg > 0
          pos <- pos[keep]
          neg <- neg[keep]
          stopifnot(sum(pos) > 0, sum(neg) > 0)
          recall <- c(0, cumsum(pos) / sum(pos))
          fpr <- c(0, cumsum(neg) / sum(neg))
          precision <- c(1, cumsum(pos) / cumsum(pos + neg))
          c(
            roc_auc = sum(diff(fpr) * (head(recall, -1) + tail(recall, -1)) / 2),
            pr_auc = sum(diff(recall) * (head(precision, -1) + tail(precision, -1)) / 2),
            brier = sum(weights * (x$p_high_raw - x$y_high)^2) / sum(weights)
          )
        }
        point <- evaluate_predictor(rep(1L, n))
        boot <- vapply(seq_len(predictor_bootstrap), function(b) {
          evaluate_predictor(w[, b])
        }, numeric(3))
        key <- paste(a, s, sep = ":")
        if (f == predictor_folds[1]) {
          predictor_point[[key]] <- point / length(predictor_folds)
          predictor_boot[[key]] <- boot / length(predictor_folds)
        } else {
          predictor_point[[key]] <- predictor_point[[key]] + point / length(predictor_folds)
          predictor_boot[[key]] <- predictor_boot[[key]] + boot / length(predictor_folds)
        }
      }
    }
    message("Predictor comparison completed: fold ", f)
  }
  predictor_rows <- list()
  for (a in predictor_algorithms) {
    original <- paste(a, "original_full", sep = ":")
    restricted <- paste(a, "original_restricted", sep = ":")
    for (m in c("roc_auc", "pr_auc", "brier")) {
      ci_original <- quantile(predictor_boot[[original]][m, ], c(0.025, 0.975))
      ci_restricted <- quantile(predictor_boot[[restricted]][m, ], c(0.025, 0.975))
      ci_delta <- quantile(
        predictor_boot[[restricted]][m, ] - predictor_boot[[original]][m, ],
        c(0.025, 0.975)
      )
      predictor_rows[[length(predictor_rows) + 1L]] <- data.frame(
        algorithm = a, metric = m,
        original = unname(predictor_point[[original]][m]),
        original_lower = ci_original[1], original_upper = ci_original[2],
        restricted = unname(predictor_point[[restricted]][m]),
        restricted_lower = ci_restricted[1], restricted_upper = ci_restricted[2],
        delta = unname(predictor_point[[restricted]][m] - predictor_point[[original]][m]),
        delta_lower = ci_delta[1], delta_upper = ci_delta[2]
      )
    }
  }
  predictor_comparison <- do.call(rbind, predictor_rows)
  stopifnot(
    nrow(predictor_comparison) == 24L,
    all(is.finite(as.matrix(predictor_comparison[, 3:11])))
  )
  save_table(
    predictor_comparison,
    file.path(table_dir, "predictor_comparison.csv"), run_stamp
  )
  save_rds(
    list(
      seed = predictor_seed, bootstrap = predictor_bootstrap,
      delta = "Restricted minus full candidate predictors; equal-weight five-fold means",
      interval = "paired ordinary patient bootstrap within folds, shared across scenarios and algorithms; fixed predictions; excludes retraining and overlapping training-set dependence; conditional 95% percentile intervals, no multiplicity adjustment",
      scope = "No oversampling in either scenario; predefined exclusions from the full candidate pool; feature selection and tuning performed separately; comparison of complete pipelines",
      input_md5 = tools::md5sum(c(prediction_file, restricted_prediction_file)),
      run_ids = c(full = run_id, restricted = report_settings$restricted_run_id),
      candidate_counts = c(full = sum(main_registry$include_full),
        restricted = sum(main_registry$include_restricted)),
      excluded_variables = main_registry$variable[
        main_registry$include_full & !main_registry$include_restricted]
    ),
    file.path(table_dir, "predictor_comparison_metadata.rds"), run_stamp
  )
}

finish_report(report)
