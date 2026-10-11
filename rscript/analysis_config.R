# 搜索空间及方法配置与结果一起保存；smoke 仅检验程序。
analysis_config <- function(
  profile = "full",
  selected_scenarios = NULL
) {
  stopifnot(profile %in% c(
    "full",
    "smoke"
  ))
  search_spaces <- list(
    logistic = list(),
    logistic_spline = list(),
    tree = list(
      minsplit = list(
        type = "choice",
        values = c(
          2L,
          4L,
          8L,
          16L,
          32L,
          64L,
          128L
        )
      ),
      minbucket = list(
        type = "choice",
        values = c(
          1L,
          2L,
          4L,
          8L,
          16L,
          32L,
          64L
        )
      ),
      cp = list(
        type = "choice",
        values = c(
          1e-4,
          1e-3,
          1e-2,
          1e-1
        )
      )
    ),
    rf = list(
      num.trees = list(
        type = "choice",
        values = seq(
          50L,
          2000L,
          by = 10L
        )
      ),
      mtry.ratio = list(
        type = "uniform",
        lower = 0.1,
        upper = 0.5
      ),
      replace = list(
        type = "choice",
        values = c(
          FALSE,
          TRUE
        )
      ),
      sample.fraction = list(
        type = "uniform",
        lower = 0.6,
        upper = 1
      )
    ),
    svm = list(
      cost = list(
        type = "log_uniform",
        lower = 1e-4,
        upper = 1e4
      ),
      gamma = list(
        type = "log_uniform",
        lower = 1e-4,
        upper = 1e4
      )
    ),
    xgboost = list(
      nrounds = list(
        type = "integer",
        lower = 50L,
        upper = 1000L
      ),
      eta = list(
        type = "log_uniform",
        lower = 0.01,
        upper = 0.2
      ),
      max_depth = list(
        type = "integer",
        lower = 3L,
        upper = 10L
      ),
      colsample_bytree = list(
        type = "uniform",
        lower = 0.5,
        upper = 1
      ),
      subsample = list(
        type = "uniform",
        lower = 0.5,
        upper = 1
      ),
      lambda = list(
        type = "log_uniform",
        lower = 1e-3,
        upper = 100
      ),
      min_child_weight = list(
        type = "integer",
        lower = 1L,
        upper = 20L
      )
    ),
    knn = list(
      k = list(
        type = "choice",
        values = seq(
          3L,
          21L,
          by = 2L
        )
      ),
      distance = list(
        type = "choice",
        values = c(
          1,
          2
        )
      ),
      kernel = list(
        type = "choice",
        values = c(
          "rectangular",
          "inv",
          "optimal"
        )
      )
    ),
    naive_bayes = list()
  )
  config <- list(
    schema_version = 4L,
    boruta_importance = "ranger_scaled_permutation",
    boruta_trees = 500L,
    naive_bayes_laplace = 1,
    profile = profile,
    seed = 123L,
    outer_folds = 5L,
    outer_repeats = 1L,
    inner_folds = 5L,
    lasso_folds = 5L,
    lasso_rule = "lambda.1se",
    lasso_maxit = 1000000L,
    boruta_max_runs = 100L,
    boruta_tentative = "exclude",
    selectors = c(
      "lasso",
      "boruta"
    ),
    algorithms = names(search_spaces),
    search_spaces = search_spaces,
    search_method = c(
      logistic = "fixed",
      logistic_spline = "fixed",
      tree = "grid",
      rf = "random",
      svm = "random",
      xgboost = "random",
      knn = "grid",
      naive_bayes = "fixed"
    ),
    search_budget = c(
      rf = 100L,
      svm = 150L,
      xgboost = 150L
    ),
    candidate_limit = Inf,
    workers = as.integer(min(
      4L,
      parallelly::availableCores()
    )),
    search_batch_size = 32L,
    checkpoint_dir = NULL,
    smote_k = 5L,
    smote_ratio = 1,
    smote_geometry = "eligible_training_candidates",
    known_unobserved_levels = "encode_and_flag",
    zero_feature_policy = "original_prevalence_intercept",
    spline_df = 3L,
    outer_fold_ids = 1:5,
    fit_final = TRUE,
    scenarios = data.frame(
      scenario = c(
        "smotenc_full",
        "original_full",
        "smotenc_restricted",
        "original_restricted"
      ),
      sampling = c(
        "smotenc",
        "original",
        "smotenc",
        "original"
      ),
      predictor_set = c(
        "full",
        "full",
        "restricted",
        "restricted"
      ),
      stringsAsFactors = FALSE
    ),
    excluded = list(
      identifier = "id",
      fixed_population = c(
        "v1",
        "v2",
        "v3"
      ),
      duplicate = "v15",
      geography_detail = c(
        "v14_city",
        "v14_province"
      ),
      multi_response_text = c(
        "v16",
        "v30",
        "v31",
        "v35",
        "v40",
        "v44",
        "v45",
        "v52"
      ),
      outcome_or_proxy = c(
        "v48",
        "v49",
        "v50",
        "v51"
      ),
      bleeding_utilization = c(
        "v21",
        "v22",
        "v23",
        "v24"
      ),
      incompatible_dose_units = "v41"
    ),
    restricted_excluded = c(
      "v16_osteoarthritis",
      "v35_arthritis",
      "v36",
      "v37",
      "v38",
      "v44_symptom_change",
      "v53",
      "v52_finger",
      "v52_left_ankle",
      "v52_left_elbow",
      "v52_left_knee",
      "v52_left_shoulder",
      "v52_other",
      "v52_right_ankle",
      "v52_right_elbow",
      "v52_right_knee",
      "v52_right_shoulder",
      "v52_toe"
    )
  )
  if (profile == "smoke") {
    config$inner_folds <- 2L
    config$lasso_folds <- 3L
    config$selectors <- "lasso"
    config$algorithms <- "tree"
    config$candidate_limit <- 1L
    config$workers <- 1L
    config$outer_fold_ids <- 1L
    config$fit_final <- FALSE
  }
  if (!is.null(selected_scenarios)) {
    stopifnot(
      is.character(selected_scenarios),
      length(selected_scenarios) > 0L,
      !anyNA(selected_scenarios),
      !anyDuplicated(selected_scenarios),
      all(selected_scenarios %in% config$scenarios$scenario)
    )
    config$scenarios <- config$scenarios[
      match(
        selected_scenarios,
        config$scenarios$scenario
      ),
      ,
      drop = FALSE
    ]
  }
  config
}
