# 外层测试标签不得传入训练函数；所有统计状态随模型保存。
check_training_environment <- function(project_dir) {
  lock_path <- file.path(
    project_dir,
    "renv.lock"
  )
  stopifnot(file.exists(lock_path))
  lock <- renv::lockfile_read(lock_path)
  if (getRversion() != package_version(lock$R$Version)) {
    stop(
      "R 版本不一致：当前 ",
      getRversion(),
      "，锁定 ",
      lock$R$Version,
      "。请先安装锁定的 R 版本。"
    )
  }
  expected_library <- renv::paths$library(project = project_dir)
  if (!identical(
    normalizePath(.libPaths()[1]),
    normalizePath(
      expected_library,
      mustWork = TRUE
    )
  )) {
    stop("训练未使用项目 renv 包库。请打开项目后执行 renv::restore()。")
  }
  for (package in names(lock$Packages)) {
    if (!requireNamespace(
      package,
      quietly = TRUE
    )) {
      stop(
        "锁定依赖无法加载：",
        package,
        "。请执行 renv::restore()。"
      )
    }
    expected <- package_version(lock$Packages[[package]]$Version)
    installed <- utils::packageVersion(
      package,
      lib.loc = expected_library
    )
    loaded <- getNamespaceVersion(package)
    if (installed != expected || loaded != expected) {
      stop(
        "依赖版本不一致：",
        package,
        "；请重启 R 并执行 renv::restore()。"
      )
    }
  }
  invisible(TRUE)
}

training_dependencies <- function() {
  c(
    "mlr3",
    "mlr3learners",
    "mlr3pipelines",
    "glmnet",
    "Boruta",
    "ranger",
    "xgboost",
    "kknn",
    "e1071",
    "themis",
    "digest",
    "readr",
    "parallelly",
    "future",
    "future.apply",
    "renv",
    "rpart"
  )
}

check_training_dependencies <- function() {
  for (package in training_dependencies()) {
    if (!requireNamespace(
      package,
      quietly = TRUE
    )) {
      stop(
        "Missing package: ",
        package
      )
    }
  }
  invisible(TRUE)
}

seed_for <- function(
  seed,
  context
) {
  h <- digest::digest(
    paste(
      seed,
      context
    ),
    algo = "xxhash32"
  )
  as.integer(strtoi(
    substr(
      h,
      1,
      7
    ),
    base = 16L
  ))
}

check_probabilities <- function(
  p,
  n
) {
  stopifnot(
    length(p) == n,
    is.numeric(p),
    all(is.finite(p))
  )
  stopifnot(all(p >= 0 & p <= 1))
  invisible(TRUE)
}

stratified_folds <- function(
  y,
  k,
  seed
) {
  stopifnot(
    k >= 2,
    !anyNA(y),
    identical(
      sort(unique(y)),
      c(
        0L,
        1L
      )
    )
  )
  stopifnot(all(table(y) >= k))
  set.seed(seed)
  fold <- integer(length(y))
  for (class in 0:1) {
    i <- which(y == class)
    fold[i] <- sample(rep(
      seq_len(k),
      length.out = length(i)
    ))
  }
  stopifnot(all(table(
    fold,
    y
  ) > 0))
  fold
}

auc_high <- function(
  y,
  p
) {
  check_probabilities(
    p,
    length(y)
  )
  stopifnot(
    all(y %in% 0:1),
    all(table(factor(
      y,
      levels = 0:1
    )) > 0)
  )
  n1 <- sum(y == 1L)
  n0 <- sum(y == 0L)
  (sum(rank(p)[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

load_analysis_data <- function(
  project_dir,
  config
) {
  env <- new.env(parent = emptyenv())
  loaded <- load(
    file.path(
      project_dir,
      "data_interim",
      "recode_data.RData"
    ),
    env
  )
  stopifnot(identical(
    loaded,
    "recode_data"
  ))
  data <- as.data.frame(env$recode_data)
  stopifnot(
    !anyNA(data),
    !anyDuplicated(data$id),
    all(is.finite(data$v50))
  )
  stopifnot(
    all(data$v50 >= 0),
    identical(
      data$v9,
      data$v15
    )
  )
  excluded <- unlist(
    config$excluded,
    use.names = FALSE
  )
  stopifnot(
    !anyDuplicated(excluded),
    all(excluded %in% names(data))
  )
  stopifnot(all(config$restricted_excluded %in% names(data)))
  book <- readr::read_csv(
    file.path(
      project_dir,
      "data_interim",
      "code_book.csv"
    ),
    show_col_types = FALSE
  )
  stopifnot(
    !anyDuplicated(book$variable),
    setequal(
      book$variable,
      names(data)
    )
  )
  book <- as.data.frame(book[match(
    names(data),
    book$variable
  ), ])
  book$exclusion_reason <- ""
  for (reason in names(config$excluded)) {
    book$exclusion_reason[book$variable %in% config$excluded[[reason]]] <- reason
  }
  book$include_full <- !book$variable %in% excluded
  book$include_restricted <- book$include_full &
    !book$variable %in% config$restricted_excluded
  book$restricted_reason <- ifelse(
    book$variable %in% config$restricted_excluded,
    "bleeding_consequence_or_summary_or_response",
    ""
  )
  features <- book$variable[book$include_full]
  stopifnot(all(vapply(
    data[features],
    function(x) is.factor(x) || is.numeric(x),
    logical(1)
  )))
  data$patient_id <- as.character(data$id)
  data$y_high <- as.integer(data$v50 > 1)
  list(
    data = data,
    registry = book
  )
}

new_training_audit <- function() {
  audit <- new.env(parent = emptyenv())
  audit$splits <- list()
  audit$searches <- list()
  audit$selections <- list()
  audit
}

record_split <- function(
  audit,
  data,
  fold,
  context
) {
  audit$splits[[context]] <- data.frame(
    context = context,
    patient_id = data$patient_id,
    fold = fold,
    y_high = data$y_high
  )
  invisible(TRUE)
}

# 使用码本已规定的水平；不根据验证患者重新学习水平或参考组。
fit_design <- function(
  x,
  spline_df = 0L
) {
  stopifnot(
    ncol(x) > 0,
    !anyNA(x)
  )
  parts <- list()
  state <- list()
  mapping <- list()
  for (v in names(x)) {
    z <- x[[v]]
    if (is.factor(z)) {
      stopifnot(length(levels(z)) >= 2L)
      levels_v <- levels(z)
      m <- vapply(
        levels_v[-1],
        function(l) as.numeric(z == l),
        numeric(nrow(x))
      )
      m <- matrix(
        m,
        nrow = nrow(x)
      )
      labels <- paste0(
        v,
        "__",
        seq_len(ncol(m))
      )
      state[[v]] <- list(
        kind = "factor",
        levels = levels_v,
        observed = unique(as.character(z)),
        columns = labels
      )
      detail <- levels_v[-1]
    } else {
      stopifnot(
        is.numeric(z),
        all(is.finite(z))
      )
      knots <- numeric()
      if (spline_df > 1L && length(unique(z)) > spline_df + 1L) {
        knots <- unique(as.numeric(stats::quantile(
          z,
          probs = seq_len(spline_df - 1L) / spline_df,
          names = FALSE
        )))
        knots <- knots[knots > min(z) & knots < max(z)]
      }
      if (length(knots) > 0L) {
        m <- splines::ns(
          z,
          knots = knots,
          Boundary.knots = range(z)
        )
        state[[v]] <- list(
          kind = "spline",
          basis = m
        )
      } else {
        m <- matrix(
          z,
          ncol = 1L
        )
        state[[v]] <- list(kind = "numeric")
      }
      labels <- paste0(
        v,
        "__",
        seq_len(ncol(m))
      )
      state[[v]]$columns <- labels
      detail <- if (state[[v]]$kind == "spline") {
        paste0(
          "spline_",
          seq_len(ncol(m))
        )
      } else {
        "numeric"
      }
    }
    colnames(m) <- labels
    parts[[v]] <- m
    mapping[[v]] <- data.frame(
      column = labels,
      variable = v,
      component = detail
    )
  }
  matrix <- do.call(
    cbind,
    parts
  )
  keep <- vapply(
    seq_len(ncol(matrix)),
    function(j) length(unique(matrix[, j])) > 1L,
    logical(1)
  )
  stopifnot(any(keep))
  removed_columns <- colnames(matrix)[!keep]
  matrix <- matrix[, keep, drop = FALSE]
  center <- colMeans(matrix)
  scale <- apply(
    matrix,
    2,
    stats::sd
  )
  stopifnot(
    all(is.finite(scale)),
    all(scale > 0)
  )
  list(
    states = state,
    columns = colnames(matrix),
    center = center,
    scale = scale,
    mapping = do.call(
      rbind,
      mapping
    ),
    removed_constant_columns = removed_columns,
    matrix = sweep(
      sweep(
        matrix,
        2,
        center,
        "-"
      ),
      2,
      scale,
      "/"
    )
  )
}

apply_design <- function(
  design,
  x
) {
  if (identical(
    design$representation,
    "mixed_naive_bayes"
  )) {
    stopifnot(
      all(design$columns %in% names(x)),
      !anyNA(x)
    )
    result <- x[design$columns]
    for (v in design$columns) {
      state <- design$states[[v]]
      if (state$kind == "factor") {
        stopifnot(all(as.character(result[[v]]) %in% state$levels))
        result[[v]] <- factor(
          result[[v]],
          levels = state$levels
        )
      } else {
        stopifnot(
          is.numeric(result[[v]]),
          all(is.finite(result[[v]]))
        )
      }
    }
    return(result)
  }
  stopifnot(
    all(names(design$states) %in% names(x)),
    !anyNA(x)
  )
  parts <- lapply(
    names(design$states),
    function(v) {
      s <- design$states[[v]]
      z <- x[[v]]
      if (s$kind == "factor") {
        stopifnot(all(as.character(z) %in% s$levels))
        m <- vapply(
          s$levels[-1],
          function(l) as.numeric(z == l),
          numeric(nrow(x))
        )
        m <- matrix(
          m,
          nrow = nrow(x)
        )
      } else if (s$kind == "spline") {
        m <- stats::predict(
          s$basis,
          newx = z
        )
      } else {
        m <- matrix(
          z,
          ncol = 1L
        )
      }
      colnames(m) <- s$columns
      m
    }
  )
  m <- do.call(
    cbind,
    parts
  )[, design$columns, drop = FALSE]
  m <- sweep(
    sweep(
      m,
      2,
      design$center,
      "-"
    ),
    2,
    design$scale,
    "/"
  )
  stopifnot(all(is.finite(m)))
  m
}

select_predictors <- function(
  data,
  features,
  selector,
  config,
  seed,
  audit,
  context
) {
  x <- data[features]
  varying <- vapply(
    x,
    function(z) length(unique(z)) > 1L,
    logical(1)
  )
  features <- features[varying]
  stopifnot(length(features) > 0L)
  x <- x[features]
  set.seed(seed)
  if (selector == "lasso") {
    design <- fit_design(x)
    fold <- stratified_folds(
      data$y_high,
      config$lasso_folds,
      seed
    )
    record_split(
      audit,
      data,
      fold,
      paste0(
        context,
        "/lambda"
      )
    )
    fitted <- glmnet::cv.glmnet(
      x = design$matrix,
      y = data$y_high,
      family = "binomial",
      alpha = 1,
      foldid = fold,
      type.measure = "deviance",
      standardize = TRUE,
      maxit = config$lasso_maxit
    )
    stopifnot(fitted$glmnet.fit$jerr == 0L)
    coefficient <- as.matrix(stats::coef(
      fitted,
      s = config$lasso_rule
    ))
    chosen <- rownames(coefficient)[coefficient[, 1] != 0]
    chosen <- setdiff(
      chosen,
      "(Intercept)"
    )
    selected <- unique(design$mapping$variable[design$mapping$column %in% chosen])
    detail <- list(
      lambda = fitted[[config$lasso_rule]],
      coefficients = coefficient,
      mapping = design$mapping,
      cv = data.frame(
        lambda = fitted$lambda,
        deviance = fitted$cvm,
        se = fitted$cvsd
      )
    )
  } else if (selector == "boruta") {
    stopifnot(identical(
      config$boruta_importance,
      "ranger_scaled_permutation"
    ))
    importance <- function(
      x,
      y,
      ...
    ) {
      value <- Boruta::getImpRfZ(
        x,
        y,
        num.trees = config$boruta_trees,
        num.threads = 1L
      )
      if (any(!is.finite(value))) {
        stop(
          "Nonfinite Boruta importance at ",
          context,
          ": ",
          paste(
            names(x)[!is.finite(value)],
            collapse = ", "
          )
        )
      }
      value
    }
    fitted <- Boruta::Boruta(
      x = x,
      y = factor(
        data$y_high,
        levels = 0:1
      ),
      maxRuns = config$boruta_max_runs,
      getImp = importance,
      pValue = 0.01,
      mcAdj = TRUE,
      doTrace = 0,
      holdHistory = TRUE
    )
    stopifnot(config$boruta_tentative == "exclude")
    selected <- names(fitted$finalDecision)[fitted$finalDecision == "Confirmed"]
    detail <- list(
      decisions = fitted$finalDecision,
      importance_history = fitted$ImpHistory,
      importance_source = config$boruta_importance,
      trees = config$boruta_trees,
      boruta_version = as.character(utils::packageVersion("Boruta")),
      ranger_version = as.character(utils::packageVersion("ranger"))
    )
  } else {
    stop(
      "Unknown selector: ",
      selector
    )
  }
  # 零变量是明确的截距模型结果，不能私自添加变量。
  result <- list(
    selector = selector,
    selected = selected,
    candidate_features = features,
    removed_constants = names(varying)[!varying],
    training_ids = data$patient_id,
    detail = detail
  )
  audit$selections[[context]] <- result
  result
}

make_task <- function(
  x,
  y
) {
  backend <- as.data.frame(x)
  backend$target <- factor(
    ifelse(
      y == 1L,
      "High",
      "Low"
    ),
    levels = c(
      "Low",
      "High"
    )
  )
  mlr3::TaskClassif$new(
    id = "hemophilia",
    backend = backend,
    target = "target",
    positive = "High"
  )
}

sample_training <- function(
  x,
  y,
  sampling,
  config,
  seed
) {
  if (sampling == "original") {
    return(list(
      x = x,
      y = y
    ))
  }
  stopifnot(
    sampling == "smotenc",
    all(table(y) > config$smote_k)
  )
  if (!any(vapply(
    x,
    is.numeric,
    logical(1)
  ))) {
    stop("SMOTENC requires a numeric predictor; selected set is categorical only.")
  }
  if (!any(vapply(
    x,
    is.factor,
    logical(1)
  ))) {
    stop("SMOTENC requires a categorical predictor; selected set is numeric only.")
  }
  stopifnot(all(vapply(
    x,
    function(z) length(unique(z)) > 1L,
    logical(1)
  )))
  # 距离只按本次真实训练患者标准化，保留 factor 供 SMOTENC 使用。
  numeric <- names(x)[vapply(
    x,
    is.numeric,
    logical(1)
  )]
  centers <- vapply(
    x[numeric],
    mean,
    numeric(1)
  )
  scales <- vapply(
    x[numeric],
    stats::sd,
    numeric(1)
  )
  for (v in numeric) x[[v]] <- as.numeric((x[[v]] - centers[[v]]) / scales[[v]])
  task <- make_task(
    x,
    y
  )
  op <- mlr3pipelines::po(
    "smotenc",
    k = config$smote_k,
    over_ratio = config$smote_ratio
  )
  set.seed(seed)
  result <- as.data.frame(op$train(list(task))[[1]]$data())
  sampled_y <- as.integer(result$target == "High")
  result <- result[names(x)]
  for (v in numeric) result[[v]] <- result[[v]] * scales[[v]] + centers[[v]]
  for (v in setdiff(
    names(x),
    numeric
  )) {
    stopifnot(all(as.character(result[[v]]) %in% levels(x[[v]])))
    result[[v]] <- factor(
      result[[v]],
      levels = levels(x[[v]])
    )
  }
  stopifnot(
    nrow(result) >= nrow(x),
    !anyNA(result)
  )
  list(
    x = result,
    y = sampled_y
  )
}

make_learner <- function(
  algorithm,
  params,
  seed
) {
  ids <- c(
    logistic = "classif.log_reg",
    logistic_spline = "classif.log_reg",
    tree = "classif.rpart",
    rf = "classif.ranger",
    svm = "classif.svm",
    xgboost = "classif.xgboost",
    knn = "classif.kknn",
    naive_bayes = "classif.naive_bayes"
  )
  stopifnot(algorithm %in% names(ids))
  learner <- mlr3::lrn(
    ids[[algorithm]],
    predict_type = "prob"
  )
  fixed <- switch(algorithm,
    rf = list(
      num.threads = 1L,
      seed = seed
    ),
    svm = list(
      type = "C-classification",
      kernel = "radial",
      scale = FALSE
    ),
    xgboost = list(
      nthread = 1L,
      seed = seed,
      verbosity = 0L
    ),
    knn = list(scale = FALSE),
    tree = list(xval = 0L),
    logistic = list(maxit = 100L),
    logistic_spline = list(maxit = 100L),
    naive_bayes = list()
  )
  learner$param_set$values <- utils::modifyList(
    learner$param_set$values,
    c(
      fixed,
      params
    )
  )
  learner
}

prepare_base_input <- function(
  data,
  selection,
  algorithm,
  sampling,
  config,
  seed
) {
  selected <- selection$selected
  if (length(selected) == 0L) {
    return(list(type = "intercept_only"))
  }
  x <- data[selected]
  design <- if (algorithm == "naive_bayes") {
    list(
      representation = "mixed_naive_bayes",
      columns = names(x),
      states = lapply(
        x,
        function(z) {
          if (is.factor(z)) {
            list(
              kind = "factor",
              levels = levels(z),
              observed = unique(as.character(z))
            )
          } else {
            stopifnot(
              is.numeric(z),
              all(is.finite(z))
            )
            list(kind = "numeric")
          }
        }
      )
    )
  } else {
    fit_design(
      x,
      if (algorithm == "logistic_spline") config$spline_df else 0L
    )
  }
  # 筛选仅用真实患者；采样近邻使用当前训练折的合格候选空间。
  sampled <- sample_training(
    data[selection$candidate_features],
    data$y_high,
    sampling,
    config,
    seed
  )
  matrix <- apply_design(
    design,
    sampled$x[selected]
  )
  list(
    type = "learner",
    design = design,
    matrix = matrix,
    sampled_y = sampled$y
  )
}

fit_base <- function(
  data,
  selection,
  algorithm,
  params,
  sampling,
  config,
  seed,
  prepared = NULL
) {
  stopifnot(config$smote_geometry == "eligible_training_candidates")
  stopifnot(config$known_unobserved_levels == "encode_and_flag")
  stopifnot(config$zero_feature_policy == "original_prevalence_intercept")
  selected <- selection$selected
  if (length(selected) == 0L) {
    return(list(
      algorithm = algorithm,
      selection = selection,
      type = "intercept_only",
      prevalence = mean(data$y_high),
      training_ids = data$patient_id,
      params = params,
      sampling = "not_applied_intercept_only"
    ))
  }
  if (is.null(prepared)) {
    prepared <- prepare_base_input(
      data,
      selection,
      algorithm,
      sampling,
      config,
      seed
    )
  }
  stopifnot(prepared$type == "learner")
  design <- prepared$design
  matrix <- prepared$matrix
  # 固定训练内可识别的列；保留哪些冗余列被移除，避免 glm 的隐式 NA 系数。
  if (algorithm %in% c(
    "logistic",
    "logistic_spline"
  )) {
    q <- qr(cbind(
      intercept = 1,
      design$matrix
    ))
    selected_columns <- q$pivot[seq_len(q$rank)]
    selected_columns <- selected_columns[selected_columns != 1L] - 1L
    design$columns <- design$columns[selected_columns]
    design$center <- design$center[design$columns]
    design$scale <- design$scale[design$columns]
    matrix <- matrix[, design$columns, drop = FALSE]
  }
  if (algorithm == "naive_bayes") {
    stopifnot(
      is.null(params$laplace),
      length(config$naive_bayes_laplace) == 1L,
      is.finite(config$naive_bayes_laplace),
      config$naive_bayes_laplace > 0
    )
    params$laplace <- config$naive_bayes_laplace
    numeric_columns <- names(matrix)[vapply(
      matrix,
      is.numeric,
      logical(1)
    )]
    for (v in numeric_columns) {
      deviations <- tapply(
        matrix[[v]],
        prepared$sampled_y,
        stats::sd
      )
      if (any(!is.finite(deviations) | deviations <= 0)) {
        stop(
          "Naive Bayes numeric feature has invalid within-class SD: ",
          v
        )
      }
    }
  }
  learner <- make_learner(
    algorithm,
    params,
    seed
  )
  set.seed(seed)
  learner$train(make_task(
    matrix,
    prepared$sampled_y
  ))
  if (length(learner$errors) > 0L) {
    stop(paste(
      learner$errors,
      collapse = "\n"
    ))
  }
  if (length(learner$warnings) > 0L) {
    warning(paste(
      learner$warnings,
      collapse = "\n"
    ))
  }
  if (algorithm %in% c(
    "logistic",
    "logistic_spline"
  )) {
    stopifnot(
      isTRUE(learner$model$converged),
      all(is.finite(stats::coef(learner$model)))
    )
  }
  design$matrix <- NULL
  list(
    type = "learner",
    algorithm = algorithm,
    learner = learner,
    design = design,
    selection = selection,
    params = params,
    sampling = sampling,
    training_ids = data$patient_id,
    sampling_features = selection$candidate_features,
    counts_original = table(data$y_high),
    counts_sampled = table(prepared$sampled_y),
    warnings = learner$warnings
  )
}

predict_base <- function(
  model,
  newdata
) {
  if (model$type == "intercept_only") {
    return(rep(
      model$prevalence,
      nrow(newdata)
    ))
  }
  matrix <- apply_design(
    model$design,
    newdata[model$selection$selected]
  )
  probability <- model$learner$predict_newdata(as.data.frame(matrix))$prob
  stopifnot("High" %in% colnames(probability))
  p <- as.numeric(probability[, "High"])
  check_probabilities(
    p,
    nrow(newdata)
  )
  p
}

unobserved_level_flags <- function(
  model,
  newdata
) {
  flags <- rep(
    "",
    nrow(newdata)
  )
  if (model$type == "intercept_only") {
    return(flags)
  }
  for (v in names(model$design$states)) {
    state <- model$design$states[[v]]
    if (state$kind == "factor") {
      values <- as.character(newdata[[v]])
      stopifnot(all(values %in% state$levels))
      unseen <- !values %in% state$observed
      flags[unseen] <- paste0(
        flags[unseen],
        v,
        "=",
        values[unseen],
        ";"
      )
    }
  }
  flags
}

# 参数组合只由配置和种子生成，不读取患者数据或验证分数。
algorithm_parameters <- function(
  config,
  algorithm,
  context
) {
  space <- config$search_spaces[[algorithm]]
  method <- config$search_method[[algorithm]]
  set.seed(seed_for(
    config$seed,
    paste(
      context,
      algorithm,
      "parameters"
    )
  ))
  if (method == "fixed") {
    stopifnot(length(space) == 0L)
    return(list(list()))
  }
  if (method == "grid") {
    stopifnot(all(vapply(
      space,
      function(x) x$type == "choice",
      logical(1)
    )))
    grid <- expand.grid(
      lapply(
        space,
        function(x) x$values
      ),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    n <- min(
      nrow(grid),
      config$candidate_limit
    )
    return(lapply(
      seq_len(n),
      function(i) as.list(grid[i, , drop = FALSE])
    ))
  }
  stopifnot(method == "random")
  n <- min(
    config$search_budget[[algorithm]],
    config$candidate_limit
  )
  columns <- lapply(
    space,
    function(spec) {
      switch(spec$type,
        choice = sample(
          spec$values,
          n,
          replace = TRUE
        ),
        integer = sample(
          seq.int(
            spec$lower,
            spec$upper
          ),
          n,
          replace = TRUE
        ),
        uniform = stats::runif(
          n,
          spec$lower,
          spec$upper
        ),
        log_uniform = exp(stats::runif(
          n,
          log(spec$lower),
          log(spec$upper)
        )),
        stop(
          "Unknown parameter distribution: ",
          spec$type
        )
      )
    }
  )
  lapply(
    seq_len(n),
    function(i) {
      lapply(
        columns,
        function(x) x[[i]]
      )
    }
  )
}

search_candidates <- function(
  config,
  context
) {
  candidates <- list()
  for (algorithm in config$algorithms) {
    parameters <- algorithm_parameters(
      config,
      algorithm,
      context
    )
    for (selector in config$selectors) {
      for (j in seq_along(parameters)) {
        id <- paste(
          algorithm,
          selector,
          j,
          sep = "_"
        )
        candidates[[id]] <- list(
          algorithm = algorithm,
          selector = selector,
          params = parameters[[j]]
        )
      }
    }
  }
  candidates
}

search_training <- function(
  data,
  features,
  sampling,
  config,
  audit,
  context
) {
  message(
    "Training search: ",
    context
  )
  seed <- seed_for(
    config$seed,
    context
  )
  fold <- stratified_folds(
    data$y_high,
    config$inner_folds,
    seed
  )
  record_split(
    audit,
    data,
    fold,
    paste0(
      context,
      "/inner"
    )
  )
  candidates <- search_candidates(
    config,
    context
  )
  candidate_oof <- matrix(
    NA_real_,
    nrow(data),
    length(candidates),
    dimnames = list(
      data$patient_id,
      names(candidates)
    )
  )
  scores <- list()
  for (f in seq_len(config$inner_folds)) {
    train <- data[fold != f, , drop = FALSE]
    valid <- data[fold == f, , drop = FALSE]
    stopifnot(!any(train$patient_id %in% valid$patient_id))
    selections <- list()
    for (selector in config$selectors) {
      key <- paste(
        context,
        "inner",
        f,
        selector,
        sep = "/"
      )
      message("Selecting predictors: ", key)
      selections[[selector]] <- select_predictors(
        train,
        features,
        selector,
        config,
        seed_for(
          config$seed,
          key
        ),
        audit,
        key
      )
    }
    prepared <- list()
    for (selector in config$selectors) {
      representations <- c(
        "linear",
        if ("logistic_spline" %in% config$algorithms) "spline",
        if ("naive_bayes" %in% config$algorithms) "mixed"
      )
      for (basis in representations) {
        key <- paste(
          selector,
          basis,
          sep = "/"
        )
        prepared[[key]] <- prepare_base_input(
          train,
          selections[[selector]],
          switch(basis,
            spline = "logistic_spline",
            mixed = "naive_bayes",
            linear = "logistic"
          ),
          sampling,
          config,
          seed_for(
            config$seed,
            paste(
              context,
              f,
              selector,
              "sampling"
            )
          )
        )
      }
    }
    evaluate_candidate <- function(id) {
      candidate <- candidates[[id]]
      key <- paste(
        context,
        "inner",
        f,
        id,
        sep = "/"
      )
      withCallingHandlers(
        {
          basis <- switch(candidate$algorithm,
            logistic_spline = "spline",
            naive_bayes = "mixed",
            "linear"
          )
          fit <- fit_base(
            train,
            selections[[candidate$selector]],
            candidate$algorithm,
            candidate$params,
            sampling,
            config,
            seed_for(
              config$seed,
              key
            ),
            prepared = prepared[[paste(
              candidate$selector,
              basis,
              sep = "/"
            )]]
          )
          p <- predict_base(
            fit,
            valid
          )
          list(
            probability = p,
            score = data.frame(
              candidate = id,
              algorithm = candidate$algorithm,
              selector = candidate$selector,
              fold = f,
              auc = auc_high(
                valid$y_high,
                p
              ),
              n_features = length(fit$selection$selected)
            )
          )
        },
        error = function(e) {
          message("FAILED candidate: ", key, " | ", conditionMessage(e))
        }
      )
    }
    ids <- names(candidates)
    batches <- split(
      ids,
      ceiling(seq_along(ids) / config$search_batch_size)
    )
    for (batch_id in seq_along(batches)) {
      batch <- batches[[batch_id]]
      checkpoint <- if (is.null(config$checkpoint_dir)) {
        NULL
      } else {
        file.path(
          config$checkpoint_dir,
          context,
          paste0("inner_", f),
          paste0("batch_", batch_id, ".rds")
        )
      }
      identity <- list(
        candidates = candidates[batch],
        training_ids = train$patient_id,
        validation_ids = valid$patient_id
      )
      if (!is.null(checkpoint) && file.exists(checkpoint)) {
        cached <- readRDS(checkpoint)
        stopifnot(identical(cached$identity, identity))
        batch_scores <- cached$scores
        message("Restored batch: ", context, "/", f, "/", batch_id)
      } else {
        batch_scores <- if (config$workers == 1L) {
          lapply(
            batch,
            evaluate_candidate
          )
        } else {
          future.apply::future_lapply(
            batch,
            evaluate_candidate,
            future.seed = TRUE,
            future.packages = c(
              "mlr3",
              "mlr3learners",
              "splines"
            )
          )
        }
        if (!is.null(checkpoint)) {
          training_checkpoint_save(
            list(identity = identity, scores = batch_scores),
            checkpoint
          )
        }
      }
      names(batch_scores) <- batch
      scores <- c(
        scores,
        lapply(
          batch_scores,
          function(x) x$score
        )
      )
      for (id in batch) {
        candidate_oof[fold == f, id] <- batch_scores[[id]]$probability
      }
      message(
        context,
        " | inner fold ",
        f,
        "/",
        config$inner_folds,
        " | candidates ",
        min(
          batch_id * config$search_batch_size,
          length(ids)
        ),
        "/",
        length(ids)
      )
    }
  }
  score <- do.call(
    rbind,
    scores
  )
  summary <- do.call(
    rbind,
    lapply(
      names(candidates),
      function(id) {
        s <- score[score$candidate == id, ]
        data.frame(
          candidate = id,
          algorithm = s$algorithm[1],
          selector = s$selector[1],
          mean_auc = mean(s$auc),
          mean_features = mean(s$n_features)
        )
      }
    )
  )
  # 最后一个排序键是配置顺序，完全并列时也能复现。
  priority <- order(
    -summary$mean_auc,
    summary$mean_features,
    seq_len(nrow(summary))
  )
  ranked <- summary[priority, , drop = FALSE]
  winners <- ranked[!duplicated(ranked$algorithm), , drop = FALSE]
  full_selections <- list()
  for (selector in unique(winners$selector)) {
    key <- paste(
      context,
      "refit",
      selector,
      sep = "/"
    )
    message("Selecting predictors: ", key)
    full_selections[[selector]] <- select_predictors(
      data,
      features,
      selector,
      config,
      seed_for(
        config$seed,
        key
      ),
      audit,
      key
    )
  }
  models <- list()
  for (algorithm in config$algorithms) {
    row <- winners[winners$algorithm == algorithm, ]
    candidate <- candidates[[row$candidate]]
    message("Refitting model: ", context, "/", algorithm)
    models[[algorithm]] <- fit_base(
      data,
      full_selections[[candidate$selector]],
      algorithm,
      candidate$params,
      sampling,
      config,
      seed_for(
        config$seed,
        paste(
          context,
          algorithm
        )
      )
    )
  }
  chosen_algorithm <- ranked$algorithm[1]
  models$selection <- models[[chosen_algorithm]]
  stopifnot(!anyNA(candidate_oof))
  tuning_predictions <- do.call(
    rbind,
    lapply(
      names(models),
      function(algorithm) {
        selected_algorithm <- if (algorithm == "selection") chosen_algorithm else algorithm
        candidate_id <- winners$candidate[winners$algorithm == selected_algorithm]
        stopifnot(length(candidate_id) == 1L)
        data.frame(
          patient_id = data$patient_id,
          algorithm = algorithm,
          candidate = candidate_id,
          fold = fold,
          y_high = data$y_high,
          p_high_raw = candidate_oof[, candidate_id]
        )
      }
    )
  )
  result <- list(
    models = models,
    tuning_predictions = tuning_predictions,
    chosen_algorithm = chosen_algorithm,
    training_ids = data$patient_id
  )
  audit$searches[[context]] <- list(
    search_spaces = config$search_spaces,
    search_method = config$search_method,
    search_budget = config$search_budget,
    scores = score,
    summary = summary,
    candidates = candidates,
    winners = winners,
    chosen_algorithm = chosen_algorithm,
    training_ids = data$patient_id
  )
  result
}

predict_search <- function(
  bundle,
  newdata
) {
  result <- lapply(
    names(bundle$models),
    function(algorithm) {
      data.frame(
        patient_id = newdata$patient_id,
        algorithm = algorithm,
        p_high_raw = predict_base(
          bundle$models[[algorithm]],
          newdata
        ),
        known_unobserved_levels = unobserved_level_flags(
          bundle$models[[algorithm]],
          newdata
        )
      )
    }
  )
  do.call(
    rbind,
    result
  )
}

choose_youden <- function(
  y,
  p
) {
  check_probabilities(
    p,
    length(y)
  )
  stopifnot(all(table(factor(
    y,
    levels = 0:1
  )) > 0L))
  # 中点避免相等概率分割；J 完全并列时采用最高阈值。
  unique_p <- sort(unique(p))
  midpoints <- if (length(unique_p) > 1L) {
    (unique_p[-1] + unique_p[-length(unique_p)]) / 2
  } else {
    numeric()
  }
  thresholds <- sort(unique(c(
    0,
    midpoints,
    1
  )))
  j <- vapply(
    thresholds,
    function(t) {
      predicted <- p >= t
      mean(predicted[y == 1]) + mean(!predicted[y == 0]) - 1
    },
    numeric(1)
  )
  best <- max(thresholds[j == max(j)])
  list(
    threshold = best,
    curve = data.frame(
      threshold = thresholds,
      youden = j
    )
  )
}

fit_training_bundle_impl <- function(
  data,
  features,
  sampling,
  config,
  audit,
  context
) {
  # 内层验证预测用于开发阈值；只有外层测试用于评价完整选择流程。
  bundle <- search_training(
    data,
    features,
    sampling,
    config,
    audit,
    context
  )
  oof <- bundle$tuning_predictions
  thresholds <- list()
  curves <- list()
  for (algorithm in c(
    config$algorithms,
    "selection"
  )) {
    rows <- oof[oof$algorithm == algorithm, ]
    stopifnot(
      nrow(rows) == nrow(data),
      !anyDuplicated(rows$patient_id)
    )
    for (version in "raw") {
      decision <- choose_youden(
        rows$y_high,
        rows[[paste0(
          "p_high_",
          version
        )]]
      )
      key <- paste(
        algorithm,
        version,
        sep = "/"
      )
      thresholds[[key]] <- data.frame(
        algorithm = algorithm,
        probability_version = version,
        threshold = decision$threshold
      )
      curves[[key]] <- decision$curve
    }
  }
  bundle$thresholds <- do.call(
    rbind,
    thresholds
  )
  bundle$threshold_curves <- curves
  bundle$threshold_predictions <- oof
  bundle$context <- context
  bundle$features <- features
  bundle$sampling <- sampling
  bundle
}

predict_training_bundle <- function(
  bundle,
  newdata
) {
  predictions <- predict_search(
    bundle,
    newdata
  )
  for (version in "raw") {
    thresholds <- bundle$thresholds[bundle$thresholds$probability_version == version, ]
    t <- thresholds$threshold[match(
      predictions$algorithm,
      thresholds$algorithm
    )]
    stopifnot(!anyNA(t))
    predictions[[paste0(
      "threshold_",
      version
    )]] <- t
    predictions[[paste0(
      "class_",
      version
    )]] <- as.integer(
      predictions[[paste0(
        "p_high_",
        version
      )]] >= t
    )
    predictions[[paste0(
      "class_",
      version,
      "_05"
    )]] <- as.integer(
      predictions[[paste0(
        "p_high_",
        version
      )]] >= 0.5
    )
  }
  predictions$selected_algorithm <- ifelse(
    predictions$algorithm == "selection",
    bundle$chosen_algorithm,
    predictions$algorithm
  )
  predictions
}

run_training_impl <- function(
  project_dir,
  config,
  run_id
) {
  check_training_dependencies()
  stopifnot(config$workers >= 1L)
  if (config$workers > 1L) {
    old_plan <- future::plan(
      future::multisession,
      workers = config$workers
    )
    on.exit(
      future::plan(old_plan),
      add = TRUE
    )
  }

  stopifnot(grepl(
    "^[A-Za-z0-9_]+$",
    run_id
  ))
  stopifnot(all(config$outer_fold_ids %in% seq_len(config$outer_folds)))
  log_dir <- file.path(
    project_dir,
    "output",
    "logs",
    "training",
    run_id
  )
  if (dir.exists(log_dir)) {
    stop(
      "Run already exists; use a new run_id: ",
      run_id
    )
  }
  dir.create(
    log_dir,
    recursive = TRUE
  )
  stopifnot(dir.exists(log_dir))
  model_dir <- file.path(
    project_dir,
    "output",
    "lrns",
    run_id
  )
  table_dir <- file.path(
    project_dir,
    "output",
    "tables",
    "training",
    run_id
  )
  instance_dir <- file.path(
    project_dir,
    "output",
    "instances",
    run_id
  )
  stopifnot(dir.exists(config$checkpoint_dir))
  loaded <- load_analysis_data(
    project_dir,
    config
  )
  data <- loaded$data
  registry <- loaded$registry
  training_preflight(loaded, config)
  save_table(
    registry,
    file.path(
      table_dir,
      "variable_registry.csv"
    ),
    run_id
  )
  save_rds(
    config,
    file.path(
      log_dir,
      "config.rds"
    ),
    run_id
  )
  stopifnot(file.copy(
    file.path(
      project_dir,
      "renv.lock"
    ),
    file.path(
      log_dir,
      "renv.lock"
    ),
    overwrite = FALSE
  ))
  backup_file(
    file.path(
      log_dir,
      "renv.lock"
    ),
    run_id
  )
  files <- c(
    "rscript/03_modeling.R",
    "rscript/model_functions.R",
    "rscript/analysis_config.R",
    "rscript/tool_functions.R",
    "renv.lock",
    "DESCRIPTION",
    "data_interim/recode_data.RData",
    "data_interim/code_book.csv"
  )
  hashes <- tools::md5sum(file.path(
    project_dir,
    files
  ))
  names(hashes) <- files
  manifest <- list(
    run_id = run_id,
    profile = config$profile,
    status = "running",
    started = Sys.time(),
    files = hashes,
    config_hash = digest::digest(
      config,
      algo = "sha256"
    ),
    session = utils::sessionInfo(),
    n = nrow(data),
    counts = table(data$y_high)
  )
  save_rds(
    manifest,
    file.path(
      log_dir,
      "manifest_started.rds"
    ),
    run_id
  )
  writeLines(
    capture.output(utils::sessionInfo()),
    file.path(
      log_dir,
      "session_info.txt"
    )
  )
  backup_file(
    file.path(
      log_dir,
      "session_info.txt"
    ),
    run_id
  )
  all_predictions <- list()
  for (repeat_id in seq_len(config$outer_repeats)) {
    outer_fold <- stratified_folds(
      data$y_high,
      config$outer_folds,
      seed_for(
        config$seed,
        paste0(
          "outer_",
          repeat_id
        )
      )
    )
    save_table(
      data.frame(
        patient_id = data$patient_id,
        repeat_id = repeat_id,
        outer_fold = outer_fold,
        y_high = data$y_high
      ),
      file.path(
        table_dir,
        paste0(
          "outer_folds_",
          repeat_id,
          ".csv"
        )
      ),
      run_id
    )
    for (s in seq_len(nrow(config$scenarios))) {
      scenario <- config$scenarios[s, ]
      include <- registry[[paste0(
        "include_",
        scenario$predictor_set
      )]]
      features <- registry$variable[include]
      input <- data[c(
        "patient_id",
        "y_high",
        features
      )]
      for (f in config$outer_fold_ids) {
        key <- paste(
          scenario$scenario,
          paste0(
            "repeat",
            repeat_id
          ),
          paste0(
            "fold",
            f
          ),
          sep = "_"
        )
        train <- input[outer_fold != f, , drop = FALSE]
        test <- input[outer_fold == f, , drop = FALSE]
        stopifnot(!any(train$patient_id %in% test$patient_id))
        audit <- new_training_audit()
        bundle <- fit_training_bundle(
          train,
          features,
          scenario$sampling,
          config,
          audit,
          key
        )
        # 预测函数只接收特征及 ID，外层结局在预测后拼接。
        predicted <- predict_training_bundle(
          bundle,
          test[setdiff(
            names(test),
            "y_high"
          )]
        )
        predicted$y_high <- test$y_high[match(
          predicted$patient_id,
          test$patient_id
        )]
        predicted$scenario <- scenario$scenario
        predicted$repeat_id <- repeat_id
        predicted$outer_fold <- f
        predicted$model_id <- paste(
          run_id,
          key,
          predicted$algorithm,
          sep = "/"
        )
        predicted$training_prevalence <- mean(train$y_high)
        save_rds(
          bundle,
          file.path(
            model_dir,
            paste0(
              key,
              ".rds"
            )
          ),
          run_id
        )
        save_rds(
          as.list(audit),
          file.path(
            instance_dir,
            paste0(
              key,
              "_audit.rds"
            )
          ),
          run_id
        )
        save_table(
          predicted,
          file.path(
            table_dir,
            paste0(
              key,
              "_predictions.csv"
            )
          ),
          run_id
        )
        all_predictions[[key]] <- predicted
      }
    }
  }
  predictions <- do.call(
    rbind,
    all_predictions
  )
  key_columns <- c(
    "scenario",
    "repeat_id",
    "algorithm",
    "patient_id"
  )
  stopifnot(!anyDuplicated(predictions[key_columns]))
  if (setequal(
    config$outer_fold_ids,
    seq_len(config$outer_folds)
  )) {
    counts <- table(
      predictions$scenario,
      predictions$repeat_id,
      predictions$algorithm
    )
    stopifnot(all(counts == nrow(data)))
  }
  save_table(
    predictions,
    file.path(
      table_dir,
      "outer_predictions.csv"
    ),
    run_id
  )
  if (config$fit_final) {
    for (s in seq_len(nrow(config$scenarios))) {
      scenario <- config$scenarios[s, ]
      features <- registry$variable[registry[[paste0(
        "include_",
        scenario$predictor_set
      )]]]
      audit <- new_training_audit()
      key <- paste0(
        scenario$scenario,
        "_final"
      )
      bundle <- fit_training_bundle(
        data[c(
          "patient_id",
          "y_high",
          features
        )],
        features,
        scenario$sampling,
        config,
        audit,
        key
      )
      save_rds(
        bundle,
        file.path(
          model_dir,
          paste0(
            key,
            ".rds"
          )
        ),
        run_id
      )
      save_rds(
        as.list(audit),
        file.path(
          instance_dir,
          paste0(
            key,
            "_audit.rds"
          )
        ),
        run_id
      )
    }
  }
  manifest$status <- if (config$profile == "smoke") {
    "smoke_complete"
  } else if (setequal(
    config$outer_fold_ids,
    seq_len(config$outer_folds)
  ) && config$fit_final) {
    "complete"
  } else {
    "partial_complete"
  }
  manifest$finished <- Sys.time()
  manifest$prediction_rows <- nrow(predictions)
  save_rds(
    manifest,
    file.path(
      log_dir,
      "manifest_finished.rds"
    ),
    run_id
  )
  message(
    "Training complete: ",
    run_id,
    " (",
    manifest$status,
    ")"
  )
  invisible(predictions)
}

# 临时文件与目标位于同一目录；未完成的写入不得成为可恢复检查点。
training_checkpoint_save <- function(
  object,
  path
) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )
  stopifnot(!file.exists(path))
  temporary <- tempfile(
    "pending_",
    tmpdir = dirname(path)
  )
  on.exit(
    unlink(temporary),
    add = TRUE
  )
  saveRDS(
    object,
    temporary
  )
  stopifnot(file.rename(
    temporary,
    path
  ))
  invisible(path)
}

fit_training_bundle <- function(
  data,
  features,
  sampling,
  config,
  audit,
  context
) {
  path <- if (is.null(config$checkpoint_dir)) {
    NULL
  } else {
    file.path(
      config$checkpoint_dir,
      context,
      "bundle.rds"
    )
  }
  identity <- digest::digest(list(
    data,
    features,
    sampling,
    config,
    context
  ))
  if (!is.null(path) && file.exists(path)) {
    cached <- readRDS(path)
    stopifnot(identical(
      cached$identity,
      identity
    ))
    list2env(
      cached$audit,
      envir = audit
    )
    message(
      "Restored completed training: ",
      context
    )
    return(cached$bundle)
  }
  bundle <- fit_training_bundle_impl(
    data,
    features,
    sampling,
    config,
    audit,
    context
  )
  if (!is.null(path)) {
    training_checkpoint_save(
      list(
        identity = identity,
        bundle = bundle,
        audit = as.list(audit)
      ),
      path
    )
  }
  bundle
}

training_preflight <- function(
  loaded,
  config
) {
  path <- file.path(
    config$checkpoint_dir,
    "preflight_passed.rds"
  )
  if (file.exists(path)) {
    stopifnot(isTRUE(readRDS(path)$passed))
    message("Preflight: restored matching successful check.")
    return(invisible(TRUE))
  }
  message("Preflight: real data, all configured models/selectors/scenarios.")
  diagnostic <- config
  diagnostic$inner_folds <- 2L
  diagnostic$candidate_limit <- 1L
  diagnostic$checkpoint_dir <- NULL
  fold <- stratified_folds(
    loaded$data$y_high,
    config$outer_folds,
    config$seed
  )
  for (s in seq_len(nrow(config$scenarios))) {
    scenario <- config$scenarios[s, ]
    features <- loaded$registry$variable[loaded$registry[[paste0(
      "include_",
      scenario$predictor_set
    )]]]
    train <- loaded$data[fold != 1L, c(
      "patient_id",
      "y_high",
      features
    )]
    valid <- loaded$data[fold == 1L, c(
      "patient_id",
      features
    )]
    bundle <- fit_training_bundle_impl(
      train,
      features,
      scenario$sampling,
      diagnostic,
      new_training_audit(),
      paste0(
        "preflight_",
        scenario$scenario
      )
    )
    expected <- predict_training_bundle(
      bundle,
      valid
    )
    temporary <- tempfile(
      tmpdir = config$checkpoint_dir,
      fileext = ".rds"
    )
    saveRDS(
      bundle,
      temporary
    )
    # 在独立 worker 中重新读取，覆盖模型跨进程加载与预测。
    actual <- future.apply::future_lapply(
      seq_len(config$workers),
      function(i) {
        predict_training_bundle(
          readRDS(temporary),
          valid
        )
      },
      future.seed = TRUE,
      future.packages = c(
        "mlr3",
        "mlr3learners",
        "splines"
      )
    )
    stopifnot(all(vapply(
      actual,
      function(x) {
        isTRUE(all.equal(
          x,
          expected
        ))
      },
      logical(1)
    )))
    unlink(temporary)
    message(
      "Preflight passed: ",
      scenario$scenario
    )
  }
  training_checkpoint_save(
    list(
      passed = TRUE,
      time = Sys.time()
    ),
    path
  )
  message("Preflight complete. Starting the full configured search.")
  invisible(TRUE)
}

run_training <- function(
  project_dir,
  config,
  run_id
) {
  check_training_environment(project_dir)
  check_training_dependencies()
  stopifnot(grepl(
    "^[A-Za-z0-9_]+$",
    run_id
  ))
  # 只有代码、数据、配置及软件环境完全相同才共用缓存。
  files <- c(
    "rscript/model_functions.R",
    "rscript/analysis_config.R",
    "rscript/tool_functions.R",
    "renv.lock",
    "DESCRIPTION",
    "data_interim/recode_data.RData",
    "data_interim/code_book.csv"
  )
  installed <- utils::installed.packages()[, c(
    "Package",
    "Version"
  )]
  signature <- digest::digest(
    list(
      config,
      unname(tools::md5sum(file.path(
        project_dir,
        files
      ))),
      R.version,
      installed
    ),
    algo = "sha256"
  )
  config$checkpoint_dir <- file.path(
    project_dir,
    "output",
    "instances",
    "training_cache",
    signature
  )
  dir.create(
    config$checkpoint_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  lock <- file.path(
    config$checkpoint_dir,
    "active.lock"
  )
  if (!dir.create(
    lock,
    showWarnings = FALSE
  )) {
    stop(
      "已有任务或上次异常退出留下运行锁。确认没有训练进程后删除：", lock
    )
  }
  on.exit(
    unlink(
      lock,
      recursive = TRUE
    ),
    add = TRUE
  )
  writeLines(
    paste(
      Sys.info()[["nodename"]],
      Sys.getpid()
    ),
    file.path(
      lock,
      "owner.txt"
    )
  )
  log_root <- file.path(
    project_dir,
    "output",
    "logs",
    "training"
  )
  dir.create(
    log_root,
    recursive = TRUE,
    showWarnings = FALSE
  )
  log_path <- file.path(
    log_root,
    paste0(
      run_id,
      "_progress.log"
    )
  )
  status_path <- file.path(
    log_root,
    paste0(
      run_id,
      "_status.txt"
    )
  )
  stopifnot(
    !file.exists(log_path),
    !file.exists(status_path)
  )
  log_event <- function(text) {
    cat(
      format(Sys.time()),
      " | ",
      text,
      "\n",
      file = log_path,
      append = TRUE
    )
  }
  writeLines(
    c(
      "RUNNING",
      paste(
        "cache:",
        config$checkpoint_dir
      )
    ),
    status_path
  )
  log_event(paste(
    "START",
    run_id,
    "workers",
    config$workers
  ))
  completed <- FALSE
  on.exit(
    {
      if (!completed) {
        writeLines(
          c(
            "FAILED_OR_INTERRUPTED",
            paste(
              "log:",
              log_path
            )
          ),
          status_path
        )
      }
    },
    add = TRUE
  )
  result <- withCallingHandlers(
    run_training_impl(
      project_dir,
      config,
      run_id
    ),
    message = function(m) log_event(conditionMessage(m)),
    warning = function(w) {
      log_event(paste(
        "WARNING",
        conditionMessage(w)
      ))
    },
    error = function(e) {
      log_event(paste(
        "ERROR",
        conditionMessage(e)
      ))
      log_event(paste(
        vapply(
          sys.calls(),
          deparse1,
          character(1)
        ),
        collapse = "\n"
      ))
    }
  )
  completed <- TRUE
  writeLines(
    c(
      "COMPLETED",
      paste(
        "run:",
        run_id
      )
    ),
    status_path
  )
  invisible(result)
}
