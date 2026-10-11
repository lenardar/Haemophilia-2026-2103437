# 公共文件输出函数；不覆盖 R 或包中的同名函数。

ensure_parent_dir <- function(path) {
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE)
  }
  stopifnot(dir.exists(parent))
  invisible(path)
}

backup_path <- function(
  path,
  run_stamp = format(Sys.time(), "%Y%m%d_%H%M%OS6")
) {
  destination <- file.path(dirname(path), "backup", run_stamp, basename(path))
  ensure_parent_dir(destination)
  stopifnot(!file.exists(destination))
  destination
}

backup_file <- function(
  path,
  run_stamp = format(Sys.time(), "%Y%m%d_%H%M%OS6")
) {
  stopifnot(file.exists(path), !dir.exists(path))
  destination <- backup_path(path, run_stamp)
  stopifnot(file.copy(path, destination, overwrite = FALSE))
  invisible(destination)
}

save_table <- function(
  x,
  file,
  run_stamp = format(Sys.time(), "%Y%m%d_%H%M%OS6")
) {
  ensure_parent_dir(file)
  destination <- backup_path(file, run_stamp)
  readr::write_csv(x, file)
  stopifnot(file.copy(file, destination, overwrite = FALSE))
  invisible(file)
}

save_plot <- function(
  filename,
  plot = ggplot2::last_plot(),
  width = 10,
  height = 7,
  dpi = 300,
  run_stamp = format(Sys.time(), "%Y%m%d_%H%M%OS6"),
  ...
) {
  ensure_parent_dir(filename)
  destination <- backup_path(filename, run_stamp)
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    ...
  )
  stopifnot(file.copy(filename, destination, overwrite = FALSE))
  invisible(filename)
}

save_rds <- function(
  object,
  file,
  run_stamp = format(Sys.time(), "%Y%m%d_%H%M%OS6")
) {
  ensure_parent_dir(file)
  destination <- backup_path(file, run_stamp)
  base::saveRDS(object, file)
  stopifnot(file.copy(file, destination, overwrite = FALSE))
  invisible(file)
}

save_rdata <- function(
  ...,
  file,
  envir = parent.frame(),
  run_stamp = format(Sys.time(), "%Y%m%d_%H%M%OS6")
) {
  ensure_parent_dir(file)
  destination <- backup_path(file, run_stamp)
  base::save(..., file = file, envir = envir)
  stopifnot(file.copy(file, destination, overwrite = FALSE))
  invisible(file)
}

# 保存分析输入指纹、源码快照及产物清单，不改变模型或统计方法。
start_report <- function(project_dir, run_id, stage, inputs, run_stamp) {
  stopifnot(
    length(run_id) == 1L,
    grepl("^[A-Za-z0-9_-]+$", run_id),
    stage %in% c("evaluation", "shap", "tables"),
    length(inputs) > 0L, all(file.exists(inputs)),
    !anyDuplicated(inputs)
  )
  folder <- file.path(
    project_dir, "output", "logs", "reporting",
    run_id, stage, run_stamp
  )
  stopifnot(!dir.exists(folder))
  dir.create(folder, recursive = TRUE)
  stopifnot(dir.exists(folder))
  sources <- c(
    list.files(file.path(project_dir, "rscript"),
      pattern = "\\.R$", full.names = TRUE
    ),
    file.path(project_dir, c("DESCRIPTION", "renv.lock"))
  )
  dir.create(file.path(folder, "source"))
  stopifnot(all(file.copy(sources, file.path(folder, "source"))))
  result <- list(
    status = "started", run_id = run_id, stage = stage,
    run_stamp = run_stamp, started_at = Sys.time(),
    project_dir = project_dir, folder = folder,
    input_md5 = tools::md5sum(inputs), source_md5 = tools::md5sum(sources),
    session = sessionInfo()
  )
  base::saveRDS(result, file.path(folder, "manifest_started.rds"))
  result
}

finish_report <- function(report) {
  stopifnot(identical(
    tools::md5sum(names(report$input_md5)), report$input_md5
  ))
  paths <- list.files(file.path(report$project_dir, "output"),
    recursive = TRUE, full.names = TRUE
  )
  marker <- paste0("/backup/", report$run_stamp, "/")
  snapshots <- paths[grepl(marker, paths, fixed = TRUE)]
  stopifnot(length(snapshots) > 0L)
  current <- vapply(
    strsplit(snapshots, marker, fixed = TRUE),
    function(parts) paste(parts, collapse = "/"), character(1)
  )
  stopifnot(all(file.exists(current)))
  fingerprints <- unname(tools::md5sum(snapshots))
  stopifnot(identical(fingerprints, unname(tools::md5sum(current))))
  inventory <- data.frame(
    current_file = substring(current, nchar(report$project_dir) + 2L),
    snapshot_file = substring(snapshots, nchar(report$project_dir) + 2L),
    md5 = fingerprints, bytes = file.info(current)$size
  )
  readr::write_csv(inventory, file.path(report$folder, "outputs.csv"))
  report$status <- "complete"
  report$finished_at <- Sys.time()
  report$outputs <- inventory
  report$session <- sessionInfo()
  base::saveRDS(report, file.path(report$folder, "manifest_finished.rds"))
  message("报告完成：", report$stage, "；批次：", report$run_id,
          "；产物清单：", file.path(report$folder, "outputs.csv"))
  invisible(report)
}
