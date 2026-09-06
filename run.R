# Build the full workflow from the command line with: Rscript run.R

if (!requireNamespace("targets", quietly = TRUE)) {
  stop(
    "The targets package is unavailable. Run `Rscript setup.R` first.",
    call. = FALSE
  )
}

targets::tar_make()
