# Initialize or restore the project-specific R environment.

options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

if (!file.exists("renv/activate.R")) {
  renv::init(
    bare = TRUE,
    restart = FALSE
  )
}

# Dependencies are declared explicitly in DESCRIPTION.
renv::settings$snapshot.type("explicit")

if (file.exists("renv.lock")) {
  message("Restoring package versions from renv.lock ...")
  renv::restore(prompt = FALSE)
} else {
  message("Installing packages declared in DESCRIPTION ...")
  renv::install(prompt = FALSE)
  renv::snapshot(type = "explicit", prompt = FALSE)
}

message(
  "\nEnvironment is ready. Run:\n",
  "  targets::tar_make()\n"
)
