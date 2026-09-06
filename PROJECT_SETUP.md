# Repository maintenance

This document is for maintainers. Readers who only want to reproduce the
analysis should follow `README.md`.

## Changing the analysis

1. Define reusable functions and small parameter objects in `R/`.
2. Declare every simulation, transformation, figure, table, and report in
   `_targets.R`.
3. Avoid top-level analysis calls and file-writing side effects in `R/`.
4. Run `targets::tar_manifest()` to validate the pipeline definition.
5. Run `targets::tar_make()` and confirm that `targets::tar_outdated()` returns
   no target names.

Functions that write files must create their parent directories, write one
declared artifact, and return its path. File-producing targets must use
`format = "file"`.

## Changing dependencies

Add direct R dependencies to `DESCRIPTION`, install them, and refresh the
lockfile:

```r
renv::install("package-name")
renv::snapshot(type = "explicit")
renv::status()
```

Commit `DESCRIPTION`, `renv.lock`, `.Rprofile`, `renv/activate.R`, and
`renv/settings.json`. Do not commit `renv/library/`.

## Preparing a release

1. Rebuild on a clean checkout or confirm the GitHub Actions workflow passes.
2. Check every output against the corresponding manuscript figure or table.
3. Update `CITATION.cff` with the manuscript DOI, repository DOI, release date,
   version, author ORCIDs, and repository URL.
4. Update the version in `DESCRIPTION` and `CITATION.cff`.
5. Create a tagged GitHub release and archive it in a long-term repository such
   as Zenodo.
