#Run tests/pipeline_check.R on every sample export in data/ (plus synthetic Q1000 small molecule and 2 plate exports).
#Sample data is in data/ (gitignored), see CLAUDE.md.
#usage: Rscript tests/run_all.R          (from the repo root or anywhere)
#  env TEST_OUT output directory (default: <tempdir>/biocrates_tests), per-run logs in <TEST_OUT>/<tag>.log
script_dir <- function() { f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); dirname(normalizePath(f)) }
tests_dir <- script_dir(); app_dir <- dirname(tests_dir)
test_out <- Sys.getenv("TEST_OUT", file.path(tempdir(), "biocrates_tests"))
dir.create(test_out, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TEST_OUT = test_out, APPDIR = app_dir)
d <- file.path(app_dir, "data"); synth <- file.path(test_out, "synth")

cat("Creating synthetic Q1000 exports...\n")
system2("Rscript", c(shQuote(file.path(tests_dir, "make_synthetic_q1000.R")), shQuote(synth)), stdout = FALSE)

#tag, config, export, material to process, fixed LOD
runs <- list(
  list("q500_single",       "Q500/Q500_Config_020325.xlsx",              "Q500/Q500-single plate.txt",         "brain_tissue", TRUE),
  list("q500_multi",        "Q500/Q500_Config_020325.xlsx",              "Q500/Q500-multiple plate.txt",       "brain_tissue", TRUE),
  list("q500_multi_random", "Q500/Q500_Config_020325.xlsx",              "Q500/Q500-multiple plate.txt",       "brain_tissue", FALSE),
  list("ba_single",         "Bile_Acids/Bile_Acids_Config_020325.xlsx",  "Bile_Acids/new BA-simple plate.txt", "serum",        TRUE),
  list("ba_multi",          "Bile_Acids/Bile_Acids_Config_020325.xlsx",  "Bile_Acids/BA-multiple plates.txt",  "serum",        TRUE),
  list("q1000_full",        "Q1000/Q1000_Full Panel_config_110226.xlsx", "Q1000/Q1000-Full panel.txt",         "conditioned_media", TRUE),
  list("q1000_lipids",      "Q1000/Q1000_Lipids_config_110226.xlsx",     "Q1000/Q1000 lipid.txt",              "Tissue",       TRUE),
  list("q1000_lipids_cfg_on_full", "Q1000/Q1000_Lipids_config_110226.xlsx", "Q1000/Q1000-Full panel.txt",      "conditioned_media", TRUE),
  list("q1000_small_synth", "Q1000/Q1000_Small Molecules_config_110226.xlsx", file.path(synth, "Q1000-small molecules (synthetic).txt"), "conditioned_media", TRUE),
  list("q1000_2plates_synth", "Q1000/Q1000_Full Panel_config_110226.xlsx", file.path(synth, "Q1000-full panel 2 plates (synthetic).txt"), "conditioned_media", TRUE)
)

results <- c()
for (r in runs) {
  tag <- r[[1]]; cfg <- file.path(d, r[[2]]); txt <- if (file.exists(r[[3]])) r[[3]] else file.path(d, r[[3]])
  if (!file.exists(cfg) || !file.exists(txt)) { cat(sprintf("%-26s SKIPPED (missing %s)\n", tag, if (!file.exists(cfg)) cfg else txt)); next }
  Sys.setenv(FIXED = if (r[[5]]) "TRUE" else "FALSE")
  log <- file.path(test_out, stringr::str_c(tag, ".log"))
  status <- system2("Rscript", c(shQuote(file.path(tests_dir, "pipeline_check.R")), shQuote(cfg), shQuote(txt), tag, shQuote(r[[4]])), stdout = log, stderr = log)
  out <- readLines(log, warn = FALSE)
  cat(sprintf("%-26s %s\n", tag, if (status == 0) "PASSED" else "FAILED"))
  if (status != 0) { cat(paste0("    ", grep("^(FAIL|STEP)", out, value = TRUE)), sep = "\n") }
  results[tag] <- status == 0
}

#a config for another panel must stop the load with a clear message (expected failure)
wrong_cfg <- file.path(d, "Q1000/Q1000_Full Panel_config_110226.xlsx"); wrong_txt <- file.path(d, "Q1000/Q1000 lipid.txt")
if (file.exists(wrong_cfg) && file.exists(wrong_txt)) {
  log <- file.path(test_out, "wrong_config.log")
  system2("Rscript", c(shQuote(file.path(tests_dir, "pipeline_check.R")), shQuote(wrong_cfg), shQuote(wrong_txt), "wrong_config"), stdout = log, stderr = log)
  ok <- any(grepl("STEP FAILED: indicators - Data file is missing 325 config analyte", readLines(log, warn = FALSE)))
  cat(sprintf("%-26s %s\n", "wrong_config_stops", if (ok) "PASSED" else "FAILED"))
  results["wrong_config_stops"] <- ok
}

cat(sprintf("\n%d of %d passed, logs in %s\n", sum(results), length(results), test_out))
quit(save = "no", status = if (all(results)) 0 else 1)
