#Headless pipeline run + verification BY ANALYTE NAME against the raw WebIDQ export.
#Runs the *_bg pipeline functions in order (config, load, status, indicators, separate, report template,
#replace LOD, QC, process material, filters, Excel) on a scratch database, then checks that every analyte
#value in every table (and the Excel sheets) is the raw value of that analyte, i.e. nothing is scrambled.
#
#usage: Rscript tests/pipeline_check.R <config.xlsx> <data.txt> <tag> [material]
#  env APPDIR   app directory (default: repo root)
#  env TEST_OUT output directory (default: <tempdir>/biocrates_tests), results in <TEST_OUT>/run/<tag>
#  env FIXED    "TRUE" (default) fixed LOD imputation, "FALSE" random 0.1-1 x LOD
#prints ok:/FAIL: lines and a final "===== <tag> : ALL CHECKS PASSED" line, exit status 1 on failure
suppressMessages({library(dplyr); library(stringr); library(RSQLite); library(tidyverse)})
args <- commandArgs(TRUE)
cfg <- args[1]; txt <- args[2]; tag <- args[3]; material_arg <- if (length(args) >= 4) args[4] else NA
script_dir <- function() { f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); dirname(normalizePath(f)) }
app_dir <- Sys.getenv("APPDIR", dirname(script_dir()))
test_out <- Sys.getenv("TEST_OUT", file.path(tempdir(), "biocrates_tests"))
cfg <- normalizePath(cfg); txt <- normalizePath(txt)
setwd(app_dir)
out_dir <- file.path(test_out, "run", tag); unlink(out_dir, recursive = TRUE); dir.create(out_dir, recursive = TRUE)
errcon <- file(file.path(out_dir, "stderr.txt"), open = "wt"); sink(errcon, type = "message")
source("Shiny_File.R"); source("Shiny_Data.R"); source("Shiny_Misc_Functions.R")

fails <- c()
check <- function(ok, msg) { if (!isTRUE(ok)) { fails <<- c(fails, msg); cat("FAIL:", msg, "\n") } else cat("ok:  ", msg, "\n") }
step <- function(name, expr) { r <- try(expr, silent = TRUE); if (inherits(r, "try-error")) { cat("STEP FAILED:", name, "-", conditionMessage(attr(r, "condition")), "\n"); fails <<- c(fails, name); return(NULL) }; r }

params <- data.frame(database_path = file.path(out_dir, "test.db"), data_path = str_c(out_dir, "/"), error_path = out_dir,
                     plot_path = str_c(out_dir, "/"), simple_plate = FALSE, fixed_lod = Sys.getenv("FIXED", "TRUE") == "TRUE", qc_acc = 20, stringsAsFactors = FALSE)

#--- config + kit
invisible(step("config", excel_to_db(cfg, "config", params$database_path, list("Analytes", "QC"))))
if (exists("config_kit")) { params <- step("config_kit", config_kit(params)) } else {
  n <- get_max_rowid("Analytes", params)[[1]]; params$data_source <- if (n == 20) "BileAcid" else "Q500"; params$analyte_count <- n }
cat("data_source:", params$data_source, "\n")
an <- read_table_try("Analytes", params, check_names = FALSE)
cat("analytes:", nrow(an), "\n")

#--- load + split
invisible(step("load", load_data_bg(list(datapath = txt), params)))
invisible(step("status", remove_status_cols_bg(params)))
invisible(step("indicators", remove_indicators_bg(params)))
sep <- step("separate", separate_data_bg(params))
if (!is.null(sep)) { params <- sep[[2]]; check(isTRUE(sep[[1]]), "analyte_match"); if (nchar(sep[[3]]) > 0) cat("export message:", sep[[3]], "\n") }
cat("plates:", params$plates, " plate_number:", params$plate_number, " materials:", params$materials, "\n")
lp <- step("report_template", report_template_bg(params)); if (length(lp)) cat("LOD problems:", lp, sep = "\n  ")
lp <- step("replace_lod", replace_lod_bg(params)); if (length(lp)) cat("replace LOD problems:", lp, sep = "\n  ")
qp <- step("qc_calc", qc_calc_bg(params)); if (length(qp)) cat("QC problems:", qp, sep = "\n  ")

#--- material
mats <- unlist(str_split(params$materials, ","))
material <- if (!is.na(material_arg)) material_arg else mats[1]
params$material_select <- material; params$norm_select <- if (params$plate_number > 1) "SPQC" else "None"
params$spqc_filter <- TRUE; params$spqc_filter_value <- 30; params$missing_filter <- TRUE; params$missing_filter_value <- 50
params$spqc_replace <- "No"
cm <- step("check_material", check_material(params)); if (length(cm)) cat("material errors:", cm, "\n")
if (length(cm) == 0) {
  np <- step("process", process_data_bg(params)); if (length(np)) cat("norm problems:", np, sep = "\n  ")
  invisible(step("filter", spqc_missing_filter_bg(params, use_norm = FALSE)))
  if (params$norm_select != "None") invisible(step("filter_norm", spqc_missing_filter_bg(params, use_norm = TRUE)))
  params$material_list <- material; params$material_norms <- str_c(material, "=", params$norm_select)
  invisible(step("excel", final_excel_bg(params, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, "test.xlsx")))
}

#================= verification by NAME against the raw export =================
raw <- as.data.frame(data.table::fread(txt, header = TRUE, skip = 1, sep = "\t", fill = TRUE, check.names = FALSE))
raw_rows <- raw[grepl("^[0-9]", raw[[1]]), ]
nm <- an$Name; mk <- make.names(nm)
numv <- function(x) suppressWarnings(as.numeric(x))

#analyte columns in every analyte table: last n columns, in config order, with the config's mangled names
tables <- list_tables(params)
for (t in intersect(c("data_no_indicators", "data_start", "data_impute", material, str_c("SPQC_Norm_", material)), tables)) {
  d <- read_table_try(t, params)
  check(identical(tail(colnames(d), length(nm)), mk), str_c(t, ": last ", length(nm), " columns are the config analytes in config order"))
}
rt <- read_table_try("Report_template", params, check_names = FALSE)
check(identical(rt$Name, nm), "Report_template rows = config analytes in order")
check(identical(rt$R_colnames, mk), "Report_template R_colnames = make.names(config names)")

#data_start values == raw values, cell by cell, by analyte name and row identity
ds <- read_table_try("data_start", params)
key <- function(d, cols) { k <- gsub("[;:?&*]", "_", do.call(paste, c(d[cols], sep = "|"))); paste(k, ave(seq_along(k), k, FUN = seq_along), sep = "#") }
raw_cols <- c("Plate bar code", "Sample bar code", "Well position", "Measurement time")
raw_key <- key(raw_rows, raw_cols)
ds_key <- key(ds, make.names(raw_cols))
m <- match(ds_key, raw_key)
check(!any(is.na(m)) && !any(duplicated(m)), str_c("data_start rows map 1:1 to raw rows (", nrow(ds), ")"))
clean_val <- function(x) gsub("[;:?&*]", "_", gsub("\\[.*?M\\]", "[uM]", x))
if (!any(is.na(m))) {
  a <- as.matrix(ds[, mk]); b <- as.matrix(raw_rows[m, nm])
  a_num <- numv(a); b_num <- numv(b)
  same <- (is.na(a_num) & is.na(b_num) & (trimws(ifelse(is.na(a), "", a)) == trimws(ifelse(is.na(b), "", clean_val(b))))) |
          (!is.na(a_num) & !is.na(b_num) & abs(a_num - b_num) <= 1e-9 * pmax(1, abs(b_num)))
  check(all(same), str_c("data_start values equal raw values by name (", sum(!same), " mismatches of ", length(same), ")"))
}

#LOD per plate/analyte: OP LOD of the barcodes in the plate group (calc LOD if no OP)
lab_col <- which(sapply(raw, function(c) any(grepl("^LOD \\(", c, useBytes = TRUE))))[1]
lod_rows <- raw[grepl("^LOD \\(", raw[[lab_col]], useBytes = TRUE), ]
plates <- unlist(str_split(params$plates, ","))
for (p in plates) {
  bcs <- trimws(unlist(str_split(p, fixed("|"))))
  pick <- function(kind) { r <- lod_rows[vapply(lod_rows[[lab_col]], function(l) grepl(kind, l, fixed = TRUE, useBytes = TRUE) && any(vapply(bcs, function(b) grepl(str_c(" ", b, " "), l, fixed = TRUE, useBytes = TRUE), logical(1))), logical(1)), nm, drop = FALSE]
                           if (nrow(r) == 0) return(rep(NA_real_, length(nm))); v <- matrix(numv(as.matrix(r)), nrow = nrow(r)); apply(v, 2, function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE)) }
  op <- pick("(from OP)"); cl <- pick("(calc.)"); exp_lod <- ifelse(is.na(op) | op == 0, cl, op); exp_lod[is.na(exp_lod)] <- 0
  col <- which(colnames(rt) == str_c("LOD_", p, ", uM"))
  check(length(col) == 1 && isTRUE(all.equal(numv(rt[[col]]), exp_lod)), str_c("LOD column for plate ", p, " = OP LOD by name"))
}

#imputed values: non-LOD cells unchanged, LOD cells = plate LOD (fixed_lod)
di <- read_table_try("data_impute", params)
di_key <- key(di, make.names(raw_cols))
mi <- match(di_key, raw_key)
if (!any(is.na(mi))) {
  b <- as.matrix(raw_rows[mi, nm]); a_num <- numv(as.matrix(di[, mk])); b_num <- numv(b)
  is_lod <- matrix(grepl("LOD", b), nrow = nrow(b)) | (!is.na(b_num) & b_num == 0)
  ok_val <- is_lod | (is.na(a_num) & is.na(b_num)) | (!is.na(a_num) & !is.na(b_num) & abs(a_num - b_num) <= 1e-9 * pmax(1, abs(b_num)))
  check(all(ok_val), str_c("data_impute non-LOD values unchanged by name (", sum(!ok_val), " bad)"))
  plate_of <- sapply(di$Plate.bar.code, function(pb) { bc <- trimws(unlist(str_split(pb, fixed("|")))); hit <- plates[sapply(plates, function(p) all(bc %in% trimws(unlist(str_split(p, fixed("|"))))))]; if (length(hit) == 1) hit else NA })
  exp <- sapply(seq_len(nrow(di)), function(i) if (is.na(plate_of[i])) rep(NA, length(nm)) else numv(rt[[str_c("LOD_", plate_of[i], ", uM")]]))
  exp <- t(matrix(exp, nrow = length(nm)))
  if (params$fixed_lod) {
    ok_lod <- !is_lod | (abs(a_num - exp) <= 1e-6 * pmax(1, exp))
  } else {
    #random 0.1-1 x LOD, rounded to 5 digits
    ok_lod <- !is_lod | (a_num >= 0.1 * exp - 1e-5 & a_num <= exp + 1e-5)
  }
  check(all(ok_lod, na.rm = TRUE) && !any(is.na(ok_lod[is_lod])), str_c("data_impute < LOD cells = ", if (params$fixed_lod) "" else "0.1-1 x ", "plate LOD by name (", sum(!ok_lod, na.rm = TRUE), " bad, ", sum(is.na(ok_lod[is_lod])), " NA)"))
}

#QC report: recompute mean/accuracy per plate group straight from raw rows by name
if ("QC_Report" %in% tables) {
  qr <- read_table_try("QC_Report", params, check_names = FALSE)
  qcfg <- as.data.frame(readxl::read_excel(cfg, "QC"))
  check(identical(qr$Name, nm), "QC_Report rows = config analytes in order")
  qc_map <- c(QC1 = "QC Level 1", QC2 = "QC Level 2", QC3 = "QC Level 3")
  for (q in names(qc_map)) {
    expv <- numv(unlist(qcfg[qcfg[[1]] == q, nm]))
    check(isTRUE(all.equal(numv(qr[[str_c(q, " Level (uM)")]]), expv)), str_c(q, " expected values matched by name"))
    for (p in plates) {
      bcs <- trimws(unlist(str_split(p, fixed("|"))))
      rr <- raw_rows[raw_rows[["Sample type"]] == qc_map[[q]] & sapply(raw_rows[["Plate bar code"]], function(pb) all(trimws(unlist(str_split(pb, fixed("|")))) %in% bcs)), nm, drop = FALSE]
      v <- matrix(numv(as.matrix(rr)), nrow = nrow(rr)); v[!is.na(v) & v == 0] <- NA
      mu <- round(colMeans(v, na.rm = TRUE), 5); mu[is.nan(mu)] <- NA
      col <- str_c(q, " ", p, " Mean (uM)")
      if (!(col %in% colnames(qr))) { check(FALSE, str_c("QC_Report has column ", col)); next }
      check(isTRUE(all.equal(numv(qr[[col]]), unname(mu))), str_c(col, " matches raw by name (", sum(!is.na(mu)), " analytes with values)"))
    }
  }
  cat("QC_Report columns:", ncol(qr), "\n")
}

#normalization: factors from the raw SPQC rows by name, normalized = material value / factor
norm_tab <- str_c("SPQC_Norm_", material)
if (norm_tab %in% tables) {
  nf <- read_table_try(str_c("SPQC_Norm_Factor_", material), params, check_names = FALSE)
  spqc <- raw_rows[grepl("SPQC", raw_rows[["Sample description"]]) & gsub(" ", "_", trimws(raw_rows$Material)) == material, ]
  v <- matrix(numv(as.matrix(spqc[, nm])), nrow = nrow(spqc)); v[!is.na(v) & v == 0] <- NA
  pm <- sapply(plates, function(p) { x <- colMeans(v[spqc[["Plate bar code"]] == p, , drop = FALSE], na.rm = TRUE); x[is.nan(x)] <- NA; x })
  fac <- pm / rowMeans(pm, na.rm = TRUE); fac[is.na(fac)] <- 1
  for (p in plates) {
    col <- str_c("SPQC ", p, " ", material)
    check(isTRUE(all.equal(numv(nf[[col]]), unname(fac[, p]))), str_c("norm factor ", p, " matches raw SPQC by name"))
  }
  dn <- read_table_try(norm_tab, params); dm <- read_table_try(material, params)
  k_n <- key(dn, make.names(raw_cols)); k_m <- key(dm, make.names(raw_cols)); mm <- match(k_n, k_m)
  ok_rows <- !is.na(mm)
  fac_rows <- t(fac[, dn$Plate.bar.code[ok_rows], drop = FALSE])
  expv <- round(matrix(numv(as.matrix(dm[mm[ok_rows], mk])), nrow = sum(ok_rows)) / fac_rows, 5)
  got <- matrix(numv(as.matrix(dn[ok_rows, mk])), nrow = sum(ok_rows))
  check(isTRUE(all.equal(got, expv, check.attributes = FALSE, tolerance = 1e-6)), str_c("normalized values = material / factor by name (", sum(ok_rows), " rows)"))
  cat("norm factor range:", range(fac), "\n")
}

#Excel: analyte headers restored and values match the material table by name
xl <- file.path(out_dir, "test.xlsx")
if (file.exists(xl)) {
  sh <- openxlsx::getSheetNames(xl); cat("sheets:", sh, "\n")
  for (s in intersect(c("Imputed Data", material), sh)) {
    x <- openxlsx::read.xlsx(xl, s, check.names = FALSE, sep.names = " ")
    check(identical(tail(colnames(x), length(nm)), nm), str_c("Excel '", s, "' analyte headers = config names in order"))
    src <- read_table_try(if (s == "Imputed Data") "data_impute" else material, params)
    check(isTRUE(all.equal(unname(as.matrix(x[, nm])), unname(matrix(numv(as.matrix(src[, mk])), nrow = nrow(src))), check.attributes = FALSE)), str_c("Excel '", s, "' values match DB by name"))
  }
  fs <- grep("^Filtered_", sh, value = TRUE)
  for (s in fs) {
    x <- openxlsx::read.xlsx(xl, s, check.names = FALSE, sep.names = " ")
    src <- read_table_try(s, params); keep <- nm[mk %in% colnames(src)]
    check(identical(tail(colnames(x), length(keep)), keep), str_c("Excel '", s, "' headers = kept analytes (", length(keep), ")"))
    check(isTRUE(all.equal(unname(as.matrix(x[, keep])), unname(matrix(numv(as.matrix(src[, make.names(keep)])), nrow = nrow(src))), check.attributes = FALSE)), str_c("Excel '", s, "' values match by name"))
  }
}

sink(type = "message")
cat("\n=====", tag, ":", if (length(fails)) str_c(length(fails), " FAILURES: ", str_c(fails, collapse = " || ")) else "ALL CHECKS PASSED", "\n")
quit(save = "no", status = if (length(fails)) 1 else 0)
