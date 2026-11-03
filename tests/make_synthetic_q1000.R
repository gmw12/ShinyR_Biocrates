#Synthetic Q1000 exports built from the real full panel export (cell strings kept as-is), for cases with no real export:
#  1. small molecules only: lipid columns and the lipid barcode removed
#  2. two plates: a copy with new barcodes, analyte j scaled by 1 + 0.5 * j / n (a different norm factor for every analyte)
#usage: Rscript tests/make_synthetic_q1000.R [output dir]  (default <TEST_OUT>/synth)
script_dir <- function() { f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)); dirname(normalizePath(f)) }
app_dir <- Sys.getenv("APPDIR", dirname(script_dir()))
test_out <- Sys.getenv("TEST_OUT", file.path(tempdir(), "biocrates_tests"))
args <- commandArgs(TRUE)
synth_dir <- if (length(args) >= 1) args[1] else file.path(test_out, "synth")
dir.create(synth_dir, recursive = TRUE, showWarnings = FALSE)
setwd(app_dir)
src <- "data/Q1000/Q1000-Full panel.txt"
first <- readLines(src, n = 1)
d <- data.table::fread(src, skip = 1, header = FALSE, colClasses = "character", sep = "\t", fill = TRUE)
d <- as.data.frame(d); hdr <- unlist(d[1, ]); body <- d[-1, ]
pb <- which(hdr == "Plate bar code"); lab <- which(hdr == "Measurement time")
L <- readxl::read_excel("data/Q1000/Q1000_Lipids_config_110226.xlsx", "Analytes")$Name
F <- readxl::read_excel("data/Q1000/Q1000_Full Panel_config_110226.xlsx", "Analytes")$Name
out <- function(df, file) { con <- file(file, "w"); writeLines(first, con); close(con)
  data.table::fwrite(rbind(setNames(as.data.frame(t(hdr)), names(df)), df), file, sep = "\t", col.names = FALSE, append = TRUE, quote = FALSE) }
is_data <- grepl("^[0-9]", body[[1]])

#1. small molecules only: drop lipid columns and the lipid barcode (1055979741-1)
keep_cols <- !(hdr %in% L)
sm <- body[, keep_cols]
sm_hdr <- hdr[keep_cols]
drop_row <- (is_data & sm[[pb]] == "1055979741-1") | (!is_data & grepl("1055979741-1", sm[[lab]], fixed = TRUE))
sm <- sm[!drop_row, ]
sm[[pb]] <- sub(" | 1055979741-1", "", sm[[pb]], fixed = TRUE)
hdr_save <- hdr; hdr <- sm_hdr; out(sm, file.path(synth_dir, "Q1000-small molecules (synthetic).txt")); hdr <- hdr_save
cat("small molecules:", ncol(sm), "cols,", nrow(sm), "rows, plates:", unique(sm[[pb]][grepl("^[0-9]", sm[[1]])]), "\n")

#2. two plates: plate 2 = copy with new barcodes, analyte j scaled by 1 + 0.5 * j / n (different factor for every analyte)
p2 <- body
bc_new <- function(x) gsub("10559797(22|37|41)-1", "20559797\\1-1", x)
p2[[pb]] <- bc_new(p2[[pb]]); p2[[lab]] <- ifelse(is_data, p2[[lab]], bc_new(p2[[lab]]))
sb <- which(hdr == "Sample bar code"); p2[[sb]] <- ifelse(is_data, paste0(p2[[sb]], "_P2"), p2[[sb]])
an_cols <- match(F, hdr)
for (j in seq_along(an_cols)) { c <- an_cols[j]; v <- suppressWarnings(as.numeric(p2[[c]])); sc <- is_data & !is.na(v)
  p2[[c]][sc] <- format(signif(v[sc] * (1 + 0.5 * j / length(F)), 6), scientific = FALSE, trim = TRUE) }
two <- rbind(body[!is_data, ], p2[!is_data, ], body[is_data, ], p2[is_data, ])
out(two, file.path(synth_dir, "Q1000-full panel 2 plates (synthetic).txt"))
cat("two plates:", ncol(two), "cols,", nrow(two), "rows\n")
