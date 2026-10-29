#!/usr/bin/env Rscript
# Replace ý (U+00FD) with ẏ (U+1E8F) in corpus raw data files.

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))

Y_OLD <- "\u00fd"
Y_NEW <- "\u1e8f"

normalize_y <- function(x) {
  if (is.character(x)) gsub(Y_OLD, Y_NEW, x, fixed = TRUE) else x
}

merge_word_df <- function(df) {
  cols <- tolower(names(df))
  names(df) <- cols
  text_cols <- intersect(c("word", "norm", "lemma", "grapho"), cols)
  for (col in text_cols) {
    df[[col]] <- normalize_y(df[[col]])
  }
  if (!"word" %in% cols) {
    return(df)
  }
  df <- df[!is.na(df$word) & df$word != "", ]
  if (!"frequency" %in% cols) {
    return(df)
  }
  df$frequency <- as.numeric(df$frequency)
  df <- df[!is.na(df$frequency), ]
  other <- setdiff(cols, c("word", "frequency"))
  df |>
    group_by(word) |>
    summarise(
      frequency = sum(frequency, na.rm = TRUE),
      across(all_of(other), ~ dplyr::first(.x)),
      .groups = "drop"
    ) |>
    arrange(desc(frequency))
}

process_csv <- function(path) {
  if (!file.exists(path)) {
    return(invisible(NULL))
  }
  lines <- readLines(path, encoding = "UTF-8")
  n_before <- sum(grepl(Y_OLD, lines, fixed = TRUE))
  df <- read_csv(path, show_col_types = FALSE)
  out <- merge_word_df(df)
  write_csv(out, path)
  message(sprintf("%s: %d lines with legacy y; %d rows after merge", path, n_before, nrow(out)))
}

process_txt_wordlist <- function(path) {
  if (!file.exists(path)) {
    return(invisible(NULL))
  }
  lines <- readLines(path, encoding = "UTF-8")
  n_before <- sum(grepl(Y_OLD, lines, fixed = TRUE))
  lines <- normalize_y(lines)
  if (any(grepl("\t", lines))) {
    df <- read.delim(
      text = lines,
      header = FALSE,
      stringsAsFactors = FALSE,
      col.names = c("word", "frequency")
    )
    if (identical(df$word[1], "Word")) {
      df <- df[-1, ]
    }
    df$frequency <- as.numeric(df$frequency)
    out <- merge_word_df(df)
    write.table(
      out,
      path,
      sep = "\t",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      fileEncoding = "UTF-8"
    )
    message(sprintf("%s: %d lines with legacy y; %d rows after merge", path, n_before, nrow(out)))
  } else {
    writeLines(lines, path, useBytes = TRUE)
    message(sprintf("%s: %d lines with legacy y (plain text)", path, n_before))
  }
}

process_edition <- function(path) {
  if (!file.exists(path)) {
    return(invisible(NULL))
  }
  lines <- readLines(path, encoding = "UTF-8")
  n_before <- sum(grepl(Y_OLD, lines, fixed = TRUE))
  lines <- normalize_y(lines)
  writeLines(lines, path, useBytes = TRUE)
  message(sprintf("%s: %d lines with legacy y", path, n_before))
}

process_csv("word_list_with_refkorpus.csv")
process_csv("word_list_with_kmhd.csv")
process_txt_wordlist("word_list.txt")
process_edition("editioYolanda.txt")

for (f in c(
  "word_list_with_refkorpus.csv",
  "word_list_with_kmhd.csv",
  "word_list.txt",
  "editioYolanda.txt"
)) {
  if (file.exists(f)) {
    n <- sum(grepl(Y_OLD, readLines(f, encoding = "UTF-8"), fixed = TRUE))
    message("remaining legacy y in ", f, ": ", n)
  }
}
