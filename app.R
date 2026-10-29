# Shiny App for Enhanced Letter Combination Analyzer
# This app provides an interactive web interface for analyzing letter combinations
# up to 4 letters on left and right of a specific letter

library(shiny)
library(tidyverse)
library(stringr)
library(DT)
library(htmlwidgets)
library(webshot2)

# Canonical ẏ grapheme (display + analysis only).
Y_GRAPH <- "\u1e8f"
Y_GRAPH_LEGACY <- Y_GRAPH

has_y_grapheme <- function(x) {
  grepl(Y_GRAPH, x, fixed = TRUE)
}

normalize_y_grapheme <- function(x) {
  x
}

swap_y_to_i <- function(w) {
  gsub(Y_GRAPH, "i", w, fixed = TRUE)
}

swap_i_to_y <- function(w) {
  gsub("i", Y_GRAPH, w, fixed = TRUE)
}

y_letter_in_word <- function(word) {
  if (grepl(Y_GRAPH, word, fixed = TRUE)) Y_GRAPH
  else Y_GRAPH
}

find_word_row <- function(word_data, w) {
  m <- word_data[word_data$word == w, , drop = FALSE]
  if (nrow(m) > 0) return(m)
  m
}

# Canonical long-s grapheme (display + analysis only).
LONG_S_GRAPH <- "\u017f"

has_long_s_grapheme <- function(x) {
  grepl(LONG_S_GRAPH, x, fixed = TRUE)
}

swap_longs_to_round <- function(w) {
  gsub(LONG_S_GRAPH, "s", w, fixed = TRUE)
}

swap_round_to_longs <- function(w) {
  gsub("s", LONG_S_GRAPH, w, fixed = TRUE)
}

s_letter_in_word <- function(word) {
  if (grepl(LONG_S_GRAPH, word, fixed = TRUE)) LONG_S_GRAPH else "s"
}

# Canonical phoneme symbols for analysis display (grapho raw → normalized).
PHONEME_MACRON <- "\u0304"
PHONEME_u_bar <- paste0("u", PHONEME_MACRON)
PHONEME_i_bar <- "\u012b"
PHONEME_e_bar <- paste0("e", PHONEME_MACRON)
PHONEME_o_bar <- paste0("o", PHONEME_MACRON)
PHONEME_a_bar <- paste0("a", PHONEME_MACRON)
PHONEME_a_umlaut_bar <- paste0("\u00e4", PHONEME_MACRON)
PHONEME_o_umlaut_bar <- paste0("\u00f6", PHONEME_MACRON)
PHONEME_u_umlaut_bar <- paste0("\u00fc", PHONEME_MACRON)
PHONEME_ue <- paste0("\u00fc", "e")
PHONEME_ou_umlaut <- paste0("\u00f6", "\u00fc")

# Map legacy grapho phoneme labels to canonical analysis symbols.
normalize_phoneme <- function(ph) {
  if (is.na(ph) || !nzchar(ph)) {
    return(ph)
  }
  ph <- stringi::stri_trans_nfc(trimws(as.character(ph)))
  switch(
    ph,
    "\u00e8" = "\u00e4",
    "i\u00e8" = "ie",
    "\u00e8i" = "ei",
    "\u00fc\u00e8" = PHONEME_ue,
    "iu" = PHONEME_u_umlaut_bar,
    "\u00ee" = PHONEME_i_bar,
    "\u00fb" = PHONEME_u_bar,
    "\u00ea" = PHONEME_e_bar,
    "o\u00e8" = PHONEME_o_umlaut_bar,
    "\u00f4" = PHONEME_o_bar,
    "\u00e2" = PHONEME_a_bar,
    "\u00e6" = PHONEME_a_umlaut_bar,
    "\u00f6u" = PHONEME_ou_umlaut,
    ph
  )
}

# Global definitions for letter combinations
# Define diphthongs (vowel combinations), digraphs (consonant combinations), and trigraphs
DIPHTHONGS <- c("ei", "ai", "oi", "ui", "ůe" ,"ůi")  # Vowel combinations
DIGRAPHS <- c("ch", "ſc", "zc")    # Consonant combinations
TRIGRAPHS <- c("ſch")              # Three-letter combinations

# Markers for processing (private Unicode characters)
DIPHTHONG_MARKERS <- c("\ue000", "\ue001", "\ue002", "\ue007", "\ue008", "\ue009")
names(DIPHTHONG_MARKERS) <- DIPHTHONGS

DIGRAPH_MARKERS <- c("\ue003", "\ue004", "\ue006")
names(DIGRAPH_MARKERS) <- DIGRAPHS

TRIGRAPH_MARKERS <- c("\ue005")
names(TRIGRAPH_MARKERS) <- TRIGRAPHS

# Isolated lines that should be ignored in rhyme analysis
ISOLATED_LINES <- c(4793, 4401, 4255, 3941, 3942, 3817, 3637, 3349, 2961, 1663, 1571, 1443, 1399, 1199, 729, 409)

# Function to read and process the word list
read_word_list <- function(file_path) {
  # Read the word list file
  word_data <- read.delim(file_path, header = FALSE, col.names = c("word", "frequency"), 
                          stringsAsFactors = FALSE, encoding = "UTF-8")
  
  # Remove the header row if it exists
  if (word_data$word[1] == "Word") {
    word_data <- word_data[-1, ]
  }
  
  # Convert frequency to numeric
  word_data$frequency <- as.numeric(word_data$frequency)
  
  # Remove any rows with NA frequencies
  word_data <- word_data[!is.na(word_data$frequency), ]
  
  return(word_data)
}

# Helper: load/save base word list with refkorpus data
load_base_word_list <- function() {
  # First try to load the refkorpus-enhanced word list
  refkorpus_path <- "word_list_with_refkorpus.csv"
  if (file.exists(refkorpus_path)) {
    df <- readr::read_csv(refkorpus_path, show_col_types = FALSE)
    colnames(df) <- tolower(colnames(df))
    # Select only refkorpus columns
    available_cols <- intersect(c("word", "frequency", "norm", "lemma", "grapho"), colnames(df))
    df <- df %>% select(all_of(available_cols))
    
    # Remove rows with NA values in word or frequency
    df <- df[!is.na(df$word) & !is.na(df$frequency), ]
    
    # Aggregate duplicate words by summing their frequencies
    # Keep the first occurrence of norm, lemma, grapho for each unique word
    df <- df %>%
      group_by(word) %>%
      summarise(
        frequency = sum(frequency, na.rm = TRUE),
        norm = first(norm),
        lemma = first(lemma),
        grapho = first(grapho),
        .groups = 'drop'
      ) %>%
      arrange(desc(frequency))
    
    return(normalize_word_refkorpus_columns(df))
  }
  
  # Fallback to the old kmhd CSV
  csv_path <- "word_list_with_kmhd.csv"
  if (file.exists(csv_path)) {
    df <- readr::read_csv(csv_path, show_col_types = FALSE)
    colnames(df) <- tolower(colnames(df))
    # Add missing refkorpus columns
    if (!"norm" %in% colnames(df)) df$norm <- ""
    if (!"lemma" %in% colnames(df)) df$lemma <- ""
    if (!"grapho" %in% colnames(df)) df$grapho <- ""
    df <- df %>% select(word, frequency, norm, lemma, grapho)
    
    # Aggregate duplicate words by summing their frequencies
    df <- df %>%
      group_by(word) %>%
      summarise(
        frequency = sum(frequency, na.rm = TRUE),
        norm = first(norm),
        lemma = first(lemma),
        grapho = first(grapho),
        .groups = 'drop'
      ) %>%
      arrange(desc(frequency))
    
    return(normalize_word_refkorpus_columns(df))
  }
  
  # Final fallback to word_list.txt
  if (file.exists("word_list.txt")) {
    df <- read_word_list("word_list.txt")
    # Add refkorpus columns
    df$norm <- ""
    df$lemma <- ""
    df$grapho <- ""
    
    # Aggregate duplicate words by summing their frequencies
    df <- df %>%
      group_by(word) %>%
      summarise(
        frequency = sum(frequency, na.rm = TRUE),
        norm = first(norm),
        lemma = first(lemma),
        grapho = first(grapho),
        .groups = 'drop'
      ) %>%
      arrange(desc(frequency))
    
    df <- normalize_word_refkorpus_columns(df)
    readr::write_csv(df, refkorpus_path)
    return(df)
  }
  stop("No base word list found")
}

normalize_word_refkorpus_columns <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }
  for (col in intersect(c("norm", "lemma", "grapho"), colnames(df))) {
    df[[col]] <- if_else(is.na(df[[col]]), "", as.character(df[[col]]))
  }
  df
}

save_word_list_csv <- function(df, path = "word_list_with_refkorpus.csv") {
  readr::write_csv(normalize_word_refkorpus_columns(df), path)
}

# Column names whose DT filter boxes should accept JavaScript regex patterns.
WORD_FIELD_FILTER_COLUMNS <- c(
  "word", "Word", "words", "word(s)", "forms",
  "Word1", "Word2",
  "y_word", "i_word",
  "longs_word", "round_word",
  "norm", "lemma", "grapho",
  "lexeme", "lexeme (norm)",
  "y_grapho", "i_grapho",
  "longs_grapho", "round_grapho",
  "Word1_Grapho", "Word2_Grapho",
  "Rhyme"
)

# Enable regex search on word/norm/lemma/grapho column filters in DT tables.
dt_add_word_field_regex_filters <- function(options = list(), colnames_df, extra_init_body = NULL) {
  regex_names <- colnames_df[colnames_df %in% WORD_FIELD_FILTER_COLUMNS]
  if (length(regex_names) == 0 && is.null(extra_init_body)) {
    return(options)
  }

  regex_block <- if (length(regex_names) > 0) {
    names_json <- jsonlite::toJSON(as.character(regex_names), auto_unbox = FALSE)
    sprintf(
      "function attachWordFieldRegexFilters(settings) {
        var regexNames = %s;
        var api = new $.fn.dataTable.Api(settings);
        var aoCols = settings.aoColumns;
        var $table = $(api.table().node());
        var $filterCells = $table.children('thead').children('tr:last').children('td');
        if (!$filterCells.length) return;
        var colTitle = function(idx) {
          var title = aoCols[idx].sTitle || '';
          if (!title && aoCols[idx].nTh) {
            title = aoCols[idx].nTh.textContent || '';
          }
          return $('<div>').html(title).text().replace(/\\s+/g, ' ').trim();
        };
        var applyRegexFilter = function(colIdx, val) {
          if (val === '') {
            api.column(colIdx).search('', false, false, true);
          } else if (val === '^$') {
            // Match empty cells (DT omits NA from JSON, so also match literal NA strings).
            api.column(colIdx).search('^$|^<NA>$|^NA$', true, false, true);
          } else if (val === '^\\\\s*$') {
            api.column(colIdx).search('^\\\\s*$|^\\\\s*<NA>\\\\s*$|^\\\\s*NA\\\\s*$', true, false, true);
          } else {
            try {
              api.column(colIdx).search(val, true, false, true);
            } catch (e) {
              api.column(colIdx).search(val, false, false, true);
            }
          }
          api.draw();
        };
        api.columns().every(function() {
          var colIdx = this.index();
          if (regexNames.indexOf(colTitle(colIdx)) === -1) return;
          var $td = $($filterCells.get(colIdx));
          if (!$td.length) return;
          var $input = $td.children('div').first().children('input');
          if (!$input.length || $input.prop('disabled')) return;
          $input.attr('title', 'Regex (JavaScript). Empty: ^$. Literal | in grapho: \\\\|u=u\\\\|');
          $input.attr('placeholder', 'regex…');
          // Always replace DT handlers (DT binds after initComplete; do not skip re-attach).
          $input.off('input blur');
          $input.on('input blur', function() {
            $input.next('span').toggle(!!$input.val());
          });
          $input.on('input', function() {
            applyRegexFilter(colIdx, this.value);
          });
        });
      }
      // DT attaches default column filters after initComplete returns; defer until then.
      setTimeout(function() { attachWordFieldRegexFilters(settings); }, 0);",
      names_json
    )
  } else {
    ""
  }
  extra_block <- if (!is.null(extra_init_body)) extra_init_body else ""
  options$initComplete <- JS(sprintf(
    "function(settings, json) {
      %s
      %s
    }",
    regex_block,
    extra_block
  ))
  options
}

# Build spelling-variant table: one row per lemma with multiple manuscript word forms
format_labelled_freqs <- function(labels, freqs) {
  if (length(labels) == 0) {
    return("")
  }
  ord <- order(freqs, labels, decreasing = TRUE)
  paste0(labels[ord], " (", freqs[ord], ")", collapse = ", ")
}

build_lemma_variants_table <- function(word_data) {
  empty <- data.frame(
    lemma = character(),
    words = character(),
    norm = character(),
    grapho = character(),
    n_forms = integer(),
    Total_frequency = numeric(),
    primary_word = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(word_data) || nrow(word_data) == 0) {
    return(empty)
  }

  df <- word_data %>%
    filter(!is.na(word), word != "", !is.na(lemma), lemma != "") %>%
    mutate(
      norm = if_else(is.na(norm), "", as.character(norm)),
      grapho = if_else(is.na(grapho), "", as.character(grapho))
    )

  if (nrow(df) == 0) {
    return(empty)
  }

  variant_lemmas <- df %>%
    group_by(lemma) %>%
    filter(n_distinct(word) > 1) %>%
    ungroup()

  if (nrow(variant_lemmas) == 0) {
    return(empty)
  }

  variant_lemmas %>%
    arrange(desc(frequency), word) %>%
    group_by(lemma) %>%
    summarise(
      words = format_labelled_freqs(word, frequency),
      n_forms = n_distinct(word),
      Total_frequency = sum(frequency, na.rm = TRUE),
      primary_word = word[which.max(frequency)],
      .groups = "drop"
    ) %>%
    left_join(
      variant_lemmas %>%
        filter(norm != "") %>%
        group_by(lemma, norm) %>%
        summarise(freq = sum(frequency, na.rm = TRUE), .groups = "drop") %>%
        group_by(lemma) %>%
        summarise(norm = format_labelled_freqs(norm, freq), .groups = "drop"),
      by = "lemma"
    ) %>%
    left_join(
      variant_lemmas %>%
        filter(grapho != "") %>%
        group_by(lemma, grapho) %>%
        summarise(freq = sum(frequency, na.rm = TRUE), .groups = "drop") %>%
        group_by(lemma) %>%
        summarise(grapho = format_labelled_freqs(grapho, freq), .groups = "drop"),
      by = "lemma"
    ) %>%
    mutate(
      norm = coalesce(norm, ""),
      grapho = coalesce(grapho, "")
    ) %>%
    arrange(desc(Total_frequency), lemma)
}

format_corpus_count <- function(x) {
  format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_corpus_freq <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    return("—")
  }
  format(round(x, 2), nsmall = 2, trim = TRUE)
}

round_pct <- function(x) {
  round(x, digits = 0)
}

format_pct <- function(x) {
  paste0(round_pct(x), "%")
}

frequency_stats <- function(freqs) {
  if (length(freqs) == 0) {
    return(list(
      types = 0L,
      tokens = 0L,
      mean = NA_real_,
      median = NA_real_,
      min = NA_real_,
      max = NA_real_
    ))
  }
  list(
    types = length(freqs),
    tokens = sum(freqs, na.rm = TRUE),
    mean = mean(freqs, na.rm = TRUE),
    median = stats::median(freqs, na.rm = TRUE),
    min = min(freqs, na.rm = TRUE),
    max = max(freqs, na.rm = TRUE)
  )
}

compute_corpus_statistics <- function(word_data) {
  empty_overview <- data.frame(
    Metric = character(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  empty_breakdown <- data.frame(
    Metric = character(),
    `With grapho` = character(),
    `Without grapho` = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  empty_missing <- data.frame(
    word = character(),
    frequency = integer(),
    norm = character(),
    lemma = character(),
    stringsAsFactors = FALSE
  )

  if (is.null(word_data) || nrow(word_data) == 0) {
    return(list(
      overview = empty_overview,
      grapho_breakdown = empty_breakdown,
      top_without_grapho = empty_missing
    ))
  }

  df <- word_data %>%
    mutate(
      norm = if_else(is.na(norm), "", as.character(norm)),
      lemma = if_else(is.na(lemma), "", as.character(lemma)),
      grapho = if_else(is.na(grapho), "", as.character(grapho)),
      has_grapho = grapho != ""
    )

  total_tokens <- sum(df$frequency, na.rm = TRUE)
  total_types <- nrow(df)
  distinct_norm <- n_distinct(df$norm[df$norm != ""])
  distinct_lemma <- n_distinct(df$lemma[df$lemma != ""])

  with_grapho <- frequency_stats(df$frequency[df$has_grapho])
  without_grapho <- frequency_stats(df$frequency[!df$has_grapho])

  overview <- data.frame(
    Metric = c(
      "Sum of words (tokens)",
      "Different words (types)",
      "Different norm forms",
      "Different lemmas",
      "Types with grapho",
      "Tokens with grapho",
      "Types without grapho",
      "Tokens without grapho"
    ),
    Value = c(
      format_corpus_count(total_tokens),
      format_corpus_count(total_types),
      format_corpus_count(distinct_norm),
      format_corpus_count(distinct_lemma),
      format_corpus_count(with_grapho$types),
      format_corpus_count(with_grapho$tokens),
      format_corpus_count(without_grapho$types),
      format_corpus_count(without_grapho$tokens)
    ),
    stringsAsFactors = FALSE
  )

  grapho_breakdown <- data.frame(
    Metric = c(
      "Types",
      "Tokens",
      "Share of corpus (%)",
      "Mean frequency (per type)",
      "Median frequency (per type)",
      "Minimum frequency",
      "Maximum frequency"
    ),
    `With grapho` = c(
      format_corpus_count(with_grapho$types),
      format_corpus_count(with_grapho$tokens),
      format(round_pct(100 * with_grapho$tokens / total_tokens), trim = TRUE),
      format_corpus_freq(with_grapho$mean),
      format_corpus_freq(with_grapho$median),
      format_corpus_count(with_grapho$min),
      format_corpus_count(with_grapho$max)
    ),
    `Without grapho` = c(
      format_corpus_count(without_grapho$types),
      format_corpus_count(without_grapho$tokens),
      format(round_pct(100 * without_grapho$tokens / total_tokens), trim = TRUE),
      format_corpus_freq(without_grapho$mean),
      format_corpus_freq(without_grapho$median),
      format_corpus_count(without_grapho$min),
      format_corpus_count(without_grapho$max)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  top_without_grapho <- df %>%
    filter(!has_grapho) %>%
    arrange(desc(frequency), word) %>%
    slice_head(n = 15) %>%
    transmute(
      word,
      frequency,
      norm,
      lemma
    )

  list(
    overview = overview,
    grapho_breakdown = grapho_breakdown,
    top_without_grapho = top_without_grapho
  )
}

get_lemma_variant_words_df <- function(word_data, lemma_value) {
  if (is.null(word_data) || is.null(lemma_value) || lemma_value == "") {
    return(data.frame(
      Word = character(),
      Frequency = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  word_data %>%
    filter(!is.na(lemma), lemma == lemma_value, !is.na(word), word != "") %>%
    transmute(
      Word = word,
      Frequency = frequency,
      norm = if_else(is.na(norm), "", as.character(norm)),
      lemma = if_else(is.na(lemma), "", as.character(lemma)),
      grapho = if_else(is.na(grapho), "", as.character(grapho))
    ) %>%
    arrange(desc(Frequency), Word)
}

# Function to extract unique letters from corpus
extract_unique_letters <- function(word_data) {
  # Combine all words into a single string
  all_text <- paste(word_data$word, collapse = "")
  
  # Normalize Unicode to combine diacritics with their base letters
  normalized_text <- stringi::stri_trans_nfc(all_text)
  
  # Use grapheme clusters to properly handle combining diacritics
  grapheme_clusters <- stringi::stri_extract_all_boundaries(normalized_text, type = "character")[[1]]
  
  # Extract unique grapheme clusters (letters with their diacritics)
  unique_chars <- unique(grapheme_clusters)
  
  # Add diphthongs, digraphs, and trigraphs to the unique characters
  unique_chars <- c(unique_chars, DIPHTHONGS, DIGRAPHS, TRIGRAPHS)
  
  # Convert all letters to lowercase to avoid duplicates
  unique_chars <- tolower(unique_chars)
  
  # Remove duplicates that might have been created by case conversion
  unique_chars <- unique(unique_chars)
  
  # Sort alphabetically
  unique_chars <- sort(unique_chars)
  
  return(unique_chars)
}

# Function to process words and handle diphthongs, digraphs, and trigraphs
process_word_with_combinations <- function(word) {
  # Normalize the word to combine diacritics with their base letters
  normalized_word <- stringi::stri_trans_nfc(word)
  
  # Extract grapheme clusters (proper character boundaries)
  grapheme_clusters <- stringi::stri_extract_all_boundaries(normalized_word, type = "character")[[1]]
  
  # Convert grapheme clusters to a single string for processing
  word_string <- paste(grapheme_clusters, collapse = "")
  
  processed_word <- word_string
  
  # Process trigraphs first (longest patterns first to avoid conflicts)
  for (trigraph in TRIGRAPHS) {
    processed_word <- gsub(trigraph, TRIGRAPH_MARKERS[trigraph], processed_word, fixed = TRUE)
  }
  
  # Process diphthongs
  for (diphthong in DIPHTHONGS) {
    processed_word <- gsub(diphthong, DIPHTHONG_MARKERS[diphthong], processed_word, fixed = TRUE)
  }
  
  # Process digraphs
  for (digraph in DIGRAPHS) {
    processed_word <- gsub(digraph, DIGRAPH_MARKERS[digraph], processed_word, fixed = TRUE)
  }
  
  # Convert back to grapheme clusters
  processed_clusters <- stringi::stri_extract_all_boundaries(processed_word, type = "character")[[1]]
  
  return(processed_clusters)
}

# Function to analyze letter combinations up to 4 letters on each side
analyze_letter_combinations_enhanced <- function(word_data, search_letter, max_context = 4) {
  # Initialize results
  left_combinations <- list()
  right_combinations <- list()
  combined_combinations <- list()
  
  # If search_letter is a diphthong, digraph, or trigraph, convert it to its marker
  if (search_letter %in% DIPHTHONGS) {
    search_letter_marker <- DIPHTHONG_MARKERS[search_letter]
  } else if (search_letter %in% DIGRAPHS) {
    search_letter_marker <- DIGRAPH_MARKERS[search_letter]
  } else if (search_letter %in% TRIGRAPHS) {
    search_letter_marker <- TRIGRAPH_MARKERS[search_letter]
  } else {
    search_letter_marker <- search_letter
  }
  
  # Process each word
  for (i in 1:nrow(word_data)) {
    word <- word_data$word[i]
    freq <- word_data$frequency[i]
    
    # Process the word to handle diphthongs and digraphs
    grapheme_clusters <- process_word_with_combinations(word)
    
    # Find all positions of the search letter in the grapheme clusters
    # Use case-insensitive matching to handle case differences
    search_positions <- which(tolower(grapheme_clusters) == tolower(search_letter_marker))
    
    for (pos in search_positions) {
      # Get left context (up to max_context letters)
      if (pos > 1) {
        for (context_size in 1:min(max_context, pos - 1)) {
          start_pos <- pos - context_size
          end_pos <- pos - 1
          left_context <- paste(grapheme_clusters[start_pos:end_pos], collapse = "")
          if (!is.na(left_context) && left_context != "") {
            # Convert diphthong, digraph, and trigraph markers back to their original form for display
            for (diphthong in DIPHTHONGS) {
              left_context <- gsub(DIPHTHONG_MARKERS[diphthong], diphthong, left_context, fixed = TRUE)
            }
            for (digraph in DIGRAPHS) {
              left_context <- gsub(DIGRAPH_MARKERS[digraph], digraph, left_context, fixed = TRUE)
            }
            for (trigraph in TRIGRAPHS) {
              left_context <- gsub(TRIGRAPH_MARKERS[trigraph], trigraph, left_context, fixed = TRUE)
            }
            key <- left_context
            if (is.null(left_combinations[[key]])) {
              left_combinations[[key]] <- list(
                context = left_context,
                size = context_size,
                type_freq = 0,
                token_freq = 0,
                words = list()
              )
            }
            left_combinations[[key]]$token_freq <- left_combinations[[key]]$token_freq + freq
            left_combinations[[key]]$words[[length(left_combinations[[key]]$words) + 1]] <- 
              list(word = word, frequency = freq)
          }
        }
      } else {
        # Word boundary at start
        key <- "^"
        if (is.null(left_combinations[[key]])) {
          left_combinations[[key]] <- list(
            context = "^",
            size = 0,
            type_freq = 0,
            token_freq = 0,
            words = list()
          )
        }
        left_combinations[[key]]$token_freq <- left_combinations[[key]]$token_freq + freq
        left_combinations[[key]]$words[[length(left_combinations[[key]]$words) + 1]] <- 
          list(word = word, frequency = freq)
      }
      
      # Get right context (up to max_context letters)
      if (pos < length(grapheme_clusters)) {
        for (context_size in 1:min(max_context, length(grapheme_clusters) - pos)) {
          start_pos <- pos + 1
          end_pos <- pos + context_size
          right_context <- paste(grapheme_clusters[start_pos:end_pos], collapse = "")
          if (!is.na(right_context) && right_context != "") {
            # Convert diphthong, digraph, and trigraph markers back to their original form for display
            for (diphthong in DIPHTHONGS) {
              right_context <- gsub(DIPHTHONG_MARKERS[diphthong], diphthong, right_context, fixed = TRUE)
            }
            for (digraph in DIGRAPHS) {
              right_context <- gsub(DIGRAPH_MARKERS[digraph], digraph, right_context, fixed = TRUE)
            }
            for (trigraph in TRIGRAPHS) {
              right_context <- gsub(TRIGRAPH_MARKERS[trigraph], trigraph, right_context, fixed = TRUE)
            }
            key <- right_context
            if (is.null(right_combinations[[key]])) {
              right_combinations[[key]] <- list(
                context = right_context,
                size = context_size,
                type_freq = 0,
                token_freq = 0,
                words = list()
              )
            }
            right_combinations[[key]]$token_freq <- right_combinations[[key]]$token_freq + freq
            right_combinations[[key]]$words[[length(right_combinations[[key]]$words) + 1]] <- 
              list(word = word, frequency = freq)
          }
        }
      } else {
        # Word boundary at end
        key <- "$"
        if (is.null(right_combinations[[key]])) {
          right_combinations[[key]] <- list(
            context = "$",
            size = 0,
            type_freq = 0,
            token_freq = 0,
            words = list()
          )
        }
        right_combinations[[key]]$token_freq <- right_combinations[[key]]$token_freq + freq
        right_combinations[[key]]$words[[length(right_combinations[[key]]$words) + 1]] <- 
          list(word = word, frequency = freq)
      }
      
      # Get combined left-right context
      if (pos > 1 && pos < length(grapheme_clusters)) {
        for (left_size in 1:min(max_context, pos - 1)) {
          for (right_size in 1:min(max_context, length(grapheme_clusters) - pos)) {
            left_start <- pos - left_size
            left_end <- pos - 1
            right_start <- pos + 1
            right_end <- pos + right_size
            
            left_context <- paste(grapheme_clusters[left_start:left_end], collapse = "")
            right_context <- paste(grapheme_clusters[right_start:right_end], collapse = "")
            
            # Convert diphthong, digraph, and trigraph markers back to their original form for display
            for (diphthong in DIPHTHONGS) {
              left_context <- gsub(DIPHTHONG_MARKERS[diphthong], diphthong, left_context, fixed = TRUE)
              right_context <- gsub(DIPHTHONG_MARKERS[diphthong], diphthong, right_context, fixed = TRUE)
            }
            for (digraph in DIGRAPHS) {
              left_context <- gsub(DIGRAPH_MARKERS[digraph], digraph, left_context, fixed = TRUE)
              right_context <- gsub(DIGRAPH_MARKERS[digraph], digraph, right_context, fixed = TRUE)
            }
            for (trigraph in TRIGRAPHS) {
              left_context <- gsub(TRIGRAPH_MARKERS[trigraph], trigraph, left_context, fixed = TRUE)
              right_context <- gsub(TRIGRAPH_MARKERS[trigraph], trigraph, right_context, fixed = TRUE)
            }
            
            if (!is.na(left_context) && left_context != "" && 
                !is.na(right_context) && right_context != "") {
              key <- paste0(left_context, "|", search_letter, "|", right_context)
              if (is.null(combined_combinations[[key]])) {
                combined_combinations[[key]] <- list(
                  left_context = left_context,
                  search_letter = search_letter,
                  right_context = right_context,
                  left_size = left_size,
                  right_size = right_size,
                  type_freq = 0,
                  token_freq = 0,
                  words = list()
                )
              }
              combined_combinations[[key]]$token_freq <- combined_combinations[[key]]$token_freq + freq
              combined_combinations[[key]]$words[[length(combined_combinations[[key]]$words) + 1]] <- 
                list(word = word, frequency = freq)
            }
          }
        }
      } else if (pos == 1 && pos < length(grapheme_clusters)) {
        # Word boundary at start
        for (right_size in 1:min(max_context, length(grapheme_clusters) - pos)) {
          right_start <- pos + 1
          right_end <- pos + right_size
          right_context <- paste(grapheme_clusters[right_start:right_end], collapse = "")
          if (!is.na(right_context) && right_context != "") {
            # Convert diphthong, digraph, and trigraph markers back to their original form for display
            for (diphthong in DIPHTHONGS) {
              right_context <- gsub(DIPHTHONG_MARKERS[diphthong], diphthong, right_context, fixed = TRUE)
            }
            for (digraph in DIGRAPHS) {
              right_context <- gsub(DIGRAPH_MARKERS[digraph], digraph, right_context, fixed = TRUE)
            }
            for (trigraph in TRIGRAPHS) {
              right_context <- gsub(TRIGRAPH_MARKERS[trigraph], trigraph, right_context, fixed = TRUE)
            }
            key <- paste0("^|", search_letter, "|", right_context)
            if (is.null(combined_combinations[[key]])) {
              combined_combinations[[key]] <- list(
                left_context = "^",
                search_letter = search_letter,
                right_context = right_context,
                left_size = 0,
                right_size = right_size,
                type_freq = 0,
                token_freq = 0,
                words = list()
              )
            }
            combined_combinations[[key]]$token_freq <- combined_combinations[[key]]$token_freq + freq
            combined_combinations[[key]]$words[[length(combined_combinations[[key]]$words) + 1]] <- 
              list(word = word, frequency = freq)
          }
        }
      } else if (pos > 1 && pos == length(grapheme_clusters)) {
        # Word boundary at end
        for (left_size in 1:min(max_context, pos - 1)) {
          left_start <- pos - left_size
          left_end <- pos - 1
          left_context <- paste(grapheme_clusters[left_start:left_end], collapse = "")
          if (!is.na(left_context) && left_context != "") {
            # Convert diphthong, digraph, and trigraph markers back to their original form for display
            for (diphthong in DIPHTHONGS) {
              left_context <- gsub(DIPHTHONG_MARKERS[diphthong], diphthong, left_context, fixed = TRUE)
            }
            for (digraph in DIGRAPHS) {
              left_context <- gsub(DIGRAPH_MARKERS[digraph], digraph, left_context, fixed = TRUE)
            }
            for (trigraph in TRIGRAPHS) {
              left_context <- gsub(TRIGRAPH_MARKERS[trigraph], trigraph, left_context, fixed = TRUE)
            }
            key <- paste0(left_context, "|", search_letter, "|$")
            if (is.null(combined_combinations[[key]])) {
              combined_combinations[[key]] <- list(
                left_context = left_context,
                search_letter = search_letter,
                right_context = "$",
                left_size = left_size,
                right_size = 0,
                type_freq = 0,
                token_freq = 0,
                words = list()
              )
            }
            combined_combinations[[key]]$token_freq <- combined_combinations[[key]]$token_freq + freq
            combined_combinations[[key]]$words[[length(combined_combinations[[key]]$words) + 1]] <- 
              list(word = word, frequency = freq)
          }
        }
      } else if (pos == 1 && pos == length(grapheme_clusters)) {
        # Single letter word
        key <- paste0("^|", search_letter, "|$")
        if (is.null(combined_combinations[[key]])) {
          combined_combinations[[key]] <- list(
            left_context = "^",
            search_letter = search_letter,
            right_context = "$",
            left_size = 0,
            right_size = 0,
            type_freq = 0,
            token_freq = 0,
            words = list()
          )
        }
        combined_combinations[[key]]$token_freq <- combined_combinations[[key]]$token_freq + freq
        combined_combinations[[key]]$words[[length(combined_combinations[[key]]$words) + 1]] <- 
          list(word = word, frequency = freq)
      }
    }
  }
  
  # Calculate type frequencies (unique words)
  for (key in names(left_combinations)) {
    unique_words <- unique(sapply(left_combinations[[key]]$words, function(x) x$word))
    left_combinations[[key]]$type_freq <- length(unique_words)
  }
  
  for (key in names(right_combinations)) {
    unique_words <- unique(sapply(right_combinations[[key]]$words, function(x) x$word))
    right_combinations[[key]]$type_freq <- length(unique_words)
  }
  
  for (key in names(combined_combinations)) {
    unique_words <- unique(sapply(combined_combinations[[key]]$words, function(x) x$word))
    combined_combinations[[key]]$type_freq <- length(unique_words)
  }
  
  return(list(
    left = left_combinations, 
    right = right_combinations, 
    combined = combined_combinations
  ))
}

# Function to convert combinations to data frames for display
combinations_to_dataframe <- function(combinations, type, search_letter = NULL) {
  if (length(combinations) == 0) {
    return(data.frame())
  }
  
  if (type == "left") {
    df <- data.frame(
      Context = sapply(combinations, function(x) paste0(x$context, "|", search_letter)),
      Size = sapply(combinations, function(x) x$size),
      Type_Frequency = sapply(combinations, function(x) x$type_freq),
      Token_Frequency = sapply(combinations, function(x) x$token_freq),
      stringsAsFactors = FALSE
    )
  } else if (type == "right") {
    df <- data.frame(
      Context = sapply(combinations, function(x) paste0(search_letter, "|", x$context)),
      Size = sapply(combinations, function(x) x$size),
      Type_Frequency = sapply(combinations, function(x) x$type_freq),
      Token_Frequency = sapply(combinations, function(x) x$token_freq),
      stringsAsFactors = FALSE
    )
  } else if (type == "combined") {
    df <- data.frame(
      Left_Context = sapply(combinations, function(x) x$left_context),
      Right_Context = sapply(combinations, function(x) x$right_context),
      Left_Size = sapply(combinations, function(x) x$left_size),
      Right_Size = sapply(combinations, function(x) x$right_size),
      Type_Frequency = sapply(combinations, function(x) x$type_freq),
      Token_Frequency = sapply(combinations, function(x) x$token_freq),
      stringsAsFactors = FALSE
    )
  }
  
  # Sort by token frequency (descending)
  df <- df[order(df$Token_Frequency, decreasing = TRUE), ]
  
  return(df)
}

# Function to get word examples for a specific combination
get_word_examples <- function(combinations, selected_context, type) {
  if (type == "left") {
    combo <- combinations$left[[selected_context]]
  } else if (type == "right") {
    combo <- combinations$right[[selected_context]]
  } else if (type == "combined") {
    # For combined, we need to find the right key
    for (key in names(combinations$combined)) {
      combo_data <- combinations$combined[[key]]
      if (paste0(combo_data$left_context, "|", combo_data$search_letter, "|", combo_data$right_context) == selected_context) {
        combo <- combo_data
        break
      }
    }
  }
  
  if (is.null(combo) || is.null(combo$words) || length(combo$words) == 0) {
    return(data.frame(Word = character(), Frequency = numeric(), stringsAsFactors = FALSE))
  }
  
  # Create a data frame with words and their actual frequencies
  word_data <- data.frame(
    Word = sapply(combo$words, function(x) x$word),
    Frequency = sapply(combo$words, function(x) x$frequency),
    stringsAsFactors = FALSE
  )
  
  # Aggregate frequencies for duplicate words (if any)
  word_data <- aggregate(Frequency ~ Word, data = word_data, FUN = sum)
  
  # Sort by frequency (descending)
  word_data <- word_data[order(word_data$Frequency, decreasing = TRUE), ]
  
  return(word_data)
}

# Function to search for word context in editioYolanda.txt
get_word_context <- function(selected_word) {
  if (is.null(selected_word) || selected_word == "") {
    return(data.frame(Context = character(), stringsAsFactors = FALSE))
  }
  
  # Read the text file
  text_lines <- readLines("editioYolanda.txt", encoding = "UTF-8")
  
  # Find lines containing the exact word (case-insensitive, word boundaries)
  # Escape special regex characters in the selected_word
  escaped_word <- gsub("([][{}()*+?.\\^$|])", "\\\\\\1", selected_word)
  # Use word boundaries that work with Unicode characters
  # Look for word boundaries: start of line, space, or punctuation before the word
  # and end of line, space, or punctuation after the word
  pattern <- paste0("(^|[[:space:][:punct:]])", escaped_word, "([[:space:][:punct:]]|$)")
  matching_lines <- grep(pattern, text_lines, ignore.case = TRUE, value = FALSE)
  
  # If no matches found with word boundaries, try without boundaries for Unicode characters
  if (length(matching_lines) == 0) {
    # For Unicode characters, try a more flexible approach
    pattern_simple <- paste0("\\b", escaped_word, "\\b")
    matching_lines <- grep(pattern_simple, text_lines, ignore.case = TRUE, value = FALSE)
  }
  
  if (length(matching_lines) == 0) {
    return(data.frame(Context = character(), stringsAsFactors = FALSE))
  }
  
  # Get context for each match (2 lines above and below)
  context_data <- data.frame(
    Context = character(),
    stringsAsFactors = FALSE
  )
  
  for (line_num in matching_lines) {
    # Calculate start and end lines for context
    start_line <- max(1, line_num - 2)
    end_line <- min(length(text_lines), line_num + 2)
    
    # Get context lines
    context_lines <- text_lines[start_line:end_line]
    
    # Create context string with line numbers and proper line breaks
    context_string <- paste(
      sapply(start_line:end_line, function(i) {
        line_text <- context_lines[i - start_line + 1]
        if (i == line_num) {
          # Highlight the selected word with yellow background (exact match, case-insensitive)
          # Handle Unicode characters properly
          # First try with word boundaries
          highlighted_text <- gsub(
            paste0("\\b(", escaped_word, ")\\b"), 
            '<span style="background-color: yellow;">\\1</span>', 
            line_text, 
            ignore.case = TRUE
          )
          
          # If no highlighting occurred, try without word boundaries for Unicode
          if (!grepl('<span style="background-color: yellow;">', highlighted_text, fixed = TRUE)) {
            highlighted_text <- gsub(
              paste0("(", escaped_word, ")"), 
              '<span style="background-color: yellow;">\\1</span>', 
              line_text, 
              ignore.case = TRUE
            )
          }
          sprintf("%d: %s", i, highlighted_text)
        } else {
          sprintf("%d: %s", i, line_text)
        }
      }),
      collapse = "<br>"
    )
    
    # Add to context data
    context_data <- rbind(context_data, data.frame(
      Context = context_string,
      stringsAsFactors = FALSE
    ))
  }
  
  return(context_data)
}

# Function to get all words containing the search letter
get_all_words_with_letter <- function(word_data, search_letter) {
  if (is.null(search_letter) || search_letter == "") {
    return(data.frame(Word = character(), Frequency = numeric(), stringsAsFactors = FALSE))
  }
  
  # Find words containing the search letter
  # Handle diphthongs, digraphs, and trigraphs properly
  # If search_letter is a diphthong, digraph, or trigraph, use exact matching (case-insensitive)
  if (search_letter %in% c(DIPHTHONGS, DIGRAPHS, TRIGRAPHS)) {
    matching_words <- word_data[str_detect(word_data$word, fixed(search_letter, ignore_case = TRUE)), ]
  } else {
    # For single letters, use grapheme-aware matching (case-insensitive)
    # First, process each word to handle diphthongs and digraphs
    matching_indices <- sapply(word_data$word, function(word) {
      grapheme_clusters <- process_word_with_combinations(word)
      any(tolower(grapheme_clusters) == tolower(search_letter))
    })
    matching_words <- word_data[matching_indices, ]
  }
  
  if (nrow(matching_words) == 0) {
    return(data.frame(Word = character(), Frequency = numeric(), stringsAsFactors = FALSE))
  }
  
  # Keep word, frequency, and refkorpus columns
  if (all(c("word","frequency") %in% colnames(matching_words))) {
    # Select all available columns, prioritizing word and frequency first
    available_cols <- colnames(matching_words)
    refkorpus_cols <- intersect(c("norm", "lemma", "grapho"), available_cols)
    core_cols <- c("word", "frequency")
    all_cols <- c(core_cols, refkorpus_cols)
    matching_words <- matching_words[, all_cols, drop = FALSE]
  }
  
  # Sort by frequency (descending)
  matching_words <- matching_words[order(matching_words$frequency, decreasing = TRUE), ]
  
  # Rename columns for consistency (only rename word and frequency)
  colnames(matching_words)[colnames(matching_words) == "word"] <- "Word"
  colnames(matching_words)[colnames(matching_words) == "frequency"] <- "Frequency"
  
  return(matching_words)
}

# Function to find common ending between two words
find_common_ending <- function(word1, word2) {
  # Convert to lowercase for comparison
  w1 <- tolower(word1)
  w2 <- tolower(word2)
  
  # Find the minimum length
  min_len <- min(nchar(w1), nchar(w2))
  
  if (min_len == 0) return("")
  
  # Start from the end and find common characters
  common_ending <- ""
  for (i in 1:min_len) {
    char1 <- substr(w1, nchar(w1) - i + 1, nchar(w1) - i + 1)
    char2 <- substr(w2, nchar(w2) - i + 1, nchar(w2) - i + 1)
    
    if (char1 == char2) {
      common_ending <- paste0(char1, common_ending)
    } else {
      break
    }
  }
  
  return(common_ending)
}

# Function to analyze rhyming words at the end of lines
analyze_rhyming_words <- function(text_file_path) {
  # Read the text file
  text_lines <- readLines(text_file_path, encoding = "UTF-8")
  
  # Initialize results
  rhyme_pairs <- list()
  
  # Process lines in pairs (odd-even)
  for (i in seq(1, length(text_lines) - 1, by = 2)) {
    line1_num <- i
    line2_num <- i + 1
    
    # Skip isolated lines
    if (line1_num %in% ISOLATED_LINES || line2_num %in% ISOLATED_LINES) {
      next
    }
    
    # Get the lines
    line1 <- text_lines[line1_num]
    line2 <- text_lines[line2_num]
    
    # Skip empty lines
    if (line1 == "" || line2 == "") {
      next
    }
    
    # Extract final words (remove trailing ' ·' if present)
    final_word1 <- str_trim(line1)
    final_word2 <- str_trim(line2)
    
    # Remove trailing ' ·' if present
    final_word1 <- str_replace(final_word1, " ·$", "")
    final_word2 <- str_replace(final_word2, " ·$", "")
    
    # Extract the last word from each line
    words1 <- str_split(final_word1, "\\s+")[[1]]
    words2 <- str_split(final_word2, "\\s+")[[1]]
    
    if (length(words1) > 0 && length(words2) > 0) {
      last_word1 <- words1[length(words1)]
      last_word2 <- words2[length(words2)]
      
      # Skip if either word is empty
      if (last_word1 != "" && last_word2 != "") {
        # Calculate the common ending (rhyme)
        common_ending <- find_common_ending(last_word1, last_word2)
        
        # Create a key for the rhyme pair
        pair_key <- paste0(line1_num, "_", line2_num)
        
        rhyme_pairs[[pair_key]] <- list(
          line1_number = line1_num,
          line2_number = line2_num,
          word1 = last_word1,
          word2 = last_word2,
          rhyme = common_ending,
          line1_text = line1,
          line2_text = line2
        )
      }
    }
  }
  
  return(rhyme_pairs)
}

# Function to convert rhyme pairs to data frame
rhyme_pairs_to_dataframe <- function(rhyme_pairs, word_data = NULL) {
  if (length(rhyme_pairs) == 0) {
    return(data.frame())
  }
  
  # Convert to data frame
  df <- data.frame(
    Line1 = sapply(rhyme_pairs, function(x) x$line1_number),
    Line2 = sapply(rhyme_pairs, function(x) x$line2_number),
    Word1 = sapply(rhyme_pairs, function(x) x$word1),
    Word2 = sapply(rhyme_pairs, function(x) x$word2),
    Rhyme = sapply(rhyme_pairs, function(x) x$rhyme),
    Line1_Text = sapply(rhyme_pairs, function(x) x$line1_text),
    Line2_Text = sapply(rhyme_pairs, function(x) x$line2_text),
    stringsAsFactors = FALSE
  )
  
  # Add grapho columns if word_data is provided
  if (!is.null(word_data) && "grapho" %in% colnames(word_data)) {
    # Initialize grapho columns
    df$Word1_Grapho <- ""
    df$Word2_Grapho <- ""
    
    # Look up grapho data for each word
    for (i in 1:nrow(df)) {
      word1 <- df$Word1[i]
      word2 <- df$Word2[i]
      
      # Find grapho for Word1
      word1_match <- word_data[word_data$word == word1, ]
      if (nrow(word1_match) > 0) {
        df$Word1_Grapho[i] <- ifelse(is.na(word1_match$grapho[1]), "", as.character(word1_match$grapho[1]))
      }
      
      # Find grapho for Word2
      word2_match <- word_data[word_data$word == word2, ]
      if (nrow(word2_match) > 0) {
        df$Word2_Grapho[i] <- ifelse(is.na(word2_match$grapho[1]), "", as.character(word2_match$grapho[1]))
      }
    }
  }
  
  # Sort by line numbers
  df <- df[order(df$Line1), ]
  
  return(df)
}

# Summarize unique rhyming word pairs with occurrence counts
rhyme_word_pair_counts <- function(rhyme_pairs) {
  if (length(rhyme_pairs) == 0) {
    return(data.frame(
      Word1 = character(),
      Word2 = character(),
      Rhyme = character(),
      Count = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  rhyme_pairs_to_dataframe(rhyme_pairs) |>
    count(Word1, Word2, Rhyme, name = "Count", sort = TRUE)
}

# Summarize rhyming components (common endings) with occurrence counts
rhyme_component_counts <- function(rhyme_pairs) {
  rhyme_component_counts_from_pairs(rhyme_word_pair_counts(rhyme_pairs))
}

# Summarize rhyming components from edited rhyme pair frequency table
rhyme_component_counts_from_pairs <- function(pair_counts_df) {
  if (is.null(pair_counts_df) || nrow(pair_counts_df) == 0) {
    return(data.frame(
      Rhyme = character(),
      Count = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  raw_counts <- pair_counts_df |>
    count(Rhyme, wt = Count, name = "Count", sort = TRUE)
  
  group_rhyme_component_counts(raw_counts)
}

# Canonical key for grouping graphically equivalent rhyme components
canonical_rhyme_component <- function(rhyme) {
  rhyme |>
    str_replace_all(fixed("\u017f"), "s") |>
    str_replace_all(fixed(Y_GRAPH), "i") |>
    str_replace("ch$", "g") |>
    str_replace("[tdvu]$", "\u00d7")
}

# Group rhyme components that differ only in t/d, v/u, ẏ/i, g/ch, or s/ſ
group_rhyme_component_counts <- function(component_counts_df) {
  if (is.null(component_counts_df) || nrow(component_counts_df) == 0) {
    return(data.frame(
      Rhyme = character(),
      Count = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  component_counts_df |>
    mutate(canonical = canonical_rhyme_component(Rhyme)) |>
    group_by(canonical) |>
    summarise(
      Rhyme = if (n() == 1) {
        first(Rhyme)
      } else {
        paste(sort(unique(Rhyme)), collapse = " / ")
      },
      Count = sum(Count),
      .groups = "drop"
    ) |>
    select(Rhyme, Count) |>
    arrange(desc(Count))
}

build_rhyme_component_publication_table <- function(component_counts_df) {
  empty <- data.frame(
    Reimkomponente = character(),
    n = integer(),
    `Anteil (%)` = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (is.null(component_counts_df) || nrow(component_counts_df) == 0L) {
    return(empty)
  }

  total <- sum(component_counts_df$Count)
  if (total <= 0L) {
    return(empty)
  }

  component_counts_df |>
    mutate(
      Reimkomponente = dplyr::if_else(.data$Rhyme == "", "\u2014", .data$Rhyme),
      n = .data$Count,
      `Anteil (%)` = sprintf("%.1f", 100 * .data$Count / total)
    ) |>
    select(Reimkomponente, n, `Anteil (%)`)
}

split_publication_table_columns <- function(pub_df, n_cols = 3L) {
  empty <- pub_df[0, , drop = FALSE]
  n <- nrow(pub_df)
  if (n == 0L) {
    return(rep(list(empty), n_cols))
  }

  n_per <- as.integer(ceiling(n / n_cols))
  lapply(seq_len(n_cols), function(i) {
    from <- (i - 1L) * n_per + 1L
    to <- min(i * n_per, n)
    if (from > n) {
      empty
    } else {
      pub_df[from:to, , drop = FALSE]
    }
  })
}

render_rhyme_component_publication_html <- function(pub_df, caption_text, n_cols = 3L, revision = 0L) {
  chunks <- split_publication_table_columns(pub_df, n_cols = n_cols)

  column_tables <- lapply(chunks, function(chunk) {
    body_rows <- if (nrow(chunk) == 0L) {
      list()
    } else {
      lapply(seq_len(nrow(chunk)), function(i) {
        htmltools::tags$tr(
          htmltools::tags$td(chunk$Reimkomponente[i], style = "padding: 0 2px;"),
          htmltools::tags$td(
            format(chunk$n[i], big.mark = " "),
            style = "text-align: right; padding: 0 2px;"
          ),
          htmltools::tags$td(chunk$`Anteil (%)`[i], style = "text-align: right; padding: 0 2px;")
        )
      })
    }

    htmltools::tags$div(
      style = "flex: 1 1 0; min-width: 0;",
      htmltools::tags$table(
        class = "rhyme-pub-table",
        style = paste0(
          "width: 100%; font-size: 0.72em; line-height: 1.25; border-collapse: collapse; ",
          "font-variant-numeric: tabular-nums;"
        ),
        htmltools::tags$thead(
          htmltools::tags$tr(
            htmltools::tags$th(
              "Reim",
              style = "text-align: left; font-weight: 600; border-bottom: 1px solid #ccc; padding: 0 2px 2px;"
            ),
            htmltools::tags$th(
              "n",
              style = "text-align: right; font-weight: 600; border-bottom: 1px solid #ccc; padding: 0 2px 2px;"
            ),
            htmltools::tags$th(
              "%",
              style = "text-align: right; font-weight: 600; border-bottom: 1px solid #ccc; padding: 0 2px 2px;"
            )
          )
        ),
        htmltools::tags$tbody(body_rows)
      )
    )
  })

  htmltools::tagList(
    htmltools::tags$div(
      id = paste0("rhyme-pub-rev-", revision),
      style = "display: flex; gap: 16px; align-items: flex-start;",
      column_tables
    ),
    htmltools::tags$p(
      style = paste0(
        "font-size: 0.88em; color: #444; margin: 8px 0 0; ",
        "line-height: 1.45; white-space: normal;"
      ),
      caption_text
    )
  )
}

rhyme_component_publication_caption <- function(total_pairs, grouped_rows, n_cols = 3L) {
  paste0(
    "Endreim-Komponenten (gruppiert): n = ", total_pairs, " Reimpaare; ",
    grouped_rows, " distinkte Komponenten in ", n_cols, " Spalten (absteigend nach H\u00e4ufigkeit). ",
    "Gruppierung: t/d, v/u, \u1e8f/i, g/ch, s/\u017f bei sonst identischem Auslaut."
  )
}

RHYME_PAIR_COUNTS_PATH <- "rhyme_pair_counts.csv"

resolve_app_data_path <- function(filename) {
  if (file.exists(filename)) {
    return(normalizePath(filename, mustWork = FALSE))
  }
  nested <- file.path("Yolanda", filename)
  if (file.exists(nested)) {
    return(normalizePath(nested, mustWork = FALSE))
  }
  if (file.exists("editioYolanda.txt")) {
    return(file.path(getwd(), filename))
  }
  if (file.exists(file.path("Yolanda", "editioYolanda.txt"))) {
    return(file.path(getwd(), nested))
  }
  filename
}

rhyme_pair_counts_path <- function() {
  resolve_app_data_path(RHYME_PAIR_COUNTS_PATH)
}

add_rhyme_pair_row_ids <- function(df, reset = FALSE) {
  if (is.null(df) || nrow(df) == 0L) {
    return(df)
  }
  if (reset || !"._pair_id" %in% names(df) || length(df$._pair_id) != nrow(df)) {
    df <- df |> select(-dplyr::any_of("._pair_id"))
    df$._pair_id <- seq_len(nrow(df))
  }
  df
}

save_rhyme_pair_counts <- function(df, path = rhyme_pair_counts_path()) {
  if (is.null(df) || nrow(df) == 0) {
    return(invisible(NULL))
  }

  df |>
    select(Word1, Word2, Rhyme, Count) |>
    readr::write_csv(path)

  invisible(path)
}

normalize_saved_rhyme <- function(rhyme) {
  if_else(is.na(rhyme), "", as.character(rhyme))
}

apply_saved_rhyme_edits <- function(pair_counts, path = rhyme_pair_counts_path()) {
  if (is.null(pair_counts) || nrow(pair_counts) == 0 || !file.exists(path)) {
    return(pair_counts)
  }
  
  saved <- readr::read_csv(path, show_col_types = FALSE, na = character())
  if (!all(c("Word1", "Word2", "Rhyme") %in% names(saved))) {
    return(pair_counts)
  }
  
  saved_rhymes <- saved |>
    distinct(Word1, Word2, .keep_all = TRUE) |>
    transmute(
      Word1,
      Word2,
      saved_rhyme = normalize_saved_rhyme(Rhyme),
      edited = TRUE
    )
  
  pair_counts |>
    left_join(saved_rhymes, by = c("Word1", "Word2")) |>
    mutate(Rhyme = if_else(!is.na(edited), saved_rhyme, Rhyme)) |>
    select(Word1, Word2, Rhyme, Count) |>
    arrange(desc(Count))
}

load_saved_rhyme_pair_counts <- function(path = rhyme_pair_counts_path()) {
  if (!file.exists(path)) {
    return(NULL)
  }
  
  saved <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, na = character()),
    error = function(e) NULL
  )
  if (is.null(saved) || !all(c("Word1", "Word2", "Rhyme", "Count") %in% names(saved))) {
    return(NULL)
  }
  
  saved |>
    mutate(Rhyme = normalize_saved_rhyme(Rhyme)) |>
    add_rhyme_pair_row_ids(reset = TRUE)
}

parse_dt_cell_edit <- function(info) {
  if (is.null(info)) {
    return(NULL)
  }
  if (is.data.frame(info)) {
    return(info)
  }
  if (is.list(info) && !is.null(info$row)) {
    return(data.frame(
      row = info$row,
      col = info$col,
      value = info$value,
      stringsAsFactors = FALSE
    ))
  }
  NULL
}

# UI definition
ui <- fluidPage(
  titlePanel("Graphematic Analyser for the Ẏoland Manuscript"),
  
  # Add bottom padding so fixed footer does not cover pagination controls
  tags$style(HTML("body { padding-bottom: 70px; }")),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Analysis Settings"),
      
      selectInput("search_letter", 
                  label = "Search Letter:", 
                  choices = NULL,
                  selected = NULL),
      
      sliderInput("max_context", 
                  label = "Maximum Context Size:", 
                  min = 1, 
                  max = 4, 
                  value = 2,
                  step = 1),
      
      actionButton("analyze", "Analyze", class = "btn-primary"),
      actionButton("analyze_rhymes", "Analyze Rhymes", class = "btn-success"),
      
      hr(),
      
      h4("Summary"),
      verbatimTextOutput("summary_stats"),
      
      hr(),
      
      h4("Rhyme Summary"),
      verbatimTextOutput("rhyme_summary"),
      
      hr(),
      
      h4("Legend"),
      p("^ = Word beginning"),
      p("$ = Word ending"),
      p("Click any row in the tables to see word examples")
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        column(
          width = 7,
          tabsetPanel(
            tabPanel("Corpus Information",
                     h3("Corpus Information"),
                     p("Overview of the word list loaded from ",
                       tags$code("word_list_with_refkorpus.csv"),
                       ": ",
                       tags$strong("Sum of words"),
                       " = total token count (each word form counted by its frequency); ",
                       tags$strong("Different words"),
                       " = number of distinct manuscript word forms (types)."),
                     verbatimTextOutput("corpus_info_summary"),
                     DT::dataTableOutput("corpus_info_table"),
                     hr(),
                     h4("Grapho coverage"),
                     p("Frequency statistics by type (each row in the word list counts once; token counts weight by frequency)."),
                     DT::dataTableOutput("corpus_grapho_breakdown_table"),
                     hr(),
                     h4("Most frequent words without grapho"),
                     p("Highest-frequency manuscript forms that still lack a grapho annotation."),
                     DT::dataTableOutput("corpus_without_grapho_table")
            ),

            tabPanel("Left Combinations", 
                     h3("Graphemes on the Left"),
                     p("Click any row to see word examples"),
                     DT::dataTableOutput("left_table")
            ),
            
            tabPanel("Right Combinations", 
                     h3("Graphemes on the Right"),
                     p("Click any row to see word examples"),
                     DT::dataTableOutput("right_table")
            ),
            
            tabPanel("Combined Combinations", 
                     h3("Graphemes Left & Right"),
                     p("Click any row to see word examples"),
                     DT::dataTableOutput("combined_table")
            ),
            
            tabPanel("All Words",
                     h3("All Words with Search Letter"),
                     p("All words containing the search letter with refkorpus data (norm, lemma, grapho). ",
                       "Column filters for word, norm, lemma, and grapho support regex."),
                     DT::dataTableOutput("all_words_table")
            ),
            
            
            tabPanel("Word List Editor",
                     h3("Edit base word list (norm, lemma, grapho columns are editable)"),
                     div(
                       style = "margin-bottom: 15px;",
                       actionButton("save_wordlist_btn", "Save Changes", 
                                  class = "btn-primary",
                                  style = "margin-right: 10px;"),
                       uiOutput("save_status_indicator")
                     ),
                     p("Make changes in the table below, then click 'Save Changes' to persist them. ",
                       "Column filters for word, norm, lemma, and grapho support regex."),
                     DT::DTOutput("wordlist_editor")
            ),
            
                         tabPanel("Rhyme Analysis",
                      h3("Rhyming Words"),
                      p("Analyzes words at the end of lines for rhyming pairs. Every second final word rhymes with the corresponding word in the previous line (Endreim)."),
                      p("Note: Isolated lines and lines ending with ' ·' are ignored."),
                      p("Isolated lines: 4793, 4401, 4255, 3941, 3942, 3817, 3637, 3349, 2961, 1663, 1571, 1443, 1399, 1199, 729, 409"),
                      div(
                        style = "margin-bottom: 15px;",
                        actionButton("analyze_rhymes_standalone", "Analyze Rhymes", 
                                   class = "btn-success",
                                   style = "margin-right: 10px;"),
                        downloadButton("download_rhymes_csv", "Download CSV", 
                                     class = "btn-info",
                                     style = "margin-right: 10px;"),
                        uiOutput("csv_status_indicator")
                      ),
                      h4("Rhyme Pair Frequencies"),
                      p("Each unique pair of rhyming line-final words and how often it occurs. ",
                        "The ", tags$strong("Rhyme"), " column is editable (double-click a cell, edit, then Enter or click outside); ",
                        "edits are saved automatically to ",
                        tags$code(rhyme_pair_counts_path()), "."),
                      DT::dataTableOutput("rhyme_pair_counts_table"),
                      hr(),
                      h4("Rhyming Component Frequencies"),
                      p("The shared rhyming segment (common ending) and how often it appears. ",
                        "Updated automatically from the rhyme pair table above. ",
                        "Variants ending in ", tags$em("t"), " or ", tags$em("d"), ", ",
                        tags$em("v"), " or ", tags$em("u"), ", ",
                        tags$em("g"), " or ", tags$em("ch"), ", and those differing only by ",
                        tags$em("s"), " vs. ", tags$em("ſ"), ", ",
                        tags$em(Y_GRAPH), " vs. ", tags$em("i"), " are grouped when otherwise identical."),
                      DT::dataTableOutput("rhyme_component_counts_table"),
                      hr(),
                      h4("Publikationstabelle (Reimkomponenten)"),
                      p("Kompakte Zeitungsspalten-Ansicht (3 Spalten nebeneinander) mit allen Reimkomponenten, ",
                        "aktualisiert automatisch aus der Reimkomponenten-Tabelle oben."),
                      downloadButton("download_rhyme_component_publication_csv", "Download Publikation CSV",
                                     class = "btn-info", style = "margin-bottom: 10px;"),
                      uiOutput("rhyme_component_publication_table"),
                      hr(),
                      h4("All Rhyme Occurrences"),
                      p("Column filters for Word1, Word2, Rhyme, and grapho support regex (placeholder ",
                        tags$code("regex…"), "). Use ",
                        tags$code("^$"), " in the Rhyme filter for pairs with no common ending. ",
                        "In grapho columns, escape literal pipes, e.g. ",
                        tags$code("\\|u=u\\|"), " not ",
                        tags$code("|u=u|"), " (",
                        tags$code("|"), " means OR in regex). ",
                        "Clear other column filters first if results look wrong."),
                      DT::dataTableOutput("rhyme_pairs_table")
              ),
             
             
             tabPanel("Grapheme Analysis",
                      h3("Complete Graphematical Analysis"),
                      p("Comprehensive analysis of graphemes extracted from the grapho column (element before each = sign in |grapheme=phoneme| pairs)."),
                      div(
                        style = "margin-bottom: 15px;",
                        actionButton("analyze_graphemes", "Analyze Graphemes", 
                                   class = "btn-primary",
                                   style = "margin-right: 10px;"),
                        downloadButton("download_graphemes_csv", "Download CSV", 
                                     class = "btn-info",
                                     style = "margin-right: 10px;"),
                        uiOutput("grapheme_status_indicator")
                      ),
                      h4("Grapheme Distribution"),
                      DT::dataTableOutput("grapheme_distribution_table"),
                      hr(),
                      h4("Vowel Graphemes: Type and Token Frequencies"),
                      uiOutput("vowel_grapheme_freq_table"),
                      hr(),
                      h4("Consonant Graphemes: Type and Token Frequencies"),
                      uiOutput("consonant_grapheme_freq_table"),
                      hr(),
                      h4("Grapheme Frequency Visualization"),
                      plotOutput("grapheme_frequency_plot"),
                      hr(),
                      h4("Grapheme Length Analysis"),
                      plotOutput("grapheme_length_plot"),
                      hr(),
                      h4("Grapheme Position Analysis"),
                      plotOutput("grapheme_position_plot")
              ),
             
             tabPanel("Grapheme-Phoneme Analysis",
                      h3("Grapheme-Phoneme Mapping Analysis"),
                      p("Analysis of grapheme-phoneme relationships extracted from the grapho column (|grapheme=phoneme| format)."),
                      div(
                        style = "margin-bottom: 15px;",
                        actionButton("analyze_grapheme_phoneme", "Analyze Grapheme-Phoneme Mappings", 
                                   class = "btn-primary",
                                   style = "margin-right: 10px;"),
                        downloadButton("download_grapheme_phoneme_csv", "Download CSV", 
                                     class = "btn-info",
                                     style = "margin-right: 10px;"),
                        uiOutput("grapheme_phoneme_status_indicator")
                      ),
                      h4("Phoneme to Grapheme Mappings"),
                      DT::dataTableOutput("phoneme_to_grapheme_table"),
                      hr(),
                      h4("Grapheme to Phoneme Mappings"),
                      p("Format: <phoneme> (tokens/types, token%/type%), percentages within each grapheme."),
                      DT::dataTableOutput("grapheme_to_phoneme_table"),
                      hr(),
                      h4("Short Vowel Phonemes"),
                      p("Distribution of graphemes for short vowel phonemes (i, e, \u00eb, \u00e4, \u00fc, \u00f6, u, o, a). Format: <grapheme> (tokens/types, token%/type%), percentages within each phoneme."),
                      DT::dataTableOutput("short_vowel_table"),
                      hr(),
                      h4("Long Vowel Phonemes"),
                      p("Distribution of graphemes for long vowel phonemes (\u012b, e\u0304, \u00e4\u0304, \u00fc\u0304, \u00f6\u0304, u\u0304, o\u0304, a\u0304). Format: <grapheme> (tokens/types, token%/type%), percentages within each phoneme."),
                      DT::dataTableOutput("long_vowel_table"),
                      hr(),
                      h4("Diphthongs"),
                      p("Distribution of graphemes for diphthongs (ie, ei, \u00fce, ou, \u00f6\u00fc, uo). Format: <grapheme> (tokens/types, token%/type%), percentages within each phoneme."),
                      DT::dataTableOutput("diphthong_table"),
                      hr(),
                      h4("Selected Phonemes: Grapheme Matrix (Type-Based)"),
                      p("Shows grapheme distribution for selected phonemes (ou, ei, ie, a\u0304, a, o\u0304, u\u0304, e\u0304, \u012b, i). Percentages are normalized within each phoneme."),
                      plotOutput("combined_vowel_type_matrix", height = "520px"),
                      hr(),
                      h4("Selected Phonemes: Grapheme Matrix (Token-Based)"),
                      p("Same selected phonemes, weighted by token frequency."),
                      plotOutput("combined_vowel_token_matrix", height = "520px"),
                      hr(),
                      h4("Selected Phonemes: Grapheme Matrix (Type-Based, Column %)"),
                      p("Percentages normalized within each grapheme (column-wise)."),
                      plotOutput("combined_vowel_type_matrix_colwise", height = "520px"),
                      hr(),
                      h4("Selected Phonemes: Grapheme Matrix (Token-Based, Column %)"),
                      p("Token-weighted percentages normalized within each grapheme (column-wise)."),
                      plotOutput("combined_vowel_token_matrix_colwise", height = "520px"),
                      hr(),
                      h4("Focus Summary (Type-Based)"),
                      plotOutput("focus_scatter_type_plot", height = "520px", width = "100%"),
                      DT::dataTableOutput("focus_summary_type_table"),
                      hr(),
                      h4("Mutual Focus Pairs (Type-Based)"),
                      DT::dataTableOutput("mutual_focus_type_table"),
                      hr(),
                      h4("Focus Summary (Token-Based)"),
                      plotOutput("focus_scatter_token_plot", height = "520px", width = "100%"),
                      DT::dataTableOutput("focus_summary_token_table"),
                      hr(),
                      h4("Mutual Focus Pairs (Token-Based)"),
                      DT::dataTableOutput("mutual_focus_token_table")
              ),

              tabPanel("u/v Graphie",
                      h3("Graphie f\u00fcr /u/, /\u00fb/, /uo/ nach Wortposition"),
                      p("Auswertung der ", tags$code("grapho"), "-Spalte f\u00fcr ",
                        tags$code("|v=u|"), ", ", tags$code("|v=\u016b|"), ", ", tags$code("|v=uo|"),
                        " sowie ", tags$code("|u=u|"), ", ", tags$code("|u=\u016b|"), ", ",
                        tags$code("|u=uo|"), " (Phoneme normalisiert aus grapho). Z\u00e4hlung auf ",
                        tags$strong("Lemma-Ebene"), " (nicht token-gewichtet; fehlendes Lemma \u2192 Wortform). ",
                        "Position = Slot im grapho-String: ",
                        tags$strong("initial"), " (erster Graphem-Phonem-Slot), ",
                        tags$strong("final"), " (letzter Slot), sonst ",
                        tags$strong("medial"), "."),
                      div(
                        style = "margin-bottom: 15px;",
                        actionButton("analyze_u_graphie", "Analyse starten",
                                     class = "btn-primary",
                                     style = "margin-right: 10px;"),
                        downloadButton("download_u_graphie_csv", "CSV herunterladen",
                                       class = "btn-info",
                                       style = "margin-right: 10px;"),
                        uiOutput("u_graphie_status_indicator")
                      ),
                      verbatimTextOutput("u_graphie_corpus_summary"),
                      hr(),
                      h4("Lemma-H\u00e4ufigkeiten"),
                      DT::dataTableOutput("u_graphie_distribution_table"),
                      hr(),
                      h4("Visualisierung"),
                      plotOutput("u_graphie_position_plot", height = "420px", width = "100%")
              ),
              
              tabPanel("Grapheme-Phoneme Sankey",
                      h3("Grapheme \u2194 Phoneme Sankey"),
                      p("Horizontal Sankey (",
                        tags$a(href = "https://pepijn-devries.github.io/ggsankeyfier/",
                               target = "_blank", "ggsankeyfier"),
                        "): Grapheme links zu Vokalphonemen (ohne Konsonanten), feste Reihenfolge unten /a/ \u2026 oben /\u00fce/. Breite proportional zur H\u00e4ufigkeit."),
                      div(
                        style = "margin-bottom: 15px;",
                        actionButton("build_sankey", "Build / Refresh Sankey",
                                     class = "btn-primary", style = "margin-right: 10px;"),
                        uiOutput("sankey_status_indicator")
                      ),
                      fluidRow(
                        column(3, radioButtons("sankey_count_mode", "Count by:",
                                               choices = c("Tokens" = "token", "Types" = "type"),
                                               selected = "token")),
                        column(3, numericInput("sankey_min_count", "Minimum link count:",
                                               value = 10, min = 1, step = 1),
                               p(class = "help-block", style = "margin-top: 4px;",
                                 "Per grapheme\u2192phoneme link (not pooled by phoneme).")),
                        column(3, numericInput("sankey_max_links", "Max links shown:",
                                               value = 120, min = 10, step = 10))
                      ),
                      fluidRow(
                        column(3, numericInput("sankey_base_size", "Base font (pt):",
                                               value = 14, min = 8, max = 28, step = 1)),
                        column(3, numericInput("sankey_label_size", "Node label size (mm):",
                                               value = 3.2, min = 2, max = 8, step = 0.2)),
                        column(3, numericInput("sankey_title_size", "Title size (pt):",
                                               value = 16, min = 10, max = 32, step = 1)),
                        column(3, numericInput("sankey_flow_alpha", "Flow opacity:",
                                               value = 0.4, min = 0.1, max = 1, step = 0.05))
                      ),
                      fluidRow(
                        column(3, numericInput("sankey_export_dpi", "PNG dpi:",
                                               value = 300, min = 150, max = 600, step = 50))
                      ),
                      fluidRow(
                        column(3, numericInput("sankey_export_width", "Export width (in):",
                                               value = 16, min = 8, max = 30, step = 1)),
                        column(3, numericInput("sankey_export_height", "Export height (in):",
                                               value = 12, min = 8, max = 30, step = 1)),
                        column(3, downloadButton("download_sankey_png", "Download PNG",
                                                 class = "btn-info")),
                        column(3, downloadButton("download_sankey_pdf", "Download PDF",
                                                 class = "btn-info"))
                      ),
                      plotOutput("grapheme_phoneme_sankey", height = "850px", width = "100%"),
                      hr(),
                      h4("Quantitative Typologie (kombiniert)"),
                      p(tags$strong("N"), " = Typen (Wortformen). Entropie/Dominanz gem\u00e4\u00df Count by. ",
                        "Alle Graphem- und Phonem-Einheiten in einer Tabelle; ",
                        tags$strong("Perspektive"), " kennzeichnet G\u2192P bzw. P\u2192G. ",
                        tags$strong("Kategorien:"), " Biunique (G\u2194P), ",
                        "L\u00f6sungsraum P\u2192G (eindeutig / dominant / diffus), ",
                        "Monophonie (G\u2192P), Polyvalenz G\u2192P (dominant / polyphon). ",
                        "Marginal &lt;5\u202f% ignoriert; Dominanz-Toleranz: \u226580\u202f% ",
                        "oder \u226555\u202f% bei mindestens doppeltem Vorsprung. ",
                        "Mit ", tags$code("e \u2192 /e/"), "; im Sankey oben ausgeblendet."),
                      uiOutput("sankey_typology_summary"),
                      downloadButton("download_sankey_typology_csv", "Download Typologie CSV",
                                     class = "btn-info", style = "margin-bottom: 10px;"),
                      DT::dataTableOutput("sankey_combined_typology_table"),
                      hr(),
                      h4("Publikationstabellen"),
                      uiOutput("sankey_publication_methodology"),
                      downloadButton("download_sankey_publication_csv", "Download Publikation CSV",
                                     class = "btn-info", style = "margin-bottom: 10px;"),
                      tabsetPanel(
                        id = "sankey_publication_tabs",
                        tabPanel(
                          "1 Eineindeutig",
                          br(),
                          tags$p(class = "pub-tab-intro",
                                 tags$strong("Biunique (G\u2194P): "),
                                 "Wechselseitig eindeutiges Kernpaar \u2014 das Graphem mappt produktiv ",
                                 "auf genau ein Phonem und ist dessen Hauptschreibung."),
                          DT::dataTableOutput("sankey_pub_biunique_table")
                        ),
                        tabPanel(
                          "2.1 Lösungsraum eindeutig",
                          br(),
                          tags$p(class = "pub-tab-intro",
                                 tags$strong("L\u00f6sungsraum P\u2192G (eindeutig): "),
                                 "Produktiv genau ein Schreibweg f\u00fcr dieses Phonem (Entropie = 0). ",
                                 "Unabh\u00e4ngig davon, ob das Graphem in Leserichtung (G\u2192P) polyvalent ist ",
                                 "oder von anderen Phonemen mitbenutzt wird (z.\u202fB. /a/ und /a\u0304/ \u2192 \u2329a\u232a)."),
                          DT::dataTableOutput("sankey_pub_eindeutig_p_table")
                        ),
                        tabPanel(
                          "2.2 Monophonie",
                          br(),
                          tags$p(class = "pub-tab-intro",
                                 tags$strong("Monophonie (G\u2192P): "),
                                 "Produktiv genau eine Phonem-Lesung; nicht polyphon. ",
                                 "Muss nicht Biunique sein, wenn das Zielphonem alternative Schreibungen hat."),
                          DT::dataTableOutput("sankey_pub_eindeutig_g_table")
                        ),
                        tabPanel(
                          "3 L\u00f6sungsraum P\u2192G",
                          br(),
                          tags$p(class = "pub-tab-intro",
                                 tags$strong("L\u00f6sungsraum P\u2192G "),
                                 "(Meletis & D\u00fcrscheid 2022): mehrere Grapheme f\u00fcr dasselbe Phonem ",
                                 "(graphematischer L\u00f6sungsraum; nicht Allographie als Variante desselben Graphems)."),
                          h5("3.1 L\u00f6sungsraum P\u2192G (dominant)"),
                          DT::dataTableOutput("sankey_pub_loesungsraum_dom_table"),
                          hr(),
                          h5("3.2 L\u00f6sungsraum P\u2192G (diffus)"),
                          DT::dataTableOutput("sankey_pub_loesungsraum_diffus_table")
                        ),
                        tabPanel(
                          "4 Polyvalenz G\u2192P",
                          br(),
                          tags$p(class = "pub-tab-intro",
                                 tags$strong("Polyvalenz G\u2192P "),
                                 "(graphic polyvalency): ein Graphem, mehrere Phonem-Lesungen."),
                          h5("4.1 Polyvalenz G\u2192P (dominant)"),
                          DT::dataTableOutput("sankey_pub_polyvalenz_dom_table"),
                          hr(),
                          h5("4.2 Polyvalenz G\u2192P (polyphon)"),
                          DT::dataTableOutput("sankey_pub_polyvalenz_polyphon_table")
                        )
                      )
              ),
              
              tabPanel("ẏ/i Analysis",
                      h3("ẏ and i Letter Comparison Analysis"),
                      p("Compares word pairs where 'ẏ' and 'i' can be interchanged. The first column shows ẏ words, the second column shows corresponding i words."),
                      div(
                        style = "margin-bottom: 15px;",
                        actionButton("analyze_y_i", "Analyze ẏ/i Patterns", 
                                   class = "btn-primary",
                                   style = "margin-right: 10px;"),
                        downloadButton("download_y_i_csv", "Download CSV", 
                                     class = "btn-info",
                                     style = "margin-right: 10px;"),
                        uiOutput("y_i_status_indicator")
                      ),
                      p("Word pairs with ẏ and i variants, showing frequencies and grapho data for each variant."),
                      DT::dataTableOutput("y_i_analysis_table"),
                      hr(),
                      h4("Words by lemma"),
                      p("All word forms with standalone ẏ or i, grouped by lemma (entries without lemma are omitted). The ", tags$code("<i>"), " in ", tags$code("<ei>"), ", ", tags$code("<oi>"), ", and ", tags$code("<ui>"), " is ignored. Each form is listed as word (tokens, class) with class ẏ, i, or ẏ+i. ẏ share uses only exclusive ẏ vs i forms (ẏ+i forms are counted separately)."),
                      numericInput("y_i_lemma_min_tokens", "Minimum tokens per lemma:", value = 1, min = 1, step = 1),
                      DT::dataTableOutput("y_i_lemma_table"),
                      hr(),
                      h3("ẏ/i Preference"),
                      p("Scope: only words whose grapho confirms a genuine ẏ/i alternation for the phonemes /i/ or /iè/ are included, i.e. one of the four pairs |ẏ=i| |ẏ=iè| |i=i| |i=iè|. The /î/ phoneme (which is essentially always written <i>) is excluded. Each lexeme's ẏ share is computed from its ẏ and i tokens."),
                      h4("Lexeme-specific distribution (bimodality)"),
                      p("The ẏ/i choice is largely a property of the individual lexeme (the normalized form 'norm'). If lexemes cluster near 0% and 100% rather than in the middle, the choice is lexically conditioned. Raise the minimum token count to focus on reliably-estimated lexemes."),
                      numericInput("y_i_lexeme_min_n", "Minimum tokens per lexeme:", value = 5, min = 1, step = 1),
                      plotOutput("y_i_lexeme_hist", height = "420px", width = "100%"),
                      hr(),
                      h4("Word lists by grapheme preference"),
                      p("Lexemes split by their dominant grapheme (ẏ share above vs below 50%, shrinkage-adjusted). Use the minimum token count above to restrict to reliably-estimated lexemes."),
                      fluidRow(
                        column(6, h5("Prefer ẏ"), DT::dataTableOutput("y_i_pref_y_list")),
                        column(6, h5("Prefer i"), DT::dataTableOutput("y_i_pref_i_list"))
                      ),
                      hr(),
                      h3("Consonant context (/i/, /iè/)"),
                      p("For each consonant before or after the ẏ/i slot: is the vowel written more often as ẏ or as i? Only /i/ and /iè/ are included. Example: if the corpus uses 70% ẏ overall, but after <r> the share is 95% ẏ, then <r> favours ẏ. Points right of the dashed line = more ẏ; left = more i. Green = significantly more ẏ than corpus; purple = significantly more i."),
                      numericInput("y_i_cons_min_tokens", "Minimum tokens per consonant context:",
                                   value = 10, min = 3, step = 1),
                      verbatimTextOutput("y_i_consonant_overview"),
                      fluidRow(
                        column(6,
                               h5("Consonant before ẏ/i (to the left of the vowel)"),
                               p(tags$small("Example: ", tags$code("...rẏ..."), " vs ", tags$code("...ri..."), " — how often is the vowel ẏ vs i when ", tags$em("r"), " precedes?")),
                               plotOutput("y_i_prec_cons_forest", height = "480px", width = "100%")
                        ),
                        column(6,
                               h5("Consonant after ẏ/i (to the right of the vowel)"),
                               p(tags$small("Example: ", tags$code("...ẏt..."), " vs ", tags$code("...it..."), " — how often is the vowel ẏ vs i when ", tags$em("t"), " follows?")),
                               plotOutput("y_i_foll_cons_forest", height = "480px", width = "100%")
                        )
                      ),
                      fluidRow(
                        column(6, DT::dataTableOutput("y_i_prec_cons_table")),
                        column(6, DT::dataTableOutput("y_i_foll_cons_table"))
                      ),
                      hr(),
                      h4("Multivariate models (logistic regression)"),
                      p(tags$em("Random intercept per word type; reference level = first consonant in R's default coding.")),
                      verbatimTextOutput("y_i_prec_cons_model_note"),
                      DT::dataTableOutput("y_i_prec_cons_model_table"),
                      verbatimTextOutput("y_i_foll_cons_model_note"),
                      DT::dataTableOutput("y_i_foll_cons_model_table")
              ),

              tabPanel("s/\u017f Analysis",
                      h3("Round s and long s (\u017f) comparison"),
                      p("Compares word pairs where round ", tags$code("s"), " and long ", tags$code("\u017f"),
                        " can alternate at the same phoneme slot (grapho ", tags$code("|s=s|"), ")."),
                      div(
                        style = "margin-bottom: 15px;",
                        actionButton("analyze_s_longs", "Analyze s/\u017f Patterns",
                                   class = "btn-primary",
                                   style = "margin-right: 10px;"),
                        downloadButton("download_s_longs_csv", "Download CSV",
                                     class = "btn-info",
                                     style = "margin-right: 10px;"),
                        uiOutput("s_longs_status_indicator")
                      ),
                      verbatimTextOutput("s_longs_corpus_summary"),
                      p("Word pairs with \u017f and s variants, showing token frequencies and grapho data."),
                      DT::dataTableOutput("s_longs_analysis_table"),
                      hr(),
                      h3("\u017f/s preference"),
                      p("Scope: word types whose grapho confirms phoneme /s/ written with grapheme ", tags$code("s"),
                        " (", tags$code("|s=s|"), "). Each lexeme's long-s share is computed from attested \u017f and s word forms."),
                      h4("Lexeme-specific distribution"),
                      p("If lexemes cluster near 0% and 100% rather than in the middle, the choice is lexically conditioned."),
                      numericInput("s_longs_lexeme_min_n", "Minimum tokens per lexeme:", value = 5, min = 1, step = 1),
                      plotOutput("s_longs_lexeme_hist", height = "420px", width = "100%"),
                      hr(),
                      h4("Position in the word"),
                      p("Share of \u017f vs round s by position of the contested letter (initial / medial / final)."),
                      plotOutput("s_longs_position_plot", height = "360px", width = "100%"),
                      DT::dataTableOutput("s_longs_position_table"),
                      hr(),
                      h4("Word lists by grapheme preference"),
                      fluidRow(
                        column(6, h5("Prefer \u017f"), DT::dataTableOutput("s_longs_pref_longs_list")),
                        column(6, h5("Prefer s"), DT::dataTableOutput("s_longs_pref_round_list"))
                      ),
                      hr(),
                      h3("Consonant context (/s/)"),
                      p("For each consonant before or after the s/\u017f slot: is /s/ written more often as \u017f or as round s?"),
                      numericInput("s_longs_cons_min_tokens", "Minimum tokens per consonant context:",
                                   value = 10, min = 3, step = 1),
                      verbatimTextOutput("s_longs_consonant_overview"),
                      fluidRow(
                        column(6,
                               h5("Consonant before s/\u017f"),
                               plotOutput("s_longs_prec_cons_forest", height = "480px", width = "100%")
                        ),
                        column(6,
                               h5("Consonant after s/\u017f"),
                               plotOutput("s_longs_foll_cons_forest", height = "480px", width = "100%")
                        )
                      ),
                      fluidRow(
                        column(6, DT::dataTableOutput("s_longs_prec_cons_table")),
                        column(6, DT::dataTableOutput("s_longs_foll_cons_table"))
                      )
              ),

              tabPanel("Varianten",
                      h3("Varianten"),
                      p("Spelling variants in the manuscript: for each lemma, all attested word forms with token frequencies, e.g. ", tags$code("zvei (4), zvein (3)"), " for lemma ", tags$code("zwêin"), "."),
                      verbatimTextOutput("varianten_summary"),
                      DT::dataTableOutput("varianten_table")
              )
          )
        ),
        
        column(
          width = 5,
          h3("Words"),
          verbatimTextOutput("selected_context_info"),
          hr(),
          DT::dataTableOutput("word_examples_table"),
          hr(),
          h4("Text Context"),
          p("Click a word above to see its context in the original text"),
          verbatimTextOutput("selected_word_info"),
          DT::dataTableOutput("context_table")
        )
      )
    )
  ),
  
  # Footer with copyright
  tags$footer(
    style = "position: fixed; bottom: 0; width: 100%; background-color: #f8f9fa; border-top: 1px solid #dee2e6; padding: 10px; text-align: center; font-size: 12px; color: #6c757d;",
    "© 2025 Peter Gilles. All rights reserved."
  )
)

# Grapheme Analysis Functions
parse_grapho_pairs <- function(word, grapho) {
  if (is.na(grapho) || grapho == "") {
    return(data.frame(
      grapheme = character(),
      phoneme = character(),
      stringsAsFactors = FALSE
    ))
  }

  grapho_pairs <- str_extract_all(grapho, "[^|]+")[[1]]
  word_contains_long_s <- grepl("ſ", word, fixed = TRUE)
  graphemes <- character()
  phonemes <- character()

  for (pair in grapho_pairs) {
    if (!grepl("=", pair, fixed = TRUE)) next
    parts <- strsplit(pair, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) next
    grapheme <- trimws(parts[1])
    phoneme <- trimws(parts[2])
    if (grapheme == "" || grapheme == "-" || phoneme == "") next

    grapheme <- normalize_y_grapheme(grapheme)
    if (word_contains_long_s && grapheme == "s") {
      grapheme <- "ſ"
    }
    phoneme <- normalize_phoneme(phoneme)

    graphemes <- c(graphemes, grapheme)
    phonemes <- c(phonemes, phoneme)
  }

  data.frame(grapheme = graphemes, phoneme = phonemes, stringsAsFactors = FALSE)
}

parse_grapho_graphemes <- function(word, grapho) {
  parse_grapho_pairs(word, grapho)$grapheme
}

analyze_graphemes_comprehensive <- function(word_data) {
  # Extract all graphemes from the grapho column (element before '=')
  all_graphemes <- character(0)
  grapheme_positions <- list()
  grapheme_word_context <- list()

  if (!"grapho" %in% colnames(word_data)) {
    return(list(
      stats = data.frame(
        grapheme = character(),
        total_frequency = numeric(),
        unique_words = numeric(),
        avg_position = numeric(),
        position_variance = numeric(),
        word_length_avg = numeric(),
        stringsAsFactors = FALSE
      ),
      positions = list(),
      contexts = list()
    ))
  }

  for (i in 1:nrow(word_data)) {
    word <- word_data$word[i]
    freq <- word_data$frequency[i]
    grapheme_clusters <- parse_grapho_graphemes(word, word_data$grapho[i])
    if (length(grapheme_clusters) == 0) next

    # Store each grapheme with its position and context
    for (pos in 1:length(grapheme_clusters)) {
      grapheme <- grapheme_clusters[pos]
      all_graphemes <- c(all_graphemes, grapheme)

      # Store position information
      if (!grapheme %in% names(grapheme_positions)) {
        grapheme_positions[[grapheme]] <- numeric(0)
        grapheme_word_context[[grapheme]] <- list()
      }

      grapheme_positions[[grapheme]] <- c(grapheme_positions[[grapheme]], pos)
      grapheme_word_context[[grapheme]] <- c(grapheme_word_context[[grapheme]],
                                           list(list(word = word, position = pos, frequency = freq)))
    }
  }
  
  # Calculate statistics for each grapheme
  grapheme_stats <- data.frame(
    grapheme = character(),
    total_frequency = numeric(),
    unique_words = numeric(),
    avg_position = numeric(),
    position_variance = numeric(),
    word_length_avg = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (grapheme in unique(all_graphemes)) {
    positions <- grapheme_positions[[grapheme]]
    contexts <- grapheme_word_context[[grapheme]]
    
    # Calculate total frequency (sum of word frequencies)
    total_freq <- sum(sapply(contexts, function(x) x$frequency))
    
    # Count unique words
    unique_words <- length(unique(sapply(contexts, function(x) x$word)))
    
    # Calculate average position (1-based)
    avg_pos <- mean(positions)
    
    # Calculate position variance
    pos_variance <- var(positions)
    
    # Calculate average word length for this grapheme
    word_lengths <- sapply(contexts, function(x) nchar(x$word))
    avg_word_length <- mean(word_lengths)
    
    grapheme_stats <- rbind(grapheme_stats, data.frame(
      grapheme = grapheme,
      total_frequency = total_freq,
      unique_words = unique_words,
      avg_position = avg_pos,
      position_variance = pos_variance,
      word_length_avg = avg_word_length,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by total frequency
  grapheme_stats <- grapheme_stats[order(grapheme_stats$total_frequency, decreasing = TRUE), ]
  
  return(list(
    stats = grapheme_stats,
    positions = grapheme_positions,
    contexts = grapheme_word_context
  ))
}

# Function to create grapheme frequency plot
create_grapheme_frequency_plot <- function(grapheme_stats) {
  # Take top 30 graphemes for readability
  top_graphemes <- head(grapheme_stats, 30)
  
  ggplot(top_graphemes, aes(x = reorder(grapheme, total_frequency), y = total_frequency)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    labs(
      title = "Top 30 Graphemes by Frequency",
      x = "Grapheme",
      y = "Total Frequency"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold")
    )
}

# Function to create grapheme length analysis plot
create_grapheme_length_plot <- function(grapheme_stats) {
  # Add grapheme length column
  grapheme_stats$length <- nchar(grapheme_stats$grapheme)
  
  ggplot(grapheme_stats, aes(x = factor(length), y = total_frequency)) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.6) +
    labs(
      title = "Grapheme Frequency by Length",
      x = "Grapheme Length",
      y = "Total Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold")
    )
}

# Function to create grapheme position analysis plot
create_grapheme_position_plot <- function(grapheme_stats) {
  # Take top 20 graphemes for readability
  top_graphemes <- head(grapheme_stats, 20)
  
  ggplot(top_graphemes, aes(x = reorder(grapheme, avg_position), y = avg_position)) +
    geom_point(size = 3, color = "red", alpha = 0.7) +
    geom_errorbar(aes(ymin = avg_position - sqrt(position_variance), 
                      ymax = avg_position + sqrt(position_variance)), 
                  width = 0.2, color = "red", alpha = 0.5) +
    coord_flip() +
    labs(
      title = "Grapheme Position Analysis (Top 20)",
      x = "Grapheme",
      y = "Average Position in Word"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold")
    )
}

# u/v grapheme spellings for phonemes u, ū, uo (from grapho |grapheme=phoneme| pairs)
U_GRAPHIE_TARGET_PHONEMES <- c("u", PHONEME_u_bar, "uo")
U_GRAPHIE_TARGET_GRAPHEMES <- c("v", "u")
U_GRAPHIE_POSITION_LEVELS <- c("initial", "medial", "final")

grapho_slot_position_label <- function(slot_index, n_slots) {
  if (n_slots <= 0L) {
    return(NA_character_)
  }
  if (slot_index == 1L) {
    "initial"
  } else if (slot_index == n_slots) {
    "final"
  } else {
    "medial"
  }
}

u_graphie_lemma_id <- function(word_data, row_index) {
  if ("lemma" %in% colnames(word_data)) {
    lemma <- word_data$lemma[row_index]
    if (!is.na(lemma) && as.character(lemma) != "") {
      return(as.character(lemma))
    }
  }
  as.character(word_data$word[row_index])
}

analyze_u_graphie_position <- function(word_data) {
  empty_stats <- data.frame(
    grapheme = character(),
    phoneme = character(),
    position = character(),
    lemmas = integer(),
    word_types = integer(),
    lemma_pct_in_grapheme_position = numeric(),
    stringsAsFactors = FALSE
  )

  if (is.null(word_data) || nrow(word_data) == 0 || !"grapho" %in% colnames(word_data)) {
    return(list(
      stats = empty_stats,
      total_lemma_hits = 0L,
      total_unique_lemmas = 0L
    ))
  }

  bucket_lemmas <- list()
  bucket_words <- list()
  grapheme_position_lemmas <- list()

  for (i in seq_len(nrow(word_data))) {
    word <- word_data$word[i]
    lemma <- u_graphie_lemma_id(word_data, i)
    pairs <- parse_grapho_pairs(word, word_data$grapho[i])
    if (nrow(pairs) == 0) {
      next
    }

    n_slots <- nrow(pairs)
    for (j in seq_len(n_slots)) {
      gr <- pairs$grapheme[j]
      ph <- pairs$phoneme[j]
      if (!(gr %in% U_GRAPHIE_TARGET_GRAPHEMES) || !(ph %in% U_GRAPHIE_TARGET_PHONEMES)) {
        next
      }

      pos <- grapho_slot_position_label(j, n_slots)
      key <- paste(gr, ph, pos, sep = "\x1f")
      bucket_lemmas[[key]] <- unique(c(bucket_lemmas[[key]], lemma))
      bucket_words[[key]] <- unique(c(bucket_words[[key]], word))

      gp_key <- paste(gr, pos, sep = "\x1f")
      grapheme_position_lemmas[[gp_key]] <- unique(c(grapheme_position_lemmas[[gp_key]], lemma))
    }
  }

  if (length(bucket_lemmas) == 0) {
    return(list(
      stats = empty_stats,
      total_lemma_hits = 0L,
      total_unique_lemmas = 0L
    ))
  }

  keys <- names(bucket_lemmas)
  parts <- strsplit(keys, "\x1f", fixed = TRUE)
  stats <- data.frame(
    grapheme = vapply(parts, `[[`, character(1), 1),
    phoneme = vapply(parts, `[[`, character(1), 2),
    position = vapply(parts, `[[`, character(1), 3),
    lemmas = vapply(bucket_lemmas, length, integer(1)),
    word_types = vapply(bucket_words, length, integer(1)),
    stringsAsFactors = FALSE
  )
  stats$position <- factor(stats$position, levels = U_GRAPHIE_POSITION_LEVELS)

  gp_lemma_totals <- vapply(
    names(grapheme_position_lemmas),
    function(k) length(grapheme_position_lemmas[[k]]),
    integer(1)
  )

  stats$lemma_pct_in_grapheme_position <- NA_real_
  for (i in seq_len(nrow(stats))) {
    gp_key <- paste(stats$grapheme[i], stats$position[i], sep = "\x1f")
    gp_lemmas <- gp_lemma_totals[[gp_key]]
    stats$lemma_pct_in_grapheme_position[i] <- round_pct(100 * stats$lemmas[i] / gp_lemmas)
  }

  stats <- stats[order(stats$grapheme, stats$position, stats$phoneme), , drop = FALSE]
  list(
    stats = stats,
    total_lemma_hits = sum(stats$lemmas),
    total_unique_lemmas = length(unique(unlist(bucket_lemmas)))
  )
}

create_u_graphie_position_plot <- function(stats, title_suffix = NULL) {
  if (nrow(stats) == 0) {
    return(ggplot() +
      annotate("text", x = 0, y = 0, label = "No |v/u = u, \u00fb, uo| mappings in grapho") +
      theme_void())
  }

  plot_df <- stats
  plot_df$phoneme_label <- paste0("/", plot_df$phoneme, "/")
  plot_df$grapheme_label <- ifelse(
    plot_df$grapheme == "v",
    "Graphem v",
    "Graphem u"
  )

  subtitle <- "Anteil der Lemmas innerhalb von Graphem \u00d7 Position (Summe = 100%)"
  if (!is.null(title_suffix) && nzchar(title_suffix)) {
    subtitle <- paste0(subtitle, " \u2014 ", title_suffix)
  }

  ggplot(plot_df, aes(x = position, y = lemmas, fill = phoneme_label)) +
    geom_col(position = position_dodge(width = 0.85), width = 0.75) +
    geom_text(
      aes(label = paste0(lemma_pct_in_grapheme_position, "%")),
      position = position_dodge(width = 0.85),
      vjust = -0.35,
      size = 3
    ) +
    facet_wrap(~grapheme_label, ncol = 2) +
    scale_x_discrete(limits = U_GRAPHIE_POSITION_LEVELS) +
    labs(
      title = "Graphien f\u00fcr /u/, /\u00fb/, /uo/: Verteilung nach Wortposition",
      subtitle = subtitle,
      x = "Position im grapho-String (initial / medial / final)",
      y = "Anzahl Lemmas",
      fill = "Phonem"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Grapheme-Phoneme Analysis Functions
analyze_grapheme_phoneme_mappings <- function(word_data) {
  mappings <- data.frame(
    grapheme = character(),
    phoneme = character(),
    word = character(),
    frequency = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(word_data)) {
    word <- word_data$word[i]
    freq <- word_data$frequency[i]
    grapho <- if ("grapho" %in% colnames(word_data)) word_data$grapho[i] else ""
    pairs <- parse_grapho_pairs(word, grapho)
    if (nrow(pairs) == 0) next

    mappings <- rbind(
      mappings,
      data.frame(
        grapheme = pairs$grapheme,
        phoneme = pairs$phoneme,
        word = word,
        frequency = freq,
        stringsAsFactors = FALSE
      )
    )
  }

  return(mappings)
}

# Function to analyze phonemes mapping to same grapheme (many-to-one relationship)
analyze_phoneme_to_grapheme <- function(mappings) {
  # Group by phoneme and find all graphemes that map to it
  phoneme_groups <- split(mappings, mappings$phoneme)
  
  phoneme_to_grapheme_stats <- data.frame(
    phoneme = character(),
    grapheme_count = numeric(),
    graphemes = character(),
    total_frequency = numeric(),
    unique_words = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (phoneme in names(phoneme_groups)) {
    group <- phoneme_groups[[phoneme]]
    unique_graphemes <- unique(group$grapheme)
    total_freq <- sum(group$frequency)
    unique_words <- length(unique(group$word))
    
    # Calculate token and type frequencies for each grapheme and sort by frequency
    grapheme_freq_data <- sapply(unique_graphemes, function(g) {
      g_data <- group[group$grapheme == g, ]
      token_freq <- sum(g_data$frequency)
      type_freq <- nrow(g_data)
      list(grapheme = g, token_freq = token_freq, type_freq = type_freq)
    }, simplify = FALSE)
    
    # Sort by token frequency (descending)
    grapheme_freq_data <- grapheme_freq_data[order(sapply(grapheme_freq_data, function(x) x$token_freq), decreasing = TRUE)]
    
    grapheme_freqs <- sapply(grapheme_freq_data, function(x) {
      paste0(x$grapheme, " (", x$token_freq, "/", x$type_freq, ")")
    })
    
    phoneme_to_grapheme_stats <- rbind(phoneme_to_grapheme_stats, data.frame(
      phoneme = phoneme,
      grapheme_count = length(unique_graphemes),
      graphemes = paste(grapheme_freqs, collapse = ", "),
      total_frequency = total_freq,
      unique_words = unique_words,
      stringsAsFactors = FALSE
    ))
  }
  
  # Filter out phonemes with total frequency < 3
  phoneme_to_grapheme_stats <- phoneme_to_grapheme_stats[phoneme_to_grapheme_stats$total_frequency >= 3, ]
  
  # Sort by grapheme count (descending) then by total frequency
  phoneme_to_grapheme_stats <- phoneme_to_grapheme_stats[order(
    phoneme_to_grapheme_stats$grapheme_count, 
    phoneme_to_grapheme_stats$total_frequency, 
    decreasing = TRUE
  ), ]
  
  return(phoneme_to_grapheme_stats)
}

# Function to analyze graphemes mapping to same phoneme (one-to-many relationship)
analyze_grapheme_to_phoneme <- function(mappings) {
  # Group by grapheme and find all phonemes that map to it
  grapheme_groups <- split(mappings, mappings$grapheme)
  
  grapheme_to_phoneme_stats <- data.frame(
    grapheme = character(),
    phoneme_count = numeric(),
    phonemes = character(),
    total_frequency = numeric(),
    unique_words = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (grapheme in names(grapheme_groups)) {
    group <- grapheme_groups[[grapheme]]
    unique_phonemes <- unique(group$phoneme)
    total_freq <- sum(group$frequency)
    unique_words <- length(unique(group$word))
    
    # Filter out phonemes with total frequency less than 2
    significant_phonemes <- character(0)
    phoneme_freqs <- character(0)
    
    # Calculate frequencies for all phonemes first
    phoneme_freq_data <- list()
    for (p in unique_phonemes) {
      p_data <- group[group$phoneme == p, ]
      token_freq <- sum(p_data$frequency)
      
      if (token_freq >= 2) {
        type_freq <- nrow(p_data)
        phoneme_freq_data[[p]] <- list(phoneme = p, token_freq = token_freq, type_freq = type_freq)
      }
    }
    
    # Sort by token frequency (descending)
    if (length(phoneme_freq_data) > 0) {
      phoneme_freq_data <- phoneme_freq_data[order(sapply(phoneme_freq_data, function(x) x$token_freq), decreasing = TRUE)]
      token_total <- sum(vapply(phoneme_freq_data, function(x) x$token_freq, numeric(1)))
      type_total <- sum(vapply(phoneme_freq_data, function(x) x$type_freq, numeric(1)))

      significant_phonemes <- vapply(phoneme_freq_data, function(x) x$phoneme, character(1))
      phoneme_freqs <- vapply(phoneme_freq_data, function(x) {
        token_pct <- if (token_total > 0) round_pct(100 * x$token_freq / token_total) else 0
        type_pct <- if (type_total > 0) round_pct(100 * x$type_freq / type_total) else 0
        paste0(
          x$phoneme, " (", x$token_freq, "/", x$type_freq,
          ", ", format_pct(token_pct), "/", format_pct(type_pct), ")"
        )
      }, character(1))
    }
    
    # Only include graphemes that have at least one significant phoneme
    if (length(significant_phonemes) > 0) {
      grapheme_to_phoneme_stats <- rbind(grapheme_to_phoneme_stats, data.frame(
        grapheme = grapheme,
        phoneme_count = length(significant_phonemes),
        phonemes = paste(phoneme_freqs, collapse = ", "),
        total_frequency = total_freq,
        unique_words = unique_words,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Filter out graphemes with total frequency < 3
  grapheme_to_phoneme_stats <- grapheme_to_phoneme_stats[grapheme_to_phoneme_stats$total_frequency >= 3, ]
  
  # Sort by phoneme count (descending) then by total frequency
  grapheme_to_phoneme_stats <- grapheme_to_phoneme_stats[order(
    grapheme_to_phoneme_stats$phoneme_count, 
    grapheme_to_phoneme_stats$total_frequency, 
    decreasing = TRUE
  ), ]
  
  return(grapheme_to_phoneme_stats)
}

# Function to create grapheme-phoneme network plot
create_grapheme_phoneme_network_plot <- function(mappings) {
  # Create a simplified network visualization
  # Count unique grapheme-phoneme pairs
  pair_counts <- mappings %>%
    group_by(grapheme, phoneme) %>%
    summarise(frequency = sum(frequency), .groups = 'drop') %>%
    arrange(desc(frequency))
  
  # Take top 30 pairs for readability
  top_pairs <- head(pair_counts, 30)
  
  ggplot(top_pairs, aes(x = reorder(paste(grapheme, "→", phoneme), frequency), y = frequency)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    labs(
      title = "Top 30 Grapheme-Phoneme Mappings by Frequency",
      x = "Grapheme → Phoneme",
      y = "Total Frequency"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold")
    )
}

# Function to create mapping frequency analysis plot
create_mapping_frequency_plot <- function(grapheme_to_phoneme_stats, phoneme_to_grapheme_stats) {
  # Create side-by-side comparison
  grapheme_data <- data.frame(
    type = "Graphemes per Phoneme",
    count = grapheme_to_phoneme_stats$grapheme_count,
    stringsAsFactors = FALSE
  )
  
  phoneme_data <- data.frame(
    type = "Phonemes per Grapheme", 
    count = phoneme_to_grapheme_stats$phoneme_count,
    stringsAsFactors = FALSE
  )
  
  combined_data <- rbind(grapheme_data, phoneme_data)
  
  ggplot(combined_data, aes(x = count, fill = type)) +
    geom_histogram(binwidth = 1, alpha = 0.7, position = "dodge") +
    labs(
      title = "Distribution of Mapping Complexity",
      x = "Number of Mappings",
      y = "Count",
      fill = "Mapping Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    ) +
    scale_fill_manual(values = c("Graphemes per Phoneme" = "steelblue", 
                                "Phonemes per Grapheme" = "darkorange"))
}

# Vowel Analysis Functions
get_vowel_categories <- function() {
  list(
    short_vowels = c("i", "e", "\u00eb", "\u00e4", "\u00fc", "\u00f6", "u", "o", "a"),
    long_vowels = c(
      PHONEME_i_bar, PHONEME_e_bar, PHONEME_a_umlaut_bar, PHONEME_u_umlaut_bar,
      PHONEME_o_umlaut_bar, PHONEME_u_bar, PHONEME_o_bar, PHONEME_a_bar
    ),
    diphthongs = c("ie", "ei", PHONEME_ue, "ou", PHONEME_ou_umlaut, "uo")
  )
}

get_focus_graphemes <- function() {
  c("i", Y_GRAPH, "e", "a", "ai", "o", "oi")
}

get_focus_phonemes <- function() {
  unique(c(
    "ou", "ei", "ie", PHONEME_a_bar, "a", PHONEME_o_bar, PHONEME_u_bar, PHONEME_e_bar,
    PHONEME_i_bar, "i", "u", "o", "uo", PHONEME_a_umlaut_bar, "e", "\u00e4", "\u00eb"
  ))
}

format_vowel_distribution_pct <- function(x) {
  format_pct(x)
}

create_vowel_tables <- function(mappings) {
  vowel_categories <- get_vowel_categories()
  
  # Function to create a vowel table
  create_vowel_table <- function(vowel_list, mappings) {
    vowel_data <- data.frame(
      Phoneme = character(),
      Graphemes = character(),
      stringsAsFactors = FALSE
    )
    
    for (vowel in vowel_list) {
      # Find all graphemes that map to this vowel
      vowel_mappings <- mappings[mappings$phoneme == vowel, ]
      
      if (nrow(vowel_mappings) > 0) {
        # Group by grapheme and calculate frequencies
        grapheme_freqs <- vowel_mappings %>%
          group_by(grapheme) %>%
          summarise(
            token_freq = sum(frequency),
            type_freq = dplyr::n_distinct(word),
            .groups = "drop"
          ) %>%
          filter(token_freq >= 2) %>%
          arrange(desc(token_freq))

        if (nrow(grapheme_freqs) > 0) {
          token_total <- sum(grapheme_freqs$token_freq)
          type_total <- sum(grapheme_freqs$type_freq)
          grapheme_list <- vapply(seq_len(nrow(grapheme_freqs)), function(i) {
            g <- grapheme_freqs$grapheme[i]
            token_freq <- grapheme_freqs$token_freq[i]
            type_freq <- grapheme_freqs$type_freq[i]
            token_pct <- if (token_total > 0) {
              round_pct(100 * token_freq / token_total)
            } else {
              0
            }
            type_pct <- if (type_total > 0) {
              round_pct(100 * type_freq / type_total)
            } else {
              0
            }
            paste0(
              "<", g, "> (", token_freq, "/", type_freq,
              ", ", format_vowel_distribution_pct(token_pct),
              "/", format_vowel_distribution_pct(type_pct), ")"
            )
          }, character(1))
          
          vowel_data <- rbind(vowel_data, data.frame(
            Phoneme = paste0("/", vowel, "/"),
            Graphemes = paste(grapheme_list, collapse = ", "),
            stringsAsFactors = FALSE
          ))
        } else {
          # No graphemes with frequency >= 3 for this vowel
          vowel_data <- rbind(vowel_data, data.frame(
            Phoneme = paste0("/", vowel, "/"),
            Graphemes = "---",
            stringsAsFactors = FALSE
          ))
        }
      } else {
        # No mappings found for this vowel
        vowel_data <- rbind(vowel_data, data.frame(
          Phoneme = paste0("/", vowel, "/"),
          Graphemes = "---",
          stringsAsFactors = FALSE
        ))
      }
    }
    
    return(vowel_data)
  }
  
  return(list(
    short_vowels = create_vowel_table(vowel_categories$short_vowels, mappings),
    long_vowels = create_vowel_table(vowel_categories$long_vowels, mappings),
    diphthongs = create_vowel_table(vowel_categories$diphthongs, mappings)
  ))
}

create_vowel_confusion_matrix_data <- function(mappings, vowel_list, focus_graphemes = NULL, count_mode = c("token", "type"), normalize_by = c("phoneme", "grapheme")) {
  count_mode <- match.arg(count_mode)
  normalize_by <- match.arg(normalize_by)
  vowel_list <- unique(vowel_list)
  
  filtered_mappings <- mappings |> filter(phoneme %in% vowel_list)
  if (!is.null(focus_graphemes)) {
    filtered_mappings <- filtered_mappings |> filter(grapheme %in% focus_graphemes)
  }
  if (nrow(filtered_mappings) == 0) {
    return(data.frame())
  }
  
  pair_counts_base <- filtered_mappings |>
    group_by(phoneme, grapheme) |>
    summarise(
      token_count = sum(frequency),
      type_count = n(),
      .groups = "drop"
    )
  
  # Keep consistency with vowel tables by excluding very rare mappings.
  pair_counts <- pair_counts_base |>
    filter(token_count >= 2) |>
    mutate(count = if (count_mode == "token") token_count else type_count)
  
  if (nrow(pair_counts) == 0) {
    return(data.frame())
  }
  
  if (is.null(focus_graphemes)) {
    grapheme_order <- pair_counts |>
      group_by(grapheme) |>
      summarise(total = sum(count), .groups = "drop") |>
      arrange(desc(total)) |>
      pull(grapheme)
  } else {
    grapheme_order <- focus_graphemes
  }
  
  complete_pairs <- expand.grid(
    phoneme = vowel_list,
    grapheme = grapheme_order,
    stringsAsFactors = FALSE
  ) |>
    left_join(pair_counts, by = c("phoneme", "grapheme")) |>
    mutate(count = ifelse(is.na(count), 0, count))
  
  if (normalize_by == "phoneme") {
    complete_pairs <- complete_pairs |>
      mutate(total = sum(count), .by = phoneme)
  } else {
    complete_pairs <- complete_pairs |>
      mutate(total = sum(count), .by = grapheme)
  }
  
  complete_pairs <- complete_pairs |>
    mutate(
      percentage = ifelse(total > 0, 100 * count / total, 0),
      phoneme_label = paste0("/", phoneme, "/")
    )
  
  relevant_phonemes <- complete_pairs |>
    group_by(phoneme) |>
    summarise(total_count = sum(count), .groups = "drop") |>
    filter(total_count > 0) |>
    pull(phoneme)
  
  complete_pairs <- complete_pairs |>
    filter(phoneme %in% relevant_phonemes)
  
  if (nrow(complete_pairs) == 0) {
    return(data.frame())
  }
  
  complete_pairs$phoneme_label <- factor(
    complete_pairs$phoneme_label,
    levels = paste0("/", vowel_list[vowel_list %in% relevant_phonemes], "/")
  )
  complete_pairs$grapheme <- factor(complete_pairs$grapheme, levels = grapheme_order)
  
  complete_pairs
}

create_vowel_confusion_matrix_plot <- function(matrix_data, title_text) {
  if (nrow(matrix_data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No mappings available for this category") +
        theme_void()
    )
  }
  
  ggplot(matrix_data, aes(x = grapheme, y = phoneme_label, fill = percentage)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(
      aes(
        label = ifelse(percentage > 0, format_pct(percentage), ""),
        color = ifelse(percentage >= 55, "white", "black")
      ),
      size = 4
    ) +
    scale_fill_gradient(
      low = "#f7fbff",
      high = "#08306b",
      limits = c(0, 100),
      name = "Percent"
    ) +
    scale_color_identity() +
    labs(
      title = title_text,
      x = "Grapheme",
      y = "Phoneme"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 16),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 18),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
}

# Vowel phonemes only; fixed column order (bottom to top in the Sankey plot).
SANKEY_PHONEME_ORDER <- c(
  "a", PHONEME_a_bar, "ei", "e", PHONEME_e_bar, "\u00e4", "\u00eb", PHONEME_a_umlaut_bar,
  "i", PHONEME_i_bar, "ie",
  "o", PHONEME_o_bar, "\u00f6", PHONEME_o_umlaut_bar, "ou", PHONEME_ou_umlaut,
  "u", PHONEME_u_bar, "\u00fc", PHONEME_u_umlaut_bar, "uo", PHONEME_ue
)

sankey_phoneme_order <- function(phonemes) {
  intersect(SANKEY_PHONEME_ORDER, unique(phonemes))
}

# Aggregate grapheme -> phoneme flows for Sankey / alluvial plots.
prepare_grapheme_phoneme_sankey_data <- function(mappings, count_mode = "token",
                                                 min_count = 10, max_links = 120) {
  if (is.null(mappings) || nrow(mappings) == 0) return(NULL)

  df <- filter_vowel_grapheme_phoneme_mappings(mappings, exclude_e_to_e = TRUE)
  if (is.null(df)) return(NULL)

  agg <- df |>
    group_by(grapheme, phoneme) |>
    summarise(
      value = if (count_mode == "type") dplyr::n_distinct(word) else sum(frequency),
      .groups = "drop"
    ) |>
    filter(value >= min_count) |>
    arrange(desc(value))

  if (nrow(agg) == 0) return(NULL)
  if (nrow(agg) > max_links) agg <- agg[seq_len(max_links), , drop = FALSE]

  list(
    agg = agg,
    unit_label = if (count_mode == "type") "types" else "tokens",
    min_count = min_count
  )
}

# Vowel mappings for Sankey / typology (optionally omit ⟨e⟩ → /e/ for Sankey plot).
filter_vowel_grapheme_phoneme_mappings <- function(mappings, exclude_e_to_e = FALSE) {
  if (is.null(mappings) || nrow(mappings) == 0) {
    return(NULL)
  }

  df <- mappings
  df$grapheme <- normalize_y_grapheme(df$grapheme)
  df <- df[df$phoneme %in% SANKEY_PHONEME_ORDER, , drop = FALSE]
  if (isTRUE(exclude_e_to_e)) {
    df <- df[!(df$grapheme == "e" & df$phoneme == "e"), , drop = FALSE]
  }
  if (nrow(df) == 0) {
    return(NULL)
  }

  df
}

filter_sankey_grapheme_phoneme_mappings <- function(mappings) {
  filter_vowel_grapheme_phoneme_mappings(mappings, exclude_e_to_e = TRUE)
}

filter_typology_grapheme_phoneme_mappings <- function(mappings) {
  filter_vowel_grapheme_phoneme_mappings(mappings, exclude_e_to_e = FALSE)
}

aggregate_vowel_grapheme_phoneme <- function(mappings, count_mode = "token",
                                             exclude_e_to_e = FALSE) {
  count_mode <- match.arg(count_mode, c("token", "type"))
  df <- filter_vowel_grapheme_phoneme_mappings(mappings, exclude_e_to_e = exclude_e_to_e)
  if (is.null(df)) {
    return(NULL)
  }

  df |>
    group_by(grapheme, phoneme) |>
    summarise(
      value = if (count_mode == "type") dplyr::n_distinct(word) else sum(frequency),
      .groups = "drop"
    )
}

# Full G→P aggregation for Sankey vowels (no min_count / max_links filter).
aggregate_sankey_grapheme_phoneme <- function(mappings, count_mode = "token") {
  aggregate_vowel_grapheme_phoneme(mappings, count_mode, exclude_e_to_e = TRUE)
}

aggregate_typology_grapheme_phoneme <- function(mappings, count_mode = "token") {
  aggregate_vowel_grapheme_phoneme(mappings, count_mode, exclude_e_to_e = FALSE)
}

entropy_log2_probs <- function(prob) {
  prob <- prob[prob > 0]
  if (length(prob) == 0) {
    return(0)
  }
  -sum(prob * log2(prob))
}

# Links below this share are ignored for Entropie, Dominanz and Klassifikation.
CORRESPONDENCE_MARGINAL_SHARE <- 0.05
CORRESPONDENCE_CATEGORY_BIUNIQUE <- "Biunique (G\u2194P)"
CORRESPONDENCE_CATEGORY_EINDEUTIG_G <- "Monophonie (G\u2192P)"
CORRESPONDENCE_CATEGORY_EINDEUTIG_P <- "L\u00f6sungsraum P\u2192G (eindeutig)"
CORRESPONDENCE_CATEGORY_POLYVALENT_DOM <- "Polyvalenz G\u2192P (dominant)"
CORRESPONDENCE_CATEGORY_POLYVALENT_POLY <- "Polyvalenz G\u2192P (polyphon)"
CORRESPONDENCE_CATEGORY_LOESUNGSRAUM_DOM <- "L\u00f6sungsraum P\u2192G (dominant)"
CORRESPONDENCE_CATEGORY_LOESUNGSRAUM_DIFFUS <- "L\u00f6sungsraum P\u2192G (diffus)"

# Dominant if share is high, or clearly ahead of the runner-up (tolerance for polyvalent graphemes).
correspondence_is_dominant <- function(dominance, top_value, second_value,
                                       strict_share = 0.8,
                                       tolerant_share = 0.55,
                                       min_lead_ratio = 2) {
  strict <- dominance >= strict_share
  tolerant <- dominance >= tolerant_share
  lead <- is.na(second_value) | second_value <= 0 | (top_value / second_value >= min_lead_ratio)
  strict | (tolerant & lead)
}

summarise_correspondence_node <- function(agg, node_col,
                                          marginal_share = CORRESPONDENCE_MARGINAL_SHARE) {
  node_sym <- rlang::sym(node_col)
  partner_col <- if (node_col == "grapheme") "phoneme" else "grapheme"
  partner_sym <- rlang::sym(partner_col)
  top_label_col <- if (node_col == "grapheme") "top_phoneme" else "top_grapheme"

  full_degree <- agg |>
    group_by(!!node_sym) |>
    summarise(degree = n_distinct(!!partner_sym), .groups = "drop")

  productive <- agg |>
    group_by(!!node_sym) |>
    mutate(share = value / sum(value)) |>
    filter(share >= marginal_share | value == max(value)) |>
    mutate(share = value / sum(value)) |>
    summarise(
      productive_degree = dplyr::n(),
      total_value = sum(value),
      top_value = max(value),
      second_value = if (dplyr::n() >= 2L) sort(value, decreasing = TRUE)[2] else 0,
      entropy = entropy_log2_probs(share),
      dominance = max(share),
      !!top_label_col := as.character(!!partner_sym)[which.max(value)],
      .groups = "drop"
    ) |>
    mutate(
      is_dominant = correspondence_is_dominant(dominance, top_value, second_value)
    )

  productive |>
    left_join(full_degree, by = as.character(node_col))
}

summarise_correspondence_partners <- function(agg, node_col) {
  node_sym <- rlang::sym(node_col)
  partner_col <- if (node_col == "grapheme") "phoneme" else "grapheme"
  partner_sym <- rlang::sym(partner_col)

  agg |>
    group_by(!!node_sym) |>
    mutate(
      share = value / sum(value),
      partner_label = if (node_col == "grapheme") {
        paste0("/", !!partner_sym, "/")
      } else {
        paste0("\u2329", !!partner_sym, "\u232a")
      }
    ) |>
    arrange(desc(value), partner_label) |>
    summarise(
      partners = paste(
        sprintf("%s %.1f%%", partner_label, 100 * share),
        collapse = ", "
      ),
      .groups = "drop"
    )
}

# Core pair G↔P is biunique when G maps to one P productively and G is the primary
# spelling of P (P may still have alternative spellings in its graphematic solution space).
correspondence_is_core_biunique <- function(grapheme_productive_degree,
                                            grapheme_top_phoneme,
                                            phoneme_top_grapheme,
                                            grapheme, phoneme) {
  grapheme_productive_degree == 1L &
    !is.na(grapheme_top_phoneme) &
    !is.na(phoneme_top_grapheme) &
    as.character(grapheme_top_phoneme) == as.character(phoneme) &
    as.character(phoneme_top_grapheme) == as.character(grapheme)
}

classify_grapheme_category <- function(degree, productive_degree, phoneme_top_grapheme,
                                      grapheme, top_phoneme, is_dominant) {
  biunique <- correspondence_is_core_biunique(
    productive_degree, top_phoneme, phoneme_top_grapheme, grapheme, top_phoneme
  )
  dplyr::case_when(
    biunique ~ "Biunique (G\u2194P)",
    productive_degree == 1L ~ CORRESPONDENCE_CATEGORY_EINDEUTIG_G,
    degree > 1L & is_dominant ~ CORRESPONDENCE_CATEGORY_POLYVALENT_DOM,
    degree > 1L ~ CORRESPONDENCE_CATEGORY_POLYVALENT_POLY,
    TRUE ~ CORRESPONDENCE_CATEGORY_EINDEUTIG_G
  )
}

classify_phoneme_category <- function(degree, productive_degree, grapheme_productive_degree,
                                      grapheme_top_phoneme, phoneme, top_grapheme, is_dominant) {
  biunique <- correspondence_is_core_biunique(
    grapheme_productive_degree, grapheme_top_phoneme, top_grapheme, top_grapheme, phoneme
  )
  dplyr::case_when(
    biunique ~ "Biunique (G\u2194P)",
    productive_degree == 1L ~ CORRESPONDENCE_CATEGORY_EINDEUTIG_P,
    degree > 1L & is_dominant ~ CORRESPONDENCE_CATEGORY_LOESUNGSRAUM_DOM,
    degree > 1L ~ CORRESPONDENCE_CATEGORY_LOESUNGSRAUM_DIFFUS,
    TRUE ~ CORRESPONDENCE_CATEGORY_EINDEUTIG_P
  )
}

compute_correspondence_mutual_information <- function(agg) {
  total <- sum(agg$value)
  if (total <= 0) {
    return(list(
      mutual_information = NA_real_,
      entropy_phoneme = NA_real_,
      entropy_phoneme_given_grapheme = NA_real_,
      entropy_grapheme = NA_real_,
      entropy_grapheme_given_phoneme = NA_real_
    ))
  }

  joint <- agg$value / total
  p_g <- stats::setNames(
    vapply(split(agg$value, agg$grapheme), sum, numeric(1)) / total,
    unique(agg$grapheme)
  )
  p_p <- stats::setNames(
    vapply(split(agg$value, agg$phoneme), sum, numeric(1)) / total,
    unique(agg$phoneme)
  )

  mi <- sum(
    joint * log2(joint / (p_g[agg$grapheme] * p_p[agg$phoneme])),
    na.rm = TRUE
  )

  h_p <- entropy_log2_probs(p_p)
  h_g <- entropy_log2_probs(p_g)

  h_p_given_g <- sum(
    vapply(
      split(agg, agg$grapheme),
      function(g_df) {
        pg <- sum(g_df$value) / total
        pg * entropy_log2_probs(g_df$value / sum(g_df$value))
      },
      numeric(1)
    )
  )

  h_g_given_p <- sum(
    vapply(
      split(agg, agg$phoneme),
      function(p_df) {
        pp <- sum(p_df$value) / total
        pp * entropy_log2_probs(p_df$value / sum(p_df$value))
      },
      numeric(1)
    )
  )

  list(
    mutual_information = mi,
    entropy_phoneme = h_p,
    entropy_phoneme_given_grapheme = h_p_given_g,
    entropy_grapheme = h_g,
    entropy_grapheme_given_phoneme = h_g_given_p
  )
}

build_correspondence_combined_table <- function(grapheme_table, phoneme_table) {
  empty <- data.frame(
    Einheit = character(),
    Perspektive = character(),
    N = integer(),
    Degree = integer(),
    Entropie = numeric(),
    Dominanz = numeric(),
    Partner = character(),
    Kategorie = character(),
    stringsAsFactors = FALSE
  )
  if (nrow(grapheme_table) == 0L && nrow(phoneme_table) == 0L) {
    return(empty)
  }

  g_rows <- grapheme_table |>
    transmute(
      Einheit = .data$Graphem,
      Perspektive = "G \u2192 P",
      N = .data$N,
      Degree = .data$Degree,
      Entropie = .data$Entropie,
      Dominanz = .data$Dominanz,
      Partner = .data$Phoneme,
      Kategorie = .data$Kategorie
    )

  p_rows <- phoneme_table |>
    transmute(
      Einheit = .data$Phonem,
      Perspektive = "P \u2192 G",
      N = .data$N,
      Degree = .data$Degree,
      Entropie = .data$Entropie,
      Dominanz = .data$Dominanz,
      Partner = .data$Grapheme,
      Kategorie = .data$Kategorie
    )

  bind_rows(g_rows, p_rows) |>
    arrange(desc(.data$Entropie), desc(.data$Degree), desc(.data$N))
}

publication_table_empty <- function(cols) {
  stats::setNames(
    as.data.frame(replicate(length(cols), vector(mode = "character", length = 0L), simplify = FALSE)),
    cols
  )
}

build_publication_biunique_table <- function(grapheme_table, phoneme_table) {
  col_g_to_p <- paste0("G ", "\u2192", " P")
  col_p_to_g <- paste0("P ", "\u2192", " G")
  cols <- c("Graphie", "Phonem", "N (Graphie)", "N (Phonem)", col_g_to_p, col_p_to_g)
  g_bio <- grapheme_table |>
    filter(.data$Kategorie == CORRESPONDENCE_CATEGORY_BIUNIQUE)
  p_bio <- phoneme_table |>
    filter(.data$Kategorie == CORRESPONDENCE_CATEGORY_BIUNIQUE)
  if (nrow(g_bio) == 0L) {
    return(publication_table_empty(cols))
  }

  joined <- g_bio |>
    mutate(
      Phonem = sub(" .*", "", .data$Phoneme),
      `N (Graphem)` = .data$N,
      !!col_g_to_p := .data$Phoneme
    ) |>
    left_join(
      p_bio |>
        transmute(
          Phonem = .data$Phonem,
          `N (Phonem)` = .data$N,
          !!col_p_to_g := .data$Grapheme
        ),
      by = "Phonem"
    )

  joined |>
    transmute(
      Graphem = .data$Graphem,
      Phonem = .data$Phonem,
      `N (Graphem)` = .data$`N (Graphem)`,
      `N (Phonem)` = .data$`N (Phonem)`,
      !!col_g_to_p := .data[[col_g_to_p]],
      !!col_p_to_g := .data[[col_p_to_g]]
    ) |>
    arrange(desc(.data$`N (Graphem)`))
}

build_publication_eindeutig_p_table <- function(phoneme_table) {
  phoneme_table |>
    filter(.data$Kategorie == CORRESPONDENCE_CATEGORY_EINDEUTIG_P) |>
    transmute(
      Phonem = .data$Phonem,
      N = .data$N,
      Grad = .data$Degree,
      Zuordnungen = .data$Grapheme
    ) |>
    arrange(desc(.data$N))
}

build_publication_eindeutig_g_table <- function(grapheme_table) {
  grapheme_table |>
    filter(.data$Kategorie == CORRESPONDENCE_CATEGORY_EINDEUTIG_G) |>
    transmute(
      Graphem = .data$Graphem,
      N = .data$N,
      Grad = .data$Degree,
      Zuordnungen = .data$Phoneme
    ) |>
    arrange(desc(.data$N))
}

build_publication_loesungsraum_dom_table <- function(phoneme_table) {
  phoneme_table |>
    filter(.data$Kategorie == CORRESPONDENCE_CATEGORY_LOESUNGSRAUM_DOM) |>
    transmute(
      Phonem = .data$Phonem,
      N = .data$N,
      Grad = .data$Degree,
      Dominanz = sprintf("%.0f%%", 100 * .data$Dominanz),
      Entropie = round(.data$Entropie, 2),
      Zuordnungen = .data$Grapheme
    ) |>
    arrange(desc(.data$N))
}

build_publication_loesungsraum_diffus_table <- function(phoneme_table) {
  phoneme_table |>
    filter(.data$Kategorie == CORRESPONDENCE_CATEGORY_LOESUNGSRAUM_DIFFUS) |>
    transmute(
      Phonem = .data$Phonem,
      N = .data$N,
      Grad = .data$Degree,
      Dominanz = sprintf("%.0f%%", 100 * .data$Dominanz),
      Entropie = round(.data$Entropie, 2),
      Zuordnungen = .data$Grapheme
    ) |>
    arrange(desc(.data$N))
}

build_publication_polyvalenz_dom_table <- function(grapheme_table) {
  grapheme_table |>
    filter(.data$Kategorie == CORRESPONDENCE_CATEGORY_POLYVALENT_DOM) |>
    transmute(
      Graphem = .data$Graphem,
      N = .data$N,
      Grad = .data$Degree,
      Dominanz = sprintf("%.0f%%", 100 * .data$Dominanz),
      Entropie = round(.data$Entropie, 2),
      Zuordnungen = .data$Phoneme
    ) |>
    arrange(desc(.data$N))
}

build_publication_polyvalenz_polyphon_table <- function(grapheme_table) {
  grapheme_table |>
    filter(.data$Kategorie == CORRESPONDENCE_CATEGORY_POLYVALENT_POLY) |>
    transmute(
      Graphem = .data$Graphem,
      N = .data$N,
      Grad = .data$Degree,
      Dominanz = sprintf("%.0f%%", 100 * .data$Dominanz),
      Entropie = round(.data$Entropie, 2),
      Zuordnungen = .data$Phoneme
    ) |>
    arrange(desc(.data$N))
}

build_correspondence_publication_tables <- function(grapheme_table, phoneme_table) {
  list(
    biunique = build_publication_biunique_table(grapheme_table, phoneme_table),
    eindeutig_p = build_publication_eindeutig_p_table(phoneme_table),
    eindeutig_g = build_publication_eindeutig_g_table(grapheme_table),
    loesungsraum_dominant = build_publication_loesungsraum_dom_table(phoneme_table),
    loesungsraum_diffus = build_publication_loesungsraum_diffus_table(phoneme_table),
    polyvalenz_dominant = build_publication_polyvalenz_dom_table(grapheme_table),
    polyvalenz_polyphon = build_publication_polyvalenz_polyphon_table(grapheme_table)
  )
}

publication_typology_methodology_lines <- function(count_mode, metrics_unit_label) {
  count_label <- if (count_mode == "type") "Typen (Wortformen)" else "Token (H\u00e4ufigkeiten)"
  c(
    paste0("Datengrundlage: Yolanda-Korpus (word_list_with_refkorpus.csv); Auswertung nach ", count_label, "."),
    "Untersuchungsgegenstand: Vokal-Graphem-Phonem-Zuordnungen aus den grapho-Annotationen (ohne Konsonanten).",
    "Typologie umfasst e \u2192 /e/; diese Zuordnung ist im Sankey-Diagramm oben ausgeblendet.",
    paste0("N: Anzahl betroffener Wortformen (Typen), immer unabh\u00e4ngig von Count by. ",
           "Zuordnungsanteile, Entropie und Dominanz gem\u00e4\u00df Count by (", metrics_unit_label, ")."),
    publication_metric_explanation_lines(count_mode, metrics_unit_label),
    "Biunique (G\u2194P): Kernpaar \u2014 G mappt produktiv auf ein Phonem und ist dessen Hauptschreibung (Meletis & D\u00fcrscheid 2022).",
    "L\u00f6sungsraum P\u2192G (eindeutig): produktiv ein Schreibweg f\u00fcr dieses Phonem; unabh\u00e4ngig von G\u2192P-Polyvalenz oder Mitbenutzung desselben Graphems durch andere Phoneme. (dominant/diffus): mehrere Schreibwege f\u00fcr dasselbe Phonem.",
    "Monophonie (G\u2192P): produktiv eine Phonem-Lesung; Polyvalenz G\u2192P: mehrere Lesungen (polyphon).",
    "Terminologie: Meletis, Dimitrios & Christa D\u00fcrscheid (2022). Writing Systems and Their Use. De Gruyter."
  )
}

publication_metric_explanation_lines <- function(count_mode, metrics_unit_label) {
  count_word <- if (count_mode == "type") {
    "jede Wortform (Typ) z\u00e4hlt einmal"
  } else {
    "jede Wortform z\u00e4hlt mit ihrer Token-H\u00e4ufigkeit"
  }
  marg <- as.integer(100 * CORRESPONDENCE_MARGINAL_SHARE)
  c(
    paste0("Zuordnungen: Anteile der Partner pro Zeile; Basis: ", count_word, " (", metrics_unit_label, ")."),
    paste0("Grad: Anzahl verschiedener Partner in der Zeile (roh, alle im Korpus belegten Verkn\u00fcpfungen, ",
           "inkl. Anteile unter ", marg, " %)."),
    paste0("Produktiv relevant (f\u00fcr Entropie, Dominanz, Klassifikation): Partner mit Anteil \u2265 ", marg,
           " % oder \u2014 falls alle darunter \u2014 stets der h\u00e4ufigste Partner; ",
           "danach Neunormierung auf 100 %."),
    "Entropie H = \u2212\u03a3 p\u00b7log\u2082(p) (Bit) \u00fcber produktiv relevante Partner; 0 = kein Rest-Unsicherheitsanteil.",
    "Dominanz: Anteil p_max des st\u00e4rksten produktiv relevanten Partners nach Neunormierung (Tabellen: gerundet in %).",
    paste0("dominant: Grad > 1 und p_max \u2265 80 %, oder p_max \u2265 55 % bei mindestens doppeltem ",
           "Z\u00e4hlvorsprung (Count by) \u00fcber den Zweitpartner."),
    "diffus / polyphon: Grad > 1, aber kein dominant \u2014 mehrere Partner ohne klaren Vorsprung (L\u00f6sungsraum P\u2192G bzw. Polyvalenz G\u2192P)."
  )
}

publication_table_subcaption <- function(table_key, count_mode, metrics_unit_label) {
  count_label <- if (count_mode == "type") "Typen" else "Token"
  marg <- as.integer(100 * CORRESPONDENCE_MARGINAL_SHARE)
  base_style <- paste0(
    "caption-side: bottom; text-align: left; font-size: 0.88em; color: #444; ",
    "padding: 8px 4px 12px; line-height: 1.45; white-space: normal;"
  )

  n_txt <- htmltools::tagList(
    tags$strong("N"), ": Anzahl betroffener Wortformen (Typen), unabh\u00e4ngig von Count by."
  )
  grad_txt <- htmltools::tagList(
    tags$strong("Grad"), ": Anzahl verschiedener Partner dieser Einheit im Korpus (alle belegten ",
    "G\u2194P-Verkn\u00fcpfungen, auch unter ", marg, " %)."
  )
  zuord_txt <- htmltools::tagList(
    tags$strong("Zuordnungen"), ": Partner mit Anteil in %; gez\u00e4hlt nach Count by (", count_label, "). ",
    if (count_mode == "type") "Jede Wortform z\u00e4hlt einmal." else "Gewichtet mit Token-H\u00e4ufigkeit."
  )
  metrics_txt <- htmltools::tagList(
    grad_txt, " ",
    tags$strong("Entropie"), ": Shannon-Entropie H = \u2212\u03a3\u2009p\u2009log\u2082(p) (Bit) ",
    "nur \u00fcber produktiv relevante Partner (Anteil \u2265 ", marg, " %, sonst nur der H\u00e4ufigste; ",
    "dann Neunormierung). ", tags$strong("Dominanz"), ": gr\u00f6\u00dfter Anteil p",
    tags$sub("max"), " nach demselben Filter (in % gerundet). ", zuord_txt
  )
  dominant_rule <- htmltools::tagList(
    tags$strong("dominant"), " vs. ", tags$strong("diffus/polyphon"), ": Bei Grad > 1 gilt ",
    tags$em("dominant"), ", wenn p", tags$sub("max"), " \u2265 80 %, oder p", tags$sub("max"),
    " \u2265 55 % und der st\u00e4rkste Partner mindestens doppelt so oft vorkommt (Count by) wie der zweite ",
    "produktiv relevante Partner; sonst ", tags$em("diffus"), " (P\u2192G) bzw. ", tags$em("polyphon"), " (G\u2192P)."
  )

  caption_body <- switch(
    table_key,
    biunique = htmltools::tagList(
      n_txt, " ", zuord_txt, " ",
      "Biunique: produktiv nur ein Partner je Seite; G ist Hauptschreibung des Zielphonems."
    ),
    eindeutig_p = htmltools::tagList(
      n_txt, " ", grad_txt, " ", zuord_txt, " ",
      "L\u00f6sungsraum P\u2192G (eindeutig): produktiv ein Schreibweg f\u00fcr dieses Phonem (Entropie = 0). ",
      "Gilt pro Phonem-Zeile: dasselbe Graphem kann in G\u2192P polyvalent sein und von mehreren Phonemen ",
      "genutzt werden (Konvergenz im L\u00f6sungsraum), ohne die P\u2192G-Eindeutigkeit des jeweiligen Phonems aufzuheben."
    ),
    eindeutig_g = htmltools::tagList(
      n_txt, " ", grad_txt, " ", zuord_txt, " ",
      "Monophonie (G\u2192P): produktiv eine Lesung; nicht Biunique, wenn das Graphem nicht ",
      "Hauptschreibung des Zielphonems ist oder das Phonem einen weiteren L\u00f6sungsraum hat."
    ),
    loesungsraum_dominant = htmltools::tagList(n_txt, " ", metrics_txt, " ", dominant_rule),
    loesungsraum_diffus = htmltools::tagList(n_txt, " ", metrics_txt, " ", dominant_rule),
    polyvalenz_dominant = htmltools::tagList(n_txt, " ", metrics_txt, " ", dominant_rule),
    polyvalenz_polyphon = htmltools::tagList(n_txt, " ", metrics_txt, " ", dominant_rule),
    htmltools::tagList(n_txt, " ", zuord_txt)
  )

  htmltools::tags$caption(
    class = "pub-table-subcaption",
    style = base_style,
    caption_body
  )
}

bind_correspondence_publication_tables <- function(publication_tables) {
  labels <- c(
    biunique = "1 Eineindeutige Zuordnung (Biunique G\u2194P)",
    eindeutig_p = "2.1 L\u00f6sungsraum P \u2192 G (eindeutig)",
    eindeutig_g = "2.2 Monophonie (G \u2192 P)",
    loesungsraum_dominant = "3.1 L\u00f6sungsraum P \u2192 G (dominant)",
    loesungsraum_diffus = "3.2 L\u00f6sungsraum P \u2192 G (diffus)",
    polyvalenz_dominant = "4.1 Polyvalenz G \u2192 P (dominant)",
    polyvalenz_polyphon = "4.2 Polyvalenz G \u2192 P (polyphon)"
  )
  bind_rows(lapply(names(labels), function(name) {
    df <- publication_tables[[name]]
    if (nrow(df) == 0L) {
      return(NULL)
    }
    df |> mutate(Tabelle = labels[[name]], .before = 1)
  }))
}

export_correspondence_publication_csv <- function(publication_tables, count_mode, metrics_unit_label) {
  data_df <- bind_correspondence_publication_tables(publication_tables)
  meta_lines <- publication_typology_methodology_lines(count_mode, metrics_unit_label)
  meta_df <- tibble::tibble(
    Tabelle = NA_character_,
    Hinweis = c("--- Methodik ---", meta_lines, "--- Daten ---")
  )
  if (nrow(data_df) == 0L) {
    return(meta_df)
  }
  if (!"Hinweis" %in% names(data_df)) {
    data_df <- data_df |> mutate(Hinweis = NA_character_)
  }
  dplyr::bind_rows(meta_df, data_df)
}

compute_sankey_correspondence_typology <- function(mappings, count_mode = "token") {
  count_mode <- match.arg(count_mode, c("token", "type"))
  typology_df <- filter_typology_grapheme_phoneme_mappings(mappings)
  agg <- aggregate_typology_grapheme_phoneme(mappings, count_mode)
  if (is.null(agg)) {
    return(list(
      graphemes = data.frame(),
      phonemes = data.frame(),
      combined = data.frame(),
      publication = build_correspondence_publication_tables(
        data.frame(), data.frame()
      ),
      system = list(),
      metrics_unit_label = if (count_mode == "type") "Typen" else "Token"
    ))
  }

  grapheme_n_types <- typology_df |>
    group_by(grapheme) |>
    summarise(n = dplyr::n_distinct(word), .groups = "drop")

  phoneme_n_types <- typology_df |>
    group_by(phoneme) |>
    summarise(n = dplyr::n_distinct(word), .groups = "drop")

  system_metrics <- compute_correspondence_mutual_information(agg)
  metrics_unit_label <- if (count_mode == "type") "Typen" else "Token"

  grapheme_stats <- summarise_correspondence_node(agg, "grapheme")
  phoneme_stats <- summarise_correspondence_node(agg, "phoneme")

  grapheme_partners <- summarise_correspondence_partners(agg, "grapheme")
  phoneme_partners <- summarise_correspondence_partners(agg, "phoneme")

  grapheme_table <- grapheme_stats |>
    left_join(grapheme_n_types, by = "grapheme") |>
    left_join(grapheme_partners, by = "grapheme") |>
    left_join(
      phoneme_stats |>
        select(
          phoneme,
          phoneme_top_grapheme = top_grapheme
        ),
      by = c("top_phoneme" = "phoneme")
    ) |>
    mutate(
      grapheme_label = paste0("\u2329", grapheme, "\u232a"),
      Kategorie = classify_grapheme_category(
        degree, productive_degree, phoneme_top_grapheme, grapheme, top_phoneme, is_dominant
      )
    ) |>
    arrange(desc(entropy), desc(degree), desc(n)) |>
    select(
      Graphem = grapheme_label,
      N = n,
      Degree = degree,
      Entropie = entropy,
      Dominanz = dominance,
      Phoneme = partners,
      Kategorie
    )

  phoneme_table <- phoneme_stats |>
    left_join(phoneme_n_types, by = "phoneme") |>
    left_join(phoneme_partners, by = "phoneme") |>
    left_join(
      grapheme_stats |>
        select(
          grapheme,
          grapheme_productive_degree = productive_degree,
          grapheme_top_phoneme = top_phoneme
        ),
      by = c("top_grapheme" = "grapheme")
    ) |>
    mutate(
      phoneme_label = paste0("/", phoneme, "/"),
      Kategorie = classify_phoneme_category(
        degree, productive_degree, grapheme_productive_degree,
        grapheme_top_phoneme, phoneme, top_grapheme, is_dominant
      )
    ) |>
    arrange(desc(entropy), desc(degree), desc(n)) |>
    select(
      Phonem = phoneme_label,
      N = n,
      Degree = degree,
      Entropie = entropy,
      Dominanz = dominance,
      Grapheme = partners,
      Kategorie
    )

  list(
    graphemes = grapheme_table,
    phonemes = phoneme_table,
    combined = build_correspondence_combined_table(grapheme_table, phoneme_table),
    publication = build_correspondence_publication_tables(grapheme_table, phoneme_table),
    system = system_metrics,
    metrics_unit_label = metrics_unit_label
  )
}

sankey_ggsankeyfier_available <- function() {
  if (!requireNamespace("ggsankeyfier", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    return(FALSE)
  }
  utils::packageVersion("ggsankeyfier") >= numeric_version("0.1.8.15")
}

sankey_ggsankeyfier_install_hint <- function() {
  paste(
    "Package ggsankeyfier (>= 0.1.8.15) is required for fixed phoneme ordering.",
    "Install with:",
    "remotes::install_github(\"pepijn-devries/ggsankeyfier\")"
  )
}

# Custom stacking: grapheme column by barycenter order, phoneme column fixed.
make_sankey_bipartite_order_fn <- function(grapheme_order, phoneme_order, node_levels) {
  g_rank <- stats::setNames(seq_along(grapheme_order), grapheme_order)
  p_rank <- stats::setNames(seq_along(phoneme_order), phoneme_order)

  function(data) {
    if ("edge_id" %in% names(data)) {
      src <- node_levels[data$group]
      dst <- node_levels[data$group_end]
      data$edge_order <- g_rank[src]
      data$edge_order_end <- p_rank[dst]
      na <- is.na(data$edge_order)
      if (any(na)) {
        data$edge_order[na] <- 900 + seq_len(sum(na))
      }
      na <- is.na(data$edge_order_end)
      if (any(na)) {
        data$edge_order_end[na] <- 900 + seq_len(sum(na))
      }
    } else {
      nm <- node_levels[data$group]
      xnum <- as.numeric(data$x)
      data$node_order <- ifelse(xnum == 2, p_rank[nm], g_rank[nm])
      na <- is.na(data$node_order)
      if (any(na)) {
        data$node_order[na] <- 900 + seq_len(sum(na))
      }
    }
    data
  }
}

# Normalize grapheme/phoneme labels for vowel-based ordering.
sankey_normalize_symbol <- function(symbol) {
  s <- gsub("^/|/$", "", symbol)
  s <- tolower(normalize_y_grapheme(s))
  s
}

# Primary vowel group (a, e, i, o, u, y) from earliest vowel in the symbol.
sankey_primary_vowel_rank <- function(symbol) {
  s <- sankey_normalize_symbol(symbol)
  vowels <- c("a", "e", "i", "o", "u", "y")
  positions <- vapply(vowels, function(v) {
    pos <- regexpr(v, s, fixed = TRUE)[1]
    if (pos < 0) Inf else pos
  }, numeric(1))
  if (min(positions) == Inf) {
    return(99L)
  }
  which.min(positions)
}

# Grapheme order follows links; phoneme order is fixed (see SANKEY_PHONEME_ORDER).
order_sankey_bipartite <- function(agg, n_iter = 10) {
  g_nodes <- unique(agg$grapheme)
  p_ord <- sankey_phoneme_order(unique(agg$phoneme))
  p_pos <- stats::setNames(seq_along(p_ord), p_ord)

  g_tot <- stats::setNames(
    vapply(g_nodes, function(g) sum(agg$value[agg$grapheme == g]), numeric(1)),
    g_nodes
  )

  g_pos <- stats::setNames(
    order(vapply(g_nodes, sankey_primary_vowel_rank, integer(1)), -g_tot[g_nodes]),
    g_nodes
  )

  for (iter in seq_len(n_iter)) {
    g_scores <- vapply(g_nodes, function(g) {
      idx <- agg$grapheme == g
      mean(p_pos[agg$phoneme[idx]], w = agg$value[idx])
    }, numeric(1))
    g_ord <- g_nodes[order(
      g_scores,
      vapply(g_nodes, sankey_primary_vowel_rank, integer(1)),
      -g_tot[g_nodes]
    )]
    g_pos <- stats::setNames(seq_along(g_ord), g_ord)
  }

  list(
    grapheme_order = g_ord,
    phoneme_order = p_ord
  )
}

# ggsankeyfier align="center" does not shift shorter bipartite stages; offset graphemes instead.
sankey_grapheme_column_offset <- function(data) {
  if (!"connector" %in% names(data)) {
    return(0)
  }
  is_from <- !is.na(data$connector) & data$connector == "from"
  is_to <- !is.na(data$connector) & data$connector == "to"
  g_max <- NA_real_
  p_max <- NA_real_
  if (any(is_from) && "ymax" %in% names(data)) {
    g_max <- max(data$ymax[is_from], na.rm = TRUE)
  }
  if (any(is_to) && "ymax" %in% names(data)) {
    p_max <- max(data$ymax[is_to], na.rm = TRUE)
  }
  if ("y" %in% names(data) && "yend" %in% names(data)) {
    if (!is.finite(g_max)) {
      g_max <- max(data$y, na.rm = TRUE)
    }
    if (!is.finite(p_max)) {
      p_max <- max(data$yend, na.rm = TRUE)
    }
  }
  if (!is.finite(g_max) || !is.finite(p_max) || p_max <= g_max) {
    return(0)
  }
  (p_max - g_max) / 2
}

sankey_shift_grapheme_column <- function(data, offset) {
  if (!is.finite(offset) || offset <= 0) {
    return(data)
  }
  is_from <- !is.na(data$connector) & data$connector == "from"
  if ("ymin" %in% names(data)) {
    data$ymin[is_from] <- data$ymin[is_from] + offset
    data$ymax[is_from] <- data$ymax[is_from] + offset
  }
  if ("y" %in% names(data) && "yend" %in% names(data)) {
    data$y <- data$y + offset
  } else if ("y" %in% names(data)) {
    data$y[is_from] <- data$y[is_from] + offset
  }
  if ("y_node" %in% names(data)) {
    data$y_node <- data$y_node + offset
  }
  data
}

make_sankey_grapheme_centered_position <- function(width = "auto",
                                                   order_fn,
                                                   v_space = "auto",
                                                   nudge_x = 0) {
  base <- ggsankeyfier::position_sankey(
    v_space = v_space,
    width = width,
    align = "bottom",
    order = order_fn,
    nudge_x = nudge_x
  )
  base_proto <- ggsankeyfier::PositionSankey
  ggplot2::ggproto(
    NULL,
    base,
    compute_layer = function(self, data, params, layout) {
      data <- ggplot2::ggproto_parent(base_proto, self)$compute_layer(data, params, layout)
      offset <- sankey_grapheme_column_offset(data)
      sankey_shift_grapheme_column(data, offset)
    }
  )
}

# Horizontal Sankey via ggsankeyfier (fixed phoneme column order).
create_grapheme_phoneme_sankey_ggplot <- function(mappings,
                                                  count_mode = "token",
                                                  min_count = 10,
                                                  max_links = 120,
                                                  base_size = 14,
                                                  label_size = 3.2,
                                                  title_size = 16,
                                                  flow_alpha = 0.4,
                                                  node_width = 0.12,
                                                  layout = c("preview", "export")) {
  layout <- match.arg(layout)
  if (!sankey_ggsankeyfier_available()) {
    return(NULL)
  }

  prep <- prepare_grapheme_phoneme_sankey_data(
    mappings, count_mode, min_count, max_links
  )
  if (is.null(prep)) {
    return(NULL)
  }

  agg <- prep$agg
  node_order <- order_sankey_bipartite(agg)
  agg$grapheme <- factor(agg$grapheme, levels = node_order$grapheme_order)
  agg$phoneme <- factor(agg$phoneme, levels = node_order$phoneme_order)
  agg$flow <- agg$value
  agg <- agg[order(agg$grapheme, agg$phoneme), , drop = FALSE]

  long <- ggsankeyfier::pivot_stages_longer(
    agg,
    stages_from = c("grapheme", "phoneme"),
    values_from = "flow"
  )
  node_levels <- levels(long$node)
  order_fn <- make_sankey_bipartite_order_fn(
    node_order$grapheme_order,
    node_order$phoneme_order,
    node_levels
  )
  label_layout <- if (layout == "export") {
    list(nudge_factor = 0.05, x_expand = 0.06, margin_lr = 6)
  } else {
    list(nudge_factor = 0.11, x_expand = 0.14, margin_lr = 12)
  }
  label_nudge <- label_layout$nudge_factor * (label_size / 3.2)
  pos <- make_sankey_grapheme_centered_position(
    v_space = "auto",
    width = node_width,
    order = order_fn
  )
  pos_labels_gr <- make_sankey_grapheme_centered_position(
    v_space = "auto",
    width = node_width,
    order = order_fn,
    nudge_x = -label_nudge
  )
  pos_labels_ph <- make_sankey_grapheme_centered_position(
    v_space = "auto",
    width = node_width,
    order = order_fn,
    nudge_x = label_nudge
  )

  long <- long |>
    dplyr::mutate(
      stage_side = dplyr::if_else(
        as.character(stage) == "grapheme",
        "grapheme",
        "phoneme"
      ),
      node_label = dplyr::if_else(
        stage_side == "grapheme",
        paste0("\u2329", node, "\u232a"),
        paste0("/", node, "/")
      )
    )

  unit_de <- if (count_mode == "type") "Typen" else "Token"
  # title_text <- sprintf(
  #   "Graphem \u2192 Phonem (%s, min. %d, %d Verbindungen)",
  #   unit_de,
  #   prep$min_count,
  #   nrow(agg)
  # )

  ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = stage,
      y = flow,
      group = node,
      connector = connector,
      edge_id = edge_id
    )
  ) +
    ggsankeyfier::geom_sankeyedge(
      ggplot2::aes(fill = stage_side),
      position = pos,
      alpha = flow_alpha,
      color = NA
    ) +
    ggsankeyfier::geom_sankeynode(
      ggplot2::aes(fill = stage_side),
      position = pos,
      color = "black",
      linewidth = 0.25
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = dplyr::if_else(
          stage_side == "grapheme" & connector == "from",
          node_label,
          NA_character_
        )
      ),
      stat = ggsankeyfier::StatSankeynode,
      position = pos_labels_gr,
      size = label_size,
      color = "#1a4d7a",
      fontface = "bold",
      hjust = 1,
      lineheight = 0.9,
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = dplyr::if_else(
          stage_side == "phoneme" & connector == "to",
          node_label,
          NA_character_
        )
      ),
      stat = ggsankeyfier::StatSankeynode,
      position = pos_labels_ph,
      size = label_size,
      color = "#b8472d",
      fontface = "bold",
      hjust = 0,
      lineheight = 0.9,
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = c("grapheme" = "#2166ac", "phoneme" = "#d6604d"),
      guide = "none"
    ) +
    ggplot2::scale_x_discrete(
      labels = c("grapheme" = "Graphie", "phoneme" = "Phonem"),
      expand = ggplot2::expansion(
        mult = c(label_layout$x_expand, label_layout$x_expand)
      )
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    # ggplot2::labs(
    #   title = title_text,
    #   x = NULL,
    #   y = NULL
    # ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        size = title_size,
        face = "bold",
        hjust = 0.5,
        margin = ggplot2::margin(b = 10)
      ),
      axis.text.x = ggplot2::element_text(size = base_size * 0.95, face = "bold"),
      plot.margin = ggplot2::margin(
        14,
        label_layout$margin_lr,
        14,
        label_layout$margin_lr
      )
    )
}

sankey_export_height_inches <- function(n_links, base_height = 12) {
  as.numeric(max(base_height, min(26, 8 + n_links * 0.07)))
}

# renderPlot height 850 px at res 120 ≈ 7.08 in; match export size for label scaling.
SANKEY_PREVIEW_HEIGHT_IN <- 850 / 120

sankey_export_font_scale <- function(export_height_in,
                                     preview_height_in = SANKEY_PREVIEW_HEIGHT_IN) {
  as.numeric(export_height_in / preview_height_in)
}

# Extra multiplier for node labels in PNG/PDF (preview unchanged).
SANKEY_EXPORT_LABEL_BOOST <- 1.35

save_grapheme_phoneme_sankey_ggplot <- function(p,
                                                file,
                                                width = 16,
                                                height = 12,
                                                dpi = 300) {
  if (is.null(p)) {
    return(invisible(FALSE))
  }

  ext <- tolower(tools::file_ext(file))
  if (ext == "pdf") {
    ggplot2::ggsave(
      filename = file,
      plot = p,
      width = width,
      height = height,
      device = grDevices::cairo_pdf,
      limitsize = FALSE
    )
  } else if (requireNamespace("ragg", quietly = TRUE)) {
    ggplot2::ggsave(
      filename = file,
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      device = ragg::agg_png,
      limitsize = FALSE
    )
  } else {
    ggplot2::ggsave(
      filename = file,
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      limitsize = FALSE
    )
  }

  invisible(TRUE)
}

create_focus_tables <- function(mappings, vowel_list, count_mode = c("token", "type")) {
  count_mode <- match.arg(count_mode)
  
  filtered_mappings <- mappings |>
    filter(phoneme %in% unique(vowel_list))
  
  if (nrow(filtered_mappings) == 0) {
    return(list(
      summary = data.frame(),
      mutual = data.frame(),
      scatter = data.frame()
    ))
  }
  
  pair_counts <- filtered_mappings |>
    group_by(phoneme, grapheme) |>
    summarise(
      token_count = sum(frequency),
      type_count = n(),
      .groups = "drop"
    ) |>
    filter(token_count >= 2) |>
    mutate(count = if (count_mode == "token") token_count else type_count)
  
  if (nrow(pair_counts) == 0) {
    return(list(
      summary = data.frame(),
      mutual = data.frame(),
      scatter = data.frame()
    ))
  }
  
  row_totals <- pair_counts |>
    group_by(phoneme) |>
    summarise(row_total = sum(count), .groups = "drop")
  
  col_totals <- pair_counts |>
    group_by(grapheme) |>
    summarise(col_total = sum(count), .groups = "drop")
  
  pair_probs <- pair_counts |>
    left_join(row_totals, by = "phoneme") |>
    left_join(col_totals, by = "grapheme") |>
    mutate(
      p_grapheme_given_phoneme = 100 * count / row_total,
      p_phoneme_given_grapheme = 100 * count / col_total,
      mutual_strength = sqrt(p_grapheme_given_phoneme * p_phoneme_given_grapheme)
    ) |>
    mutate(
      mutual_level = cut(
        mutual_strength,
        breaks = c(-Inf, 25, 50, 75, Inf),
        labels = c("Very low", "Low", "Medium", "High")
      ),
      mutual_level = factor(
        mutual_level,
        levels = c("Very low", "Low", "Medium", "High"),
        ordered = TRUE
      )
    )
  
  summarise_focus <- function(df, item_col, partner_col, total_col, perspective_label) {
    item_sym <- rlang::sym(item_col)
    partner_sym <- rlang::sym(partner_col)
    total_sym <- rlang::sym(total_col)
    
    df |>
      group_by(!!item_sym) |>
      mutate(
        prob = count / sum(count),
        entropy_component = ifelse(prob > 0, -prob * log(prob), 0)
      ) |>
      summarise(
        total_count = dplyr::first(!!total_sym),
        top_partner = as.character((!!partner_sym)[which.max(prob)]),
        top_share = 100 * max(prob),
        entropy = sum(entropy_component),
        partners_used = n(),
        effective_choices = exp(entropy),
        .groups = "drop"
      ) |>
      mutate(
        entropy_norm = ifelse(partners_used > 1, entropy / log(partners_used), 0),
        focus = 1 - entropy_norm,
        perspective = perspective_label,
        item = as.character(!!item_sym)
      ) |>
      select(
        perspective,
        item,
        top_partner,
        top_share,
        focus,
        effective_choices,
        total_count,
        partners_used
      )
  }
  
  phoneme_summary <- pair_probs |>
    summarise_focus("phoneme", "grapheme", "row_total", "phoneme")
  
  grapheme_summary <- pair_probs |>
    summarise_focus("grapheme", "phoneme", "col_total", "grapheme")
  
  summary_table <- bind_rows(phoneme_summary, grapheme_summary) |>
    mutate(
      item = ifelse(perspective == "phoneme", paste0("/", item, "/"), paste0("<", item, ">")),
      top_partner = ifelse(
        perspective == "phoneme",
        paste0("<", top_partner, ">"),
        paste0("/", top_partner, "/")
      )
    )
  
  mutual_table <- pair_probs |>
    transmute(
      phoneme = paste0("/", phoneme, "/"),
      grapheme = paste0("<", grapheme, ">"),
      count = count,
      p_grapheme_given_phoneme = p_grapheme_given_phoneme,
      p_phoneme_given_grapheme = p_phoneme_given_grapheme,
      mutual_strength = mutual_strength,
      mutual_level = as.character(mutual_level)
    ) |>
    arrange(desc(mutual_strength), desc(count))
  
  scatter_table <- pair_probs |>
    left_join(
      phoneme_summary |>
        select(item, phoneme_focus = focus),
      by = c("phoneme" = "item")
    ) |>
    left_join(
      grapheme_summary |>
        select(item, grapheme_focus = focus),
      by = c("grapheme" = "item")
    ) |>
    transmute(
      phoneme = paste0("/", phoneme, "/"),
      grapheme = paste0("<", grapheme, ">"),
      count = count,
      phoneme_focus = phoneme_focus,
      grapheme_focus = grapheme_focus,
      mutual_strength = mutual_strength,
      mutual_level = as.character(mutual_level)
    )
  
  list(
    summary = summary_table,
    mutual = mutual_table,
    scatter = scatter_table
  )
}

create_focus_scatter_plot <- function(scatter_data, title_text) {
  if (nrow(scatter_data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No focus data available") +
        theme_void()
    )
  }
  
  jitter_pos <- position_jitter(width = 0.015, height = 0.015, seed = 123)
  
  ggplot(
    scatter_data,
    aes(
      x = phoneme_focus,
      y = grapheme_focus,
      size = count,
      color = mutual_level
    )
  ) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey70") +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey70") +
    geom_point(alpha = 0.75, position = jitter_pos) +
    geom_text(
      aes(label = paste0(phoneme, " \u2192 ", grapheme)),
      color = "black",
      check_overlap = FALSE,
      position = jitter_pos,
      size = 3.6
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_color_manual(
      values = c(
        "Very low" = "#440154",
        "Low" = "#31688e",
        "Medium" = "#35b779",
        "High" = "#fde725"
      ),
      breaks = c("Very low", "Low", "Medium", "High"),
      labels = c("Very low (<=25)", "Low (25-50)", "Medium (50-75)", "High (>75)"),
      name = "Mutual\nstrength (4 levels)"
    ) +
    scale_size_continuous(name = "Count", range = c(2, 9)) +
    labs(
      title = title_text,
      x = "Phoneme focus (1 - normalized entropy)",
      y = "Grapheme focus (1 - normalized entropy)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )
}

# ẏ/i Analysis Function
analyze_y_i_patterns <- function(word_data) {
  # Get all words containing ẏ (or legacy ẏ) or i
  y_words <- word_data[vapply(word_data$word, has_y_grapheme, logical(1)), , drop = FALSE]
  i_words <- word_data[grepl("i", word_data$word, fixed = TRUE), ]
  
  if (nrow(y_words) == 0 && nrow(i_words) == 0) {
    return(data.frame(
      y_word = character(0),
      i_word = character(0),
      y_frequency = integer(0),
      i_frequency = integer(0),
      frequency_difference = integer(0),
      has_i_version = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Create pairs by finding corresponding words
  pairs_list <- list()
  
  # Process ẏ words and find their i counterparts
  for (i in 1:nrow(y_words)) {
    y_word <- y_words$word[i]
    y_freq <- y_words$frequency[i]
    y_grapho <- if ("grapho" %in% colnames(y_words)) y_words$grapho[i] else ""
    
    # Create the corresponding i word by replacing ẏ/ẏ with i
    i_word <- swap_y_to_i(y_word)
    
    # Check if the i word exists in the data
    i_match <- word_data[word_data$word == i_word, ]
    if (nrow(i_match) > 0) {
      i_freq <- i_match$frequency[1]
      i_grapho <- if ("grapho" %in% colnames(i_match)) i_match$grapho[1] else ""
      has_i_version <- TRUE
    } else {
      i_freq <- 0
      i_grapho <- ""
      has_i_version <- FALSE
    }
    
    # Only include pairs where at least one version exists
    if (y_freq > 0 || i_freq > 0) {
      pairs_list[[length(pairs_list) + 1]] <- list(
        y_word = y_word,
        i_word = i_word,
        y_frequency = y_freq,
        i_frequency = i_freq,
        y_grapho = y_grapho,
        i_grapho = i_grapho,
        has_i_version = has_i_version
      )
    }
  }
  
  # Process i words and find their ẏ counterparts (only if not already processed)
  for (i in 1:nrow(i_words)) {
    i_word <- i_words$word[i]
    i_freq <- i_words$frequency[i]
    i_grapho <- if ("grapho" %in% colnames(i_words)) i_words$grapho[i] else ""
    
    # Create the corresponding ẏ word by replacing i with ẏ
    y_word <- swap_i_to_y(i_word)
    
    # Check if this pair was already processed from the ẏ side
    already_processed <- any(sapply(pairs_list, function(pair) {
      (pair$y_word == y_word && pair$i_word == i_word) || 
      (pair$y_word == i_word && pair$i_word == y_word)
    }))
    
    if (!already_processed) {
      # Check if the ẏ word exists in the data (ẏ or legacy ẏ spelling)
      y_match <- find_word_row(word_data, y_word)
      if (nrow(y_match) > 0) {
        y_freq <- y_match$frequency[1]
        y_grapho <- if ("grapho" %in% colnames(y_match)) y_match$grapho[1] else ""
        has_y_version <- TRUE
      } else {
        y_freq <- 0
        y_grapho <- ""
        has_y_version <- FALSE
      }
      
      # Only include pairs where at least one version exists
      if (y_freq > 0 || i_freq > 0) {
        pairs_list[[length(pairs_list) + 1]] <- list(
          y_word = y_word,
          i_word = i_word,
          y_frequency = y_freq,
          i_frequency = i_freq,
          y_grapho = y_grapho,
          i_grapho = i_grapho,
          has_i_version = has_y_version  # This indicates if the ẏ version exists
        )
      }
    }
  }
  
  if (length(pairs_list) == 0) {
    return(data.frame(
      y_word = character(0),
      i_word = character(0),
      y_frequency = integer(0),
      i_frequency = integer(0),
      frequency_difference = integer(0),
      has_i_version = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Convert to data frame
  analysis_df <- data.frame(
    y_word = sapply(pairs_list, function(x) x$y_word),
    i_word = sapply(pairs_list, function(x) x$i_word),
    y_frequency = sapply(pairs_list, function(x) x$y_frequency),
    i_frequency = sapply(pairs_list, function(x) x$i_frequency),
    y_grapho = sapply(pairs_list, function(x) x$y_grapho),
    i_grapho = sapply(pairs_list, function(x) x$i_grapho),
    has_i_version = sapply(pairs_list, function(x) x$has_i_version),
    stringsAsFactors = FALSE
  )
  
  # Calculate additional metrics
  analysis_df$frequency_difference <- analysis_df$i_frequency - analysis_df$y_frequency
  
  # Calculate percentage: y_frequency / (y_frequency + i_frequency) * 100
  total_freq <- analysis_df$y_frequency + analysis_df$i_frequency
  analysis_df$y_frequency_percent <- ifelse(total_freq > 0, 
                                           round_pct(100 * analysis_df$y_frequency / total_freq), 
                                           0)
  
  # Reorder columns
  base_cols <- c("y_word", "i_word", "y_frequency", "i_frequency", "y_frequency_percent", "frequency_difference", "has_i_version")
  
  # Add grapho columns if they exist
  if ("y_grapho" %in% colnames(analysis_df)) {
    base_cols <- c(base_cols, "y_grapho", "i_grapho")
  }
  
  analysis_df <- analysis_df[, base_cols]
  
  # Sort by frequency difference (descending) then by y_frequency
  analysis_df <- analysis_df[order(analysis_df$frequency_difference, analysis_df$y_frequency, decreasing = TRUE), ]
  
  return(analysis_df)
}

# Diphthongs whose <i> is not treated as an ẏ/i alternation slot.
YI_IGNORED_DIPHTHONGS <- c("ei", "oi", "ui")

# Grapheme clusters for ẏ/i inventory: merge <ei>, <oi>, <ui> only.
grapheme_clusters_for_y_i <- function(word) {
  normalized_word <- stringi::stri_trans_nfc(word)
  grapheme_clusters <- stringi::stri_extract_all_boundaries(normalized_word, type = "character")[[1]]
  word_string <- paste(grapheme_clusters, collapse = "")
  processed_word <- word_string
  for (diphthong in YI_IGNORED_DIPHTHONGS) {
    processed_word <- gsub(diphthong, DIPHTHONG_MARKERS[diphthong], processed_word, fixed = TRUE)
  }
  stringi::stri_extract_all_boundaries(processed_word, type = "character")[[1]]
}

# Classify standalone ẏ / i (not the <i> in <ei>, <oi>, <ui>).
classify_y_i_letters <- function(word) {
  clusters <- grapheme_clusters_for_y_i(word)
  has_y <- any(clusters == Y_GRAPH)
  has_i <- any(clusters == "i")
  if (has_y && has_i) {
    "ẏ+i"
  } else if (has_y) {
    "ẏ"
  } else if (has_i) {
    "i"
  } else {
    NA_character_
  }
}

has_y_i_outside_ignored_diphthongs <- function(word) {
  !is.na(classify_y_i_letters(word))
}

# Group word forms with standalone ẏ or i by lemma (omit rows without lemma).
summarise_y_i_by_lemma <- function(word_data, min_tokens = 1) {
  empty <- data.frame(
    lemma = character(0),
    words = character(0),
    n_forms = integer(0),
    n_tokens = integer(0),
    y_tokens = integer(0),
    i_tokens = integer(0),
    both_tokens = integer(0),
    y_share_pct = numeric(0),
    stringsAsFactors = FALSE
  )
  if (is.null(word_data) || nrow(word_data) == 0) {
    return(empty)
  }

  df <- word_data
  if (!"lemma" %in% colnames(df)) {
    return(empty)
  }
  df$lemma <- as.character(df$lemma)
  df <- df[!is.na(df$lemma) & df$lemma != "", , drop = FALSE]
  if (nrow(df) == 0) {
    return(empty)
  }

  mask <- vapply(df$word, has_y_i_outside_ignored_diphthongs, logical(1))
  df <- df[mask, , drop = FALSE]
  if (nrow(df) == 0) {
    return(empty)
  }

  df$y_i_class <- vapply(df$word, classify_y_i_letters, character(1))

  format_word_entry <- function(word, freq, class) {
    paste0(word, " (", freq, ", ", class, ")")
  }

  df |>
    group_by(lemma) |>
    summarise(
      words = {
        ord <- order(-frequency, word)
        paste(
          vapply(seq_along(ord), function(j) {
            idx <- ord[j]
            format_word_entry(word[idx], frequency[idx], y_i_class[idx])
          }, character(1)),
          collapse = ", "
        )
      },
      n_forms = dplyr::n(),
      n_tokens = sum(frequency),
      y_tokens = sum(frequency[y_i_class == "ẏ"]),
      i_tokens = sum(frequency[y_i_class == "i"]),
      both_tokens = sum(frequency[y_i_class == "ẏ+i"]),
      .groups = "drop"
    ) |>
    filter(n_tokens >= min_tokens) |>
    mutate(
      y_share_pct = ifelse(
        (y_tokens + i_tokens) > 0,
        round_pct(100 * y_tokens / (y_tokens + i_tokens)),
        NA_real_
      )
    ) |>
    arrange(desc(n_tokens), lemma)
}

# ---------------------------------------------------------------------------
# Frequency-normalized ẏ/i preference analysis
# ---------------------------------------------------------------------------

# Wilson score confidence interval for a binomial proportion (vectorized).
wilson_ci <- function(x, n, z = 1.96) {
  phat <- ifelse(n > 0, x / n, NA_real_)
  denom <- 1 + z^2 / n
  center <- (phat + z^2 / (2 * n)) / denom
  half <- (z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))) / denom
  data.frame(
    lower = ifelse(n > 0, pmax(0, center - half), NA_real_),
    upper = ifelse(n > 0, pmin(1, center + half), NA_real_)
  )
}

# Method-of-moments Beta prior with a weak fallback toward the corpus mean.
estimate_beta_prior <- function(y, n) {
  keep <- n >= 2
  m0 <- sum(y) / sum(n)
  if (sum(keep) >= 5) {
    p <- y[keep] / n[keep]
    m <- mean(p)
    v <- stats::var(p)
    if (!is.na(v) && v > 0 && v < m * (1 - m)) {
      common <- m * (1 - m) / v - 1
      return(list(alpha = m * common, beta = (1 - m) * common))
    }
  }
  # Fallback: weak prior (pseudo-count 2) centered on the token-level mean.
  k0 <- 2
  list(alpha = m0 * k0, beta = (1 - m0) * k0)
}

# The ẏ/i slot can only stand for these three phonemes; only the six pairs
# |ẏ=i| |ẏ=iè| |ẏ=î| |i=i| |i=iè| |i=î| are relevant for the analysis.
YI_ALLOWED_PHONEMES <- c("i", "iè", "î")

# Extract the phoneme that a target grapheme maps to from a |g=p|... string.
# When `allowed` is supplied, only a phoneme in that set is returned (the scan
# continues past non-matching pairs); anything else yields NA.
extract_phoneme_for_grapheme <- function(grapho, target, allowed = NULL) {
  if (is.na(grapho) || grapho == "") return(NA_character_)
  pairs <- str_extract_all(grapho, "[^|]+")[[1]]
  for (pair in pairs) {
    if (grepl("=", pair, fixed = TRUE)) {
      parts <- strsplit(pair, "=", fixed = TRUE)[[1]]
      if (length(parts) == 2 && trimws(parts[1]) == target) {
        phon <- trimws(parts[2])
        if (is.null(allowed)) return(phon)
        if (phon %in% allowed) return(phon)
      }
    }
  }
  NA_character_
}

extract_phoneme_for_y <- function(grapho, allowed = NULL) {
  extract_phoneme_for_grapheme(grapho, Y_GRAPH, allowed)
}

# Position class of the contested letter (initial / medial / final).
letter_position_class <- function(word, target) {
  if (is.na(word) || word == "") return(NA_character_)
  clusters <- stringi::stri_extract_all_boundaries(word, type = "character")[[1]]
  pos <- which(clusters == target)
  if (length(pos) == 0) return(NA_character_)
  p <- pos[1]
  if (p == 1) "initial" else if (p == length(clusters)) "final" else "medial"
}

# Preceding grapheme of the contested letter ("#" = word start).
preceding_letter <- function(word, target) {
  if (is.na(word) || word == "") return(NA_character_)
  clusters <- stringi::stri_extract_all_boundaries(word, type = "character")[[1]]
  pos <- which(clusters == target)
  if (length(pos) == 0) return(NA_character_)
  p <- pos[1]
  if (p == 1) "#" else clusters[p - 1]
}

# Following grapheme of the contested letter ("$" = word end).
following_letter <- function(word, target) {
  if (is.na(word) || word == "") return(NA_character_)
  clusters <- stringi::stri_extract_all_boundaries(word, type = "character")[[1]]
  pos <- which(clusters == target)
  if (length(pos) == 0) return(NA_character_)
  p <- pos[1]
  if (p == length(clusters)) "$" else clusters[p + 1]
}

# Graphemes treated as vowels when classifying consonant neighbours.
yolanda_vowel_graphemes <- function() {
  c(
    "a", "e", "i", "o", "u", Y_GRAPH,
    "ë", "è", "ê", "î", "â", "ô", "û", "ü", "æ", "œ", "ů"
  )
}

is_consonant_grapheme <- function(g) {
  if (is.na(g) || g %in% c("#", "$", "")) return(FALSE)
  if (g %in% DIPHTHONGS || g %in% DIGRAPHS || g %in% TRIGRAPHS) return(TRUE)
  !g %in% yolanda_vowel_graphemes()
}

# Vowel multigraphs attested in grapho but not in surface-form diphthong list.
GRAPHO_VOWEL_MULTIGRAPHS <- c("oe")

is_vowel_grapheme <- function(g) {
  if (is.na(g) || g == "" || g == "-") return(FALSE)
  g %in% DIPHTHONGS || g %in% GRAPHO_VOWEL_MULTIGRAPHS || g %in% yolanda_vowel_graphemes()
}

grapheme_freq_lookup <- function(grapheme_stats) {
  tokens <- setNames(grapheme_stats$total_frequency, grapheme_stats$grapheme)
  types <- setNames(grapheme_stats$unique_words, grapheme_stats$grapheme)
  function(key) {
    token_val <- tokens[[key]]
    type_val <- types[[key]]
    list(
      token = as.integer(round(if (is.null(token_val) || is.na(token_val)) 0 else token_val)),
      type = as.integer(if (is.null(type_val) || is.na(type_val)) 0 else type_val)
    )
  }
}

format_grapheme_freq_line <- function(label, token, type) {
  sprintf(
    "<strong>&lt;%s&gt;</strong> %s/%s",
    htmltools::htmlEscape(label),
    token,
    type
  )
}

format_grapheme_freq_cell <- function(entries, lookup) {
  if (length(entries) == 0) return("")
  lines <- vapply(entries, function(entry) {
    freq <- lookup(entry$key)
    format_grapheme_freq_line(entry$label, freq$token, freq$type)
  }, character(1))
  paste(lines, collapse = "<br/>")
}

vowel_region_table_layout <- function() {
  list(
    list(
      palatal = list(list(label = "i", key = "i")),
      velar = list(
        list(label = "u", key = "u"),
        list(label = "ů/v\u031a", key = "ů"),
        list(label = "v", key = "v"),
        list(label = "ůe", key = "ůe"),
        list(label = "ůi", key = "ůi")
      )
    ),
    list(
      palatal = list(list(label = "ẏ", key = "ẏ")),
      velar = list()
    ),
    list(
      palatal = list(
        list(label = "e", key = "e"),
        list(label = "ei", key = "ei")
      ),
      velar = list(
        list(label = "o", key = "o"),
        list(label = "oi", key = "oi"),
        list(label = "oe", key = "oe")
      )
    ),
    list(
      palatal = list(),
      velar = list(
        list(label = "a", key = "a"),
        list(label = "ai", key = "ai")
      )
    )
  )
}

build_vowel_region_table_html <- function(grapheme_stats) {
  lookup <- grapheme_freq_lookup(grapheme_stats)
  cell_style <- "border:1px solid #333; padding:8px 12px; vertical-align:top; text-align:left;"
  header_style <- paste0(cell_style, " font-weight:bold;")

  row_html <- vapply(vowel_region_table_layout(), function(row) {
    sprintf(
      "<tr><td style=\"%s\">%s</td><td style=\"%s\">%s</td></tr>",
      cell_style,
      format_grapheme_freq_cell(row$palatal, lookup),
      cell_style,
      format_grapheme_freq_cell(row$velar, lookup)
    )
  }, character(1))

  htmltools::HTML(paste0(
    "<table style=\"border-collapse:collapse;\">",
    "<thead><tr>",
    "<th style=\"", header_style, "\">Palatalbereich</th>",
    "<th style=\"", header_style, "\">Velarbereich</th>",
    "</tr></thead><tbody>",
    paste(row_html, collapse = ""),
    "</tbody></table>"
  ))
}

build_consonant_freq_html <- function(grapheme_stats) {
  consonants <- split_grapheme_type_token_freqs(grapheme_stats)$consonants
  if (nrow(consonants) == 0) {
    return(htmltools::HTML("<em>No consonant graphemes found in grapho data.</em>"))
  }

  lines <- sprintf(
    "<strong>&lt;%s&gt;</strong> %s/%s",
    htmltools::htmlEscape(consonants$Grapheme),
    consonants$Token_Frequency,
    consonants$Type_Frequency
  )
  htmltools::HTML(paste(lines, collapse = "<br/>"))
}

split_grapheme_type_token_freqs <- function(grapheme_stats) {
  vowel_mask <- vapply(grapheme_stats$grapheme, is_vowel_grapheme, logical(1))

  make_table <- function(stats) {
    if (nrow(stats) == 0) {
      return(data.frame(
        Grapheme = character(),
        Type_Frequency = numeric(),
        Token_Frequency = numeric(),
        stringsAsFactors = FALSE
      ))
    }
    df <- data.frame(
      Grapheme = stats$grapheme,
      Type_Frequency = stats$unique_words,
      Token_Frequency = round(stats$total_frequency, 0),
      stringsAsFactors = FALSE
    )
    df[order(df$Token_Frequency, decreasing = TRUE), , drop = FALSE]
  }

  list(
    vowels = make_table(grapheme_stats[vowel_mask, , drop = FALSE]),
    consonants = make_table(grapheme_stats[!vowel_mask, , drop = FALSE])
  )
}

# Phonemes in scope for ẏ/i consonant-context analysis.
YI_PHONEMES_MAIN <- c("i", "iè")

filter_y_i_main_phonemes <- function(enriched) {
  if (nrow(enriched) == 0) return(enriched)
  enriched[enriched$phoneme %in% YI_PHONEMES_MAIN, , drop = FALSE]
}

# Collapse rare factor levels using token counts.
collapse_by_token_weight <- function(x, weights, min_tokens = 10) {
  x <- as.character(x)
  weights <- as.numeric(weights)
  ok <- !is.na(x) & x != ""
  if (!any(ok)) return(x)
  totals <- tapply(weights[ok], x[ok], sum)
  keep <- names(totals)[totals >= min_tokens]
  ifelse(is.na(x) | x == "", NA_character_, ifelse(x %in% keep, x, "other"))
}

# Build an enriched per-word-type table for preference analysis.
enrich_y_i_data <- function(analysis_df, word_data) {
  if (nrow(analysis_df) == 0) return(analysis_df)
  
  has_grapho <- all(c("y_grapho", "i_grapho") %in% colnames(analysis_df))
  
  have_lemma <- "lemma" %in% colnames(word_data)
  have_norm <- "norm" %in% colnames(word_data)
  lookup_col <- function(w, col, have) {
    if (!have || is.na(w)) return(NA_character_)
    idx <- match(w, word_data$word)
    if (is.na(idx)) return(NA_character_)
    word_data[[col]][idx]
  }
  lookup_lemma <- function(w) lookup_col(w, "lemma", have_lemma)
  lookup_norm <- function(w) lookup_col(w, "norm", have_norm)
  
  n_rows <- nrow(analysis_df)
  phoneme <- character(n_rows)
  position <- character(n_rows)
  prec <- character(n_rows)
  foll <- character(n_rows)
  lemma <- character(n_rows)
  norm <- character(n_rows)
  
  for (i in seq_len(n_rows)) {
    yg <- if (has_grapho) analysis_df$y_grapho[i] else NA_character_
    ig <- if (has_grapho) analysis_df$i_grapho[i] else NA_character_
    
    ph <- extract_phoneme_for_y(yg)
    if (is.na(ph)) ph <- extract_phoneme_for_grapheme(ig, "i", allowed = YI_ALLOWED_PHONEMES)
    phoneme[i] <- ph
    
    ref_word <- analysis_df$y_word[i]
    if (is.na(ref_word) || ref_word == "" || !has_y_grapheme(ref_word)) {
      ref_word <- analysis_df$i_word[i]
    }
    y_tgt <- y_letter_in_word(ref_word)
    position[i] <- letter_position_class(ref_word, y_tgt)
    prec[i] <- preceding_letter(ref_word, y_tgt)
    foll[i] <- following_letter(ref_word, y_tgt)
    
    lm <- lookup_lemma(analysis_df$i_word[i])
    if (is.na(lm) || lm == "") lm <- lookup_lemma(analysis_df$y_word[i])
    lemma[i] <- if (is.na(lm)) NA_character_ else lm
    
    nm <- lookup_norm(analysis_df$i_word[i])
    if (is.na(nm) || nm == "") nm <- lookup_norm(analysis_df$y_word[i])
    norm[i] <- if (is.na(nm)) NA_character_ else nm
  }
  
  out <- analysis_df
  out$y_freq <- out$y_frequency
  out$i_freq <- out$i_frequency
  out$n <- out$y_freq + out$i_freq
  out$p_y_raw <- ifelse(out$n > 0, out$y_freq / out$n, NA_real_)
  out$phoneme <- ifelse(is.na(phoneme) | phoneme == "", NA_character_, phoneme)
  out$position <- position
  out$prec_letter <- prec
  out$foll_letter <- foll
  out$prec_cons <- ifelse(vapply(prec, is_consonant_grapheme, logical(1)), prec, NA_character_)
  out$foll_cons <- ifelse(vapply(foll, is_consonant_grapheme, logical(1)), foll, NA_character_)
  out$word_length <- nchar(out$i_word)
  out$length_band <- cut(
    out$word_length,
    breaks = c(-Inf, 3, 4, 5, Inf),
    labels = c("<=3", "4", "5", ">=6")
  )
  out$freq_band <- cut(
    out$n,
    breaks = c(-Inf, 2, 10, 50, 200, Inf),
    labels = c("1-2", "3-10", "11-50", "51-200", "200+")
  )
  out$lemma <- ifelse(is.na(lemma) | lemma == "", NA_character_, lemma)
  out$norm <- ifelse(is.na(norm) | norm == "", NA_character_, norm)
  
  # Restrict to genuine ẏ/i alternations: keep only words whose grapho contains
  # one of the six relevant pairs (|ẏ=i| |ẏ=iè| |ẏ=î| |i=i| |i=iè| |i=î|). This
  # drops words where the i/ẏ is part of a diphthong or unrelated context.
  out <- out[!is.na(out$phoneme) & out$n > 0, ]
  if (nrow(out) == 0) return(out)
  
  prior <- estimate_beta_prior(out$y_freq, out$n)
  out$p_y_shrunk <- (out$y_freq + prior$alpha) / (out$n + prior$alpha + prior$beta)
  
  ci <- wilson_ci(out$y_freq, out$n)
  out$ci_lower <- ci$lower
  out$ci_upper <- ci$upper
  
  attr(out, "beta_prior") <- prior
  out
}

# Token-level group preference + Wilson CI for the forest plot.
summarise_y_i_group <- function(enriched, group_col, min_tokens = 5, max_groups = 25) {
  if (nrow(enriched) == 0 || !group_col %in% colnames(enriched)) return(data.frame())
  
  df <- enriched
  df$.group <- as.character(df[[group_col]])
  df <- df[!is.na(df$.group) & df$.group != "", ]
  if (nrow(df) == 0) return(data.frame())
  
  grouped <- df |>
    group_by(.group) |>
    summarise(
      y = sum(y_freq),
      n = sum(n),
      n_words = dplyr::n(),
      .groups = "drop"
    ) |>
    filter(n >= min_tokens)
  
  if (nrow(grouped) == 0) return(data.frame())
  
  grouped <- grouped |> arrange(desc(n)) |> head(max_groups)
  ci <- wilson_ci(grouped$y, grouped$n)
  grouped$share <- grouped$y / grouped$n
  grouped$lower <- ci$lower
  grouped$upper <- ci$upper
  grouped |> arrange(desc(share))
}

create_y_i_funnel_plot <- function(enriched, z = 1.96) {
  df <- enriched[enriched$n > 0, ]
  if (nrow(df) == 0) {
    return(ggplot() + annotate("text", x = 0, y = 0, label = "No data") + theme_void())
  }
  
  m0 <- sum(df$y_freq) / sum(df$n)
  df$expected_upper <- pmin(1, m0 + z * sqrt(m0 * (1 - m0) / df$n))
  df$expected_lower <- pmax(0, m0 - z * sqrt(m0 * (1 - m0) / df$n))
  df$verdict <- ifelse(
    df$p_y_raw > df$expected_upper, "ẏ-preferring",
    ifelse(df$p_y_raw < df$expected_lower, "i-preferring", "within expected")
  )
  
  n_seq <- exp(seq(log(min(df$n)), log(max(df$n)), length.out = 200))
  band <- data.frame(
    n = n_seq,
    upper = pmin(1, m0 + z * sqrt(m0 * (1 - m0) / n_seq)),
    lower = pmax(0, m0 - z * sqrt(m0 * (1 - m0) / n_seq))
  )
  
  ggplot(df, aes(x = n, y = p_y_raw)) +
    geom_line(data = band, aes(x = n, y = upper), inherit.aes = FALSE,
              linetype = "dashed", color = "grey50") +
    geom_line(data = band, aes(x = n, y = lower), inherit.aes = FALSE,
              linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = m0, color = "grey30") +
    geom_point(aes(color = verdict, size = n), alpha = 0.75) +
    scale_x_log10() +
    scale_color_manual(values = c(
      "ẏ-preferring" = "#1b7837",
      "i-preferring" = "#762a83",
      "within expected" = "#999999"
    ), name = "Verdict (95%)") +
    scale_size_continuous(name = "Tokens (n)", range = c(1.5, 8)) +
    labs(
      title = "ẏ/i Funnel Plot: preference vs evidence",
      subtitle = paste0("Corpus ẏ rate = ", round_pct(100 * m0),
                        "%. Points outside the funnel deviate significantly."),
      x = "Total attestations per word (log scale)",
      y = "ẏ share of the word"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

create_y_i_forest_plot <- function(group_df, group_label, corpus_mean = NA_real_) {
  if (nrow(group_df) == 0) {
    return(ggplot() +
             annotate("text", x = 0, y = 0,
                      label = "No groups with enough data") +
             theme_void())
  }
  
  group_df$.group <- factor(group_df$.group, levels = rev(group_df$.group))
  
  p <- ggplot(group_df, aes(x = share, y = .group)) +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y",
                  width = 0.3, color = "grey50") +
    geom_point(aes(size = n), color = "#2166ac") +
    geom_text(aes(label = paste0("n=", n, " (", n_words, "w)")),
              hjust = -0.15, size = 3, color = "grey30") +
    scale_x_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.25)) +
    scale_size_continuous(name = "Tokens", range = c(2, 7)) +
    labs(
      title = paste0("ẏ preference by ", group_label),
      x = "ẏ share (token-based) with 95% Wilson CI",
      y = group_label
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
  
  if (!is.na(corpus_mean)) {
    p <- p + geom_vline(xintercept = corpus_mean, linetype = "dashed", color = "grey30")
  }
  p
}

# Aggregate tokens to the lexeme level (norm; fall back to the word form) and
# compute each lexeme's ẏ share. Used to show lexical bimodality.
summarise_y_i_lexeme <- function(enriched, min_tokens = 1) {
  if (nrow(enriched) == 0) return(data.frame())
  df <- enriched
  # Only the i and iè phonemes are relevant here; /î/ is the i-only phoneme and
  # is excluded from the lexeme distribution and word lists.
  df <- df[df$phoneme %in% c("i", "iè"), ]
  if (nrow(df) == 0) return(data.frame())
  df$lexeme <- ifelse(is.na(df$norm) | df$norm == "", df$i_word, df$norm)
  df |>
    group_by(lexeme) |>
    summarise(
      y = sum(y_freq),
      n = sum(n),
      n_forms = dplyr::n(),
      forms = paste(sort(unique(c(y_word[y_freq > 0], i_word[i_freq > 0]))), collapse = ", "),
      phoneme = names(which.max(tapply(y_freq + i_freq, phoneme, sum))),
      .groups = "drop"
    ) |>
    filter(n >= min_tokens) |>
    mutate(share = y / n)
}

create_y_i_lexeme_hist <- function(lex_df, min_tokens) {
  if (nrow(lex_df) == 0) {
    return(ggplot() +
             annotate("text", x = 0, y = 0, label = "No lexemes with enough tokens") +
             theme_void())
  }
  
  m0 <- sum(lex_df$y) / sum(lex_df$n)
  near0 <- sum(lex_df$share <= 0.05)
  near1 <- sum(lex_df$share >= 0.95)
  
  has_phoneme <- "phoneme" %in% colnames(lex_df) && length(unique(lex_df$phoneme)) > 1
  
  # Per-phoneme mean lexeme ẏ-share, to judge whether i vs iè separate.
  sub2 <- ""
  if (has_phoneme) {
    by_ph <- tapply(lex_df$share, lex_df$phoneme, function(s) round_pct(100 * mean(s)))
    cnt <- table(lex_df$phoneme)
    sub2 <- paste0(
      "\nMean lexeme ẏ-share by phoneme: ",
      paste(sprintf("/%s/ %s (%d lex.)", names(by_ph), format_pct(by_ph), as.integer(cnt[names(by_ph)])),
            collapse = ",  ")
    )
  }
  
  p <- ggplot(lex_df, aes(x = 100 * share))
  if (has_phoneme) {
    p <- p + geom_histogram(aes(fill = phoneme), binwidth = 5, boundary = 0, color = "white") +
      scale_fill_manual(values = c("i" = "#2166ac", "iè" = "#d6604d"), name = "Phoneme")
  } else {
    p <- p + geom_histogram(binwidth = 5, boundary = 0, fill = "#2166ac", color = "white")
  }
  
  p +
    geom_vline(xintercept = 100 * m0, linetype = "dashed", color = "grey30") +
    labs(
      title = paste0("Per-lexeme ẏ share (", nrow(lex_df), " lexemes, n >= ", min_tokens, " tokens)"),
      subtitle = paste0(
        "Dashed line = pooled mean ", round_pct(100 * m0), "% ẏ.  ",
        near1, " lexemes are >=95% ẏ, ", near0, " are <=5% ẏ (bimodal = lexically conditioned).",
        sub2
      ),
      x = "ẏ share of the lexeme (%)",
      y = "Number of lexemes"
    ) +
    scale_x_continuous(breaks = seq(0, 100, 25)) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

# Mixed-effects (or fallback fixed-effects) logistic model of the ẏ vs i choice.
# Phoneme and word are modeled as random effects: partial pooling avoids the
# complete-separation blow-up that some phonemes (e.g. /î/, /j/ ~ always i) cause,
# while position / word length / frequency are estimated as fixed effects.
fit_y_i_model <- function(enriched) {
  dat <- enriched
  dat <- dat[dat$n >= 2 & !is.na(dat$position) & !is.na(dat$length_band), ]
  if (nrow(dat) < 20) {
    return(list(table = data.frame(),
                note = "Not enough data (need >=20 word types with n>=2 and a known position)."))
  }
  
  dat$position <- factor(dat$position)
  dat$length_band <- factor(dat$length_band)
  dat$log_freq_z <- as.numeric(scale(log10(dat$n)))
  dat$word_id <- factor(seq_len(nrow(dat)))
  dat$phoneme_f <- factor(ifelse(is.na(dat$phoneme), "unknown", dat$phoneme))
  
  candidate_preds <- c("position", "length_band", "log_freq_z")
  preds <- candidate_preds[vapply(candidate_preds, function(p) {
    if (is.factor(dat[[p]])) nlevels(droplevels(dat[[p]])) >= 2 else stats::sd(dat[[p]], na.rm = TRUE) > 0
  }, logical(1))]
  
  if (length(preds) == 0) {
    return(list(table = data.frame(), note = "No predictor has enough variation to model."))
  }
  
  fixed <- paste(preds, collapse = " + ")
  use_phoneme_re <- nlevels(droplevels(dat$phoneme_f)) >= 3
  re_terms <- if (use_phoneme_re) "(1 | phoneme_f) + (1 | word_id)" else "(1 | word_id)"
  
  model <- tryCatch(
    suppressWarnings(lme4::glmer(
      stats::as.formula(paste0("cbind(y_freq, i_freq) ~ ", fixed, " + ", re_terms)),
      data = dat, family = binomial
    )),
    error = function(e) NULL
  )
  
  if (!is.null(model)) {
    coefs <- summary(model)$coefficients
    vc <- as.data.frame(lme4::VarCorr(model))
    re_txt <- paste(
      sprintf("%s (SD=%.2f)", vc$grp, vc$sdcor),
      collapse = ", "
    )
    note <- paste0(
      "Mixed-effects logistic regression (glmer). Fixed effects below; ",
      "phoneme and word are random effects (partial pooling) to avoid separation ",
      "and to absorb frequency/overdispersion. N = ", nrow(dat),
      " word types. Random-effect SDs (logit scale): ", re_txt,
      ". Positive estimate / OR>1 favors ẏ."
    )
  } else {
    glm_model <- tryCatch(
      stats::glm(
        stats::as.formula(paste0("cbind(y_freq, i_freq) ~ ", fixed)),
        data = dat, family = binomial
      ),
      error = function(e) NULL
    )
    if (is.null(glm_model)) {
      return(list(table = data.frame(), note = "Model failed to converge."))
    }
    coefs <- summary(glm_model)$coefficients
    note <- paste0("Mixed model did not converge; showing fixed-effects logistic regression (glm). N = ",
                   nrow(dat), " word types. Positive estimate / OR>1 favors ẏ.")
  }
  
  tbl <- data.frame(
    term = rownames(coefs),
    estimate_logodds = round(coefs[, 1], 3),
    odds_ratio = round(exp(coefs[, 1]), 3),
    std_error = round(coefs[, 2], 3),
    p_value = signif(coefs[, ncol(coefs)], 3),
    stringsAsFactors = FALSE
  )
  rownames(tbl) <- NULL
  list(table = tbl, note = note)
}

# Label whether a context uses more ẏ, more i, or matches the corpus (with significance).
label_y_i_context_preference <- function(share, lower, upper, corpus_share, significant) {
  ifelse(
    !significant,
    "no clear difference",
    ifelse(
      lower > corpus_share,
      "more ẏ",
      ifelse(upper < corpus_share, "more i", ifelse(share >= corpus_share, "more ẏ", "more i"))
    )
  )
}

format_consonant_preference_summary <- function(df, side_label, corpus_share) {
  if (nrow(df) == 0) {
    return(paste0("  ", side_label, ": no consonant contexts with enough tokens."))
  }
  fmt_one <- function(sub, label) {
    if (nrow(sub) == 0) return(character(0))
    sub <- sub[order(-sub$n), ]
    paste0(
      "  ", label, " (", side_label, "): ",
      paste(sprintf("<%s> %d%% ẏ (n=%d)", sub$consonant, round_pct(100 * sub$share), sub$n), collapse = ", ")
    )
  }
  y_fav <- df[df$preference == "more ẏ", , drop = FALSE]
  i_fav <- df[df$preference == "more i", , drop = FALSE]
  c(
    fmt_one(y_fav, "More ẏ than corpus"),
    fmt_one(i_fav, "More i than corpus")
  )
}

# Token-weighted ẏ share by neighbouring consonant (preceding or following).
summarise_y_i_consonant_context <- function(enriched, side = c("prec", "foll"),
                                             min_tokens = 10, max_groups = 30) {
  side <- match.arg(side)
  enriched <- filter_y_i_main_phonemes(enriched)
  if (nrow(enriched) == 0) return(data.frame())

  group_col <- if (side == "prec") "prec_cons" else "foll_cons"
  df <- summarise_y_i_group(enriched, group_col, min_tokens = min_tokens, max_groups = max_groups)
  if (nrow(df) == 0) return(df)

  m0 <- sum(enriched$y_freq) / sum(enriched$n)
  df$corpus_share <- m0
  df$i <- df$n - df$y
  df$side <- side
  df$p_value <- vapply(seq_len(nrow(df)), function(i) {
    tryCatch(
      stats::prop.test(df$y[i], df$n[i], p = m0)$p.value,
      error = function(e) NA_real_
    )
  }, numeric(1))
  df$p_adj <- stats::p.adjust(df$p_value, method = "BH")
  df$significant <- df$p_adj < 0.05
  df$context <- if (side == "prec") "preceding" else "following"
  df$preference <- label_y_i_context_preference(df$share, df$lower, df$upper, m0, df$significant)
  df |>
    rename(consonant = .group) |>
    select(context, consonant, preference, y, i, n, n_words, share, lower, upper, p_value, p_adj, significant)
}

create_y_i_consonant_forest_plot <- function(consonant_df, side = c("prec", "foll"), corpus_mean) {
  side <- match.arg(side)
  if (nrow(consonant_df) == 0) {
    return(ggplot() +
             annotate("text", x = 0, y = 0, label = "No consonant contexts with enough tokens") +
             theme_void())
  }

  plot_labels <- if (side == "prec") {
    list(
      title = "Which consonant sits before ẏ/i?",
      ylab = "Consonant to the left"
    )
  } else {
    list(
      title = "Which consonant sits after ẏ/i?",
      ylab = "Consonant to the right"
    )
  }

  plot_df <- consonant_df
  plot_df$consonant <- factor(plot_df$consonant, levels = rev(plot_df$consonant[order(plot_df$share)]))
  ggplot(plot_df, aes(x = share, y = consonant)) +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = 0.3, color = "grey50") +
    geom_point(aes(size = n, color = preference), alpha = 0.85) +
    geom_vline(xintercept = corpus_mean, linetype = "dashed", color = "grey30") +
    scale_color_manual(
      values = c(
        "more ẏ" = "#1b7837",
        "more i" = "#762a83",
        "no clear difference" = "#999999"
      ),
      name = "vs corpus"
    ) +
    scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
    scale_size_continuous(name = "Tokens", range = c(2, 8)) +
    labs(
      title = plot_labels$title,
      subtitle = paste0(
        "Corpus baseline = ", round_pct(100 * corpus_mean),
        "% ẏ (dashed line). Right of line = more ẏ; left = more i."
      ),
      x = "Share written as ẏ (remainder = i)",
      y = plot_labels$ylab
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

# Mixed-effects logistic regression: does neighbouring consonant predict ẏ vs i?
fit_y_i_consonant_model <- function(enriched, side = c("prec", "foll"), min_context_tokens = 10) {
  side <- match.arg(side)
  enriched <- filter_y_i_main_phonemes(enriched)
  group_col <- if (side == "prec") "prec_cons" else "foll_cons"

  dat <- enriched[!is.na(enriched[[group_col]]) & enriched$n >= 1, , drop = FALSE]
  if (nrow(dat) < 15) {
    return(list(
      table = data.frame(),
      note = paste0("Not enough word types with a ", side, "-consonant context (need >= 15).")
    ))
  }

  dat$word_id <- factor(paste(dat$y_word, dat$i_word, sep = "|"))
  dat$phoneme_f <- factor(dat$phoneme)
  dat$.cons <- collapse_by_token_weight(dat[[group_col]], dat$n, min_context_tokens)
  dat$.cons <- factor(dat$.cons)

  if (nlevels(droplevels(dat$.cons)) < 2) {
    return(list(
      table = data.frame(),
      note = paste0("Need at least two ", side, "-consonant groups with >= ", min_context_tokens, " tokens.")
    ))
  }

  use_phoneme <- nlevels(droplevels(dat$phoneme_f)) >= 2
  fixed <- if (use_phoneme) "cons_f + phoneme_f" else "cons_f"
  dat$cons_f <- dat$.cons
  form_re <- stats::as.formula(paste0("cbind(y_freq, i_freq) ~ ", fixed, " + (1 | word_id)"))
  form_glm <- stats::as.formula(paste0("cbind(y_freq, i_freq) ~ ", fixed))

  model <- tryCatch(
    suppressWarnings(lme4::glmer(form_re, data = dat, family = binomial)),
    error = function(e) NULL
  )

  if (is.null(model)) {
    glm_model <- tryCatch(
      stats::glm(form_glm, data = dat, family = binomial),
      error = function(e) NULL
    )
    if (is.null(glm_model)) {
      return(list(table = data.frame(), note = "Model failed to converge."))
    }
    coefs <- summary(glm_model)$coefficients
    note <- paste0(
      "Fixed-effects logistic regression (glm fallback). ",
      if (side == "prec") "Preceding" else "Following",
      " consonant + phoneme; N = ", nrow(dat),
      " word types. OR > 1 favors ẏ relative to the reference consonant."
    )
  } else {
    coefs <- summary(model)$coefficients
    note <- paste0(
      "Mixed-effects logistic regression (random intercept per word type). ",
      if (side == "prec") "Preceding" else "Following",
      " consonant + phoneme; N = ", nrow(dat),
      " word types. OR > 1 favors ẏ relative to the reference consonant ",
      "(first level in R's default treatment coding)."
    )
  }

  tbl <- data.frame(
    term = rownames(coefs),
    estimate_logodds = round(coefs[, 1], 3),
    odds_ratio = round(exp(coefs[, 1]), 3),
    std_error = round(coefs[, 2], 3),
    p_value = signif(coefs[, ncol(coefs)], 3),
    stringsAsFactors = FALSE
  )
  rownames(tbl) <- NULL
  tbl$context <- if (side == "prec") "preceding" else "following"
  list(table = tbl, note = note, corpus_share = sum(dat$y_freq) / sum(dat$n))
}

fit_y_i_consonant_analysis <- function(enriched, min_context_tokens = 10) {
  enriched <- filter_y_i_main_phonemes(enriched)
  if (nrow(enriched) == 0) {
    return(list(
      prec_summary = data.frame(),
      foll_summary = data.frame(),
      prec_model = list(table = data.frame(), note = "No /i/ or /iè/ data."),
      foll_model = list(table = data.frame(), note = "No /i/ or /iè/ data."),
      overview = "No data for phonemes /i/ or /iè/."
    ))
  }

  prec_summary <- summarise_y_i_consonant_context(
    enriched, side = "prec", min_tokens = min_context_tokens
  )
  foll_summary <- summarise_y_i_consonant_context(
    enriched, side = "foll", min_tokens = min_context_tokens
  )
  prec_model <- fit_y_i_consonant_model(enriched, side = "prec", min_context_tokens = min_context_tokens)
  foll_model <- fit_y_i_consonant_model(enriched, side = "foll", min_context_tokens = min_context_tokens)

  m0 <- sum(enriched$y_freq) / sum(enriched$n)
  n_sig_prec <- sum(prec_summary$significant, na.rm = TRUE)
  n_sig_foll <- sum(foll_summary$significant, na.rm = TRUE)

  answer_lines <- c(
    paste0(
      "Question: after certain consonants, is the vowel written more often as ẏ or as i? ",
      "Answer: compare each row to the corpus baseline (", round_pct(100 * m0),
      "% ẏ for /", paste(YI_PHONEMES_MAIN, collapse = "/, /"), "/; ",
      sum(enriched$n), " tokens in ", nrow(enriched), " word types)."
    ),
    "",
    "Pattern (significant contexts only; BH p_adj < 0.05):",
    format_consonant_preference_summary(prec_summary, "consonant before ẏ/i", m0),
    format_consonant_preference_summary(foll_summary, "consonant after ẏ/i", m0),
    "",
    paste0(
      "Counts: ", n_sig_prec, " significant preceding and ", n_sig_foll,
      " significant following consonant contexts. ",
      "Grey points = not significantly different from corpus. ",
      "Multivariate models below test the same question while controlling for /i/ vs /iè/ and word identity."
    )
  )
  overview <- paste(answer_lines, collapse = "\n")

  list(
    prec_summary = prec_summary,
    foll_summary = foll_summary,
    prec_model = prec_model,
    foll_model = foll_model,
    corpus_share = m0,
    overview = overview
  )
}

# ---------------------------------------------------------------------------
# s / long-s (\u017f) analysis
# ---------------------------------------------------------------------------

analyze_s_longs_patterns <- function(word_data) {
  longs_words <- word_data[vapply(word_data$word, has_long_s_grapheme, logical(1)), , drop = FALSE]
  round_words <- word_data[grepl("s", word_data$word, fixed = TRUE), ]

  empty_df <- function() {
    data.frame(
      longs_word = character(0),
      round_word = character(0),
      longs_frequency = integer(0),
      round_frequency = integer(0),
      frequency_difference = integer(0),
      has_round_version = logical(0),
      stringsAsFactors = FALSE
    )
  }

  if (nrow(longs_words) == 0 && nrow(round_words) == 0) {
    return(empty_df())
  }

  pairs_list <- list()

  for (i in seq_len(nrow(longs_words))) {
    longs_word <- longs_words$word[i]
    longs_freq <- longs_words$frequency[i]
    longs_grapho <- if ("grapho" %in% colnames(longs_words)) longs_words$grapho[i] else ""
    round_word <- swap_longs_to_round(longs_word)

    round_match <- word_data[word_data$word == round_word, , drop = FALSE]
    if (nrow(round_match) > 0) {
      round_freq <- round_match$frequency[1]
      round_grapho <- if ("grapho" %in% colnames(round_match)) round_match$grapho[1] else ""
      has_round_version <- TRUE
    } else {
      round_freq <- 0L
      round_grapho <- ""
      has_round_version <- FALSE
    }

    if (longs_freq > 0 || round_freq > 0) {
      pairs_list[[length(pairs_list) + 1L]] <- list(
        longs_word = longs_word,
        round_word = round_word,
        longs_frequency = longs_freq,
        round_frequency = round_freq,
        longs_grapho = longs_grapho,
        round_grapho = round_grapho,
        has_round_version = has_round_version
      )
    }
  }

  for (i in seq_len(nrow(round_words))) {
    round_word <- round_words$word[i]
    round_freq <- round_words$frequency[i]
    round_grapho <- if ("grapho" %in% colnames(round_words)) round_words$grapho[i] else ""
    longs_word <- swap_round_to_longs(round_word)

    already_processed <- any(vapply(pairs_list, function(pair) {
      (pair$longs_word == longs_word && pair$round_word == round_word) ||
        (pair$longs_word == round_word && pair$round_word == longs_word)
    }, logical(1)))

    if (!already_processed) {
      longs_match <- find_word_row(word_data, longs_word)
      if (nrow(longs_match) > 0) {
        longs_freq <- longs_match$frequency[1]
        longs_grapho <- if ("grapho" %in% colnames(longs_match)) longs_match$grapho[1] else ""
        has_longs_version <- TRUE
      } else {
        longs_freq <- 0L
        longs_grapho <- ""
        has_longs_version <- FALSE
      }

      if (longs_freq > 0 || round_freq > 0) {
        pairs_list[[length(pairs_list) + 1L]] <- list(
          longs_word = longs_word,
          round_word = round_word,
          longs_frequency = longs_freq,
          round_frequency = round_freq,
          longs_grapho = longs_grapho,
          round_grapho = round_grapho,
          has_round_version = has_longs_version
        )
      }
    }
  }

  if (length(pairs_list) == 0) {
    return(empty_df())
  }

  analysis_df <- data.frame(
    longs_word = vapply(pairs_list, function(x) x$longs_word, character(1)),
    round_word = vapply(pairs_list, function(x) x$round_word, character(1)),
    longs_frequency = vapply(pairs_list, function(x) x$longs_frequency, numeric(1)),
    round_frequency = vapply(pairs_list, function(x) x$round_frequency, numeric(1)),
    longs_grapho = vapply(pairs_list, function(x) x$longs_grapho, character(1)),
    round_grapho = vapply(pairs_list, function(x) x$round_grapho, character(1)),
    has_round_version = vapply(pairs_list, function(x) x$has_round_version, logical(1)),
    stringsAsFactors = FALSE
  )

  analysis_df$frequency_difference <- analysis_df$round_frequency - analysis_df$longs_frequency
  total_freq <- analysis_df$longs_frequency + analysis_df$round_frequency
  analysis_df$longs_frequency_percent <- ifelse(
    total_freq > 0,
    round_pct(100 * analysis_df$longs_frequency / total_freq),
    0
  )

  base_cols <- c(
    "longs_word", "round_word", "longs_frequency", "round_frequency",
    "longs_frequency_percent", "frequency_difference", "has_round_version",
    "longs_grapho", "round_grapho"
  )
  analysis_df <- analysis_df[, base_cols]
  analysis_df[order(analysis_df$frequency_difference, analysis_df$longs_frequency, decreasing = TRUE), , drop = FALSE]
}

grapho_has_s_phoneme <- function(grapho) {
  extract_phoneme_for_grapheme(grapho, "s") == "s"
}

enrich_s_longs_data <- function(analysis_df, word_data) {
  if (nrow(analysis_df) == 0) return(analysis_df)

  have_lemma <- "lemma" %in% colnames(word_data)
  have_norm <- "norm" %in% colnames(word_data)
  lookup_col <- function(w, col, have) {
    if (!have || is.na(w)) return(NA_character_)
    idx <- match(w, word_data$word)
    if (is.na(idx)) return(NA_character_)
    word_data[[col]][idx]
  }

  n_rows <- nrow(analysis_df)
  phoneme <- position <- prec <- foll <- lemma <- norm <- character(n_rows)

  for (i in seq_len(n_rows)) {
    lg <- analysis_df$longs_grapho[i]
    rg <- analysis_df$round_grapho[i]
    ph <- extract_phoneme_for_grapheme(lg, "s")
    if (is.na(ph)) ph <- extract_phoneme_for_grapheme(rg, "s")
    phoneme[i] <- ph

    ref_word <- analysis_df$longs_word[i]
    if (is.na(ref_word) || ref_word == "" || !has_long_s_grapheme(ref_word)) {
      ref_word <- analysis_df$round_word[i]
    }
    s_tgt <- s_letter_in_word(ref_word)
    position[i] <- letter_position_class(ref_word, s_tgt)
    prec[i] <- preceding_letter(ref_word, s_tgt)
    foll[i] <- following_letter(ref_word, s_tgt)

    lm <- lookup_col(analysis_df$round_word[i], "lemma", have_lemma)
    if (is.na(lm) || lm == "") lm <- lookup_col(analysis_df$longs_word[i], "lemma", have_lemma)
    lemma[i] <- lm

    nm <- lookup_col(analysis_df$round_word[i], "norm", have_norm)
    if (is.na(nm) || nm == "") nm <- lookup_col(analysis_df$longs_word[i], "norm", have_norm)
    norm[i] <- nm
  }

  out <- analysis_df
  out$y_freq <- out$longs_frequency
  out$i_freq <- out$round_frequency
  out$y_word <- out$longs_word
  out$i_word <- out$round_word
  out$n <- out$y_freq + out$i_freq
  out$p_y_raw <- ifelse(out$n > 0, out$y_freq / out$n, NA_real_)
  out$phoneme <- phoneme
  out$position <- position
  out$prec_letter <- prec
  out$foll_letter <- foll
  out$prec_cons <- ifelse(vapply(prec, is_consonant_grapheme, logical(1)), prec, NA_character_)
  out$foll_cons <- ifelse(vapply(foll, is_consonant_grapheme, logical(1)), foll, NA_character_)
  out$lemma <- ifelse(is.na(lemma) | lemma == "", NA_character_, lemma)
  out$norm <- ifelse(is.na(norm) | norm == "", NA_character_, norm)

  keep_grapho <- vapply(seq_len(nrow(out)), function(i) {
    grapho_has_s_phoneme(out$longs_grapho[i]) || grapho_has_s_phoneme(out$round_grapho[i])
  }, logical(1))
  out <- out[keep_grapho & !is.na(out$phoneme) & out$phoneme == "s" & out$n > 0, , drop = FALSE]
  if (nrow(out) == 0) return(out)

  prior <- estimate_beta_prior(out$y_freq, out$n)
  out$p_y_shrunk <- (out$y_freq + prior$alpha) / (out$n + prior$alpha + prior$beta)
  ci <- wilson_ci(out$y_freq, out$n)
  out$ci_lower <- ci$lower
  out$ci_upper <- ci$upper
  attr(out, "beta_prior") <- prior
  out
}

format_s_longs_corpus_summary <- function(enriched, raw_pairs) {
  if (nrow(enriched) == 0) {
    return("No word pairs with grapho |s=s| and attested tokens. Run analysis after loading refkorpus grapho data.")
  }
  m0 <- sum(enriched$y_freq) / sum(enriched$n)
  n_lex <- length(unique(ifelse(is.na(enriched$norm) | enriched$norm == "", enriched$i_word, enriched$norm)))
  paste0(
    "Corpus (|s=s| pairs): ", sum(enriched$n), " tokens across ", nrow(enriched),
    " word types (", n_lex, " lexemes with norm). ",
    "Pooled long-s (\u017f) share: ", round_pct(100 * m0), "%. ",
    "Raw alternation pairs in table: ", nrow(raw_pairs), "."
  )
}

summarise_s_longs_lexeme <- function(enriched, min_tokens = 1) {
  if (nrow(enriched) == 0) return(data.frame())
  df <- enriched
  df$lexeme <- ifelse(is.na(df$norm) | df$norm == "", df$i_word, df$norm)
  df |>
    group_by(lexeme) |>
    summarise(
      longs = sum(y_freq),
      n = sum(n),
      n_forms = dplyr::n(),
      forms = paste(sort(unique(c(
        longs_word[y_freq > 0],
        round_word[i_freq > 0]
      ))), collapse = ", "),
      .groups = "drop"
    ) |>
    filter(n >= min_tokens) |>
    mutate(share = longs / n)
}

create_s_longs_lexeme_hist <- function(lex_df, min_tokens) {
  if (nrow(lex_df) == 0) {
    return(ggplot() +
      annotate("text", x = 0, y = 0, label = "No lexemes with enough tokens") +
      theme_void())
  }

  m0 <- sum(lex_df$longs) / sum(lex_df$n)
  near0 <- sum(lex_df$share <= 0.05)
  near1 <- sum(lex_df$share >= 0.95)

  ggplot(lex_df, aes(x = 100 * share)) +
    geom_histogram(binwidth = 5, boundary = 0, fill = "#4d9221", color = "white") +
    geom_vline(xintercept = 100 * m0, linetype = "dashed", color = "grey30") +
    labs(
      title = paste0("Per-lexeme long-s share (", nrow(lex_df), " lexemes, n >= ", min_tokens, " tokens)"),
      subtitle = paste0(
        "Dashed line = pooled mean ", round_pct(100 * m0), "% \u017f. ",
        near1, " lexemes are >=95% \u017f, ", near0, " are <=5% \u017f."
      ),
      x = "Long-s (\u017f) share of the lexeme (%)",
      y = "Number of lexemes"
    ) +
    scale_x_continuous(breaks = seq(0, 100, 25)) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

summarise_s_longs_position <- function(enriched, min_tokens = 5) {
  df <- summarise_y_i_group(enriched, "position", min_tokens = min_tokens, max_groups = 10)
  if (nrow(df) == 0) return(df)
  df |>
    rename(position = .group) |>
    mutate(
      longs = y,
      round = n - y,
      longs_pct = round_pct(100 * share)
    ) |>
    select(position, longs, round, n, n_words, longs_pct, share, lower, upper)
}

create_s_longs_position_plot <- function(pos_df, corpus_mean) {
  if (nrow(pos_df) == 0) {
    return(ggplot() +
      annotate("text", x = 0, y = 0, label = "No position groups with enough tokens") +
      theme_void())
  }
  pos_df$position <- factor(pos_df$position, levels = c("initial", "medial", "final"))
  ggplot(pos_df, aes(x = position, y = share)) +
    geom_col(fill = "#4d9221", width = 0.6) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.8) +
    geom_hline(yintercept = corpus_mean, linetype = "dashed", color = "grey30") +
    geom_text(aes(label = paste0(longs_pct, "% \u017f\n(n=", n)), vjust = -0.5, size = 3.5) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1.12)) +
    labs(
      title = "Long-s (\u017f) share by position in the word",
      subtitle = paste0("Corpus baseline ", round_pct(100 * corpus_mean), "% \u017f (dashed)"),
      x = "Position of contested s/\u017f",
      y = "Share written as \u017f"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

label_s_longs_context_preference <- function(share, lower, upper, corpus_share, significant) {
  ifelse(
    !significant,
    "no clear difference",
    ifelse(
      lower > corpus_share,
      "more \u017f",
      ifelse(upper < corpus_share, "more s", ifelse(share >= corpus_share, "more \u017f", "more s"))
    )
  )
}

format_s_longs_consonant_summary <- function(df, side_label, corpus_share) {
  if (nrow(df) == 0) {
    return(paste0("  ", side_label, ": no consonant contexts with enough tokens."))
  }
  fmt_one <- function(sub, label) {
    if (nrow(sub) == 0) return(character(0))
    sub <- sub[order(-sub$n), ]
    paste0(
      "  ", label, " (", side_label, "): ",
      paste(sprintf("<%s> %d%% \u017f (n=%d)", sub$consonant, round_pct(100 * sub$share), sub$n), collapse = ", ")
    )
  }
  longs_fav <- df[df$preference == "more \u017f", , drop = FALSE]
  round_fav <- df[df$preference == "more s", , drop = FALSE]
  c(
    fmt_one(longs_fav, "More \u017f than corpus"),
    fmt_one(round_fav, "More s than corpus")
  )
}

summarise_s_longs_consonant_context <- function(enriched, side = c("prec", "foll"),
                                                min_tokens = 10, max_groups = 30) {
  side <- match.arg(side)
  if (nrow(enriched) == 0) return(data.frame())

  group_col <- if (side == "prec") "prec_cons" else "foll_cons"
  df <- summarise_y_i_group(enriched, group_col, min_tokens = min_tokens, max_groups = max_groups)
  if (nrow(df) == 0) return(df)

  m0 <- sum(enriched$y_freq) / sum(enriched$n)
  df$corpus_share <- m0
  df$round <- df$n - df$y
  df$p_value <- vapply(seq_len(nrow(df)), function(i) {
    tryCatch(stats::prop.test(df$y[i], df$n[i], p = m0)$p.value, error = function(e) NA_real_)
  }, numeric(1))
  df$p_adj <- stats::p.adjust(df$p_value, method = "BH")
  df$significant <- df$p_adj < 0.05
  df$preference <- label_s_longs_context_preference(df$share, df$lower, df$upper, m0, df$significant)
  df |>
    rename(consonant = .group) |>
    select(consonant, preference, y, round, n, n_words, share, lower, upper, p_value, p_adj, significant)
}

create_s_longs_consonant_forest_plot <- function(consonant_df, side = c("prec", "foll"), corpus_mean) {
  side <- match.arg(side)
  if (nrow(consonant_df) == 0) {
    return(ggplot() +
      annotate("text", x = 0, y = 0, label = "No consonant contexts with enough tokens") +
      theme_void())
  }

  plot_labels <- if (side == "prec") {
    list(title = "Consonant before s/\u017f", ylab = "Consonant to the left")
  } else {
    list(title = "Consonant after s/\u017f", ylab = "Consonant to the right")
  }

  plot_df <- consonant_df
  plot_df$consonant <- factor(plot_df$consonant, levels = rev(plot_df$consonant[order(plot_df$share)]))
  ggplot(plot_df, aes(x = share, y = consonant)) +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = 0.3, color = "grey50") +
    geom_point(aes(size = n, color = preference), alpha = 0.85) +
    geom_vline(xintercept = corpus_mean, linetype = "dashed", color = "grey30") +
    scale_color_manual(
      values = c(
        "more \u017f" = "#1b7837",
        "more s" = "#762a83",
        "no clear difference" = "#999999"
      ),
      name = "vs corpus"
    ) +
    scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
    scale_size_continuous(name = "Tokens", range = c(2, 8)) +
    labs(
      title = plot_labels$title,
      subtitle = paste0(
        "Corpus baseline = ", round_pct(100 * corpus_mean),
        "% \u017f (dashed). Right of line = more \u017f; left = more s."
      ),
      x = "Share written as \u017f (remainder = round s)",
      y = plot_labels$ylab
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

fit_s_longs_consonant_analysis <- function(enriched, min_context_tokens = 10) {
  if (nrow(enriched) == 0) {
    return(list(
      prec_summary = data.frame(),
      foll_summary = data.frame(),
      corpus_share = NA_real_,
      overview = "No data with grapho |s=s|."
    ))
  }

  prec_summary <- summarise_s_longs_consonant_context(
    enriched, side = "prec", min_tokens = min_context_tokens
  )
  foll_summary <- summarise_s_longs_consonant_context(
    enriched, side = "foll", min_tokens = min_context_tokens
  )
  m0 <- sum(enriched$y_freq) / sum(enriched$n)
  n_sig_prec <- sum(prec_summary$significant, na.rm = TRUE)
  n_sig_foll <- sum(foll_summary$significant, na.rm = TRUE)

  overview <- paste(
    c(
      paste0(
        "Pooled long-s share: ", round_pct(100 * m0), "% \u017f (",
        sum(enriched$n), " tokens, ", nrow(enriched), " word types with |s=s|)."
      ),
      "",
      "Significant contexts (BH p_adj < 0.05):",
      format_s_longs_consonant_summary(prec_summary, "consonant before s/\u017f", m0),
      format_s_longs_consonant_summary(foll_summary, "consonant after s/\u017f", m0),
      "",
      paste0(
        n_sig_prec, " significant preceding and ", n_sig_foll,
        " significant following consonant contexts."
      )
    ),
    collapse = "\n"
  )

  list(
    prec_summary = prec_summary,
    foll_summary = foll_summary,
    corpus_share = m0,
    overview = overview
  )
}

# Server logic
server <- function(input, output, session) {
  
  # Reactive values to store analysis results
  analysis_results <- reactiveVal(NULL)
  word_data <- reactiveVal(NULL)
  unique_letters <- reactiveVal(NULL)
  pending_edits <- reactiveVal(list())  # Store pending edits until save button is clicked
  
  # Reactive values for grapheme analysis
  grapheme_analysis <- reactiveVal(NULL)
  grapheme_distribution <- reactiveVal(NULL)
  
  # Reactive values for grapheme-phoneme analysis
  grapheme_phoneme_analysis <- reactiveVal(NULL)
  grapheme_to_phoneme_mappings <- reactiveVal(NULL)
  phoneme_to_grapheme_mappings <- reactiveVal(NULL)
  vowel_tables <- reactiveVal(NULL)
  rhyme_pairs_data <- reactiveVal(NULL)  # Store rhyme analysis results
  rhyme_pair_counts_data <- reactiveVal(NULL)  # Editable rhyme pair frequency table
  rhyme_pair_table_seed <- reactiveVal(NULL)  # Table render snapshot (analyze / load only)
  rhyme_pair_counts_revision <- reactiveVal(0L)  # Bumps when pair/component data changes
  
  rhyme_component_summary <- reactive({
    rhyme_pair_counts_revision()
    df <- rhyme_pair_counts_data()
    req(df)
    rhyme_component_counts_from_pairs(df)
  })
  
  # Reactive values for ẏ/i analysis
  y_i_analysis_data <- reactiveVal(NULL)

  # Reactive values for s / long-s analysis
  s_longs_analysis_data <- reactiveVal(NULL)

  # Reactive values for u/v graphie position analysis
  u_graphie_analysis_data <- reactiveVal(NULL)
  
  refresh_rhyme_pair_counts <- function(rhyme_pairs) {
    pair_counts <- rhyme_word_pair_counts(rhyme_pairs) |>
      apply_saved_rhyme_edits() |>
      add_rhyme_pair_row_ids(reset = TRUE)
    rhyme_pair_counts_data(pair_counts)
    rhyme_pair_table_seed(pair_counts)
    rhyme_pair_counts_revision(rhyme_pair_counts_revision() + 1L)
  }
  
  update_rhyme_pair_counts <- function(df, notify = TRUE) {
    df <- add_rhyme_pair_row_ids(df)
    rhyme_pair_counts_data(df)
    rhyme_pair_counts_revision(rhyme_pair_counts_revision() + 1L)
    save_path <- rhyme_pair_counts_path()
    tryCatch(
      {
        save_rhyme_pair_counts(df, path = save_path)
        if (notify) {
          showNotification(
            paste0("Rhyme edit saved to ", basename(save_path)),
            type = "message",
            duration = 3
          )
        }
      },
      error = function(e) {
        showNotification(
          paste0("Could not save rhyme edits: ", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      }
    )
    invisible(save_path)
  }
  
  # Load saved rhyme pair edits on startup (before first analysis)
  observe({
    if (!is.null(isolate(rhyme_pair_counts_data()))) {
      return()
    }
    saved <- load_saved_rhyme_pair_counts()
    if (is.null(saved) || nrow(saved) == 0L) {
      return()
    }
    rhyme_pair_counts_data(saved)
    rhyme_pair_table_seed(saved)
    rhyme_pair_counts_revision(1L)
  })
  
  # Load word data on app start
  observe({
    # Load CSV with kmhd. column if present; otherwise bootstrap from word_list.txt
    data <- load_base_word_list()
    word_data(data)
    letters <- extract_unique_letters(data)
    unique_letters(letters)
    
    # Update search letter dropdown
    updateSelectInput(session, "search_letter", 
                      choices = letters,
                      selected = if (Y_GRAPH %in% letters) {
                        Y_GRAPH
                      } else {
                        letters[1]
                      })
  })
  
  # Clear ẏ/i and s/\u017f analysis when word data changes
  observeEvent(word_data(), {
    y_i_analysis_data(NULL)
    s_longs_analysis_data(NULL)
    u_graphie_analysis_data(NULL)
  }, ignoreInit = TRUE)
  
  # Clear selections when search letter changes
  observeEvent(input$search_letter, {
    selected_row(NULL)
    selected_word(NULL)
    selected_variant_lemma(NULL)
  })
  
  # Perform analysis when analyze button is clicked
  observeEvent(input$analyze, {
    req(word_data())
    req(input$search_letter)
    
    # Clear previous selections when starting new analysis
    selected_row(NULL)
    selected_word(NULL)
    selected_variant_lemma(NULL)
    
    withProgress(message = "Analyzing letter combinations...", {
      results <- analyze_letter_combinations_enhanced(
        word_data(), 
        input$search_letter, 
        input$max_context
      )
      analysis_results(results)
    })
    
    # Also analyze rhyming words
    withProgress(message = "Analyzing rhyming words...", {
      rhyme_pairs <- analyze_rhyming_words("editioYolanda.txt")
      rhyme_pairs_data(rhyme_pairs)
      refresh_rhyme_pair_counts(rhyme_pairs)
    })
  })
  
  # Analyze rhymes when analyze rhymes button is clicked
  observeEvent(input$analyze_rhymes, {
    withProgress(message = "Analyzing rhyming words...", {
      rhyme_pairs <- analyze_rhyming_words("editioYolanda.txt")
      rhyme_pairs_data(rhyme_pairs)
      refresh_rhyme_pair_counts(rhyme_pairs)
      
      # Automatically save CSV after analysis
      if (length(rhyme_pairs) > 0) {
        df <- rhyme_pairs_to_dataframe(rhyme_pairs, word_data())
        csv_filename <- paste0("yolanda_rhyme_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        write.csv(df, csv_filename, row.names = FALSE, fileEncoding = "UTF-8")
        showNotification(paste0("CSV automatically saved as: ", csv_filename), type = "default")
      }
    })
  })
  
  # Analyze rhymes when standalone analyze rhymes button is clicked
  observeEvent(input$analyze_rhymes_standalone, {
    withProgress(message = "Analyzing rhyming words...", {
      rhyme_pairs <- analyze_rhyming_words("editioYolanda.txt")
      rhyme_pairs_data(rhyme_pairs)
      refresh_rhyme_pair_counts(rhyme_pairs)
      
      # Automatically save CSV after analysis
      if (length(rhyme_pairs) > 0) {
        df <- rhyme_pairs_to_dataframe(rhyme_pairs, word_data())
        csv_filename <- paste0("yolanda_rhyme_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        write.csv(df, csv_filename, row.names = FALSE, fileEncoding = "UTF-8")
        showNotification(paste0("CSV automatically saved as: ", csv_filename), type = "default")
      }
    })
  })
  
  # Analyze graphemes when analyze graphemes button is clicked
  observeEvent(input$analyze_graphemes, {
    req(word_data())
    
    withProgress(message = "Analyzing graphemes...", {
      grapheme_results <- analyze_graphemes_comprehensive(word_data())
      grapheme_analysis(grapheme_results)
      grapheme_distribution(grapheme_results$stats)
      
      showNotification("Grapheme analysis completed!", type = "message")
    })
  })
  
  observeEvent(input$analyze_u_graphie, {
    req(word_data())

    withProgress(message = "Analysiere u/v-Graphien nach Position...", {
      u_graphie_analysis_data(analyze_u_graphie_position(word_data()))
      showNotification("u/v-Graphie-Analyse abgeschlossen.", type = "message")
    })
  })

  # Analyze grapheme-phoneme mappings when button is clicked
  observeEvent(input$analyze_grapheme_phoneme, {
    req(word_data())
    
    withProgress(message = "Analyzing grapheme-phoneme mappings...", {
      # Extract mappings from grapho column
      mappings <- analyze_grapheme_phoneme_mappings(word_data())
      
      if (nrow(mappings) > 0) {
        # Analyze phoneme to grapheme mappings (many-to-one relationship)
        phoneme_to_grapheme <- analyze_phoneme_to_grapheme(mappings)
        
        # Analyze grapheme to phoneme mappings (one-to-many relationship)
        grapheme_to_phoneme <- analyze_grapheme_to_phoneme(mappings)
        
        # Create vowel tables
        vowel_tables_data <- create_vowel_tables(mappings)
        
        # Store results
        grapheme_phoneme_analysis(mappings)
        grapheme_to_phoneme_mappings(grapheme_to_phoneme)
        phoneme_to_grapheme_mappings(phoneme_to_grapheme)
        vowel_tables(vowel_tables_data)
        
        showNotification("Grapheme-phoneme analysis completed!", type = "message")
      } else {
        showNotification("No grapheme-phoneme mappings found in grapho column!", type = "warning")
      }
    })
  })

  # Sankey tab: compute mappings on demand (reuses the shared reactiveVal).
  observeEvent(input$build_sankey, {
    req(word_data())
    if (is.null(grapheme_phoneme_analysis())) {
      withProgress(message = "Extracting grapheme-phoneme mappings...", {
        mappings <- analyze_grapheme_phoneme_mappings(word_data())
        if (nrow(mappings) > 0) {
          grapheme_phoneme_analysis(mappings)
        } else {
          showNotification("No grapheme-phoneme mappings found in grapho column!", type = "warning")
        }
      })
    }
  })

  output$sankey_status_indicator <- renderUI({
    if (is.null(grapheme_phoneme_analysis())) {
      tags$span(style = "color: #d6604d;",
                "No mappings loaded yet \u2013 click \"Build / Refresh Sankey\".")
    } else {
      tags$span(style = "color: #2166ac;",
                paste0(nrow(grapheme_phoneme_analysis()), " mappings loaded."))
    }
  })

  build_sankey_plot <- function(for_export = FALSE) {
    shiny::validate(shiny::need(
      sankey_ggsankeyfier_available(),
      sankey_ggsankeyfier_install_hint()
    ))
    mappings <- grapheme_phoneme_analysis()
    shiny::validate(shiny::need(
      !is.null(mappings),
      "Click \"Build / Refresh Sankey\" to generate the diagram."
    ))

    prep <- prepare_grapheme_phoneme_sankey_data(
      mappings,
      input$sankey_count_mode,
      input$sankey_min_count,
      input$sankey_max_links
    )
    shiny::validate(shiny::need(
      !is.null(prep),
      "No links match the current filters \u2013 lower the minimum link count."
    ))

    label_size <- input$sankey_label_size
    base_size <- input$sankey_base_size
    title_size <- input$sankey_title_size
    if (isTRUE(for_export)) {
      export_h <- sankey_export_height_inches(
        nrow(prep$agg),
        base_height = input$sankey_export_height
      )
      font_scale <- sankey_export_font_scale(export_h)
      label_size <- label_size * font_scale * SANKEY_EXPORT_LABEL_BOOST
      base_size <- base_size * font_scale
      title_size <- title_size * font_scale
    }

    p <- create_grapheme_phoneme_sankey_ggplot(
      mappings,
      count_mode = input$sankey_count_mode,
      min_count = input$sankey_min_count,
      max_links = input$sankey_max_links,
      base_size = base_size,
      label_size = label_size,
      title_size = title_size,
      flow_alpha = input$sankey_flow_alpha,
      layout = if (isTRUE(for_export)) "export" else "preview"
    )
    shiny::validate(shiny::need(
      !is.null(p),
      "No links match the current filters \u2013 lower the minimum link count."
    ))
    p
  }

  output$grapheme_phoneme_sankey <- renderPlot({
    build_sankey_plot()
  }, height = 850, res = 120)

  output$download_sankey_png <- downloadHandler(
    filename = function() {
      paste0(
        "grapheme_phoneme_sankey_",
        input$sankey_export_dpi, "dpi_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".png"
      )
    },
    content = function(file) {
      prep <- prepare_grapheme_phoneme_sankey_data(
        grapheme_phoneme_analysis(),
        input$sankey_count_mode,
        input$sankey_min_count,
        input$sankey_max_links
      )
      export_h <- sankey_export_height_inches(
        nrow(prep$agg),
        base_height = input$sankey_export_height
      )
      p <- build_sankey_plot(for_export = TRUE)
      save_grapheme_phoneme_sankey_ggplot(
        p,
        file,
        width = input$sankey_export_width,
        height = export_h,
        dpi = input$sankey_export_dpi
      )
    }
  )

  output$download_sankey_pdf <- downloadHandler(
    filename = function() {
      paste0("grapheme_phoneme_sankey_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      prep <- prepare_grapheme_phoneme_sankey_data(
        grapheme_phoneme_analysis(),
        input$sankey_count_mode,
        input$sankey_min_count,
        input$sankey_max_links
      )
      export_h <- sankey_export_height_inches(
        nrow(prep$agg),
        base_height = input$sankey_export_height
      )
      p <- build_sankey_plot(for_export = TRUE)
      save_grapheme_phoneme_sankey_ggplot(
        p,
        file,
        width = input$sankey_export_width,
        height = export_h
      )
    }
  )

  sankey_typology_data <- reactive({
    req(grapheme_phoneme_analysis())
    compute_sankey_correspondence_typology(
      grapheme_phoneme_analysis(),
      count_mode = input$sankey_count_mode
    )
  })

  output$sankey_typology_summary <- renderUI({
    req(grapheme_phoneme_analysis())
    typ <- sankey_typology_data()
    sys <- typ$system
    if (length(sys) == 0 || is.na(sys$mutual_information)) {
      return(tags$p(style = "color: #666;", "Keine Typologie-Daten verf\u00fcgbar."))
    }

    tags$div(
      style = "margin-bottom: 12px; padding: 10px 12px; background: #f7f7f7; border-radius: 4px;",
      tags$p(
        tags$strong("Systemkennzahlen "),
        sprintf(
          "(%s, Count by): I(G;P) = %.3f bit; H(P) = %.3f; H(P|G) = %.3f; H(G) = %.3f; H(G|P) = %.3f",
          typ$metrics_unit_label,
          sys$mutual_information,
          sys$entropy_phoneme,
          sys$entropy_phoneme_given_grapheme,
          sys$entropy_grapheme,
          sys$entropy_grapheme_given_phoneme
        )
      ),
      tags$p(
        style = "margin-bottom: 0; color: #444;",
        "I(G;P) misst die globale Transparenz; hohe Werte = starke Kopplung von Graphem und Phonem."
      )
    )
  })

  output$sankey_combined_typology_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    df <- sankey_typology_data()$combined
    shiny::validate(shiny::need(nrow(df) > 0, "Keine Typologie-Daten verf\u00fcgbar."))

    df <- df |>
      mutate(
        Entropie = round(Entropie, 2),
        Dominanz = round(Dominanz, 2)
      )

    DT::datatable(
      df,
      filter = "top",
      options = list(
        paging = TRUE,
        pageLength = 25,
        order = list(list(4, "desc")),
        columnDefs = list(
          list(targets = c(2, 3, 4, 5), className = "dt-right"),
          list(targets = 6, width = "420px"),
          list(targets = 7, width = "220px")
        )
      ),
      rownames = FALSE,
      selection = "none",
      caption = paste0(
        "Kombinierte Typologie (G\u2192P und P\u2192G): N = Typen (Wortformen); ",
        "Entropie/Dominanz (", tolower(sankey_typology_data()$metrics_unit_label), ", Count by)"
      )
    )
  })

  output$sankey_publication_methodology <- renderUI({
    req(grapheme_phoneme_analysis())
    typ <- sankey_typology_data()
    lines <- publication_typology_methodology_lines(
      input$sankey_count_mode,
      typ$metrics_unit_label
    )
    tags$div(
      class = "sankey-publication-methodology",
      style = "margin-bottom: 14px; padding: 12px 14px; background: #f7f7f7; border-radius: 4px;",
      tags$p(
        style = "margin-top: 0;",
        tags$strong("Publikationstabellen "),
        "werden automatisch aus den Korpusdaten berechnet (Count by: ",
        if (input$sankey_count_mode == "type") "Typen" else "Token",
        "). Kopierbar \u00fcber CSV-Download inkl. Methodik."
      ),
      tags$details(
        tags$summary(tags$strong("Methodik, Begriffe und Berechnung")),
        tags$ul(
          style = "margin-bottom: 0; padding-left: 1.2em;",
          lapply(lines, function(line) tags$li(line))
        )
      )
    )
  })

  render_sankey_publication_table <- function(table_key, empty_message) {
    typ <- sankey_typology_data()
    pub <- typ$publication[[table_key]]
    shiny::validate(shiny::need(nrow(pub) > 0, empty_message))

    numeric_cols <- which(names(pub) %in% c("N", "Grad", "Entropie", "N (Graphem)", "N (Phonem)")) - 1L
    partner_col <- which(grepl("Zuordnungen|\u2192", names(pub))) - 1L
    column_defs <- list(list(targets = numeric_cols, className = "dt-right"))
    if (length(partner_col) > 0L) {
      column_defs <- c(column_defs, list(list(targets = partner_col, width = "420px")))
    }

    DT::datatable(
      pub,
      options = list(
        dom = "t",
        ordering = FALSE,
        columnDefs = column_defs
      ),
      rownames = FALSE,
      selection = "none",
      caption = publication_table_subcaption(
        table_key,
        input$sankey_count_mode,
        typ$metrics_unit_label
      )
    )
  }

  output$sankey_pub_biunique_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    render_sankey_publication_table(
      "biunique",
      "Keine eineindeutigen Biunique-Kernpaare."
    )
  })

  output$sankey_pub_eindeutig_p_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    render_sankey_publication_table(
      "eindeutig_p",
      "Kein eindeutiger L\u00f6sungsraum P\u2192G."
    )
  })

  output$sankey_pub_eindeutig_g_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    render_sankey_publication_table(
      "eindeutig_g",
      "Keine Monophonie (G\u2192P)."
    )
  })

  output$sankey_pub_loesungsraum_dom_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    render_sankey_publication_table(
      "loesungsraum_dominant",
      "Kein L\u00f6sungsraum P\u2192G (dominant)."
    )
  })

  output$sankey_pub_loesungsraum_diffus_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    render_sankey_publication_table(
      "loesungsraum_diffus",
      "Kein diffuser L\u00f6sungsraum P\u2192G."
    )
  })

  output$sankey_pub_polyvalenz_dom_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    render_sankey_publication_table(
      "polyvalenz_dominant",
      "Keine dominante Polyvalenz G\u2192P."
    )
  })

  output$sankey_pub_polyvalenz_polyphon_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    render_sankey_publication_table(
      "polyvalenz_polyphon",
      "Keine polyphone Polyvalenz G\u2192P."
    )
  })

  output$download_sankey_typology_csv <- downloadHandler(
    filename = function() {
      paste0(
        "sankey_typology_",
        input$sankey_count_mode, "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv"
      )
    },
    content = function(file) {
      req(grapheme_phoneme_analysis())
      typ <- sankey_typology_data()
      write.csv(typ$combined, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$download_sankey_publication_csv <- downloadHandler(
    filename = function() {
      paste0(
        "sankey_publikation_",
        input$sankey_count_mode, "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv"
      )
    },
    content = function(file) {
      req(grapheme_phoneme_analysis())
      typ <- sankey_typology_data()
      export_df <- export_correspondence_publication_csv(
        typ$publication,
        input$sankey_count_mode,
        typ$metrics_unit_label
      )
      write.csv(export_df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Analyze ẏ/i patterns when button is clicked
  observeEvent(input$analyze_y_i, {
    req(word_data())
    
    withProgress(message = "Analyzing ẏ/i patterns...", {
      y_i_results <- analyze_y_i_patterns(word_data())
      y_i_analysis_data(y_i_results)
      
      showNotification("ẏ/i analysis completed!", type = "message")
    })
  })

  observeEvent(input$analyze_s_longs, {
    req(word_data())
    withProgress(message = "Analyzing s/\u017f patterns...", {
      s_longs_analysis_data(analyze_s_longs_patterns(word_data()))
      showNotification("s/\u017f analysis completed!", type = "message")
    })
  })
  
  # Summary statistics
  output$summary_stats <- renderText({
    req(word_data())
    req(input$search_letter)
    
    word_matches <- str_detect(word_data()$word, fixed(input$search_letter, ignore_case = TRUE))
    total_words <- sum(word_matches, na.rm = TRUE)
    total_freq <- sum(word_data()$frequency[word_matches], na.rm = TRUE)
    
    paste0(
      "Unique words ", nrow(word_data()), "\n",
      "Unique letters: ", length(unique_letters()), "\n",
      "Unique words with '", input$search_letter, "': ", total_words, "\n",
      "Tokens with '", input$search_letter, "': ", total_freq, "\n",
      "Max context: ", input$max_context, " letters"
    )
  })
  
  # Rhyme summary statistics
  output$rhyme_summary <- renderText({
    if (is.null(rhyme_pairs_data())) {
      return("Click 'Analyze Rhymes' to start rhyme analysis")
    }
    
    total_pairs <- length(rhyme_pairs_data())
    unique_words1 <- length(unique(sapply(rhyme_pairs_data(), function(x) x$word1)))
    unique_words2 <- length(unique(sapply(rhyme_pairs_data(), function(x) x$word2)))
    
    # Get unique rhymes and their frequencies
    all_rhymes <- sapply(rhyme_pairs_data(), function(x) x$rhyme)
    unique_rhymes <- length(unique(all_rhymes))
    
    # Find most common rhymes
    rhyme_freq <- table(all_rhymes)
    top_rhymes <- head(sort(rhyme_freq, decreasing = TRUE), 5)
    top_rhymes_text <- paste(sapply(names(top_rhymes), function(r) paste0(r, " (", top_rhymes[r], ")")), collapse = ", ")
    
    paste0(
      "Total rhyme pairs: ", total_pairs, "\n",
      "Unique words in odd lines: ", unique_words1, "\n",
      "Unique words in even lines: ", unique_words2, "\n",
      "Unique rhyme patterns: ", unique_rhymes, "\n",
      "Top rhymes: ", top_rhymes_text, "\n",
      "Lines analyzed: 1-", max(sapply(rhyme_pairs_data(), function(x) x$line2_number)), "\n",
      "Isolated lines ignored: ", length(ISOLATED_LINES)
    )
  })
  
  # Left combinations table
  output$left_table <- DT::renderDataTable({
    req(analysis_results())
    
    df <- combinations_to_dataframe(analysis_results()$left, "left", input$search_letter)
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No left combinations found"))
    }
    
    DT::datatable(df, 
                  filter = "top",
                  options = list(
                    pageLength = 25, 
                    order = list(list(4, 'desc')),
                    columnDefs = list(
                      list(targets = 0, width = "150px"),  # Context column
                      list(targets = 1, width = "60px"),   # Size column
                      list(targets = 2, width = "100px"),  # Type_Frequency column
                      list(targets = 3, width = "120px")   # Token_Frequency column
                    )
                  ),
                  selection = list(mode = 'single', target = 'row'),
                  caption = paste0("Left combinations for letter '", input$search_letter, "'"))
  })
  
  # Right combinations table
  output$right_table <- DT::renderDataTable({
    req(analysis_results())
    
    df <- combinations_to_dataframe(analysis_results()$right, "right", input$search_letter)
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No right combinations found"))
    }
    
    DT::datatable(df, 
                  filter = "top",
                  options = list(
                    pageLength = 25, 
                    order = list(list(4, 'desc')),
                    columnDefs = list(
                      list(targets = 0, width = "100px"),  # Context column
                      list(targets = 1, width = "60px"),   # Size column
                      list(targets = 2, width = "100px"),  # Type_Frequency column
                      list(targets = 3, width = "100px")   # Token_Frequency column
                    )
                  ),
                  selection = list(mode = 'single', target = 'row'),
                  caption = paste0("Right combinations for letter '", input$search_letter, "'"))
  })
  
  # Combined combinations table
  output$combined_table <- DT::renderDataTable({
    req(analysis_results())
    
    df <- combinations_to_dataframe(analysis_results()$combined, "combined", input$search_letter)
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No combined combinations found"))
    }
    
    DT::datatable(df, 
                  filter = "top",
                  options = list(
                    pageLength = 25, 
                    order = list(list(6, 'desc')),
                    columnDefs = list(
                      list(targets = 0, width = "60px"),  # Left_Context column
                      list(targets = 1, width = "60px"),  # Right_Context column
                      list(targets = 2, width = "40px"),   # Left_Size column
                      list(targets = 3, width = "40px"),   # Right_Size column
                      list(targets = 4, width = "60px"),  # Type_Frequency column
                      list(targets = 5, width = "60px")   # Token_Frequency column
                    )
                  ),
                  selection = list(mode = 'single', target = 'row'),
                  caption = paste0("Combined combinations for letter '", input$search_letter, "'"))
  })
  
  # All words table
  output$all_words_table <- DT::renderDataTable({
    req(word_data())
    req(input$search_letter)
    
    df <- get_all_words_with_letter(word_data(), input$search_letter)
    
    if (nrow(df) == 0) {
      return(data.frame(Message = paste0("No words found containing '", input$search_letter, "'")))
    }
    
    # Create column definitions for better display
    col_defs <- list()
    if ("Word" %in% colnames(df)) {
      col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "Word") - 1, width = "120px")))
    }
    if ("Frequency" %in% colnames(df)) {
      col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "Frequency") - 1, width = "80px")))
    }
    if ("norm" %in% colnames(df)) {
      col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "norm") - 1, width = "120px")))
    }
    if ("lemma" %in% colnames(df)) {
      col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "lemma") - 1, width = "120px")))
    }
    if ("grapho" %in% colnames(df)) {
      col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "grapho") - 1, width = "200px")))
    }
    
    DT::datatable(df, 
                  filter = "top",
                  options = dt_add_word_field_regex_filters(list(
                    paging = TRUE,
                    pageLength = 25,
                    lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
                    dom = '<"top"lpf>rt<"clear">',
                    order = list(list(which(colnames(df) == "Frequency") - 1, 'desc')),
                    columnDefs = col_defs,
                    scrollX = TRUE
                  ), colnames(df)),
                  rownames = FALSE,
                  selection = list(mode = 'single', target = 'row'),
                  caption = paste0("All words containing '", input$search_letter, "' with refkorpus data"))
  })
  
  rhyme_analysis_empty_table <- function(message, caption) {
    DT::datatable(
      data.frame(Message = message, stringsAsFactors = FALSE),
      options = list(pageLength = 1, dom = "t"),
      rownames = FALSE,
      caption = caption
    )
  }
  
  # Rhyme pair frequency table
  output$rhyme_pair_counts_table <- DT::renderDT({
    if (is.null(rhyme_pair_table_seed())) {
      return(rhyme_analysis_empty_table(
        "Click 'Analyze Rhymes' button above to analyze rhyming words in the text.",
        "Rhyme Pair Frequencies"
      ))
    }
    
    df <- rhyme_pair_table_seed()
    
    if (nrow(df) == 0) {
      return(rhyme_analysis_empty_table(
        "No rhyming word pairs found.",
        "Rhyme Pair Frequencies"
      ))
    }
    
    rhyme_col_index <- which(colnames(df) == "Rhyme") - 1
    pair_id_col_index <- which(colnames(df) == "._pair_id") - 1
    non_editable_cols <- setdiff(seq(0, ncol(df) - 1), rhyme_col_index)
    
    DT::datatable(
      df,
      filter = "top",
      editable = list(target = "cell", disable = list(columns = non_editable_cols)),
      options = dt_add_word_field_regex_filters(list(
        pageLength = 50,
        ordering = FALSE,
        lengthMenu = list(c(25, 50, 100, 250, -1), c("25", "50", "100", "250", "All")),
        dom = '<"top"lpf>rt<"clear">',
        columnDefs = list(
          list(targets = c(0, 1), width = "120px"),
          list(targets = 2, width = "100px"),
          list(targets = 3, width = "80px", className = "dt-right"),
          list(targets = pair_id_col_index, visible = FALSE)
        )
      ), colnames(df)),
      rownames = FALSE,
      selection = "none",
      caption = "Rhyme Pair Frequencies (Rhyme column editable; sorted by Count desc)"
    )
  })
  
  rhyme_pair_counts_proxy <- DT::dataTableProxy("rhyme_pair_counts_table")
  
  observeEvent(input$rhyme_pair_counts_table_cell_edit, {
    info <- parse_dt_cell_edit(input$rhyme_pair_counts_table_cell_edit)
    if (is.null(info) || nrow(info) == 0L) {
      return()
    }
    
    df <- rhyme_pair_counts_data()
    req(nrow(df) > 0)
    
    rhyme_col <- which(colnames(df) == "Rhyme") - 1L
    info <- info[info$col == rhyme_col, , drop = FALSE]
    if (nrow(info) == 0L) {
      return()
    }
    
    old_rhymes <- normalize_saved_rhyme(df$Rhyme)
    df <- DT::editData(df, info, rownames = FALSE)
    df$Rhyme <- normalize_saved_rhyme(df$Rhyme)
    
    if (identical(old_rhymes, df$Rhyme)) {
      return()
    }
    
    update_rhyme_pair_counts(df)
    DT::replaceData(
      rhyme_pair_counts_proxy,
      df,
      resetPaging = FALSE,
      rownames = FALSE
    )
  }, ignoreInit = TRUE)
  
  # Rhyming component frequency table
  output$rhyme_component_counts_table <- DT::renderDataTable({
    if (is.null(rhyme_pair_counts_data())) {
      return(rhyme_analysis_empty_table(
        "Click 'Analyze Rhymes' button above to analyze rhyming words in the text.",
        "Rhyming Component Frequencies"
      ))
    }
    
    df <- rhyme_component_summary()
    
    if (nrow(df) == 0) {
      return(rhyme_analysis_empty_table(
        "No rhyming components found.",
        "Rhyming Component Frequencies"
      ))
    }
    
    DT::datatable(
      df,
      filter = "top",
      options = dt_add_word_field_regex_filters(list(
        pageLength = 25,
        order = list(list(1, "desc")),
        lengthMenu = list(c(25, 50, 100, 250, -1), c("25", "50", "100", "250", "All")),
        dom = '<"top"lpf>rt<"clear">',
        columnDefs = list(
          list(targets = 0, width = "120px"),
          list(targets = 1, width = "80px", className = "dt-right")
        )
      ), colnames(df)),
      rownames = FALSE,
      selection = "none",
      caption = "Rhyming Component Frequencies"
    )
  })

  output$rhyme_component_publication_table <- renderUI({
    rhyme_pair_counts_revision()
    
    if (is.null(rhyme_pair_counts_data())) {
      return(
        htmltools::tags$p(
          class = "text-muted",
          "Click 'Analyze Rhymes' button above to analyze rhyming words in the text."
        )
      )
    }

    components <- rhyme_component_summary()
    pub <- build_rhyme_component_publication_table(components)

    if (nrow(pub) == 0L) {
      return(htmltools::tags$p(class = "text-muted", "No rhyming components found."))
    }

    total_pairs <- sum(rhyme_pair_counts_data()$Count)
    render_rhyme_component_publication_html(
      pub,
      caption_text = rhyme_component_publication_caption(
        total_pairs = total_pairs,
        grouped_rows = nrow(components),
        n_cols = 3L
      ),
      n_cols = 3L,
      revision = rhyme_pair_counts_revision()
    )
  })
  
  outputOptions(output, "rhyme_component_publication_table", suspendWhenHidden = FALSE)
  outputOptions(output, "rhyme_component_counts_table", suspendWhenHidden = FALSE)

  output$download_rhyme_component_publication_csv <- downloadHandler(
    filename = function() {
      paste0("reimkomponenten_publikation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(rhyme_pair_counts_data())
      components <- rhyme_component_summary()
      pub <- build_rhyme_component_publication_table(components)
      total_pairs <- sum(rhyme_pair_counts_data()$Count)
      meta <- data.frame(
        Reimkomponente = c(
          "--- Methodik ---",
          paste0("Endreim-Paare gesamt (n): ", total_pairs),
          paste0("Distinkte gruppierte Komponenten: ", nrow(components)),
          "Spalten: Reimkomponente | n (absolute H\u00e4ufigkeit) | Anteil (%)",
          "Gruppierung: t/d, v/u, \u1e8f/i, g/ch, s/\u017f bei sonst identischem Auslaut",
          "Leerer Auslaut (\u2014): kein gemeinsamer Reimrest in der automatischen Extraktion",
          "--- Daten (alle Komponenten) ---"
        ),
        n = NA_integer_,
        `Anteil (%)` = NA_character_,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      write.csv(
        dplyr::bind_rows(meta, pub),
        file,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  )

  # Rhyme pairs table
  output$rhyme_pairs_table <- DT::renderDataTable({
    if (is.null(rhyme_pairs_data())) {
      # No rhyme data yet - show instruction message
      return(DT::datatable(
        data.frame(
          Message = "Click 'Analyze Rhymes' button above to analyze rhyming words in the text.",
          stringsAsFactors = FALSE
        ),
        options = list(pageLength = 1, dom = 't'),
        rownames = FALSE,
        caption = "Rhyme Analysis Instructions"
      ))
    }
    
    df <- rhyme_pairs_to_dataframe(rhyme_pairs_data(), word_data())
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No rhyming word pairs found."))
    }
    
    DT::datatable(df, 
                  options = dt_add_word_field_regex_filters(list(
                    pageLength = 25, 
                    order = list(list(0, 'asc')), # Sort by Line1_Number
                    lengthMenu = list(c(25, 50, 100, 250, -1), c('25', '50', '100', '250', 'All')),
                    dom = '<"top"lpf>rt<"clear">',
                    columnDefs = list(
                      list(targets = 0, width = "80px"),  # Line1 column
                      list(targets = 1, width = "80px"),  # Line2 column
                      list(targets = 2, width = "120px"), # Word1 column
                      list(targets = 3, width = "120px"), # Word2 column
                      list(targets = 4, width = "100px"), # Rhyme column
                      list(targets = 5, width = "200px"), # Line1_Text column
                      list(targets = 6, width = "200px"), # Line2_Text column
                      list(targets = 7, width = "200px"), # Word1_Grapho column
                      list(targets = 8, width = "200px")  # Word2_Grapho column
                    )
                  ), colnames(df)),
                  filter = "top",
                  selection = list(mode = 'single', target = 'row'),
                  caption = "Rhyming Word Pairs")
  })
  
  # Reactive values to store selected row information
  selected_row <- reactiveVal(NULL)
  selected_word <- reactiveVal(NULL)
  selected_variant_lemma <- reactiveVal(NULL)
  
  # Handle row selection from left combinations table
  observeEvent(input$left_table_rows_selected, {
    if (!is.null(input$left_table_rows_selected) && length(input$left_table_rows_selected) > 0) {
      selected_variant_lemma(NULL)
      df <- combinations_to_dataframe(analysis_results()$left, "left", input$search_letter)
      if (nrow(df) > 0) {
        selected_row_index <- input$left_table_rows_selected[1]
        if (selected_row_index <= nrow(df)) {
          # Extract the original context (without the search letter)
          context_with_search <- df$Context[selected_row_index]
          selected_context <- str_replace(context_with_search, paste0("\\|", input$search_letter, "$"), "")
          selected_row(list(
            context = selected_context,
            type = "left",
            row_index = selected_row_index
          ))
        }
      }
    }
  })
  
  # Handle row selection from right combinations table
  observeEvent(input$right_table_rows_selected, {
    if (!is.null(input$right_table_rows_selected) && length(input$right_table_rows_selected) > 0) {
      selected_variant_lemma(NULL)
      df <- combinations_to_dataframe(analysis_results()$right, "right", input$search_letter)
      if (nrow(df) > 0) {
        selected_row_index <- input$right_table_rows_selected[1]
        if (selected_row_index <= nrow(df)) {
          # Extract the original context (without the search letter)
          context_with_search <- df$Context[selected_row_index]
          selected_context <- str_replace(context_with_search, paste0("^", input$search_letter, "\\|"), "")
          selected_row(list(
            context = selected_context,
            type = "right",
            row_index = selected_row_index
          ))
        }
      }
    }
  })
  
  # Handle row selection from combined combinations table
  observeEvent(input$combined_table_rows_selected, {
    if (!is.null(input$combined_table_rows_selected) && length(input$combined_table_rows_selected) > 0) {
      selected_variant_lemma(NULL)
      df <- combinations_to_dataframe(analysis_results()$combined, "combined", input$search_letter)
      if (nrow(df) > 0) {
        selected_row_index <- input$combined_table_rows_selected[1]
        if (selected_row_index <= nrow(df)) {
          # For combined, we need to construct the key
          left_context <- df$Left_Context[selected_row_index]
          right_context <- df$Right_Context[selected_row_index]
          selected_context <- paste0(left_context, "|", input$search_letter, "|", right_context)
          selected_row(list(
            context = selected_context,
            type = "combined",
            row_index = selected_row_index
          ))
        }
      }
    }
  })
  
  corpus_stats <- reactive({
    req(word_data())
    compute_corpus_statistics(word_data())
  })

  output$corpus_info_summary <- renderText({
    stats <- corpus_stats()
    df <- stats$overview
    if (nrow(df) == 0) {
      return("No word list loaded.")
    }
    total_tokens <- as.numeric(gsub(",", "", df$Value[df$Metric == "Sum of words (tokens)"]))
    tokens_with_grapho <- as.numeric(gsub(",", "", df$Value[df$Metric == "Tokens with grapho"]))
    tokens_without_grapho <- as.numeric(gsub(",", "", df$Value[df$Metric == "Tokens without grapho"]))
    total_types <- as.numeric(gsub(",", "", df$Value[df$Metric == "Different words (types)"]))
    types_with_grapho <- as.numeric(gsub(",", "", df$Value[df$Metric == "Types with grapho"]))
    types_without_grapho <- as.numeric(gsub(",", "", df$Value[df$Metric == "Types without grapho"]))
    breakdown <- stats$grapho_breakdown
    mean_without <- breakdown$`Without grapho`[breakdown$Metric == "Mean frequency (per type)"]
    median_without <- breakdown$`Without grapho`[breakdown$Metric == "Median frequency (per type)"]
    sprintf(
      paste0(
        "Grapho coverage: %s of %s tokens (%d%%), %s of %s types (%d%%). ",
        "Without grapho: %s types, %s tokens; mean frequency %.2f, median %.2f (per type)."
      ),
      format(tokens_with_grapho, big.mark = ","),
      format(total_tokens, big.mark = ","),
      round_pct(100 * tokens_with_grapho / total_tokens),
      format(types_with_grapho, big.mark = ","),
      format(total_types, big.mark = ","),
      round_pct(100 * types_with_grapho / total_types),
      format(types_without_grapho, big.mark = ","),
      format(tokens_without_grapho, big.mark = ","),
      as.numeric(mean_without),
      as.numeric(median_without)
    )
  })

  output$corpus_info_table <- DT::renderDataTable({
    df <- corpus_stats()$overview
    DT::datatable(
      df,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE
      ),
      rownames = FALSE,
      caption = "Corpus statistics"
    )
  })

  output$corpus_grapho_breakdown_table <- DT::renderDataTable({
    df <- corpus_stats()$grapho_breakdown
    DT::datatable(
      df,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE
      ),
      rownames = FALSE,
      caption = "With vs. without grapho"
    )
  })

  output$corpus_without_grapho_table <- DT::renderDataTable({
    df <- corpus_stats()$top_without_grapho
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "All word forms have grapho annotations.", stringsAsFactors = FALSE),
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE
      ))
    }
    DT::datatable(
      df,
      filter = "top",
      options = dt_add_word_field_regex_filters(list(
        pageLength = 15,
        dom = "tip",
        order = list(list(1, "desc"))
      ), colnames(df)),
      rownames = FALSE,
      caption = "Top 15 types without grapho (by token frequency)"
    )
  })

  varianten_data <- reactive({
    req(word_data())
    build_lemma_variants_table(word_data())
  })

  output$varianten_summary <- renderText({
    df <- varianten_data()
    if (nrow(df) == 0) {
      return("No spelling variants found in the current word list.")
    }
    n_forms <- sum(df$n_forms)
    sprintf(
      "Found %d lemma(s) with %d manuscript word form(s) in total.",
      nrow(df),
      n_forms
    )
  })

  output$varianten_table <- DT::renderDataTable({
    df <- varianten_data()
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(
          Message = "No variants found. Variants appear when two or more manuscript word forms share the same lemma.",
          stringsAsFactors = FALSE
        ),
        options = list(pageLength = 5, dom = "t"),
        rownames = FALSE
      ))
    }

    display_df <- df %>%
      select(lemma, words, norm, grapho, n_forms, Total_frequency)

    col_defs <- list(
      list(targets = 0, width = "100px"),
      list(targets = 1, width = "260px"),
      list(targets = 2, width = "140px"),
      list(targets = 3, width = "260px"),
      list(targets = 4, width = "70px"),
      list(targets = 5, width = "90px")
    )

    DT::datatable(
      display_df,
      filter = "top",
      options = dt_add_word_field_regex_filters(list(
        pageLength = 25,
        order = list(list(5, "desc"), list(0, "asc")),
        columnDefs = col_defs,
        scrollX = TRUE
      ), colnames(display_df)),
      rownames = FALSE,
      selection = list(mode = "single", target = "row"),
      caption = "Spelling variants by lemma (one row per lemma)"
    )
  })

  # Handle row selection from Varianten table
  observeEvent(input$varianten_table_rows_selected, {
    if (!is.null(input$varianten_table_rows_selected) &&
        length(input$varianten_table_rows_selected) > 0) {
      df <- varianten_data()
      if (nrow(df) > 0) {
        selected_row_index <- input$varianten_table_rows_selected[1]
        if (selected_row_index <= nrow(df)) {
          selected_row(NULL)
          selected_word(NULL)
          selected_variant_lemma(df$lemma[selected_row_index])
        }
      }
    }
  })

  # Handle row selection from all words table
  observeEvent(input$all_words_table_rows_selected, {
    if (!is.null(input$all_words_table_rows_selected) && length(input$all_words_table_rows_selected) > 0) {
      selected_variant_lemma(NULL)
      df <- get_all_words_with_letter(word_data(), input$search_letter)
      if (nrow(df) > 0) {
        selected_row_index <- input$all_words_table_rows_selected[1]
        if (selected_row_index <= nrow(df)) {
          selected_word(df$Word[selected_row_index])
        }
      }
    }
  })
  
  # Handle row selection from Word List Editor table
  observeEvent(input$wordlist_editor_rows_selected, {
    if (!is.null(input$wordlist_editor_rows_selected) && length(input$wordlist_editor_rows_selected) > 0) {
      selected_variant_lemma(NULL)
      df <- word_data()
      if (nrow(df) > 0) {
        selected_row_index <- input$wordlist_editor_rows_selected[1]
        if (selected_row_index <= nrow(df)) {
          selected_word(df$word[selected_row_index])
        }
      }
    }
  })
  
  # Context information display
  output$selected_context_info <- renderText({
    if (!is.null(selected_row())) {
      # Word selected from combination tables
      selected_context <- selected_row()$context
      context_type <- selected_row()$type
      
      # Create context display string
      if (context_type == "left") {
        if (selected_context == "^") {
          context_display <- paste0("^|", input$search_letter, " (word beginning)")
        } else {
          context_display <- paste0(selected_context, "|", input$search_letter)
        }
      } else if (context_type == "right") {
        if (selected_context == "$") {
          context_display <- paste0(input$search_letter, "|$ (word ending)")
        } else {
          context_display <- paste0(input$search_letter, "|", selected_context)
        }
      } else if (context_type == "combined") {
        context_display <- selected_context
      }
      
      paste0("Context: ", context_display)
    } else if (!is.null(selected_variant_lemma())) {
      paste0("Varianten for lemma: '", selected_variant_lemma(), "'")
    } else if (!is.null(selected_word())) {
      # Word selected from a word table (All Words, Varianten, Word List Editor, ...)
      paste0("Word: '", selected_word(), "'")
    } else {
      ""
    }
  })
  
  # Unified word examples table
  output$word_examples_table <- DT::renderDataTable({
    if (!is.null(selected_row())) {
      # Word selected from combination tables
      selected_context <- selected_row()$context
      context_type <- selected_row()$type
      
      df <- get_word_examples(analysis_results(), selected_context, context_type)
      
      if (nrow(df) == 0) {
        return(data.frame(Message = "No words found for this context"))
      }
      
      # Join refkorpus data from base word list
      base_df <- word_data()
      join_cols <- intersect(c("word", "norm", "lemma", "grapho"), colnames(base_df))
      df <- df %>% left_join(base_df %>% select(all_of(join_cols)), by = c("Word" = "word"))
      
      # Create context display string for caption
      if (context_type == "left") {
        if (selected_context == "^") {
          context_display <- paste0("^|", input$search_letter, " (word beginning)")
        } else {
          context_display <- paste0(selected_context, "|", input$search_letter)
        }
      } else if (context_type == "right") {
        if (selected_context == "$") {
          context_display <- paste0(input$search_letter, "|$ (word ending)")
        } else {
          context_display <- paste0(input$search_letter, "|", selected_context)
        }
      } else if (context_type == "combined") {
        context_display <- selected_context
      }
      
      # Create column definitions for better display
      col_defs <- list()
      if ("Word" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "Word") - 1, width = "100px")))
      }
      if ("Frequency" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "Frequency") - 1, width = "60px")))
      }
      if ("norm" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "norm") - 1, width = "100px")))
      }
      if ("lemma" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "lemma") - 1, width = "100px")))
      }
      if ("grapho" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "grapho") - 1, width = "150px")))
      }
      
      DT::datatable(df, 
                    filter = "top",
                    options = dt_add_word_field_regex_filters(list(
                      pageLength = 15, 
                      order = list(list(which(colnames(df) == "Frequency") - 1, 'desc')),
                      columnDefs = col_defs,
                      scrollX = TRUE
                    ), colnames(df)),
                    rownames = FALSE,
                    selection = list(mode = 'single', target = 'row'),
                    caption = paste0("Words with '", context_display, "'"))
    } else if (!is.null(selected_variant_lemma())) {
      df <- get_lemma_variant_words_df(word_data(), selected_variant_lemma())

      if (nrow(df) == 0) {
        return(data.frame(Message = paste0("No words found for lemma '", selected_variant_lemma(), "'")))
      }

      col_defs <- list()
      if ("Word" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "Word") - 1, width = "100px")))
      }
      if ("Frequency" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "Frequency") - 1, width = "60px")))
      }
      if ("norm" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "norm") - 1, width = "100px")))
      }
      if ("lemma" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "lemma") - 1, width = "100px")))
      }
      if ("grapho" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "grapho") - 1, width = "150px")))
      }

      DT::datatable(
        df,
        filter = "top",
        options = dt_add_word_field_regex_filters(list(
          pageLength = 15,
          order = list(list(which(colnames(df) == "Frequency") - 1, "desc")),
          columnDefs = col_defs,
          scrollX = TRUE
        ), colnames(df)),
        rownames = FALSE,
        selection = list(mode = "single", target = "row"),
        caption = paste0("Spelling variants for lemma '", selected_variant_lemma(), "'")
      )
    } else if (!is.null(selected_word())) {
      # Word selected from "All Words" table - show single word info with refkorpus data
      word_row <- word_data()[word_data()$word == selected_word(), ]
      if (nrow(word_row) > 0) {
        df <- data.frame(
          Word = selected_word(),
          Frequency = word_row$frequency,
          stringsAsFactors = FALSE
        )
        
        # Add refkorpus columns if available
        if ("norm" %in% colnames(word_row)) df$norm <- word_row$norm
        if ("lemma" %in% colnames(word_row)) df$lemma <- word_row$lemma
        if ("grapho" %in% colnames(word_row)) df$grapho <- word_row$grapho
      } else {
        df <- data.frame(Message = "Word not found in data")
      }
      
      # Create column definitions for better display
      col_defs <- list()
      if ("Word" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "Word") - 1, width = "100px")))
      }
      if ("Frequency" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "Frequency") - 1, width = "60px")))
      }
      if ("norm" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "norm") - 1, width = "100px")))
      }
      if ("lemma" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "lemma") - 1, width = "100px")))
      }
      if ("grapho" %in% colnames(df)) {
        col_defs <- append(col_defs, list(list(targets = which(colnames(df) == "grapho") - 1, width = "150px")))
      }
      
      DT::datatable(df, 
                    filter = "top",
                    options = dt_add_word_field_regex_filters(list(
                      pageLength = 1, 
                      order = list(list(which(colnames(df) == "Frequency") - 1, 'desc')),
                      columnDefs = col_defs,
                      scrollX = TRUE
                    ), colnames(df)),
                    rownames = FALSE,
                    selection = list(mode = 'single', target = 'row'),
                    caption = paste0("Selected word: '", selected_word(), "'"))
    } else {
      # No selection
      data.frame(Message = "Select a word from any table to see details")
    }
  })
  
  # Handle word selection from word examples table
  observeEvent(input$word_examples_table_rows_selected, {
    if (!is.null(input$word_examples_table_rows_selected) && length(input$word_examples_table_rows_selected) > 0) {
      if (!is.null(selected_row())) {
        # Word selected from combination tables
        selected_context <- selected_row()$context
        context_type <- selected_row()$type
        
        df <- get_word_examples(analysis_results(), selected_context, context_type)
        
        if (nrow(df) > 0) {
          selected_word_index <- input$word_examples_table_rows_selected[1]
          if (selected_word_index <= nrow(df)) {
            selected_word(df$Word[selected_word_index])
          }
        }
      } else if (!is.null(selected_variant_lemma())) {
        df <- get_lemma_variant_words_df(word_data(), selected_variant_lemma())
        if (nrow(df) > 0) {
          selected_word_index <- input$word_examples_table_rows_selected[1]
          if (selected_word_index <= nrow(df)) {
            selected_word(df$Word[selected_word_index])
          }
        }
      } else if (!is.null(selected_word())) {
        # Word already selected from "All Words" table, no change needed
        # The word is already set in selected_word()
      }
    }
  })
  
  # Selected word information display
  output$selected_word_info <- renderText({
    req(selected_word())
    
    paste0("Selected word: '", selected_word(), "'")
  })
  
  # Context table display
  output$context_table <- DT::renderDataTable({
    req(selected_word())
    
    context_data <- get_word_context(selected_word())
    
    if (nrow(context_data) == 0) {
      return(data.frame(Message = paste0("Word '", selected_word(), "' not found in editioYolanda.txt")))
    }
    
    DT::datatable(context_data,
                  filter = "top",
                  options = list(
                    pageLength = 10,
                    order = list(list(0, 'asc')),
                    columnDefs = list(
                      list(targets = 0, width = "500px")   # Context column
                    )
                  ),
                  rownames = FALSE,
                  caption = paste0("Context for word '", selected_word(), "' in editioYolanda.txt"),
                  escape = FALSE) %>%  # Allow HTML formatting
      DT::formatStyle(
        columns = "Context",
        whiteSpace = "pre-line"
      )
  })
  
  
  # Grapheme Analysis Outputs
  output$grapheme_distribution_table <- DT::renderDataTable({
    req(grapheme_distribution())
    
    df <- grapheme_distribution()
    
    # Format the data for better display
    df$total_frequency <- round(df$total_frequency, 0)
    df$avg_position <- round(df$avg_position, 2)
    df$position_variance <- round(df$position_variance, 2)
    df$word_length_avg <- round(df$word_length_avg, 2)
    
    # Rename columns for better display
    colnames(df) <- c("Grapheme", "Total Frequency", "Unique Words", 
                     "Avg Position", "Position Variance", "Avg Word Length")
    
    DT::datatable(df, 
                  filter = "top",
                  options = list(
                    paging = TRUE,
                    pageLength = 25,
                    lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
                    dom = '<"top"lpf>rt<"clear">',
                    order = list(list(1, 'desc')),
                    columnDefs = list(
                      list(targets = 0, width = "100px"),  # Grapheme
                      list(targets = 1, width = "120px"),  # Total Frequency
                      list(targets = 2, width = "100px"),  # Unique Words
                      list(targets = 3, width = "100px"),  # Avg Position
                      list(targets = 4, width = "120px"),  # Position Variance
                      list(targets = 5, width = "120px")   # Avg Word Length
                    )
                  ),
                  rownames = FALSE,
                  selection = list(mode = 'single', target = 'row'),
                  caption = "Complete Grapheme Distribution Analysis")
  })

  output$vowel_grapheme_freq_table <- renderUI({
    req(grapheme_distribution())
    build_vowel_region_table_html(grapheme_distribution())
  })

  output$consonant_grapheme_freq_table <- renderUI({
    req(grapheme_distribution())
    build_consonant_freq_html(grapheme_distribution())
  })
  
  output$grapheme_frequency_plot <- renderPlot({
    req(grapheme_distribution())
    create_grapheme_frequency_plot(grapheme_distribution())
  })
  
  output$grapheme_length_plot <- renderPlot({
    req(grapheme_distribution())
    create_grapheme_length_plot(grapheme_distribution())
  })
  
  output$grapheme_position_plot <- renderPlot({
    req(grapheme_distribution())
    create_grapheme_position_plot(grapheme_distribution())
  })
  
  # Download handler for grapheme analysis CSV
  output$download_graphemes_csv <- downloadHandler(
    filename = function() {
      paste0("yolanda_grapheme_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(grapheme_distribution())
      write.csv(grapheme_distribution(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$u_graphie_corpus_summary <- renderText({
    res <- u_graphie_analysis_data()
    if (is.null(res) || nrow(res$stats) == 0) {
      return("Noch keine Analyse \u2014 bitte \"Analyse starten\" klicken.")
    }
    stats <- res$stats
    v_hits <- sum(stats$lemmas[stats$grapheme == "v"])
    u_hits <- sum(stats$lemmas[stats$grapheme == "u"])
    paste0(
      "Eindeutige Lemmas mit Treffer: ", format(res$total_unique_lemmas, big.mark = ","), "\n",
      "Lemma-Zuordnungen (Graphem \u00d7 Phonem \u00d7 Position): ",
      format(res$total_lemma_hits, big.mark = ","), "\n",
      "  davon Graphem v: ", format(v_hits, big.mark = ","),
      " Lemmas | Graphem u: ", format(u_hits, big.mark = ","), " Lemmas"
    )
  })

  output$u_graphie_distribution_table <- DT::renderDataTable({
    res <- u_graphie_analysis_data()
    if (is.null(res) || nrow(res$stats) == 0) {
      return(DT::datatable(
        data.frame(Meldung = "Keine |v/u = u, \u00fb, uo|-Mappings in grapho gefunden."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    df <- res$stats
    df$Graphie <- paste0("|", df$grapheme, "=", df$phoneme, "|")
    df <- df[, c(
      "Graphie", "grapheme", "phoneme", "position",
      "lemmas", "word_types", "lemma_pct_in_grapheme_position"
    )]
    colnames(df) <- c(
      "Grapho-Paar", "Graphie", "Phonem", "Position",
      "Lemmas", "Wortformen",
      "Lemma-% (innerh. Graphie+Position)"
    )

    DT::datatable(
      df,
      filter = "top",
      options = list(
        paging = TRUE,
        pageLength = 25,
        order = list(list(3, "asc"), list(1, "asc"), list(4, "desc"))
      ),
      rownames = FALSE,
      selection = "none",
      caption = "Lemma-Z\u00e4hlung: jedes Lemma z\u00e4hlt einmal pro Zelle; % innerhalb Graphem \u00d7 Position"
    )
  })

  output$u_graphie_position_plot <- renderPlot({
    res <- u_graphie_analysis_data()
    if (is.null(res)) {
      return(ggplot() +
        annotate("text", x = 0, y = 0, label = "Bitte Analyse starten") +
        theme_void())
    }
    create_u_graphie_position_plot(res$stats)
  })

  output$download_u_graphie_csv <- downloadHandler(
    filename = function() {
      paste0("yolanda_u_graphie_position_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      res <- u_graphie_analysis_data()
      if (is.null(res) || nrow(res$stats) == 0) {
        write.csv(data.frame(), file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        write.csv(res$stats, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )

  output$u_graphie_status_indicator <- renderUI({
    res <- u_graphie_analysis_data()
    if (is.null(res)) {
      span("Noch keine Analyse", style = "color: #6c757d; font-style: italic;")
    } else if (nrow(res$stats) == 0) {
      span("Analyse ohne Treffer", style = "color: #856404; font-weight: bold;")
    } else {
      span("\u2713 u/v-Graphie-Analyse abgeschlossen", style = "color: #28a745; font-weight: bold;")
    }
  })

  # Status indicator for grapheme analysis
  output$grapheme_status_indicator <- renderUI({
    if (is.null(grapheme_distribution())) {
      span("No grapheme analysis performed yet", style = "color: #6c757d; font-style: italic;")
    } else {
      span("✓ Grapheme analysis completed", style = "color: #28a745; font-weight: bold;")
    }
  })
  
  # Phoneme-Grapheme Analysis Outputs
  output$phoneme_to_grapheme_table <- DT::renderDataTable({
    req(phoneme_to_grapheme_mappings())
    
    df <- phoneme_to_grapheme_mappings()
    
    # Format the data for better display
    df$total_frequency <- round(df$total_frequency, 0)
    
    # Rename columns for better display
    colnames(df) <- c("Phoneme", "Grapheme Count", "Graphemes (token/type freq)", "Total Frequency", "Unique Words")
    
    DT::datatable(df, 
                  filter = "top",
                  options = list(
                    paging = TRUE,
                    pageLength = 25,
                    lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
                    dom = '<"top"lpf>rt<"clear">',
                    order = list(list(1, 'desc')),
                    columnDefs = list(
                      list(targets = 0, width = "100px"),  # Phoneme
                      list(targets = 1, width = "120px"),  # Grapheme Count
                      list(targets = 2, width = "300px"),  # Graphemes (token/type freq)
                      list(targets = 3, width = "120px"),  # Total Frequency
                      list(targets = 4, width = "100px")   # Unique Words
                    )
                  ),
                  rownames = FALSE,
                  selection = list(mode = 'single', target = 'row'),
                  caption = "Graphemes Mapping to Same Phoneme (Many-to-One)")
  })
  
  output$grapheme_to_phoneme_table <- DT::renderDataTable({
    req(grapheme_to_phoneme_mappings())
    
    df <- grapheme_to_phoneme_mappings()
    
    # Format the data for better display
    df$total_frequency <- round(df$total_frequency, 0)
    
    # Rename columns for better display
    colnames(df) <- c("Grapheme", "Phoneme Count", "Phonemes (tokens/types, token%/type%)", "Total Frequency", "Unique Words")
    
    DT::datatable(df, 
                  filter = "top",
                  options = list(
                    paging = TRUE,
                    pageLength = 25,
                    lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
                    dom = '<"top"lpf>rt<"clear">',
                    order = list(list(1, 'desc')),
                    columnDefs = list(
                      list(targets = 0, width = "100px"),  # Grapheme
                      list(targets = 1, width = "120px"),  # Phoneme Count
                      list(targets = 2, width = "300px"),  # Phonemes (token/type freq)
                      list(targets = 3, width = "120px"),  # Total Frequency
                      list(targets = 4, width = "100px")   # Unique Words
                    )
                  ),
                  rownames = FALSE,
                  selection = list(mode = 'single', target = 'row'),
                  caption = "Phonemes Mapping to Same Grapheme (One-to-Many)")
  })
  
  # Vowel Tables Outputs
  output$short_vowel_table <- DT::renderDataTable({
    req(vowel_tables())
    
    df <- vowel_tables()$short_vowels
    
    DT::datatable(df, 
                  filter = "top",
                  options = list(
                    paging = FALSE,
                    searching = TRUE,
                    ordering = FALSE,
                    info = FALSE,
                    columnDefs = list(
                      list(targets = 0, width = "100px"),  # Phoneme
                      list(targets = 1, width = "750px")
                    )
                  ),
                  rownames = FALSE,
                  selection = "none",
                  caption = "Distribution of Graphemes for Short Vowel Phonemes (counts and % per phoneme)")
  })
  
  output$long_vowel_table <- DT::renderDataTable({
    req(vowel_tables())
    
    df <- vowel_tables()$long_vowels
    
    DT::datatable(df, 
                  filter = "top",
                  options = list(
                    paging = FALSE,
                    searching = TRUE,
                    ordering = FALSE,
                    info = FALSE,
                    columnDefs = list(
                      list(targets = 0, width = "100px"),  # Phoneme
                      list(targets = 1, width = "750px")
                    )
                  ),
                  rownames = FALSE,
                  selection = "none",
                  caption = "Distribution of Graphemes for Long Vowel Phonemes (counts and % per phoneme)")
  })
  
  output$diphthong_table <- DT::renderDataTable({
    req(vowel_tables())
    
    df <- vowel_tables()$diphthongs
    
    DT::datatable(df, 
                  filter = "top",
                  options = list(
                    paging = FALSE,
                    searching = TRUE,
                    ordering = FALSE,
                    info = FALSE,
                    columnDefs = list(
                      list(targets = 0, width = "100px"),  # Phoneme
                      list(targets = 1, width = "750px")
                    )
                  ),
                  rownames = FALSE,
                  selection = "none",
                  caption = "Distribution of Graphemes for Diphthongs (counts and % per phoneme)")
  })
  
  output$combined_vowel_type_matrix <- renderPlot({
    req(grapheme_phoneme_analysis())
    selected_phonemes <- get_focus_phonemes()
    
    matrix_data <- create_vowel_confusion_matrix_data(
      grapheme_phoneme_analysis(),
      selected_phonemes,
      focus_graphemes = NULL,
      count_mode = "type",
      normalize_by = "phoneme"
    )
    create_vowel_confusion_matrix_plot(
      matrix_data,
      "Type Percentages per Selected Phoneme"
    )
  })
  
  output$combined_vowel_token_matrix <- renderPlot({
    req(grapheme_phoneme_analysis())
    selected_phonemes <- get_focus_phonemes()
    
    matrix_data <- create_vowel_confusion_matrix_data(
      grapheme_phoneme_analysis(),
      selected_phonemes,
      focus_graphemes = NULL,
      count_mode = "token",
      normalize_by = "phoneme"
    )
    create_vowel_confusion_matrix_plot(
      matrix_data,
      "Token Percentages per Selected Phoneme"
    )
  })
  
  output$combined_vowel_type_matrix_colwise <- renderPlot({
    req(grapheme_phoneme_analysis())
    selected_phonemes <- get_focus_phonemes()
    
    matrix_data <- create_vowel_confusion_matrix_data(
      grapheme_phoneme_analysis(),
      selected_phonemes,
      focus_graphemes = NULL,
      count_mode = "type",
      normalize_by = "grapheme"
    )
    create_vowel_confusion_matrix_plot(
      matrix_data,
      "Type Percentages per Grapheme (Column-Wise)"
    )
  })
  
  output$combined_vowel_token_matrix_colwise <- renderPlot({
    req(grapheme_phoneme_analysis())
    selected_phonemes <- get_focus_phonemes()
    
    matrix_data <- create_vowel_confusion_matrix_data(
      grapheme_phoneme_analysis(),
      selected_phonemes,
      focus_graphemes = NULL,
      count_mode = "token",
      normalize_by = "grapheme"
    )
    create_vowel_confusion_matrix_plot(
      matrix_data,
      "Token Percentages per Grapheme (Column-Wise)"
    )
  })
  
  output$focus_summary_type_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    selected_phonemes <- get_focus_phonemes()
    focus_tables <- create_focus_tables(grapheme_phoneme_analysis(), selected_phonemes, count_mode = "type")
    df <- focus_tables$summary
    req(nrow(df) > 0)
    
    df <- df |>
      mutate(
        top_share = round_pct(top_share),
        focus = round(focus, 3),
        effective_choices = round(effective_choices, 2)
      )
    
    DT::datatable(
      df,
      filter = "top",
      options = list(
        paging = TRUE,
        pageLength = 15,
        order = list(list(4, "desc")),
        columnDefs = list(
          list(targets = c(3, 4, 5, 6, 7), className = "dt-right")
        )
      ),
      rownames = FALSE,
      selection = "none",
      caption = "How focused each phoneme/grapheme is (type-based). Higher focus = more concentrated mapping."
    )
  })
  
  output$focus_scatter_type_plot <- renderPlot({
    req(grapheme_phoneme_analysis())
    selected_phonemes <- get_focus_phonemes()
    focus_tables <- create_focus_tables(grapheme_phoneme_analysis(), selected_phonemes, count_mode = "type")
    scatter_df <- focus_tables$scatter
    req(nrow(scatter_df) > 0)
    
    create_focus_scatter_plot(
      scatter_df,
      "Type-Based Focus Map: Stable vs Ambiguous Pairs"
    )
  })
  
  output$mutual_focus_type_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    selected_phonemes <- get_focus_phonemes()
    focus_tables <- create_focus_tables(grapheme_phoneme_analysis(), selected_phonemes, count_mode = "type")
    df <- focus_tables$mutual
    req(nrow(df) > 0)
    
    df <- df |>
      mutate(
        p_grapheme_given_phoneme = round_pct(p_grapheme_given_phoneme),
        p_phoneme_given_grapheme = round_pct(p_phoneme_given_grapheme),
        mutual_strength = round_pct(mutual_strength)
      )
    
    DT::datatable(
      df,
      filter = "top",
      options = list(
        paging = TRUE,
        pageLength = 20,
        order = list(list(5, "desc")),
        columnDefs = list(
          list(targets = c(2, 3, 4, 5), className = "dt-right")
        )
      ),
      rownames = FALSE,
      selection = "none",
      caption = "Bidirectional concentration score per grapheme-phoneme pair (type-based)."
    )
  })
  
  output$focus_summary_token_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    selected_phonemes <- get_focus_phonemes()
    focus_tables <- create_focus_tables(grapheme_phoneme_analysis(), selected_phonemes, count_mode = "token")
    df <- focus_tables$summary
    req(nrow(df) > 0)
    
    df <- df |>
      mutate(
        top_share = round_pct(top_share),
        focus = round(focus, 3),
        effective_choices = round(effective_choices, 2)
      )
    
    DT::datatable(
      df,
      filter = "top",
      options = list(
        paging = TRUE,
        pageLength = 15,
        order = list(list(4, "desc")),
        columnDefs = list(
          list(targets = c(3, 4, 5, 6, 7), className = "dt-right")
        )
      ),
      rownames = FALSE,
      selection = "none",
      caption = "How focused each phoneme/grapheme is (token-based). Higher focus = more concentrated mapping."
    )
  })
  
  output$focus_scatter_token_plot <- renderPlot({
    req(grapheme_phoneme_analysis())
    selected_phonemes <- get_focus_phonemes()
    focus_tables <- create_focus_tables(grapheme_phoneme_analysis(), selected_phonemes, count_mode = "token")
    scatter_df <- focus_tables$scatter
    req(nrow(scatter_df) > 0)
    
    create_focus_scatter_plot(
      scatter_df,
      "Token-Based Focus Map: Stable vs Ambiguous Pairs"
    )
  })
  
  output$mutual_focus_token_table <- DT::renderDataTable({
    req(grapheme_phoneme_analysis())
    selected_phonemes <- get_focus_phonemes()
    focus_tables <- create_focus_tables(grapheme_phoneme_analysis(), selected_phonemes, count_mode = "token")
    df <- focus_tables$mutual
    req(nrow(df) > 0)
    
    df <- df |>
      mutate(
        p_grapheme_given_phoneme = round_pct(p_grapheme_given_phoneme),
        p_phoneme_given_grapheme = round_pct(p_phoneme_given_grapheme),
        mutual_strength = round_pct(mutual_strength)
      )
    
    DT::datatable(
      df,
      filter = "top",
      options = list(
        paging = TRUE,
        pageLength = 20,
        order = list(list(5, "desc")),
        columnDefs = list(
          list(targets = c(2, 3, 4, 5), className = "dt-right")
        )
      ),
      rownames = FALSE,
      selection = "none",
      caption = "Bidirectional concentration score per grapheme-phoneme pair (token-based)."
    )
  })
  
  
  # ẏ/i Analysis Outputs
  output$y_i_analysis_table <- DT::renderDataTable({
    req(y_i_analysis_data())
    
    df <- y_i_analysis_data()
    
    
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No words with 'ẏ' or 'i' found"),
        options = list(dom = 't')
      ))
    }
    
    # Create column definitions for better display
    col_defs <- list(
      list(targets = 0, width = "120px"),  # y_word
      list(targets = 1, width = "120px"),  # i_word
      list(targets = 2, width = "80px"),   # y_frequency
      list(targets = 3, width = "80px"),   # i_frequency
      list(targets = 4, width = "80px"),   # y_frequency_percent
      list(targets = 5, width = "100px"),  # frequency_difference
      list(targets = 6, width = "80px")    # has_i_version
    )
    
    # Add grapho column widths if they exist
    if ("y_grapho" %in% colnames(df)) {
      col_defs <- append(col_defs, list(
        list(targets = 7, width = "200px"),  # y_grapho
        list(targets = 8, width = "200px")   # i_grapho
      ))
    }
    
    DT::datatable(df, 
                  filter = "top",
                  options = dt_add_word_field_regex_filters(list(
                    paging = TRUE,
                    pageLength = 25,
                    lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
                    dom = '<"top"lpf>rt<"clear">',
                    order = list(list(5, 'desc')),  # Sort by frequency_difference
                    columnDefs = col_defs,
                    scrollX = TRUE,
                    headerCallback = JS("
                      function(thead, data, start, end, display) {
                        var api = this.api();
                        var col2 = api.column(2, {page: 'current'}).data();
                        var col3 = api.column(3, {page: 'current'}).data();
                        
                        var sum2 = col2.reduce(function(a, b) {
                          return parseInt(a) + parseInt(b);
                        }, 0);
                        
                        var sum3 = col3.reduce(function(a, b) {
                          return parseInt(a) + parseInt(b);
                        }, 0);
                        
                        $(api.column(2).header()).html('y_frequency<br><small>Sum: ' + sum2 + '</small>');
                        $(api.column(3).header()).html('i_frequency<br><small>Sum: ' + sum3 + '</small>');
                      }
                    ")
                  ), colnames(df)),
                  rownames = FALSE,
                  selection = list(mode = 'single', target = 'row')
    )
  })
  
  y_i_lemma_summary <- reactive({
    req(word_data())
    min_n <- input$y_i_lemma_min_tokens
    if (is.null(min_n) || is.na(min_n) || min_n < 1) min_n <- 1
    summarise_y_i_by_lemma(word_data(), min_tokens = min_n)
  })

  output$y_i_lemma_table <- DT::renderDataTable({
    df <- y_i_lemma_summary()
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No lemmatized words with standalone ẏ or i found for the current filter."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    show <- data.frame(
      lemma = df$lemma,
      words = df$words,
      types = df$n_forms,
      tokens = df$n_tokens,
      `ẏ tok` = df$y_tokens,
      `i tok` = df$i_tokens,
      `ẏ+i tok` = df$both_tokens,
      `ẏ %` = df$y_share_pct,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    DT::datatable(
      show,
      filter = "top",
      options = dt_add_word_field_regex_filters(list(
        paging = TRUE,
        pageLength = 25,
        lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
        dom = '<"top"lpf>rt<"clear">',
        order = list(list(3, "desc")),
        columnDefs = list(
          list(targets = 0, width = "140px"),
          list(targets = 1, width = "420px"),
          list(targets = 2, width = "70px"),
          list(targets = 3, width = "80px"),
          list(targets = 4, width = "70px"),
          list(targets = 5, width = "70px"),
          list(targets = 6, width = "80px"),
          list(targets = 7, width = "70px")
        ),
        scrollX = TRUE
      ), colnames(show)),
      rownames = FALSE,
      selection = list(mode = "single", target = "row"),
      caption = "Lemma inventory of surface forms with standalone ẏ or i (<ei>/<oi>/<ui> excluded)."
    )
  })

  # Enriched per-word-type data for frequency-normalized preference analysis
  y_i_enriched <- reactive({
    req(y_i_analysis_data())
    df <- y_i_analysis_data()
    if (nrow(df) == 0) return(df)
    enrich_y_i_data(df, word_data())
  })
  
  output$y_i_lexeme_hist <- renderPlot({
    enriched <- y_i_enriched()
    req(nrow(enriched) > 0)
    min_n <- input$y_i_lexeme_min_n
    if (is.null(min_n) || is.na(min_n) || min_n < 1) min_n <- 1
    lex_df <- summarise_y_i_lexeme(enriched, min_tokens = min_n)
    create_y_i_lexeme_hist(lex_df, min_n)
  })
  
  # Word lists split by dominant grapheme (lexeme-level, ẏ share vs 50%).
  y_i_pref_lists <- reactive({
    enriched <- y_i_enriched()
    req(nrow(enriched) > 0)
    min_n <- input$y_i_lexeme_min_n
    if (is.null(min_n) || is.na(min_n) || min_n < 1) min_n <- 1
    summarise_y_i_lexeme(enriched, min_tokens = min_n)
  })
  
  output$y_i_pref_y_list <- DT::renderDataTable({
    lex <- y_i_pref_lists()
    lex <- lex[lex$share >= 0.5, ]
    req(nrow(lex) > 0)
    df <- data.frame(
      lexeme = lex$lexeme,
      forms = lex$forms,
      n = lex$n,
      pct = round_pct(100 * lex$share),
      y_tok = lex$y,
      i_tok = lex$n - lex$y,
      stringsAsFactors = FALSE
    )
    colnames(df) <- c("lexeme (norm)", "word(s)", "n", "ẏ %", "ẏ tok", "i tok")
    df <- df[order(-df[["ẏ %"]], -df$n), ]
    DT::datatable(
      df,
      filter = "top",
      options = dt_add_word_field_regex_filters(list(
        paging = TRUE, pageLength = 15,
        lengthMenu = list(c(15, 30, 50, -1), c('15', '30', '50', 'All')),
        order = list(list(3, 'desc'))
      ), colnames(df)),
      rownames = FALSE,
      selection = "none",
      caption = "Lexemes that mostly use ẏ (ẏ share >= 50%)."
    )
  })
  
  y_i_consonant_analysis <- reactive({
    enriched <- y_i_enriched()
    req(nrow(enriched) > 0)
    min_tok <- input$y_i_cons_min_tokens
    if (is.null(min_tok) || is.na(min_tok) || min_tok < 1) min_tok <- 10
    fit_y_i_consonant_analysis(enriched, min_context_tokens = min_tok)
  })

  output$y_i_consonant_overview <- renderText({
    analysis <- y_i_consonant_analysis()
    analysis$overview
  })

  output$y_i_prec_cons_forest <- renderPlot({
    analysis <- y_i_consonant_analysis()
    create_y_i_consonant_forest_plot(
      analysis$prec_summary,
      "preceding",
      analysis$corpus_share
    )
  })

  output$y_i_foll_cons_forest <- renderPlot({
    analysis <- y_i_consonant_analysis()
    create_y_i_consonant_forest_plot(
      analysis$foll_summary,
      "following",
      analysis$corpus_share
    )
  })

  render_y_i_cons_table <- function(df) {
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No consonant contexts with enough tokens."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    show <- df |>
      mutate(
        `ẏ %` = round_pct(100 * share),
        `i %` = round_pct(100 * (1 - share)),
        `p (adj)` = signif(p_adj, 3)
      ) |>
      select(consonant, preference, `ẏ %`, `i %`, n, n_words, `p (adj)`)
    DT::datatable(
      show,
      options = list(paging = TRUE, pageLength = 15, order = list(list(2, "desc"))),
      rownames = FALSE,
      selection = "none"
    )
  }

  output$y_i_prec_cons_table <- DT::renderDataTable({
    render_y_i_cons_table(y_i_consonant_analysis()$prec_summary)
  })

  output$y_i_foll_cons_table <- DT::renderDataTable({
    render_y_i_cons_table(y_i_consonant_analysis()$foll_summary)
  })

  render_y_i_cons_model_table <- function(model_obj) {
    df <- model_obj$table
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = model_obj$note),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    DT::datatable(
      df |> select(context, term, odds_ratio, estimate_logodds, std_error, p_value),
      options = list(paging = FALSE, order = list(list(5, "asc"))),
      rownames = FALSE,
      selection = "none"
    )
  }

  output$y_i_prec_cons_model_note <- renderText({
    y_i_consonant_analysis()$prec_model$note
  })

  output$y_i_prec_cons_model_table <- DT::renderDataTable({
    render_y_i_cons_model_table(y_i_consonant_analysis()$prec_model)
  })

  output$y_i_foll_cons_model_note <- renderText({
    y_i_consonant_analysis()$foll_model$note
  })

  output$y_i_foll_cons_model_table <- DT::renderDataTable({
    render_y_i_cons_model_table(y_i_consonant_analysis()$foll_model)
  })

  output$y_i_pref_i_list <- DT::renderDataTable({
    lex <- y_i_pref_lists()
    lex <- lex[lex$share < 0.5, ]
    req(nrow(lex) > 0)
    df <- data.frame(
      lexeme = lex$lexeme,
      forms = lex$forms,
      n = lex$n,
      pct = round_pct(100 * (1 - lex$share)),
      i_tok = lex$n - lex$y,
      y_tok = lex$y,
      stringsAsFactors = FALSE
    )
    colnames(df) <- c("lexeme (norm)", "word(s)", "n", "i %", "i tok", "ẏ tok")
    df <- df[order(-df[["i %"]], -df$n), ]
    DT::datatable(
      df,
      filter = "top",
      options = dt_add_word_field_regex_filters(list(
        paging = TRUE, pageLength = 15,
        lengthMenu = list(c(15, 30, 50, -1), c('15', '30', '50', 'All')),
        order = list(list(3, 'desc'))
      ), colnames(df)),
      rownames = FALSE,
      selection = "none",
      caption = "Lexemes that mostly use i (ẏ share < 50%)."
    )
  })
  
  # Status indicator for ẏ/i analysis
  output$y_i_status_indicator <- renderUI({
    if (is.null(y_i_analysis_data())) {
      span("No ẏ/i analysis performed yet", style = "color: #6c757d; font-style: italic;")
    } else {
      span("✓ ẏ/i analysis completed", style = "color: #28a745; font-weight: bold;")
    }
  })
  
  # Download handler for ẏ/i analysis CSV
  output$download_y_i_csv <- downloadHandler(
    filename = function() {
      paste0("yolanda_y_i_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(y_i_analysis_data())
      write.csv(y_i_analysis_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  s_longs_enriched <- reactive({
    req(s_longs_analysis_data())
    df <- s_longs_analysis_data()
    if (nrow(df) == 0) return(df)
    enrich_s_longs_data(df, word_data())
  })

  output$s_longs_corpus_summary <- renderText({
    req(s_longs_analysis_data())
    format_s_longs_corpus_summary(s_longs_enriched(), s_longs_analysis_data())
  })

  output$s_longs_analysis_table <- DT::renderDataTable({
    req(s_longs_analysis_data())
    df <- s_longs_analysis_data()
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No words with long s or round s found"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    DT::datatable(
      df,
      filter = "top",
      options = dt_add_word_field_regex_filters(list(
        paging = TRUE,
        pageLength = 25,
        lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
        order = list(list(5, "desc")),
        scrollX = TRUE
      ), colnames(df)),
      rownames = FALSE,
      selection = list(mode = "single", target = "row")
    )
  })

  output$s_longs_lexeme_hist <- renderPlot({
    enriched <- s_longs_enriched()
    if (nrow(enriched) == 0) {
      return(ggplot() +
        annotate("text", x = 0, y = 0, label = "No |s=s| pairs with tokens") +
        theme_void())
    }
    min_n <- input$s_longs_lexeme_min_n
    if (is.null(min_n) || is.na(min_n) || min_n < 1) min_n <- 1
    lex_df <- summarise_s_longs_lexeme(enriched, min_tokens = min_n)
    create_s_longs_lexeme_hist(lex_df, min_n)
  })

  s_longs_pref_lists <- reactive({
    enriched <- s_longs_enriched()
    if (nrow(enriched) == 0) return(data.frame())
    min_n <- input$s_longs_lexeme_min_n
    if (is.null(min_n) || is.na(min_n) || min_n < 1) min_n <- 1
    summarise_s_longs_lexeme(enriched, min_tokens = min_n)
  })

  output$s_longs_pref_longs_list <- DT::renderDataTable({
    lex <- s_longs_pref_lists()
    if (nrow(lex) == 0) {
      return(DT::datatable(
        data.frame(Message = "No lexemes above minimum token count"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    lex <- lex[lex$share >= 0.5, , drop = FALSE]
    if (nrow(lex) == 0) {
      return(DT::datatable(
        data.frame(Message = "No lexemes prefer long s at >= 50%"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    df <- data.frame(
      lexeme = lex$lexeme,
      forms = lex$forms,
      n = lex$n,
      longs_pct = round_pct(100 * lex$share),
      longs_tok = lex$longs,
      round_tok = lex$n - lex$longs,
      stringsAsFactors = FALSE
    )
    colnames(df)[4] <- "\u017f %"
    colnames(df)[5] <- "\u017f tok"
    df <- df[order(-df[[4]], -df$n), ]
    DT::datatable(
      df,
      filter = "top",
      options = dt_add_word_field_regex_filters(list(pageLength = 15), colnames(df)),
      rownames = FALSE,
      selection = "none"
    )
  })

  output$s_longs_pref_round_list <- DT::renderDataTable({
    lex <- s_longs_pref_lists()
    if (nrow(lex) == 0) {
      return(DT::datatable(
        data.frame(Message = "No lexemes above minimum token count"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    lex <- lex[lex$share < 0.5, , drop = FALSE]
    if (nrow(lex) == 0) {
      return(DT::datatable(
        data.frame(Message = "No lexemes prefer round s at >= 50%"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    df <- data.frame(
      lexeme = lex$lexeme,
      forms = lex$forms,
      n = lex$n,
      round_pct = round_pct(100 * (1 - lex$share)),
      round_tok = lex$n - lex$longs,
      longs_tok = lex$longs,
      stringsAsFactors = FALSE
    )
    df <- df[order(-df$round_pct, -df$n), ]
    DT::datatable(
      df,
      filter = "top",
      options = dt_add_word_field_regex_filters(list(pageLength = 15), colnames(df)),
      rownames = FALSE,
      selection = "none"
    )
  })

  output$s_longs_position_plot <- renderPlot({
    enriched <- s_longs_enriched()
    if (nrow(enriched) == 0) {
      return(ggplot() +
        annotate("text", x = 0, y = 0, label = "No |s=s| pairs with tokens") +
        theme_void())
    }
    pos_df <- summarise_s_longs_position(enriched, min_tokens = 3)
    create_s_longs_position_plot(pos_df, sum(enriched$y_freq) / sum(enriched$n))
  })

  output$s_longs_position_table <- DT::renderDataTable({
    enriched <- s_longs_enriched()
    if (nrow(enriched) == 0) {
      return(DT::datatable(
        data.frame(Message = "No |s=s| pairs with tokens"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    pos_df <- summarise_s_longs_position(enriched, min_tokens = 3)
    if (nrow(pos_df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No position groups with enough tokens"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    DT::datatable(pos_df, options = list(paging = FALSE), rownames = FALSE, selection = "none")
  })

  s_longs_consonant_analysis <- reactive({
    enriched <- s_longs_enriched()
    if (nrow(enriched) == 0) {
      return(list(
        prec_summary = data.frame(),
        foll_summary = data.frame(),
        corpus_share = NA_real_,
        overview = "No data with grapho |s=s|."
      ))
    }
    min_tok <- input$s_longs_cons_min_tokens
    if (is.null(min_tok) || is.na(min_tok) || min_tok < 1) min_tok <- 10
    fit_s_longs_consonant_analysis(enriched, min_context_tokens = min_tok)
  })

  output$s_longs_consonant_overview <- renderText({
    req(s_longs_analysis_data())
    s_longs_consonant_analysis()$overview
  })

  output$s_longs_prec_cons_forest <- renderPlot({
    req(s_longs_analysis_data())
    analysis <- s_longs_consonant_analysis()
    create_s_longs_consonant_forest_plot(
      analysis$prec_summary, side = "prec", corpus_mean = analysis$corpus_share
    )
  })

  output$s_longs_foll_cons_forest <- renderPlot({
    req(s_longs_analysis_data())
    analysis <- s_longs_consonant_analysis()
    create_s_longs_consonant_forest_plot(
      analysis$foll_summary, side = "foll", corpus_mean = analysis$corpus_share
    )
  })

  render_s_longs_cons_table <- function(df) {
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No contexts with enough tokens"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    show <- df |>
      mutate(
        longs_pct = paste0(round_pct(100 * share), "%"),
        ci = paste0("[", round_pct(100 * lower), "%, ", round_pct(100 * upper), "%]")
      ) |>
      select(consonant, preference, longs_pct, n, n_words, ci, p_adj)
    DT::datatable(show, options = list(paging = FALSE, order = list(list(3, "desc"))), rownames = FALSE)
  }

  output$s_longs_prec_cons_table <- DT::renderDataTable({
    req(s_longs_analysis_data())
    render_s_longs_cons_table(s_longs_consonant_analysis()$prec_summary)
  })

  output$s_longs_foll_cons_table <- DT::renderDataTable({
    req(s_longs_analysis_data())
    render_s_longs_cons_table(s_longs_consonant_analysis()$foll_summary)
  })

  output$s_longs_status_indicator <- renderUI({
    if (is.null(s_longs_analysis_data())) {
      span("No s/\u017f analysis performed yet", style = "color: #6c757d; font-style: italic;")
    } else {
      span("\u2713 s/\u017f analysis completed", style = "color: #28a745; font-weight: bold;")
    }
  })

  output$download_s_longs_csv <- downloadHandler(
    filename = function() {
      paste0("yolanda_s_longs_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(s_longs_analysis_data())
      write.csv(s_longs_analysis_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Download handler for grapheme-phoneme analysis CSV
  output$download_grapheme_phoneme_csv <- downloadHandler(
    filename = function() {
      paste0("yolanda_grapheme_phoneme_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(grapheme_phoneme_analysis())
      write.csv(grapheme_phoneme_analysis(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Status indicator for grapheme-phoneme analysis
  output$grapheme_phoneme_status_indicator <- renderUI({
    if (is.null(grapheme_phoneme_analysis())) {
      span("No grapheme-phoneme analysis performed yet", style = "color: #6c757d; font-style: italic;")
    } else {
      span("✓ Grapheme-phoneme analysis completed", style = "color: #28a745; font-weight: bold;")
    }
  })
  
  
  # Create a reactive expression for the display data with pending edits
  wordlist_display_data <- reactive({
    req(word_data())
    base_data <- word_data()
    current_edits <- pending_edits()
    
    if (length(current_edits) == 0) {
      return(base_data)
    }
    
    # Apply pending edits to create display data
    display_data <- base_data
    
    for (edit_key in names(current_edits)) {
      edit_info <- current_edits[[edit_key]]
      row_idx <- edit_info$row
      col_name <- edit_info$column
      new_value <- edit_info$value
      
      if (col_name %in% c("norm", "lemma", "grapho")) {
        display_data[row_idx, col_name] <- as.character(new_value)
      } else if (col_name == "frequency") {
        display_data[row_idx, col_name] <- suppressWarnings(as.numeric(new_value))
      }
    }
    
    return(display_data)
  })

  # Word List Editor
  output$wordlist_editor <- DT::renderDT({
    req(word_data())
    
    df <- normalize_word_refkorpus_columns(word_data())
    
    # Determine which columns are editable (norm, lemma, grapho)
    editable_cols <- which(colnames(df) %in% c("norm", "lemma", "grapho")) - 1
    non_editable_cols <- which(colnames(df) %in% c("word", "frequency")) - 1
    
    # Find the frequency column index (0-based for JavaScript)
    frequency_col_index <- which(colnames(df) == "frequency") - 1
    
    DT::datatable(
                  df,
                  filter = "top",
                  editable = list(target = "cell", disable = list(columns = non_editable_cols)),
                  options = dt_add_word_field_regex_filters(list(
                    paging = TRUE,
                    pageLength = 50,
                    lengthMenu = list(c(25, 50, 100, 250, -1), c('25', '50', '100', '250', 'All')),
                    dom = '<"top"lpf>rt<"clear">',
                    order = list(list(1, 'desc')),
                    columnDefs = list(
                      list(targets = which(colnames(df) == "word") - 1, width = "150px"),     # Word column
                      list(targets = which(colnames(df) == "frequency") - 1, width = "80px"), # Frequency column
                      list(targets = which(colnames(df) == "norm") - 1, width = "150px"),     # Norm column
                      list(targets = which(colnames(df) == "lemma") - 1, width = "150px"),    # Lemma column
                      list(targets = which(colnames(df) == "grapho") - 1, width = "200px")    # Grapho column
                    ),
                    headerCallback = JS(paste0("
                      function(thead, data, start, end, display) {
                        var api = this.api();
                        var col", frequency_col_index, " = api.column(", frequency_col_index, ", {page: 'current'}).data();
                        
                        var sum", frequency_col_index, " = col", frequency_col_index, ".reduce(function(a, b) {
                          return parseInt(a) + parseInt(b);
                        }, 0);
                        
                        $(api.column(", frequency_col_index, ").header()).html('frequency<br><small>Sum: ' + sum", frequency_col_index, " + '</small>');
                      }
                    "))
                  ), colnames(df)),
                  rownames = FALSE,
                  selection = list(mode = 'single', target = 'row')
                  )
  })
  
  # DT proxy for efficient updates
  wordlist_proxy <- DT::dataTableProxy("wordlist_editor")
  
  # Observe edits in the word list editor and store them as pending
  observeEvent(input$wordlist_editor_cell_edit, {
    info <- input$wordlist_editor_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    if (!is.null(i) && !is.null(j)) {
      # Get the current word from the table to identify the row
      current_data <- isolate(word_data())
      word_value <- current_data$word[i]
      
      # Map DT column index (0-based) to data frame name
      col_name <- colnames(current_data)[j+1]
      
      # Store the edit in pending_edits - use a unique key for each edit
      current_edits <- isolate(pending_edits())
      edit_key <- paste0(word_value, "_", col_name, "_", i)
      current_edits[[edit_key]] <- list(
        word = word_value,
        row = i,
        column = col_name,
        value = v
      )
      pending_edits(current_edits)
      
      # Don't update the table display here - just store the edit
      # The table will show the original data until save is clicked
    }
  })
  
  # Handle save button click
  observeEvent(input$save_wordlist_btn, {
    current_edits <- isolate(pending_edits())
    
    if (length(current_edits) == 0) {
      # No edits to save
      showNotification("No changes to save", type = "default")
      return()
    }
    
    # Apply all pending edits to the word_data
    updated_data <- isolate(word_data())
    
    # Debug: check data structure
    print(paste("Data class:", class(updated_data)))
    print(paste("Data dimensions:", if(is.data.frame(updated_data)) paste(nrow(updated_data), ncol(updated_data)) else "not a data frame"))
    print(paste("Column names:", paste(colnames(updated_data), collapse = ", ")))
    
    # Ensure updated_data is a data frame
    if (!is.data.frame(updated_data)) {
      showNotification("Error: Data is not in the expected format", type = "error")
      return()
    }
    
    for (edit_key in names(current_edits)) {
      edit_info <- current_edits[[edit_key]]
      row_idx <- edit_info$row
      col_name <- edit_info$column
      new_value <- edit_info$value
      
      print(paste("Processing edit:", edit_key, "row:", row_idx, "col:", col_name, "value:", new_value))
      
      # Ensure row_idx is within bounds
      if (row_idx > 0 && row_idx <= nrow(updated_data)) {
        if (col_name %in% c("norm", "lemma", "grapho")) {
          updated_data[row_idx, col_name] <- as.character(new_value)
        } else if (col_name == "frequency") {
          updated_data[row_idx, col_name] <- suppressWarnings(as.numeric(new_value))
        }
      } else {
        print(paste("Row index out of bounds:", row_idx, "max rows:", nrow(updated_data)))
      }
    }
    
    # Save to CSV and update reactive value
    updated_data <- normalize_word_refkorpus_columns(updated_data)
    save_word_list_csv(updated_data)
    word_data(updated_data)
    
    # Clear pending edits
    pending_edits(list())
    
    y_i_analysis_data(NULL)
    s_longs_analysis_data(NULL)
    
    showNotification(
      "Changes saved successfully! Please re-run ẏ/i and s/\u017f analysis to see updated results.",
      type = "default"
    )
    
    # Update the table to reflect the saved state
    DT::replaceData(wordlist_proxy, updated_data, resetPaging = FALSE, rownames = FALSE)
  })
  
  # Render save status indicator
  output$save_status_indicator <- renderUI({
    current_edits <- pending_edits()
    if (length(current_edits) > 0) {
      tags$span(
        style = "color: #dc3545; font-weight: bold;",
        paste0("⚠ ", length(current_edits), " pending change(s)")
      )
    } else {
      tags$span(
        style = "color: #28a745; font-style: italic;",
        "✓ All changes saved"
      )
    }
  })
  
  # Download handler for rhyme analysis CSV
  output$download_rhymes_csv <- downloadHandler(
    filename = function() {
      paste0("yolanda_rhyme_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      if (is.null(rhyme_pairs_data())) {
        # Create empty CSV with message if no data
        empty_df <- data.frame(
          Message = "No rhyme data available. Please run rhyme analysis first.",
          stringsAsFactors = FALSE
        )
        write.csv(empty_df, file, row.names = FALSE, fileEncoding = "UTF-8")
        return()
      }
      
      # Get the rhyme data
      df <- rhyme_pairs_to_dataframe(rhyme_pairs_data(), word_data())
      
      if (nrow(df) == 0) {
        # Create empty CSV with message if no pairs found
        empty_df <- data.frame(
          Message = "No rhyming word pairs found in the text.",
          stringsAsFactors = FALSE
        )
        write.csv(empty_df, file, row.names = FALSE, fileEncoding = "UTF-8")
        return()
      }
      
      # Save the rhyme data to CSV
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # CSV status indicator
  output$csv_status_indicator <- renderUI({
    if (is.null(rhyme_pairs_data())) {
      tags$span(
        style = "color: #6c757d; font-style: italic;",
        "No data to export"
      )
    } else {
      total_pairs <- length(rhyme_pairs_data())
      tags$span(
        style = "color: #28a745; font-weight: bold;",
        paste0("✓ ", total_pairs, " pairs ready for export")
      )
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server) 