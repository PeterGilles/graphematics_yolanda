# Graphematics of Yolanda

Shiny app for the graphematic analysis of the *Yolanda* manuscript: letter combinations, grapheme–phoneme correspondences, rhyme words and rhyme pairs, based on a diplomatic edition of the text.

## Live version

The app is deployed at **https://petergill.shinyapps.io/Yolanda/**

## Files

| File | Purpose |
|------|---------|
| `app.R` | The Shiny app (UI and server) |
| `editioYolanda.txt` | Edition of the Yolanda text |
| `word_list.txt` | Word frequency list |
| `word_list_with_refkorpus.csv` | Word list with normalisation, lemma and grapheme–phoneme alignment |
| `word_list_with_kmhd.csv` | Word list (fallback if the Refkorpus file is missing) |
| `rhyme_pair_counts.csv` | Saved, manually edited rhyme pair counts |
| `scripts/replace_y_graph.R` | Helper script for normalising the ẏ grapheme |

## Running locally

```r
install.packages(c("shiny", "tidyverse", "stringr", "DT", "htmlwidgets", "webshot2"))
shiny::runApp()
```

Run it from this directory, since the app reads its data files from the working directory.
