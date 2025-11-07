suppressPackageStartupMessages({
  library(tidyverse)
  library(pdftools)
  library(tokenizers)
  library(jsonlite)
  library(glue)
  library(text2vec)
})

CONFIG <- list(
  max_chunk_words = 500,
  download_retries = 3,
  download_sleep = 0.5
)

### Ingesting the text from .pdf format caused periodic appearance of garbled text in the final product. I tried to overcome this by ingesting the .html format instead,
### but the website blocked me at the pass. There may be an API, but instead I stayed on the .pdf ingestion route and exposed Claude and GPT-5 to several iterations until
### it gave me a working solution which is in the code block below. My objective was to have the UI retrieve and display the original text to the user in order to establish
### credibility/transparency for the retrieval system. 

fix_mojibake <- function(text) {
  ### Function only operates on vector with type character
  if (!is.character(text)) return(text)
  ## Force conversion to UTF-8 encoding
  text <- enc2utf8(text)
  ## Remove soft hyphen that can interfere with word matching
  text <- gsub("\u00AD", "", text)
  ## Remove zero width characters - space, non-joiner, joiner, word joiner and use upgraded Perl-compatible regular expressions over R base regex engine
  text <- gsub("[\u200B\u200C\u200D\u2060]", "", text, perl = TRUE)
  ## Remove byte order marks (invisible character at start of text files to dictate encoding type) if they are there
  text <- gsub("\uFEFF", "", text)
  ## Replace EM dash with hyphen and force R to match the actual byte codes in lieu of the unicode character 
  text <- gsub("\xe2\x80\x93", "-", text, useBytes = TRUE)
  ## Replace EN dash with hyphen
  text <- gsub("\xe2\x80\x94", "-", text, useBytes = TRUE)
  ## Replace curly left single quote with straight single quote
  text <- gsub("\xe2\x80\x98", "'", text, useBytes = TRUE)
  ## Replace curly right single quote with straight single quote
  text <- gsub("\xe2\x80\x99", "'", text, useBytes = TRUE)
  ## Replace curly left double quote with straight double quote
  text <- gsub("\xe2\x80\x9c", '"', text, useBytes = TRUE)
  ## Replace curly right double quote with straight double quote
  text <- gsub("\xe2\x80\x9d", '"', text, useBytes = TRUE)
  ## Replace ellipsis with three periods
  text <- gsub("\xe2\x80\xa6", "...", text, useBytes = TRUE)
  ## After several trial and error iterations Claude recommended leave UTF-8 for ASCII and drop out anything that does not convert
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  text
}

dehyphenate <- function(text) {
  ## Operates only on character vectors
  if (!is.character(text)) return(text)
  ## Fix hyphenated line breaks with newline, seek pattern letter followed by optional space followed by linebreak followed by optional space followed by letter
  ## replace with two letters concatenated 
  text <- gsub("([A-Za-z])-\\s*\n\\s*([A-Za-z])", "\\1\\2", text, perl = TRUE)
  ## Fix hyphenated linebreaks without new line, similar pattern as above but without newline and same replacement
  text <- gsub("([A-Za-z])-\\s{1,2}([A-Za-z])", "\\1\\2", text, perl = TRUE)
  ## If a word is split with a linebreak a space is inserted between the two words vice newline - may improve search functionality
  text <- gsub("([a-z])\n([a-z])", "\\1 \\2", text)
  text
}

count_tokens <- function(text) {
  round(str_count(text, "\\S+") * 1.3)
}

## Downloaded initial assignment dataframe as metadata.csv, read in all the columns as type character 
meta <- read_csv("metadata.csv", show_col_types = FALSE, col_types = cols(.default = "c"))

meta <- meta %>%
  mutate(
    executive_order_number = as.numeric(executive_order_number),
    signing_date = as.Date(signing_date, format = "%Y-%m-%d")
  ) 
  
dirs <- c("executive_orders_pdf", "executive_orders_txt", "executive_orders_cleaned")
for (d in dirs) dir.create(d, showWarnings = FALSE)

download_with_retry <- function(url, dest, retries = CONFIG$download_retries) {
  for (i in 1:retries) {
    ## result gets loaded with numeric value 0 if download is a success, otherwise a try-error object containing the error message
    result <- try(download.file(url, dest, mode = "wb", quiet = TRUE), silent = TRUE)
    ## if all three conditions are met then move on to the next one, otherwise expend remaining tries, otherwise kill the loop
    if (!inherits(result, "try-error") && file.exists(dest) && file.size(dest) > 0) {
      return(TRUE)
    }
    Sys.sleep(CONFIG$download_sleep)
  }
  FALSE
}

### Use pdftools to extract text from PDF, save to .txt file, return TRUE if successful, FALSE otherwise. Don't re-download if .txt file already exists and is non-zero size.

extract_pdf_text <- function(eo_number, pdf_url) {
  pdf_path <- file.path("executive_orders_pdf", glue("EO_{eo_number}.pdf"))
  txt_path <- file.path("executive_orders_txt", glue("EO_{eo_number}.txt"))
  
  if (file.exists(txt_path) && file.size(txt_path) > 0) return(TRUE)
  
  if (!file.exists(pdf_path) || file.size(pdf_path) == 0) {
    if (!download_with_retry(pdf_url, pdf_path)) return(FALSE)
  }
  
  text <- tryCatch({
    pages <- pdf_text(pdf_path)
    if (length(pages) > 0) paste(pages, collapse = "\n\n") else NULL
  }, error = function(e) NULL)
  
  if (is.null(text) || !nzchar(text)) return(FALSE)
  
  write_file(text, txt_path)
  TRUE
}

### Execute the download procedure if needed

invisible(lapply(seq_len(nrow(meta)), function(i) {
  extract_pdf_text(
    eo_number = meta$executive_order_number[i],
    pdf_url   = meta$pdf_url[i]
  )
}))

clean_text <- function(raw_text) {
  text <- raw_text
  text <- fix_mojibake(text)
  text <- dehyphenate(text)
  
## Remove clutter that consistently appears on their own lines throughout the documents
  patterns <- c(
    "^.*verdate.*$", "^.*billing code.*$", "^.*\\[fr doc.*$",
    "^.*filed \\d.*$", "^.*_prezdoc.*$", "^.*presidential documents.*$"
  )
  
  for (pat in patterns) {
    text <- str_remove_all(text, regex(pat, multiline = TRUE, ignore_case = TRUE))
  }
  
  ## Take out the headers - remove everything from federal register to presidential documents
  text <- str_remove_all(text, regex("federal register.*?presidential documents", 
                                     ignore_case = TRUE, dotall = TRUE))
  ## Using word boundary pattern to find isolated dates that are not discussed inside the EO text - good candidates for removal                                  
  text <- str_remove_all(text, "\\b\\d{2}[a-z]{3}\\d{1,2}\\b")
  ## Remove number sequences four to six digits that appear on their own lines
  text <- str_remove_all(text, regex("^\\s*\\d{4,6}\\s*$", multiline = TRUE))
  ## Target date lines that appear below THE WHITE HOUSE.
  text <- str_remove_all(text, regex("[a-z]+day,\\s*[a-z]+\\s*\\d+,\\s*\\d{4}", 
                                     ignore_case = TRUE))
  ## I used an LLM to inspect the extracted text from the PDFs... Even though not visually present in the PDFs the post-extraction raw text manifested odd codes with
  ## jkt, po, frm, fmt, and sfmt patterns that were tainting the corpus 
  text <- str_remove_all(text, regex("jkt\\s+\\d+|po\\s+\\d+|frm\\s+\\d+|fmt\\s+\\d+|sfmt\\s+\\d+", 
                                     ignore_case = TRUE))
  ## Remove any trailing THE WHITE HOUSE lines with year at the end of the document                                   
  text <- str_remove_all(text, regex("the white house.*?\\d{4}", ignore_case = TRUE))
  ## Remove blocks of white space greater than three lines  
  text <- str_replace_all(text, "\\n{3,}", "\n\n")
  ## Normalize spaces and tabs to single space
  text <- str_replace_all(text, "[ \\t]+", " ")
  ## Remove leading and trailing whitespace
  text <- str_trim(text)
  
  fix_mojibake(text)
}

### Execute the cleaning procedure if needed

raw_files <- list.files(
  "executive_orders_txt",
  pattern = "\\.txt$",
  full.names = TRUE
)

invisible(lapply(raw_files, function(f) {
  raw <- readr::read_file(f)
  cleaned <- clean_text(raw)
  
  out_name <- gsub("\\.txt$", "_cleaned.txt", basename(f))
  out_path <- file.path("executive_orders_cleaned", out_name)
  
  readr::write_file(cleaned, out_path)
}))


chunk_document <- function(text_original, eo_number) {
  text_cleaned <- str_to_lower(text_original)
  
## Keeping the original text for display purposes later. Split words based on any whitespace 

  words_original <- str_split(text_original, "\\s+")[[1]]
  words_cleaned <- str_split(text_cleaned, "\\s+")[[1]]

## If the two vectors do not have the same length, then truncate to the shorter length to maintain alignment

  if (length(words_cleaned) != length(words_original)) {
    min_len <- min(length(words_cleaned), length(words_original))
    words_cleaned <- words_cleaned[1:min_len]
    words_original <- words_original[1:min_len]
  }
## Use tokenize_sentences from the tokenizers package to split cleaned text into sentences
  sentences <- tokenize_sentences(text_cleaned, strip_punct = FALSE, lowercase = FALSE)[[1]]
  
  if (length(sentences) == 0) return(tibble())

## Build list of chunks using sentence boundaries, but do not exceed 500 words.

  chunks <- list()
  current_chunk <- NULL
  word_pos <- 1
  
  for (sent in sentences) {
## Count words which we define here as tokens that are not spaces/whitespace 
    sent_word_count <- str_count(sent, "\\S+")
## If no words in there do not pack it into a chunk
    if (sent_word_count == 0) next
## Continue iterating over each sentence and adding to the current chunk until max words is exceeded at 500. If the next sentence trips the 500 word limit
## then the chunk will be packed with the last completed sentence and the next sentence will start a new chunk. 
    if (is.null(current_chunk)) {
      current_chunk <- list(start = word_pos, end = word_pos + sent_word_count - 1,
                            word_count = sent_word_count)
    } else if (current_chunk$word_count + sent_word_count > CONFIG$max_chunk_words) {
      chunks[[length(chunks) + 1]] <- current_chunk
      current_chunk <- list(start = word_pos, end = word_pos + sent_word_count - 1,
                            word_count = sent_word_count)
    } else {
      current_chunk$end <- word_pos + sent_word_count - 1
      current_chunk$word_count <- current_chunk$word_count + sent_word_count
    }
    
    word_pos <- word_pos + sent_word_count
  }
## Pack the last chunk since it likely did not exceed the trip limit  
  if (!is.null(current_chunk)) {
    chunks[[length(chunks) + 1]] <- current_chunk
  }

### Use map2_dfr fxn from purrr to iterate over chunks and build a dataframe with original and cleaned text for each chunk. seq_along used to generate chunk_id.
### Also count words and tokens for each chunk. map2_dfr takes two inputs - chunks list and sequence of indices, applies the function to each pair, 
### and combines results into a dataframe. 

  map2_dfr(chunks, seq_along(chunks), function(chunk, idx) {
    start_idx <- chunk$start
    end_idx <- min(chunk$end, length(words_cleaned))
    
    text_orig <- paste(words_original[start_idx:end_idx], collapse = " ")
    text_clean <- paste(words_cleaned[start_idx:end_idx], collapse = " ")
    
    tibble(
      doc_id = glue("EO_{eo_number}"),
      eo_number = as.numeric(eo_number),
      chunk_id = idx,
      text_original = text_orig,
      text = text_clean,
      word_count = end_idx - start_idx + 1,
      token_count = count_tokens(text_clean)
    )
  })
}

cleaned_files <- list.files("executive_orders_cleaned", "_cleaned\\.txt$", full.names = TRUE)
chunks_list <- list()

### For every .txt file in the cleaned directory, extract the EO number from the filename, read in the text, chunk it, and store the resulting dataframe in a list.
### Ensure we only include dataframes with more than zero rows to avoid errors during binding.

for (i in seq_along(cleaned_files)) {
  eo_num <- str_extract(basename(cleaned_files[i]), "\\d+")
  text_orig <- read_file(cleaned_files[i])
  chunks <- chunk_document(text_orig, eo_num)
  if (nrow(chunks) > 0) chunks_list[[eo_num]] <- chunks
}

### Combine all chunk dataframes into a single dataframe

chunks_df <- bind_rows(chunks_list)

### Hook chunks back up to the original assignment dataframe

chunks_final <- chunks_df %>%
  left_join(meta %>% select(executive_order_number, title, signing_date),
            by = c("eo_number" = "executive_order_number")) %>%
  mutate(title = coalesce(title, glue("Executive Order {eo_number}")),
         title = fix_mojibake(title)) %>%
  select(doc_id, eo_number, chunk_id, title, signing_date, 
         text_original, text, word_count, token_count)

write_csv(chunks_final, "eo_chunks_final.csv")


### Mine tokens from each chunk
tokens <- word_tokenizer(chunks_final$text)
### text2vec enables us to use the itoken function to feed downstream functions one chunk at a time instead of everything at once improving memory use
it <- itoken(tokens, progressbar = FALSE)
### vocabulary df contains the term, number of times in the whole corpus, number of chunks containing term, and number of words in the token
vocab <- create_vocabulary(it)
### text2vec builds function necessary for DFM creation
vectorizer <- vocab_vectorizer(vocab)
### text2vec builds DFM
dtm <- create_dtm(it, vectorizer)
### text2vec creates a TF-IDF transformer that will convert raw term counts into weighted importance scores
tfidf_model <- TfIdf$new()
### Fit the TF-IDF model on the DTM and return a new matrix of TF-IDF-weighted values
dtm_tfidf <- tfidf_model$fit_transform(dtm)

export_data <- chunks_final %>%
  left_join(meta %>% select(executive_order_number, pdf_url),
            by = c("eo_number" = "executive_order_number")) %>%
  mutate(signing_date = as.character(signing_date)) %>%
  select(doc_id, eo_number, chunk_id, title, signing_date, 
         text_original, text, word_count, token_count, pdf_url)

json_data <- toJSON(export_data, auto_unbox = TRUE, pretty = FALSE)
json_base64 <- base64enc::base64encode(charToRaw(json_data))

### Export TF-IDF vectors and vocabulary for JavaScript to use
tfidf_matrix <- as.matrix(dtm_tfidf)
vocab_terms <- colnames(tfidf_matrix)

### Better IDF extraction
idf_vector <- tfidf_model$idf
names(idf_vector) <- vocab_terms

### Export TF-IDF vectors and vocabulary for JavaScript to use
tfidf_matrix <- as.matrix(dtm_tfidf)
vocab_terms <- colnames(tfidf_matrix)

### Calculate IDF scores from the DTM (since tfidf_model doesn't expose them directly)
### IDF = log(total_docs / docs_containing_term)
dtm_binary <- as.matrix(dtm > 0)  # Convert to binary (term present or not)
doc_freq <- colSums(dtm_binary)   # Count docs containing each term
num_docs <- nrow(dtm)
idf_scores <- log(num_docs / doc_freq)
names(idf_scores) <- vocab_terms

### Create a list structure that JavaScript can use
tfidf_export <- list(
  vocabulary = vocab_terms,
  idf_scores = as.list(idf_scores),
  doc_vectors = lapply(seq_len(nrow(tfidf_matrix)), function(i) {
    row <- tfidf_matrix[i, ]
    ### Only export non-zero values to reduce size
    non_zero <- which(row != 0)
    if (length(non_zero) == 0) return(list())
    setNames(as.list(row[non_zero]), vocab_terms[non_zero])
  })
)

### Convert to JSON and base64 encode
tfidf_json <- toJSON(tfidf_export, auto_unbox = TRUE, pretty = FALSE)
tfidf_base64 <- base64enc::base64encode(charToRaw(tfidf_json))
html_file <- "EO_Retrieval_System.html"
html_conn <- file(html_file, "w", encoding = "UTF-8")

writeLines('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Executive Orders Retrieval System</title>
<style>
* { margin: 0; padding: 0; box-sizing: border-box; }
body {
  font-family: "Source Sans Pro", -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
  background: #f1f1f1;
  min-height: 100vh;
  padding: 0;
  color: #212121;
}
.gov-banner {
  background: #f0f0f0;
  border-bottom: 1px solid #d6d7d9;
  padding: 8px 0;
  font-size: 0.85em;
}
.gov-banner .container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 0 20px;
  color: #5b616b;
}
.site-header {
  background: #112e51;
  color: white;
  padding: 20px 0;
  border-bottom: 3px solid #0071bc;
}
.site-header .container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 0 20px;
  display: flex;
  justify-content: space-between;
  align-items: center;
}
.site-title {
  font-size: 1.8em;
  font-weight: 700;
  color: white;
}
.site-subtitle {
  font-size: 0.9em;
  color: #aeb0b5;
  margin-top: 2px;
}
.container { max-width: 1200px; margin: 0 auto; padding: 0 20px; }
.content-area {
  padding: 40px 0;
}
.page-header {
  background: white;
  padding: 30px 40px;
  border-radius: 3px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
  margin-bottom: 30px;
  border-left: 4px solid #0071bc;
}
.page-header h1 { 
  color: #112e51; 
  font-size: 2.2em; 
  margin-bottom: 8px;
  font-weight: 700;
}
.page-header p { color: #5b616b; font-size: 1.05em; line-height: 1.5; }
.search-box {
  background: white;
  padding: 30px 40px;
  border-radius: 3px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
  margin-bottom: 30px;
}
.toggle-container {
  display: flex;
  align-items: center;
  gap: 15px;
  margin-bottom: 25px;
  padding: 18px;
  background: #f9f9f9;
  border-radius: 3px;
  border: 1px solid #d6d7d9;
}
.toggle-label { 
  font-weight: 600; 
  color: #112e51; 
  font-size: 1.05em; 
}
.toggle-switch {
  position: relative;
  width: 60px;
  height: 30px;
  background: #aeb0b5;
  border-radius: 15px;
  cursor: pointer;
  transition: background 0.3s;
}
.toggle-switch.active { background: #0071bc; }
.toggle-switch::after {
  content: "";
  position: absolute;
  top: 3px;
  left: 3px;
  width: 24px;
  height: 24px;
  background: white;
  border-radius: 50%;
  transition: transform 0.3s;
  box-shadow: 0 1px 3px rgba(0,0,0,0.3);
}
.toggle-switch.active::after { transform: translateX(30px); }
.mode-indicator { 
  font-size: 1.05em; 
  color: #0071bc; 
  font-weight: 700; 
}
.mode-description { 
  font-size: 0.9em; 
  color: #5b616b; 
  font-style: italic; 
}
.search-input-group { 
  display: flex; 
  gap: 12px; 
  margin-bottom: 20px;
}
.search-input {
  flex: 1;
  padding: 14px 16px;
  font-size: 16px;
  border: 1px solid #aeb0b5;
  border-radius: 3px;
  transition: border-color 0.3s, box-shadow 0.3s;
  font-family: inherit;
}
.search-input:focus { 
  outline: none; 
  border-color: #0071bc; 
  box-shadow: 0 0 0 2px rgba(0, 113, 188, 0.2);
}
.search-button {
  padding: 14px 32px;
  background: #0071bc;
  color: white;
  border: none;
  border-radius: 3px;
  font-size: 16px;
  font-weight: 700;
  cursor: pointer;
  transition: background 0.3s;
  font-family: inherit;
}
.search-button:hover { background: #205493; }
.search-button:active { background: #112e51; }
.example-queries {
  background: #f9f9f9;
  padding: 16px;
  border-radius: 3px;
  border: 1px solid #d6d7d9;
}
.example-queries-label {
  font-size: 0.9em;
  color: #5b616b;
  margin-bottom: 8px;
  font-weight: 600;
}
.example-query {
  display: inline-block;
  background: white;
  padding: 6px 14px;
  border-radius: 3px;
  margin: 4px;
  cursor: pointer;
  border: 1px solid #aeb0b5;
  transition: all 0.2s;
  font-size: 0.9em;
  color: #0071bc;
  font-weight: 600;
}
.example-query:hover {
  background: #0071bc;
  color: white;
  border-color: #0071bc;
}
.result-card {
  background: white;
  padding: 32px;
  margin-bottom: 20px;
  border-radius: 3px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
  border-left: 4px solid #0071bc;
}
.result-header {
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  margin-bottom: 16px;
  padding-bottom: 16px;
  border-bottom: 1px solid #d6d7d9;
}
.result-title {
  font-size: 1.35em;
  color: #112e51;
  font-weight: 700;
  flex: 1;
  line-height: 1.4;
}
.result-title a { 
  color: #0071bc; 
  text-decoration: underline; 
  transition: color 0.2s; 
}
.result-title a:hover { 
  color: #205493; 
}
.similarity-badge {
  background: #0071bc;
  color: white;
  padding: 6px 14px;
  border-radius: 3px;
  font-weight: 700;
  font-size: 0.9em;
  white-space: nowrap;
  margin-left: 20px;
}
.metadata {
  display: flex;
  gap: 24px;
  margin-bottom: 20px;
  flex-wrap: wrap;
  color: #5b616b;
  font-size: 0.9em;
}
.metadata-item { 
  display: flex; 
  gap: 6px; 
}
.metadata-item strong { 
  color: #112e51; 
  font-weight: 700;
}
.chunk-text {
  background: #f9f9f9;
  padding: 20px;
  border-radius: 3px;
  line-height: 1.7;
  color: #212121;
  border: 1px solid #d6d7d9;
  font-size: 0.95em;
}
.highlight { 
  background: #fdb81e; 
  padding: 2px 4px;
  border-radius: 2px;
  font-weight: 600;
  color: #212121;
}
.loading {
  text-align: center;
  padding: 50px;
  background: white;
  border-radius: 3px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
}
.loading h3 {
  color: #112e51;
  font-size: 1.4em;
  margin-bottom: 10px;
}
.loading p {
  color: #5b616b;
  font-size: 1.05em;
}
</style>
</head>
<body>
<div class="container">
<div class="header">
  <h1>Executive Orders Retrieval System</h1>
  <p id="dataset-info">Loading...</p>
</div>
<div class="search-box">
  <div class="toggle-container">
    <span class="toggle-label">Search Mode:</span>
    <div id="toggleSwitch" class="toggle-switch" onclick="toggleSearchMode()"></div>
    <span class="mode-indicator" id="modeIndicator">TF-IDF</span>
    <span class="mode-description" id="modeDescription">(Semantic similarity)</span>
  </div>
  <div class="search-input-group">
    <input type="text" id="searchInput" class="search-input" 
           placeholder="e.g., immigration policy, energy subsidies..."
           onkeypress="if(event.key===\'Enter\') performSearch()">
    <button class="search-button" onclick="performSearch()">Search</button>
  </div>
  <div class="example-queries">
    <span class="example-query" onclick="setQuery(\'immigration policy\')">immigration policy</span>
    <span class="example-query" onclick="setQuery(\'energy subsidies\')">energy subsidies</span>
    <span class="example-query" onclick="setQuery(\'federal government\')">federal government</span>
    <span class="example-query" onclick="setQuery(\'tax credits\')">tax credits</span>
  </div>
</div>
<div id="resultsContainer"></div>
</div>
<script>', html_conn)

writeLines(glue('const CHUNKS_DATA_BASE64 = "{json_base64}";
const TFIDF_DATA_BASE64 = "{tfidf_base64}";'), html_conn)

writeLines('
function loadData() {
  try {
    const jsonStr = atob(CHUNKS_DATA_BASE64);
    return JSON.parse(jsonStr);
  } catch(e) {
    console.error("Failed to load data:", e);
    return [];
  }
}

const CHUNKS_DATA = loadData();
let searchMode = "tfidf";

class TfIdfRetriever {
  constructor(documents, precomputedTfidf) {
    this.documents = documents;
    this.vocabulary = new Set(precomputedTfidf.vocabulary);
    this.idf = precomputedTfidf.idf_scores;
    this.tfidfVectors = precomputedTfidf.doc_vectors;
  }
  
  tokenize(text) {
    return (text || "").toLowerCase()
      .replace(/[^\\w\\s]/g, " ")
      .split(/\\s+/)
      .filter(token => token.length > 2);
  }
  
  cosineSimilarity(vec1, vec2) {
    let dotProduct = 0;
    let mag1 = 0;
    let mag2 = 0;
    
    const allTerms = new Set([...Object.keys(vec1), ...Object.keys(vec2)]);
    
    allTerms.forEach(term => {
      const v1 = vec1[term] || 0;
      const v2 = vec2[term] || 0;
      
      dotProduct += v1 * v2;
      mag1 += v1 * v1;
      mag2 += v2 * v2;
    });
    
    if (mag1 === 0 || mag2 === 0) return 0;
    
    return dotProduct / (Math.sqrt(mag1) * Math.sqrt(mag2));
  }
  
  search(query, topN = 3) {
    const tokens = this.tokenize(query);
    const tf = {};
    
    tokens.forEach(token => {
      tf[token] = (tf[token] || 0) + 1;
    });
    
    const queryTfidf = {};
    for (const term in tf) {
      queryTfidf[term] = tf[term] * (this.idf[term] || 0);
    }
    
    const similarities = this.tfidfVectors.map((docVector, index) => ({
      index: index,
      similarity: this.cosineSimilarity(queryTfidf, docVector),
      document: this.documents[index]
    }));
    
    similarities.sort((a, b) => b.similarity - a.similarity);
    
    return similarities.slice(0, topN);
  }
}

function keywordSearch(query, topN = 3) {
  const queryTerms = query.toLowerCase()
    .replace(/[^\\w\\s]/g, " ")
    .split(/\\s+/)
    .filter(t => t.length > 2);
  
  if (queryTerms.length === 0) return [];
  
  const scored = CHUNKS_DATA.map(doc => {
    const text = (doc.text || "").toLowerCase();
    let score = 0;
    let matchCount = 0;
    
    queryTerms.forEach(term => {
      const regex = new RegExp("\\\\b" + term + "\\\\b", "g");
      const matches = text.match(regex);
      if (matches) {
        matchCount++;
        score += matches.length;
      }
    });
    
    const termCoverage = matchCount / queryTerms.length;
    const frequency = score / text.split(/\\s+/).length;
    const similarity = (termCoverage * 0.7 + frequency * 0.3);
    
    return {
      index: CHUNKS_DATA.indexOf(doc),
      similarity: similarity,
      document: doc
    };
  });
  
  scored.sort((a, b) => b.similarity - a.similarity);
  
  return scored.slice(0, topN);
}

let retriever = null;

function toggleSearchMode() {
  const toggle = document.getElementById("toggleSwitch");
  const indicator = document.getElementById("modeIndicator");
  const description = document.getElementById("modeDescription");
  
  toggle.classList.toggle("active");
  
  if (toggle.classList.contains("active")) {
    searchMode = "keyword";
    indicator.textContent = "Keyword";
    description.textContent = "(Exact word matching)";
  } else {
    searchMode = "tfidf";
    indicator.textContent = "TF-IDF";
    description.textContent = "(Semantic similarity)";
  }
  
  const query = document.getElementById("searchInput").value.trim();
  if (query) {
    performSearch();
  }
}

function highlightTerms(text, query) {
  if (!query) return text;
  
  const terms = query.toLowerCase()
    .replace(/[^\\w\\s]/g, " ")
    .split(/\\s+/)
    .filter(t => t.length > 2);
  
  let result = text;
  
  terms.forEach(term => {
    const escapedTerm = term.replace(/[.*+?^${}()|[\\]\\\\]/g, "\\\\$&");
    const regex = new RegExp("\\\\b(" + escapedTerm + ")\\\\b", "gi");
    result = result.replace(regex, \'<span class="highlight">$1</span>\');
  });
  
  return result;
}

function initializeApp() {
  if (CHUNKS_DATA.length === 0) {
    document.getElementById("dataset-info").textContent = "ERROR: No data loaded";
    document.querySelector(".search-button").disabled = true;
    return;
  }
  
  // Load precomputed TF-IDF data
  let tfidfData;
  try {
    const tfidfStr = atob(TFIDF_DATA_BASE64);
    tfidfData = JSON.parse(tfidfStr);
  } catch(e) {
    console.error("Failed to load TF-IDF data:", e);
    document.getElementById("dataset-info").textContent = "ERROR: Failed to load TF-IDF model";
    return;
  }
  
  retriever = new TfIdfRetriever(CHUNKS_DATA, tfidfData);
  
  const uniqueEOs = new Set(CHUNKS_DATA.map(c => c.eo_number)).size;
  document.getElementById("dataset-info").textContent = 
    `${CHUNKS_DATA.length} chunks from ${uniqueEOs} Executive Orders (using R text2vec TF-IDF)`;
}

function setQuery(query) {
  document.getElementById("searchInput").value = query;
  performSearch();
}

function performSearch() {
  const query = document.getElementById("searchInput").value.trim();
  
  if (!query) {
    alert("Please enter a search query");
    return;
  }
  
  if (!retriever) {
    alert("System not initialized");
    return;
  }
  
  const resultsContainer = document.getElementById("resultsContainer");
  resultsContainer.innerHTML = `<div class="loading">Searching...</div>`;
  
  setTimeout(() => {
    let results;
    if (searchMode === "tfidf") {
      results = retriever.search(query, 3);
    } else {
      results = keywordSearch(query, 3);
    }
    displayResults(query, results);
  }, 100);
}

function displayResults(query, results) {
  const resultsContainer = document.getElementById("resultsContainer");
  
  if (results.length === 0 || results[0].similarity === 0) {
    resultsContainer.innerHTML = `
      <div class="loading">
        <h3>No results found</h3>
        <p>Try different search terms or switch modes</p>
      </div>
    `;
    return;
  }
  
  let html = "";
  
  results.forEach((result, index) => {
    const doc = result.document;
    const simPct = (result.similarity * 100).toFixed(1);
    
    const date = doc.signing_date ? 
      new Date(doc.signing_date).toLocaleDateString("en-US", { 
        year: "numeric", month: "long", day: "numeric" 
      }) : "Date unknown";
    
    let displayText = doc.text_original || doc.text || "";
    if (displayText.length > 800) {
      displayText = displayText.substring(0, 800) + "...";
    }
    
    displayText = highlightTerms(displayText, query);
    
    const titleDisplay = doc.pdf_url 
      ? `<a href="${doc.pdf_url}" target="_blank">#${index + 1}: ${doc.title}</a>`
      : `#${index + 1}: ${doc.title}`;
    
    html += `
      <div class="result-card">
        <div class="result-header">
          <div class="result-title">${titleDisplay}</div>
          <div class="similarity-badge">${simPct}%</div>
        </div>
        <div class="metadata">
          <div class="metadata-item"><strong>EO:</strong> ${doc.eo_number}</div>
          <div class="metadata-item"><strong>Date:</strong> ${date}</div>
          <div class="metadata-item"><strong>Chunk:</strong> ${doc.chunk_id}</div>
          <div class="metadata-item"><strong>Words:</strong> ${doc.word_count}</div>
        </div>
        <div class="chunk-text">${displayText}</div>
      </div>
    `;
  });
  
  resultsContainer.innerHTML = html;
}

window.addEventListener("DOMContentLoaded", initializeApp);
</script>
</body>
</html>', html_conn)

close(html_conn)