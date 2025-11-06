suppressPackageStartupMessages({
  library(tidyverse)
  library(pdftools)
  library(tokenizers)
  library(jsonlite)
  library(glue)
  library(text2vec)
})

cat("\n", strrep("=", 70), "\n")
cat("EXECUTIVE ORDERS RETRIEVAL SYSTEM\n")
cat(strrep("=", 70), "\n\n")

CONFIG <- list(
  max_chunk_words = 500,
  download_retries = 3,
  download_sleep = 0.5
)

progress_bar <- function(current, total, width = 50) {
  pct <- current / total
  filled <- round(width * pct)
  bar <- paste0("[", strrep("#", filled), strrep("-", width - filled), 
                "] ", sprintf("%3.0f%%", pct * 100))
  cat("\r", bar, sep = "")
  if (current == total) cat("\n")
  flush.console()
}

fix_mojibake <- function(text) {
  if (!is.character(text)) return(text)
  text <- enc2utf8(text)
  text <- gsub("\u00AD", "", text)
  text <- gsub("[\u200B\u200C\u200D\u2060]", "", text, perl = TRUE)
  text <- gsub("\uFEFF", "", text)
  text <- gsub("\xe2\x80\x93", "-", text, useBytes = TRUE)
  text <- gsub("\xe2\x80\x94", "-", text, useBytes = TRUE)
  text <- gsub("\xe2\x80\x98", "'", text, useBytes = TRUE)
  text <- gsub("\xe2\x80\x99", "'", text, useBytes = TRUE)
  text <- gsub("\xe2\x80\x9c", '"', text, useBytes = TRUE)
  text <- gsub("\xe2\x80\x9d", '"', text, useBytes = TRUE)
  text <- gsub("\xe2\x80\xa6", "...", text, useBytes = TRUE)
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  text
}

dehyphenate <- function(text) {
  if (!is.character(text)) return(text)
  text <- gsub("([A-Za-z])-\\s*\n\\s*([A-Za-z])", "\\1\\2", text, perl = TRUE)
  text <- gsub("([A-Za-z])-\\s{1,2}([A-Za-z])", "\\1\\2", text, perl = TRUE)
  text <- gsub("([a-z])\n([a-z])", "\\1 \\2", text)
  text
}

count_tokens <- function(text) {
  round(str_count(text, "\\S+") * 1.3)
}

cat("Step 1: Validating metadata...\n")

if (!file.exists("metadata.csv")) {
  stop("metadata.csv not found")
}

meta <- read_csv("metadata.csv", show_col_types = FALSE, col_types = cols(.default = "c"))

required_cols <- c("pdf_url", "executive_order_number", "title", "signing_date")
missing_cols <- setdiff(required_cols, names(meta))

if (length(missing_cols) > 0) {
  stop(glue("metadata.csv missing columns: {paste(missing_cols, collapse = ', ')}"))
}

meta <- meta %>%
  mutate(
    executive_order_number = as.numeric(executive_order_number),
    signing_date = as.Date(signing_date, format = "%Y-%m-%d")
  ) %>%
  filter(!is.na(pdf_url), !is.na(executive_order_number)) %>%
  distinct(executive_order_number, .keep_all = TRUE)

cat(glue("Found {nrow(meta)} Executive Orders\n\n"))

cat("Step 2: Setting up directories...\n")

dirs <- c("executive_orders_pdf", "executive_orders_txt", "executive_orders_cleaned")
for (d in dirs) dir.create(d, showWarnings = FALSE, recursive = TRUE)

cat("Directories ready\n\n")

cat("Step 3: Downloading and extracting PDFs...\n")

download_with_retry <- function(url, dest, retries = CONFIG$download_retries) {
  for (i in 1:retries) {
    result <- try(download.file(url, dest, mode = "wb", quiet = TRUE), silent = TRUE)
    if (!inherits(result, "try-error") && file.exists(dest) && file.size(dest) > 0) {
      return(TRUE)
    }
    Sys.sleep(CONFIG$download_sleep)
  }
  FALSE
}

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

success_count <- 0
for (i in 1:nrow(meta)) {
  row <- meta[i, ]
  if (extract_pdf_text(row$executive_order_number, row$pdf_url)) {
    success_count <- success_count + 1
  }
  progress_bar(i, nrow(meta))
  Sys.sleep(0.1)
}

cat(glue("Extracted {success_count}/{nrow(meta)} EOs\n\n"))

cat("Step 4: Cleaning text...\n")

clean_text <- function(raw_text) {
  text <- raw_text
  text <- fix_mojibake(text)
  text <- dehyphenate(text)
  
  patterns <- c(
    "^.*verdate.*$", "^.*billing code.*$", "^.*\\[fr doc.*$",
    "^.*filed \\d.*$", "^.*_prezdoc.*$", "^.*presidential documents.*$"
  )
  
  for (pat in patterns) {
    text <- str_remove_all(text, regex(pat, multiline = TRUE, ignore_case = TRUE))
  }
  
  text <- str_remove_all(text, regex("federal register.*?presidential documents", 
                                     ignore_case = TRUE, dotall = TRUE))
  text <- str_remove_all(text, "\\b\\d{2}[a-z]{3}\\d{1,2}\\b")
  text <- str_remove_all(text, regex("^\\s*\\d{4,6}\\s*$", multiline = TRUE))
  text <- str_remove_all(text, regex("[a-z]+day,\\s*[a-z]+\\s*\\d+,\\s*\\d{4}", 
                                     ignore_case = TRUE))
  text <- str_remove_all(text, regex("jkt\\s+\\d+|po\\s+\\d+|frm\\s+\\d+|fmt\\s+\\d+|sfmt\\s+\\d+", 
                                     ignore_case = TRUE))
  text <- str_remove_all(text, regex("the white house.*?\\d{4}", ignore_case = TRUE))
  
  text <- str_replace_all(text, "\\n{3,}", "\n\n")
  text <- str_replace_all(text, "[ \\t]+", " ")
  text <- str_trim(text)
  
  fix_mojibake(text)
}

txt_files <- list.files("executive_orders_txt", "^EO_\\d+\\.txt$", full.names = TRUE)
cleaned_count <- 0

for (i in seq_along(txt_files)) {
  raw <- read_file(txt_files[i])
  cleaned <- clean_text(raw)
  out_path <- file.path("executive_orders_cleaned",
                        str_replace(basename(txt_files[i]), "\\.txt$", "_cleaned.txt"))
  write_file(cleaned, out_path)
  cleaned_count <- cleaned_count + 1
  progress_bar(i, length(txt_files))
}

cat(glue("Cleaned {cleaned_count} files\n\n"))

cat("Step 5: Chunking documents...\n")

chunk_document <- function(text_original, eo_number) {
  text_cleaned <- str_to_lower(text_original)
  
  words_original <- str_split(text_original, "\\s+")[[1]]
  words_cleaned <- str_split(text_cleaned, "\\s+")[[1]]
  
  if (length(words_cleaned) != length(words_original)) {
    min_len <- min(length(words_cleaned), length(words_original))
    words_cleaned <- words_cleaned[1:min_len]
    words_original <- words_original[1:min_len]
  }
  
  sentences <- tokenize_sentences(text_cleaned, strip_punct = FALSE, lowercase = FALSE)[[1]]
  
  if (length(sentences) == 0) return(tibble())
  
  chunks <- list()
  current_chunk <- NULL
  word_pos <- 1
  
  for (sent in sentences) {
    sent_word_count <- str_count(sent, "\\S+")
    if (sent_word_count == 0) next
    
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
  
  if (!is.null(current_chunk)) {
    chunks[[length(chunks) + 1]] <- current_chunk
  }
  
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

for (i in seq_along(cleaned_files)) {
  eo_num <- str_extract(basename(cleaned_files[i]), "\\d+")
  text_orig <- read_file(cleaned_files[i])
  chunks <- chunk_document(text_orig, eo_num)
  if (nrow(chunks) > 0) chunks_list[[eo_num]] <- chunks
  progress_bar(i, length(cleaned_files))
}

chunks_df <- bind_rows(chunks_list)

chunks_final <- chunks_df %>%
  left_join(meta %>% select(executive_order_number, title, signing_date),
            by = c("eo_number" = "executive_order_number")) %>%
  mutate(title = coalesce(title, glue("Executive Order {eo_number}")),
         title = fix_mojibake(title)) %>%
  select(doc_id, eo_number, chunk_id, title, signing_date, 
         text_original, text, word_count, token_count)

write_csv(chunks_final, "eo_chunks_final.csv")

cat(glue("Created {nrow(chunks_final)} chunks from {n_distinct(chunks_final$eo_number)} EOs\n\n"))

cat("Step 6: Building search index...\n")

tokens <- word_tokenizer(chunks_final$text)
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)
tfidf_model <- TfIdf$new()
dtm_tfidf <- tfidf_model$fit_transform(dtm)

cat(glue("Index built ({nrow(dtm_tfidf)} chunks x {ncol(dtm_tfidf)} terms)\n\n"))

retrieve_chunks <- function(query, top_n = 3) {
  if (!nzchar(query)) {
    cat("Empty query.\n")
    return(invisible(NULL))
  }
  
  q_tokens <- word_tokenizer(tolower(query))
  q_it <- itoken(q_tokens, progressbar = FALSE)
  q_dtm <- create_dtm(q_it, vectorizer)
  q_tfidf <- transform(q_dtm, tfidf_model)
  
  similarities <- sim2(dtm_tfidf, q_tfidf, method = "cosine", norm = "l2")
  top_indices <- order(similarities[, 1], decreasing = TRUE)[1:min(top_n, nrow(chunks_final))]
  
  results <- chunks_final[top_indices, ] %>%
    mutate(similarity = round(similarities[top_indices, 1] * 100, 1))
  
  cat(glue('\nTop {nrow(results)} results for: "{query}"\n\n'))
  
  for (i in 1:nrow(results)) {
    r <- results[i, ]
    cat(glue("[{i}] EO {r$eo_number}: {r$title}\n"))
    cat(glue("    Date: {r$signing_date} | Similarity: {r$similarity}%\n"))
    cat(glue("    Preview: {str_sub(r$text_original, 1, 200)}...\n\n"))
  }
  
  invisible(results)
}

cat("Console retrieval ready: retrieve_chunks('your query')\n\n")

cat("Step 7: Generating HTML...\n")

export_data <- chunks_final %>%
  left_join(meta %>% select(executive_order_number, pdf_url),
            by = c("eo_number" = "executive_order_number")) %>%
  mutate(signing_date = as.character(signing_date)) %>%
  select(doc_id, eo_number, chunk_id, title, signing_date, 
         text_original, text, word_count, token_count, pdf_url)

json_data <- toJSON(export_data, auto_unbox = TRUE, pretty = FALSE)
json_base64 <- base64enc::base64encode(charToRaw(json_data))

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
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  min-height: 100vh;
  padding: 20px;
}
.container { max-width: 1200px; margin: 0 auto; }
.header {
  background: white;
  padding: 40px;
  border-radius: 15px;
  box-shadow: 0 10px 30px rgba(0,0,0,0.2);
  margin-bottom: 30px;
  text-align: center;
}
.header h1 { color: #667eea; font-size: 2.5em; margin-bottom: 10px; }
.header p { color: #666; font-size: 1.1em; }
.search-box {
  background: white;
  padding: 30px;
  border-radius: 15px;
  box-shadow: 0 10px 30px rgba(0,0,0,0.2);
  margin-bottom: 30px;
}
.toggle-container {
  display: flex;
  align-items: center;
  gap: 15px;
  margin-bottom: 20px;
  padding: 15px;
  background: #f8f9fa;
  border-radius: 8px;
}
.toggle-label { font-weight: 600; color: #333; font-size: 1.1em; }
.toggle-switch {
  position: relative;
  width: 60px;
  height: 30px;
  background: #ccc;
  border-radius: 15px;
  cursor: pointer;
  transition: background 0.3s;
}
.toggle-switch.active { background: #667eea; }
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
}
.toggle-switch.active::after { transform: translateX(30px); }
.mode-indicator { font-size: 1.1em; color: #667eea; font-weight: 600; }
.mode-description { font-size: 0.9em; color: #666; font-style: italic; }
.search-input-group { display: flex; gap: 10px; }
.search-input {
  flex: 1;
  padding: 15px;
  font-size: 16px;
  border: 2px solid #ddd;
  border-radius: 8px;
  transition: border-color 0.3s;
}
.search-input:focus { outline: none; border-color: #667eea; }
.search-button {
  padding: 15px 40px;
  background: #667eea;
  color: white;
  border: none;
  border-radius: 8px;
  font-size: 16px;
  font-weight: 600;
  cursor: pointer;
  transition: background 0.3s;
}
.search-button:hover { background: #5568d3; }
.example-queries {
  background: #f8f9fa;
  padding: 15px;
  border-radius: 8px;
  margin-top: 15px;
}
.example-query {
  display: inline-block;
  background: white;
  padding: 5px 12px;
  border-radius: 15px;
  margin: 5px;
  cursor: pointer;
  border: 1px solid #ddd;
  transition: all 0.3s;
  font-size: 0.9em;
}
.example-query:hover {
  background: #667eea;
  color: white;
  border-color: #667eea;
}
.result-card {
  background: white;
  padding: 30px;
  margin-bottom: 15px;
  border-radius: 10px;
  box-shadow: 0 4px 12px rgba(0,0,0,0.1);
}
.result-header {
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  margin-bottom: 15px;
  padding-bottom: 15px;
  border-bottom: 2px solid #f0f0f0;
}
.result-title {
  font-size: 1.4em;
  color: #333;
  font-weight: 600;
  flex: 1;
}
.result-title a { color: #333; text-decoration: none; transition: color 0.3s; }
.result-title a:hover { color: #667eea; text-decoration: underline; }
.similarity-badge {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  padding: 8px 16px;
  border-radius: 20px;
  font-weight: 600;
  font-size: 0.9em;
  white-space: nowrap;
  margin-left: 20px;
}
.metadata {
  display: flex;
  gap: 20px;
  margin-bottom: 20px;
  flex-wrap: wrap;
  color: #666;
  font-size: 0.95em;
}
.metadata-item strong { color: #333; }
.chunk-text {
  background: #f8f9fa;
  padding: 20px;
  border-radius: 8px;
  line-height: 1.8;
  color: #444;
  border-left: 4px solid #667eea;
}
.highlight { 
  background: #fff59d; 
  padding: 2px 4px;
  border-radius: 3px;
  font-weight: 500;
}
.loading {
  text-align: center;
  padding: 40px;
  background: white;
  border-radius: 15px;
  box-shadow: 0 10px 30px rgba(0,0,0,0.2);
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

writeLines(glue('const CHUNKS_DATA_BASE64 = "{json_base64}";'), html_conn)

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
  constructor(documents) {
    this.documents = documents;
    this.vocabulary = new Set();
    this.idf = {};
    this.tfidfVectors = [];
    this.buildIndex();
  }
  
  tokenize(text) {
    return (text || "").toLowerCase()
      .replace(/[^\\w\\s]/g, " ")
      .split(/\\s+/)
      .filter(token => token.length > 2);
  }
  
  buildIndex() {
    const docFrequency = {};
    
    this.documents.forEach(doc => {
      const tokens = this.tokenize(doc.text);
      const uniqueTokens = new Set(tokens);
      
      uniqueTokens.forEach(token => {
        this.vocabulary.add(token);
        docFrequency[token] = (docFrequency[token] || 0) + 1;
      });
    });
    
    const numDocs = this.documents.length;
    for (const term of this.vocabulary) {
      this.idf[term] = Math.log(numDocs / (docFrequency[term] || 1));
    }
    
    this.documents.forEach(doc => {
      const tokens = this.tokenize(doc.text);
      const tf = {};
      
      tokens.forEach(token => {
        tf[token] = (tf[token] || 0) + 1;
      });
      
      const tfidf = {};
      for (const term in tf) {
        tfidf[term] = tf[term] * (this.idf[term] || 0);
      }
      
      this.tfidfVectors.push(tfidf);
    });
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
  
  retriever = new TfIdfRetriever(CHUNKS_DATA);
  
  const uniqueEOs = new Set(CHUNKS_DATA.map(c => c.eo_number)).size;
  document.getElementById("dataset-info").textContent = 
    `${CHUNKS_DATA.length} chunks from ${uniqueEOs} Executive Orders`;
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

file_size <- file.size(html_file) / 1024 / 1024
cat(glue("Generated {html_file} ({round(file_size, 2)} MB)\n\n"))

date_range <- range(chunks_final$signing_date, na.rm = TRUE)

cat(strrep("=", 70), "\n")
cat("COMPLETE\n")
cat(strrep("=", 70), "\n\n")
cat(glue("Chunks: {nrow(chunks_final)} from {n_distinct(chunks_final$eo_number)} EOs\n"))
cat(glue("Date range: {date_range[1]} to {date_range[2]}\n"))
cat(glue("Output: {html_file}\n\n"))