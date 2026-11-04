# The 26x26 orthogonal letter matrix ('a' through 'z')
letter_mat <- diag(26)
rownames(letter_mat) <- letters
colnames(letter_mat) <- letters

#  Append the padding row '_' (a vector of 26 zeros)
pad_row <- matrix(0, nrow = 1, ncol = 26, dimnames = list("_", letters))
orth_dict <- rbind(letter_mat, pad_row)

# Read dataset and guarantee lowercase
df <- read.csv("phon-code-kid.csv", stringsAsFactors = FALSE)
words <- tolower(trimws(as.character(df$word)))

# Padding: right-pad words to max length (9 slots) with '_'
slots <- max(nchar(words))
if (any(nchar(words) != slots)) {
  words_padded <- vapply(words, function(w) {
    paste0(w, paste(rep("_", slots - nchar(w)), collapse = ""))
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
} else {
  words_padded <- words
}

# Reshape into 2,869 rows x (slots * 26) columns
all_letters <- unlist(strsplit(words_padded, ""))
lookup_indices <- match(all_letters, rownames(orth_dict))
expanded_letters <- orth_dict[lookup_indices, , drop = FALSE]

orth_binary <- matrix(
  t(expanded_letters),
  nrow = length(words_padded),
  ncol = slots * 26,
  byrow = TRUE
)

# Export to CSV without row names or column headers
write.table(
  orth_binary,
  file = "orth.csv",
  sep = ",",
  row.names = FALSE,
  col.names = FALSE
)
