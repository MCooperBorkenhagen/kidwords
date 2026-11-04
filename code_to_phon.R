
phonreps = read.csv("phonreps.csv", stringsAsFactors = F)
# Set rownames on phonreps
phonreps <- as.data.frame(phonreps)

# If the first column contains the phoneme characters, use it as row names:
if ("phoneme" %in% colnames(phonreps)) {
  rownames(phonreps) <- trimws(as.character(phonreps$phoneme))
  phonreps$phoneme <- NULL
} else if (!is.null(rownames(phonreps)) && !all(rownames(phonreps) == as.character(1:nrow(phonreps)))) {
  rownames(phonreps) <- trimws(rownames(phonreps))
} else {
  # If phonemes are in column 1 without the name 'phoneme'
  rownames(phonreps) <- trimws(as.character(phonreps[[1]]))
  phonreps[[1]] <- NULL
}

# Convert feature values to numeric matrix
phon_mat <- as.matrix(phonreps)
mode(phon_mat) <- "numeric"

# Append the padding row "_" of zeros
pad_row <- matrix(0, nrow = 1, ncol = ncol(phon_mat), dimnames = list("_", colnames(phon_mat)))
phon_dict <- rbind(phon_mat, pad_row)

# Load codes
df <- read.csv("phon-code-kid.csv", stringsAsFactors = FALSE)

# Avoid any subscript bounds errors
all_chars <- unlist(strsplit(df$code, ""))

# Check for unexpected characters
missing_chars <- unique(all_chars[!all_chars %in% rownames(phon_dict)])
stopifnot(length(missing_chars) == 0)


# Reshape into 2869 rows x (7 * num_features) columns
lookup_indices <- match(all_chars, rownames(phon_dict))
expanded_features <- phon_dict[lookup_indices, , drop = FALSE]

# Reshape into per-word binary rows
num_features <- ncol(phon_dict)
phon_binary <- matrix(t(expanded_features), nrow = nrow(df), ncol = 7 * num_features, byrow = TRUE)

# Export to CSV
write.table(
  phon_binary,
  file = "phon.csv",
  sep = ",",
  row.names = FALSE,
  col.names = FALSE
)
