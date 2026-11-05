#Orthography
# Define the 26x26 orthogonal letter matrix ('a' through 'z')
letter_mat <- diag(26)
rownames(letter_mat) <- letters
colnames(letter_mat) <- letters

# Append the padding row '_'
pad_row <- matrix(0, nrow = 1, ncol = 26, dimnames = list("_", letters))
orth_dict <- rbind(letter_mat, pad_row)

# Load orthographic codes
orth_df <- read.csv("orth-code.csv", stringsAsFactors = FALSE)
orth_codes <- trimws(as.character(orth_df$code))
num_orth_slots <- nchar(orth_codes[1]) # 10 slots

# Lookup one-hot binary features per letter slot
all_orth_letters <- unlist(strsplit(orth_codes, ""))
orth_lookup <- orth_dict[match(all_orth_letters, rownames(orth_dict)), , drop = FALSE]

# Reshape into a 2D matrix: 2,601 words x 260 binary input units (10 slots * 26)
orth_binary <- matrix(
  t(orth_lookup),
  nrow = nrow(orth_df),
  ncol = num_orth_slots * 26,
  byrow = TRUE
)

# Export orthographic input matrix
write.table(
  orth_binary,
  file = "orth.csv",
  sep = ",",
  row.names = FALSE,
  col.names = FALSE
)


# Phonology
# Reps
phonreps <- read.csv("phonreps.csv", stringsAsFactors = FALSE)
rownames(phonreps) <- trimws(as.character(phonreps$phoneme))
phonreps$phoneme <- NULL
phon_mat <- as.matrix(phonreps)
mode(phon_mat) <- "numeric"

num_phon_features <- ncol(phon_mat) # 19 features

# Append the padding row '_' (a vector of 19 zeros)
phon_pad_row <- matrix(0, nrow = 1, ncol = num_phon_features, dimnames = list("_", colnames(phon_mat)))
phon_dict <- rbind(phon_mat, phon_pad_row)

# Load phonological codes
phon_df <- read.csv("phon-code.csv", stringsAsFactors = FALSE)
phon_codes <- trimws(as.character(phon_df$code))
num_phon_slots <- nchar(phon_codes[1]) # 7 slots

# Lookup distinctive feature values per phoneme slot
all_phon_chars <- unlist(strsplit(phon_codes, ""))
phon_lookup <- phon_dict[match(all_phon_chars, rownames(phon_dict)), , drop = FALSE]

# Reshape into a 2D matrix: 2,601 words x 133 binary output units (7 slots * 19)
phon_binary <- matrix(
  t(phon_lookup),
  nrow = nrow(phon_df),
  ncol = num_phon_slots * num_phon_features,
  byrow = TRUE
)

# Export phonological output matrix
write.table(
  phon_binary,
  file = "phon.csv",
  sep = ",",
  row.names = FALSE,
  col.names = FALSE
)


