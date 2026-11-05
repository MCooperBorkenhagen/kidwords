# ==============================================================================
# TEST SUITE: Verification of Dimensions, Word Order & Code Reconstruction
# ==============================================================================

cat("\n======================================================\n")
cat(" TESTS TESTS TESTS\n")
cat("======================================================\n\n")



# ------------------------------------------------------------------------------
# TEST 1: Word Order & Alignment
# ------------------------------------------------------------------------------
cat("[Test 1/5] Checking alignment and order of source word lists...\n")
stopifnot(
  "Row counts between orth-code.csv and phon-code.csv do not match" = 
    nrow(orth_df) == nrow(phon_df),
  "Word order between orth-code.csv and phon-code.csv is misaligned" = 
    all(orth_df$word == phon_df$word)
)
cat(" -> PASS: 2,601 words are identical and strictly aligned in order.\n\n")


# ------------------------------------------------------------------------------
# TEST 2: Matrix Dimensions
# ------------------------------------------------------------------------------
cat("[Test 2/5] Validating output matrix dimensions...\n")

# Expected dimensions
exp_rows         <- nrow(orth_df)                      # 2,601
exp_orth_cols    <- num_orth_slots * 26                # 10 * 26 = 260
exp_phon_cols    <- num_phon_slots * num_phon_features # 7 * 19 = 133

stopifnot(
  "orth_binary has incorrect row count"    = nrow(orth_binary) == exp_rows,
  "orth_binary has incorrect column count" = ncol(orth_binary) == exp_orth_cols,
  "phon_binary has incorrect row count"    = nrow(phon_binary) == exp_rows,
  "phon_binary has incorrect column count" = ncol(phon_binary) == exp_phon_cols
)
cat(sprintf(" -> PASS: orth_binary is %d x %d (expected %d x %d).\n", 
            nrow(orth_binary), ncol(orth_binary), exp_rows, exp_orth_cols))
cat(sprintf(" -> PASS: phon_binary is %d x %d (expected %d x %d).\n\n", 
            nrow(phon_binary), ncol(phon_binary), exp_rows, exp_phon_cols))


# ------------------------------------------------------------------------------
# TEST 3: Binary Value Range Validation
# ------------------------------------------------------------------------------
cat("[Test 3/5] Checking that matrices contain strictly binary (0/1) values...\n")
stopifnot(
  "orth_binary contains non-binary values" = all(orth_binary %in% c(0, 1)),
  "phon_binary contains non-binary values" = all(phon_binary %in% c(0, 1))
)
cat(" -> PASS: All cells are strictly binary (0 or 1) with no medial/NA values.\n\n")


# ------------------------------------------------------------------------------
# TEST 4: Inverse Reconstruction of Orthographic Codes
# ------------------------------------------------------------------------------
cat("[Test 4/5] Reconstructing orthographic codes from orth_binary...\n")

# Reshape all 10-slot letter blocks (2,601 * 10 = 26,010 slots)
orth_blocks <- matrix(t(orth_binary), ncol = 26, byrow = TRUE)

# 1-hot vector -> letter; all-zeros -> "_"
orth_reconstructed_chars <- apply(orth_blocks, 1, function(v) {
  idx <- which(v == 1)
  if (length(idx) == 0) return("_")
  letters[idx]
})

# Collapse back into 10-character word strings
orth_reconstructed_codes <- apply(
  matrix(orth_reconstructed_chars, nrow = exp_rows, byrow = TRUE), 
  1, 
  paste0, 
  collapse = ""
)

orth_mismatches <- which(orth_reconstructed_codes != orth_codes)

if (length(orth_mismatches) > 0) {
  stop(sprintf(
    "Orthographic reconstruction failed for %d words. First failure at row %d ('%s'): expected '%s', got '%s'",
    length(orth_mismatches), orth_mismatches[1], orth_df$word[orth_mismatches[1]],
    orth_codes[orth_mismatches[1]], orth_reconstructed_codes[orth_mismatches[1]]
  ))
}
cat(" -> PASS: 100% of rows in orth_binary reconstruct to the exact target codes in orth-code.csv.\n\n")


# ------------------------------------------------------------------------------
# TEST 5: Inverse Reconstruction of Phonological Codes
# ------------------------------------------------------------------------------
cat("[Test 5/5] Reconstructing phonological codes from phon_binary...\n")

# Build dictionary hash map: string representation of 19-bit vector -> symbol
dict_signatures <- apply(phon_dict, 1, paste0, collapse = "")
names(dict_signatures) <- rownames(phon_dict)

# Reshape all 7-slot phoneme blocks (2,601 * 7 = 18,207 slots)
phon_blocks <- matrix(t(phon_binary), ncol = num_phon_features, byrow = TRUE)
phon_block_signatures <- apply(phon_blocks, 1, paste0, collapse = "")

# Invert via exact signature lookup
phon_reconstructed_chars <- names(dict_signatures)[match(phon_block_signatures, dict_signatures)]

if (any(is.na(phon_reconstructed_chars))) {
  stop("Found unrecognized 19-bit phonological feature patterns during reconstruction.")
}

# Collapse back into 7-character word strings
phon_reconstructed_codes <- apply(
  matrix(phon_reconstructed_chars, nrow = exp_rows, byrow = TRUE), 
  1, 
  paste0, 
  collapse = ""
)

phon_mismatches <- which(phon_reconstructed_codes != phon_codes)

if (length(phon_mismatches) > 0) {
  stop(sprintf(
    "Phonological reconstruction failed for %d words! First failure at row %d ('%s'): expected '%s', got '%s'",
    length(phon_mismatches), phon_mismatches[1], phon_df$word[phon_mismatches[1]],
    phon_codes[phon_mismatches[1]], phon_reconstructed_codes[phon_mismatches[1]]
  ))
}
cat(" -> PASS: 100% of rows in phon_binary reconstruct to the exact target codes in phon-code.csv.\n\n")

cat("======================================================\n")
cat(" ALL TESTS PASSED: MATRICES ARE RIGHT ON\n")
cat("======================================================\n")

