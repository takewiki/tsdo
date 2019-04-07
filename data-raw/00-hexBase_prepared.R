hexBase_upper <- c(as.character(0:9),LETTERS[1:6]);
hexBase_lower <- c(as.character(0:9),letters[1:6]);
hexBase <- list(U=hexBase_upper,L=hexBase_lower);
devtools::use_data(hexBase,internal = T,overwrite = T);
