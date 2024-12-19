
##Clean matrix and imputation functions
cleanMatrixForClusterSE <- function(se, f_row = 0.5, f_col = 0.5) {
  mtx = assay(se)
  cat(sprintf("Filter rows with >%1.2f missingness and columns with >%1.2f missingness.\n",
              f_row, f_col))
  cat("Before: ", nrow(mtx), "rows and ", ncol(mtx),"columns.\n")
  namtx = is.na(mtx)
  good_row = rowSums(namtx) <= ncol(mtx) * f_row
  good_col = colSums(namtx) <= nrow(mtx) * f_col
  cat("After: ", sum(good_row), "rows and ", sum(good_col),"columns.\n")
  se[good_row, good_col]
}

imputeRowMean <- function(mtx) {
  k <- which(is.na(mtx), arr.ind=TRUE)
  mtx[k] <- rowMeans(mtx, na.rm=TRUE)[k[,1]]
  mtx
}

function(se, perplexity=30, seed=1) {
  library(Rtsne)
  set.seed(seed)
  se = cleanMatrixForClusterSE(se)
  mx = imputeRowMean(assay(se))
  ## samples = colnames(mx)
  tsne = Rtsne(t(mx), dims=2, perplexity=perplexity)
  df = as.data.frame(tsne$Y)
  colnames(df) = c("tSNE1", "tSNE2")
  df$sample = colnames(mx)
  cbind(df, as_tibble(colData(se))) #[samples,]))
}


splitString <- function (x, s, i) {
  l <- strsplit(x, s)
  vapply(l, function(x) x[[i]], character(1))
}
