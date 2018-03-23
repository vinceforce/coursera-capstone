# for (thing in ls()) { message(thing); print(object.size(get(thing)), units='auto') }
####################################################################################
library(ngram)
library(quanteda)
library(data.table)
library(knitr)
library(ggplot2)
library(grid)
library(methods)
library(gridBase)
library(stringr)
library(stringi)
library(plyr)
library(readtext)
library(parallel)
library(doParallel)
library(stats)
library(plotly)
library(knitr)

parallelizeTask <- function(task, ...) {
  ncores <- detectCores() - 1
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  r <- task(...)
  stopCluster(cl)
  r
}

###################################################
### LOAD DATA +++++++++++++++++++++++++++++++++++++
###################################################

fnDataBlog <- "en_US/en_US.blogs.txt"
fnDataNews <- "en_US/en_US.news.txt"
fnDataTwitter <- "en_US/en_US.twitter.txt"

load.blog.time <- system.time(ds.blog <- readLines(fnDataBlog, skipNul = TRUE))[3]
load.news.time <- system.time(ds.news <- readLines(fnDataNews, skipNul = TRUE))[3]
load.twitter.time <- system.time(ds.twitter <- readLines(fnDataTwitter, skipNul = TRUE))[3]

###################################################
### TRAIN / TESTDEV / TEST ++++++++++++++++++++++++
###################################################

propSampleData = 0.99

set.seed(145627)

sampleTrain <- function(v, prop) {
  nbRec <- length(v)
  nbsamples <- floor(nbRec * prop)
  sliceSize <- floor(nbRec / nbsamples)
  rnd <- runif(n = nbsamples)
  inTrain <- unique(sapply(1:nbsamples, function(i) {
    return(1 + (i-1) * sliceSize + floor(sliceSize * rnd[i]))
  }))
  return(list(training = v[inTrain], testing = v[-inTrain]))
}

sample.blog.time <- system.time(sample.blog <- sampleTrain(v = ds.blog, prop = propSampleData))[3]
sample.blog.training <- sample.blog[["training"]]
sample.blog.testing <- sample.blog[["testing"]]
rm(ds.blog); rm(sample.blog); gc()

sample.news.time <- system.time(sample.news <- sampleTrain(v = ds.news, prop = propSampleData))[3]
sample.news.training <- sample.news[["training"]]
sample.news.testing <- sample.news[["testing"]]
rm(ds.news); rm(sample.news); gc()

sample.twitter.time <- system.time(sample.twitter <- sampleTrain(v = ds.twitter, prop = propSampleData))[3]
sample.twitter.training <- sample.twitter[["training"]]
sample.twitter.testing <- sample.twitter[["testing"]]
rm(ds.twitter); rm(sample.twitter); gc()

training.full <- c(sample.blog.training, c(sample.news.training, c(sample.twitter.training)))
testing.full <- c(sample.blog.testing, c(sample.news.testing, c(sample.twitter.testing)))
rm(sample.blog.training); rm(sample.news.training); rm(sample.twitter.training)
rm(sample.blog.testing); rm(sample.news.testing); rm(sample.twitter.testing)
gc()

sample.testing <- sampleTrain(v = testing.full, prop = 0.001)
testing.dev.full <- sample.testing[["training"]]
testing.full <- sample.testing[["testing"]]
rm(sample.testing); gc()

###################################################
### SAMPLE ++++++++++++++++++++++++++++++++++++++++
###################################################

sampleDS <- function(v, nbsamples=NULL, prop) {
  nbRec <- length(v)
  if (is.null(nbsamples)) nbsamples = floor(prop * nbRec)
  sliceSize <- floor(nbRec / nbsamples)
  rnd <- runif(n = nbsamples)
  inTrain <- unique(sapply(1:nbsamples, function(i) {
    return(1 + (i-1) * sliceSize + floor(sliceSize * rnd[i]))
  }))
  return(v[inTrain])
}

train.sample <- sampleDS(training.full, prop = .001)
test.sample <- sampleDS(testing.full, prop = .05)

# train.sample <- training.full
# test.sample <-testing.full

###################################################
### SAMPLE MODEL ++++++++++++++++++++++++++++++++++
###################################################
NbGram <- 4


vocabularyProcess <- function(train.sample, threshold=1) {
  corpus.training.sample <- quanteda::corpus(train.sample)
  delim = ' .,;:"()?!'
  exclude_pattern <- paste0("[^a-zA-Z'’", delim, "-]")
  corpus.training.sample <- corpus_trim(corpus.training.sample, what = "sentences", exclude_pattern = exclude_pattern)
  corpus.sentences <- quanteda::corpus_reshape(corpus.training.sample, 'sentences', remove_punct = FALSE, remove_separators = TRUE)
  ngrams.1 <- quanteda::tokens(corpus.sentences, what = "word", remove_numbers = TRUE, remove_punct = TRUE,
                               remove_symbols = TRUE, remove_separators = TRUE,
                               remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE,
                               ngrams = 1L, skip = 0L, concatenator = " ")
  dfm.1 <- quanteda::dfm(ngrams.1, tolower = FALSE)
  frequencies.1 <- colSums(dfm.1)
  frequencies.1.dt <- data.table(feature = names(frequencies.1), frequency = frequencies.1)
  frequencies.1.dt <- frequencies.1.dt[frequency > threshold]
  frequencies.1.dt[, frequency:=NULL]
  return(frequencies.1.dt)
}

# vocabulary.1 <- vocabularyProcess(train.sample, 1)
vocabulary.1 <- parallelizeTask(vocabularyProcess, train.sample, 1)
write.csv2(vocabulary.1, paste0("ngrams/vocabulary.1.csv"), row.names = FALSE)
vocabulary.1 <- data.table::fread("ngrams/vocabulary.1.csv")

ngrams <- function(train.sample, what = "word", nbGram = 4, stem=FALSE, weight=NULL, smooth=NULL, skip=0, min_docfreq = 1, vocabulary, exclude = FALSE) {
  corpus.training.sample <- quanteda::corpus(train.sample)
  delim = ' .,;:"()?!'
  if (exclude) {
    exclude_pattern <- paste0("[^a-zA-Z'’", delim, "-]")
    corpus.training.sample <- corpus_trim(corpus.training.sample, what = "sentences", exclude_pattern = exclude_pattern)
  }
  corpus.sentences <- quanteda::corpus_reshape(corpus.training.sample, 'sentences', remove_punct = FALSE, remove_separators = TRUE)
  texts(corpus.sentences) <- stringi::stri_replace_all_regex(tolower(texts(corpus.sentences)), c("^[ ]*", "[.;!? ]*$"), c("BEGIN ", " END"), vectorize_all = FALSE)
  texts(corpus.sentences) <- stringi::stri_replace_all_regex(texts(corpus.sentences), c(" END END"), c(" END"), vectorize_all = FALSE)
  ngrams.1 <- quanteda::tokens(corpus.sentences, what = "word", remove_numbers = TRUE, remove_punct = TRUE,
                               remove_symbols = TRUE, remove_separators = TRUE,
                               remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE,
                               ngrams = 1L, skip = 0L, concatenator = " ")
  dfm.1 <- quanteda::dfm(ngrams.1, tolower = FALSE)
  frequencies.1 <- colSums(dfm.1)
  terms <- names(frequencies.1)
  terms_not_in_voc <- sapply(setdiff(terms, vocabulary$feature), function(t) paste0(" ", t, " "))
  unk_terms <- rep(" UNK ", length(terms_not_in_voc))
  texts(corpus.sentences) <- stringi::stri_replace_all_regex(texts(corpus.sentences), terms_not_in_voc, unk_terms, vectorize_all = FALSE)
  
  ngrams.train.sample <- quanteda::tokens(corpus.sentences, what = "word", remove_numbers = TRUE, remove_punct = TRUE,
                                          remove_symbols = TRUE, remove_separators = TRUE,
                                          remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE,
                                          ngrams = 1:4, skip = 0L, concatenator = " ")
  
  dfm.train.sample <- quanteda::dfm(ngrams.train.sample, tolower = FALSE)
  if (min_docfreq > 1) dfm.train.sample <- quanteda::dfm_trim(dfm.train.sample, min_docfreq = min_docfreq, verbose = TRUE)
  if (!is.null(smooth)) {
    dfm.train.sample <- quanteda::dfm_smooth(dfm.train.sample, smoothing = smooth)
  }
  if (!is.null(weight)) {
    dfm.train.sample <- quanteda::dfm_weight(dfm.train.sample, scheme = weight, weights = NULL, base = 10, K = 0.5)
  }
  frequencies.train.sample <- colSums(dfm.train.sample)
  freq.train.dt.sample <- data.table(feature = names(frequencies.train.sample), frequency = frequencies.train.sample)
  freq.train.dt.sample$wc <- sapply(freq.train.dt.sample$feature, ngram::wordcount, sep = " ")

  ng.4.train.sample <- freq.train.dt.sample[wc==4]
  ng.3.train.sample <- freq.train.dt.sample[wc==3]
  ng.2.train.sample <- freq.train.dt.sample[wc==2]
  ng.1.train.sample <- freq.train.dt.sample[wc==1]
  
  return(list(ng.1 = ng.1.train.sample, ng.2 = ng.2.train.sample, ng.3 = ng.3.train.sample, ng.4 = ng.4.train.sample))
}

docstofiles <- function(docs, ii) {
  # ng <- ngrams(docs, what = "word", nbGram = 4, stem=TRUE, weight=NULL, smooth=NULL, skip=0, min_docfreq = 1, vocabulary = vocabulary.1)
  ng <- parallelizeTask(ngrams, docs, what = "word", nbGram = 4, stem=TRUE, weight=NULL, smooth=NULL, skip=0,
                        min_docfreq = 1, vocabulary = vocabulary.1, exclude = TRUE)
  ng.1.train <- ng[["ng.1"]]
  write.csv2(ng.1.train, paste0("ngrams/1/ng.1.train_", ii, ".csv"), row.names = FALSE)
  ng.2.train <- ng[["ng.2"]]
  write.csv2(ng.2.train, paste0("ngrams/2/ng.2.train_", ii, ".csv"), row.names = FALSE)
  ng.3.train <- ng[["ng.3"]]
  write.csv2(ng.3.train, paste0("ngrams/3/ng.3.train_", ii, ".csv"), row.names = FALSE)
  ng.4.train <- ng[["ng.4"]]
  write.csv2(ng.4.train, paste0("ngrams/4/ng.4.train_", ii, ".csv"), row.names = FALSE)
}
chunk_size <- 1000
nb_chunks <- floor(length(train.sample) / chunk_size)
print(paste0("nb_chunks : ", nb_chunks))

filesngrams <- function(nb_chunks) {
  for (ic in 1:(nb_chunks-1)) {
  print(paste0("CALL docstofiles(train.sample[((ic-1)*chunk_size+1):(ic*chunk_size)], ic) - ic = ", ic))
  docstofiles(train.sample[((ic-1)*chunk_size+1):(ic*chunk_size)], ic)
}
  docstofiles(train.sample[(nb_chunks-1)*chunk_size+1:length(train.sample)], nb_chunks)
}

parallelizeTask(filesngrams, nb_chunks)

#+++++++++++++++++++++++++++++++++++++++++++++
# Combination of frequencies from the files
#+++++++++++++++++++++++++++++++++++++++++++++
# ID, feature, frequency, wc
combineNgramFiles <- function() {
  for (ng_i in 1:4) {
    fold_i <- paste0("ngrams/", ng_i, "/")
    fL <- list.files(path = fold_i)
    freq.train.dt <- data.table::fread(paste0(fold_i, fL[1]))
    for (f in fL[-1]) {
      print(paste("Running for file", f))
      freq.train.dt.tmp <- data.table::fread(paste0(fold_i, f))
      freq.train.dt.bind <- rbind(freq.train.dt, freq.train.dt.tmp)
      freq.train.dt <- freq.train.dt.bind[, sum(frequency), by=feature]
      data.table::setnames(freq.train.dt, old = "V1", new = "frequency")
      freq.train.dt[freq.train.dt.bind, on="feature", wc:=i.wc]
    }
    write.csv2(freq.train.dt, paste0(fold_i, "ng1-4_ng.", ng_i, ".train.csv"), row.names = FALSE)
    saveRDS(freq.train.dt, paste0(fold_i, "ng1-4_ng.", ng_i, ".train.rds"))
    freq.train.dt.prune.1 <- freq.train.dt[frequency > 1]
    write.csv2(freq.train.dt.prune.1, paste0(fold_i, "ng1-4.prune.1_ng.", ng_i, ".train.csv"), row.names = FALSE)
    saveRDS(freq.train.dt.prune.1, paste0(fold_i, "ng1-4.prune.1_ng.", ng_i, ".train.rds"))
    freq.train.dt.prune.2 <- freq.train.dt.prune.1[frequency > 2]
    write.csv2(freq.train.dt.prune.2, paste0(fold_i, "ng1-4.prune.2_ng.", ng_i, ".train.csv"), row.names = FALSE)
    saveRDS(freq.train.dt.prune.2, paste0(fold_i, "ng1-4.prune.2_ng.", ng_i, ".train.rds"))
  }
}

combineNgramFiles()

##########################################################"
##########################################################"
# bak_red
# ng.4.train <- readRDS("ngrams/bak_red/ng1-4_ng.4.train.rds")
# ng.3.train <- readRDS("ngrams/bak_red/ng1-4_ng.3.train.rds")
# ng.2.train <- readRDS("ngrams/bak_red/ng1-4_ng.2.train.rds")
# ng.1.train <- readRDS("ngrams/bak_red/ng1-4_ng.1.train.rds")

ng.4.train <- data.table::fread("ngrams/4/ng1-4_ng.4.train.csv")
ng.3.train <- data.table::fread("ngrams/3/ng1-4_ng.3.train.csv")
ng.2.train <- data.table::fread("ngrams/2/ng1-4_ng.2.train.csv")
ng.1.train <- data.table::fread("ngrams/1/ng1-4_ng.1.train.csv")

vocabulary.1 <- data.table::fread("ngrams/vocabulary.1.csv")

# discountfactor.gts <- function(c, N.e, N, k=20) {
#   if (c > k) return((c + 1) * (N.e(c+1)/(c*N.e(c))))
#   else return((c + 1) * (N[c+1]/(c*N[c])))
# }

computeProbs <- function(ng.1.train, ng.2.train, ng.3.train, ng.4.train, vocabulary.1, writetodisk = TRUE) {

  K = 5
  #####################
  # 1-grams
  #####################
  print("# 1-grams")
  N <- table(ng.1.train$frequency)
  
  # Maximum Likelihood
  # N.mle <- sum(as.integer(names(N))*N)
  N.mle <- sum(ng.1.train$frequency)
  ng.1.train[, p.mle := frequency / N.mle]
  
  ## Good-Turing Smoothing
  X <- log(as.integer(names(N)))
  Y <- log(N)
  mod <- lm(Y~X)
  a0.1 <- summary(mod)$coefficients[1]
  a1.1 <- summary(mod)$coefficients[2]
  N.1 <- function(f) {
    return (exp(a1.1*log(f) + a0.1))
  }
  ng.1.train$gts.cnt <- sapply(ng.1.train$frequency, function(f) {
    if (f <= 5) return ((f + 1) * (N[as.character(f + 1)] / N[as.character(f)]))
    else return (f)
    # else return ((f + 1) * (N.1(f + 1) / N.1(f)))
  })
  ng.1.train[, d.gts := gts.cnt / frequency]
  # ng.1.train$d.gts <- sapply(ng.1.train$frequency, function(c) discountfactor.gts(c, N.1, N, k = K))
  N.gts <- sum(ng.1.train$gts.cnt)
  # N.gts <- sum(as.integer(names(N)) * N)
  ng.1.train[, p.gts := gts.cnt / N.gts]
  
  # NK <- (K + 1) * N.1(K + 1) / N.1(1)
  NK <- (K + 1) * N[K + 1] / N[1]
  ng.1.train[, d.ktz := ((d.gts - NK) / (1 - NK))]
  
  ng.1.train[, ktz.cnt := d.ktz * frequency]
  N.ktz <- sum(ng.1.train$ktz.cnt)
  # ng.1.train[, p.ktz := ktz.cnt / N.ktz]
  ng.1.train[, p.ktz := d.ktz * p.mle]
  
  
  #####################
  # 2-grams
  #####################
  print("# 2-grams")
  N <- table(ng.2.train$frequency)
  
  # Maximum Likelihood
  # N.mle <- sum(as.integer(names(N)))
  # ng.2.train[, p.mle := frequency / N.mle]
  
  ## Good-Turing Smoothing
  X <- log(as.integer(names(N)))
  Y <- log(N)
  mod <- lm(Y~X)
  a0.2 <- summary(mod)$coefficients[1]
  a1.2 <- summary(mod)$coefficients[2]
  N.2 <- function(f) {
    return (exp(a1.2*log(f) + a0.2))
  }
  ng.2.train$gts.cnt <- sapply(ng.2.train$frequency, function(f) {
    if (f <= 5) return ((f + 1) * (N[f + 1] / N[f]))
    else return (f)
    # else return ((f + 1) * (N.2(f + 1) / N.2(f)))
  })
  ng.2.train[, d.gts := gts.cnt / frequency]
  # ng.2.train$d.gts <- sapply(ng.2.train$frequency, function(c) discountfactor.gts(c, N.2, N, k = K))
  N.gts <- sum(ng.2.train$gts.cnt)
  # N.gts <- sum(as.integer(names(N)) * N)
  ng.2.train[, p.gts := gts.cnt / N.gts]
  
  # NK <- (K + 1) * N.2(K + 1) / N.2(1)
  NK <- (K + 1) * N[K + 1] / N[1]
  ng.2.train[, d.ktz := ((d.gts - NK) / (1 - NK))]
  
  ng.2.train[, ktz.cnt := d.ktz * frequency]
  # N.ktz <- sum(ng.2.train$ktz.cnt)
  # ng.2.train[, p.ktz := ktz.cnt / N.ktz]
  
  #####################
  # 3-grams
  #####################
  print("# 3-grams")
  N <- table(ng.3.train$frequency)
  N.cnt <- length(N)
  
  # Maximum Likelihood
  # N.mle <- sum(as.integer(names(N)))
  # ng.3.train[, p.mle := frequency / N.mle]
  
  ## Good-Turing Smoothing
  X <- log(as.integer(names(N[(K+1):N.cnt])))
  Y <- log(N[(K+1):N.cnt])
  mod <- lm(Y~X)
  a0.3 <- summary(mod)$coefficients[1]
  a1.3 <- summary(mod)$coefficients[2]
  N.3 <- function(f) {
    return (exp(a1.3*log(f) + a0.3))
  }
  ng.3.train$gts.cnt <- sapply(ng.3.train$frequency, function(f) {
    if (f <= 5) return ((f + 1) * (N[f + 1] / N[f]))
    else return (f)
    # else return ((f + 1) * (N.3(f + 1) / N.3(f)))
  })
  ng.3.train[, d.gts := gts.cnt / frequency]
  # ng.3.train$d.gts <- sapply(ng.3.train$frequency, function(c) discountfactor.gts(c, N.3, N, k = K))
  N.gts <- sum(ng.3.train$gts.cnt)
  # N.gts <- sum(as.integer(names(N)) * N)
  ng.3.train[, p.gts := gts.cnt / N.gts]
  
  # NK <- (K + 1) * N.3(K + 1) / N.3(1)
  NK <- (K + 1) * N[K + 1] / N[1]
  ng.3.train[, d.ktz := ((d.gts - NK) / (1 - NK))]
  
  ng.3.train[, ktz.cnt := d.ktz * frequency]
  # N.ktz <- sum(ng.3.train$ktz.cnt)
  # ng.3.train[, p.ktz := ktz.cnt / N.ktz]
  
  #####################
  # 4-grams
  #####################
  print("# 4-grams")
  N <- table(ng.4.train$frequency)
  
  # Maximum Likelihood
  # N.mle <- sum(as.integer(names(N)))
  # ng.4.train[, p.mle := frequency / N.mle]
  
  ## Good-Turing Smoothing
  X <- log(as.integer(names(N)))
  Y <- log(N)
  mod <- lm(Y~X)
  a0.4 <- summary(mod)$coefficients[1]
  a1.4 <- summary(mod)$coefficients[2]
  N.4 <- function(f) {
    return (exp(a1.4*log(f) + a0.4))
  }
  ng.4.train$gts.cnt <- sapply(ng.4.train$frequency, function(f) {
    if (f <= 5) return ((f + 1) * (N[f + 1] / N[f]))
    else return (f)
    # else return ((f + 1) * (N.4(f + 1) / N.4(f)))
  })
  ng.4.train[, d.gts := gts.cnt / frequency]
  # ng.4.train$d.gts <- sapply(ng.4.train$frequency, function(c) discountfactor.gts(c, N.4, N, k = K))
  N.gts <- sum(ng.4.train$gts.cnt)
  # N.gts <- sum(as.integer(names(N)) * N)
  ng.4.train[, p.gts := gts.cnt / N.gts]
  
  # NK <- (K + 1) * N.4(K + 1) / N.4(1)
  NK <- (K + 1) * N[K + 1] / N[1]
  ng.4.train[, d.ktz := ((d.gts - NK) / (1 - NK))]
  
  ng.4.train[, ktz.cnt := d.ktz * frequency]
  # N.ktz <- sum(ng.4.train$ktz.cnt)
  # ng.4.train[, p.ktz := ktz.cnt / N.ktz]
  
  if (writetodisk) {
    data.table::fwrite(ng.1.train, file = paste0("ngrams/1/ng1-4_ng.1.train.prob.csv"), row.names = FALSE)
    data.table::fwrite(ng.2.train, file = paste0("ngrams/2/ng1-4_ng.2.train.prob.csv"), row.names = FALSE)
    data.table::fwrite(ng.3.train, file = paste0("ngrams/3/ng1-4_ng.3.train.prob.csv"), row.names = FALSE)
    data.table::fwrite(ng.4.train, file = paste0("ngrams/4/ng1-4_ng.4.train.prob.csv"), row.names = FALSE)
  }
  
  return(list(ng.1 = ng.1.train, ng.2 = ng.2.train, ng.3 = ng.3.train, ng.4 = ng.4.train))

}

ng.train <- computeProbs(ng.1.train, ng.2.train, ng.3.train, ng.4.train, vocabulary.1)

ng.4.train <- ng.train[["ng.4"]]
ng.3.train <- ng.train[["ng.3"]]
ng.2.train <- ng.train[["ng.2"]]
ng.1.train <- ng.train[["ng.1"]]


ng.4.train <- data.table::fread("ngrams/4/ng1-4_ng.4.train.prob.csv")
ng.3.train <- data.table::fread("ngrams/3/ng1-4_ng.3.train.prob.csv")
ng.2.train <- data.table::fread("ngrams/2/ng1-4_ng.2.train.prob.csv")
ng.1.train <- data.table::fread("ngrams/1/ng1-4_ng.1.train.prob.csv")

vocabulary.1 <- data.table::fread("ngrams/vocabulary.1.csv")


pairedngrams <- function(ngram, nbGram, stem = FALSE, pngram1 = NULL, k = 5) {
  if (nbGram == 1) {
    ng.paired <- data.table(grams.1 = trimws(ngram$feature), grams.0 = trimws(ngram$feature), freq = ngram$frequency, gts.cnt = ngram$gts.cnt,
                            d.gts = ngram$d.gts, p.mle = ngram$p.mle, p.gts = ngram$p.gts,
                            d.ktz = ngram$d.ktz)
    ng.paired[, p.ktz:=d.ktz*p.mle]
  }
  else {
    split.ngram <- strsplit(as.character(trimws(ngram$feature)), " ", fixed = TRUE)
    if (stem) grams.n.stem <- sapply(split.ngram, function(s) {if (length(s) == nbGram) Reduce("paste", quanteda::char_wordstem(s[1:nbGram-1]))})
    grams.n <- sapply(split.ngram, function(s) {if (length(s) == nbGram) Reduce("paste", s[1:(nbGram-1)])})
    if (nbGram > 2) grams.n.1 <- sapply(split.ngram, function(s) {if (length(s) == nbGram) Reduce("paste", s[2:(nbGram-1)])})
    grams.0 <- sapply(split.ngram, function(s) {if (length(s) == nbGram) s[nbGram]})
    # if (nbGram > 2) ng.paired <- data.table(grams = ngram$feature, grams.n = grams.n, grams.n.1 = grams.n.1, grams.n.stem = grams.n.stem, grams.0 = grams.0, freq = ngram$frequency,
    #                         d.gts = ngram$d.gts, d.ktz = ngram$d.ktz, p.ktz = ngram$p.ktz)
    # else ng.paired <- data.table(grams = ngram$feature, grams.n = grams.n, grams.n.stem = grams.n.stem, grams.0 = grams.0, freq = ngram$frequency,
    #                              d.gts = ngram$d.gts, d.ktz = ngram$d.ktz, p.ktz = ngram$p.ktz)
    if (nbGram > 2) ng.paired <- data.table(grams = ngram$feature, grams.n = grams.n, grams.n.1 = grams.n.1, grams.n.stem = grams.n.stem, grams.0 = grams.0, freq = ngram$frequency,
                                            d.gts = ngram$d.gts, d.ktz = ngram$d.ktz)
    else ng.paired <- data.table(grams = ngram$feature, grams.n = grams.n, grams.n.stem = grams.n.stem, grams.0 = grams.0, freq = ngram$frequency,
                                 d.gts = ngram$d.gts, d.ktz = ngram$d.ktz)
    
    data.table::setnames(pngram1, old=paste0("grams.", nbGram - 1), new="grams.n")
    if (nbGram > 2) data.table::setnames(pngram1, old=paste0("grams.", nbGram - 2), new="grams.n.1")

    ng.paired[pngram1, on=.(grams.n), freq.n:=i.freq]
    ng.paired[, p.mle:=(freq / freq.n)]
    # MLE NORMALISATION
    N.mle <- sum(ng.paired$p.mle)
    ng.paired[, p.mle:=(p.mle / N.mle)]
    ng.paired[, p.gts:=d.gts * p.mle]
    ng.paired[, p.ktz:=d.ktz * p.mle]
    
    if (nbGram > 2) ng.paired[pngram1, on=.(grams.0, grams.n.1), p.ktz.n:=i.p.ktz]
    else ng.paired[pngram1, on=.(grams.0), p.ktz.n:=i.p.mle]
    ng.paired.p.ktz.n.sum <- ng.paired[, sum(p.ktz.n), by=grams.n]
    data.table::setnames(ng.paired.p.ktz.n.sum, old="V1", new="p.ktz.n.sum")
    ng.paired[ng.paired.p.ktz.n.sum, on=.(grams.n), sigma.k.n:=i.p.ktz.n.sum]
      
    ng.paired.p.ktz.sum <- ng.paired[, sum(p.ktz), by=grams.n]
    data.table::setnames(ng.paired.p.ktz.sum, old="V1", new="p.ktz.sum")
    ng.paired[ng.paired.p.ktz.sum, on=.(grams.n), beta.k.n:=i.p.ktz.sum]
    ng.paired[, alpha:=(1-beta.k.n)/(1-sigma.k.n)]
    
    data.table::setnames(pngram1, old="grams.n", new=paste0("grams.", nbGram - 1))
    if (nbGram > 2) data.table::setnames(pngram1, old="grams.n.1", new=paste0("grams.", nbGram - 2))
    data.table::setnames(ng.paired, old="grams", new=paste0("grams.", nbGram))
    data.table::setnames(ng.paired, old = "grams.n", new = paste0("grams.", nbGram - 1))
    if (nbGram > 2) data.table::setnames(ng.paired, old="grams.n.1", new=paste0("grams.", nbGram - 2))
    data.table::setnames(ng.paired, old = "grams.n.stem", new = paste0("grams.", nbGram - 1, ".stem"))
    data.table::setnames(ng.paired, old = "freq.n", new = paste0("freq.", nbGram - 1))
  }
  return(ng.paired[order(p.gts, decreasing = TRUE)])
}

png.1.train <- pairedngrams(ngram = ng.1.train, nbGram = 1, stem = TRUE, k = 5)
data.table::fwrite(png.1.train, file = paste0("ngrams/1/ng1-4_ng.1.train.prob.png.csv"), row.names = FALSE)

png.1.train <- data.table::fread("ngrams/1/ng1-4_ng.1.train.prob.png.csv")
ng.2.train <- data.table::fread("ngrams/2/ng1-4_ng.2.train.prob.csv")
png.2.train <- pairedngrams(ngram = ng.2.train, nbGram = 2, stem = TRUE, pngram1 = png.1.train, k = 5)
data.table::fwrite(png.2.train, file = paste0("ngrams/2/ng1-4_ng.2.train.prob.png.csv"), row.names = FALSE)

png.2.train <- data.table::fread("ngrams/2/ng1-4_ng.2.train.prob.png.csv")
ng.3.train <- data.table::fread("ngrams/3/ng1-4_ng.3.train.prob.csv")
png.3.train <- pairedngrams(ngram = ng.3.train, nbGram = 3, stem = TRUE, pngram1 = png.2.train, k = 5)
data.table::fwrite(png.3.train, file = paste0("ngrams/3/ng1-4_ng.3.train.prob.png.csv"), row.names = FALSE)

png.3.train <- data.table::fread("ngrams/3/ng1-4_ng.3.train.prob.png.csv")
ng.4.train <- data.table::fread("ngrams/4/ng1-4_ng.4.train.prob.csv")
png.4.train <- pairedngrams(ngram = ng.4.train, nbGram = 4, stem = TRUE, pngram1 = png.3.train, k = 5)
data.table::fwrite(png.4.train, file = paste0("ngrams/4/ng1-4_ng.4.train.prob.png.csv"), row.names = FALSE)


png.4.train <- data.table::fread("ngrams/4/ng1-4_ng.4.train.prob.png.csv")
png.3.train <- data.table::fread("ngrams/3/ng1-4_ng.3.train.prob.png.csv")
png.2.train <- data.table::fread("ngrams/2/ng1-4_ng.2.train.prob.png.csv")
png.1.train <- data.table::fread("ngrams/1/ng1-4_ng.1.train.prob.png.csv")

# VERSION 2 : resultats trop proches !!
png.1.train[png.2.train, on=.(grams.0), by=.EACHI, alpha.n:=i.alpha]
png.2.train[png.3.train, on=.(grams.1), by=.EACHI, alpha.n:=i.alpha]
png.3.train[png.4.train, on=.(grams.2), by=.EACHI, alpha.n:=i.alpha]

# VERSION 1
# png.1.train[png.2.train, on=.(grams.1), by=.EACHI, alpha.n:=i.alpha]
# png.2.train[png.3.train, on=.(grams.2), by=.EACHI, alpha.n:=i.alpha]
# png.3.train[png.4.train, on=.(grams.3), by=.EACHI, alpha.n:=i.alpha]


png.1.train[is.na(alpha.n)]
png.2.train[is.na(alpha.n)]
png.3.train[is.na(alpha.n)]
summary(png.1.train$alpha.n)
summary(png.2.train$alpha.n)
summary(png.3.train$alpha.n)
summary(png.4.train$alpha.n)

# Profanity words deleted from potential predictons
profanity.words <- data.table(read.csv("dicts/Terms-to-Block.csv", header = FALSE))

png.1.train <- png.1.train[!is.element(grams.1, profanity.words$V1)]
png.2.train <- png.2.train[!is.element(grams.0, profanity.words$V1)]
png.3.train <- png.3.train[!is.element(grams.0, profanity.words$V1)]
png.4.train <- png.4.train[!is.element(grams.0, profanity.words$V1)]

png.1.train <- png.1.train[grams.1 != "BEGIN"]
# unknowns deletion
png.1.train <- png.1.train[grams.1 != "UNK"]
png.2.train <- png.2.train[grams.0 != "UNK"]
png.3.train <- png.3.train[grams.0 != "UNK"]
png.4.train <- png.4.train[grams.0 != "UNK"]

# png.1.train <- png.1.train[grams.1 != "BEGIN" & grams.1 != "END"]
# png.2.train <- png.2.train[grams.0 != "BEGIN" & grams.0 != "END"]
# png.3.train <- png.3.train[grams.0 != "BEGIN" & grams.0 != "END"]
# png.4.train <- png.4.train[grams.0 != "BEGIN" & grams.0 != "END"]

png.1.train[, gts.cnt:=NULL]; png.1.train[, d.gts:=NULL]; png.1.train[, d.ktz:=NULL]; png.1.train[, p.gts:=NULL]
png.1.train[, grams.1:=NULL]

png.2.train[, d.gts:=NULL]; png.2.train[, d.ktz:=NULL]; png.2.train[, p.gts:=NULL]
png.2.train[, sigma.k.n:=NULL]; png.2.train[, beta.k.n:=NULL]; png.2.train[, alpha:=NULL]
png.2.train[, freq.1:=NULL];
png.2.train[, p.ktz.n:=NULL]

png.3.train[, d.gts:=NULL]; png.3.train[, d.ktz:=NULL]; png.3.train[, p.gts:=NULL]
png.3.train[, sigma.k.n:=NULL]; png.3.train[, beta.k.n:=NULL]; png.3.train[, alpha:=NULL]
png.3.train[, freq.2:=NULL];
png.3.train[, p.ktz.n:=NULL]; png.3.train[, grams.1:=NULL]

png.4.train[, d.gts:=NULL]; png.4.train[, d.ktz:=NULL]; png.4.train[, p.gts:=NULL]
png.4.train[, sigma.k.n:=NULL]; png.4.train[, beta.k.n:=NULL]; png.4.train[, alpha:=NULL]
png.4.train[, freq.3:=NULL];
png.4.train[, p.ktz.n:=NULL]; png.4.train[, grams.2:=NULL]

gc()

write.csv2(png.1.train, "pairedngrams/ng1-4_png.1.train.csv", row.names = FALSE)
saveRDS(png.1.train, "pairedngrams/ng1-4_png.1.train.rds")
write.csv2(png.2.train, "pairedngrams/ng1-4_png.2.train.csv", row.names = FALSE)
saveRDS(png.2.train, "pairedngrams/ng1-4_png.2.train.rds")
write.csv2(png.3.train, "pairedngrams/ng1-4_png.3.train.csv", row.names = FALSE)
saveRDS(png.3.train, "pairedngrams/ng1-4_png.3.train.rds")
write.csv2(png.4.train, "pairedngrams/ng1-4_png.4.train.csv", row.names = FALSE)
saveRDS(png.4.train, "pairedngrams/ng1-4_png.4.train.rds")

png.1.train.prune.1 <- png.1.train[freq>1]
png.2.train.prune.1 <- png.2.train[freq>1]
png.3.train.prune.1 <- png.3.train[freq>1]
png.4.train.prune.1 <- png.4.train[freq>1]

png.1.train.prune.2 <- png.1.train.prune.1[freq>2]
png.2.train.prune.2 <- png.2.train.prune.1[freq>2]
png.3.train.prune.2 <- png.3.train.prune.1[freq>2]
png.4.train.prune.2 <- png.4.train.prune.1[freq>2]
png.1.train.prune.2[, freq:=NULL]
png.2.train.prune.2[, freq:=NULL]
png.3.train.prune.2[, freq:=NULL]
png.4.train.prune.2[, freq:=NULL]
write.csv2(png.1.train.prune.2, "pairedngrams/ng1-4.prune.2_png.1.train.csv", row.names = FALSE)
saveRDS(png.1.train.prune.2, "pairedngrams/ng1-4.prune.2_png.1.train.rds")
write.csv2(png.2.train.prune.2, "pairedngrams/ng1-4.prune.2_png.2.train.csv", row.names = FALSE)
saveRDS(png.2.train.prune.2, "pairedngrams/ng1-4.prune.2_png.2.train.rds")
write.csv2(png.3.train.prune.2, "pairedngrams/ng1-4.prune.2_png.3.train.csv", row.names = FALSE)
saveRDS(png.3.train.prune.2, "pairedngrams/ng1-4.prune.2_png.3.train.rds")
write.csv2(png.4.train.prune.2, "pairedngrams/ng1-4.prune.2_png.4.train.csv", row.names = FALSE)
saveRDS(png.4.train.prune.2, "pairedngrams/ng1-4.prune.2_png.4.train.rds")
sum(object.size(c(png.1.train.prune.2, png.2.train.prune.2, png.3.train.prune.2, png.4.train.prune.2)))

png.1.train.prune.1[, freq:=NULL]
png.2.train.prune.1[, freq:=NULL]
png.3.train.prune.1[, freq:=NULL]
png.4.train.prune.1[, freq:=NULL]
write.csv2(png.1.train.prune.1, "pairedngrams/ng1-4.prune.1_png.1.train.csv", row.names = FALSE)
saveRDS(png.1.train.prune.1, "pairedngrams/ng1-4.prune.1_png.1.train.rds")
write.csv2(png.2.train.prune.1, "pairedngrams/ng1-4.prune.1_png.2.train.csv", row.names = FALSE)
saveRDS(png.2.train.prune.1, "pairedngrams/ng1-4.prune.1_png.2.train.rds")
write.csv2(png.3.train.prune.1, "pairedngrams/ng1-4.prune.1_png.3.train.csv", row.names = FALSE)
saveRDS(png.3.train.prune.1, "pairedngrams/ng1-4.prune.1_png.3.train.rds")
write.csv2(png.4.train.prune.1, "pairedngrams/ng1-4.prune.1_png.4.train.csv", row.names = FALSE)
saveRDS(png.4.train.prune.1, "pairedngrams/ng1-4.prune.1_png.4.train.rds")

sum(object.size(c(png.1.train.prune.1, png.2.train.prune.1, png.3.train.prune.1, png.4.train.prune.1)))

png.4.train <- data.table::fread("pairedngrams/ng1-4_png.4.train.csv")
png.3.train <- data.table::fread("pairedngrams/ng1-4_png.3.train.csv")
png.2.train <- data.table::fread("pairedngrams/ng1-4_png.2.train.csv")
png.1.train <- data.table::fread("pairedngrams/ng1-4_png.1.train.csv")



png.1.train.prune.10 <- png.1.train[freq>10]
png.2.train.prune.10 <- png.2.train[freq>10]
png.3.train.prune.10 <- png.3.train[freq>10]
png.4.train.prune.10 <- png.4.train[freq>10]
png.1.train.prune.10[, freq:=NULL]
png.2.train.prune.10[, freq:=NULL]
png.3.train.prune.10[, freq:=NULL]
png.4.train.prune.10[, freq:=NULL]
write.csv2(png.1.train.prune.10, "pairedngrams/ng1-4.prune.10_png.1.train.csv", row.names = FALSE)
saveRDS(png.1.train.prune.10, "pairedngrams/ng1-4.prune.10_png.1.train.rds")
write.csv2(png.2.train.prune.10, "pairedngrams/ng1-4.prune.10_png.2.train.csv", row.names = FALSE)
saveRDS(png.2.train.prune.10, "pairedngrams/ng1-4.prune.10_png.2.train.rds")
write.csv2(png.3.train.prune.10, "pairedngrams/ng1-4.prune.10_png.3.train.csv", row.names = FALSE)
saveRDS(png.3.train.prune.10, "pairedngrams/ng1-4.prune.10_png.3.train.rds")
write.csv2(png.4.train.prune.10, "pairedngrams/ng1-4.prune.10_png.4.train.csv", row.names = FALSE)
saveRDS(png.4.train.prune.10, "pairedngrams/ng1-4.prune.10_png.4.train.rds")

sum(object.size(c(png.1.train.prune.10, png.2.train.prune.10, png.3.train.prune.10, png.4.train.prune.10)))

# png.1.train[, freq:=NULL]
# png.2.train[, freq:=NULL]
# png.3.train[, freq:=NULL]
# png.4.train[, freq:=NULL]

write.csv2(png.1.train, "pairedngrams/ng1-4_png.1.train.csv", row.names = FALSE)
saveRDS(png.1.train, "pairedngrams/ng1-4_png.1.train.rds")
write.csv2(png.2.train, "pairedngrams/ng1-4_png.2.train.csv", row.names = FALSE)
saveRDS(png.2.train, "pairedngrams/ng1-4_png.2.train.rds")
write.csv2(png.3.train, "pairedngrams/ng1-4_png.3.train.csv", row.names = FALSE)
saveRDS(png.3.train, "pairedngrams/ng1-4_png.3.train.rds")
write.csv2(png.4.train, "pairedngrams/ng1-4_png.4.train.csv", row.names = FALSE)
saveRDS(png.4.train, "pairedngrams/ng1-4_png.4.train.rds")

sum(object.size(c(png.1.train, png.2.train, png.3.train, png.4.train)))


##############################################################"
# Prediction part
##############################################################"

png.4 <- readRDS("pairedngrams/ng1-4_png.4.train.rds")
png.3 <- readRDS("pairedngrams/ng1-4_png.3.train.rds")
png.2 <- readRDS("pairedngrams/ng1-4_png.2.train.rds")
png.1 <- readRDS("pairedngrams/ng1-4_png.1.train.rds")
sum(object.size(c(png.1, png.2, png.3, png.4)))

png.4 <- readRDS("pairedngrams/ng1-4.prune.1_png.4.train.rds")
png.3 <- readRDS("pairedngrams/ng1-4.prune.1_png.3.train.rds")
png.2 <- readRDS("pairedngrams/ng1-4.prune.1_png.2.train.rds")
png.1 <- readRDS("pairedngrams/ng1-4.prune.1_png.1.train.rds")
sum(object.size(c(png.1, png.2, png.3, png.4)))

png.4 <- readRDS("pairedngrams/ng1-4.prune.2_png.4.train.rds")
png.3 <- readRDS("pairedngrams/ng1-4.prune.2_png.3.train.rds")
png.2 <- readRDS("pairedngrams/ng1-4.prune.2_png.2.train.rds")
png.1 <- readRDS("pairedngrams/ng1-4.prune.2_png.1.train.rds")
sum(object.size(c(png.1, png.2, png.3, png.4)))

png.4 <- readRDS("pairedngrams/bak_big/ng1-4.prune.1_png.4.train.rds")
png.3 <- readRDS("pairedngrams/bak_big/ng1-4.prune.1_png.3.train.rds")
png.2 <- readRDS("pairedngrams/bak_big/ng1-4.prune.1_png.2.train.rds")
png.1 <- readRDS("pairedngrams/bak_big/ng1-4.prune.1_png.1.train.rds")
sum(object.size(c(png.1, png.2, png.3, png.4)))

png.4 <- readRDS("pairedngrams/ng1-4.prune.10_png.4.train.rds")
png.3 <- readRDS("pairedngrams/ng1-4.prune.10_png.3.train.rds")
png.2 <- readRDS("pairedngrams/ng1-4.prune.10_png.2.train.rds")
png.1 <- readRDS("pairedngrams/ng1-4.prune.10_png.1.train.rds")
sum(object.size(c(png.1, png.2, png.3, png.4)))


data.table::setkey(png.4, grams.3, grams.3.stem, grams.0)
data.table::setkey(png.3, grams.2, grams.2.stem, grams.0)
data.table::setkey(png.2, grams.1, grams.1.stem, grams.0)
data.table::setkey(png.1, grams.0)

vocabulary.1 <- data.table::fread("ngrams/vocabulary.1.csv")

removeFirstWord <- function(s) {
  splt <- str_split(s, " ")[[1]]
  nbw <- length(splt)
  rfw <- str_c(splt[2:nbw], collapse = " ")
  if (is.na(rfw)) rfw <- ""
  return(rfw)
}

ngram.predict <- function(input, nbrep = 10, d = 0.4, k = 5, preprocessinput = TRUE) {
  input <- trimws(gsub(tolower(input), pattern = "( ){2,}", replacement = ""))
  if (wordcount(input) == 0) no_input <- TRUE
  else no_input <- FALSE
  wci <- wordcount(input)
  res.gts <- data.table(); res.gts.stem <- data.table(); res.ktz <- data.table(); res.ktz.stem <- data.table()
  input.stem <- paste(char_wordstem(str_split(input, " ")[[1]]), collapse=" ")
  if (!no_input) {
    if (preprocessinput) {
      input.reverse <- stringi::stri_reverse(input)
      input.reverse.short <- gsub(input.reverse, pattern = "[.:,!;](.*)", replacement = "")
      input <- trimws(stringi::stri_reverse(input.reverse.short))
      wci <- wordcount(input)
      input <- gsub(x = input, pattern = "^", replacement = "BEGIN ")
      terms <- str_split(trimws(input), " ")[[1]]
      terms_not_in_voc <- sapply(setdiff(terms, vocabulary.1$feature), function(t) paste0(" ", t, " "))
      if (length(terms_not_in_voc) != 0) {
        unk_terms <- rep(" UNK ", length(terms_not_in_voc))
        input <- trimws(stringi::stri_replace_all_regex(paste0(input, " "), terms_not_in_voc, unk_terms, vectorize_all = FALSE)[1])
      }
      input.stem <- paste(char_wordstem(str_split(input, " ")[[1]]), collapse=" ")
      input <- trimws(gsub(input, pattern = "( ){2,}", replacement = ""))
      input.stem <- paste(char_wordstem(str_split(input, " ")[[1]]), collapse=" ")
      if (wordcount(input) == 0) no_input <- TRUE
      else no_input <- FALSE
    }
    # print(paste0("ngram.predict : input = ", input))
    # print(paste0("ngram.predict : input.stem = ", input.stem))
    # print("-----------------------------------")
    options(datatable.nomatch=0)
    
    while (ngram::wordcount(input) > 3) {
      input <- removeFirstWord(input)
      input.stem <- removeFirstWord(input.stem)
    }
    if (ngram::wordcount(input) == 3) {
      matches <- png.4[grams.3 == input]
      matches <- matches[!is.na(matches$p.mle)]
      if (nrow(matches) != 0) {
        res.matches <- matches[order(p.mle, decreasing = TRUE)]
        res.gts <- rbind(res.gts, data.frame(n = rep(4, nrow(res.matches)), grams.n = res.matches$grams.3,
                                             grams = res.matches$grams.0, p.gts = res.matches$p.mle))
        res.matches <- matches[order(p.ktz, decreasing = TRUE)]
        res.ktz <- rbind(res.ktz, data.frame(n = rep(4, nrow(res.matches)), grams.n = res.matches$grams.3,
                                             grams = res.matches$grams.0, p.ktz = res.matches$p.ktz))
      } 
      matches <- png.4[grams.3.stem == input.stem][order(p.mle, decreasing = TRUE)]
      matches <- matches[!is.na(matches$p.mle)]
      if (nrow(matches) != 0) {
        res.matches <- matches[order(p.mle, decreasing = TRUE)]
        res.gts.stem <- rbind(res.gts.stem, data.frame(n = rep(4, nrow(res.matches)), grams.n = res.matches$grams.3,
                                                       grams = res.matches$grams.0, p.gts = res.matches$p.mle))
        res.matches <- matches[order(p.ktz, decreasing = TRUE)]
        res.ktz.stem <- rbind(res.ktz.stem, data.frame(n = rep(4, nrow(res.matches)), grams.n = res.matches$grams.3,
                                                       grams = res.matches$grams.0, p.ktz = res.matches$p.ktz))
      } 
      input <- removeFirstWord(input)
      input.stem <- removeFirstWord(input.stem)
    }
    if (ngram::wordcount(input) == 2) {
      matches <- png.3[grams.2 == input]
      matches <- matches[!is.na(matches$p.mle)]
      if (nrow(matches) != 0) {
        res.matches <- matches[order(p.mle, decreasing = TRUE)]
        if (wci <= 1) res.gts <- rbind(res.gts, data.frame(n = rep(3, nrow(res.matches)), grams.n = res.matches$grams.2,
                                                           grams = res.matches$grams.0, p.gts = res.matches$p.mle))
        else res.gts <- rbind(res.gts, data.frame(n = rep(3, nrow(res.matches)), grams.n = res.matches$grams.2,
                                                  grams = res.matches$grams.0, p.gts = d * res.matches$p.mle))
        res.matches <- matches[order(p.ktz, decreasing = TRUE)]
        if (wci <= 1) res.ktz <- rbind(res.ktz, data.frame(n = rep(3, nrow(res.matches)), grams.n = res.matches$grams.2,
                                                           grams = res.matches$grams.0, p.ktz = res.matches$p.ktz))
        else res.ktz <- rbind(res.ktz, data.frame(n = rep(3, nrow(res.matches)), grams.n = res.matches$grams.2,
                                                  grams = res.matches$grams.0, p.ktz = res.matches$alpha.n * res.matches$p.ktz))
        
      } 
      matches <- png.3[grams.2.stem == input.stem]
      matches <- matches[!is.na(matches$p.mle)]
      if (nrow(matches) != 0) {
        res.matches <- matches[order(p.mle, decreasing = TRUE)]
        if (wci <= 1) res.gts.stem <- rbind(res.gts.stem, data.frame(n = rep(3, nrow(res.matches)), grams.n = res.matches$grams.2,
                                                                     grams = res.matches$grams.0, p.gts = res.matches$p.mle))
        else res.gts.stem <- rbind(res.gts.stem, data.frame(n = rep(3, nrow(res.matches)), grams.n = res.matches$grams.2,
                                                            grams = res.matches$grams.0, p.gts = d * res.matches$p.mle))
        res.matches <- matches[order(p.ktz, decreasing = TRUE)]
        if (wci <= 1) res.ktz.stem <- rbind(res.ktz.stem, data.frame(n = rep(3, nrow(res.matches)), grams.n = res.matches$grams.2,
                                                                     grams = res.matches$grams.0, p.ktz = res.matches$p.ktz))
        else res.ktz.stem <- rbind(res.ktz.stem, data.frame(n = rep(3, nrow(res.matches)), grams.n = res.matches$grams.2,
                                                            grams = res.matches$grams.0, p.ktz = res.matches$alpha.n * res.matches$p.ktz))
      } 
      input <- removeFirstWord(input)
      input.stem <- removeFirstWord(input.stem)
    }
    if (ngram::wordcount(input) == 1) {
      matches <- png.2[grams.1 == input]
      matches <- matches[!is.na(matches$p.mle)]
      if (nrow(matches) != 0) {
        res.matches <- matches[order(p.mle, decreasing = TRUE)]
        if (wci <= 1) res.gts <- rbind(res.gts, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                                           grams = res.matches$grams.0, p.gts = res.matches$p.mle))
        else res.gts <- rbind(res.gts, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                                  grams = res.matches$grams.0, p.gts = d^2 * res.matches$p.mle))
        res.matches <- matches[order(p.ktz, decreasing = TRUE)]
        if (wci <= 1) res.ktz <- rbind(res.ktz, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                                           grams = res.matches$grams.0, p.ktz = res.matches$p.ktz))
        else res.ktz <- rbind(res.ktz, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                                  grams = res.matches$grams.0, p.ktz = res.matches$alpha.n * res.matches$p.ktz))
      } 
      matches <- png.2[grams.1.stem == input.stem]
      matches <- matches[!is.na(matches$p.mle)]
      if (nrow(matches) != 0) {
        res.matches <- matches[order(p.mle, decreasing = TRUE)]
        if (wci <= 1) res.gts.stem <- rbind(res.gts.stem, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                                                     grams = res.matches$grams.0, p.gts = res.matches$p.mle))
        else res.gts.stem <- rbind(res.gts.stem, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                                            grams = res.matches$grams.0, p.gts = d^2 * res.matches$p.mle))
        res.matches <- matches[order(p.ktz, decreasing = TRUE)]
        if (wci <= 1) res.ktz.stem <- rbind(res.ktz.stem, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                                                     grams = res.matches$grams.0, p.ktz = res.matches$p.ktz))
        else res.ktz.stem <- rbind(res.ktz.stem, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                                            grams = res.matches$grams.0, p.ktz = res.matches$alpha.n * res.matches$p.ktz))
      } 
      input <- removeFirstWord(input)
      input.stem <- removeFirstWord(input.stem)
    }
  }
  
  if (ngram::wordcount(input) == 0) {
    if (no_input) {
      input = "BEGIN"
      input.stem = "BEGIN"
      matches <- png.2[grams.1 == input]
      matches <- matches[!is.na(matches$p.mle)]
      if (nrow(matches) != 0) {
        res.matches <- matches[order(p.mle, decreasing = TRUE)]
        res.gts <- rbind(res.gts, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                             grams = res.matches$grams.0, p.gts = res.matches$p.mle))
        res.matches <- matches[order(p.ktz, decreasing = TRUE)]
        res.ktz <- rbind(res.ktz, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                             grams = res.matches$grams.0, p.ktz = res.matches$p.ktz))
      } 
      matches <- png.2[grams.1.stem == input.stem]
      matches <- matches[!is.na(matches$p.mle)]
      if (nrow(matches) != 0) {
        res.matches <- matches[order(p.mle, decreasing = TRUE)]
        res.gts.stem <- rbind(res.gts.stem, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                                       grams = res.matches$grams.0, p.gts = res.matches$p.mle))
        res.matches <- matches[order(p.ktz, decreasing = TRUE)]
        res.ktz.stem <- rbind(res.ktz.stem, data.frame(n = rep(2, nrow(res.matches)), grams.n = res.matches$grams.1,
                                                       grams = res.matches$grams.0, p.ktz = res.matches$p.ktz))
      }
    }
    else {
      matches <- png.1[!is.na(png.1$p.mle)]
      res.matches <- matches[order(p.mle, decreasing = TRUE)]
      res.gts <- rbind(res.gts, data.frame(n = rep(1, nrow(res.matches)), grams.n = res.matches$grams.0,
                                           grams = res.matches$grams.0, p.gts = d^3 * res.matches$p.mle))
      res.gts.stem <- rbind(res.gts.stem, data.frame(n = rep(1, nrow(res.matches)), grams.n = res.matches$grams.0,
                                                     grams = res.matches$grams.0, p.gts = d^3 * res.matches$p.mle))
      res.matches <- matches[order(p.ktz, decreasing = TRUE)]
      res.ktz <- rbind(res.ktz, data.frame(n = rep(1, nrow(res.matches)), grams.n = res.matches$grams.0,
                                           grams = res.matches$grams.0, p.ktz = res.matches$alpha.n * res.matches$p.ktz))
      res.ktz.stem <- rbind(res.ktz.stem, data.frame(n = rep(1, nrow(res.matches)), grams.n = res.matches$grams.0,
                                                     grams = res.matches$grams.0, p.ktz = res.matches$alpha.n * res.matches$p.ktz))
      
    }
  }
  
  res.gts$grams.n <- as.character(res.gts$grams.n)
  res.gts$grams <- as.character(res.gts$grams)
  res.gts.stem$grams.n <- as.character(res.gts.stem$grams.n)
  res.gts.stem$grams <- as.character(res.gts.stem$grams)
  
  res.ktz$grams.n <- as.character(res.ktz$grams.n)
  res.ktz$grams <- as.character(res.ktz$grams)
  res.ktz.stem$grams.n <- as.character(res.ktz.stem$grams.n)
  res.ktz.stem$grams <- as.character(res.ktz.stem$grams)
  
  if (nbrep!=Inf) {
    res.gts.out <- res.gts[order(n, p.gts, decreasing = TRUE)][1:(min(dim(res.gts)[1], nbrep))]
    res.gts.stem.out <- res.gts.stem[order(n, p.gts, decreasing = TRUE)][1:(min(dim(res.gts.stem)[1], nbrep))]
    res.ktz.out <- res.ktz[order(n, p.ktz, decreasing = TRUE)][1:(min(dim(res.ktz)[1], nbrep))]
    res.ktz.stem.out <- res.ktz.stem[order(n, p.ktz, decreasing = TRUE)][1:(min(dim(res.ktz.stem)[1], nbrep))]
  }
  else {
    res.gts.out <- res.gts[order(n, p.gts, decreasing = TRUE)]
    res.gts.stem.out <- res.gts.stem[order(n, p.gts, decreasing = TRUE)]
    res.ktz.out <- res.ktz[order(n, p.ktz, decreasing = TRUE)]
    res.ktz.stem.out <- res.ktz.stem[order(n, p.ktz, decreasing = TRUE)]
  }
  
  return(list(res.gts = res.gts.out, res.gts.stem = res.gts.stem.out, res.ktz = res.ktz.out, res.ktz.stem = res.ktz.stem.out))
}

d <- 0.4

input <- "show must go "
choices <- c("offense", "players", "crowd", "pain")

predict.time <- system.time(predictions <- ngram.predict(input, nbrep = 10, d = d))[3]
predictions[is.element(grams, choices)][order(n, p.gts, decreasing = TRUE)]
predictions

pred_type <- "res.ktz"
unique_pred <- unique(predictions[[pred_type]]$grams)
Prob <- matrix(rep(0, 40), nrow = 10, ncol = 4)
rn <- vector(length=10)
for (i in 1:length(unique_pred)) {
  for (j in 1:4) {
    pred <- predictions[[pred_type]][n==j & grams==unique_pred[i]]
    if (nrow(pred)!=0) Prob[i, j] <- pred[1, p.ktz]
  }
  rn[i] <- as.character(unique_pred[i])
}
if (length(unique_pred) > 10) for (i in (length(unique_pred) + 1):10) rn[i] <- as.character(i)

rownames(Prob) <- rn
Prob
axx <- list(nticks = 4, title = "Nb grams")
axy <- list(nticks = 10, title = "Predictions")
g <- plot_ly(x = 1:4, y = as.factor(unique_pred), z = ~Prob, type = "surface", showscale = FALSE) %>%
  layout(autosize = T, title = "Katz Backoff", scene = list(yaxis=axy, xaxis=axx, margin = list(l=0,r=0,t=0,b=0,pad=0),
                                                            camera = list(eye = list(x = 2.0, y = 1.8, z = 1.4))))
g
Prob

testing.dev.sample <- sampleDS(testing.dev.full, prop = 1)
vocabulary.1 <- data.table::fread("ngrams/vocabulary.1.csv")

ng <- ngrams(testing.dev.sample, what = "word", nbGram = 4, stem=TRUE, weight=NULL, smooth=NULL, skip=0, min_docfreq = 1, vocabulary.1)
ng.1.testing.dev.sample <- ng[["ng.1"]]
ng.2.testing.dev.sample <- ng[["ng.2"]]
ng.3.testing.dev.sample <- ng[["ng.3"]]
ng.4.testing.dev.sample <- ng[["ng.4"]]

ng.prob <- computeProbs(ng.1.testing.dev.sample, ng.2.testing.dev.sample, ng.3.testing.dev.sample, ng.4.testing.dev.sample, vocabulary.1, writetodisk = FALSE)
ng.1.testing.dev.sample <- ng.prob[["ng.1"]]
ng.2.testing.dev.sample <- ng.prob[["ng.2"]]
ng.3.testing.dev.sample <- ng.prob[["ng.3"]]
ng.4.testing.dev.sample <- ng.prob[["ng.4"]]

png.1.testing.dev.sample <- pairedngrams(ngram = ng.1.testing.dev.sample, nbGram = 1, stem = TRUE)
png.2.testing.dev.sample <- pairedngrams(ngram = ng.2.testing.dev.sample, nbGram = 2, stem = TRUE, pngram1 = png.1.testing.dev.sample)
png.3.testing.dev.sample <- pairedngrams(ngram = ng.3.testing.dev.sample, nbGram = 3, stem = TRUE, pngram1 = png.2.testing.dev.sample)
png.4.testing.dev.sample <- pairedngrams(ngram = ng.4.testing.dev.sample, nbGram = 4, stem = TRUE, pngram1 = png.3.testing.dev.sample)

png.1.testing.dev.sample[png.2.testing.dev.sample, on=.(grams.0), alpha.n:=i.alpha]
png.2.testing.dev.sample[png.3.testing.dev.sample, on=.(grams.1), alpha.n:=i.alpha]
png.3.testing.dev.sample[png.4.testing.dev.sample, on=.(grams.2), alpha.n:=i.alpha]
# png.1.testing.dev.sample[png.2.testing.dev.sample, on=.(grams.1), alpha.n:=i.alpha]
# png.2.testing.dev.sample[png.3.testing.dev.sample, on=.(grams.2), alpha.n:=i.alpha]
# png.3.testing.dev.sample[png.4.testing.dev.sample, on=.(grams.3), alpha.n:=i.alpha]

ngram.predict.gts <- function(ng, d = d) {
  print(paste("ngram.predict.gts :", ng))
  ngp <- ngram.predict(ng, nbrep = 10, d = d, preprocessinput = FALSE)
  pred.gts <- ngp[["res.gts"]]
  return (pred.gts)
}
ngram.predict.gts.stem <- function(ng, d = d) {
  print(paste("ngram.predict.gts.stem :", ng))
  ngp <- ngram.predict(ng, nbrep = 10, d = d, preprocessinput = FALSE)
  pred.gts <- ngp[["res.gts.stem"]]
  return (pred.gts)
}
ngram.predict.ktz <- function(ng, d = d) {
  print(paste("ngram.predict.ktz :", ng))
  ngp <- ngram.predict(ng, nbrep = 10, d = d, preprocessinput = FALSE)
  pred.ktz <- ngp[["res.ktz"]]
  return (pred.ktz)
}
ngram.predict.ktz.stem <- function(ng, d = d) {
  print(paste("ngram.predict.ktz.stem :", ng))
  ngp <- ngram.predict(ng, nbrep = 10, d = d, preprocessinput = FALSE)
  pred.ktz <- ngp[["res.ktz.stem"]]
  return (pred.ktz)
}

ngram.accuracy <- function(ngrams.test, d = d, pred.fn) {
  predict.dev <- sapply(ngrams.test$grams.3, pred.fn, d = d)
  predict.dev.gram.list <- predict.dev[3,]
  predict.dev.gram.1 <- sapply(predict.dev.gram.list, function(listitem) listitem[1])
  predict.dev.gram.2 <- sapply(predict.dev.gram.list, function(listitem) listitem[2])
  predict.dev.gram.3 <- sapply(predict.dev.gram.list, function(listitem) listitem[3])
  predict.dev.gram.4 <- sapply(predict.dev.gram.list, function(listitem) listitem[4])
  predict.dev.gram.5 <- sapply(predict.dev.gram.list, function(listitem) listitem[5])
  predict.dev.gram.6 <- sapply(predict.dev.gram.list, function(listitem) listitem[6])
  predict.dev.gram.7 <- sapply(predict.dev.gram.list, function(listitem) listitem[7])
  predict.dev.gram.8 <- sapply(predict.dev.gram.list, function(listitem) listitem[8])
  predict.dev.gram.9 <- sapply(predict.dev.gram.list, function(listitem) listitem[9])
  predict.dev.gram.10 <- sapply(predict.dev.gram.list, function(listitem) listitem[10])
  pred.dt <- data.table(grams.pred.1 = predict.dev.gram.1, grams.pred.2 = predict.dev.gram.2, grams.pred.3 = predict.dev.gram.3,
                        grams.pred.4 = predict.dev.gram.4, grams.pred.5 = predict.dev.gram.5, grams.pred.6 = predict.dev.gram.6,
                        grams.pred.7 = predict.dev.gram.7, grams.pred.8 = predict.dev.gram.8, grams.pred.9 = predict.dev.gram.9,
                        grams.pred.10 = predict.dev.gram.10, freq = ngrams.test$freq, grams.0 = ngrams.test$grams.0)
  errors <- pred.dt[grams.pred.1 != grams.0]
  errors.weight <- sum(errors$freq)
  error_rate <- errors.weight / sum(ngrams.test$freq)
  errors.3 <- pred.dt[grams.pred.1 != grams.0 & grams.pred.2 != grams.0 & grams.pred.3 != grams.0]
  errors.3.weight <- sum(errors.3$freq)
  error.3_rate <- errors.3.weight / sum(ngrams.test$freq)
  errors.10 <- pred.dt[grams.pred.1 != grams.0 & grams.pred.2 != grams.0 & grams.pred.3 != grams.0 & grams.pred.4 != grams.0
                       & grams.pred.5 != grams.0 & grams.pred.6 != grams.0 & grams.pred.7 != grams.0 & grams.pred.8 != grams.0
                       & grams.pred.9 != grams.0 & grams.pred.10 != grams.0]
  errors.10.weight <- sum(errors.10$freq)
  error.10_rate <- errors.10.weight / sum(ngrams.test$freq)
  return (list(acc = 1 - error_rate, acc.3 = 1 - error.3_rate, acc.10 = 1 - error.10_rate))
}
acc.gts <- parallelizeTask(ngram.accuracy, png.4.testing.dev.sample, d = d, ngram.predict.gts)

acc.gts.stem <- parallelizeTask(ngram.accuracy, png.4.testing.dev.sample, d = d, ngram.predict.gts.stem)

acc.ktz <- parallelizeTask(ngram.accuracy, png.4.testing.dev.sample, d = d, ngram.predict.ktz)

acc.ktz.stem <- parallelizeTask(ngram.accuracy, png.4.testing.dev.sample, d = d, ngram.predict.ktz.stem)

accuracy_table <- data.frame(Type = c("SBO", "SBO-stemming", "KBO", "KBO-stemming"),
                             Acc = c(acc.gts[["acc"]], acc.gts.stem[["acc"]], acc.ktz[["acc"]], acc.ktz.stem[["acc"]]),
                             Acc_3 = c(acc.gts[["acc.3"]], acc.gts.stem[["acc.3"]], acc.ktz[["acc.3"]], acc.ktz.stem[["acc.3"]]),
                             Acc_10 = c(acc.gts[["acc.10"]], acc.gts.stem[["acc.10"]], acc.ktz[["acc.10"]], acc.ktz.stem[["acc.10"]]))
knitr::kable(accuracy_table)
data.table::fwrite(accuracy_table, "pairedngrams/accuracy_table.csv")

#####################################################
## SMALL : train .001 of full #######################
#####################################################

# |Type         |  Accuracy| Accuracy_2| Accuracy_3| Accuracy_4|
# |:------------|---------:|----------:|----------:|----------:|
# |SBO          | 0.1211073|  0.1753172|  0.2122261|          1|
# |SBO-stemming | 0.1188005|  0.1718570|  0.2053057|          1|
# |KBO          | 0.1384083|  0.1868512|  0.2191465|          1|
# |KBO-stemming | 0.1407151|  0.1891580|  0.2145329|          1|
#####################################################
## MED : train .01 of full ##########################
#####################################################

# |Type         |  Accuracy| Accuracy_3| Accuracy_2| Accuracy_10|
# |:------------|---------:|----------:|----------:|-----------:|
# |SBO          | 0.1413613|  0.2443281|  0.2111693|           1|
# |SBO-stemming | 0.1413613|  0.2303665|  0.1937173|           1|
# |KBO          | 0.1588133|  0.2513089|  0.2164049|           1|
# |KBO-stemming | 0.1657941|  0.2390925|  0.2024433|           1|
#####################################################
## BIG : train .1 of full ###########################
#####################################################

# sum(object.size(c(png.1, png.2, png.3, png.4)))
# [1] 228656640

# |Type         |  Accuracy| Accuracy_3| Accuracy_10|
# |:------------|---------:|----------:|-----------:|
# |SBO          | 0.1954625|  0.2949389|           1|
# |SBO-stemming | 0.1832461|  0.2670157|           1|
# |KBO          | 0.1954625|  0.2949389|           1|
# |KBO-stemming | 0.1884817|  0.2774869|           1|

# *************************************************************
# *************************************************************
# *************************************************************

#####################################################
## SMALL : train .001 of full #######################
#####################################################
# |Type         |       Acc|     Acc_3|    Acc_10|
# |:------------|---------:|---------:|---------:|
# |SBO          | 0.1316583| 0.2160804| 0.3396985|
# |SBO-stemming | 0.1266332| 0.2030151| 0.3366834|
# |KBO          | 0.1316583| 0.2160804| 0.3346734|
# |KBO-stemming | 0.1316583| 0.2100503| 0.3256281|
#####################################################
## MED : train .01 of full ##########################
#####################################################
# |Type         |       Acc|     Acc_3|    Acc_10|
# |:------------|---------:|---------:|---------:|
# |SBO          | 0.1707989| 0.2589532| 0.3567493|
# |SBO-stemming | 0.1611570| 0.2438017| 0.3498623|
# |KBO          | 0.1707989| 0.2589532| 0.3567493|
# |KBO-stemming | 0.1749311| 0.2534435| 0.3498623|

#####################################################
## BIG : train .1 of full ###########################
#####################################################
# |Type         |       Acc|     Acc_3|    Acc_10|
# |:------------|---------:|---------:|---------:|
# |SBO          | 0.1959799| 0.3095477| 0.4211055|
# |SBO-stemming | 0.1688442| 0.2834171| 0.3919598|
# |KBO          | 0.1919598| 0.3095477| 0.4170854|
# |KBO-stemming | 0.1819095| 0.2984925| 0.3949749|

#####################################################
## BIGGER : train .5 of full ########################
#####################################################
## NB : END a été enlevé des prédictions possibles !! => résultats moins bons
# sum(object.size(c(png.1, png.2, png.3, png.4)))
# [1] 228656640
# 
# |Type         |  Accuracy| Accuracy_2| Accuracy_3| Accuracy_4|
# |:------------|---------:|----------:|----------:|----------:|
# |SBO          | 0.1718570|  0.2387543|  0.2733564|          1|
# |SBO-stemming | 0.1534025|  0.2168397|  0.2387543|          1|
# |KBO          | 0.1718570|  0.2387543|  0.2722030|          1|
# |KBO-stemming | 0.1637832|  0.2318339|  0.2595156|          1|

ds <- seq(0.1, 0.9, 0.1)
accuracies <- function(ds) {
  accs <- sapply(ds, function(dsi) {
    return(ngram.accuracy(png.4.testing.dev.sample, d = dsi, ngram.predict.gts))
  })
  return(accs)
}
a <- accuracies(ds)
a
##############################################################################


