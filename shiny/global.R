library(ngram)
library(wordcloud)
library(RColorBrewer)
library(quanteda)
library(data.table)
library(ggplot2)
library(grid)
library(methods)
library(gridBase)
library(stringr)
library(shiny)
library(shinythemes)
library(shinyjs)
library(parallel)
library(doParallel)
library(plotly)
library(knitr)


png.4.high <- readRDS("pairedngrams/big/ng1-4.prune.1_png.4.train.rds")
png.3.high <- readRDS("pairedngrams/big/ng1-4.prune.1_png.3.train.rds")
png.2.high <- readRDS("pairedngrams/big/ng1-4.prune.1_png.2.train.rds")
png.1.high <- readRDS("pairedngrams/big/ng1-4.prune.1_png.1.train.rds")
data.table::setkey(png.4.high, grams.3, grams.0)
data.table::setkey(png.3.high, grams.2, grams.0)
data.table::setkey(png.2.high, grams.1, grams.0)
data.table::setkey(png.1.high, grams.0)
vocabulary.1.high <- data.table::fread("pairedngrams/big/vocabulary.1.csv")

png.4.medium <- readRDS("pairedngrams/medium/ng1-4.prune.1_png.4.train.rds")
png.3.medium <- readRDS("pairedngrams/medium/ng1-4.prune.1_png.3.train.rds")
png.2.medium <- readRDS("pairedngrams/medium/ng1-4.prune.1_png.2.train.rds")
png.1.medium <- readRDS("pairedngrams/medium/ng1-4.prune.1_png.1.train.rds")
data.table::setkey(png.4.medium, grams.3, grams.0)
data.table::setkey(png.3.medium, grams.2, grams.0)
data.table::setkey(png.2.medium, grams.1, grams.0)
data.table::setkey(png.1.medium, grams.0)
vocabulary.1.medium <- data.table::fread("pairedngrams/medium/vocabulary.1.csv")

png.4.small <- readRDS("pairedngrams/small/ng1-4.prune.1_png.4.train.rds")
png.3.small <- readRDS("pairedngrams/small/ng1-4.prune.1_png.3.train.rds")
png.2.small <- readRDS("pairedngrams/small/ng1-4.prune.1_png.2.train.rds")
png.1.small <- readRDS("pairedngrams/small/ng1-4.prune.1_png.1.train.rds")
data.table::setkey(png.4.small, grams.3, grams.0)
data.table::setkey(png.3.small, grams.2, grams.0)
data.table::setkey(png.2.small, grams.1, grams.0)
data.table::setkey(png.1.small, grams.0)
vocabulary.1.small <- data.table::fread("pairedngrams/small/vocabulary.1.csv")


png.4 <- png.4.medium
png.3 <- png.3.medium
png.2 <- png.2.medium
png.1 <- png.1.medium

vocabulary.1 <- vocabulary.1.medium

removeFirstWord <- function(s) {
  splt <- str_split(s, " ")[[1]]
  nbw <- length(splt)
  rfw <- str_c(splt[2:nbw], collapse = " ")
  if (is.na(rfw)) rfw <- ""
  return(rfw)
}

ngram.predict <- function(input, quality, nbrep = 10, d = 0.4, k = 5, preprocessinput = TRUE) {
  if (quality == "high") {
    png.4 <- png.4.high
    png.3 <- png.3.high
    png.2 <- png.2.high
    png.1 <- png.1.high
  }
  if (quality == "medium") {
    png.4 <- png.4.medium
    png.3 <- png.3.medium
    png.2 <- png.2.medium
    png.1 <- png.1.medium
  }
  if (quality == "small") {
    png.4 <- png.4.small
    png.3 <- png.3.small
    png.2 <- png.2.small
    png.1 <- png.1.small
  }

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

computeProbs <- function(ng.1.train, ng.2.train, ng.3.train, ng.4.train, vocabulary.1, writetodisk = TRUE) {
  
  K = 5
  #####################
  # 1-grams
  #####################
  print("# 1-grams")
  N <- table(ng.1.train$frequency)
  
  # Maximum Likelihood
  N.mle <- sum(as.integer(names(N))*N)
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
    # else return (f)
    else return ((f + 1) * (N.1(f + 1) / N.1(f)))
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
  ng.1.train[, p.ktz := ktz.cnt / N.ktz]
  # ng.1.train[, p.ktz := d.ktz * p.mle]
  
  
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
    # else return (f)
    else return ((f + 1) * (N.2(f + 1) / N.2(f)))
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
  N.ktz <- sum(ng.2.train$ktz.cnt)
  ng.2.train[, p.ktz := ktz.cnt / N.ktz]
  
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
    # else return (f)
    else return ((f + 1) * (N.3(f + 1) / N.3(f)))
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
  N.ktz <- sum(ng.3.train$ktz.cnt)
  ng.3.train[, p.ktz := ktz.cnt / N.ktz]
  
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
    # else return (f)
    else return ((f + 1) * (N.4(f + 1) / N.4(f)))
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
  N.ktz <- sum(ng.4.train$ktz.cnt)
  ng.4.train[, p.ktz := ktz.cnt / N.ktz]
  
  if (writetodisk) {
    data.table::fwrite(ng.1.train, file = paste0("ngrams/1/ng1-4_ng.1.train.prob.csv"), row.names = FALSE)
    data.table::fwrite(ng.2.train, file = paste0("ngrams/2/ng1-4_ng.2.train.prob.csv"), row.names = FALSE)
    data.table::fwrite(ng.3.train, file = paste0("ngrams/3/ng1-4_ng.3.train.prob.csv"), row.names = FALSE)
    data.table::fwrite(ng.4.train, file = paste0("ngrams/4/ng1-4_ng.4.train.prob.csv"), row.names = FALSE)
  }
  
  return(list(ng.1 = ng.1.train, ng.2 = ng.2.train, ng.3 = ng.3.train, ng.4 = ng.4.train))
  
}

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
    if (nbGram > 2) ng.paired <- data.table(grams = ngram$feature, grams.n = grams.n, grams.n.1 = grams.n.1, grams.n.stem = grams.n.stem, grams.0 = grams.0, freq = ngram$frequency,
                                            d.gts = ngram$d.gts, d.ktz = ngram$d.ktz, p.ktz = ngram$p.ktz)
    else ng.paired <- data.table(grams = ngram$feature, grams.n = grams.n, grams.n.stem = grams.n.stem, grams.0 = grams.0, freq = ngram$frequency,
                                 d.gts = ngram$d.gts, d.ktz = ngram$d.ktz, p.ktz = ngram$p.ktz)
    
    data.table::setnames(pngram1, old=paste0("grams.", nbGram - 1), new="grams.n")
    if (nbGram > 2) data.table::setnames(pngram1, old=paste0("grams.", nbGram - 2), new="grams.n.1")
    
    ng.paired[pngram1, on=.(grams.n), freq.n:=i.freq]
    ng.paired[, p.mle:=(freq / freq.n)]
    ng.paired[, p.gts:=d.gts * p.mle]
    # ng.paired[, p.ktz:=d.ktz * p.mle]
    
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

ngram.predict.gts <- function(ngcomb, d = d, session, traceVals, quality) {
  ngsplit <- str_split(ngcomb, "_/_")[[1]]
  ng <- ngsplit[1]
  grams0 <- ngsplit[2]
  print(paste("ngram.predict.sbo :", ng))
  ngp <- ngram.predict(ng, quality, nbrep = 10, d = d, preprocessinput = FALSE)
  pred.gts <- ngp[["res.gts"]]
  tracePred <- paste(" > ", pred.gts[1, n], " | ", pred.gts[1, grams], sep = "")
  traceVals$content <- paste(traceVals$content, paste0("ngram.predict.sbo : ", ng, tracePred, " (", grams0, ")"), "\n")
  return (pred.gts)
}
ngram.predict.gts.stem <- function(ngcomb, d = d, session, traceVals, quality) {
  ngsplit <- str_split(ngcomb, "_/_")[[1]]
  ng <- ngsplit[1]
  grams0 <- ngsplit[2]
  print(paste("ngram.predict.sbo.stem :", ng))
  ngp <- ngram.predict(ng, quality, nbrep = 10, d = d, preprocessinput = FALSE)
  pred.gts <- ngp[["res.gts.stem"]]
  tracePred <- paste(" > ", pred.gts[1, n], " | ", pred.gts[1, grams], sep = "")
  traceVals$content <- paste(traceVals$content, paste0("ngram.predict.sbo.stem : ", ng, tracePred, " (", grams0, ")"), "\n")
  return (pred.gts)
}
ngram.predict.ktz <- function(ngcomb, d = d, session, traceVals, quality) {
  ngsplit <- str_split(ngcomb, "_/_")[[1]]
  ng <- ngsplit[1]
  grams0 <- ngsplit[2]
  print(paste("ngram.predict.kbo :", ng))
  ngp <- ngram.predict(ng, quality, nbrep = 10, d = d, preprocessinput = FALSE)
  pred.ktz <- ngp[["res.ktz"]]
  tracePred <- paste(" > ", pred.ktz[1, n], " | ", pred.ktz[1, grams], sep = "")
  traceVals$content <- paste(traceVals$content, paste0("ngram.predict.kbo : ", ng, tracePred, " (", grams0, ")"), "\n")
  return (pred.ktz)
}
ngram.predict.ktz.stem <- function(ngcomb, d = d, session, traceVals, quality) {
  ngsplit <- str_split(ngcomb, "_/_")[[1]]
  ng <- ngsplit[1]
  grams0 <- ngsplit[2]
  print(paste("ngram.predict.kbo.stem :", ng))
  ngp <- ngram.predict(ng, quality, nbrep = 10, d = d, preprocessinput = FALSE)
  pred.ktz <- ngp[["res.ktz.stem"]]
  tracePred <- paste(" > ", pred.ktz[1, n], " | ", pred.ktz[1, grams], sep = "")
  traceVals$content <- paste(traceVals$content, paste0("ngram.predict.kbo.stem : ", ng, tracePred, " (", grams0, ")"), "\n")
  return (pred.ktz)
}

ngram.accuracy <- function(ngrams.test, d = d, pred.fn, session, traceVals, quality) {
  # predict.dev.gram.list <- list()
  # for (i in 1:length(ngrams.test$grams.3)) {
  #   predict.dev.gram.list[[i]] <- do.call(pred.fn, list(ng = ngrams.test$grams.3[i], d = d, session, traceVals, ngrams.test$grams.0[i]))
  # }
  # print(str(predict.dev.gram.list))
  predict.dev <- sapply(paste(ngrams.test$grams.3, ngrams.test$grams.0, sep="_/_"), pred.fn, d = d, session, traceVals, quality)
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

parallelizeTask <- function(task, ...) {
  ncores <- detectCores() - 1
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  r <- task(...)
  stopCluster(cl)
  r
}
accuracyProcess <- function(texttoprocess, quality, session, traceVals) {
  if (quality == "high") {
    vocabulary.1 <- vocabulary.1.high
  }
  if (quality == "medium") {
    vocabulary.1 <- vocabulary.1.medium
  }
  if (quality == "small") {
    vocabulary.1 <- vocabulary.1.small
  }
  print("********** Accuracy process **********")
  testing.dev.sample <- str_split(texttoprocess, "\n")[[1]]
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
  acc.gts <- ngram.accuracy(png.4.testing.dev.sample, d = d, "ngram.predict.gts", session, traceVals, quality)
  acc.gts.stem <- ngram.accuracy(png.4.testing.dev.sample, d = d, "ngram.predict.gts.stem", session, traceVals, quality)
  acc.ktz <- ngram.accuracy(png.4.testing.dev.sample, d = d, "ngram.predict.ktz", session, traceVals, quality)
  acc.ktz.stem <- ngram.accuracy(png.4.testing.dev.sample, d = d, "ngram.predict.ktz.stem", session, traceVals, quality)
  # acc.gts <- parallelizeTask(ngram.accuracy, png.4.testing.dev.sample, d = d, ngram.predict.gts, session, traceVals)
  # acc.gts.stem <- parallelizeTask(ngram.accuracy, png.4.testing.dev.sample, d = d, ngram.predict.gts.stem, session, traceVals)
  # acc.ktz <- parallelizeTask(ngram.accuracy, png.4.testing.dev.sample, d = d, ngram.predict.ktz, session, traceVals)
  # acc.ktz.stem <- parallelizeTask(ngram.accuracy, png.4.testing.dev.sample, d = d, ngram.predict.ktz.stem, session, traceVals)
  accuracy_table <- data.frame(Type = c("SBO", "SBO-stemming", "KBO", "KBO-stemming"),
                               Acc = c(acc.gts[["acc"]], acc.gts.stem[["acc"]], acc.ktz[["acc"]], acc.ktz.stem[["acc"]]),
                               Acc_3 = c(acc.gts[["acc.3"]], acc.gts.stem[["acc.3"]], acc.ktz[["acc.3"]], acc.ktz.stem[["acc.3"]]),
                               Acc_10 = c(acc.gts[["acc.10"]], acc.gts.stem[["acc.10"]], acc.ktz[["acc.10"]], acc.ktz.stem[["acc.10"]]))
  shinyjs::hide("workinprogress")
  return(accuracy_table)
}
