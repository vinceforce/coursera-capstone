library(ngram)
library(data.table)
library(knitr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(methods)
library(wordcloud)
library(gridBase)
library(stringr)

fnDataBlog <- "en_US/en_US.blogs.txt"
fnDataNews <- "en_US/en_US.news.txt"
fnDataTwitter <- "en_US/en_US.twitter.txt"

load.blog.time <- system.time(ds.blog <- readLines(fnDataBlog, skipNul = TRUE))[3]
load.news.time <- system.time(ds.news <- readLines(fnDataNews, skipNul = TRUE))[3]
load.twitter.time <- system.time(ds.twitter <- readLines(fnDataTwitter, skipNul = TRUE))[3]

#####################
propSampleData = 0.7

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

sampleDS <- function(v, nbsamples) {
    nbRec <- length(v)
    sliceSize <- floor(nbRec / nbsamples)
    rnd <- runif(n = nbsamples)
    inTrain <- unique(sapply(1:nbsamples, function(i) {
        return(1 + (i-1) * sliceSize + floor(sliceSize * rnd[i]))
    }))
    return(v[inTrain])
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


###############
training.full <- c(sample.blog.training, c(sample.news.training, c(sample.twitter.training)))
testing.full <- c(sample.blog.testing, c(sample.news.testing, c(sample.twitter.testing)))
rm(sample.blog.training); rm(sample.news.training); rm(sample.twitter.training)
rm(sample.blog.testing); rm(sample.news.testing); rm(sample.twitter.testing)
gc()

#########################################################
training <- sampleDS(training.full, 100000)
#########################################################


######################
delim <- ' \r\n\t.,;:"()?!'

uniGramTokeniser <- function(ds) ngram(ds, n = 1, sep = delim)
biGramTokeniser <- function(ds) ngram(ds, n = 2, sep = delim)
triGramTokeniser <- function(ds) ngram(ds, n = 3, sep = delim)
tetraGramTokeniser <- function(ds) ngram(ds, n = 4, sep = delim)
pentaGramTokeniser <- function(ds) ngram(ds, n = 5, sep = delim)
hexaGramTokeniser <- function(ds) ngram(ds, n = 6, sep = delim)

tokenisers <- list(uniGramTokeniser, biGramTokeniser, triGramTokeniser, tetraGramTokeniser,
                   pentaGramTokeniser, hexaGramTokeniser)

cleanTokens <- function(token, delim = ' \r\n\t.,;:"()?!') {
    return(trimws(gsub("(^| )[^a-zA-Z]( |$)", " ", gsub("( ){2,}", " ", gsub(paste0("[^a-zA-Z'’", delim, "-]"), "",
                                     gsub("http(s)*://[[:alnum:]/?=_]", "", gsub("#\\S+", "", token)))))))
}

wC <- function(str) {
    return(wordcount(str, sep = ' \r\n\t.,;:"()?!'))
}

ngramDT <- function(ds, nbGram, tokenisers, delim = ' \r\n\t.,;:"()?!') {
    ds <- tolower(cleanTokens(ds))
    wc <- as.vector(sapply(ds, wC))
    indNOK <- which(wc < nbGram)
    if (length(indNOK) > 0) ds <- ds[- indNOK]
    
    ptDF.sum <- data.table::rbindlist(lapply(
        apply(as.array(ds), 1, tokenisers[[nbGram]]),
        get.phrasetable))[, sum(freq), by=ngrams]
    
    colnames(ptDF.sum) <- c("ngrams", "freq")

    wc <- as.vector(sapply(ptDF.sum$ngrams, wordcount))
    indNOK <- which(wc < nbGram)
    if (length(indNOK) > 0) ptDF.sum <- ptDF.sum[- indNOK]

    ngram.size <- object.size(ptDF.sum)
    
    return(list(ngram.size = ngram.size, ngram = ptDF.sum))
}

###############
pairedngram <- function(ds, nbGram = 1, tokenisers = tokenisers) {
    ng <- ngramDT(ds = ds, nbGram = nbGram, tokenisers = tokenisers)
    ngram <- ng[["ngram"]]
    ngram.size <- ng[["ngram.size"]]
    rm(ng); gc()
    if (nbGram == 1) {
        ng.paired <- data.table(grams.1 = trimws(ngram$ngrams),
                              freq = ngram$freq)[order(freq, decreasing = TRUE)]
        ng.paired.size <- object.size(ng.paired)
    }
    
    else {
        split.ngram <- strsplit(as.character(trimws(ngram$ngrams)), " ", fixed = TRUE)
        grams.n <- sapply(split.ngram, function(s) {if (length(s) == nbGram) Reduce("paste", s[1:nbGram-1])})
        grams.0 <- sapply(split.ngram, function(s) {if (length(s) == nbGram) s[nbGram]})
        ng.paired <- data.table(grams = trimws(ngram$ngrams), grams.n = grams.n, grams.0 = grams.0,
                                  freq = ngram$freq)[order(freq, decreasing = TRUE)]
        ng.paired.size <- object.size(ng.paired)
        setnames(ng.paired, old=c("grams", "grams.n", "grams.0", "freq"),
                 new=c(paste0("grams.", nbGram), paste0("grams.", nbGram - 1), "grams.0", "freq"))
    }
    return(list(ng.paired = ng.paired, ng.paired.size = ng.paired.size))
}

pairedngrams <- function(ds, tokenisers, n = 0) {
    if (n == 0) {
        print("pairedngrams : 1")
        png <- pairedngram(ds = ds, nbGram = 1, tokenisers = tokenisers)
        ng.paired.1 <- png[["ng.paired"]]
        ng.paired.1.size <- png[["ng.paired.size"]]
        
        print("pairedngrams : 2")
        png <- pairedngram(ds = ds, nbGram = 2, tokenisers = tokenisers)
        ng.paired.2 <- png[["ng.paired"]]
        ng.paired.2.size <- png[["ng.paired.size"]]
        
        print("pairedngrams : 3")
        png <- pairedngram(ds = ds, nbGram = 3, tokenisers = tokenisers)
        ng.paired.3 <- png[["ng.paired"]]
        ng.paired.3.size <- png[["ng.paired.size"]]
        
        print("pairedngrams : 4")
        png <- pairedngram(ds = ds, nbGram = 4, tokenisers = tokenisers)
        ng.paired.4 <- png[["ng.paired"]]
        ng.paired.4.size <- png[["ng.paired.size"]]
        
        print("pairedngrams : 5")
        png <- pairedngram(ds = ds, nbGram = 5, tokenisers = tokenisers)
        ng.paired.5 <- png[["ng.paired"]]
        ng.paired.5.size <- png[["ng.paired.size"]]
        
        print("pairedngrams : 6")
        png <- pairedngram(ds = ds, nbGram = 6, tokenisers = tokenisers)
        ng.paired.6 <- png[["ng.paired"]]
        ng.paired.6.size <- png[["ng.paired.size"]]
        
        return(list(ng.paired.1 = ng.paired.1, ng.paired.2 = ng.paired.2, ng.paired.3 = ng.paired.3,
                    ng.paired.4 = ng.paired.4, ng.paired.5 = ng.paired.5, ng.paired.6 = ng.paired.6,
                    ng.paired.1.size = ng.paired.1.size, ng.paired.2.size = ng.paired.2.size,
                    ng.paired.3.size = ng.paired.3.size, ng.paired.4.size = ng.paired.4.size,
                    ng.paired.5.size = ng.paired.5.size))
    }

    if (n != 0) {
        print(paste0("pairedngrams : ", nGram))
        png <- pairedngram(ds = ds, nbGram = n, tokenisers = tokenisers)
        ng.paired <- png[["ng.paired"]]
        ng.paired.size <- png[["ng.paired.size"]]
        return(list(ng.paired = ng.paired, ng.paired.size = ng.paired.size))
    }    
}

png.train <- pairedngrams(ds = training, tokenisers = tokenisers)
png.1.train = png.train[["ng.paired.1"]]
png.2.train = png.train[["ng.paired.2"]]
png.3.train = png.train[["ng.paired.3"]]
png.4.train = png.train[["ng.paired.4"]]
png.5.train = png.train[["ng.paired.5"]]
png.6.train = png.train[["ng.paired.6"]]
png.1.size.train = png.train[["ng.paired.1.size"]]
png.2.size.train = png.train[["ng.paired.2.size"]]
png.3.size.train = png.train[["ng.paired.3.size"]]
png.4.size.train = png.train[["ng.paired.4.size"]]
png.5.size.train = png.train[["ng.paired.5.size"]]
png.6.size.train = png.train[["ng.paired.6.size"]]
rm(png.train); gc()
## Laplace smoothing --> p.ls
png.6.train[png.5.train, on = 'grams.5', freq.5 := i.freq]
V.5 <- nrow(png.5.train)
png.6.train[, p.ls:=(freq + 1 )/(freq.5 + V.5)]
png.6.train <- png.6.train[order(p.ls, decreasing = TRUE)]

png.5.train[png.4.train, on = 'grams.4', freq.4 := i.freq]
V.4 <- nrow(png.4.train)
png.5.train[, p.ls:=(freq + 1 )/(freq.4 + V.4)]
png.5.train <- png.5.train[order(p.ls, decreasing = TRUE)]

png.4.train[png.3.train, on = 'grams.3', freq.3 := i.freq]
V.3 <- nrow(png.3.train)
png.4.train[, p.ls:=(freq + 1 )/(freq.3 + V.3)]
png.4.train <- png.4.train[order(p.ls, decreasing = TRUE)]

png.3.train[png.2.train, on = 'grams.2', freq.2 := i.freq]
V.2 <- nrow(png.2.train)
png.3.train[, p.ls:=(freq + 1 )/(freq.2 + V.2)]
png.3.train <- png.3.train[order(p.ls, decreasing = TRUE)]

png.2.train[png.1.train, on = 'grams.1', freq.1 := i.freq]
V.1 <- nrow(png.1.train)
png.2.train[, p.ls:=(freq + 1 )/(freq.1 + V.1)]
png.2.train <- png.2.train[order(p.ls, decreasing = TRUE)]

N.1 <- sapply(png.1.train$freq, sum)
png.1.train[, p.ls:=(freq + 1 )/(N.1 + V.1)]
png.1.train <- png.1.train[order(p.ls, decreasing = TRUE)]

## Good-Turing smoothing
gts <- function(c, N, k) {
    if (c > k) return(c)
    else return(((c+1)*(N[c+1] / N[c]) - (c*(k+1)*(N[k+1]/N[1]))) / (1 - ((k+1)*(N[k+1]/N[1]))))
}

GTS <- function(png, k) {
    n.occ <- table(png$freq)
    png$freq.gts <- sapply(png$freq, function(c) gts(c, n.occ, k))
    return(png)
}

# GTS <- function(png, k) {
#     N <- table(png[freq<=k+1]$freq)
#     png[freq<=k]$freq.gts <- sapply(png[freq<=k]$freq, function(c) gts(c, N, k))
#     png[freq>k, freq.gts:= freq]
#     return(png)
# }


k <- 5

png.1.train <- GTS(png.1.train, k)
png.2.train <- GTS(png.2.train, k)
png.3.train <- GTS(png.3.train, k)
png.4.train <- GTS(png.4.train, k)
png.5.train <- GTS(png.5.train, k)
png.6.train <- GTS(png.6.train, k)
gc()

## Laplace smoothing --> p.ls.gts
png.6.train[png.5.train, on = 'grams.5', freq.gts.5 := i.freq.gts]
V.5 <- nrow(png.5.train)
png.6.train[, p.gts.ls:=(freq.gts + 1 )/(freq.gts.5 + V.5)]
png.6.train <- png.6.train[order(p.gts.ls, decreasing = TRUE)]

png.5.train[png.4.train, on = 'grams.4', freq.gts.4 := i.freq.gts]
V.4 <- nrow(png.4.train)
png.5.train[, p.gts.ls:=(freq.gts + 1 )/(freq.gts.4 + V.4)]
png.5.train <- png.5.train[order(p.gts.ls, decreasing = TRUE)]

png.4.train[png.3.train, on = 'grams.3', freq.gts.3 := i.freq.gts]
V.3 <- nrow(png.3.train)
png.4.train[, p.gts.ls:=(freq.gts + 1 )/(freq.gts.3 + V.3)]
png.4.train <- png.4.train[order(p.gts.ls, decreasing = TRUE)]

png.3.train[png.2.train, on = 'grams.2', freq.gts.2 := i.freq.gts]
V.2 <- nrow(png.2.train)
png.3.train[, p.gts.ls:=(freq.gts + 1 )/(freq.gts.2 + V.2)]
png.3.train <- png.3.train[order(p.gts.ls, decreasing = TRUE)]

png.2.train[png.1.train, on = 'grams.1', freq.gts.1 := i.freq.gts]
V.1 <- nrow(png.1.train)
png.2.train[, p.gts.ls:=(freq.gts + 1 )/(freq.gts.1 + V.1)]
png.2.train <- png.2.train[order(p.gts.ls, decreasing = TRUE)]

N.1 <- sum(png.1.train$freq.gts)
png.1.train[, p.gts.ls:=(freq.gts + 1 )/(N.1 + V.1)]
png.1.train <- png.1.train[order(p.gts.ls, decreasing = TRUE)]


removeFirstWord <- function(s) {
    splt <- str_split(s, " ")[[1]]
    nbw <- length(splt)
    return(str_c(splt[2:nbw], collapse = " "))
}

ngram.predict <- function(input, png.6, png.5, png.4, png.3, png.2, png.1, nbrep, d = 0.3) {
    options(datatable.nomatch=0)
    res <- data.table()
    while (wordcount(input) > 5) input <- removeFirstWord(input)
    if (wordcount(input) == 5) {
        matches <- png.6[grams.5 == input]
        res.matches <- matches[1:(min(dim(matches)[1], nbrep))]
        res.matches <- res.matches[!is.na(res.matches$freq)]
        # if (nrow(res.matches) != 0) return(res.matches)
        # else input <- removeFirstWord(input)
        if (nrow(res.matches) != 0) res <- rbind(res, data.frame(n = rep(5, nrow(res.matches)),
                                            grams = res.matches$grams.0, p = res.matches$p.gts.ls))
        input <- removeFirstWord(input)
    }
    if (wordcount(input) == 4) {
        matches <- png.5[grams.4 == input]
        res.matches <- matches[1:(min(dim(matches)[1], nbrep))]
        res.matches <- res.matches[!is.na(res.matches$freq)]
        # if (nrow(res.matches) != 0) {
        #     res.matches$p.gts.ls <- d * res.matches$p.gts.ls
        #     return(res.matches)
        # }
        # else input <- removeFirstWord(input)
        if (nrow(res.matches) != 0) res <- rbind(res, data.frame(n = rep(4, nrow(res.matches)),
                                            grams = res.matches$grams.0, p = d * res.matches$p.gts.ls))
        input <- removeFirstWord(input)
    }
    if (wordcount(input) == 3) {
        matches <- png.4[grams.3 == input]
        res.matches <- matches[1:(min(dim(matches)[1], nbrep))]
        res.matches <- res.matches[!is.na(res.matches$freq)]
        # if (nrow(res.matches) != 0) {
        #     res.matches$p.gts.ls <- d^2 * res.matches$p.gts.ls
        #     return(res.matches)
        # } 
        # else input <- removeFirstWord(input)
        if (nrow(res.matches) != 0) res <- rbind(res, data.frame(n = rep(3, nrow(res.matches)),
                                            grams = res.matches$grams.0, p = d^2 * res.matches$p.gts.ls))
        input <- removeFirstWord(input)
    }
    if (wordcount(input) == 2) {
        matches <- png.3[grams.2 == input]
        res.matches <- matches[1:(min(dim(matches)[1], nbrep))]
        res.matches <- res.matches[!is.na(res.matches$freq)]
        # if (nrow(res.matches) != 0) {
        #     res.matches$p.gts.ls <- d^3 * res.matches$p.gts.ls
        #     return(res.matches)
        # } 
        # else input <- removeFirstWord(input)
        if (nrow(res.matches) != 0) res <- rbind(res, data.frame(n = rep(2, nrow(res.matches)),
                                                grams = res.matches$grams.0, p = d^3 * res.matches$p.gts.ls))
        input <- removeFirstWord(input)
    }
    if (wordcount(input) == 1) {
        matches <- png.2[grams.1 == input]
        res.matches <- matches[1:(min(dim(matches)[1], nbrep))]
        res.matches <- res.matches[!is.na(res.matches$freq)]
        # if (nrow(res.matches) != 0) {
        #     res.matches$p.gts.ls <- d^4 * res.matches$p.gts.ls
        #     return(res.matches)
        # } 
        if (nrow(res.matches) != 0) res <- rbind(res, data.frame(n = rep(1, nrow(res.matches)),
                                                grams = res.matches$grams.0, p = d^4 * res.matches$p.gts.ls))
        else {
            matches <- png.1
            res.matches <- matches[1:(min(dim(matches)[1], nbrep))]
            res <- rbind(res, data.frame(n = rep(0, nrow(res.matches)),
                                         grams = res.matches$grams.0, p = d^5 * res.matches$p.gts.ls))
            # res.matches$p.gts.ls <- d^5 * res.matches$p.gts.ls
            # return(res.matches)
        }
    }
    return(res[order(p, decreasing = TRUE)])
}

d <- 0.3 ## Discounting factor

input <- tolower("the same")
choices <- c("time", "problem", "about", "thing")
predict.time <- system.time(predictions <- ngram.predict(input, png.6.train, png.5.train, png.4.train, png.3.train,
                                            png.2.train, png.1.train, nbrep = 10, d = d))[3]
predictions
predictions[is.element(grams, choices)]


print(paste0("Find the next word for input : ", input))
print("First predictions : ")
lapply(predictions, function(p) {
    print(p[order(freq, decreasing = TRUE)])
})
print(paste0("Time for predicting : ", predict.time))

###############
testing <- sampleDS(testing.full, 500)
png.test <- pairedngrams(ds = testing, tokenisers = tokenisers, n = 6)
png.6.test = png.test[["ng.paired"]]
png.6.size.test = png.test[["ng.paired.size"]]
png.test <- pairedngrams(ds = testing, tokenisers = tokenisers, n = 5)
png.5.test = png.test[["ng.paired"]]
png.5.size.test = png.test[["ng.paired.size"]]
rm(png.test); gc()
###########################
########################## Testing / Perplexity

ngram.predict.prob <- function(input, png.6, png.5, png.4, png.3, png.2, png.1, d = 0.3, target) {
    options(datatable.nomatch=0)
    while (wordcount(input) > 5) input <- removeFirstWord(input)
    if (wordcount(input) == 5) {
        matches <- png.6[.(input, target)]
        # matches <- png.4[.(input, target), on=c("grams.3", "grams.0")]
        # matches <- png.4[grams.n == input & grams.0 == target]
        res.matches <- matches[1:(min(dim(matches)[1], 1))]
        res.matches <- res.matches[!is.na(res.matches$freq)]
        if (nrow(res.matches) != 0) return (list(target = target, prob = as.numeric(res.matches[1, "p.gts.ls"])))
        else input <- removeFirstWord(input)
    }
    if (wordcount(input) == 4) {
        matches <- png.5[.(input, target)]
        # matches <- png.4[.(input, target), on=c("grams.3", "grams.0")]
        # matches <- png.4[grams.n == input & grams.0 == target]
        res.matches <- matches[1:(min(dim(matches)[1], 1))]
        res.matches <- res.matches[!is.na(res.matches$freq)]
        if (nrow(res.matches) != 0) {
            res.matches$p.gts.ls <- d * res.matches$p.gts.ls
            return (list(target = target, prob = as.numeric(res.matches[1, "p.gts.ls"])))
        }
        else input <- removeFirstWord(input)
    }
    if (wordcount(input) == 3) {
        matches <- png.4[.(input, target)]
        # matches <- png.4[.(input, target), on=c("grams.3", "grams.0")]
        # matches <- png.4[grams.n == input & grams.0 == target]
        res.matches <- matches[1:(min(dim(matches)[1], 1))]
        res.matches <- res.matches[!is.na(res.matches$freq)]
        if (nrow(res.matches) != 0) {
            res.matches$p.gts.ls <- d^2 * res.matches$p.gts.ls
            return (list(target = target, prob = as.numeric(res.matches[1, "p.gts.ls"])))
        }
        else input <- removeFirstWord(input)
    }
    if (wordcount(input) == 2) {
        matches <- png.3[.(input, target)]
        # matches <- png.3[.(input, target), on=c("grams.2", "grams.0")]
        # matches <- png.3[grams.n == input & grams.0 == target]
        res.matches <- matches[1:(min(dim(matches)[1], 1))]
        res.matches <- res.matches[!is.na(res.matches$freq)]
        if (nrow(res.matches) != 0) {
            res.matches$p.gts.ls <- d^3 * res.matches$p.gts.ls
            return (list(target = target, prob = as.numeric(res.matches[1, "p.gts.ls"])))
        } 
        else input <- removeFirstWord(input)
    }
    if (wordcount(input) == 1) {
        matches <- png.2[.(input, target)]
        # matches <- png.2[.(input, target), on=c("grams.1", "grams.0")]
        # matches <- png.2[grams.n == input & grams.0 == target]
        res.matches <- matches[1:(min(dim(matches)[1], 1))]
        res.matches <- res.matches[!is.na(res.matches$freq)]
        if (nrow(res.matches) != 0) {
            res.matches$p.gts.ls <- d^4 * res.matches$p.gts.ls
            return (list(target = target, prob = as.numeric(res.matches[1, "p.gts.ls"])))
        } 
        else {
            matches <- png.1[.(target)]
            # matches <- png.1[.(target), on="grams.1"]
            # matches <- png.1[grams.n == target]
            res.matches <- matches[1:(min(dim(matches)[1], 1))]
            res.matches <- res.matches[!is.na(res.matches$freq)]
            if (nrow(res.matches) != 0) {
                res.matches$p.gts.ls <- d^5 * res.matches$p.gts.ls
                return (list(target = target, prob = as.numeric(res.matches[1, "p.gts.ls"])))
            }
            else {
                V.1 <- nrow(png.1)
                N.1 <- sum(png.1$freq.gts)
                return (list(target = target, prob = d^6 / (N.1 + V.1)))
            }
        }
    }
}

setkey(png.6.train, grams.5, grams.0)
setkey(png.5.train, grams.4, grams.0)
setkey(png.4.train, grams.3, grams.0)
setkey(png.3.train, grams.2, grams.0)
setkey(png.2.train, grams.1, grams.0)
setkey(png.1.train, grams.1)

input <- " about struggling but the"
choices <- c("defense", "referees", "crowd", "players")

ngram.predict.prob("helps reduce your", png.6.train, png.5.train, png.4.train, png.3.train, png.2.train, png.1.train,
                   d = d, "stress")

gram.prob.log <- function(gram, d) {
    return(log(ngram.predict.prob(gram["grams.4"], png.6.train, png.5.train, png.4.train, png.3.train, png.2.train,
                                  png.1.train, d = d, gram["grams.0"])))
}

predictionDF.log.6 <- apply(png.6.test, 1, gram.prob.log, d = d)

perplexity.6 <- exp((-1/nrow(png.6.test)*sum(predictionDF.log.6)))
# N=4, k = 5, d = 0.3, perplexity.4 = 229774.7
# N=4, k = 5, d = 0.6, perplexity.4 = 55507.79
# N=4, k = 5, d = 0.8, perplexity.4 = 55507.79
# N=4, k = 4, d = 0.8, perplexity.4 = 55301.25
# N=4, k = 4, d = 0.7, perplexity.4 = 40320.91
##############################################
# N=5, k = 5, d = 0.6, perplexity.5 = 40320.91
#   d        pp
# 1 0.3 728923.53
# 2 0.4 303177.35
# 3 0.5 153525.90
# 4 0.6  88049.78
# 5 0.7  55027.82
# 6 0.8  36622.08
# 7 0.9  25571.71
##############################################
# N = 6
# d         pp
# 1 0.3 746874.243
# 2 0.4 176017.865
# 3 0.5  57369.622
# 4 0.6  22954.950
# 5 0.7  10581.238
# 6 0.8   5409.855
# 7 0.9   2993.615

res <- data.frame()
for (d in seq(0.3, 0.9, by = 0.1)) {
    pp.log <- apply(png.6.test, 1, gram.prob.log, d = d)
    pp <- exp((-1/nrow(png.6.test)*sum(pp.log)))
    res <- rbind(res, data.frame(d = d, pp = pp))
}
res
