library(data.table)
library(xml2)
library(textplot)
library(udpipe)
x <- list.files("inst/train/data/Corpus Vosters 16R/", recursive = T, full.names = T)
x <- lapply(x, FUN=function(x){
  d <- read.delim(x, header = FALSE, col.names = c("nr", "id", "type", "jaar", "locatie", "locatie2", 
                                                   "locatie3", "locatie4", "prof", "wie", "achternaam", "voornaam", 
                                                   "content"), stringsAsFactors = FALSE, 
                  encoding = "UTF-8")
  d <- subset(d, nchar(trimws(content)) > 0)
  d <- subset(d, grepl(content, pattern = "</w>", ignore.case = TRUE))
  d$sentence_id <- seq_len(nrow(d))
  d$doc_id <- rep(basename(x), nrow(d))
  d
})
x <- data.table::rbindlist(x)
x$terms <- lapply(x$content, FUN=function(terms){
  terms <- xml_find_all(read_html(terms), "//w")
  terms <- lapply(terms, FUN=function(x){
    list(token = xml_text(x),
         lemma = xml_attr(x, "lemma"),
         xpos = xml_attr(x, "msd"))
  })
  terms <- rbindlist(terms)
  terms <- setDF(terms)
  terms
})
x <- setDT(x)
corpus <- as.data.frame(x)
corpus <- unique(corpus[, c("doc_id", setdiff(colnames(x), c("content", "terms", "sentence_id", "doc_id")))])

anno <- x[, as.data.frame(terms), by = list(doc_id, sentence_id)]
anno <- subset(anno, nchar(token) > 0)
anno$lemma <- ifelse(nchar(anno$lemma) == 0, NA_character_, anno$lemma)
anno$xpos  <- ifelse(nchar(anno$xpos) == 0, NA_character_, anno$xpos)
anno$upos <- strsplit(anno$xpos, ",|\\|")
anno$upos <- lapply(anno$upos, FUN=function(x) txt_recode(x, from = c("vnbw", "NONE", "uitdr", "znw(neper)", "tw", "QQ", "NN", "znw(neloc)", 
                                                                      "vnwbw", "telw", "bnw", "bw", "vw", 
                                                                      "lidw", "vnw", "vz", "ww", "znw"),
                                                          to = c("ADV", "X", "ADV", "PROPN", "INTJ", "X", "PUNCT", "PROPN", 
                                                                 "ADV", "NUM", "ADJ", "ADV", "CONJ", 
                                                                 "DET", "PRON", "ADP", "VERB", "NOUN")))
anno$upos <- sapply(anno$upos, FUN=function(x){
  if(length(x) == 0 || length(x) > 1) 
    return("X") 
  else 
    return(x)
})
## DATA CHECK on content which should not be in CONLLU files
## - spaces in token/lemma are not allowed in CONLLU
## - underscore in token/lemma and hashtags provided unexecpected behaviour allowed in CONLLU
table(unlist(regmatches(x = anno$token, gregexpr(pattern = "[[:space:]]", anno$token))))
table(unlist(regmatches(x = anno$lemma, gregexpr(pattern = "[[:space:]]", anno$lemma))))
unique(grep(pattern = "[[:space:]]", anno$token, value = TRUE))
unique(grep(pattern = "[[:space:]]", anno$lemma, value = TRUE))
unique(grep(pattern = "[[:punct:]]", anno$token, value = TRUE))
unique(grep(pattern = "[[:punct:]]", anno$lemma, value = TRUE))
## Remove data which is incorrect in CONLLU format + remove punctuations at the end of the token as this looks unexpected behavour in this corpus
anno$token <- gsub("[[:space:]]+", "", anno$token)
anno$lemma <- gsub("[[:space:]]+", "", anno$lemma)
anno$token <- gsub(pattern = "([[:punct:]])+$", replacement = "", anno$token)
anno$lemma <- gsub(pattern = "([[:punct:]])+$", replacement = "", anno$lemma)
table(unlist(regmatches(x = anno$token, gregexpr(pattern = "[[:space:]]", anno$token))))
table(unlist(regmatches(x = anno$lemma, gregexpr(pattern = "[[:space:]]", anno$lemma))))
table(unlist(regmatches(x = anno$token, gregexpr(pattern = "[[:punct:]]", anno$token))))
table(unlist(regmatches(x = anno$lemma, gregexpr(pattern = "[[:punct:]]", anno$lemma))))
unique(grep(pattern = "[[:space:]]", anno$token, value = TRUE))
unique(grep(pattern = "[[:space:]]", anno$lemma, value = TRUE))
unique(grep(pattern = "[[:punct:]]", anno$token, value = TRUE))
unique(grep(pattern = "[[:punct:]]", anno$lemma, value = TRUE))
anno$token <- ifelse(anno$token %in% "_", NA_character_, anno$token)
anno$lemma <- ifelse(anno$token %in% "_", NA_character_, anno$lemma)
## UDPipe does not like tokens with length longer than 256 characters
anno$token[which(nchar(anno$token) > 256)] <- NA_character_
anno$lemma[which(nchar(anno$lemma) > 256)] <- NA_character_
anno <- subset(anno, !is.na(token))
anno <- subset(anno, nchar(token) > 0)
## Set a token_id
anno <- anno[, token_id := seq_len(.N), by = list(doc_id)]
save(corpus, anno, file = "inst/train/data/corpus.RData")

textplot_bar(sort(table(corpus$jaar)), top = 20, 
             panel = "Jaar Corpus Vosters", cextext = 0.8, xlab = "Frequency")
textplot_bar(sort(table(anno$upos)), top = 20, 
             panel = "Tagset Corpus Vosters", cextext = 0.8, xlab = "Frequency")
textplot_bar(sort(table(unlist(strsplit(anno$xpos, ",|\\|")))), 
             panel = "Tagset Corpus Vosters", cextext = 0.8, xlab = "Frequency")
textplot_bar(sort(table(anno$lemma[anno$xpos %in% "znw"])), top = 20, 
             panel = "Jaar Corpus Vosters", cextext = 0.8, xlab = "Frequency")

cbind_phrases <- function(data){
  data$token_id <- as.character(data$token_id)
  x <- keywords_phrases(x = data$upos, term = data$token, pattern = "(PROPN){2,8}", is_regex = TRUE, sep = " ", ngram_max = 8, detailed = TRUE)
  x$exclude <- mapply(start = x$start, end = x$end, ngram = x$ngram, FUN=function(start, end, ngram, all_start, all_end, all_ngram){
    test <- (ngram < all_ngram) & (start >= all_start) & (end <= all_end)
    any(test)
  }, MoreArgs = list(all_start = x$start, all_end = x$end, all_ngram = x$ngram))
  x <- x[!x$exclude, ]
  x <- mapply(start = x$start, end = x$end, keyword = x$keyword, FUN=function(start, end, keyword){
    from <- data$token_id[start]
    to <- data$token_id[end]
    list(sentence_id = data$sentence_id[start], 
         token = keyword, 
         #lemma = data$lemma[start], 
         lemma = NA_character_, 
         #xpos = data$xpos[start], 
         xpos = NA_character_,
         #upos = data$upos[start], 
         upos = NA_character_,
         token_id = sprintf("%s-%s", from, to),
         mwe = TRUE)
  }, SIMPLIFY = FALSE)
  x <- rbindlist(x)
  data$mwe <- FALSE
  x <- rbindlist(list(compounds = x, x = data), use.names = TRUE, fill = TRUE)
  o <- sapply(strsplit(x$token_id, "-"), head, 1)
  x <- x[order(as.integer(o), decreasing = FALSE), ]
  x
}
anno$sentence_id <- 1L
anno <- anno[, cbind_phrases(data = .SD), by = list(doc_id)]
save(corpus, anno, file = "inst/train/data/corpus.RData")
load("inst/train/data/corpus.RData")
 

##
## Create training (70%) / test (15%) / dev (15%) dataset in conllu format
##
x <- setDF(anno)
x$sentence <- ""
set.seed(123456789)
docs <- unique(x$doc_id)
docs_training <- sample(docs, size = round(length(docs) * 0.7), replace = FALSE)
docs_dev      <- setdiff(docs, docs_training)
docs_test     <- sample(docs_dev, size = round(length(docs_dev) / 2))
docs_dev      <- setdiff(docs_dev, docs_test)
modeldata <- list()
modeldata$train <- subset(x, doc_id %in% docs_training)
modeldata$dev   <- subset(x, doc_id %in% docs_dev)
modeldata$test  <- subset(x, doc_id %in% docs_test)
modeldata <- rbindlist(modeldata, idcol = "type")
saveRDS(modeldata, file = "inst/train/data/modeldata.rds")
txt <- as_conllu(subset(modeldata, type == "train"))
cat(txt, file = file("inst/train/data/vosters-ud-train.conllu", encoding = "UTF-8"))
txt <- as_conllu(subset(modeldata, type == "dev"))
cat(txt, file = file("inst/train/data/vosters-ud-dev.conllu", encoding = "UTF-8"))
txt <- as_conllu(subset(modeldata, type == "test"))
cat(txt, file = file("inst/train/data/vosters-ud-test.conllu", encoding = "UTF-8"))



##### CHECKING DATA
# test <- test[, keywords_phrases(x = upos, term = token, pattern = "(PROPN){2,8}", is_regex = TRUE, sep = " ",
#                         ngram_max = 8, detailed = TRUE), by = list(doc_id)]
# test <- test[, exclude := mapply(start, end, ngram, FUN=function(start, end, ngram, all_start, all_end, all_ngram){
#   test <- (ngram < all_ngram) & (start >= all_start) & (end <= all_end)
#   any(test)
# }, MoreArgs = list(all_start = start, all_end = end, all_ngram = ngram)), by = list(doc_id)]
# test <- subset(test, !exclude)
# anno <- setDT(anno)
# test <- anno[, list(n = length(unique(token))), by = list(lemma)]
# test <- test[order(test$n, decreasing = TRUE), ]
# head(test)
# test <- anno[, list(n = length(unique(lemma))), by = list(token)]
# test <- test[order(test$n, decreasing = TRUE), ]
# head(test)
# test <- anno[lemma %in% "zijn", list(n = (unique(token))), by = list(lemma)]
