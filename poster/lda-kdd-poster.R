## ----setup, include=FALSE------------------------------------------------

## Visualize
library("lattice")
library("latticeExtra")
source("../_setup.R")
library("wordcloud")

## Manipulation
library("magrittr")
library("plyr")

## Text mining
library("SnowballC")
library("tm")

## Analysis
library("topicmodels")

## Reports
library("knitr")
library("xtable")
opts_chunk$set(
    warning = FALSE,
    message = FALSE,
    cache = TRUE,
    echo = FALSE,
    results = "hide",
    fig.width = 5,
    fig.height = 3,
    fig.align = "center",
    fig.pos = "H",
    dev.args = list(family = "Palatino"))

options(scipen = 9)

##-------------------------------------------
## Functions
higienize <- function(doc) {
    doc %>%
        tm_map(removeWords, stopwords("english")) %>%
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace) %>%
        tm_map(removeNumbers) %>%
        tm_map(stemDocument)
}

text2dtm <- function(text) {
    text %>%
    VectorSource %>%
    Corpus %>%
    higienize %>%
    DocumentTermMatrix
}


## ----read-data-----------------------------------------------------------

##----------------------------------------------------------------------
## Read and organize data

## Data long, each line is paper combine with topic (duplicate papers)
data_long <- read.table(
    file = "./data/papers.txt",
    header = TRUE,
    sep = ";",
    colClasses = c("factor", "character", "character")
)

## Data short, each line is unique paper
data_short <- data_long[!duplicated(data_long$title), -1]
data_short <- within(data_short, {
    ntopics = sapply(title, function(x) sum(x == data_long$title))
})

## Texts, using title and abstract texts
data_texts <- with(data_short, paste(title, abstract))

##----------------------------------------------------------------------
## Tranform to document-term matrix
dtm_texts <- text2dtm(data_texts)

##----------------------------------------------------------------------
## Word counts
mat_texts <- as.matrix(dtm_texts)
counts <- colSums(mat_texts)

##----------------------------------------------------------------------
## Frequences papers
freq_papers <- table(data_long$topic)
freq_topics <- table(paste(data_short$ntopics, "Tópico(s)"))


## ----descriptive, eval=FALSE---------------------------------------------
## 
## ##----------------------------------------------------------------------
## ## Simple descritive data
## 
## ## pdf("wordcloud-pie.pdf", width = 10, height = 5, family = "Palatino")
## paleta <- brewer.pal(9, "Greys")[-(1:4)]
## corte <- quantile(counts, probs = 0.95)
## wordcloud(words = names(counts)[counts > corte],
##           freq = counts[counts > corte],
##           min.freq = 5,
##           random.order = F,
##           colors = paleta,
##           family = "serif")
## ## dev.off()
## 
## xy1 <- barchart(prop.table(freq_topics),
##                 horizontal = FALSE,
##                 axis = axis.grid,
##                 ylab = NULL,
##                 scales = list(x = list(rot = 35)),
##                 border = "gray70",
##                 col = "gray90",
##                 ylim = extendrange(prop.table(freq_topics), f = 0.15),
##                 panel = function(x, y, subscripts, ...) {
##                     panel.barchart(x, y, ...)
##                     txt <- formatC(y*100, format = "f", digits = 1)
##                     panel.text(x, y, paste0(txt, "%"), srt = 90,
##                                adj = c(0.5, 0.5), cex = 0.7,
##                                font = "bold")
##                 })
## 
## xy2 <- barchart(sort(prop.table(freq_papers), decreasing = TRUE),
##                 horizontal = FALSE,
##                 axis = axis.grid,
##                 ylab = NULL,
##                 scales = list(x = list(rot = 35)),
##                 border = "gray70",
##                 col = "gray90",
##                 ylim = extendrange(prop.table(freq_papers), f = 0.15),
##                 panel = function(x, y, subscripts, ...) {
##                     panel.barchart(x, y, ...)
##                     txt <- formatC(y*100, format = "f", digits = 1)
##                     panel.text(x, y, paste0(txt, "%"), srt = 90,
##                                adj = c(0.5, 0.5), cex = 0.7,
##                                font = "bold")
##                 })
## 
## ## pdf("frequences.pdf", width = 4, height = 6, family = "Palatino")
## print(xy1, position = c(0, 0.55, 1, 1), more = TRUE)
## print(xy2, position = c(0, 0, 1, 0.65), more = FALSE)
## ## dev.off()
## 

## ----fitlda, eval=TRUE, cache=TRUE---------------------------------------

##----------------------------------------------------------------------
## Fit LDA model via Gibbs Sampler with ntopics of SIGKDD
tt_gibbs0 <-
    system.time(
        model_gibbs0 <- LDA(dtm_texts,
                            k = length(freq_papers),
                            method = "Gibbs",
                            control = list(
                                burnin = 1000,
                                thin = 10,
                                iter = 10000,
                                best = FALSE,
                                keep = TRUE
                                ))
    )

##----------------------------------------------------------------------
## Resamples of LDA model for choose the number of topics needed
B <- 100
tt_replic <- system.time(
    replicas <- lapply(
        X = 1:B,
        FUN = function(x) {
            ## Sample model
            model <- LDA(dtm_texts,
                         k = length(freq_papers),
                         method = "Gibbs",
                         control = list(
                             burnin = 1000,
                             thin = 10,
                             iter = 10000,
                             best = FALSE
                         ))
            ## Reduce parameters samples by mean
            list_gamma <- lapply(model@fitted, function(x) x@gamma)
            list_beta  <- lapply(model@fitted, function(x) exp(x@beta))
            par_gamma <- Reduce("+", list_gamma) / length(list_gamma)
            par_beta  <- Reduce("+", list_beta)  / length(list_beta)
            ## Choose number of topic by probs distance
            ddd <- dist(par_beta)
            hhh <- hclust(ddd, method = "ward.D2")
            par_ntopics <- with(hhh, {
                length(order) - which.max(diff(height))
            })
            ## Organize output
            out <- list(
                ## Don't keep probs matrices because (labelling problem)
                ## "gamma" = par_gamma,
                ## "beta" = par_beta,
                "ntopics" = par_ntopics
            )
        }
    )
)


## ----organize------------------------------------------------------------

##-------------------------------------------
## Results of simulations in gibbs0

## Reduce parameters samples by mean
list_gamma <- lapply(model_gibbs0@fitted, function(x) x@gamma)
list_beta <- lapply(model_gibbs0@fitted, function(x) exp(x@beta))
par_gamma <- Reduce("+", list_gamma)/length(list_gamma)
par_beta <- Reduce("+", list_beta)/length(list_beta)

## Verify composition of documents (being liberal)
pred_ntopic <- apply(par_gamma, 1,
                     function(x) sum(x >= 1/ncol(par_gamma)))

##-------------------------------------------
## Results of B iterations of LDA simulation to choose k

## The number of topics choosen
freq_ntopics <- table(unlist(replicas))
kchoosen <- names(which.max(freq_ntopics)) %>% as.integer


## ----fitlda1, cache=TRUE-------------------------------------------------

##----------------------------------------------------------------------
## Fit LDA with better K
tt_gibbs1 <-
    system.time(
        model_gibbs1 <- LDA(dtm_texts,
                            k = kchoosen,
                            method = "Gibbs",
                            control = list(
                                burnin = 1000,
                                thin = 10,
                                iter = 10000,
                                best = FALSE,
                                keep = TRUE
                                ))
    )

##-------------------------------------------
## Organize results

## Reduce parameters samples by mean
list_gamma1 <- lapply(model_gibbs1@fitted, function(x) x@gamma)
par_gamma1 <- Reduce("+", list_gamma1)/length(list_gamma1)

list_beta1 <- lapply(model_gibbs1@fitted, function(x) exp(x@beta))
par_beta1 <- Reduce("+", list_beta1)/length(list_beta1)

## Verify composition of documents (being liberal)
pred_ntopic1 <- apply(par_gamma1, 1,
                      function(x) sum(x >= 1/ncol(par_gamma1)))


## ----gammaprobs, fig.width=7.3, fig.height=3.1---------------------------

## Visualize the topics probabilities per document
levelplot(par_gamma,
          aspect = "fill",
          xlab = paste("Artigos (número de tópicos observado",
                       "é exibido acima)"),
          ylab = "Tópicos",
          scales = list(
              y = list(at = 1:ncol(par_gamma)),
              x = list(at = 1:nrow(par_gamma),
                       alternating = 3, cex = 0.7)
          ),
          par.settings = list(
              layout.heights = list(
                  axis.xlab.padding = -1
              )
          ),
          xscale.components = function(...) {
              ans <- xscale.components.default(...)
              ans$top <- ans$bottom
              ans$top$labels$labels <- data_short$ntopics
              tcktick <- rep(1, length(ans$top$ticks$at))
              tcktick[rep(c(FALSE, TRUE), length(tcktick)/2)] <- 2
              ans$top$ticks$tck <-tcktick
              ans$bottom$labels$labels <- NULL
              ans
          })


## ----kchoose, fig.width=5, fig.height=5----------------------------------

##-------------------------------------------
## Frequences of number of groups in B iteration LDA sampling
par(mar = c(0, 0, 0, 0))
with(as.data.frame(freq_ntopics), {
    fr <- paste0("(", round(100 * Freq / sum(Freq)), "%)")
    pie(Freq, labels = paste(Var1, fr),
        col = gray.colors(length(Freq)))
})
layout(1)


## ----tabwords, results="asis"--------------------------------------------

## Get the words with greater probability
nwords <- 5
tab_words <- apply(par_beta1, 1, function(x) {
    index <- order(x, decreasing = TRUE)[1:nwords]
    words <- colnames(mat_texts)[index]
    probs <- formatC(x[index], format = "f", digits = 3)
    paste0(words, " (", probs, ")")
})

## Get the words with greater probability
nwords <- 5
tab_words <- apply(par_beta1, 1, function(x) {
    index <- order(x, decreasing = TRUE)[1:nwords]
    words <- colnames(mat_texts)[index]
    probs <- formatC(x[index], format = "f", digits = 3)
    paste0(words, " (", probs, ")")
})

colnames(tab_words) <- paste("Tópico", seq(nrow(par_beta1)))
print(xtable(tab_words),
      include.rownames = FALSE,
      floating = FALSE,
      comment = FALSE)


