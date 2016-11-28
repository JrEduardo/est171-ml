## ----setup, include = FALSE----------------------------------------------

library("rmarkdown")
library("knitr")
library("pander")
source("../_setup.R")   ## Apenas customização de gráficos lattice
opts_chunk$set(
    cache = TRUE,
    echo = FALSE,
    fig.pos = "H")

options(xtable.caption.placement = "top",
        xtable.include.rownames = FALSE,
        xtable.comment = FALSE)

##----------------------------------------------------------------------
## Pacotes e funções utéis
##----------------------------------------------------------------------

library("magrittr")
library("plyr")

library("lattice")
library("latticeExtra")
library("wordcloud")
library("dendextend")
library("arulesViz")

library("SnowballC")
library("tm")
library("rslp")
library("clusterSim")
library("arules")
library("kernlab")

## Higieniza, Radicaliza e transforma para matriz binária de ocorrências
## textos
text2matrix <- function(text, lang = "portuguese",
                        stemFun = stemDocument,
                        completation = FALSE) {
    ## Create Corpus
    general <- Corpus(x = VectorSource(text),
                      readerControl = list(language = lang))
    ## Higienize and stem textDoc
    higieni <- general %>%
        tm_map(removeWords, stopwords(lang)) %>%
        tm_map(content_transformer(rslp:::remove_accents)) %>%
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace) %>%
        tm_map(removeNumbers)
    docstem <- tm_map(higieni, stemFun)
    ## Document Term Matrix
    docmat <- DocumentTermMatrix(
        docstem, control = list(weighting = weightBin))
    ## Transform to matrix
    mat <- as.matrix(docmat)
    if (completation) {
        colnames(mat) <- stemCompletion(
            colnames(mat),
            dictionary = Terms(TermDocumentMatrix(higieni))
        )
    }
    return(mat)
}

## Função para extrair estatísticas resumo de dados numéricos
mysummary <- function(x, ...) {
    funs <- list(
        "n" = length,
        "mean" = mean,
        "median" = median,
        "1st.Quartil" = function(x) fivenum(x)[2],
        "3st.Quartil" = function(x) fivenum(x)[4],
        "Std.Dev" = sd
    )
    out <- sapply(funs, function(f) f(x))
    return(out)
}

## Identificação de outlies pela maior distancia do ponto central
outlierid <- function(dados, centerfun = median, n = 5, ...) {
    centro <- apply(dados, 2, centerfun, ...)
    dists  <- apply(dados, 1, function(x) sqrt(sum((x - centro)^2)))
    ident <- order(dists, decreasing = TRUE)[1:n]
    attr(ident, "dist") <- dists[ident]
    return(ident)
}

##----------------------------------------------------------------------


## ----read----------------------------------------------------------------

##----------------------------------------------------------------------
## Lendo os dados
##----------------------------------------------------------------------
load("./data/dadosReviewGoogle.RData")
notas <- nota
rm(nota)

##----------------------------------------------------------------------
## Selecionando uma amostra
##----------------------------------------------------------------------
set.seed(5453)
index <- sample(which(!is.na(textos)), 200)
notas <- notas[index]
textos <- textos[index]

##-------------------------------------------
## Extrai os primeiros comentários com notas de 1 a 5
da <- 1:5 %>%
    lapply(FUN = function(x) {
        i <- which(notas == x)[1]
        data.frame(texto = textos[i], nota = notas[i])
    })  %>%
    ldply


## ----body1, results="asis"-----------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(da),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----t2m, cache=TRUE-----------------------------------------------------

##----------------------------------------------------------------------
## Transformando para matrix binária
##----------------------------------------------------------------------

## Radicalizando via algoritmo de Porter
## <http://snowball.tartarus.org/algorithms/portuguese/stemmer.html>
mPTER <- text2matrix(textos, stemFun = stemDocument)

## Radicalizando via algoritmo RSLP (Removedor de Sufixos Lingua PT)
## <http://www.inf.ufrgs.br/~viviane/rslp/>
mRSLP <- text2matrix(textos, stemFun = content_transformer(rslp_doc))


## ----meds, results="asis"------------------------------------------------

## Explorando as matrizes
lmats <- list("PTER" = mPTER, "RSLP" = mRSLP)
counts <- lapply(lmats, function(x) apply(x, 2, sum))
tabs <- do.call(rbind, lapply(counts, mysummary))

colnames(tabs) <- c("Nº radicais", "Média", "Mediana",
                    "1º Quartil", "3º Quartil", "Desvio Padrão")

capt <- paste("Estatísticas do número de radicais extraídos por cada",
              "algoritmo de radicalização")

print(xtable(tabs, caption = capt, label = "tab:rad",
             align = "ccccccc"), include.rownames = TRUE)


## ----rad, fig.height=4.5, fig.width=10, fig.cap="Número de ocorrências das palavras nas 200 resenhas. Frequências das ocorrências mais frequentes (esquerda) e Densidade empírica do número de ocorrências maiores que 2 (direita)."----

## Organizando em data.frames
da.counts <- data.frame(
    counts = unlist(counts),
    algorithm = rep(names(lmats), sapply(counts, length))
)
da.freq <- ldply(lapply(counts, function(x) {
    i <- 1:15
    cbind("x" = i, "f" = table(x)[i])
}))

## Densidade do log das contagens de ocorrencias de cada palavra
xy1 <- densityplot(~counts,
                   subset = counts >= 2,
                   groups = algorithm,
                   data = da.counts,
                   axis = axis.grid,
                   auto.key = list(
                       text = names(counts),
                       font = "bold",
                       corner = c(0.9, 0.9),
                       title = "Algoritmo",
                       cex.title = 1,
                       cex = 0.8
                   ))

## Frequencia das 15 palavras que ocorrem mais vezes em cada texto
cols <- trellis.par.get("superpose.symbol")$col
xy2 <- barchart(x ~ f, groups = .id,
                data = da.freq,
                axis = axis.grid,
                xlab = "Frequência absoluta",
                ylab = "Ocorrência da palavra em todos os reviews",
                col = cols[1:2],
                key = list(
                    corner = c(0.9, 0.9),
                    title = "Algoritmo",
                    cex.title = 1,
                    rectangles = list(col = cols[1:2]),
                    text = list(names(counts), font = "bold"),
                    cex = 0.8
                ),
                prepanel = function(x) {
                    x <- as.numeric(x)
                    list(xlim = c(0, max(x) * 1.05))
                },
                panel = function(x, y, groups, ...) {
                    panel.barchart(x, y, groups = groups, ...)
                    nbars <- length(groups)/length(unique(groups))
                    spread <- rep(c(-0.15, 0.15), nbars)
                    panel.text(x, y + spread, x, groups = groups,
                               cex = 0.9, adj = 0, font = "bold")
                })

print(xy2, split = c(1, 1, 2, 1), more = TRUE)
print(xy1, split = c(2, 1, 2, 1), more = FALSE)


## ----nuvemrad, fig.height=5, fig.width=10, results="hide", fig.cap="Nuvens de palavras* resultantes de cada algoritmo de radicalização."----

## Nuvem de palavras
par(mfrow = c(1, 2))
lapply(1:2, function(i) {
    wordcloud(names(counts[[i]]), counts[[i]], rot.per = 0.5,
              min.freq = 4, color = cols[i])
    title(paste("Algoritmo", names(counts)[i]), line = -2)
})
layout(1)


## ----showmPTER, fig.heigth=4.5, fig.width=11, fig.cap="Visualização da matriz documento-termo binária. Regiões de preenchimento em preto representam a ocorrência do termo no documento. Matriz completa (direita) apenas os termos 3\\% mais frequentes."----

## Visualizando a matriz binária
## Completa
lv1 <- levelplot(mPTER,
                 scales = list(y = list(draw = FALSE)),
                 aspect = "fill",
                 colorkey = FALSE,
                 xlab = "Resenhas",
                 ylab = "Palavras")
## Apenas 2% das palavras mais utilizadas
index <- counts[["PTER"]] > quantile(counts[["PTER"]], 0.97)
lv2 <- levelplot(mPTER[, index],
                 aspect = "fill",
                 colorkey = FALSE,
                 xlab = "Resenhas",
                 ylab = NULL)

print(lv1, split = c(1, 1, 2, 1), more = TRUE)
print(lv2, split = c(2, 1, 2, 1), more = FALSE)


## ----fitkmeans, cache=TRUE-----------------------------------------------

##-------------------------------------------
## K-means clustering
algs <- c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")
names(algs) <- algs
lkms <- lapply(algs, function(a) {
    k <- 2:15
    names(k) <- paste0("k", k)
    lapply(k, function(k)
        kmeans(x = mPTER, centers = k, iter.max = 50,
               nstart = 10, algorithm = a))
})
meds <- lapply(lkms, function(a) {
    m <- t(sapply(a, function(x) {
        c("intra" = x$tot.withinss, "entre" = x$betweens)
    }))
    k <- 1:length(a) + 1
    cbind(m, k)
})


## ----medskmeans, fig.height=4, fig.cap="Distâncias intra e entre clusters para cada algoritmo definido com k clusters"----

## Escolha do k com base em distâncias ao quadrado
da.meds <- ldply(meds)
xy1 <- xyplot(intra ~ k | .id, data = da.meds, type = "o", cex = 0.7,
              ylab = "Distância intra-clusters",
              layout = c(4, NA),
              scales = list(x = list(at = unique(da.meds$k))))
xy2 <- xyplot(entre ~ k | .id, data = da.meds, type = "o", cex = 0.7,
              ylab = "Distâncias entre-clusters",
              layout = c(4, NA))
doubleYScale(xy1, xy2, add.ylab2 = TRUE,
             text = c("Dentro dos clusters", "Entre os cluters"),
             title = "Soma das distâncias ao quadrado das observações",
             cex.title = 1.1,
             points = TRUE,
             column = 2) +
    layer(panel.abline(v = da.meds$k, col = "gray90", lty = 2))


## ----gap, cache=TRUE-----------------------------------------------------

## Critério de GAP Para escolha do k
## <https://github.com/cran/clusterSim/blob/master/inst/doc/IndexGap_details.pdf>
## <http://onlinelibrary.wiley.com/doi/10.1111/1467-9868.00293/abstract>
cgaps <- lapply(lkms, function(fits) {
    len <- length(fits) - 1
    mat <- matrix(nrow = len, ncol = 2)
    for (u in 1:len) {
        cls <- cbind(fits[[u]]$cluster, fits[[u + 1]]$cluster)
        gap <- index.Gap(mPTER, cls, B = 10, method = "k-means")
        mat[u, ] = do.call("c", gap)
    }
    colnames(mat) <- names(gap)
    cbind(k = 1:len + 1, mat)
})


## ----diffu, fig.height=4, fig.cap="Diferenças de índices GAP para os diferentes algoritmos."----

da.gaps <- ldply(cgaps)
xyplot(diffu ~ k | .id,
       data = da.gaps,
       type = c("g", "p"),
       pch = 19,
       layout = c(4, NA),
       panel = function(x, y, subscripts, ... ) {
           cols <- rep("gray60", length(y))
           cols[y > 0] <- 1
           panel.xyplot(x, y, col = cols, ...)
           panel.abline(h = 0, lty = 2, col = 1)
       })

## Escolha dos k's com base em Tibishirani 2001
ksel <- with(subset(da.gaps, diffu > 0), {
    tapply(k, .id, min)
})


## ----medsgap, results="asis"---------------------------------------------

## Soma de quadrados para os k selecionados
da.sqs <- subset(da.meds, k %in% ksel)
xts <- xtabs(cbind(intra, entre) ~ .id + k, data = da.sqs)

names(da.sqs) <- c("Algoritmo", "Dist. intra", "Dist. entre", "k")
da.sqs <- da.sqs[order(da.sqs$k), c(1, 4, 2, 3)]

capt <- "Distâncias intra e entre clusters para os k's indicados"
print(xtable(da.sqs, caption = capt, label = "tab:medsgap",
             align = "llccc"))


## ----fithclust, cache=TRUE-----------------------------------------------

##-------------------------------------------
## Hierarquical clustering
mets <- c(## "mcquitty", "median", "centroid",
    "ward.D2", "single", "complete", "average")
## Sobre o método Ward
## <http://adn.biol.umontreal.ca/~numericalecology/Reprints/Murtagh_Legendre_J_Class_2014.pdf>
names(mets) <- mets
lhcl <- lapply(mets, function(m) {
    ## Pode-se testar diferentes distâncias
    d <- dist(mPTER, method = "euclidean")
    hclust(d, method = m)
})


## ----dends, results="hide", fig.cap="Dendogramas referentes aos agrupamentos hierárquicos com diferentes tipos de ligação."----

par(mfrow = c(2, 2), mar = c(2, 3, 2, 1) + 0.1)
ks <- c(2, rep(1, length(lhcl) - 1))
cols <- trellis.par.get("superpose.line")$col
lapply(1:length(lhcl), function(i) {
    fit <- lhcl[[i]]
    graph <- as.dendrogram(fit)
    graph <- color_branches(graph, k = ks[i], col = cols[1:ks[i]])
    graph <- set(graph, "labels_cex", 0.8)
    plot(graph, horiz = FALSE, xlab = "Distância da ligação")
    title(main = names(lhcl)[i])
})
layout(1)


## ------------------------------------------------------------------------

##-------------------------------------------
## Comparando as abordagens hierárquicas e via hclust para k = 2
## K-means utilizado com método Lloyd
kmscl <- lkms[["Hartigan-Wong"]][["k2"]]$cluster
hclcl <- cutree(lhcl[["ward.D2"]], k = 2)
attr(kmscl, "agrup") <- "K-means"
attr(hclcl, "agrup") <- "H-clust"

## Organizando em lista
lcls <- list("K-means" = kmscl, "H-clust" = hclcl)

## Número de observações em cada cluster
nocs <- sapply(lcls, table)


## ----nuvens, fig.height=5, fig.width=5, results="hide", fig.cap="Nuvens de palavras* mais frequentes em cada cluster definido por cada método."----

## Nuvem de palavras em cada cluster
par(mfrow = c(2, 2))
lapply(lcls, function(x) {
    for (i in unique(x)) {
        mat <- mPTER[x == i, ]
        cnt <- apply(mat, 2, sum)
        wordcloud(names(cnt), cnt, rot.per = 0.3, min.freq = 3,
                  random.order = FALSE, col = rgb(0, 0, 0, 0.7))
        title(main = paste0(i, "º Cluster\n", attr(x, "agrup")),
              line = 1, col = cols[2])
    }
})
layout(1)


## ----showgps-------------------------------------------------------------

## Vendo alguns comentários originais de cada cluster
revws <- lapply(lcls, function(x) {
    metodo <- attr(x, "agrup")
    sapply(unique(x), function(i) {
        sample(textos[x == i], 2)
    })
})

da.revws <- ldply(revws, .id = "Método")
da.revws$Cluster <- rep(1:2, 2)
names(da.revws)[2:3] <- paste("Amostra de texto", 1:2)
da.revws <- da.revws[, c(1, 4, 2, 3)]


## ----body2, results="asis"-----------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(da.revws),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----fitarules, cache=TRUE, results="hide"-------------------------------

##----------------------------------------------------------------------
## Regras de Associação
##----------------------------------------------------------------------

## transforma a matrix para matrix de transações
trans <- as(mPTER, "transactions")

## Cria as regras
params <- list(
    support    = 0.04,  ## Mínimo support das regras
    confidence = 0.60   ## Mínima confiança das regras
)
rules <- apriori(trans, parameter = params)
da.rules <- as(rules, "data.frame")


## ------------------------------------------------------------------------

ll.rules <- lapply(da.rules[, -1], function(x) {
    index <- order(x, decreasing = TRUE)[1:5]
    da.rules[index, ]
})

## Escolhendo a regra 18 para exemplificar
regr <- da.rules[18, "rules"]
supp <- da.rules[18, "support"]
conf <- da.rules[18, "confidence"]
lift <- da.rules[18, "lift"]


## ----srules1, results="asis"---------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(ll.rules[["support"]]),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----srules2, results="asis"---------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(ll.rules[["confidence"]]),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----srules3, results="asis"---------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(ll.rules[["lift"]]),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----rulesgraf, fig.height=5, fig.width=6, fig.show="hide"---------------

## Visualizando as regras de associação
## <https://www.jstatsoft.org/article/view/v014i15>
fillcol <- with(da.rules, {
    x <- (lift - min(lift)) / (max(lift) - min(lift))
    x <- (x + 0.5); x <- x / max(x)
    rgb(0.1, 0.1, 0.1, x)
})
colorfun <- colorRampPalette(c("gray70",  "gray40", "gray10"))

exprlift <- expression("Lift" == P("RHS"~"|"~"LHS")~"/"~P("RHS"))
exprconf <- expression("Confiança" == P("RHS"~"|"~"LHS"))
exprsupp <- expression("Support" == P("RHS"*","~"LHS"))

xyplot(confidence ~ support,
       data = da.rules,
       type = c("g", "p"),
       fill = fillcol,
       pch = 21, cex = 2,
       main = list(exprlift, font = 1),
       xlab = exprsupp,
       ylab = exprconf,
       legend = list(
           top = list(
               fun = draw.colorkey,
               args = list(
                   key = list(
                       space = "bottom",
                       col = colorfun(50),
                       at = do.breaks(range(da.rules$lift), 50),
                       draw = FALSE))))
       )

## Grafos representando as associações
plot(rules, method = "graph",
  control = list(
      main = NULL,
      edgeCol = "black",
      nodeCol = rev(colorfun(10)),
      layout = igraph::with_lgl())
  )


## Grouped matrix plot
plot(rules, method = "grouped",
     control = list(
         main = NULL,
         col = rev(colorfun(10)),
         gp_labels = gpar(cex = 0.65),
         gp_labs = gpar(cex = 0.8, fontface = "bold")
     ))

## For show path of figures
path <- opts_current$get()$fig.path
label <- opts_current$get()$label
figs <- paste0(path, label, "-", 1:3)


## ----fitpca, cache=TRUE--------------------------------------------------

##----------------------------------------------------------------------
## Redução de dimensionalidade com PCA
cnames <- colnames(mPTER)

## Trabalhando matricialmente para compreender o método
Vcor <- cov(mPTER)
deco <- eigen(Vcor, symmetric = TRUE)
li <- deco$values
ei <- deco$vectors
pvar <- li/sum(li)

##-------------------------------------------
## Considerando menores
load3me <- apply(ei[, 1:3], 2, function(x) {
    index <- order(abs(x), decreasing = FALSE)[1:5]
    data.frame("word" = cnames[index], "loading" = x[index])
    ## names(out) <- cnames[index]
    ## out
})
da.load3me <- do.call(cbind, load3me)

##-------------------------------------------
## Considerando maiores
load3ma <- apply(ei[, 1:3], 2, function(x) {
    index <- order(abs(x), decreasing = TRUE)[1:5]
    data.frame("word" = cnames[index], "loading" = x[index])
    ## names(out) <- cnames[index]
    ## out
})
da.load3ma <- do.call(cbind, load3ma)


## ----pca2d, results="asis"-----------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(da.load3me, digits = -1),
      only.contents = TRUE)


## ----pca3d, results="asis"-----------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(da.load3ma, digits = 3),
      only.contents = TRUE)


## ----pcagraf, fig.width=10, results="hide", fig.cap="Dispersão das componentes com a indicação das notas atrucuídas a cada texto."----

## Calculando as componentes (escores)
scores2 <- mPTER %*% ei[, 1:2]
scores2 <- as.data.frame(scores2)
names(scores2) <- paste0("PC", 1:2)
xylabs2 <- paste0(names(scores2), " (", round(pvar[1:2], 3) * 100, "%)")

## Identifica os outliers
id2 <- outlierid(scores2, n = 3)

## Gráfico das componentes
xy2 <- xyplot(PC2 ~ PC1,
              groups = notas,
              data = scores2,
              type = c("g", "p"),
              xlab = xylabs2[1],
              ylab = xylabs2[2],
              pch = 21, cex = 1.4,
              id = id2,
              panel = function(x, y, id, ...) {
                  panel.xyplot(x, y, fill = "white", ...)
                  panel.xyplot(x[id], y[id], fill = "gray60", ...)
              },
              par.settings = list(
                    layout.heights = list(
                        top.padding = 5
                    )
              ))

## Calculando as componentes (escores)
scores3 <- mPTER %*% ei[, 1:3]
scores3 <- as.data.frame(scores3)
names(scores3) <- paste0("PC", 1:3)
xylabs3 <- paste0(names(scores3), " (", round(pvar[1:3], 3) * 100, "%)")

## Identifica os outliers
id3 <- outlierid(scores3, n = 3)

xy3 <- cloud(PC3 ~ PC1 * PC2,
             data = scores3,
             groups = notas,
             screen = list(z = 30, x = -80, y = 5),
             xlab = list(xylabs3[1], rot = 15),
             ylab = list(xylabs3[2], rot = -28),
             zlab = list(xylabs3[3], rot = 90),
             scales = list(arrows = FALSE),
             pch = 21, cex = 1.4, id = id3,
             panel = function(x, y, z, id, ...) {
                 grps <- list(...)$groups
                 panel.cloud(x = x, y = y, z = z, fill = 0, ...)
                 panel.cloud(x = x[id], y = y[id], z = z[id],
                             fill = "gray60", ...)
             },
             par.settings = list(
                    layout.heights = list(
                        top.padding = 5,
                        key.axis.padding = 4,
                        bottom.padding = 6.5
                    )))

## Legenda para os gráficos
key <- list(
    space = "top",
    title = "Notas",
    cex.title = 1.1,
    column = 5,
    points = list(
        pch = 21, fill = "white",
        col = trellis.par.get("superpose.symbol")$col[1:5]
    ),
    text = list(
        paste0(1:5, " (",
               prop.table(table(notas)) * 100, "%)")
    )
)

##-------------------------------------------
## Visualização
print(xy2, split = c(1, 1, 2, 1), more = TRUE)
print(xy3, split = c(2, 1, 2, 1), more = FALSE)
draw.key(key = key, draw = TRUE,
         vp = grid::viewport(
             x = grid::unit(0.5, "npc"),
             y = grid::unit(0.95, "npc")))


## ----tpca2d, results="asis"----------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(data.frame(id2, attr(id2, "dist"), textos[id2])),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----tpca3d, results="asis"----------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(data.frame(id3, attr(id3, "dist"), textos[id3])),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----fitkpca, cache=TRUE-------------------------------------------------

##----------------------------------------------------------------------
## Kernel PCA
##----------------------------------------------------------------------

##-------------------------------------------
## Kernel Gaussiano exp{-sigma*||x-x'||^2}
##-------------------------------------------

## 2 Componentes
gpca2 <- kpca(mPTER, kernel = "rbfdot", features = 2,
              kpar = list(sigma = 0.3))
## Organizando as componentes em data.frame
gscores2 <- gpca2@rotated
gscores2 <- as.data.frame(gscores2)
names(gscores2) <- paste0("KPC", 1:2)
gxylabs2 <- parse(text = paste0(names(gscores2), "~(lambda == ",
                                round(eig(gpca2), 3), ")"))
## Identifica os outliers
gid2 <- outlierid(gscores2, n = 3)

## 3 Componentes
gpca3 <- kpca(mPTER, kernel = "rbfdot", features = 3,
              kpar = list(sigma = 0.3))
## Organizando as componentes em data.frame
gscores3 <- gpca3@rotated
gscores3 <- as.data.frame(gscores3)
names(gscores3) <- paste0("KPC", 1:3)
gxylabs3 <- parse(text = paste0(names(gscores3), "~(lambda == ",
                                round(eig(gpca3), 3), ")"))
## Identifica os outliers
gid3 <- outlierid(gscores3, n = 3)

##-------------------------------------------
## Kernel Polynomial (scale * <x, x'> + offset)^degree
##-------------------------------------------

## 2 Componentes
ppca2 <- kpca(mPTER, kernel = "polydot", features = 2,
              kpar = list(degree = 1.2, scale = 1, offset = 1))
## Organizando as componentes em data.frame
pscores2 <- ppca2@rotated
pscores2 <- as.data.frame(pscores2)
names(pscores2) <- paste0("KPC", 1:2)
pxylabs2 <- parse(text = paste0(names(pscores2), "~(lambda == ",
                                round(eig(ppca2), 3), ")"))
## Identifica os outliers
pid2 <- outlierid(pscores2, n = 3)

## 3 Componentes
ppca3 <- kpca(mPTER, kernel = "polydot", features = 3,
              kpar = list(degree = 1.2, scale = 1, offset = 1))
## Organizando as componentes em data.frame
pscores3 <- ppca3@rotated
pscores3 <- as.data.frame(pscores3)
names(pscores3) <- paste0("KPC", 1:3)
pxylabs3 <- parse(text = paste0(names(pscores3), "~(lambda == ",
                                round(eig(ppca3), 3), ")"))
## Identifica os outliers
pid3 <- outlierid(pscores3, n = 3)


## ----gkpcagraf, fig.width=10, results="hide", fig.cap="Dispersão das kernel componentes gaussianas com a indicação das notas atribuídas a cada texto."----

## Gráfico das componentes
xy2 <- xyplot(KPC2 ~ KPC1,
              groups = notas,
              data = gscores2,
              type = c("g", "p"),
              xlab = gxylabs2[1],
              ylab = gxylabs2[2],
              pch = 21, cex = 1.2,
              id = gid2,
              panel = function(x, y, id, ...) {
                  panel.xyplot(x, y, fill = "white", ...)
                  panel.xyplot(x[id], y[id], fill = "gray60", ...)
              },
              par.settings = list(
                  layout.heights = list(
                      top.padding = 5
                  )
              ))

## Gráfico das componentes
xy3 <- cloud(KPC3 ~ KPC1 * KPC2,
             data = gscores3,
             groups = notas,
             screen = list(z = 15, x = -80, y = 5),
             xlab = list(gxylabs3[1], rot = 5),
             ylab = list(gxylabs3[2], rot = -50),
             zlab = list(gxylabs3[3], rot = 90),
             scales = list(arrows = FALSE),
             pch = 21, cex = 1.2, id = gid3,
             panel = function(x, y, z, id, ...) {
                 grps <- list(...)$groups
                 panel.cloud(x = x, y = y, z = z, fill = 0, ...)
                 panel.cloud(x = x[id], y = y[id], z = z[id],
                             fill = "gray60", ...)
             },
             par.settings = list(
                 layout.heights = list(
                     top.padding = 5,
                     key.axis.padding = 4,
                     bottom.padding = 6.5
                 )))

##-------------------------------------------
## Visualização
print(xy2, split = c(1, 1, 2, 1), more = TRUE)
print(xy3, split = c(2, 1, 2, 1), more = FALSE)
draw.key(key = key, draw = TRUE,
         vp = grid::viewport(
             x = grid::unit(0.5, "npc"),
             y = grid::unit(0.95, "npc")))


## ----pkpcagraf, fig.width=10, results="hide", fig.cap="Dispersão das kernel componentes polinomiais com a indicação das notas atribuídas a cada texto"----

## Gráfico das componentes
xy2 <- xyplot(KPC2 ~ KPC1,
              groups = notas,
              data = pscores2,
              type = c("g", "p"),
              xlab = pxylabs2[1],
              ylab = pxylabs2[2],
              pch = 21, cex = 1.2,
              id = pid2,
              panel = function(x, y, id, ...) {
                  panel.xyplot(x, y, fill = "white", ...)
                  panel.xyplot(x[id], y[id], fill = "gray60", ...)
              },
              par.settings = list(
                  layout.heights = list(
                      top.padding = 5
                  )
              ))

## Gráfico das componentes
xy3 <- cloud(KPC3 ~ KPC1 * KPC2,
             data = pscores3,
             groups = notas,
             screen = list(z = 30, x = -80, y = 5),
             xlab = list(pxylabs3[1], rot = 15),
             ylab = list(pxylabs3[2], rot = -28),
             zlab = list(pxylabs3[3], rot = 90),
             scales = list(arrows = FALSE),
             pch = 21, cex = 1.2, id = pid3,
             panel = function(x, y, z, id, ...) {
                 grps <- list(...)$groups
                 panel.cloud(x = x, y = y, z = z, fill = 0, ...)
                 panel.cloud(x = x[id], y = y[id], z = z[id],
                             fill = "gray60", ...)
             },
             par.settings = list(
                 layout.heights = list(
                     top.padding = 5,
                     key.axis.padding = 4,
                     bottom.padding = 6.5
                 )))

##-------------------------------------------
## Visualização
print(xy2, split = c(1, 1, 2, 1), more = TRUE)
print(xy3, split = c(2, 1, 2, 1), more = FALSE)
draw.key(key = key, draw = TRUE,
         vp = grid::viewport(
             x = grid::unit(0.5, "npc"),
             y = grid::unit(0.95, "npc")))


## ----data2---------------------------------------------------------------

data("IncomeESL")

## Remover casos com missing
IncomeESL <- IncomeESL[complete.cases(IncomeESL), ]

## Preparar os dados
IncomeESL[["income"]] <- factor((
    as.numeric(IncomeESL[["income"]]) > 6) + 1,
    levels = 1:2 , labels = c("$0-$40,000", "$40,000+"))
IncomeESL[["age"]] <- factor((
    as.numeric(IncomeESL[["age"]]) > 3) + 1,
    levels = 1:2 , labels = c("14-34", "35+"))
IncomeESL[["education"]] <- factor((
    as.numeric(IncomeESL[["education"]]) > 4) + 1,
    levels = 1:2 , labels = c("no college graduate", "college graduate"))
IncomeESL[["years in bay area"]] <- factor((
    as.numeric(IncomeESL[["years in bay area"]]) > 4) + 1,
    levels = 1:2 , labels = c("1-9", "10+"))
IncomeESL[["number in household"]] <- factor((
    as.numeric(IncomeESL[["number in household"]]) > 3) + 1,
    levels = 1 : 2 , labels = c("1", "2+"))
IncomeESL[["number of children"]] <- factor((
    as.numeric(IncomeESL[["number of children"]]) > 1) + 0,
    levels = 0 : 1 , labels = c("0", "1+"))
##  criando transactions
Income <- as(IncomeESL, "transactions")


## ----fitapriori1, results="hide"-----------------------------------------

##----------------------------------------------------------------------
## Minerando as regras de associação com restrições enunciadas
params <- list(
    support    = 0.001,  ## Mínimo support das regras
    confidence = 0.800,  ## Mínima confiança das regras
    maxlen = 3           ## Tamanho máximo de items na regra
)
regras1 <- apriori(Income, parameter = params)
## arules::inspect(regras1)

## Organizano em data frame
da.regras1 <- as(regras1, "data.frame")

## Extraindo as 10 primeiras de maior support, confidence e lift
ll.regras1 <- lapply(da.regras1[, -1], function(x) {
    index <- order(x, decreasing = TRUE)[1:10]
    da.regras1[index, ]
})


## ----srules11, results="asis"--------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(ll.regras1[["lift"]]),
      include.colnames = FALSE,
      only.contents = TRUE)


## ----srules12, results="asis"--------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(ll.regras1[["confidence"]]),
      include.colnames = FALSE,
      only.contents = TRUE)


## ----regras1graf, fig.height=5, fig.width=6, fig.show="hide"-------------

da.regras1 <- da.regras1[order(da.regras1$lift, decreasing = TRUE), ]
da.regras1 <- da.regras1[1:10, ]

## Visualizando as regras de associação
## <https://www.jstatsoft.org/article/view/v014i15>
fillcol <- with(da.regras1, {
    x <- (lift - min(lift)) / (max(lift) - min(lift))
    x <- (x + 0.5); x <- x / max(x)
    rgb(0.1, 0.1, 0.1, x)
})
colorfun <- colorRampPalette(c("gray70",  "gray40", "gray10"))

exprlift <- expression("Lift" == P("RHS"~"|"~"LHS")~"/"~P("RHS"))
exprconf <- expression("Confiança" == P("RHS"~"|"~"LHS"))
exprsupp <- expression("Support" == P("RHS"*","~"LHS"))

xyplot(confidence ~ support,
       data = da.regras1,
       type = c("g", "p"),
       fill = fillcol,
       pch = 21, cex = 2,
       main = list(exprlift, font = 1),
       xlab = exprsupp,
       ylab = exprconf,
       legend = list(
           top = list(
               fun = draw.colorkey,
               args = list(
                   key = list(
                       space = "bottom",
                       col = colorfun(50),
                       at = do.breaks(range(da.regras1$lift), 50),
                       draw = FALSE))))
       )

## Grafos representando as associações
lift.max <- sort(regras1, by = "lift", decreasing = T)
set.seed(432)
plot(lift.max[1:10], method = "graph",
     control = list(
         main = NULL,
         edgeCol = "black",
         nodeCol = rev(colorfun(10)),
         layout = igraph::with_graphopt(
             spring.const = 10, mass = 20)
     ))

## For show path of figures
path2 <- opts_current$get()$fig.path
label2 <- opts_current$get()$label
figs2 <- paste0(path2, label2, "-", 1:2)


## ----fitapriori2, results="hide"-----------------------------------------

##----------------------------------------------------------------------
params <- list(
    support    = 0.001,  ## Mínimo support das regras
    confidence = 0.700,  ## Mínima confiança das regras
    maxlen = 3,          ## Tamanho máximo de items na regra
    minlen = 2           ## Tamanho mínimo de items na regra
)
regras2 <- apriori(Income, parameter = params,
                   appearance = list(
                       default = "rhs",
                       lhs = "ethnic classification=hispanic")
                   )
## arules::inspect(regras2)

## Organizano em data frame
da.regras2 <- as(regras2, "data.frame")


## ----srules21, results="asis"--------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(da.regras2),
      include.colnames = FALSE,
      only.contents = TRUE)


## ----fitrecom, results="hide", cache=TRUE--------------------------------

library("recommenderlab")
data("MovieLense")

## Define os algoritmos
algorithms <- list(
    ## distancia baseada no item
    "IB (k=2)" = list(name="IBCF", param = list(k = 2)),
    ## distancia baseado em usuarios
    "UB (k=2)" = list(name="UBCF", param = list(nn = 2)),
    ## distancia baseada no item
    "IB (k=5)" = list(name="IBCF", param = list(k = 5)),
    ## distancia baseado em usuarios
    "UB (k=5)" = list(name="UBCF", param = list(nn = 5)),
    ## distancia baseada no item
    "IB (k=8)" = list(name="IBCF", param = list(k = 8)),
    ## distancia baseado em usuarios
    "UB (k=8)" = list(name="UBCF", param = list(nn = 8))
)

## Monta o esquema de algortimos
e <- evaluationScheme(MovieLense, method = "split",
                      train = 0.75, given = 12)

## Avalia os algoritmos
results <- evaluate(e, algorithms, type = "ratings")


## ----recomtab, results="asis"--------------------------------------------

av <- avg(results)
da.res <- ldply(av)
names(da.res)[1] <- c("Configuração")

capt <- "Valores estimados das medidas de risco MSE, RMSE e MAE."
print(xtable(da.res, caption = capt, label = "tab:recomtab",
             digits = 4), align = "llccc")


## ----recomgraf, fig.cap="Valores estimados das medidas de risco MSE, RMSE e MAE."----


part1 <- stack(da.res[, -1])
part2 <- rep(da.res[, 1], 3)
da.res1 <- cbind(part1, "metodo" = part2)

barchart(values ~ ind, data = da.res1,
         groups = metodo,
         col = colorfun(6),
         axis = axis.grid,
         ylab = "Valores",
         xlab = "Medida de qualidade de ajuste",
         auto.key = list(
             column = 3,
             space = "top",
             title = "Configuração do algoritmo de recomendação",
             cex.title = 1
             ),
         par.settings = list(
             superpose.polygon = list(
                 col = colorfun(6)
             ))
         )



## ----fitrecom2, results="hide", cache=TRUE-------------------------------

e2 <- evaluationScheme(MovieLense,
                      method = "split",
                      train = 0.75,
                      given = 12,
                      goodRating = 4)

results2 <- evaluate(e2, algorithms,
                     n = c(1, 5, 10, 20, 50, 100),
                     type = "topNList")


## ----recom2tab-----------------------------------------------------------

ll2 <- lapply(avg(results2), function(x) {
    meds <- c("precision", "recall", "TPR", "FPR")
    da <- x[, meds]
    da[, "FPR"] <- -(da[, "FPR"] - 1)
    names(da) <- c("Precisão", "Lembrança", "Sensibilidade",
                   "Especificidade")
    da
})


## ----rec1, results="asis"------------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(ll2[[1]], digits = 4),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----rec2, results="asis"------------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(ll2[[2]], digits = 4),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----rec3, results="asis"------------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(ll2[[3]], digits = 4),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----rec4, results="asis"------------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(ll2[[4]], digits = 4),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----rec5, results="asis"------------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(ll2[[5]], digits = 4),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----rec6, results="asis"------------------------------------------------
##----------------------------------------------------------------------
## Copiar e colar o corpo do resultado na customização latex abaixo
print(xtable(ll2[[6]], digits = 4),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ----recom2graf, fig.cap="Medidas de qualidade de precisão."-------------

par(mfrow = c(1, 2))
plot(results2, annotate = c(3,1))
plot(results2, "prec/rec", annotate = c(3,1), legend = "topright")


