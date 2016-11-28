## ----setup, include = FALSE----------------------------------------------

source("../_setup.R")
opts_chunk$set(
    cache = FALSE,
    echo = FALSE,
    fig.pos = "H")
cols <- trellis.par.get("superpose.line")$col

## Pacotes
library(e1071)
library(MASS)
library(FNN)
library(Epi)


## ----functions-----------------------------------------------------------

## Para partição de uma base de dados em dados de treinamento,
## validação, teste, etc.
mysplit <- function(data, percent = NA, nfolds = NA,
                    nobs = NA, seed = NULL) {
    if (sum(is.na(c(percent, nfolds, nobs))) != 2) {
        stop("Utilize um e apenas um dos argumentos")
    }
    n <- nrow(data)
    if (!sum(is.na(percent))) {
        if (sum(percent) != 1) {
            stop("Os percentuais em cada dobra devem somar 1!")
        }
        p <- percent
        folds <- n * p
    }
    if (!is.na(nfolds)) {
        if (nfolds > n) {
            stop(paste0("O número de dobras não deve exceder o ",
                       "tamanho da amostra (n=", n, ")!"))
        }
        p <- rep(1/nfolds, nfolds)
        folds <- n * p
    }
    if (!sum(is.na(nobs))) {
        if (sum(nobs) != n) {
            stop(paste0("O número de observações em cada dobra deve ",
                       "somar ", n, "!"))
        }
        folds <- nobs
    }
    if (!is.null(seed)) {
        set.seed(seed)
    }
    folds <- round(folds)
    while (sum(folds) != n) {
        g <- sample(1:length(folds), 1)
        if (sum(folds) < n) {
            folds[g] <- folds[g] + 1
        } else {
            folds[g] <- folds[g] - 1
        }
    }
    out <- vector("list", length = length(folds))
    for (i in 1:length(folds)) {
        index <- sample(nrow(data), size = folds[i])
        out[[i]] <- data[index, ]
        data <- data[-index, ]
    }
    return(out)
}


## Medidas resumo da matriz de confusão
confmeds <- function(probs, teste, corte = 0.5) {
    ##-------------------------------------------
    ## Restrita para classificadores com duas categorias
    ## A probabilidade em probs é da ultima categoria, ordenada por
    ## ordem alfabética
    ##-------------------------------------------
    te <- as.integer(teste) - 1
    cl <- ifelse(probs <= corte, 0, 1)
    ##-------------------------------------------
    pace <- sum(cl == te) / length(te)
    espe <- sum(te[cl == te] == 0) / sum(te == 0)
    sens <- sum(te[cl == te] == 1) / sum(te == 1)
    pvp <- sum(cl[cl != te] == 0) / sum(cl == 0)
    pvn <- sum(cl[cl != te] == 1) / sum(cl == 1)
    ##-------------------------------------------
    ## tab <- table(cl, teste); print(tab)
    ## if (sum(dim(tab)) == 4) {
    ##     senst <- tab[1, 1] / sum(tab[, 1])
    ##     espet <- tab[2, 2] / sum(tab[, 2])
    ##     pacet <- sum(diag(tab))/sum(tab)
    ##     print(c(senst, espet, pacet))
    ## }
    out <- c("pace" = pace,
             "sens" = sens,
             "espe" = espe
             ## "pvn" = pvn,
             ## "pvp" = pvp
             )
    ## attr(out, "class") = "rocmeds"
    return(out)
}

## Exibição gráfica da matrix de confusão
confusionPlot <- function(cl, te) {
    ##-------------------------------------------
    lev <- unique(c(cl, te))
    ## table(cl, te)
    ma <- matrix(c(sum(te[cl == te] == lev[1]),
                   sum(te[cl != te] == lev[1]),
                   sum(te[cl != te] == lev[2]),
                   sum(te[cl == te] == lev[2])),
                 ncol = 2, byrow = FALSE)
    ##-------------------------------------------
    espe <- ma[1, 1] / sum(ma[, 1])
    sens <- ma[2, 2] / sum(ma[, 2])
    perr <- 1 - sum(diag(ma))/sum(ma)
    texto <- paste(c("Classificações incorretas",
                     "Especificidade",
                     "Sensibilidade"),
                   round(c(perr, espe, sens), 4))
    key <- list(
        space = "bottom",
        lines = list(pch = rep(NA, 3), lty = rep(0, 3)),
        text = list(texto))
    ##-------------------------------------------
    colr <- colorRampPalette(c("gray90", "gray70", "gray50"))
    xy <- levelplot(
        t(prop.table(ma)),
        col.regions = colr,
        at = seq(0, 1, length.out = 12),
        ylim = c(2.5, 0.5),
        aspect = "fill",
        xlab.top = list("Classificação", cex = 1.2,
                        font = "bold"),
        ylab = list("Categoria\nreal", cex = 1.2,
                    rot = 0, font = "bold"),
        xlab = NULL,
        key = key,
        scales = list(
            at = 1:2,
            labels = lev,
            tck = 0,
            cex = 1.1,
            x = list(alternating = 2)
        ),
        par.settings = list(
            layout.heights = list(xlab.key.padding = 10)
        ),
        panel = function(x, y, z, ...) {
            panel.levelplot(x = x, y = y, z = z, ...)
            panel.text(x = x, y = y, col = 1,
                       labels = format(round(z, 2), nsmall = 2))
        })
    print(xy)
    invisible(xy)
}


## ----tabdescritiva-------------------------------------------------------

dados <- read.table("./data/titanic.txt",
                    header = TRUE,
                    sep = "\t")

xt <- addmargins(table(dados), 4, FUN = list(list(Total = sum)))
pander::pander(ftable(xt),
               caption = paste("Tabela de frequência dos",
                               "passageiros do Titanic"),
               style = "rmarkdown",
               ## justify = c(rep("left", 4),
               ##             rep("center", 2)),
               emphasize.strong.cells = rbind(
                   cbind(1, c(4, 7)), cbind(2, 1:3))
               )


## ----barchart, fig.height=3.5, fig.width=9-------------------------------

xys <- lapply(1:ncol(dados), function(i) {
    da <- data.frame(table(dados[, i]), v = names(dados)[i])
    xy <- barchart(Freq ~ Var1 | v,
                   data = da,
                   horizontal = FALSE,
                   scales = list(
                       y = list(draw = FALSE)
                   ),
                   origin = 0,
                   ylab = NULL,
                   panel = function(x, y, ...) {
                       panel.barchart(x, y, ...)
                       panel.text(x, y - mean(y) * 0.07, y)
                   },
                   par.settings = list(
                       layout.widths = list(
                           left.padding = 0,
                           right.padding = 0
                       )
                   ))
})

## Gráficos de frequência
gridExtra::marrangeGrob(xys, ncol = 4, nrow = 1, top = NA)


## ----mosaicplot, fig.height=3.5, fig.width=9, fig.cap="Gráficos descritivos da base de dados. Frequências observadas em cada variável de Titanic (superior) e Representação da tabela de contingência de forma hierárquica (inferior)."----

## Gráficos de mosaico sequenciais
## vcd::cotabplot(table(dados), layout = c(1, 4))
vcd::doubledecker(table(dados), data = dados)


## ---- echo=TRUE----------------------------------------------------------

## Particionando o conjunto de dados
dasplit <- mysplit(dados, percent = c(0.7, 0.3), seed = 1994)

## Número de observações em cada partição
sapply(dasplit, nrow)

## Atribuindo as partições em objetos de nome sujestivo
da.train <- dasplit[[1]]
da.teste <- dasplit[[2]]

## Tranformando para inteiro, para utilização de alguns métodos
X.train <- sapply(da.train, as.integer)
X.teste <- sapply(da.teste, as.integer)


## ----fitglm--------------------------------------------------------------

m0glm <- glm(Survived ~ ., data = da.train, family = "binomial")

tabglm <- summary(m0glm)$coefficients
ind <- c(0, 11, 12, 12, 2, 3)
rownames(tabglm) <- paste0("$\\beta_{", ind, "}$")
colnames(tabglm) <- gsub("\\|", "\\\\|", colnames(tabglm))
pander::pander(tabglm,
               caption = paste("Coeficientes estimados e teste",
                               "de Wald para o modelo Logístico"),
               justify = c("left", rep("center", 4)),
               style = "rmarkdown")

##-------------------------------------------
## Risco estimado
pglm <- predict(m0glm, newdata = da.teste, type = "response")
pred.glm50 <- ifelse(pglm >= 0.5, "Yes", "No")
errglm50 <- sum(da.teste[, 4] != pred.glm50)/nrow(da.teste)


## ----fitlm---------------------------------------------------------------

m0lm <- lm(as.integer(Survived) ~ ., data = da.train)

tablm <- summary(m0lm)$coefficients
rownames(tablm) <- paste0("$\\beta_{", ind, "}$")
colnames(tablm) <- gsub("\\|", "\\\\|", colnames(tablm))
pander::pander(tablm,
               caption = paste("Coeficientes estimados e teste",
                               "de Wald para o modelo Gaussiano"),
               justify = c("left", rep("center", 4)),
               style = "rmarkdown")

##-------------------------------------------
## Risco estimado
plm <- predict(m0lm, newdata = da.teste) - 1
pred.lm50 <- ifelse(plm >= 0.5, "Yes", "No")
errlm50 <- sum(da.teste[, 4] != pred.lm50)/nrow(da.teste)


## ----fitnaives-----------------------------------------------------------

m0naive <- naiveBayes(Survived ~., data = da.train)

tabnaive <- do.call(cbind, m0naive$tables)
## vars <- c(rep("Class", 4), rep("Sex", 2), rep("Age", 2))
## cnam <- paste0("$X_{", vars, "} = ", colnames(tabnaive), "$")
## colnames(tabnaive) <- cnam
pander::pander(tabnaive,
               caption = paste("Probabilidades estimadas para cada",
                               "categoria de cada covariável",
                               "condicional a Survived"),
               ## justify = c("left", rep("center", 4)),
               digits = 3,
               style = "rmarkdown")

##-------------------------------------------
## Risco estimado
pnaive <- predict(m0naive, newdata = da.teste, type = "raw")[, 2]
pred.naive50 <- ifelse(pnaive >= 0.5, "Yes", "No")
errnaive50 <- sum(da.teste[, 4] != pred.naive50)/nrow(da.teste)


## ----fitlda--------------------------------------------------------------

m0lda <- lda(Survived ~ ., data = da.train)
m0lda

##-------------------------------------------
## Risco estimado
plda <- predict(m0lda, newdata = da.teste[, -4])[["posterior"]][, 2]
pred.lda50 <- ifelse(plda >= 0.5, "Yes", "No")
errlda50 <- sum(da.teste[, 4] != pred.lda50)/nrow(da.teste)


## ----fitqda--------------------------------------------------------------

m0qda <- qda(Survived ~ ., data = da.train)
m0qda

##-------------------------------------------
## Risco estimado
pqda <- predict(m0qda, newdata = da.teste[, -4])[["posterior"]][, 2]
pred.qda50 <- ifelse(pqda >= 0.5, "Yes", "No")
errqda50 <- sum(da.teste[, 4] != pred.qda50)/nrow(da.teste)


## ------------------------------------------------------------------------

## Separa a parte do treino para validação
train.split <- mysplit(da.train, percent = c(0.8, 0.2), seed = 1994)

## Organiza em data.frame e transforma para valores inteiros
da.train2 <- train.split[[1]]
da.valid <- train.split[[2]]
X.train2 <- sapply(da.train2, as.integer)
X.valid <- sapply(da.valid, as.integer)


## ----cvknn, cache=TRUE---------------------------------------------------

## Parte bem demorada, pois faz-se para todos os k's, embora fosse
## necessário apenas para alguns...
kseq <- seq(1, nrow(X.train2), by = 2)
errs <- sapply(kseq, function(k) {
    m0 <- FNN::knn(train = X.train2[, -4], test = X.valid[, -4],
                   cl = X.train2[, 4], k = k, algorithm = "kd_tree")
    sum(X.valid[, 4] != m0)/nrow(X.valid)
})


## ----plotcvknn, fig.height=4, fig.width=9, fig.cap="Proporção de classificações incorretas pelo classificador KNN com diferentes k's. Todos os possíveis k's ímpares (esquerda) e apenas os k's próximos do k ótimo."----

## Exibe graficamente
da <- data.frame(k = kseq, R = errs)

xy1 <- xyplot(R ~ k,
              type = "S",
              data = da,
              ylab = expression(hat(R)),
              panel = function(x, y, subscripts, ...) {
                  panel.grid()
                  panel.xyplot(x, y, ...)
                  panel.abline(h = min(y, na.rm = TRUE),
                               v = x[which.min(y)],
                               lty = 2)
              })

## Exibe apenas os 8 k's mais próximos (acima e abaixo) do mínimo
k.min <- kseq[which.min(errs)]
index <- c(-10:10) + which.min(errs)
index <- index[index > 0]

xy2 <- xyplot(R ~ k,
              type = c("S", "p"),
              pch = 19,
              data = da[index, ],
              ylab = expression(hat(R)),
              panel = function(x, y, subscripts, ...) {
                  panel.grid()
                  panel.xyplot(x, y, ...)
                  panel.abline(h = min(y, na.rm = TRUE),
                               v = x[which.min(y)],
                               lty = 2)
              })

print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)


## ----fitknn--------------------------------------------------------------

## Ajusta o classificador com base no k ótimo
m0knn <- class::knn(train = X.train[, -4], test = X.teste[, -4],
                    cl = da.train[, 4], k = k.min, prob = TRUE)

##-------------------------------------------
## Risco estimado
pknn <- ifelse(m0knn == "Yes", attr(m0knn, "prob"),
               1 - attr(m0knn, "prob"))
pred.knn50 <- ifelse(pknn >= 0.5, "Yes", "No")
errknn50 <- sum(da.teste[, 4] != pred.knn50)/nrow(da.teste)


## ------------------------------------------------------------------------

## Lista de probabilidades estimadas com base nos modelos de
## classificação vistos anteriormente
lista <- list("GLM" = pglm, "LM" = plm, "NB" = pnaive,
              "LDA" = plda, "QDA" = pqda, "KNN" = pknn)

## Tabela de comparação com calculo de escore que dá mais importância
## para taxa de acertos do que para sens e espe
compare <- sapply(lista, confmeds, teste = da.teste[, 4], corte = 0.5)
escore <- apply(compare, 2, function(x) (2 * x[1] + x[2] + x[3]) / 4)

## Exibindo em formato de tabela
compare <- rbind(compare, escore)
rownames(compare) <- c("Prop. de Acertos",
                       "Sensibilidade",
                       "Especificidade",
                       "Escore")
pander::pander(compare,
               caption = paste("Comparação dos métodos utilizando o",
                               "ponto de corte usual de 0.5"),
               justify = c("left", rep("center", 6)),
               digits = 4,
               style = "rmarkdown")


## ----rocs, fig.show="hide"-----------------------------------------------

## Atualiza os modelos considerando somente o conjunto de treinamento
## reduzido da base de validação
m1glm   <- update(m0glm, data = da.train2)
m1lm    <- update(m0lm, data = da.train2)
m1lda   <- update(m0lda, data = da.train2)
m1qda   <- update(m0qda, data = da.train2)
m1naive <- naiveBayes(Survived ~., data = da.train2)
m1knn   <- class::knn(train = X.train2[, -4], test = X.valid[, -4],
                      cl = da.train2[, 4], k = k.min, prob = TRUE)

## Calcula as probabilidades
listacv <- list(
    prglm   = predict(m1glm, newd = da.valid, type = "response"),
    prlm    = predict(m1lm, newd = da.valid) - 1 ,
    prlda   = predict(m1lda, newd = da.valid[, -4])[["posterior"]][, 2],
    prqda   = predict(m1qda, newd = da.valid[, -4])[["posterior"]][, 2],
    prnaive = predict(m1naive, newd = da.valid, type = "raw")[, 2],
    prknn   = ifelse(m1knn == "Yes", attr(m1knn, "prob"),
                     1 - attr(m0knn, "prob"))
)

## Calcula as medidas resumo da matriz de confusao para cada
## classificador em cada ponto de corte definido por pseq
pseq <- seq(0.1, 0.9, by = 0.01)
rocs <- lapply(listacv, function(p) {
    ROC(test = p, stat = da.valid[, 4])
    ## meds <- do.call(
    ##     rbind, lapply(pseq, confmeds, prob = p, teste = da.valid[, 4]))
    ## as.data.frame(meds)
})


## ----plotroc, fig.cap="Curvas ROC (Receiver Operating Characteristic) para cada modelo de classificação com indicação do melhor ponto de corte e respectivo AUC (Area Under Curve) para o conjunto de validação."----

## Organiza em data.frame
da <- plyr::ldply(lapply(rocs, function(x) {
    da <- data.frame(sens = x$res[, "sens"],
                     espe = x$res[, "spec"],
                     auc = x$AUC)
    da$pcor <- x$res[which.max(with(da, sens + espe)), 5]
    da
}), .id = "Model")

fl <- c("Generalized Linear Model",
        "Linear Model",
        "Naive bayes",
        "Linear Discriminant",
        "Quadratic Discriminant",
        "K-Nearest Neighbor")

xyplot(sens ~ 1-espe | Model,
       data = da,
       layout = c(NA, 2),
       as.table = TRUE,
       type = c("l", "g"),
       lwd = 2,
       xlab = "1 - Especificidade",
       ylab = "Sensibilidade",
       strip = strip.custom(factor.levels = fl),
       panel = function(x, y, subscripts, ...) {
           index <- which.max(1 + y - x)
           texto <- paste("Ponto de corte:\n",
                          round(da$pcor[subscripts], 4))
           texto2 <- paste("Área abaixo da curva: ",
                           round(da$auc[subscripts], 4))
           panel.xyplot(x, y, ...)
           panel.text(x[index], y[index] + 0.15, texto, cex = 0.8)
           panel.points(x[index], y[index], pch = 3, lwd = 3)
           panel.text(0.5, 0.01, texto2, cex = 0.8)
           panel.abline(a = 0, b = 1, col = "gray70", lty = 2)
       })


## ------------------------------------------------------------------------

## Tabela de comparação com calculo de escore que dá mais importância
## para taxa de acertos do que para sens e espe
cortes <- with(da, tapply(pcor, Model, unique))
compare <- sapply(1:length(cortes), function(i) {
    confmeds(lista[[i]], teste = da.teste[, 4], corte = cortes[i])
})
## escore <- apply(compare, 2, function(x) (2 * x[1] + x[2] + x[3]) / 4)

## Exibindo em formato de tabela
rownames(compare) <- c("Prop. de Acertos",
                       "Sensibilidade",
                       "Especificidade")
colnames(compare) <- names(lista)
pander::pander(compare,
               caption = paste(
                   "Comparação dos métodos via resumos da matriz",
                   "de confusão da classificação vs. teste,",
                   "utilizando o ponto de corte ótimo (obtido por",
                   "validação cruzada)"),
               justify = c("left", rep("center", 6)),
               digits = 4,
               style = "rmarkdown")


