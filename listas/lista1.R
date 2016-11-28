## ----setup, include = FALSE----------------------------------------------

source("../_setup.R")
opts_chunk$set(
    cache = FALSE,
    echo = FALSE,
    fig.pos = "H")
cols <- trellis.par.get("superpose.line")$col

library(rjags)
library(glmnet)
library(FNN)


## ----functions-----------------------------------------------------------


## Para construir as matrizes do modelo (exercício 1)
buildX <- function(x, p) {
    X <- matrix(1, nrow = length(x))
    v <- 2 * pi * x
    for (p in 1:p) {
        m <- cbind(xs = sin(v * p), xc = cos(v * p))
        colnames(m) = paste0(colnames(m), p)
        X <- cbind(X, m)
    }
    return(X)
}

## Para seleção de amostra MCMC de apenas alguns parâmetros
select_pars <- function(sample, pars) {
    if (!is.mcmc.list(sample)) {
        sample <- as.mcmc.list(sample)
    }
    out <- lapply(sample, function(x) {
        sel <- gsub("\\[[0-9]+\\]", repl = "", colnames(x))
        x[, sel %in% pars]
    })
    return(as.mcmc(out))
}

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


## ----descritiva, fig.cap = "Dispersão do dados"--------------------------

dados <- read.table("./data/worldDevelopmentIndicators.csv",
                     header = TRUE, sep = ",", quote = "\"",
                     stringsAsFactors = FALSE)
names(dados) <- c("country", "y", "x")

xyplot(y ~ x, data = dados,
       type = c("g", "p", "smooth"),
       xlab = "PIB per capita",
       ylab = "Expectativa de vida")


## ----densx, fig.cap = "Densidade estimada do PIB per capita padronizado"----

dados <- transform(dados, x = (x - min(x)) / (max(x) - min(x)))
## summary(dados$x)
densityplot(~x, data = dados, grid = TRUE)


## ----fitfourier, echo = TRUE---------------------------------------------

X <- cbind()
models <- vector("list", 30)
v <- 2 * pi * dados$x

## Ajuste todos os modelos
for (p in 1:30) {
    m <- cbind(xs = sin(v * p), xc = cos(v * p))
    colnames(m) = paste0(colnames(m), p)
    X <- cbind(X, m)
    models[[p]] <- lm(y ~ X, data = dados)
}


## ----eqsfourier, echo = TRUE, cache = TRUE-------------------------------

## Calcula o erro quadrático de cada observação para cada modelo via
## cross-validation leave-one-out
results <- lapply(models, function(modelo) {
    n <- nrow(modelo$model)
    eqs <- vector("numeric", length = n)
    pred <- vector("numeric", length = n)
    for (i in 1:n) {
        ## obs <- as.data.frame(modelo$model[i, ])
        ## mod <- update(modelo, data = modelo$model[-i, ])
        ## pred[i] <- predict(mod, newdata = obs)
        obs <- modelo$model$y[i]
        mod <- with(modelo$model,
                    .lm.fit(x = cbind(1, X[-i, ]), y = y[-i]))
        pred[i] <- cbind(1, modelo$model$X)[i, ] %*% mod$coefficients
        eqs[i] <- (obs - pred[i])^2
    }
    list(eqs = eqs, pred = pred)
})

## Extraindo os erros quadráticos médios
eqms.mean <- sapply(results, function(x) mean(x[["eqs"]]))


## ----cvTools, echo = TRUE, eval = FALSE----------------------------------
## 
## ##-------------------------------------------
## ## Pra quem gosta de pacotes ...
## library(cvTools)
## teste <- sapply(models, function(x) cvLm(x, cost = mspe, K = 211)$cv)
## teste == eqms.mean
## ##-------------------------------------------
## 

## ----boxplotseqs, fig.cap="Distribuição empírica dos logarítmos dos erros quadráticos médios para cada um dos p modelos"----

eqs <- do.call(cbind, lapply(results, function(x) x[["eqs"]]))
colnames(eqs) <- 1:30
eqs <- stack(as.data.frame(eqs))
eqs$ind <- as.integer(as.character(eqs$ind))

## Exibição gráfica
##  -- Legenda
key <- list(
    corner = c(0.05, 0.9),
    points = list(pch = 15),
    text = list(expression(Média~dos~log(eqs)))
)
##  -- Gráfico
xyplot(log(values) ~ factor(ind),
       data = eqs,
       key = key,
       xlab = "p",
       ylab = expression(log(Erro~Quadrático)),
       panel = function(x, y, subscripts, ...) {
           panel.grid(h = -1, v = 0)
           panel.xyplot(x = as.integer(x) - 0.1, y = y,
                        col = "gray60", alpha = 0.7, cex = 0.8, ...)
           panel.bwplot(x = as.integer(x) + 0.1, y = y,
                        horizontal = FALSE, box.width = 0.4)
           means <- tapply(y, x, mean)
           panel.points(y = means, x = unique(as.integer(x)) + 0.1,
                        pch = 15)
       })


## ----measuresfourier, fig.height=4, fig.width=9, fig.cap="Medidas de qualidade de predição na escala logarítimica (linha pontilhada representa a indicação do melhor modelo)"----

## Medidas de qualidade de predição/ajuste
eqms.median <- sapply(results, function(x) median(x[["eqs"]]))
aics <- sapply(models, AIC)

## Agrupando as medidas
medidas <- data.frame(p = 1:30,
                      mean = eqms.mean,
                      median = eqms.median,
                      aics = aics)

## Indicação dos melhores modelos
medidas.min <- sapply(medidas[, -1], which.min)

## Representação gráfica
da <- reshape2::melt(medidas, id.vars = "p")
fl <- expression(E(EQM(model[p])),
                 Md(EQM(model[p])),
                 AIC(model[p]))
xyplot(log(value) ~ p | variable,
       type = "S",
       data = da,
       ylab = expression(log(hat(R))),
       as.table = TRUE,
       layout = c(NA, 1),
       scales = "free",
       strip = strip.custom(factor.levels = fl),
       panel = function(x, y, subscripts, ...) {
           panel.grid()
           panel.xyplot(x, y, ...)
           panel.abline(h = min(y, na.rm = TRUE),
                        v = which.min(y),
                        lty = 2)
       })


## ----curvefourier, fig.width=10, fig.height=5, fig.cap="Curvas ajustadas. Conjuntamente no intervalo dos dados (esquerda) e individuais no interrvalo de predição (direita)."----

## Os modelos que serão comparados
index <- c(1, medidas.min[1:2], 30)

## Curva dos modelos
lista <- models[index]
pred <- lapply(lista, function(modelo) {
    x = seq(0, 1, length.out = 50)
    betas <- coef(modelo)
    X <- buildX(x = x, p = (length(betas) - 1)/2)
    out <- cbind(x, X %*% betas)
    colnames(out) <- c("x", "yhat")
    out
})

names(pred) <- index
da <- plyr::ldply(pred, .id = "model")

leg <- parse(text = paste0("model[", index, "]"))
key <- list(
    space = "top",
    column = 2,
    lines = list(col = cols[seq_along(index)], lty = 1),
    text = list(leg)
)

xy1 <- xyplot(y ~ x, data = dados,
              type = c("g", "p"),
              alpha = 0.5,
              key = key,
              xlab = "PIB per capita (transformado)",
              ylab = "Expectativa de vida") +
    as.layer(
        xyplot(yhat ~ x, groups = model,
               data = da, type = "l")
    )

xy2 <- xyplot(yhat ~ x | model,
              data = da,
              type = c("l", "g"),
              layout = c(2, 2),
              as.table = TRUE,
              scales = "free",
              strip = strip.custom(
                  factor.levels = leg)) +
    xyplot(y ~ x, data = dados, alpha = 0.6)

print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)


## ----predfourier, fig.width=10, fig.height=5, fig.cap="Valores observados versus valores preditos. Traço de valores preditos (esquerda) e observados versus preditos (direita)"----

## Valores preditos vs observado
lista <- results[index]
preds <- do.call(cbind, lapply(lista, function(x) x[["pred"]]))
preds <- cbind(dados[, c("y", "x")], preds)
colnames(preds) <- c("real", "x", index)
preds <- reshape2::melt(as.data.frame(preds), id.vars = c("real", "x"))

preds <- preds[with(preds, order(x)), ]
xy1 <- xyplot(y ~ x, type = c("p", "g"),
              alpha = 0.5, key = key, data = dados) +
    as.layer(
        xyplot(value ~ x,
               groups =  variable,
               data = preds,
               type = "l")
    )

xy2 <- xyplot(real ~ value | variable, data = preds,
              scales = list(x = "free"),
              type = c("p", "g"),
              alpha = 0.5,
              as.table = TRUE,
              layout = c(2, 2),
              ylab = "Observado",
              xlab = "Predito",
              strip = strip.custom(factor.levels = leg),
              panel = function(x, y, subscripts, ...) {
                  panel.xyplot(x, y, ...)
                  panel.abline(0, 1)
              })


print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)


## ----fitlasso, results="hide", cache=TRUE--------------------------------

##-------------------------------------------
## Lasso Frequentista
library(glmnet)
m0lasso <- cv.glmnet(x = X, y = dados$y,
                     family = "gaussian",
                     alpha = 1, nfolds = 211,
                     grouped = FALSE)


## ----fitbayes, results="hide", cache=TRUE--------------------------------

library(rjags)

model <- "
model {
  ## Verossimilhanca
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- inprod(X[i, ], beta)
  }
  ## Prioris 1 ordem
  tau ~ dgamma(.1, 1.0E-5)
  for (j in 1:p) {
    beta[j] ~ ddexp(0, lambda)
  }
  ## Prioris 2 ordem
  lambda ~ dgamma(0.1, 0.1)
  ## Tranformacoes
  sigma <- sqrt(1/tau)
}
"

## str(X)
data0 <- list(
    n = nrow(dados),
    p = 1 + ncol(X),
    y = dados$y,
    X = cbind(1, X)
)
init0 <- list(
    tau = 1,
    beta = rep(0, data0$p),
    lambda = m0lasso$lambda.min
)

jagsmodel <- jags.model(
    textConnection(model),
    data = data0,
    inits = init0,
    n.chains = 3,
    n.adapt = 1000
)

amostra <- coda.samples(
    jagsmodel, c("lambda", "beta", "sigma", "mu"),
    n.iter = 10000, thin = 5,
    n.chains = 3,
    n.adapt = 1000)


## ----lambdas, fig.cap="Estimativas do parâmetro lambda pela abordagem de validação cruzada e bayesiana"----

## Coeficientes Lasso
da.lasso <- as.data.frame(
    matrix(c(m0lasso$lambda.min, m0lasso$lambda.min, m0lasso$lambda.1se),
           ncol = 3, nrow = 1))
colnames(da.lasso) <- c("fit", "lower", "upper")

## Coeficientes Bayesiano com intervalo de credibilidade HPD
amlambda <- select_pars(amostra, pars = c("lambda"))
amlambda <- as.mcmc(do.call(c, amlambda))
summary.lambda <- summary(amlambda)

da.bayes <- HPDinterval(amlambda)
da.bayes <- cbind("fit" = summary.lambda$statistics["Mean"],
                  as.data.frame(da.bayes))

## Junta e apresenta as estimativas via lasso e os intervalos de
## credibilidade via abordagem bayesiana
da.all <- rbind(cbind(da.bayes, pars = 1, model = "bayes"),
                cbind(da.lasso, pars = 1, model = "lasso"))
da.all <- da.all[with(da.all, order(pars, model)), ]

key <- list(
    ## corner = c(0.1, 0.9),
    type = "b", divide = 1,
    columns = 2,
    lines = list(pch = c(15, 17), lty = c(1, 0),
                 col = rev(cols[1:2])),
    text = list(c("Intervalo de credibilidade Bayesiano",
                  "Estimativa pontual via penalização Lasso*")))

segplot(
    pars ~ lower + upper,
    centers = fit, groups = model, data = da.all,
    horizontal = TRUE, draw = FALSE,
    key = key,
    pch = c(15, 17),
    lwd = 1.5,
    col = rev(cols[1:2]),
    gap = 0.01,
    ## scales = list(
    ##         x = list(at = 1:60,
    ##                  rot = 90,
    ##                  labels = parse(text = paste0("beta[", 1:60, "]")))
    ## ),
    panel = function(x, y, z, ...) {
        ## panel.abline(v = 1:60, lty = 2, col = "lightgray")
        panel.abline(h = 0, col = 1)
        ## panel.segplot(x, y, z, ...)
        cmpreg::panel.groups.segplot(x, y, z, ...)
    })


## ----coefs, fig.cap="Coeficientes estimado pela abordagem de penalização Lasso frequentista e bayesiana"----

## Coeficientes Lasso
da.lasso <- as.data.frame(
    matrix(coef(m0lasso, s = "lambda.min"), ncol = 3,
           nrow = length(coef(m0lasso))))
colnames(da.lasso) <- c("fit", "lower", "upper")

## Coeficientes Bayesiano com intervalo de credibilidade HPD
ambeta <- select_pars(amostra, pars = "beta")
ambeta <- as.mcmc(do.call(rbind, ambeta))
summary.beta <- summary(ambeta)

da.bayes <- HPDinterval(ambeta)
da.bayes <- cbind("fit" = summary.beta$statistics[, "Mean"],
                  as.data.frame(da.bayes))

## Junta e apresenta as estimativas via lasso e os intervalos de
## credibilidade via abordagem bayesiana
da.all <- rbind(cbind(da.bayes[-1, ], beta = 1:60, model = "bayes"),
                cbind(da.lasso[-1, ], beta = 1:60, model = "lasso"))
da.all <- da.all[with(da.all, order(beta, model)), ]

key <- list(
    ## corner = c(0.1, 0.9),
    type = "b", divide = 1,
    columns = 2,
    lines = list(pch = c(15, 17), lty = c(1, 0),
                 col = rev(cols[1:2])),
    text = list(c("Intervalo de credibilidade Bayesiano",
                  "Estimativa pontual via penalização Lasso")))

segplot(
    beta ~ lower + upper,
    centers = fit, groups = model, data = da.all,
    horizontal = FALSE, draw = FALSE,
    key = key,
    pch = c(15, 17),
    lwd = 1.5,
    col = rev(cols[1:2]),
    gap = 0.3,
    scales = list(
            x = list(at = 1:60,
                     rot = 90,
                     labels = parse(text = paste0("beta[", 1:60, "]")))
    ),
    panel = function(x, y, z, ...) {
        panel.abline(v = 1:60, lty = 2, col = "lightgray")
        panel.abline(h = 0, col = 1)
        ## panel.segplot(x, y, z, ...)
        cmpreg::panel.groups.segplot(x, y, z, ...)
    })


## ----dados2, echo=TRUE---------------------------------------------------

## Leitura dos dados
dados <- read.table("./data/dadosFacesAltaResolucao.txt",
                  header = TRUE, sep = " ")

## Verificando
## str(dados)
dim(dados)


## ----images, fig.cap="Seis primeiras imagens representadas no conjunto de dados"----

##----------------------------------------------------------------------
## Plotando as imagens
imagens <- lapply(as.data.frame(t(dados[, -1])),
                  function(x) matrix(x, ncol = 64))
xys <- lapply(1:6, function(x) {
    main <- paste("Figura", x, "(Direção ", dados$y[x], ")")
    xy <- levelplot(imagens[[x]],
                    sub = main,
                    xlab = "",
                    ylab = "",
                    scales = list(draw = FALSE),
                    par.settings = list(
                        layout.heights =
                            list(top.padding = 0,
                                 bottom.padding = 1,
                                 key.sub.padding = 0
                                 ),
                        layout.widths =
                            list(left.padding = 0,
                                 right.padding = 0)))
})

gridExtra::marrangeGrob(xys, nrow = 2, ncol = 3, top = NA)


## ----part, echo=TRUE-----------------------------------------------------

## Particionando o conjunto de dados
dasplit <- mysplit(dados, percent = c(0.6, 0.2, 0.2),
                   seed = 1994)

## Número de observações em cada partição
sapply(dasplit, dim)

## Atribuindo as partições em objetos de nome sujestivo
da.train <- dasplit[[1]]
da.valid <- dasplit[[2]]
da.teste <- dasplit[[3]]


## ----fitknn, fig.height=5, fig.width=10, fig.cap="Valores preditos pelo método KNN com 5 vizinhos. Valor preditos na ordem do conjunto de teste (esquerda) e preditos versus observados (direita)"----

library(FNN)
train <- rbind(da.train, da.valid)
knn.model <- knn.reg(train = train, test = da.teste,
                     y = train[, "y"], k = 5)

xy1 <- xyplot(knn.model$pred ~ seq_along(knn.model$pred),
              type = c("p", "g"),
              xlab = "Índice da imagem (no conjunto de teste)",
              ylab = "Valor predito para a direção")

xy2 <- xyplot(da.teste[, 1] ~ knn.model$pred,
              type = c("p", "g"),
              xlab = "Valor predito",
              yla = "Valor observado",
              panel = function(x, y, ...) {
                  panel.xyplot(x, y, ...)
                  panel.abline(0, 1)
              })

print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)


## ----fitknncv, cache=TRUE------------------------------------------------

kseq <- 1:(nrow(da.train))
resultsknn <- lapply(kseq, function(k) {
    model <- knn.reg(train = da.train, test = da.valid,
                     y = da.train[, "y"], k = k)
    pred <- model$pred
    eqs <- (da.valid[, "y"] - pred)^2
    list(eqs = eqs, pred = pred)
})


## ----plotknncv, fig.cap="Erros quadráticos médios e medianos na escala logarítimica (linha pontilhada representa a indicação do melhor modelo). Para todos o vizinhos (acima) e apenas para os k's próximos do ótimo (abaixo)."----

## Calcula os erros quadráticos médios
eqs.mean <- sapply(resultsknn, function(x) mean(x[["eqs"]]))
eqs.median <- sapply(resultsknn, function(x) median(x[["eqs"]]))

## Organiza em data.frame
medidas <- data.frame(k = kseq, mean = eqs.mean, median = eqs.median)
da <- reshape2::melt(medidas, id.vars = "k")

## Exibe graficamente
fl <- expression(E(EQM(model[p])),
                 Md(EQM(model[p])))
xy1 <- xyplot(log(value) ~ k | variable,
              type = "S",
              data = da,
              ylab = expression(log(hat(R))),
              as.table = TRUE,
              layout = c(NA, 1),
              scales = "free",
              strip = strip.custom(factor.levels = fl),
              panel = function(x, y, subscripts, ...) {
                  panel.grid()
                  panel.xyplot(x, y, ...)
                  panel.abline(h = min(y, na.rm = TRUE),
                               v = which.min(y),
                               lty = 2)
              })

## Exibe apenas os 8 k's mais próximos (acima e abaixo) do mínimo
medidas.min <- sapply(medidas[, -1], which.min)
index <- c(-8:8) + which.min(eqs.mean)
medidas <- data.frame(k = kseq[index],
                      mean = eqs.mean[index],
                      median = eqs.median[index])
da <- reshape2::melt(medidas, id.vars = "k")

xy2 <- xyplot(log(value) ~ k | variable,
              type = "S",
              data = da,
              ylab = expression(log(hat(R))),
              as.table = TRUE,
              layout = c(NA, 1),
              scales = "free",
              strip = strip.custom(factor.levels = fl),
              panel = function(x, y, subscripts, ...) {
                  panel.grid()
                  panel.xyplot(x, y, ...)
                  panel.abline(h = min(y, na.rm = TRUE),
                               v = which.min(y),
                               lty = 2)
              })

print(xy1, split = c(1, 1, 1, 2), more = TRUE)
print(xy2, split = c(1, 2, 1, 2), more = FALSE)


## ----predknn, fig.height=5, fig.width=10, fig.cap="Valores preditos pelo método KNN com 10 vizinhos. Preditos versus observados (esquerda) e desvios (direita)."----

## Indicação do melhor algoritmo preditivo
index <- which.min(eqms.median)

## Calibra o método com base no melhor k
train <- rbind(da.train, da.valid)
knn.model <- knn.reg(train = train, test = da.teste,
                     y = train[, "y"], k = kseq[index])

##
xy1 <- xyplot(da.teste[, 1] ~ knn.model$pred,
              type = c("p", "g"),
              panel = function(x, y, ...) {
                  panel.xyplot(x, y, ...)
                  panel.abline(0, 1)
              })

xy2 <- xyplot(da.teste[, 1] - knn.model$pred ~ seq_along(da.teste[, 1]),
              type = c("p", "g", "smooth"),
              panel = function(x, y, ...) {
                  panel.xyplot(x, y, ...)
                  panel.abline(h = 0, lty = 2, col = cols[2])
              })

print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)


## ----fitlassofaces, cache=TRUE-------------------------------------------

##----------------------------------------------------------------------
## Regressão com penalização do tipo Lasso
library(glmnet)
## Validação leave-one-out (demorado!)
train <- rbind(da.train, da.valid)
lasso.model <- cv.glmnet(x = as.matrix(train[, -1]), y = train[, "y"],
                         family = "gaussian", alpha = 1,
                         nfolds = nrow(train), grouped = FALSE)


## ----plotlassofaces, fig.cap="Lambdas versus erros quadráticos médios"----

plot(lasso.model)


## ----predlasso, fig.height=5, fig.width=10, fig.cap="Valores preditos pelo método Lasso. Preditos versus observados (esquerda) e desvios (direita)."----

pred.lasso <- predict(lasso.model, newx = as.matrix(da.teste[, -1]),
                      s = "lambda.min")

xy1 <- xyplot(da.teste[, 1] ~ pred.lasso,
              type = c("p", "g"),
              xlab = "Valores preditos",
              ylab = "Valores observados",
              panel = function(x, y, ...) {
                  panel.xyplot(x, y, ...)
                  panel.abline(0, 1)
              })

xy2 <- xyplot(da.teste[, 1] - pred.lasso ~ seq_along(da.teste[, 1]),
              type = c("p", "g", "smooth"),
              xlab = "Indice da imagem (no conjunto de teste)",
              ylab = "Desvios (observado - predito)",
              panel = function(x, y, ...) {
                  panel.xyplot(x, y, ...)
                  panel.abline(h = 0, lty = 2, col = cols[2])
              })

print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)


## ------------------------------------------------------------------------

ncoef <- sum(as.matrix(coef(lasso.model, s = "lambda.min")) != 0)


## ----compare, fig.cap="Comparação dos métodos preditivos KNN e regressão Lasso via erros quadráticos"----

pred.knn <- knn.reg(train = train, test = da.teste,
                     y = train[, "y"], k = kseq[index])$pred
eqm.knn <- (da.teste[, 1] - pred.knn)^2
eqm.lasso <- (da.teste[, 1] - pred.lasso)^2

da <- data.frame(eqm = c(eqm.knn, eqm.lasso),
                 model = rep(c("knn", "lasso"),
                             each = nrow(da.teste)))

xyplot(eqm ~ model,
       data = da,
       ylab = expression(Erro~Quadrático),
       xlab = "Método",
       scales = list(y = list(log = TRUE)),
       panel = function(x, y, ...) {
           panel.grid(h = -1, v = 0)
           panel.xyplot(x = as.integer(x) - 0.05, y = y,
                        col = "gray60", alpha = 0.7, cex = 0.8,
                        jitter.x = TRUE, factor = 0.1, ...)
           panel.bwplot(x = as.integer(x) + 0.05, y = y,
                        horizontal = FALSE, box.width = 0.1)
           means <- tapply(y, x, mean)
           panel.points(y = means, x = unique(as.integer(x)) + 0.05,
                        pch = 15)
       })


