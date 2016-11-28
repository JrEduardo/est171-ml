##======================================================================
## Opções gerais do knitr
library(knitr)
library(xtable)
opts_chunk$set(
    warning = FALSE,
    message = FALSE,
    echo = TRUE,
    tidy = FALSE,
    fig.width = 9,
    fig.height = 6,
    fig.align = "center",
    fig.pos = "h",
    dev.args = list(family = "Palatino"))

##======================================================================
## Configura opções de gráficos do knitr
library(lattice)
library(latticeExtra)
mycol <- c(1, "#377EB8", "#E41A1C", "#4DAF4A",
           "#ff00ff", "#FF7F00", "#984EA3", "#FFFF33")
myreg <- colorRampPalette(c(mycol[1],  "gray30", mycol[2]))(100)

## Trellis graphical style.
ps <- list(
    box.rectangle = list(col = 1, fill = c("gray70")),
    box.umbrella = list(col = 1, lty = 1),
    box.dot = list(pch = "|"),
    dot.symbol = list(col = 1, pch = 19),
    dot.line = list(col = "gray50", lty = 3),
    plot.symbol = list(col = 1),
    plot.line = list(col = 1),
    plot.polygon = list(col = "gray95"),
    superpose.line = list(col = mycol, lty = 1),
    superpose.symbol = list(col = mycol, pch = 1),
    superpose.polygon = list(col = mycol),
    strip.background = list(col = c("gray90", "gray70")),
    regions = list(col = myreg)
    )

trellis.par.set(ps)
