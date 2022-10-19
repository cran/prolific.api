## ---- r_setup ----
suppressMessages({
    stopifnot(require(knitr))
    old_width <-
        options(width = 70)
    knitr::opts_chunk$set(
        eval = FALSE,
        collapse = TRUE,
        comment = "#>",
        dev = "jpeg",
        dpi = 100,
        fig.asp = 0.8,
        fig.width = 5,
        out.width = "60%",
        fig.align = "center"
    )
    library(prolific.api)
    library(data.table)
    library(reactable)
    library(htmltools)
})

print("<[[r_setup]]>")

## ---- r_restore ----
suppressMessages({
    options(old_width)
})

print("<[[r_restore]]>")
