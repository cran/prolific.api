
## ---- list_of_prescreeners_count ----
prolific_api_access$prescreeners()[, .N]

## ---- list_of_prescreeners_create ----
if (require(data.table)) {
    list_of_prescreeners <-
        prolific_api_access$.internals$methods$prescreeners()

    list_of_prescreeners[, constraints :=
        (lapply(list_of_prescreeners$attributes, function(x) paste0(x$label, collapse = "    ")))]

    list_of_prescreeners[
        ,
        cls_type :=
            gsub("web\\.eligibility\\.models\\.(.*?)eligibilityrequirement", "\\1", tolower(cls))
    ]

    list_of_prescreeners[cls_type == "customwhitelist", constraints :=
        paste0(
            c(
                constraints,
                "allow list",
                "include"
            ),
            collapse = "    "
        )]

    list_of_prescreeners[cls_type == "customblacklist", constraints :=
        paste0(
            c(
                constraints,
                "block list",
                "disallow list",
                "exclude"
            ),
            collapse = "    "
        )]

    save(
        list_of_prescreeners,
        file =
            "list_of_prescreeners.RData",
        compress =
            "xz"
    )

    print("<[[list_of_prescreeners_reactable]]>")
}

## ---- list_of_prescreeners_reactable ----

if (suppressMessages(require(reactable) && require(htmltools))) {
    list_of_prescreeners <-
        get(load("./list_of_prescreeners.RData")[1])

    enclose <-
        function(x, delims = "\"") {
            paste0(delims, x, delims)
        }

    html_style <-
        function(x) {
            enclose(paste0(names(x), ": ", Reduce(c, x), collapse = "; "))
        }

    prescreener_value_fmt <- function(x) {
        values <- NULL

        if ("type" %in% names(x$attributes[[1]])) {
            if ("min" %in% names(x$attributes[[1]])) {
                z <- x$attributes[[1]][, .SD, .SDcols = c("min", "max")]

                if (tolower(x$attributes[[1]]$type[1]) == "date") {
                    z[, names(z) := lapply(.SD, function(x) {
                        as.IDate(x)
                    })]
                    values <- as.IDate(round(c(Reduce(cbind, as.list(z)) %*% rbind(0.8, 0.2))))
                }

                if (tolower(x$attributes[[1]]$type[1]) %in% c("number", "min", "max")) {
                    z[, names(z) := lapply(.SD, function(x) {
                        x[grepl("(false|true)", x)] <- NA
                        as.numeric(x)
                    })]
                    values <- round(c(as.matrix(z) %*% rbind(0.8, 0.2)))
                }
            } else {
                if (tolower(x$attributes[[1]]$type) == "list") {
                    if (grepl("(white|black)_list", tolower(x$attributes[[1]]$name))) {
                        values <- "list('&lt;respondents_prolific_id_1&gt;','&lt;respondents_prolific_id_2&gt;')"
                    }
                }
            }
        } else {
            if (!is.null(x$attributes)) {
                if (nrow(x$attributes[[1]]) > 0) {
                    if (tolower(x$attributes[[1]]$name[1] == "blocked_studies")) {
                        values <- "list('&lt;internal_study_name_or_id_1&gt;','&lt;internal_study_name_or_id_2&gt;')"
                    }
                }
            }
        }

        return(values)
    }


    prescreener_example_fmt <- function(x) {
        code_style <- paste0(
            "<p style=",
            html_style(
                list(
                    "margin-left" = "15px",
                    "margin-right" = "15px",
                    "background-color" = "#f2f2f2",
                    "text-align" = "center",
                    "font-size" = "90%",
                    "color" = "#FF00B7",
                    "font-family" = "Courier New",
                    "border-style" = "none none none none"
                )
            ),
            ">"
        )

        txt_style <- paste0(
            "<p style=",
            html_style(
                list(
                    "margin-left" = "15px",
                    "margin-right" = "15px",
                    # "text-align" = "center",
                    "font-size" = "90%",
                    "background-color" = "#f2f2f2",
                    "font-family" = "Verdana",
                    "color" = "#000000",
                    "border-style" = "none none none none"
                )
            ),
            ">"
        )

        if (!is.null(x$attributes)) {
            if (nrow(x$attributes[[1]]) > 10) {
                data.table::setkeyv(x$attributes[[1]], "label")
            }
        }

        values <- prescreener_value_fmt(x)

        example_txt <-
            paste0(
                txt_style,
                "Example Code for selecting participants",
                switch(x$cls_type,
                    "agerange" =
                        paste0(" with a ", tolower(x$attributes[[1]]$label), " of ", values),
                    "approvalnumbers" =
                        paste0(
                            " who participated in ",
                            vapply(x$attributes[[1]]$name, function(i) {
                                switch(i,
                                    "minimum_approvals" = "more",
                                    "maximum_approvals" = "less"
                                )
                            }, "a"),
                            " than ",
                            (values),
                            " studies on Prolific"
                        ),
                    "approvalrate" =
                        paste0(
                            " who have been approved in ",
                            vapply(x$attributes[[1]]$name, function(i) {
                                switch(i,
                                    "minimum_approval_rate" = "more",
                                    "maximum_approval_rate" = "less"
                                )
                            }, "a"),
                            " than ",
                            values,
                            "% of the studies on Prolific they participated in"
                        ),
                    "customblacklist" =
                        paste0(
                            " using a custom block list (replace ",
                            gsub("list\\((.*?)\\)", "\\1", values),
                            " with the actual Prolific ID(s)"
                        ),
                    "customwhitelist" =
                        paste0(
                            " using a custom allow list (replace ",
                            gsub("list\\((.*?)\\)", "\\1", values),
                            " with the actual Prolific ID(s)"
                        ),
                    "joinedafter" =
                        " who joined after ",
                    "joinedbefore" =
                        " who joined before ",
                    "multiselectanswer" =
                        paste0(" where '", x$title, "' is '", x$attributes[[1]]$label, "'"),
                    "previousstudies" =
                        " who did not participate in previous studies",
                    "previousstudiesallowlist" =
                        " who participated in previous studies",
                    "selectanswer" =
                        paste0(" where '", x$title, "' is '", x$attributes[[1]]$label, "'"),
                ),
                ":"
            )

        example_code <- paste0(
            code_style,
            "prolific_prescreener(",
            "title = ", enclose(x$title), ", ",
            enclose(x$attributes[[1]]$label),
            ifelse(is.null(values), "", " = "),
            values,
            ")",
            "</p>"
        )

        sep_line <- paste0(
            "<p style=",
            html_style(
                list(
                    "border-style" = "double double double double"
                )
            ),
            ">",
            "</p>"
        )

        return(
            paste0(
                sep_line,
                example_txt,
                # "<br></br>",
                example_code,
                sep_line
            )
        )
    }



    prescreener_details_fmt <-
        function(x, ncol = 4) {
            x <<- x
            title <- x$title
            cls <- x$cls


            if (!is.null(x$attributes)) {
                if (nrow(x$attributes[[1]]) > 10) {
                    data.table::setkeyv(x$attributes[[1]], "label")
                }
            }
            labels <- x$attributes[[1]]$label

            nrow <- ceiling(length(labels) / ncol)
            tbl <- matrix(c(labels, rep("", nrow * ncol - length(labels))), ncol = ncol, byrow = TRUE)
            colnames(tbl) <- paste0("V", 1:ncol)



            prescreener_examples <- prescreener_example_fmt(x)

            if ((length(prescreener_examples) > 0) && ((nrow * ncol) > 0)) {
                examples_tbl <-
                    matrix(c(prescreener_examples, rep("", nrow * ncol - length(prescreener_examples))), ncol = ncol, byrow = TRUE)
                colnames(examples_tbl) <- paste0("V", 1:ncol)
            }

            out_list <- lapply(
                seq_len(ncol(tbl)),
                function(i) {
                    details_coldef <- colDef(
                        html = TRUE,
                        resizable = TRUE,
                        name = "",
                        show = TRUE,
                        details = function(index) {
                            if (length(prescreener_examples) > 0) {
                                out <- as.character(examples_tbl[index, i])
                                if (out != "") {
                                    return(out)
                                }
                            } else {
                                return(NULL)
                            }
                        }
                    )
                }
            )

            names(out_list) <- colnames(tbl)

            reactable(tbl,
                columns = out_list,
                borderless = TRUE,
                outlined = FALSE,
                compact = TRUE,
                searchable = !TRUE,
                columnGroups = list(colGroup(
                    html = TRUE,
                    name = paste0(
                        "Possible constraints",
                        if (!is.null(title)) {
                            if (is.character(title)) {
                                if (grepl("(black|white)list", tolower(cls))) {
                                    if (grepl("blacklist", tolower(cls))) {
                                        " using a custom block list"
                                    } else {
                                        if (grepl("whitelist", tolower(cls))) {
                                            " using a custom allow list"
                                        }
                                    }
                                } else {
                                    paste0(" for prescreener '", title, "'")
                                }
                            }
                        },
                        "<br>",
                        "(Click on the respective constraint to show example R-Code)"
                    ),
                    columns = names(out_list),
                    align = "left"
                )),
                theme = reactableTheme(borderWidth = "0px", backgroundColor = "#f2f2f2", color = "#000000"),
                striped = FALSE,
                minRows = nrow,
                defaultPageSize = nrow
            )
        }

    html <- function(x, inline = FALSE) {
        container <- if (inline) htmltools::span else htmltools::div
        container(dangerouslySetInnerHTML = list("__html" = x))
    }


    category_colors <- unique(list_of_prescreeners[, .SD, .SDcols = c("category", "subcategory")], by = c("category", "subcategory"))


    color_fun <- rainbow # tableau_color_pal(palette = "Tableau 20")

    colors_gen <- color_fun(uniqueN(category_colors, by = "category"))


    category_colors[, color := colors_gen[.GRP], by = "category"]
    category_colors[, color := "black", by = "category"]

    # setcolorder(list_of_prescreeners, c("category", "subcategory"))

    reactable(list_of_prescreeners,
        searchable = TRUE,
        showSortable = TRUE,
        filterable = !TRUE,
        highlight = TRUE,
        striped = !TRUE,
        compact = TRUE,
        minRows = nrow(list_of_prescreeners),
        defaultPageSize = nrow(list_of_prescreeners),
        columns = list(
            category = colDef(
                name = "Category", show = TRUE, maxWidth = 2000, minWidth = 100,
                style = function(value) {
                    list("color" = category_colors[category == value, ]$color)
                }
            ),
            subcategory = colDef(
                name = "Subcategory",
                show = TRUE,
                maxWidth = 1010,
                minWidth = 100,
                style = function(value) {
                    list("color" = category_colors[subcategory == value, ]$color)
                }
            ),
            title = colDef(name = "Prescreener title", html = TRUE, resizable = TRUE, minWidth = 200),
            constraints = colDef(show = !TRUE, searchable = TRUE),
            id = colDef(show = FALSE),
            cls = colDef(show = FALSE),
            cls_type = colDef(show = FALSE),
            attributes = colDef(show = FALSE)
        ),
        # if there exists a comment, make row expandable
        details = function(index) {
            prescreener_details_fmt(
                list_of_prescreeners[index, ]
            )
        },
        theme = reactableTheme(color = "#000000")
    )
} else {
    "To show this section, please install packages 'reactable' and 'htmltools'."
}
