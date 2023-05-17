## ---- list_fields_methods_create ----
print("<[[list_fields_methods_reactable]]>")

## ---- list_fields_methods_reactable ----
if (suppressMessages(require(reactable) && require(htmltools))) {
    getMethods <-
        function(classname) {
            if (!is.null(classname)) {
                classname <- prolific.api:::.get_RefClass(classname)
            }

            methods <- classname$methods()

            methods <- methods[!methods %in% c(
                ".objectPackage",
                ".objectParent",
                "callSuper",
                "export",
                "field",
                "getClass",
                "getRefClass",
                "import",
                "initFields",
                "initialize",
                "show",
                "show#envRefClass",
                "trace",
                "untrace",
                "usingMethods",
                "copy",
                "$"
            )]

            return(methods)
        }

    getFields <-
        function(classname) {
            classname <- prolific.api:::.get_RefClass(classname)
            fields <- names(classname$fields())
            fields <- fields[!fields %in% c(".internals")]
            fields
        }

    enclose <-
        function(x, delims = "\"") {
            paste0(delims, x, delims)
        }

    html_style <-
        function(x) {
            enclose(paste0(names(x), ": ", Reduce(c, x), collapse = "; "))
        }

    classes <- (c("api_access", "prolific_study", "prolific_prescreener"))

    prefix_format <- function(x) {
        paste0(
            "<span style=",
            html_style(list(
                color = "#a00",
                "font-weight" = "normal"
            )),
            ">",
            x,
            ":",
            "</span> "
        )
    }

    code_format <- function(...) {
        paste0(
            "<span style=",
            html_style(list(
                "color" = "#000000",
                "font-family" = "Courier New",
                "font-size" = "110%",
                "margin-left" = "15px",
                "font-weight" = "bold"
            )),
            ">",
            paste0(..., collapse = ""),
            "</span> "
        )
    }

    classname <- classes[1]
    input_list <- lapply(classes, function(classname) {
        s4_prefix <- prefix_format("S4 Class Syntax")
        rc_prefix <- prefix_format("RefClass Syntax")
        fields_list <- as.list(getFields(classname))
        names(fields_list) <- fields_list
        fields_list[] <- lapply(fields_list, function(field) {
            c(
                paste0(
                    s4_prefix,
                    "</br>",
                    code_format(field, "(", classname, ")")
                ),
                paste0(
                    rc_prefix,
                    "</br>",
                    code_format(classname, "$", field)
                )
            )
        })

        methods_list <- as.list(getMethods(classname))
        names(methods_list) <- methods_list

        sink(file = tempfile())
        suppressMessages(invisible(x <- eval(parse(text = paste0("prolific.api::", classname, "()")))))
        sink(file = NULL)

        methods_list[] <- lapply(methods_list, function(method) {
            frmls <- paste0(names(formals(eval(parse(text = paste0("x$", method))))), collapse = ", ")
            c(
                paste0(
                    s4_prefix,
                    "</br>",
                    code_format(method, "(", classname, if (frmls != "") paste0(", ", frmls), ")")
                ),
                paste0(
                    rc_prefix,
                    "</br>",
                    code_format(classname, "$", method, "(", if (frmls != "") paste0(frmls), ")")
                )
            )
        })
        list(
            fields = fields_list,
            methods = methods_list
        )
    })

    names(input_list) <- classes

    layer <- 1
    index <- NULL

    make_table <-
        function(input_list,
                 layer = 1,
                 final = FALSE) {
            dt <- data.table(val = names(input_list))

            col_def <- list(val = colDef(
                name = "",
                show = TRUE,
                html = TRUE,
                style = if (layer < 4) list(fontWeight = "normal") else NULL
            ))

            end_nodes <- which(vapply(input_list, function(x) !is.list(x), TRUE))

            details_fun <- if (!final) {
                function(index) {
                    make_table(
                        input_list[[index]],
                        layer = layer + 1,
                        final = index %in% end_nodes
                    )
                }
            } else {
                NULL
            }

            if (nrow(dt) == 0 | ncol(dt) == 0) {
                dt <- data.table(val = input_list)
            }

            reactable(dt,
                searchable = FALSE,
                showSortable = FALSE,
                filterable = FALSE,
                highlight = TRUE,
                striped = FALSE,
                compact = TRUE,
                minRows = nrow(dt),
                defaultPageSize = nrow(dt),
                details = details_fun,
                columns = col_def,
                theme = reactableTheme(
                    color = "#000000",
                    borderWidth = "0px",
                    style = list(marginLeft = paste0(3 * (layer - 1), "%")),
                    headerStyle = list(
                        # borderColor = "#ffffff",
                        borderWidth = "0px",
                        cellPadding = "0px -20px"
                    )
                ),
                defaultExpanded = layer <= 1
            )
        }

    make_table(input_list = input_list)
} else {
    "To show this section, please install packages 'reactable' and 'htmltools'."
}
