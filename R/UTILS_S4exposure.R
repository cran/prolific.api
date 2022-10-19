# Function for getting refclass by name or class declaration
.get_RefClass <-
    function(ref_class) {
        switch(match.arg(
            tolower(
                typeof(ref_class)
            ),
            c(
                "closure",
                "character"
            )
        ),
        closure = ref_class,
        character = get(ref_class)
        )
    }

# Function for defining S4 methods (together with the respective generic method if necessary)
.defineMethod <-
    function(method,
             class,
             definition) {
        # Define generic method if undefined
        if (!methods::isGeneric(method, where = globalenv())) {
            if (grepl("<-", method)) {
                methods::setGeneric(method, function(x, ..., value) call("standardGeneric", method))
            } else {
                methods::setGeneric(method, function(x, ...) call("standardGeneric", method))
            } # , where = globalenv())
        }
        # Define class methods (setter / getter function)
        methods::setMethod(method, class, definition)
    }

# Function for exporting all RefClass methods as S4 methods
.exposeMethods_S4 <-
    function(class,
             exclude = NULL) {
        if (!is.null(class)) {
            class <- .get_RefClass(class)
        }

        methods <- class$methods()

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
            "$",
            exclude
        )]

        for (method in methods) {
            # Define S4 method
            .defineMethod(
                method,
                class@className,
                eval(parse(text = paste0("function(x, ...) x$", method, "(...)")))
            )
        }

        return(methods)
    }

.exposeFields_S4 <-
    function(class,
             exclude = NULL) {
        class <- .get_RefClass(class)
        fields <- names(class$fields())
        fields <- fields[!fields %in% c(".internals", exclude)]
        for (field in fields) {
            # Define S4 method: getter function
            .defineMethod(
                field,
                class@className,
                eval(parse(text = paste0("function(x, ...) x$", field)))
            )

            # Define S4 method: setter function
            .defineMethod(
                paste0(field, "<-"),
                class@className,
                eval(parse(text = paste0("function(x, ..., value) {x$", field, " <- value; return(x$.self)}")))
            )
        }

        return(fields)
    }

# Wrapper function for exporting all RefClass fields and methods as S4 methods
.expose_S4 <-
    function(class) {
        output <- list(list(
            fields = .exposeFields_S4(class),
            methods = .exposeMethods_S4(class)
        ))
        names(output) <- class
        return(output)
    }

# Worker function for writing documentation and export statements for the S4 methods
.document_export_S4_method <-
    function(method, class, assignment = FALSE, append = NULL) {
        paste0(
            "#' @rdname ", class,
            "\n",
            "#' @name ", method, append,
            "\n",
            "#' @export ", method,
            "\n",
            "NULL",
            "\n",
            if (assignment) {
                .document_export_S4_method(paste0(method, "<-"), class, assignment = FALSE, append = NULL)
            }
        )
    }

# methods <- export
# Wrapper function for writing documentation and export statements for the S4 methods

methods <- list("a" = list(fields = "x"), b = list(fields = "x"))

.write_S4_documentation <-
    function(methods,
             exclude = NULL) {
        dupl_names <-
            unlist(methods, recursive = TRUE)

        dupl_names <-
            dupl_names[duplicated(dupl_names)]

        alias_pos <-
            gsub("\\..*", "", names(dupl_names))

        output <-
            NULL

        sepfun <- function(i) {
            paste0("\n#", paste0(rep("-", i), collapse = ""))
        }
        i <- 1
        for (i in 1:length(methods)) {
            output <- c(
                output,
                sepfun(50),
                paste0("# ", names(methods)[i]),
                "",
                if (length(add_aliases <-
                    which(alias_pos == names(methods)[i])) > 0) {
                    paste0(
                        sepfun(30),
                        "\n",
                        paste0("## ", names(methods)[i], " -- non-unique fields or methods"),
                        "\n\n",
                        paste0(
                            "#' @rdname ", names(methods)[i],
                            "\n",
                            "#' @name ", dupl_names[add_aliases],
                            "\n",
                            "#' @export ", dupl_names[add_aliases],
                            "\nNULL"
                        )
                    )
                },
                sepfun(30),
                paste0("## ", names(methods)[i], " -- Fields"),
                "",
                .document_export_S4_method(
                    methods[[i]]$fields,
                    names(methods)[i],
                    assignment = TRUE,
                    append =
                        paste0(
                            ifelse(
                                methods[[i]]$fields %in% dupl_names,
                                paste0("-", names(methods)[i]),
                                ""
                            ),
                            ifelse(
                                methods[[i]]$fields %in% exclude,
                                "-field",
                                ""
                            )
                        )
                ),
                sepfun(30),
                paste0("## ", names(methods)[i], " -- Methods"),
                "",
                .document_export_S4_method(
                    methods[[i]]$methods,
                    names(methods)[i],
                    append =
                        paste0(
                            ifelse(
                                methods[[i]]$methods %in% dupl_names,
                                paste0("-", names(methods)[i]),
                                ""
                            ),
                            ifelse(
                                methods[[i]]$methods %in% exclude,
                                "-method",
                                ""
                            )
                        )
                )
            )
        }

        return(paste0(
            output,
            collapse = "\n"
        ))
    }




# Worker function that ensures that names / aliases in .Rd files are unique
# .rename_duplicates <-
# function(x) {
#     flat <-
#         unlist(x, recursive = TRUE)

#     replace <-
#         flat[flat %in% flat[duplicated(flat)]]

#     replace_pos <-
#         gsub("\\.", "$", names(replace))

#     for (i in 1:length(replace)) {
#         pos <-
#             paste0("x$", replace_pos[i])

#         eval(
#             parse(
#                 text =
#                     paste0(
#                         "{\n\t",
#                         "ind <- which(", pos, "%in%", "'", replace[i], "'", ")",
#                         "\n\t",
#                         pos,
#                         "[ind]",
#                         " <- ",
#                         "paste0(",
#                         pos,
#                         "[ind]",
#                         ",",
#                         "'-",
#                         gsub("\\$", "-", gsub("\\$(methods|fields)", "", replace_pos[i])),
#                         "'",
#                         ")",
#                         "\n}"
#                     )
#             )
#         )
#     }
#     return(x)
# }
