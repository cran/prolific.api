

#' @name prolific.api-package
#'
#' @title R interface to the Prolific API
#'
#' @aliases prolific.api prolific api
#'
#' @description
#' A set of user-friendly functionalities for creating and managing potentially large numbers of studies on
#' the \href{https://www.prolific.co/}{Prolific} platform via its
#' \href{https://docs.prolific.co/docs/api-docs/public/}{API}.
#' The platform is designed for recruiting participants for empirical studies via crowd-sourcing,
#' allowing to apply a number of prescreening characteristics to target specific groups of participants
#' for a study.
#'
#'
#' @details
#' # Object classes
#' \code{prolific.api} provides three \code{\link[methods:ReferenceClasses]{ReferenceClasses}} to access the \href{https://docs.prolific.co/docs/api-docs/public/}{Prolific API},
#' namely
#' \code{\link[=api_access]{api_access}}, \code{\link[=prolific_study]{prolific_study}} and \code{\link[=prolific_prescreener]{prolific_prescreener}}.
#' An overview is provided below.
#'
#' ## api_access
#' \code{\link[=api_access]{api_access}} objects
#' provide functionalities for accessing the \href{https://docs.prolific.co/docs/api-docs/public/}{API},
#' which requires to specify a valid \href{https://docs.prolific.co/docs/api-docs/public/#section/Authentication}{API token}.
#'
#' ## prolific_study
#' \code{\link[=prolific_study]{prolific_study}} objects
#' represent studies to be created or managed on Prolific.
#' Users can create new studies, or retrieve existing studies from Prolific and apply updates to them.
#'
#' ## prolific_prescreener
#' \code{\link[=prolific_prescreener]{prolific_prescreener}} objects
#' characterize the participants to be selected for a certain \code{\link[=prolific_study]{prolific_study}},
#' i.e. the requirements that a person needs to meet to be recruited for the \code{\link[=prolific_study]{study}}.
#'
#' # Authentication
#' A researcher account on \href{https://www.prolific.co/}{\code{Prolific}} is required to use the functionalities of this package.
#' To use this account, a valid \href{https://docs.prolific.co/docs/api-docs/public/#section/Authentication}{Prolific API token} must be specified
#' for authentication. These tokens are \emph{workspace-specific} and
#' can be managed in the \code{Settings -> Go to API token page} menu\cr
#' (\href{https://app.prolific.co/researcher/workspaces/<workspace_id>/settings/tokens}{https://app.prolific.co/researcher/workspaces/workspace_id/settings/tokens} for an existing \code{workspace_id}).
#'
NULL

#  Once you chose the correct workspace \href{https://www.prolific.co/}{\code{Prolific web UI}},
