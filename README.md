# prolific.api

This is an R-Package for creating and managing empirical crowd-sourcing studies via API access to the [Prolific platform](https://www.prolific.co).

It implements a number of user-friendly functionalities for creating and managing potentially large numbers of studies on the [Prolific crowd-sourcing platform](https://www.prolific.co) via its [API](https://docs.prolific.co/docs/api-docs/public/).


# Object classes
`prolific.api` provides three `ReferenceClasses` to access the [Prolific API](https://docs.prolific.co/docs/api-docs/public/),
namely
`api_access`, `prolific_study` and `prolific_prescreener`.
An overview is provided below.

## api_access  objects
provide functionalities for accessing the [API](https://docs.prolific.co/docs/api-docs/public/),
which requires to specify a valid [API token](https://docs.prolific.co/docs/api-docs/public/#section/Authentication).

## prolific_study objects
represent studies to be created or managed on Prolific.
Users can create new studies, or retrieve existing studies from Prolific and apply updates to them.

## prolific_prescreener objects
characterize the participants to be selected for a certain `prolific_study`,
i.e. the requirements that a person needs to meet to be recruited for the `prolific_study`.

# Authentication
A researcher account on [`Prolific`](https://www.prolific.co/) is required to use the functionalities of this package.
To use this account, a valid [Prolific API token](https://docs.prolific.co/docs/api-docs/public/#section/Authentication) must be specified
for authentication. These tokens are \emph{workspace-specific}.
Once you chose the correct workspace [`Prolific web UI`](https://www.prolific.co/), the tokens can be managed in the `Settings -> Go to API token page` menu.
