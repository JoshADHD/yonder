#' Modal dialogs
#'
#' Modals are a flexible alert window, which disable interaction with the page
#' behind them. Modals may include inputs or buttons or simply include text.
#'
#' @param title A character string specifying the modal's title.
#'
#' @param body A character string specifying the body of the modal or
#'   custom element to use as the body of the modal, defaults to `NULL`.
#'
#' @param footer Custom tags to include at the bottom of the modal, defaults to
#'   `NULL`.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   shinyApp(
#'     ui = container(
#'
#'     ),
#'     server = function(input, output, session) {
#'       modalEvent(onInitialized(), {
#'         modal("Initialize", "Session initialized", size = "large")
#'       })
#'     }
#'   )
#' }
#'
modalEvent <- function(event, modal, domain = getDefaultReactiveDomain()) {
  priority <- -5000
  pframe <- parent.frame()
  quoted <- FALSE

  eventFunc <- shiny::exprToFunction(event, pframe, quoted)
  eventFunc <- wrapFunctionLabel(eventFunc, "modalEvent", ..stacktraceon = TRUE)

  modalFun <- shiny::exprToFunction(modal, pframe, quoted)

  label <- sprintf(
    "modalEvent(%s)",
    paste(deparse(body(eventFunc)), collapse = "\n")
  )

  initialized <- FALSE

  o <- observe({
    e <- eventFunc()

    init <- if (is.null(e)) FALSE else e == "__INITIALIZED"

    if (!init && !initialized) {
      initialized <<- TRUE
      return(NULL)
    }

    modal <- isolate(modalFun())

    domain$sendCustomMessage("dull:modal", list(
      content = HTML(as.character(modal))
    ))

  }, label = label, suspended = FALSE, priority = priority, domain = domain,
  autoDestroy = TRUE, ..stacktraceon = FALSE)

  invisible(o)
}

#' @rdname modalEvent
#' @export
onInitialized <- function() {
  invisible("__INITIALIZED")
}

#' @rdname modalEvent
#' @export
modal <- function(title, body, footer = NULL) {
  tags$div(
    class = collate(
      "modal-dialog"
    ),
    role = "document",
    tags$div(
      class = "modal-content",
      tags$div(
        class = "modal-header",
        tags$h5(class = "modal-title", title),
        tags$button(
          type = "button",
          class = "close",
          `data-dismiss` = "modal",
          `aria-label` = "Close",
          fontAwesome("window-close")
        )
      ),
      tags$div(
        class = "modal-body",
        tags$div(
          class = "container-fluid",
          body
        )
      ),
      if (!is.null(footer)) {
        tags$div(
          class = "modal-footer",
          footer
        )
      }
    )
  )
}
