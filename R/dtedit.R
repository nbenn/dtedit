#' Edit a DT table
#'
#' Functions for adding modal-based editing functionality to a DT table in
#' shiny.
#'
#' @param input,output Shiny `input`/`output` objects
#' @param name Output name
#' @param rv_dat Data (as [shiny::reactiveVal()])
#' @param fields Modal fields
#' @param values Values for modal fields
#' @param insert Callback function for data insert operations
#' @param dt_opts Options for `DT`
#'
#' @export
dtedit <- function(input, output, name, rv_dat, fields, values, insert,
                   dt_opts = list(pageLength = 10L)) {

  dt_proxy <- DT::dataTableProxy(dt_name(name))

  output[[dt_name(name)]] <- DT::renderDT(
    rv_dat(),
    options = dt_opts,
    server = FALSE,
    selection = "single",
    rownames = FALSE
  )

  shiny::observeEvent(input[[add_name(name)]], {
    shiny::showModal(insert_modal(name, fields))
  })

  shiny::observeEvent(input[[insert_name(name)]], {

    new_data <- get_input_fields(
      input,
      c(vapply(fields, attr, character(1L), "field_id"), values)
    )

    new_data <- as.data.frame(new_data)

    insert(new_data)

    DT::addRow(dt_proxy, new_data)
    shiny::removeModal()
  })

  output[[name]] <- dtedit_ui(name)

  rv_dat
}

map_types <- function(dat, types = NULL) {

  type_map <- c(
    character = "textInput",
    Date = "dateInput",
    factor = "selectInput",
    integer = "numericInput",
    numeric = "numericInput"
  )

  valid_types <- c(unique(type_map), "textAreaInput")

  res <- vapply(dat, class, character(1L))
  res <- type_map[res]

  names(res) <- colnames(dat)

  stopifnot(!anyNA(res))

  if (!is.null(types)) {

    stopifnot(
      all(names(types) %in% colnames(dat)),
      all(types %in% valid_types)
    )

    res[names(types)] <- types
  }

  res
}

build_field <- function(x, col, typ, val, name = "dtedit", text_width = "100%",
                        textarea_width = "570px", textarea_height = "200px",
                        date_width = "100px", numeric_width = "100px",
                        select_width = "100%") {

  input_choices <- function(x) {
    if (is.factor(x)) levels(x) else unique(x)
  }

  fun <- get(typ, envir = asNamespace("shiny"))
  nme <- paste(name, typ, col, sep = "-")

  res <- switch(
    typ,
    dateInput = fun(nme, col, value = val, width = date_width),
    selectInput = fun(nme, col, choices = input_choices(x), selected = val,
                      width = select_width),
    numericInput = fun(nme, col, value = val, width = numeric_width),
    textAreaInput = fun(nme, col, value = val, width = textarea_width,
                        height = textarea_height),
    textInput = fun(nme, col, value = val, width = text_width),
    stop("Invalid input type")
  )

  attr(res, "field_id") <- nme

  res
}

#' @param dat Data (passed as `data.frame`)
#' @param cols Relevant columns
#' @param types Shiny input types
#' @param values (Optional) values
#' @param ... Forwarded to `build_field()`
#'
#' @rdname dtedit
#' @export
build_modal_fields <- function(dat, cols, types = NULL, values = NULL, ...) {

  if (is.null(values)) {
    values <- list(NULL)
  }

  tmp <- dat[, cols]
  typ <- map_types(tmp, types)

  Map(build_field, tmp, cols, typ, values, MoreArgs = list(...))
}

insert_name <- function(name) {
  paste0(name, "_insert")
}

add_name <- function(name) {
  paste0(name, "_add")
}

dt_name <- function(name) {
  paste0(name, "dt")
}

insert_modal <- function(name, fields, title = "New", size = "m") {
  shiny::modalDialog(
    title = title,
    fields,
    footer = shiny::column(
      shiny::modalButton("Cancel"),
      shiny::actionButton(insert_name(name), "Save"),
      width = 12
    ),
    size = size
  )
}

dtedit_ui <- function(name, label_add = "New") {
  shiny::renderUI(
    shiny::div(
      shiny::actionButton(add_name(name), label_add),
      shiny::br(),
      shiny::br(),
      DT::DTOutput(dt_name(name))
    )
  )
}

get_input_fields <- function(input, fields) {
  get_field <- function(field, inp) inp[[field]]
  lapply(fields, get_field, input)
}
