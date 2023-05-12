#' Edit a DT table
#'
#' Functions for adding modal-based editing functionality to a DT table in
#' shiny.
#'
#' @param input,output Shiny `input`/`output` objects
#' @param name Output name
#' @param rv_dat Data (as [shiny::reactiveVal()])
#' @param cols Columns to show
#' @param fields Modal fields
#' @param values Values for modal fields
#' @param insert,delete,update Callback functions for data insert, delete and
#' update operations
#' @param dt_opts Options for `DT`
#'
#' @export
dtedit <- function(input, output, name, rv_dat, cols, fields, values, insert,
                   delete, update, dt_opts = list(pageLength = 10L)) {

  dt_proxy <- DT::dataTableProxy(dt_name(name))

  output[[dt_name(name)]] <- DT::renderDT(
    rv_dat()[, cols],
    options = dt_opts,
    selection = "single",
    rownames = FALSE
  )

  shiny::observeEvent(input[[add_name(name)]], {
    shiny::showModal(insert_modal(name, fields))
  })

  shiny::observeEvent(input[[rm_name(name)]], {
    if (is_row_selected(input, name)) {
      shiny::showModal(delete_modal(name))
    }
  })

  shiny::observeEvent(input[[edit_name(name)]], {
    if (is_row_selected(input, name)) {
      shiny::showModal(update_modal(name, fields))
    }
  })

  shiny::observeEvent(input[[insert_name(name)]], {

    new_data <- get_input_fields(
      input,
      c(vapply(fields, attr, character(1L), "field_id"), values)
    )

    rv_dat(insert(as.data.frame(new_data), rv_dat()))

    DT::replaceData(dt_proxy, rv_dat()[, cols], rownames = FALSE)

    shiny::removeModal()
  })

  shiny::observeEvent(input[[delete_name(name)]], {

    dat <- rv_dat()
    row <- get_selected_row(input, name, nrow(dat))

    if (length(row)) {

      rv_dat(delete(row, dat))

      DT::replaceData(dt_proxy, rv_dat()[, cols], rownames = FALSE)
    }

    shiny::removeModal()
  })

  shiny::observeEvent(input[[update_name(name)]], {

    dat <- rv_dat()
    row <- get_selected_row(input, name, nrow(dat))

    if (length(row)) {

      new_data <- get_input_fields(
        input,
        c(vapply(fields, attr, character(1L), "field_id"), values)
      )

      rv_dat(update(row, as.data.frame(new_data), dat))

      DT::replaceData(dt_proxy, rv_dat()[, cols], rownames = FALSE)
    }

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

  valid_types <- c(
    unique(type_map), "textAreaInput", "shinytreeview::treecheckInput"
  )

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

default_numeric_value <- function(x) {
  list(value = stats::median(x))
}

default_select_value <- function(x) {
  list(choices = levels(x))
}

build_field <- function(typ, name, label, args) {

  fun <- strsplit(typ, "::")[[1L]]

  if (length(fun) == 2L) {
    ns <- asNamespace(fun[1L])
    fun <- fun[2L]
  } else {
    stopifnot(length(fun) == 1L)
    ns <- asNamespace("shiny")
  }

  fun <- get(fun, envir = ns)
  res <- do.call(fun, c(list(name, label), args))

  attr(res, "field_id") <- name

  res
}

#' @param dat Data (passed as `data.frame`)
#' @param cols Relevant columns
#' @param types Shiny input types
#' @param args (Optional) input field arguments
#' @param name_prefix Prefix for input names
#'
#' @rdname dtedit
#' @export
build_modal_fields <- function(dat, cols = names(dat), types = NULL,
                               args = NULL, name_prefix = "dtedit") {

  fix_num <- function(typ, args, dat) {
    sel <- names(
      typ[typ == "numericInput" & !names(typ) %in% names(args)[lengths(args)]]
    )
    args[sel] <- lapply(dat[, sel, drop = FALSE], default_numeric_value)
    args
  }

  fix_sel <- function(typ, args, dat) {
    sel <- names(
      typ[typ == "selectInput" & !names(typ) %in% names(args)[lengths(args)]]
    )
    args[sel] <- lapply(dat[, sel, drop = FALSE], default_select_value)
    args
  }

  if (is.null(args)) {
    args <- list()
  } else {
    stopifnot(!is.null(names(args)), all(names(args) %in% cols))
  }

  miss <- setdiff(cols, names(args))
  args[miss] <- rep(list(list()), length(miss))

  stopifnot(
    setequal(names(args), cols), all(vapply(args, is.list, logical(1L)))
  )

  tmp <- dat[, cols]
  typ <- map_types(tmp, types)

  args <- fix_num(typ, args, dat)
  args <- fix_sel(typ, args, dat)

  args <- args[cols]
  typ <- typ[cols]
  nme <- paste(name_prefix, sub("^.+::", "", typ), cols, sep = "-")

  Map(build_field, typ, nme, cols, args)
}

insert_name <- function(name) {
  paste0(name, "_insert")
}

delete_name <- function(name) {
  paste0(name, "_delete")
}

update_name <- function(name) {
  paste0(name, "_update")
}

add_name <- function(name) {
  paste0(name, "_add")
}

rm_name <- function(name) {
  paste0(name, "_rm")
}

edit_name <- function(name) {
  paste0(name, "_edit")
}

dt_name <- function(name) {
  paste0(name, "dt")
}

selected_row_name <- function(name) {
  paste0(dt_name(name), "_rows_selected")
}

insert_modal <- function(name, fields, title = "New", size = "m") {

  shiny::modalDialog(
    title = title,
    fields,
    footer = shiny::column(
      cancel_modal(),
      shiny::actionButton(insert_name(name), "Save"),
      width = 12
    ),
    size = size
  )
}

delete_modal <- function(name, title = "Delete", size = "m") {

  shiny::modalDialog(
    title = title,
    shiny::p("Are you sure you want to delete this record?"),
    footer = shiny::column(
      cancel_modal(),
      shiny::actionButton(delete_name(name), "Delete"),
      width = 12
    ),
    size = size
  )
}

update_modal <- function(name, fields, title = "Edit", size = "m") {

  shiny::modalDialog(
    title = title,
    fields,
    footer = shiny::column(
      cancel_modal(),
      shiny::actionButton(update_name(name), "Save"),
      width = 12
    ),
    size = size
  )
}

dtedit_ui <- function(name, label_add = "New", label_rm = "Delete",
                      label_edit = "Edit") {

  shiny::renderUI(
    shiny::div(
      shiny::actionButton(add_name(name), label_add),
      shiny::actionButton(edit_name(name), label_edit),
      shiny::actionButton(rm_name(name), label_rm),
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

cancel_modal <- function() shiny::modalButton("Cancel")

get_selected_row <- function(input, name, max) {

  row <- input[[selected_row_name(name)]]

  if (length(row)) {
    stopifnot(length(row) == 1L, row > 0L, row <= max)
  }

  row
}

is_row_selected <- function(input, name) {
  shiny::isTruthy(input[[selected_row_name(name)]])
}
