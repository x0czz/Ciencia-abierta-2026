#!/usr/bin/env Rscript

# Exporta los graficos generados por chunks R en index.qmd a PNG.
# Excluye explicitamente el chunk que inicia en la linea 971.

qmd_path <- "index.qmd"
out_dir <- file.path("output", "graphs", "chunks")
exclude_start_lines <- c(971)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

lines <- readLines(qmd_path, warn = FALSE, encoding = "UTF-8")
chunk_starts <- grep("^\\s*```\\{r", lines)

if (length(chunk_starts) == 0) {
  stop("No se encontraron chunks R en index.qmd")
}

find_chunk_end <- function(start_line, next_start_line, all_lines) {
  search_end <- if (is.na(next_start_line)) length(all_lines) else (next_start_line - 1)

  if (start_line >= search_end) {
    return(search_end)
  }

  rest <- all_lines[(start_line + 1):search_end]
  end_rel <- which(grepl("^\\s*```\\s*$", rest))

  if (length(end_rel) == 0) {
    # Si no hay cierre antes del siguiente inicio, asumimos cierre implicito.
    return(search_end)
  }

  start_line + end_rel[1]
}

extract_chunk_label <- function(header_line) {
  inside <- sub("^```\\{r\\s*", "", header_line)
  inside <- sub("\\}\\s*$", "", inside)
  if (!nzchar(inside)) {
    return("")
  }
  first_part <- strsplit(inside, ",", fixed = TRUE)[[1]][1]
  trimws(first_part)
}

sanitize_name <- function(x) {
  y <- gsub("[^A-Za-z0-9_-]+", "_", x)
  y <- gsub("_+", "_", y)
  y <- gsub("^_+|_+$", "", y)
  if (!nzchar(y)) {
    y <- "chunk"
  }
  y
}

next_starts <- c(chunk_starts[-1], NA_integer_)
chunk_ends <- mapply(
  find_chunk_end,
  start_line = chunk_starts,
  next_start_line = next_starts,
  MoreArgs = list(all_lines = lines),
  USE.NAMES = FALSE
)
valid_idx <- which(!is.na(chunk_ends) & chunk_ends > chunk_starts)

chunks <- data.frame(
  start = chunk_starts[valid_idx],
  end = chunk_ends[valid_idx],
  header = lines[chunk_starts[valid_idx]],
  stringsAsFactors = FALSE
)

chunks$label <- vapply(chunks$header, extract_chunk_label, character(1))
chunks <- chunks[!(chunks$start %in% exclude_start_lines), , drop = FALSE]

if (nrow(chunks) == 0) {
  stop("No hay chunks para procesar luego de aplicar exclusiones")
}

# Plantilla de PNG vacio para detectar chunks sin salida grafica.
blank_png <- tempfile(fileext = ".png")
grDevices::png(blank_png, width = 2000, height = 1200, res = 180)
grDevices::dev.off()
blank_size <- file.info(blank_png)$size
unlink(blank_png)

exec_env <- new.env(parent = globalenv())

saved_count <- 0
error_count <- 0
error_log <- character(0)

for (i in seq_len(nrow(chunks))) {
  chunk <- chunks[i, ]

  if ((chunk$end - chunk$start) <= 1) {
    next
  }

  code_lines <- lines[(chunk$start + 1):(chunk$end - 1)]
  code_text <- paste(code_lines, collapse = "\n")

  if (!nzchar(trimws(code_text))) {
    next
  }

  chunk_name <- if (nzchar(chunk$label)) {
    chunk$label
  } else {
    paste0("linea_", chunk$start)
  }

  file_stem <- sprintf("%03d_%s", i, sanitize_name(chunk_name))
  tmp_png <- tempfile(fileext = ".png")

  chunk_ok <- TRUE
  chunk_error <- NA_character_

  grDevices::png(tmp_png, width = 2000, height = 1200, res = 180)

  exprs <- try(parse(text = code_text), silent = TRUE)
  if (inherits(exprs, "try-error")) {
    chunk_ok <- FALSE
    chunk_error <- as.character(attr(exprs, "condition")$message)
  } else {
    for (expr_idx in seq_along(exprs)) {
      expr <- exprs[[expr_idx]]
      eval_result <- try(withVisible(eval(expr, envir = exec_env)), silent = TRUE)
      if (inherits(eval_result, "try-error")) {
        chunk_ok <- FALSE
        expr_head <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")
        expr_head <- substr(expr_head, 1, 180)
        chunk_error <- sprintf(
          "%s [expr %d: %s]",
          as.character(attr(eval_result, "condition")$message),
          expr_idx,
          expr_head
        )
        break
      }

      if (isTRUE(eval_result$visible)) {
        try(print(eval_result$value), silent = TRUE)
      }
    }
  }

  try(grDevices::dev.off(), silent = TRUE)

  png_size <- if (file.exists(tmp_png)) file.info(tmp_png)$size else NA_real_
  has_plot <- isTRUE(chunk_ok) && !is.na(png_size) && png_size > blank_size

  if (has_plot) {
    out_file <- file.path(out_dir, paste0(file_stem, ".png"))
    file.copy(tmp_png, out_file, overwrite = TRUE)
    saved_count <- saved_count + 1
    message(sprintf("[%d/%d] Guardado: %s (linea %d)", i, nrow(chunks), out_file, chunk$start))
  } else if (!chunk_ok) {
    error_count <- error_count + 1
    msg <- if (!is.na(chunk_error) && nzchar(chunk_error)) chunk_error else "Error no especificado"
    error_log <- c(error_log, sprintf("linea %d: %s", chunk$start, msg))
    message(sprintf("[%d/%d] Error al ejecutar chunk en linea %d: %s", i, nrow(chunks), chunk$start, msg))
  } else {
    message(sprintf("[%d/%d] Sin grafico: chunk en linea %d", i, nrow(chunks), chunk$start))
  }

  if (file.exists(tmp_png)) {
    unlink(tmp_png)
  }
}

message("------------------------------")
message(sprintf("Graficos guardados: %d", saved_count))
message(sprintf("Chunks con error: %d", error_count))
message(sprintf("Directorio salida: %s", normalizePath(out_dir, winslash = "/", mustWork = FALSE)))
if (length(error_log) > 0) {
  message("Detalle de errores:")
  message(paste0(" - ", error_log, collapse = "\n"))
}


