#' ggplot to 300 dpi JPG for article submission
#'
#' what this does is save as PDF, then crop it, and convert to JPG which
#'
#' - not change scale, as compared to `ggsave` as JPG and put `dpi = 300`, always set width and height! my favorites are 7-4, 7-5, 9-6
#' - crop white space surrounding (such as from `geom_sf`), with `pdfcrop` or `pdfcrop.pl` for mamba/conda.
#' - convert to JPG with `pdftoppm` (from `poppler-utils`)
#'
#' this will create a subfolder at `here::here('fig')` and save into it.
#'
#' Try adding `+ k.theme`?
#'
#' @param name name to save without extension
#' @param figure ggplot object, empty means the last plot
#' @param ... more parameters for `ggsave`
#' @export
save_jpg_for_submission <- function(name, figure = NULL, ...) {
  if (is.null(figure)) figure <- ggplot2::last_plot()
  pdfname <- paste0(here("fig", name), ".pdf")
  cropname <- paste0(here("fig", name), "-crop.pdf")
  jpgname <- here("fig", name)
  ggplot2::ggsave(pdfname, figure, ..., create.dir = T)
  system(paste("pdfcrop.pl", pdfname))
  system(paste("pdftoppm -jpeg -rx 300 -ry 300", cropname, jpgname))
}

#' Default histogram plot for ktools
#' 
#' @param .data data
#' @param x aes x
#' @param bins bins for histogram
#' @export
gghist <- function(.data, x, bins = 30) {
    x <- rlang::enquo(x)
    .data %>% 
        ggplot() +
        geom_histogram(aes(!!x), bins = bins) +
        k.theme
}

#' Default pie plot for ktools
#' 
#' @param .data data
#' @param x aes x
#' @export
ggpie <- function(.data, x) {
    x <- rlang::enquo(x)
    .data %>%
        count(!!x) %>% 
        ggplot() + 
        geom_bar(aes('', n, fill = !!x), stat = 'identity', width = 1, color = 'white') + 
        coord_polar('y', start = 0) + 
        k.theme
}
#' Default column plot for ktools
#' 
#' @param .data data
#' @param x aes x
#' @param y aes y
#' @param p position **function**
#' @export
ggcol <- function(.data, x, y, p = position_dodge()) {
    x <- rlang::enquo(x)
    y <- rlang::enquo(y)
    .data %>% 
        ggplot() +
        geom_col(aes(!!x, !!y), position = p) +
        coord_flip() +
        k.theme
}

#' Sizing the figure in Python notebook IRKernel
#' 
#' too lazy to type the long options?
#' 
#' @param w repr.plot.width
#' @param h repr.plot.height
#' @examples
#' \dontrun{
#' sizing(7, 4)
#' }
#' @export
sizing <- function(w = 7, h = 4) {
    options(repr.plot.height = h, repr.plot.width = w) 
}

#' Unwrap a facet_wrap plot
#' 
#' Useful when there are a lot of panels 
#' and you want to browse (and optionally save) them one by one
#' 
#' @param g the ggplot2 object created as normal
#' @param plot whether to plot (or just save by turn the next args to TRUE)
#' @param save whether to save the plot to disk
#' @param path where to save them?
#' @inheritDotParams ggplot2::ggsave
#' @return invisible a list of ggplot objects, one for each panels
#' @examples
#' \dontrun{
#' g <- iris %>%
#'   ggplot(aes(Sepal.Length)) +
#'   geom_histogram() +
#'   facet_wrap(~Species)
#' facet_unwrap(g)
#' }
#' @export
facet_unwrap <- function(g, plot = TRUE, save = FALSE, path = '.', ...) {
    on.exit(devAskNewPage(ask = FALSE))
    devAskNewPage(ask = TRUE)
    gb <- ggplot2::ggplot_build(g)
    fc <- names(gb$layout$facet_params$facets)
    cb <- g$data %>%
      dplyr::select(tidyselect::all_of(fc)) %>%
      dplyr::distinct()
    cb[1, , drop = FALSE]
    message("There are ", nrow(cb), " plots")
    o <- list()
    for (r in 1:nrow(cb)) {
      p <- g %+% dplyr::semi_join(g$data, cb[r, , drop = F], by = tidyselect::all_of(fc))
      o[[r]] <- p
      if (plot)
        print(p)
      if (save) {
        name <- paste0(path, "/", paste0(cb[1, ], collapse = "_"))
        dots <- list(...)
        if (exists("device", dots)) {
          name <- paste0(name, dots$device)
        } else {
          name <- paste0(name, "pdf")
        }
        ggplot2::ggsave(name, p, ...)
      }
    }
    invisible(o)
}

#' Plot a 2D matrix as image
#'
#' @param a matrix which columns are to plotted
#'
#' @export
gg_img <- function(m)
{
  if (length(dim(m)) != 2)
    stop("2D only pls")
  n_cols <- ncol(m)
  m %>% 
    as_tibble(.name_repair = "universal") %>%
    rownames_to_column() %>% 
    mutate(rowname=as.numeric(rowname)) %>% 
    pivot_longer(-rowname) %>%
    ggplot() + 
    geom_tile(aes(name, -rowname, fill=value))
}

#' Plot a 2D matrix as lines
#'
#' @param a matrix which columns are to plotted
#'
#' @export
gg_mat <- function(m)
{
  if (length(dim(m)) != 2)
    stop("2D only pls")
  n_cols <- ncol(m)
  m %>% 
    as_tibble(.name_repair = "universal") %>%
    mutate(x = 1:n()) %>% 
    pivot_longer(-x) %>%
    ggplot() +
    geom_line(aes(x, value, color=name))
}

#' Prepare ggplot by groups
#'
#' @param .data data contains variables
#' @param x x-axis
#' @param y y-axis
#' @param g grouping variabe
#'
#' @export
gg_group <- function(.data, x, y, g)
{
  .data %>% 
    ggplot(aes(x = !!rlang::enquo(x), 
               y = !!rlang::enquo(y), 
               color = !!rlang::enquo(g)))
}
