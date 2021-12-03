



#' @title Plot GSEA hypergeometric test
#'
#' @description Plots p-value or FDR results of hyper geometrics tests.
#'
#' @param n.gsets Numeric value. Maximal number of gene sets whose
#' results are plotted. The first \code{n.sets} are included starting with the
#' one with the lowest significance value.
#' @param signif.val Character value. Either \emph{'pval'} or \emph{'fdr'}.
#' @param signif.threshold Numeric value. The maximum significance value a gene set
#' can have to be included.
#' @param size.by Character value or NULL. If character, the variable whose values
#' are displayed by size. Either \emph{'geneset'}, the number of genes the gene set
#' contains, or \emph{'overlap'}, the number of genes that overlapped between
#' the provided gene signature and the gene set.
#' @param remove character value or NULL. If character, regular expression given
#' to \code{pattern} of \code{stringr::str_remove_all()}. Useful to adjust gene set
#' names in the plot for aesthetic purposes.
#' @inherit argument_dummy params
#' @param ... Additional arguments given to \code{scale_color_add_on()}.
#'
#' @return A ggplot.
#' @export
#'
plot_gsea_dot <- function(object, ...){

  UseMethod(generic = "plot_gsea_dot", object = object)

}

#' @rdname plot_gsea_dot
#' @export
plot_gsea_dot.hyp <- function(object,
                              n.gsets = 20,
                              signif.val = "fdr",
                              signif.threshold = 0.05,
                              color.by = "fdr",
                              size.by = "geneset",
                              pt.size = 2,
                              pt.color = "blue4",
                              pt.clrsp = "plasma",
                              remove = NULL,
                              ...){

  check_one_of(
    input = signif.val,
    against = c("pval", "fdr")
  )

  check_one_of(
    input = color.by,
    against = c("pval", "fdr")
  )

  if(!base::is.null(size.by)){

    check_one_of(
      input = size.by,
      against = c("overlap", "geneset"),
    )

  }

  df <-
    tibble::as_tibble(object$data) %>%
    dplyr::filter(!!rlang::sym(signif.val) < {{signif.threshold}}) %>%
    dplyr::slice_head(n = n.gsets) %>%
    dplyr::arrange(pval)

  if(base::is.character(remove)){

    is_value(remove, mode = "character")

    df[["label"]] <- stringr::str_remove_all(string = df[["label"]], pattern = remove)

  }

  df[["label"]] <- base::factor(x = df[["label"]], levels = base::unique(df[["label"]]))

  params <- adjust_ggplot_params(params = list(size = pt.size, color = pt.color), sep = ".")

  ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = .data[[signif.val]], y = label)) +
    ggplot2::layer(
      geom = "point",
      stat = "identity",
      position = "identity",
      mapping = ggplot2::aes_string(color = color.by, size = size.by),
      params = params
      ) +
    scale_color_add_on(aes = "color", variable = df[["fdr"]], clrsp = pt.clrsp, ...) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = base::toupper(x = signif.val),
      y = NULL,
      size = make_capital_letters(string = size.by),
      color = base::toupper(x = signif.val)
    ) +
    ggplot2::scale_x_continuous(labels = function(x){ base::format(x, scientific = TRUE) })

}

#' @rdname plot_gsea_dot
#' @export
plot_gsea_dot.list <- plot_gsea_dot.hyp






