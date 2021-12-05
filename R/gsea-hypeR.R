



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
#' @param remove Character value or NULL. If character, regular expression given
#' to \code{pattern} of \code{stringr::str_remove_all()}. If character, regular expression given to
#' \code{pattern} of \code{stringr::str_replace_all()}. Used to adjust gene set names.
#' @param replace Character vector of length 2 or NULL. If character vector, two regular expressions.
#' First is given to argument \code{pattern} and second is given to argument \code{replacement}
#' of \code{stringr::str_replace_all()}. Used to adjust gene set names.
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
                              remove = "MF.GO|RCTM|CC.GO|HM",
                              replace = c("_", " "),
                              ...){

  df <- tibble::as_tibble(object$data)

  plot_gsea_dot(
    object = df,
    n.gsets = n.gsets,
    signif.val = signif.val,
    signif.threshold = signif.threshold,
    color.by = color.by,
    size.by = size.by,
    pt.size = pt.size,
    pt.color = pt.color,
    pt.clrsp = pt.clrsp,
    remove = remove,
    replace = replace,
    ...
  )


}

#' @rdname plot_gsea_dot
#' @export
plot_gsea_dot.list <- plot_gsea_dot.hyp

#' @rdname plot_gsea_dot
#' @export
plot_gsea_dot.data.frame <- function(object,
                                     n.gsets = 20,
                                     signif.val = "fdr",
                                     signif.threshold = 0.05,
                                     alpha.by = NULL,
                                     alpha.trans = "identity",
                                     color.by = "fdr",
                                     color.trans = "identity",
                                     size.by = NULL,
                                     size.trans = "identity",
                                     pt.alpha = 0.9,
                                     pt.size = 2,
                                     pt.color = "blue4",
                                     pt.clrsp = "plasma",
                                     remove = "^.+?(?=_)",
                                     replace = c("_", " "),
                                     ...){

  df <- object

  check_one_of(
    input = signif.val,
    against = c("pval", "fdr")
  )

  if(!base::is.null(color.by)){

    check_one_of(
      input = color.by,
      against = c("pval", "fdr")
    )

  }

  if(!base::is.null(size.by)){

    check_one_of(
      input = size.by,
      against = c("overlap", "geneset"),
    )

  }

  df <-
    dplyr::filter(df, !!rlang::sym(signif.val) < {{signif.threshold}}) %>%
    dplyr::arrange({{signif.val}}, .by_group = TRUE) %>%
    dplyr::slice_head(n = n.gsets)

  if(base::is.character(remove)){

    is_value(remove, mode = "character")

    df[["label"]] <- stringr::str_remove(string = df[["label"]], pattern = remove)

  }

  if(confuns::is_vec(x = replace, mode = "character", of.length = 2, fdb.fn = "message", verbose = FALSE)){

    df[["label"]] <- stringr::str_replace_all(string = df[["label"]], pattern = replace[1], replacement = replace[2])

  }

  df[["label"]] <- base::factor(x = df[["label"]], levels = base::unique(df[["label"]]))

  params <- adjust_ggplot_params(params = list(size = pt.size, color = pt.color, alpha = pt.alpha), sep = ".")

  plot_dot_plot_1d(
    df = df,
    x = signif.val,
    y = "label",
    alpha.by = alpha.by,
    alpha.trans = alpha.trans,
    color.by = signif.val,
    color.trans = color.trans,
    shape.by = shape.by,
    size.by = size.by,
    size.trans = size.trans,
    pt.alpha = pt.alpha,
    pt.color = pt.color,
    pt.shape = pt.shape,
    pt.size = pt.size,
    ...
  ) +
    ggplot2::labs(
      x = base::toupper(x = signif.val),
      y = NULL,
      size = make_capital_letters(string = size.by),
      color = base::toupper(x = signif.val)
    ) +
    ggplot2::scale_x_continuous(labels = function(x){ base::format(x, scientific = TRUE) })

}






