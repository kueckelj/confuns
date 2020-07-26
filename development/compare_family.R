facet_histogram <- function(data,
                            x.axis,
                            facet,
                            scales = "fixed",
                            binwidth = 0.01,
                            fill = "steelblue"){

  facet <- wrap_strings(input = facet, wrap.in = c("`", "`"))
  x.axis <- wrap_strings(input = x.axis, wrap.in = c("`", "`"))

  ggplot2::ggplot(data = data, mapping = ggplot2::aes_string(x = x.axis)) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = fill) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(facets = stats::as.formula(base::paste("~", facet)),
                        scales = scales)

}



