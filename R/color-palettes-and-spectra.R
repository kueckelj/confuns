

#' @title Create sub cluster colors
#'
#' @description Creates a vector of colors based on sub grouping.
#'
#' @param input Character vector of group names.
#' @inherit scale_color_add_on params
#' @param clr.bottom Character value. The color up to which the sub panel
#' for each sub grouping is created.
#' @param alphabetically If \code{TRUE}, sub groups are sorted alphabetically before
#' being mapped to colors.
#' @param into Character vector of length two. A combination of \emph{'main', 'sub'}.
#' If \code{into = c(\emph{'main', 'sub'})} string parts before \code{sep} are
#' considered the main grouping and strings parts after \code{sep} are considered
#' the sub part. Works the other way around for \code{into = c(\emph{'sub', 'main'})}.
#' @param sep Character value. Is used to cut the sub grouping
#' information from the variabel in \code{main}.
#' @param offset Integer value. If 1, the default, the last sub group is mapped
#' to the color that comes directly before \code{clr.bottom}.
#'
#'
#' @return Named character vector. Names are the groups. Values are the colors.
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(confuns)
#'
#' df <-
#'  mutate(
#'   .data = iris,
#'   numbers = sample(1:3, size = nrow(iris), replace = T),
#'   spec_sub = str_c(Species, numbers, sep = ".") %>% as.factor()
#'  )
#'
#' adjust_out <- adjust_sub_colors(input = df$spec_sub, clrp = "npg", sep = ".")
#'
#' print(adjust_out)
#'
#' ggplot(data = df, mapping = aes(x = Sepal.Width, y = Sepal.Length)) +
#'  geom_point(mapping = aes(color = spec_sub), size = 4) +
#'  scale_color_add_on(
#'   clrp = "npg",
#'   variable = df$spec_sub,
#'   clrp.adjust = adjust_out
#'   )
#'
adjust_sub_colors <- function(input,
                              clrp,
                              clrp.adjust = NULL,
                              clr.bottom = "white",
                              offset = 1,
                              into = c("main", "sub"),
                              sep = "_",
                              alphabetically = TRUE){

  offset <- base::as.integer(offset)
  is_value(offset, mode = "integer") # trans successful?

  if(base::is.character(input)){

    groups <- base::unique(input)

  } else if(base::is.factor(input)){

    groups <- base::levels(input)

  }

  df <-
    base::data.frame(groups = groups) %>%
    tidyr::separate(
      col = groups,
      into = c("main", "sub"),
      sep = sep
      ) %>%
    dplyr::mutate(
      dplyr::across(
        .fns = base::as.factor
      )
    )

  main_color_vec <-
    color_vector(
      clrp = clrp,
      names = base::levels(df[["main"]]),
      clrp.adjust = clrp.adjust
    )

  sub_list <- base::vector(mode = "list", length = base::length(main_color_vec))

  for(i in base::seq_along(main_color_vec)){

    clr <- base::unname(main_color_vec)[i]
    main_group <- base::names(main_color_vec)[i]

    sub_groups <-
      dplyr::filter(df, main == {{main_group}}) %>%
      dplyr::pull(var = sub) %>%
      base::droplevels() %>%
      base::levels()

    n_sub_groups <- base::length(sub_groups)

    clr_palette <-
      grDevices::colorRampPalette(colors = c(clr, clr.bottom))(n_sub_groups+offset)

    if(base::isTRUE(alphabetically)){

      sub_groups <- base::sort(sub_groups)

    }

    sub_groups <- stringr::str_c(main_group, sub_groups, sep = sep)

    sub_list[[i]] <-
      purrr::set_names(
        x = clr_palette[1:n_sub_groups],
        nm = sub_groups
      )

  }

  color_vec_out <- purrr::flatten_chr(sub_list)

  return(color_vec_out)

}


#' @title Color spectra names
#'
#' @description Collection of color-spectra names.
#'
#' @export

sequential_single_hue <-
  c("Grays", "Light Grays", "Blues 2", "Blues 3", "Purples 2", "Purples 3", "Reds 2", "Reds 3", "Greens 2",
    "Greens 3", "Oslo")

#' @rdname sequential_single_hue
#' @export
sequential_multi_hue <-
  c("Purple-Blue", "Red-Purple", "Red-Blue", "Purple-Orange", "Purple-Yellow", "Blue-Yellow",
  "Green-Yellow", "Red-Yellow", "Heat", "Heat 2", "Terrain", "Terrain 2", "Viridis", "Plasma",
  "Inferno", "Dark Mint", "Mint", "BluGrn", "Teal", "TealGrn", "Emrld", "BluYl", "ag_GrnYl", "Peach",
  "PinkYl", "Burg", "BurgYl", "RedOr", "OrYel", "Purp", "PurpOr", "Sunset", "Magenta", "SunsetDark",
  "ag_Sunset", "BrwnYl", "YlOrRd", "YlOrBr", "OrRd", "Oranges", "YlGn", "YlGnBu", "Reds", "RdPu", "PuRd",
  "Purples", "PuBuGn", "PuBu", "Greens", "BuGn", "GnBu", "BuPu", "Blues", "Lajolla", "Turku", "inferno",
  "cividis", "viridis", "magma", "plasma", "turbo")

#' @rdname sequential_single_hue
#' @export
diverging <-
  c("Blue-Red", "Blue-Red 2", "Blue-Red 3", "Red-Green", "Purple-Green", "Purple-Brown", "Green-Brown",
    "Blue-Yellow 2", "Blue-Yellow 3", "Green-Orange", "Cyan-Magenta", "Tropic", "Broc", "Cork",
    "Vik", "Berlin", "Lisbon", "Tofino")



#' @title Color palettes
#'
#' @description Available discrete color palettes.
#'
#' @details
#'
#' \itemize{
#'  \item{\emph{"milo"}: MILO Research Group  (n = 20)}
#'  \item{\emph{"jco"}: Journal of Oncology (n = 10)}
#'  \item{\emph{"npg"}: Nature Publishing Group (n = 10)}
#'  \item{\emph{"aaas"}: American Association for the Advancement (n = 10)}
#'  \item{\emph{"nejm"}: New England Journal of Medicine (n = 10)}
#'  \item{\emph{"lo"}: Lancet Oncology {n = 10}}
#'  \item{\emph{"jama"}: The Journal of the American Medical Association (n = 10)}
#'  \item{\emph{"uc"}: University Chicago (n = 10)}
#' }
#'
#' @export
colorpalettes <- c("milo", "jco", "npg", "aaas", "nejm", "lo", "jama", "uc", "sifre")


#' @title MILO Research Group - color palette
#' @export
clrp_milo <- c("#C4432A", "#3A389C", "#478C3D", "#FFD700", "steelblue", "#FFA500", "#800000FF", "#64DB74",  "#8B2252", "#56D9ED", "#C934BD",
                 "#C9B972", "#4F1211", "#CD4F39", "#00868B", "#8B7355", "#CAFF70", "#2C6CA3", "#525252", "brown")

#' @title Journal of Oncology - color palette
#' @export
clrp_jco <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF", "#003C67FF", "#8F7700FF", "#3B3B3BFF", "#A73030FF", "#4A6990FF")

#' @title Nature Publishing Group - color palette
#' @export
clrp_npg <- c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#8491B4FF", "#91D1C2FF", "#DC0000FF", "#7E6148FF", "#B09C85FF")

#' @title American Association for the Advancement - color palette
#' @export
clrp_aaas <- c("#3B4992FF", "#EE0000FF", "#008B45FF", "#631879FF", "#008280FF", "#BB0021FF", "#5F559BFF", "#A20056FF", "#808180FF", "#1B1919FF")

#' @title New England Journal of Medicine- color palette
#' @export
clrp_nejm <- c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF", "#7876B1FF", "#6F99ADFF", "#FFDC91FF", "#EE4C97FF","#b15928","#ffff33")

#' @title Lancet Oncology - color palette
#' @export
clrp_lo <- c("#00468BFF", "#ED0000FF", "#42B540FF", "#0099B4FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "#ADB6B6FF", "#1B1919FF", "#800000FF")

#' @title The Journal of the American Medical Association - color palette
#' @export
clrp_jama <- c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#6A6599FF", "#80796BFF", "#008B45FF", "#fdbf6f", "#377eb8")

#' @title University of Chicago - color palette
#' @export
clrp_uc <- c("#800000FF", "#767676FF", "#FFA319FF", "#8A9045FF", "#155F83FF", "#C16622FF", "#8F3931FF", "#58593FFF", "#350E20FF", "#1F77B4FF")

#' @title Simon Frerich - color palette
#' @export
clrp_sifre <- c("#F44336","#FF8AC4","#9C27B0","#673AB7","#3F51B5","#03A9f4","#89CFF0","#009688", "#228B22BF", "#8BC34A","#CDDC39","#FFEB3B","#FFC107","#FF9800")

#' @title Elena Grabis - color palette
#' @export
clrp_egr <- c("#FF1744", "#E57373", "#BA68C8", "#9575CD", "#7986CB", "#64B5F6", "#4FC3F7", "#EF5350", "#EC407A", "#AB47BC", "#5C6BC0")

n_colors <-
  list(
    milo = length(clrp_milo),
    jco = length(clrp_jco),
    npg = length(clrp_npg),
    aaas = length(clrp_aaas),
    nejm = length(clrp_nejm),
    lo = length(clrp_lo),
    jama = length(clrp_jama),
    uc = length(clrp_uc),
    sfire = length(clrp_sifre),
    egr = length(clrp_egr),
    Accent = 8,
    Dark2 = 8,
    Greys = 9,
    Paired = 12,
    Pastel1 = 9,
    Pastel2 = 8,
    Set1 = 9,
    Set2 = 8,
    Set3 = 12
  )

#' @title Get vector of colors
#'
#' @description Returns a vector of color codes of the respective palette
#'
#' @param clrp Character value. Denotes the color palette of interest. Run \code{all_colorpalettes()} to
#' obtain valid inputs.
#' @param names Character vector or NULL. Assigns names to the color vector (in the same order).
#' @param clrp.adjust Named character vector or NULL. If character, it adjusts the
#' colors that are used to represent the groups. Names of the input vector must refer
#' to the group and the respective named element denotes the color with which to
#' represent the group.
#' @param n.colors Numeric value. Must be specified if \code{clrp} is set to \emph{'default'}
#' and \code{names} is NULL.
#'
#' @return Character vector.
#'
#' @export
#'

color_vector <- function(clrp, names = NULL, clrp.adjust = NULL, n.colors = NA){

  is_value(x = clrp, mode = "character")

  check_one_of(
    input = clrp,
    against = all_color_palettes_vec(),
    suggest = TRUE
    )

  # check how many colors are needed
  if(base::is.character(names)){

    n.colors <- base::length(names)

  } else if(base::is.na(n.colors) | !base::is.numeric(n.colors)){

    if(clrp %in% c("default", viridis_options)){

      stop("If `clrp` among 'default' or viridis options, please specify either `names` or `n.colors`.")

    } else {

      n.colors <- n_colors[[clrp]]

    }

  }

  # pick the color palette
  if(clrp == "default"){

    clr_vector <- scales::hue_pal()(n.colors)

  } else if(clrp %in% viridis_options){

    clr_vector <- viridis::viridis(n = n.colors, option = clrp)

  } else if(clrp %in% RColorBrewer_options){

    clr_vector <- RColorBrewer::brewer.pal(n = n.colors, name = clrp)

  } else {

    clr_vector <-
      stringr::str_c("clrp", clrp, sep = "_") %>%
      base::parse(text = .) %>%
      base::eval()

  }

  # name the vector if names is specified as a character
  if(base::is.character(names)){

    n_names <- base::length(names)
    n_colors <- base::length(clr_vector)

    if(n_names > n_colors){

      warning(glue::glue("Chosen colorpalette '{clrp}' provides {n_colors} colors. Need {n_names} colors. Returning 'default' colorpalette."))

      hues <- base::seq(15, 375, length = n_names + 1)
      clr_vector <- grDevices::hcl(h = hues, l = 65, c = 100)[1:n_names]

    }

    clr_vector <-
      stats::setNames(object = clr_vector[1:n_names], nm = names)

    if(base::is.character(clrp.adjust) && is_named(clrp.adjust)){

      clrp_adjust <- keep_named(input = clrp.adjust)

      clrp_adjust_names <- base::names(clrp_adjust)

      clr_vector <- clr_vector[!base::names(clr_vector) %in% clrp_adjust_names]

      clr_vector <- c(clr_vector, clrp_adjust)[names] # maintain order

    }

  }

  return(clr_vector)

}


#' @title Color palette names
#' @description Returns all currently valid color palettes or -spectra.
#' @return A named list.
#' @export

all_color_palettes <- function(){

  pretty_colorpalettes_list

}

#' @rdname all_color_palettes
#' @export
all_color_palettes_vec <- function(){

  base::unname(pretty_colorpalettes_list) %>%
    purrr::flatten_chr() %>%
    base::unname() %>%
    base::sort()

}


#' @rdname all_color_palettes
#' @export
all_color_spectra <- function(){

  list(
    "Diverging" = diverging,
    "Sequential single hue" = sequential_single_hue,
    "Sequential multi hue" = sequential_multi_hue
  ) %>% purrr::map(.f = function(i){base::sort(i)})

}

#' @rdname all_color_palettes
#' @export
all_color_spectra_vec <- function(){

  base::unname(all_color_spectra()) %>%
    purrr::flatten_chr() %>%
    base::sort()

}

