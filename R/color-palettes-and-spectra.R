
# Color panels and spectra  -----------------------------------------------

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
  "cividis", "viridis", "magma", "plasma")

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
colorpalettes <- c("milo", "jco", "npg", "aaas", "nejm", "lo", "jama", "uc")


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
#'
#' @return Character vector.
#'
#' @export
#'

color_vector <- function(clrp, names = NULL, clrp.adjust = NULL){

  is_value(x = clrp, mode = "character")

  check_one_of(input = clrp, against = c("default", colorpanels), ref.input = "clrp")

  clr_vector <-
    stringr::str_c("clrp", clrp, sep = "_") %>%
    base::parse(text = .) %>%
    base::eval()

  # name the vector if names is specified as a character
  if(base::is.character(names)){

    n_names <- base::length(names)
    n_colors <- base::length(clr_vector)

    if(n_names > n_colors){

      base::warning(glue::glue("Chosen colorpanel '{clrp}' provides {n_colors} colors. Need {n_names} colors. Returning 'default' colorpanel."))

      hues <- base::seq(15, 375, length = n_names + 1)
      clr_vector <- grDevices::hcl(h = hues, l = 65, c = 100)[1:n_names]

    }

    clr_vector <-
      stats::setNames(object = clr_vector[1:n_names], nm = names)

    if(base::is.character(clrp.adjust) && is_named(clrp.adjust)){

      clrp_adjust <- keep_named(input = clrp.adjust)

      clrp_adjust_names <- base::names(clrp_adjust)

      clr_vector <- clr_vector[!base::names(clr_vector) %in% clrp_adjust_names]

      clr_vector <- c(clr_vector, clrp_adjust)

    }

  }

  base::return(clr_vector)

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
all_color_spectra <- function(){

  list(
    "Diverging" = diverging,
    "Sequential single hue" = sequential_single_hue,
    "Sequential multi hue" = sequential_multi_hue
  ) %>% purrr::map(.f = function(i){base::sort(i)})

}
