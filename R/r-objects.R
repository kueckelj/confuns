

# Argument inputs ---------------------------------------------------------

pretty_bar_positions <- c("Stacked" = "stack", "Dodged" = "dodge", "Filled" = "fill")

# Colors ------------------------------------------------------------------

viridis_options <- c("cividis", "inferno", "magma", "plasma", "viridis")

pretty_journals_clrp_vector <- c("Journal of Oncology" = "jco",
                                 "Nature Publishing Group" = "npg",
                                 "American Association for the Advancement" = "aaas",
                                 "New England Journal of Medicine" = "nejm",
                                 "Lancet Oncology" = "lo",
                                 "The Journal of the American Medical Association" = "jama",
                                 "University of Chicago" = "uc"
)

pretty_ggplot2_clrp_vector <- c("Greyscale" = "greyscale",
                                "Default" = "default")

# use pretty_colorpalettes_list instead!
pretty_colorpanels_list <-
  list(
    Journals = pretty_journals_clrp_vector,
    Milo = "milo",
    ggplot2 = pretty_ggplot2_clrp_vector,
    `Viridis Options` = viridis_options
  )

pretty_colorpalettes_list <-
  list(
    Journals = pretty_journals_clrp_vector,
    Milo = "milo",
    ggplot2 = pretty_ggplot2_clrp_vector,
    `Viridis Options` = viridis_options
  )

# Descriptive statistics --------------------------------------------------

testable_plottypes <- c("boxplot", "violinplot")

pairwise_tests <- c("t.test", "wilcox.test")
groupwise_tests <- c("anova", "kruskal.test")

pretty_plottypes <- c("Violinplot" = "violinplot",
                      "Ridgeplot" = "ridgeplot",
                      "Densityplot" = "density",
                      "Boxplot" = "boxplot"
)


pretty_stattests <- c("T-test" = "t.test",
                      "Wilcox" = "wilcox.test",
                      "ANOVA" = "anova",
                      "Kruskal" = "kruskal.test")

pretty_stattests_pairwise <- c("None" = "none", pretty_stattests[1:2])
pretty_stattests_groupwise <- c("None" = "none", pretty_stattests[3:4])


# ggplot2 -----------------------------------------------------------------

fill_shapes <- 21:25
color_shapes <- 0:20



# Predefined feedback strings ---------------------------------------------


overwrite_hint <- " Set argument 'overwrite' to TRUE in order to allow overwritting."



