

# Argument inputs ---------------------------------------------------------

pretty_bar_positions <- c("Stacked" = "stack", "Dodged" = "dodge", "Filled" = "fill")



# Descriptive statistics --------------------------------------------------

testable_plottypes <- c("boxplot", "violinplot")


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


pretty_colorpanels_list <-
  list(
    Journals = pretty_journals_clrp_vector,
    Milo = "milo",
    ggplot2 = pretty_ggplot2_clrp_vector
  )
