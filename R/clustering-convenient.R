



# S4 - classes ------------------------------------------------------------

### Parent

DataConv <- setClass(Class = "DataConv",
                     slots = list(
                       data_categorical = "data.frame",
                       data_numeric = "matrix",
                       data_logical = "data.frame",
                       default = "list",
                       key_name = "character",
                       observations = "character",
                       scale = "logical",
                       variables_categorical = "character",
                       variables_logical = "character",
                       variables_numeric = "character"
                     ))

AnalysisConv <- setClass(Class = "AnalysisConv",
                         slots = list(
                           data_categorical = "data.frame",
                           data_numeric = "matrix",
                           data_logical = "data.frame",
                           default = "list",
                           dim_red = "list",
                           clustering = "ClusterConv"
                         ), contains = "DataConv")




### Hierarchical Clustering

HclustConv <- setClass(Class = "HclustConv",
                       slots = list(
                         dist_matrices = "list",
                         results = "list"
                       )
                       )


### Kmeans Clustering

KmeansConv <- setClass(Class = "KmeansConv",
                       slots = list(
                         results = "list"
                       )
                       )


### Pam Clustering

PamConv <- setClass(Class = "PamConv",
                    slots = list(
                      results = "list"
                    )
                    )

### Clustering

ClusterConv <- setClass(Class = "ClusterConv",
                        slots = list(
                          hclust = "HclustConv",
                          kmeans = "KmeansConv",
                          pam = "PamConv"
                        ))
