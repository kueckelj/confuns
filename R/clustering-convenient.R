



# S4 - classes ------------------------------------------------------------

### Parent

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
