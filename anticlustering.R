library(magrittr)

#compute differential entropy for a dataframe
# @param df: data frame
# @return double: the diff entropy
diff_entropy <- function(df) {
  d = (df %>% dim)[2]
  covDF = cov(df)
  d / 2 + log(2 * pi) * d / 2 + log(det(covDF)) / 2
}


# maxmimize objective
# @param df data frame
# @param R number of clusters to choose
# @return vector of cluster assignments
max_entropy <- function(df,R) {
  n = (df %>% dim) [1]
  #init_assignments = (rmultinom(n,1,rep(1 / R,R)) == 1) %>%
  #  apply(.,2,which)
  r=0
  init_assignments = (1:n)%>%sapply(.,function(x) {
    ifelse(r>=R,r<<-1,r<<-r+1)
    r
  })
  print(init_assignments)
  objective = -Inf
  objective_new = -Inf
  entropies = rep(0,R)
  iter = 0
  while (TRUE) {
    (1:n) %>%
      sapply(.,function(i) {
        index = init_assignments[i]
        #entropy with ith obs added to that cluster
        entropies_switch = (1:R) %>% sapply(.,function(r) {
          if (r == index) {
            cluster_indices = init_assignments == r
            df_index = df[cluster_indices != i,]
            return(diff_entropy(df_index))
          }
          ia = init_assignments; ia[i] = r
          cluster_indices = init_assignments == r
          df_index = df[cluster_indices,]
          return(diff_entropy(df_index))
        })
        best_cluster = (1:R) %>% (function(r) {
          entropies_switch[r] + entropies_switch[index] -
            entropies[r] - entropies[index]
        }) %>% which.max
        entropies[index] <<- entropies_switch[index]
        entropies[best_cluster]  <<- entropies_switch[best_cluster]
        init_assignments[i] <<- best_cluster
        print(paste(
          "best cluster for obs", i,": ",best_cluster, " previous cluster: ", index
        ))
      })
    (1:R) %>% sapply(.,function(x)
      sum(x == init_assignments))
    objective_new = entropies %>% sum()
    print(paste("objective:",objective_new))
    if (abs(objective - objective_new) < 1e-5 | iter >= 100) {
      print(paste(
        "converged. tolerance", abs(objective - objective_new), " iter ",iter
      ))
      break
    }
    objective = objective_new
    iter = iter + 1
  }
  init_assignments
}
