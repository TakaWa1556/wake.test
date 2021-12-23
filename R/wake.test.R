#' wake.test
#'
#' @return various statistical test result and graph
#'
#' @export
#'
wake.test <- function(){
  table.data <- read.csv(readline("file name.csv"))
  A <- table.data$value[table.data$group == "A"]
  B <- table.data$value[table.data$group == "B"]
  test.result <- t.test(A, B, var.equal = TRUE,
                        alternative = "two.sided", mu = 0)
  print(test.result)
  A.name <- readline("name of group A?")
  B.name <- readline("name of group B?")
  "group" <- c(A.name, B.name)
  "average" <- c(mean(A), mean(B))
  se.data <- c(sd(A)/sqrt(length(A)), sd(B)/sqrt(length(B)))
  table.data.frame <- data.frame(group, average, se.data)
  ggplot(table.data.frame, aes(x = group, y = average)) +
    geom_bar(stat="identity", width =.4, color = "black", fill = "white")+
    geom_errorbar(aes(ymin = average - se.data, ymax = average +se.data),
                  width=.1)
}
