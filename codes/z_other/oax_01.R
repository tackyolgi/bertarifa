library(oaxaca)
data("chicago")
chicago$real.wage <- exp(chicago$ln.real.wage)
results <- oaxaca(formula = real.wage ~ age + female + LTHS +
                    + some.college + college + advanced.degree | foreign.born | LTHS +
                    + some.college + college + advanced.degree, data = chicago, R = 1000)

plot(results, decomposition = "twofold", group.weight = -1, 
      unexplained.split = TRUE, components = c("unexplained A","unexplained B"), 
      component.labels = c("unexplained A" = "In Favor of Natives", "unexplained B" = "Against the Foreign-Born"),
      , component.left=TRUE, variables = c("age", "female", "college"), variable.labels = c("age" =
      "Years of Age", "female" = "Female", "college" = "College Education"))

