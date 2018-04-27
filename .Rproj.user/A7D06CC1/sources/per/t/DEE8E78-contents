### Makes a data frame with ests from two models

modelA <- "out/ahcByAge.est"
modelB <- "out/ahcByAgeWithCovariates.est"
modelC <- "out/basic.est"

modelA.ests <- fetch(filename = modelA,
                         where = c("model", "likelihood", "prob")) %>%
  collapseDimension(dimension = "age", weights = exposure) %>%
  collapseIterations(probs = c(0.025, 0.5, 0.975))

modelB.ests <- fetch(filename = modelB,
                     where = c("model", "likelihood", "prob")) %>%
  collapseDimension(dimension = "age", weights = exposure) %>%
  collapseIterations(probs = c(0.025, 0.5, 0.975))

modelC.ests <- fetch(filename = modelC,
                     where = c("model", "likelihood", "prob")) %>%
  collapseIterations(probs = c(0.025, 0.5, 0.975))

ests <- rbind(data.frame(year = rownames(modelA.ests), modelA.ests) %>% mutate(model = "modelA"),
              data.frame(year = rownames(modelB.ests), modelB.ests) %>% mutate(model = "modelB"),
              data.frame(year = rownames(modelC.ests), modelC.ests) %>% mutate(model = "modelC")
              )

ggplot(ests, aes(x=year, y=X50., group=model, color=model)) + 
  geom_line() + 
  geom_errorbar(aes(ymax=X97.5., ymin=X2.5.))
