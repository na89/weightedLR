linearReg <- function(cases, controls) {
  table <- data.frame(geno = c(0, 1, 2, 0, 1, 2), phe = c(0, 0, 0, 1, 1, 1))
  ws <- c(controls, cases)
  m <- lm(phe ~ geno, data = table, weights = ws)
  m$df.residual <- sum(ws) - 2
  ks <- summary(m)$coefficients
  list(beta = ks[2, 1], pval = ks[2, 4])
}
