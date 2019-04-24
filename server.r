library(shiny)

d1 <- function(S, X, sigma, t, r) {
  result = (log(S / X, base = exp(1)) + (r + sigma^2 / 2) * t) / sigma / sqrt(t)
  result
}

d2 <- function(d1, sigma, t) {
  result = d1 - sigma * sqrt(t)
  result
}

call_option <- function(S, X, sigma, t, r) {
  d = d1(S, X, sigma, t, r)
  result = S * pnorm(d) - X * exp(-r * t) * pnorm(d2(d, sigma, t))
  result
}

put_option <-function(S, X, sigma, t, r) {
  d = d1(S, X, sigma, t, r)
  result = X * exp(-r * t) * pnorm(-1 * d2(d, sigma, t)) - S * pnorm(-1 * d)
  result
}

simulate <- function(option, S, sigma, t, r = 0.03) {
  x = seq(80, 120, 1)
  if (option == 1) {
    res = lapply(x, function(x) call_option(S, x, sigma, t, r))
  } else if (option == 2) {
    res = lapply(x, function(x) put_option(S, x, sigma, t, r))
  }
  return(list(sample=res))
}

server<-shinyServer(function(input, output) {
  output$plot <- renderPlot({
    print(input$t)
    res <- simulate(input$option, input$S, input$sigma, input$t)
    matplot(x=seq(80, 120, 1), y=res$sample,xlab="Strike", ylab="Price",type="l",lwd=3,lty=1,main="Options price",col="black")
  }, height=500)
})
