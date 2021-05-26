# MLE Example

example_mle <- function() {
  set.seed(123)
  exp_sample <- tibble(observation = rexp(n = 1000, rate = 1))
  plot <- exp_sample %>%
    ggplot() +
    geom_histogram(aes(observation, ..density..), fill = "grey", color = "black", bins = 100) +
    stat_function(fun = dexp, args = (rate <- 1), aes(colour = "1")) +
    stat_function(fun = dexp, args = (rate <- 1.5), aes(colour = "1.5")) +
    stat_function(fun = dexp, args = (rate <- 2), aes(colour = "2")) +
    theme(
      plot.title = element_text(size = 21, face = "bold"),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 21),
      legend.text = element_text(size = 21),
      legend.title = element_text(size = 18, face = "bold")
    ) +
    ggtitle("Histogram and Densities of an Exponential Distribution") +
    scale_colour_discrete(name = expression(paste("Rate (", lambda, ")"))) +
    labs(x = "Observed Value", y = "Density")
  return(plot)
}

# Probability versus likelihood

prob_vs_likelihood <- function() {
  data <- tibble(
    quantile = seq(-3, 3, 0.01), density = dnorm(quantile)
  )
  plot <- data %>%
    ggplot() +
    geom_line(aes(x = quantile, y = density), colour = "red") +
    geom_vline(xintercept = qnorm(0.9), linetype = "dashed", colour = "blue") +
    geom_hline(yintercept = 0) +
    geom_segment(aes(x = quantile, y = 0, xend = quantile, yend = dnorm(quantile)),
      color = "blue",
      data = data %>% filter(quantile >= qnorm(0.9)), alpha = 0.3
    ) +
    geom_hline(yintercept = dnorm(qnorm(0.9)), linetype = "dashed", colour = "red") +
    annotate("text",
      x = 2.6, y = 0.05, label = paste("Pr(X > 1.28)", "=", 0.10),
      color = "darkblue", size = 6
    ) +
    annotate("text",
      x = -2.7, y = 0.2, label = expression(paste(f[X], "(x = 1.28 |", mu, ", ", sigma^2, ") = 0.18")),
      color = "red", size = 6
    ) +
    theme(
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 22),
    ) +
    scale_x_continuous(breaks = seq(-3, 3, 0.5)) +
    xlab("x") +
    ylab(expression(paste(f[X], "(x |", mu, ", ", sigma^2, ")")))
  return(plot)
}

# Three initial examples of linear models

example_1 <- function() {
  set.seed(123)
  data <- tibble(x = rnorm(50, 10, 3), y = 2 * x + rnorm(length(x)))
  plot <- data %>% ggplot() +
    geom_point(aes(x, y)) +
    ggtitle("Example 1") +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 21)
    )
  return(plot)
}

example_2 <- function() {
  set.seed(456)
  data <- tibble(x = rnorm(50, 10, 3), y = 5 * (x > 10) + rnorm(length(x), 0, 0.24))
  plot <- data %>% ggplot() +
    geom_point(aes(x, y)) +
    ggtitle("Example 2") +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 21)
    )
  return(plot)
}

example_3 <- function() {
  set.seed(789)
  data <- tibble(x = rnorm(100, 10, 3), y = 2 * sin(x) + rnorm(length(x), 0, 0.4))
  plot <- data %>% ggplot() +
    geom_point(aes(x, y)) +
    ggtitle("Example 3") +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 21)
    )
  return(plot)
}

# Example of violation of normality on residuals

example_non_normality <- function() {
  set.seed(123)
  data <- tibble(x = rchisq(100, 10), y = 3 * x + rchisq(length(x), 10))
  model <- lm(y ~ x, data)
  par(mfrow = c(1, 2))
  plot(model, 2)
  hist(residuals(object = model), breaks = 10,
    main = "Histogram of Residuals",
    xlab = "Residuals"
  )
}

# Example of heteroscedasticity

example_heteroscedasticity <- function() {
  set.seed(123)
  data <- tibble(
    x = rnorm(500), residual = rnorm(500, 0, x^2),
    y = 3 * x + residual
  )
  model <- lm(y ~ x, data)
  plot(model, 1)
}

# Example of deterministic relation

example_deterministic_relation <- function() {
  data <- tibble(x = seq(0, 5, 0.1), y = 2 * x)
  plot <- data %>% ggplot() +
    geom_line(aes(x, y)) +
    ggtitle("Deterministic Relation of Y = 2X") +
    xlab("X") +
    ylab("Y") +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 21)
    )
  return(plot)
}

# Example of stochastic relation

example_stochastic_relation <- function() {
  prob <- tibble(
    x = seq(0, 4, 0.01),
    y = 2 * x,
    y1 = 2 + dnorm(x, mean = 1, sd = 0.25),
    y2 = 6 + dnorm(x, mean = 3, sd = 0.25),
    y3 = 4 + dnorm(x, mean = 2, sd = 0.25)
  )
  plot <- prob %>%
    ggplot() +
    geom_line(aes(x, y)) +
    ggtitle("Stochastic Relation") +
    geom_line(aes(x, y1), color = "red") +
    geom_line(aes(x, y2), color = "red") +
    geom_line(aes(x, y3), color = "red") +
    geom_segment(aes(x = 1, y = 2, xend = 1, yend = 3.6), color = "red", linetype = "dotted") +
    geom_segment(aes(x = 2, y = 4, xend = 2, yend = 5.6), color = "red", linetype = "dotted") +
    geom_segment(aes(x = 3, y = 6, xend = 3, yend = 7.6), color = "red", linetype = "dotted") +
    coord_flip() +
    scale_y_continuous(
      name = "X",
      breaks = c(2, 4, 6),
      labels = c("1", "2", "3")
    ) +
    scale_x_continuous(
      name = "Y",
      breaks = c(1, 2, 3),
      labels = c("2", "4", "6")
    ) +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 21)
    )
  return(plot)
}

draw_sample <- function(n){
    return(rnorm(n, 175, 7))
}

# Q-Q plot

qqplot_dev_residuals <- function(data) {
  ggplot(data,
    mapping = aes(sample = dev_residuals)) +
      stat_qq_band() +
      stat_qq_line() +
      stat_qq_point() +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme(
        plot.title = element_text(size = 21, face = "bold"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        legend.title = element_text(size = 18, face = "bold")
      )
}

# tidy() for vlgm Objects

tidy.vglm <- function(x, conf.int = FALSE, conf.level = 0.95) {
  co <- as.data.frame(coef(summary(x)))
  names(co) <- c("estimate", "std.error", "statistic", "p.value")
  if (conf.int) {
    qq <- qnorm((1 + conf.level) / 2)
    co <- transform(co,
                    conf.low = estimate - qq * std.error,
                    conf.high = estimate + qq * std.error
    )
  }
  co <- data.frame(term = rownames(co), co)
  rownames(co) <- NULL
  return(co)
}

## Quantile regression - a very very very ugly implementation
example_quantile_regression <- function() {
    prob <- tibble(x = seq(0, 4, 0.01), 
                   y = 2 * x, 
                   y1 = 2 + dnorm(x, mean = 1, sd = 0.25), 
                   y2 = 6 + dnorm(x, mean = 3, sd = 0.25),
                   y3 = 4 + dnorm(x, mean = 2, sd = 0.25))

    prob %>% 
        ggplot() + 
        geom_line(aes(x, y - 2*1.64*0.25)) + ggtitle("0.95 Quantile regression") +
        geom_line(aes(x, y1), color = "red") +
        geom_line(aes(x,y2), color = "red") + 
        geom_line(aes(x,y3), color = "red") + 
        geom_segment(aes(x = 1 + 1.64*0.25, y = 2, xend = 1 + 1.64*0.25, yend = 2.4), color = "red") +
        geom_segment(aes(x = 2 + 1.64*0.25, y = 4, xend = 2 + 1.64*0.25, yend = 4.4), color = "red") +
        geom_segment(aes(x = 3 + 1.64*0.25, y = 6, xend = 3 + 1.64*0.25, yend = 6.4), color = "red") +
    
        geom_segment(aes(x = 1 + 1.64*0.25 - 0.1, y = 2, xend = 1 + 1.64*0.25 - 0.1, yend = 2.7), color = "red", linetype='dotted') +
        geom_segment(aes(x = 1 + 1.64*0.25 - 0.2, y = 2, xend = 1 + 1.64*0.25 - 0.2, yend = 3.1), color = "red", linetype='dotted') +
        geom_segment(aes(x = 1 + 1.64*0.25 - 0.3, y = 2, xend = 1 + 1.64*0.25 - 0.3, yend = 3.4), color = "red", linetype='dotted') +
        geom_segment(aes(x = 1 + 1.64*0.25 - 0.4, y = 2, xend = 1 + 1.64*0.25 - 0.4, yend = 3.6), color = "red", linetype='dotted') +
        geom_segment(aes(x = 1 + 1.64*0.25 - 0.5, y = 2, xend = 1 + 1.64*0.25 - 0.5, yend = 3.5), color = "red", linetype='dotted') +
        geom_segment(aes(x = 1 + 1.64*0.25 - 0.6, y = 2, xend = 1 + 1.64*0.25 - 0.6, yend = 3.2), color = "red", linetype='dotted') +
        geom_segment(aes(x = 1 + 1.64*0.25 - 0.7, y = 2, xend = 1 + 1.64*0.25 - 0.7, yend = 2.8), color = "red", linetype='dotted') +
        geom_segment(aes(x = 1 + 1.64*0.25 - 0.8, y = 2, xend = 1 + 1.64*0.25 - 0.8, yend = 2.5), color = "red", linetype='dotted') +
        geom_segment(aes(x = 1 + 1.64*0.25 - 0.9, y = 2, xend = 1 + 1.64*0.25 - 0.9, yend = 2.2), color = "red", linetype='dotted') +
        annotate("text", x = 1, y = 2.5, label = c("95%"), size = 10) +
        
        
        geom_segment(aes(x = 2 + 1.64*0.25 - 0.1, y = 4, xend = 2 + 1.64*0.25 - 0.1, yend = 4.7), color = "red", linetype='dotted') +
        geom_segment(aes(x = 2 + 1.64*0.25 - 0.2, y = 4, xend = 2 + 1.64*0.25 - 0.2, yend = 5.1), color = "red", linetype='dotted') +
        geom_segment(aes(x = 2 + 1.64*0.25 - 0.3, y = 4, xend = 2 + 1.64*0.25 - 0.3, yend = 5.4), color = "red", linetype='dotted') +
        geom_segment(aes(x = 2 + 1.64*0.25 - 0.4, y = 4, xend = 2 + 1.64*0.25 - 0.4, yend = 5.6), color = "red", linetype='dotted') +
        geom_segment(aes(x = 2 + 1.64*0.25 - 0.5, y = 4, xend = 2 + 1.64*0.25 - 0.5, yend = 5.5), color = "red", linetype='dotted') +
        geom_segment(aes(x = 2 + 1.64*0.25 - 0.6, y = 4, xend = 2 + 1.64*0.25 - 0.6, yend = 5.2), color = "red", linetype='dotted') +
        geom_segment(aes(x = 2 + 1.64*0.25 - 0.7, y = 4, xend = 2 + 1.64*0.25 - 0.7, yend = 4.8), color = "red", linetype='dotted') +
        geom_segment(aes(x = 2 + 1.64*0.25 - 0.8, y = 4, xend = 2 + 1.64*0.25 - 0.8, yend = 4.5), color = "red", linetype='dotted') +
        geom_segment(aes(x = 2 + 1.64*0.25 - 0.9, y = 4, xend = 2 + 1.64*0.25 - 0.9, yend = 4.2), color = "red", linetype='dotted') +
        annotate("text", x = 2, y = 4.5, label = c("95%"), size = 10) +
    
        geom_segment(aes(x = 3 + 1.64*0.25 - 0.1, y = 6, xend = 3 + 1.64*0.25 - 0.1, yend = 6.7), color = "red", linetype='dotted') +
        geom_segment(aes(x = 3 + 1.64*0.25 - 0.2, y = 6, xend = 3 + 1.64*0.25 - 0.2, yend = 7.1), color = "red", linetype='dotted') +
        geom_segment(aes(x = 3 + 1.64*0.25 - 0.3, y = 6, xend = 3 + 1.64*0.25 - 0.3, yend = 7.4), color = "red", linetype='dotted') +
        geom_segment(aes(x = 3 + 1.64*0.25 - 0.4, y = 6, xend = 3 + 1.64*0.25 - 0.4, yend = 7.6), color = "red", linetype='dotted') +
        geom_segment(aes(x = 3 + 1.64*0.25 - 0.5, y = 6, xend = 3 + 1.64*0.25 - 0.5, yend = 7.5), color = "red", linetype='dotted') +
        geom_segment(aes(x = 3 + 1.64*0.25 - 0.6, y = 6, xend = 3 + 1.64*0.25 - 0.6, yend = 7.2), color = "red", linetype='dotted') +
        geom_segment(aes(x = 3 + 1.64*0.25 - 0.7, y = 6, xend = 3 + 1.64*0.25 - 0.7, yend = 6.8), color = "red", linetype='dotted') +
        geom_segment(aes(x = 3 + 1.64*0.25 - 0.8, y = 6, xend = 3 + 1.64*0.25 - 0.8, yend = 6.5), color = "red", linetype='dotted') +
        geom_segment(aes(x = 3 + 1.64*0.25 - 0.9, y = 6, xend = 3 + 1.64*0.25 - 0.9, yend = 6.2), color = "red", linetype='dotted') +
        annotate("text", x = 3, y = 6.5, label = c("95%"), size = 10) +
        
    
        coord_flip() +
        scale_y_continuous(name ="X",
                         breaks = c(2, 4, 6),
                         labels = c("1","2","3")) + 
        scale_x_continuous(name ="Y",
                         breaks = c(1, 2, 3),
                         labels = c("2","4","6")) + 
        theme(plot.title = element_text(size=24), axis.text = element_text(size=18), axis.title = element_text(size=18))
}

example_quantile_cdf <- function(q){
    data <- tibble(quantile = seq(-3, 3, 0.1), F = pnorm(quantile))
    data %>% 
      ggplot() + 
      geom_line(aes(quantile, F)) +
      geom_segment(aes(x = -Inf, y = q, xend = qnorm(q), yend = q), color = "red", linetype='dotted') +
      geom_segment(aes(x = qnorm(q), y = -Inf, xend = qnorm(q), yend = q), color = "red", linetype='dotted') +
      scale_x_continuous(breaks = c(qnorm(q)), labels = c(paste0("Q(", q,") = ", round(qnorm(q), 4)))) + 
      theme(plot.title = element_text(size=24), axis.text = element_text(size=18), axis.title = element_text(size=18))    
}


example_quantile_pdf <- function(q){
    data <- tibble(quantile = seq(-3, 3, 0.1), F = pnorm(quantile))
    data %>% 
      ggplot() + 
      geom_line(aes(quantile, F)) +
      geom_segment(aes(x = -Inf, y = q, xend = qnorm(q), yend = q), color = "red", linetype='dotted') +
      geom_segment(aes(x = qnorm(q), y = -Inf, xend = qnorm(q), yend = q), color = "red", linetype='dotted') +
      scale_x_continuous(breaks = c(qnorm(q)), labels = c(paste0("Q(", q,") = ", round(qnorm(q), 4)))) + 
      theme(plot.title = element_text(size=24), axis.text = element_text(size=18), axis.title = element_text(size=18))    
}

draw_sample <- function(n){
    return(rnorm(n, 175, 7))
}


knn_tibble <- function(w, k){
  ## beaware that the way we handle ties here might differ.
    
  cow2 <- 
    cow %>%
    mutate(neighbor = w) %>% 
    mutate(neighbor = row_number(abs(neighbor - week))) %>% 
    mutate(neighbor = as_factor(if_else(neighbor > k, true = 0, false = 1)))
    
  return(cow2)    
}

knn_plot <- function(w, k){
  ## beaware that the way we handle ties here might differ.
    
  cow_n <- knn_tibble(w, k)
  avg = cow_n %>% filter(neighbor == 1) %>% .$fat %>% mean()
    
  p <- 
    knn_tibble(w, k) %>% 
    ggplot() +
    geom_point(aes(week, fat, color = neighbor), size = 4) +
    #labs(y = "Fat Content (%)") +
    #ggtitle("Fat content of cow milk") + 
    geom_vline(xintercept = w, alpha = 0.35) +
    annotate("point", x = w, y = avg, shape = 23, fill = "black", size = 4.5)+
    theme(plot.title = element_text(size=24), axis.text = element_text(size=18), axis.title = element_text(size=24))

  return(p)
}



example_variance_reduction <- function(){
    n <- 10
    data <- 
      tibble(x = rnorm(n, 10, 3),
             y = rnorm(n, 5 + 2*x, .75))

    data_color <- crossing(seq_range(data$x, 200), seq_range(data$y, 200)) %>% rename( x = "seq_range(data$x, 200)" , y = "seq_range(data$y, 200)")
    
    color_index <- NULL
    for (i in 1:nrow(data_color)){
        min_d = Inf
        color_index[i] <- 1
        for (j in 1:10){
           if (sum((data_color[i,] - data[j,] )^2)<min_d){
               color_index[i] <- j
               min_d <- sum((data_color[i,] - data[j,] )^2)
           }
        }
    }
    data_color <- data_color  %>% mutate(color = as_factor(color_index))
    
    print(data %>% 
  ggplot() +
  geom_point(aes(x,y,color=color), size=3, alpha = 1, data_color) + geom_point(aes(x,y), size = 4))
}
