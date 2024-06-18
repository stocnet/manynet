make_diff_model <- function(events, report, .data) {
  class(report) <- c("diff_model", class(report))
  attr(report, "events") <- events
  attr(report, "mode") <- node_is_mode(.data)
  attr(report, "network") <- as_tidygraph(.data)
  report
}

make_diffs_model <- function(report, .data) {
  class(report) <- c("diffs_model", class(report))
  attr(report, "mode") <- node_is_mode(.data)
  report
}

#' @export
print.diff_model <- function(x, ..., verbose = FALSE){
  x <- x[,colSums(x, na.rm=TRUE) != 0]
  if(!verbose){
    x$n <- NULL
    x$s <- NULL
    x$I_new <- NULL
    x$E_new <- NULL
    x$R_new <- NULL
  }
  print(dplyr::tibble(x, ...))
}

#' @export
print.diffs_model <- function(x, ...){
  x <- x[,colSums(x, na.rm=TRUE) != 0]
  x$I_new <- NULL
  print(dplyr::tibble(x, ...))
}

#' @export
summary.diff_model <- function(object, ...) {
  dplyr::tibble(attr(object, "events"), ...)
}

#' @export
summary.diffs_model <- function(object, ...) {
  sim <- fin <- n <- NULL
  object %>% dplyr::mutate(fin = (I!=n)*1) %>% 
    dplyr::group_by(sim) %>% dplyr::summarise(toa = sum(fin)+1)
}

#' @importFrom dplyr left_join
#' @importFrom ggplot2 geom_histogram
#' @export
plot.diff_model <- function(x, ...){
  S <- E <- I <- I_new <- n <- R <- NULL # initialize variables to avoid CMD check notes
  if(nrow(x)==1) warning("No diffusion observed.") else {
    data <- x
    p <- ggplot2::ggplot(data) + 
      ggplot2::geom_line(ggplot2::aes(x = t, y = S/n, color = "A"), linewidth = 1.25) +
      ggplot2::geom_line(ggplot2::aes(x = t, y = I/n, color = "C"), linewidth = 1.25) +
      ggplot2::geom_col(ggplot2::aes(x = t, y = I_new/n), 
                        alpha = 0.4) +
      ggplot2::theme_minimal() + 
      ggplot2::coord_cartesian(ylim = c(0,1)) + # using coord_cartesion to avoid printing warnings
      ggplot2::ylab("Proportion") + ggplot2::xlab("Steps")
    labs <- c("Susceptible", "Infected")
    if(any(data$E>0)){
      p <- p +
        ggplot2::geom_line(ggplot2::aes(x = t, y = E/n, color = "B"),size = 1.25)
      labs <- c("Susceptible", "Exposed", "Infected")
    }
    if(any(data$R>0)){
      p <- p +
        ggplot2::geom_line(ggplot2::aes(x = t, y = R/n, color = "D"),size = 1.25)
      labs <- c(labs, "Recovered")
    }
    
    p + ggplot2::scale_color_manual("Legend", 
                                    labels = labs,
                                    values = c(A = "blue", B = "orange", 
                                               C = "red", D = "darkgreen"),
                                    guide = "legend")
  }
}

#' @export
plot.diffs_model <- function(x, ...){
  S <- E <- I <- R <- n <- NULL # initialize variables to avoid CMD check notes
  data <- dplyr::tibble(x)
  # ggplot2::ggplot(data) + geom_smooth()
  p <- ggplot2::ggplot(data) + 
    # ggplot2::geom_point(ggplot2::aes(x = t, y = S/n))
    ggplot2::geom_smooth(ggplot2::aes(x = t, y = S/n, color = "A"), 
                         method = "loess", se=TRUE, level = .95, formula = 'y~x') +
    ggplot2::geom_smooth(ggplot2::aes(x = t, y = I/n, color = "C"), 
                         method = "loess", se=TRUE, level = .95, formula = 'y~x') +
    ggplot2::theme_minimal() + 
    ggplot2::coord_cartesian(ylim = c(0,1)) + # using coord_cartesion to avoid printing warnings
    ggplot2::ylab("Proportion") + ggplot2::xlab("Steps")
  labs <- c("Susceptible", "Infected")
  if(any(data$E>0)){
    p <- p +
      ggplot2::geom_smooth(ggplot2::aes(x = t, y = E/n, color = "B"), 
                           method = "loess", se=TRUE, level = .95, formula = 'y~x')
    labs <- c("Susceptible", "Exposed", "Infected")
  }
  if(any(data$R>0)){
    p <- p +
      ggplot2::geom_smooth(ggplot2::aes(x = t, y = R/n, color = "D"), 
                           method = "loess", se=TRUE, level = .95, formula = 'y~x')
    labs <- c(labs, "Recovered")
  }
  
  p + ggplot2::scale_color_manual("Legend", 
                                  labels = labs,
                                  values = c(A = "blue", B = "orange", 
                                             C = "red", D = "darkgreen"),
                                  guide = "legend")
}

# learn_model ####
make_learn_model <- function(out, .data) {
  out <- as.data.frame(out)
  if(is_labelled(.data))
    names(out) <- node_names(.data)
  class(out) <- c("learn_model", class(out))
  attr(out, "mode") <- node_is_mode(.data)
  out
}

#' @export
print.learn_model <- function(x, ...){
  print(dplyr::tibble(x))
}

#' @export
summary.learn_model <- function(object, ..., epsilon = 0.0005) {
  steps <- nrow(object)
  max_belief <- max(object[steps,])
  min_belief <- min(object[steps,])
  if(abs(max_belief - min_belief) < epsilon){
    cat(paste(steps-1, 
              "steps to convergence.\n"))
    cat("Final belief =", max_belief)
  } else 
    cat(paste("No convergence after",
              steps-1, "steps."))
}

#' @export
plot.learn_model <- function(x, ...){
  Step <- Freq <- Var1 <- n <- NULL
  y <- t(x)
  colnames(y) <- paste0("t",0:(ncol(y)-1))
  y <- as.data.frame.table(y)
  y$Step <- as.numeric(gsub("t", "", y$Var2))
  ggplot2::ggplot(y, ggplot2::aes(x = Step, y = Freq, color = Var1)) + 
    ggplot2::geom_line(show.legend = FALSE) + ggplot2::theme_minimal() +
    ggplot2::ylab("Belief")
}
