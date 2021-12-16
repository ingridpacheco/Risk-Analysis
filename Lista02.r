freqCount <- function(c, n = 0) {
    num = 0
    num_length <- length(c)
    
    for (i in c) {
        if (i == n) {
        num <- num + 1
        }
    }
    (num/num_length)*100
}

q1 <- function() {
    a1 = rbinom(10,1,0.5)
    a2 = rbinom(100,1,0.5)
    a3 = rbinom(1000,1,0.5)
    a4 = rbinom(5000,1,0.5)
    a5 = rbinom(10000,1,0.5)
    
    list(c(freqCount(a1),
            freqCount(a2),
            freqCount(a3),
            freqCount(a4),
            freqCount(a5)))
}

install.packages("ggplot2")
library(ggplot2)

q2 <- function() {
    frequencies <- c()
    for (i in seq_along(1:100)){
        a1 = rbinom(10,1,0.5)
        frequencies <- c(frequencies, freqCount(a1))
        a2 = rbinom(100,1,0.5)
        frequencies <- c(frequencies, freqCount(a2))
        a3 = rbinom(1000,1,0.5)
        frequencies <- c(frequencies, freqCount(a3))
        a4 = rbinom(5000,1,0.5)
        frequencies <- c(frequencies, freqCount(a4))
        a5 = rbinom(10000,1,0.5)
        frequencies <- c(frequencies, freqCount(a5))
    }
    
    print(length(frequencies))
    y2 <- data.frame(frequencies)
    h1 <- ggplot(y2, aes(frequencies)) + geom_histogram() + xlab("Frequência relativa") + ylab("Frequência")
    h1
}

calculate_v <- function(number, TCL) {
    for (i in seq_along(1:number)) {
        v <- runif(12,0,1)
        sum_value <- sum(v) - 6
        TCL <- c(TCL, sum_value)
    }
    TCL
}

q3 <- function() {
    TCL <- c() 
    TCL <- calculate_v(10, TCL)
    TCL <- calculate_v(100, TCL)
    TCL <- calculate_v(1000, TCL)
    
    y <- data.frame(TCL)
    h2 <- ggplot(y, aes(TCL)) + geom_histogram() + xlab("Values") + ylab("Frequency")
    h2
}

q4 <- function() {
    TCL <- c()
    TCL <- calculate_v(1000, TCL)
    
    y <- data.frame(Values = TCL)
    v <- data.frame(Values = rnorm(1000, mean = 0, sd = 1))
    
    y$sample <- 'uniforme'
    v$sample <- 'normal' 
    
    values <- rbind(v, y)
    
    ggplot(values, aes(Values, fill = sample)) + geom_density(alpha = 0.2)
}
