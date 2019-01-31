y_max = 4/100
y_tau = 0.01/100
r_tau = 50

c <- y_max * sqrt(-pi * r_tau^2 / log(y_tau / y_max))
sigma <- sqrt(-r_tau^2 / (2 * log(y_tau / y_max)))

x <- c(-70:70)
y <- c * dnorm(x, mean = 0, sd = sigma)

r_detection <- 20
y_detection <- c * dnorm(r_detection, mean = 0, sd = sigma)



# Intervention

#percent_reduction <- 50
percent_reduction <- 27
reduction_factor <- (100 - percent_reduction) / 100
y_intervention <- y * reduction_factor
r_tau_intervention <- sqrt(-2 * sigma^2 * log(y_tau * sqrt(2 * pi * sigma^2) / (reduction_factor * c)))
r_detection_intervention <- sqrt(r_detection^2 + 2 * sigma^2 * log(reduction_factor))

# Plot

plot(x, y, type = "l")
lines(x, y_intervention, col = "red")
abline(v = r_detection)
abline(v = r_detection_intervention, col = "red")
abline(h = y_detection)

# Percent reduction in mosquito bites

area_radius <- pi * r_detection^2
area_radius_intervention <- pi * r_detection_intervention^2

percent_reduction_mosquito_bite_intervention <- 1 - area_radius_intervention / area_radius
percent_reduction_mosquito_bite_intervention
