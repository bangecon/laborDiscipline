library(alabama)
library(ggplot2)
laborDiscipline <- function(b = 15,   # Unemployment benefits
                            d = 16,   # Duration of benefits
                            s = 16,   # Spell of unemployment if fired
                            m = 0.5,  # Monitoring intensity (%)
                            u = 0.04, # Unemployment rate (%)
                            a = 0.5,  # Disutility of effort
                            ...) {
  
# Initialize functions and parameters -------------------------------------
  d = min(d, s)                  # Duration of benefits <= unemployment spell
  p = 0.1*punif(m) + 
    0.9*punif(u)                 # Probability of becoming unemployed
  p_0 = 0.1*punif(0.5) + 
    0.9*punif(0.04)              # "Natural" probability of becoming unemployed
  w_r = (b*d)/(s*p/p_0)          # Reservation wage
  pi = function(x) x[2]/x[1]     # Employer objective (effort/labor cost)
  br = function(x) {
    x[2] - ((x[1] - w_r)^(1 - a)) / x[1]  
                                 # Worker's BR function as constraint <=0
  }

# Solve for the optimal wage and effort -----------------------------------

  opt <- constrOptim.nl(
    c(w_r, 0.5),                       # Set initial wage and effort
    pi,                                # Function to optimize
    heq = br,                          # Constraint
    control.optim = list(fnscale = -1) # A technical thing to max 
  )

  out <- list(
    args = list(
      b = b, 
      d = d, 
      s = s, 
      m = m, 
      u = u, 
      a = a, 
      p = p, 
      p_0 = p_0, 
      w_r = w_r, 
      pi = pi, 
      br = br),
    opt = opt,
    wage = opt$par[1],
    effort = opt$par[2]
  )
  class(out) <- c("laborDiscipline", class(out))
  out
}

# Plot the result ---------------------------------------------------------

plot.laborDiscipline <- function(eq0,
                                 eq1 = eq0,
                                 color0 = 'black',
                                 color1 = 'black')
{
  p <- ggplot() +
    geom_segment(
      aes(
        x = 0,
        xend = eq0$opt$par[1],
        y = eq0$opt$par[2] ,
        yend = eq0$opt$par[2] 
      ),
      linetype = "dotted", 
      color = color0
    ) +
    geom_segment(
      aes(
        x = eq0$opt$par[1],
        xend = eq0$opt$par[1],
        y = 0,
        yend = eq0$opt$par[2] 
      ),
      linetype = "dotted", 
      color = color0
    ) +
    geom_function(
      fun = function(x)
        x * eq0$opt$par[2] / (eq0$opt$par[1]),
      aes(color = "eq0")
    ) +
    geom_function(
      fun = function(x)
        ((x - eq0$args$w_r)^(1 - eq0$args$a)) / x,
      aes(color = "eq0")
    ) +
    geom_segment(
      aes(
        x = 0,
        xend = eq1$opt$par[1],
        y = eq1$opt$par[2] ,
        yend = eq1$opt$par[2] 
      ),
      linetype = "dotted", 
      color = color1
    ) +
    geom_segment(
      aes(
        x = eq1$opt$par[1],
        xend = eq1$opt$par[1],
        y = 0,
        yend = eq1$opt$par[2] 
      ),
      linetype = "dotted", 
      color = color1
    ) +
    geom_function(
      fun = function(x)
        x * eq1$opt$par[2] / (eq1$opt$par[1]),
      aes(color = "eq1")
    ) +
    geom_function(
      fun = function(x)
        ((x - eq1$args$w_r)^(1 - eq1$args$a)) / x,
      aes(color = "eq1")
    ) +
    lims(x = c(0, 2 * max(eq0$opt$par[1], eq1$opt$par[1])), 
         y = c(0, 2 * max(eq0$opt$par[2], eq1$opt$par[2]))) +
    labs(
      title = "Labor Discipline Equilibrium\nwith User-Defined Changes", 
      x = "Wage", y = "Effort") +
    scale_color_manual(
      values = c(color0, color1),
      breaks = c('eq0', 'eq1'),
      labels = c('Initial Equilibrium', 'New Equilibrium')
    ) +
    theme(
      legend.position = 'bottom',
      legend.title = element_blank(),
      text = element_text(size = 16)
    )
  p

}
