from random import *

# sig : doc
rrand_dists = {
    # (a, b) : in half-open or close interval, "depending on rounding" (???)
    'uniform': uniform,
    # (low=0.0, high=1.0, mode=None) : low and hight are bounds.  for mode wikipedia /Triangular_distribution
    'triangular': triangular,
    'normal': [
        # (mu, sigma)
        normalvariate,
        # (mu, sigma)
        gauss,
        # ??? thought
        # normal _was_ gaussian
    ],
    # (mu, sigma) : composition with ln (log_e) yields normal distribution
    'lognormal': lognormvariate,
    'negative exponential': None,
    # 'gamma':,
    # 'beta',
    # 'pareto',
    # 'Weibull',
}
