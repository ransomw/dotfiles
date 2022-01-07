"""
cookbook (aka sandbox)

### todo
* string formatting.  .format(), .join(); f"";  with `%` (ie <str> % <somethings>)
* plot `random_distributions_on_the_real_line`
* generate .dot file of standard types && gnuplot tree
"""

from . import metaclass as mc
# mostly todos
from . import import_machinery
from . import plot_random

# from help(random)
random_distributions_on_the_real_line = [
    'uniform',
    'triangular',
    'normal',
    'lognormal',
    'negative exponential',
    'gamma',
    'beta',
    'pareto',
    'Weibull',
]



def metaclass_experiment():
    mc.type_first()
    breakpoint()

