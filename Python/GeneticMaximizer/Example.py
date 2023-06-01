"""
Example code for Genetic Algorithm maximizing a two variable function along
defined bounds.

Creator: GurkNathe
"""

import matplotlib as mpl
from GeneticMaximizer import GeneticMaximizer

"""
Initialize with bounds:
    3 <= x <= 10 & 4 <= y <= 8
    
Function to maximize: 
    np.sin(np.pi * 10 * x + 10 / (1+y**2)) + np.log(x**2+y**2)
    
Population Size: 100
# of Generations: 300
Mutation Coefficient: 0.01
Crossover Coefficient: 0.9
"""
g = GeneticMaximizer([3,10,4,8],
                     "np.sin(np.pi * 10 * x + 10 / \
                     (1+y**2)) + np.log(x**2+y**2)",
                    100,
                    100,
                    0.01,
                    0.9)

"""
Returns where the maximum of the last generation occurs (x,y)
and the maximum value at that location (F).
"""
[x, y], F = g.generation()

#Console log information
print("Maximum:", F, "(x,y):", "(" + str(x) + "," + str(y) + ")")

"""
Plotting data

Text box from:
    https://matplotlib.org/3.3.4/gallery/recipes/placing_text_boxes.html
    
Orange dot is the final maximum.
"""
props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
fig, ax = mpl.pyplot.subplots()
mpl.pyplot.plot(g.avg_fit)
mpl.pyplot.xlabel("Generations")
mpl.pyplot.ylabel("Average Fitness")
mpl.pyplot.title("Average Fitness over " + str(g.generations) + "-Generations")
mpl.pyplot.plot(g.generations,F,"o")
textstr = '\n'.join(("Final Maximum: " + '%.5f'%F, 
                     "At (x, y): (" + str('%.3f'%x) + "," + str('%.3f'%y) + ")"))
ax.text(0.46, 0.17, textstr, transform=ax.transAxes, fontsize=14,
        verticalalignment='top', bbox=props)
mpl.pyplot.show()