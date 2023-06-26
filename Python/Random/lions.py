import math
import matplotlib.pyplot as plt

'''
    This is just an ideal modeling of 
    what a populaiton of 1 billion lions 
    would look like if they could only
    canibalize each other for food, and
    asuming half the population is female
    and can produce offspring when possible.
    
    This was inspired by the 1 billion lions
    vs every pokemon debate (https://www.youtube.com/watch?v=OtoVYVZ4deA), 
    where the argument that the lions could just 
    repopulate even though the only food source 
    is other lions.
    
    The model predicts that all lions will be dead
    within 603 days, given the most ideal numbers
    I could find.
'''

# Initial population
L = 1_000_000_000

year = 365

# How much food in kg 1 lion needs per day/iteration
food = 8

# How much food in kg 1 lion can provide
weight = 225

# How long it takes to produce a litter of lions
gestation = 108

# How many lions are born at the same time
birth = 6

# How long it takes a lion to reach sexual maturity
mature = 3 * year

# Percentage of the population that is female
per_female = 0.5

# Population tracker
pop = [L]

# Maturation vars
maturers = []
maturing = 0

while L > 1:
    if L == float("inf"):
        break

    mat_per = maturing / L
    L = math.floor((1 / (1 + (food/weight))) * L)
    maturing = math.floor(L * mat_per)

    if (len(pop) - 1) % gestation == 0 and (len(pop) - 1) != 0:
        matured = birth * math.ceil(((L - maturing) * per_female))
        maturing += matured
        L += matured
    if (len(maturers) - 1) % mature == 0 and (len(maturers) - 1) != 0:
        maturing -= math.ceil(maturers[len(maturers) - mature - 1] * mat_per)

    pop.append(L)
    maturers.append(maturing)

print(len(pop) - 1)
plt.plot([i for i in range(len(pop))], pop, "blue")
plt.plot([i for i in range(len(maturers))], maturers, "green")
plt.xlabel("Days")
plt.ylabel("Population")
plt.title("Lion population")
plt.show()