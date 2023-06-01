"""
Class that maximizes a given two-variable equation between given bounds using
a genetic algorithm.

Creator: GurkNathe
"""

import numpy as np

"""
Input format:
    bounds: (x_low, x_high, y_low, y_high)
    equation: working function in python using NumPy or standard Python
    populationSize: value for number of specimens in the population
    generations: number of generations before stopping algorithm
    mutationConst: rate of mutation
    crossoverConst: rate of crossover
"""
class GeneticMaximizer():
    
    def __init__(self, 
                 bounds,
                 equation,
                 populationSize,
                 generations,
                 mutationConst,
                 crossoverConst):
        #constants that set the bounds
        self.x_low = bounds[0]
        self.x_high = bounds[1]
        self.y_low = bounds[2]
        self.y_high = bounds[3]
        
        #population variables
        self.populationSize = populationSize
        self.generations = generations
        self.mutationConst = mutationConst
        self.crossoverConst = crossoverConst
        
        #average fitness each generation
        self.avg_fit = []
        
        #maximum of current generation
        self.max_val = 0
        self.max_ind = 0
        
        #Input equation
        self.equation = equation
    
    """
    Returns:
        An initial population.
    """
    def init_pop(self):
        init_population = [np.random.uniform(self.x_low,
                                             self.x_high,
                                             self.populationSize),
                           np.random.uniform(self.y_low,
                                             self.y_high,
                                             self.populationSize)]
        population = []
        for i in range(self.populationSize):
            population.append([init_population[0][i],
                               init_population[1][i]])
        return population
    
    """
    Fitness determiner (i.e. the function we are trying to maximize)
    
    Equation has to be properly defined.
    
    Example: 
        np.sin(np.pi * 10 * x + 10 / (1+y**2)) + np.log(x**2+y**2)
        
    Returns:
        Fitness value for a single specimen
    """
    def function(self, x, y):
        return eval(self.equation)
    
    """
    Gives every organism in the population a fitness
    returns a new array with fitness assigned to each organism and array of the
    fitnesses
    
    Returns:
        The fitness of every specimen in the population.
    """
    def fitness(self, population):
        fitnessPop = []
        for i in range(self.populationSize):
            x = population[i][0]
            y = population[i][1]
            fitness = self.function(x, y)
            fitnessPop.append(fitness)
        return fitnessPop
        
    """
    Does crossover over the population
    
    Returns:
        Two 2-variable specimens after being crossed over.
        
    E.g.:
        Before:
            123456789
            987654321
            
        Crossover start at index 4.
        
        After:
            123454321
            987656789
            
    """
    def crossover(self, o1, o2, crossRate):
        
        if np.random.rand() < crossRate:
            o1_x = str(o1[0])
            o1_y = str(o1[1])
            o2_x = str(o2[0])
            o2_y = str(o2[1])
            
            r1 = np.random.randint(0, len(o1_x))
            r2 = np.random.randint(0, len(o1_y))
            
            o1[0] = float(o1_x[:r1] + o2_x[r1:])
            o1[1] = float(o1_y[:r2] + o2_y[r2:])
            
            o2[0] = float(o2_x[:r1] + o1_x[r1:])
            o2[1] = float(o2_y[:r2] + o1_y[r2:])
    
        return o1, o2
        
    """
    If the selected organism was selected for mutation,
    mutate a random number of values in the x/y values.
    
    Returns:
        The two-variable location with a possible mutation of one digit in
        either variable or both.
    """
    def mutation(self, org, mutRate):
        o_x = str(org[0])
        o_y = str(org[1])
        if np.random.rand() < mutRate:
            index = np.random.randint(0, len(o_x))
            o_x.replace(o_x[index], str(np.random.randint(0,9)))
        if np.random.rand() < mutRate:
            index = np.random.randint(0, len(o_y))
            o_y.replace(o_y[index], str(np.random.randint(0,9)))
        return [float(o_x), float(o_y)]
    
    
    """
    Selects the organisms to reproduce for the next
    generation based on fitness.
    
    Returns:
        Selected population from current population.
    """
    def selection(self, pop, fit):
        sum_fit = sum(fit)
        pro_fit = [i / sum_fit for i in fit]
        parent_index = np.random.choice(self.populationSize,
                                        self.populationSize,
                                        True,
                                        pro_fit)
        selected = []
        for i in range(self.populationSize):
            selected.append(pop[parent_index[i]])
        return selected
    
    """
    Goes through every generation and applies the genetic algorithm to
    maximize the values within the given range.
    
    Returns:
        [a, b]: Two-variable outputs for the maximized value
        F: Maximum value in the given range of values at [a, b]
    """
    def generation(self):
        #Initializing population
        population = self.init_pop()
        for i in range(self.generations):
            
            #getting fitness of each organism
            fit = self.fitness(population)
    
            #Max values form current generation
            max_val = np.amax(fit)
            max_ind = fit.index(max(fit))
    
            #For average graph
            self.avg_fit.append(np.mean(fit))
            
            #Selecting parents for next generation
            parents = self.selection(population, fit)
            
            children = []
            #Crossover and mutation
            for i in range(0, int(self.populationSize / 2)):
    
                #Indecies for two parents
                p1_i = np.random.randint(0,self.populationSize)
                p2_i = np.random.randint(0,self.populationSize)
                
                #Getting the two parents
                p1 = parents[p1_i]
                p2 = parents[p2_i]
    
                #Peforming Crossover on the parents to make two children
                [p1, p2] = self.crossover(p1, p2, self.crossoverConst)
    
                #Mutating the new children
                p1 = self.mutation(p1, self.mutationConst)
                p2 = self.mutation(p2, self.mutationConst)
                
                #Adding new children to the population
                children.append(p1)
                children.append(p2)
            
            #New population is set to the children of old population
            population = children
            
        return [population[max_ind][0], population[max_ind][1]], max_val