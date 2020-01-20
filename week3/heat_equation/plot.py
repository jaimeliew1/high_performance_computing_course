'''
Author: Jaime Liew (jyli@dtu.dk)

Animates the timestep output files of the heat equation solver.

Requirements:
- numpy
- matplotlib
- moviepy
'''
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import axes3d

import numpy as np


def make_plot(fn_in, fn_out):

    A = np.loadtxt(fn_in)
    x, y, T = A[:, 0].reshape((21, 21)), A[:, 1].reshape(
        (21, 21)), A[:, 2].reshape((21, 21))

    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.plot_wireframe(x, y, T)
    ax.set_zlim(0, 1)


    plt.savefig(fn_out, dpi=200, bbox_inches='tight')



if __name__ == '__main__':
    make_plot('final.dat', 'final.png')
