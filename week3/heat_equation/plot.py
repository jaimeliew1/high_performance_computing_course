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
from pathlib import Path

import numpy as np


def make_plot(fn_root, fn_out):
    index = 0
    this_x = 0
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    fn_in = f'{fn_root}{index:02d}.dat'

    cmap = plt.get_cmap('hsv')
    while Path(fn_in).exists():
        A = np.loadtxt(fn_in)
        Ny, Nx = A.shape
        x, y = np.meshgrid(range(this_x, this_x + Nx), range(Ny))

        ax.plot_wireframe(x, y, A, color=cmap((index%8)/8))

        index += 1
        this_x = this_x + Nx
        fn_in = f'{fn_root}{index:02d}.dat'
    ax.set_zlim(0, 1)
    plt.savefig(fn_out, dpi=200, bbox_inches='tight')
    plt.show()



if __name__ == '__main__':
    make_plot('final', 'final.png')
