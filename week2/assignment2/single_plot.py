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
from moviepy.editor import VideoClip
from moviepy.video.io.bindings import mplfig_to_npimage
import numpy as np

filename = 'final.dat'
def make_frame(fn_in, fn_out):


    A = np.loadtxt(fn_in)
    N = int(np.sqrt(len(A)))
    x, y, T = A[:, 0].reshape((N, N)), A[:, 1].reshape(
        (N, N)), A[:, 2].reshape((N, N))

    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.plot_wireframe(x, y, T)
    #ax.set_zlim(0, 1)

    plt.savefig(fn_out, dpi=200, bbox_inches='tight')


make_frame.counter = 1

if __name__ == '__main__':
    make_frame('final.dat', 'final.png')
