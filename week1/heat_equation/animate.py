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


def make_frame(t):
    filename = f'diff{make_frame.counter:06d}.dat'

    A = np.loadtxt(filename)
    x, y, T = A[:, 0].reshape((21, 21)), A[:, 1].reshape(
        (21, 21)), A[:, 2].reshape((21, 21))

    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.plot_wireframe(x, y, T)
    ax.set_zlim(0, 1)

    img = mplfig_to_npimage(fig)
    plt.close(fig)

    make_frame.counter += 1
    return img


make_frame.counter = 1

if __name__ == '__main__':
    fps = 20
    N = 200
    duration = (N - 1) / fps

    animation = VideoClip(make_frame, duration=duration)
    animation.write_gif('gif.gif', fps=fps)
