import numpy as np
import matplotlib.pyplot as plt

if __name__ == '__main__':
    A = np.loadtxt('bandwidth.txt')
    msg_size, bandwidth = A[:, 0], A[:, 1]

    # Fit regression
    coefs = np.polyfit(msg_size[-5:], bandwidth[-5:], 1)
    print(f'slope: {coefs[0]}')
    print(f'intercept: {coefs[1]}')

    plt.figure()
    plt.xlabel('Message size $[N_{doubles}]$')
    plt.ylabel('Transfer time $[s]$')

    plt.plot(msg_size, bandwidth, '-xr')
    plt.plot(msg_size, np.polyval(coefs, msg_size), '--xk')


    plt.savefig('bandwidth.png', dpi=200, bbox_inches='tight')
