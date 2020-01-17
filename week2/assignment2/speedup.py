import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


labels = {
    #'N10': '$N_{10}$',
    'N50': '$N_{50}$',
    'N100': '$N_{100}$',
    'N200': '$N_{200}$',
    'N500': '$N_{500}$',
    'N1000': '$N_{1000}$',
}
if __name__ == '__main__':
    df = pd.read_csv('speedup_data.csv', delimiter='\t')


    plt.figure()
    # plot linear
    plt.plot(df.N_cores, df.N_cores, '--', label='linear')

    # plot data
    for i, (key, label) in enumerate(labels.items()):
        plt.plot(df.N_cores, df[key], label=label)

    plt.xlabel('Number of cores')
    plt.ylabel('Speedup factor')
    plt.legend()
    plt.savefig('speedup.png', dpi=200, bboxinches='tight')
