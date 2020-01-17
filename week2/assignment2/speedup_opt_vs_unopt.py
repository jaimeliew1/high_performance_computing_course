import numpy as np
import pandas as pd
import matplotlib.pyplot as plt



if __name__ == '__main__':
    df = pd.read_csv('opt_vs_unopt_data.csv', delimiter='\t')

    ## SPEEDUP PLOT
    plt.figure()
    # plot linear
    plt.plot(df.N_cores, df.N_cores, '--', label='linear')

    # plot data
    plt.plot(df.N_cores, df.s_opt, label='with optimisation')
    plt.plot(df.N_cores, df.s_unopt, label='without optimisation')

    plt.xlabel('Number of cores')
    plt.ylabel('Speedup factor')
    plt.legend()
    plt.savefig('speedup_opt_vs_unopt.png', dpi=200, bboxinches='tight')



    ## TIME PLOT
    plt.figure()

    plt.semilogy(df.N_cores, df.t_opt, label='with optimisation')
    plt.semilogy(df.N_cores, df.t_unopt, label='without optimisation')

    plt.xlabel('Number of cores')
    plt.ylabel('Walltime [s]')

    plt.legend()
    plt.savefig('speedup_time_opt_vs_unopt.png', dpi=200, bboxinches='tight')
