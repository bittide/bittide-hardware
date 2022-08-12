import pandas as pd
import matplotlib.pyplot as plt

nodes=range(0,5)

# generate data from dumpCsv
dfs=[pd.read_csv('clocks' + str(i) + '.csv') for i in nodes]

for df in dfs:
    df.set_index('t', inplace=True)

df=pd.concat(dfs,axis=0)

eb_names=['eb'+str(i)+str(j) for i in nodes for j in nodes if j != i]

df[eb_names].plot()
plt.xlabel('Time (ps)')
plt.ylabel('Elastic buffer occupancy')
plt.title('Step size 1')

plt.savefig('elasticbuffers.pdf')

clk_names=['clk'+str(i) for i in nodes]
df[clk_names].plot()
plt.xlabel('Time (ps)')
plt.ylabel('Period')
plt.title('Step size 1')
plt.savefig('clocks.pdf')
