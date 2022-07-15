import pandas as pd
import matplotlib.pyplot as plt

# generate data from dumpCsv
df0=pd.read_csv('clocks0.csv')
df1=pd.read_csv('clocks1.csv')
df2=pd.read_csv('clocks2.csv')

df0.set_index('t', inplace=True)
df1.set_index('t', inplace=True)
df2.set_index('t', inplace=True)

df=pd.concat([df0,df1,df2],axis=0)

df[['eb01','eb02','eb10','eb12','eb20','eb21']].plot()
plt.xlabel('Time (ps)')
plt.ylabel('Elastic buffer occupancy')
plt.title('Step size 1')

plt.show()

df[['clk0','clk1','clk2']].plot()
plt.xlabel('Time (ps)')
plt.ylabel('Period')
plt.title('Step size 1')

plt.show()
