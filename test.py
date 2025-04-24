# %%
import pickle

file = "PickledStaircases/20_stairData.pkl.psydat"

with open(file,'rb') as f:
    stairData = pickle.load(f)
# %%
