%%writefile score.py
import json
import numpy as np
import pandas as pd
import os
import pickle
import rpy2
import rpy2.robjects as robjects


def init(model_path):
    global model
    f = open(model_path, 'rb')
    model = pickle.load(f)

def run(raw_data): 
    ts = robjects.r['ts']
    # raw_data is a DataFrame serialized by pd.DataFrame.to_json()
    data = pd.read_json(raw_data).to_dict(orient='list')
    data = {key:ts(robjects.FloatVector(value)) for key, value in data.items()}
    data = robjects.DataFrame(data)

    # make prediction
    predict = robjects.r['predict']
    y_hat = list(predict(model, data))
    return y_hat