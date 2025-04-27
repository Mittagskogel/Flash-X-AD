#!/usr/bin/env python3
#
# Estimate the speedup of a truncated workload
#
# along with the original columns and a bunch of intermediate computation
# columns, the output should contain these columns:
#
# compute_bound_speedup: speedup under compute bound assumption
# memory_bound_speedup: speedup under memory bound assumption
# speedup: one of the two depending on whether we think the workload is memory or compute bound

import argparse
import re
import sys
import numpy as np
import pandas as pd
from scipy.optimize import minimize
from scipy.optimize import shgo
from scipy.optimize import differential_evolution
from scipy.optimize import NonlinearConstraint

coeffs = [0.00215612, 0.20331858, 0.3378887]
predictor = np.poly1d(coeffs)

fast = False

parser = argparse.ArgumentParser()
parser.add_argument('--input', required=True)
parser.add_argument('--output', required=True)
parser.add_argument('--baseline', type=int, default=52)
args = parser.parse_args()

def get_fpu_runtime(num, perf_density, area):
    return num / (area * perf_density)
def get_fpu_runtimes(nums, perf_densities, areas):
    return [get_fpu_runtime(*d) for d in zip(nums, perf_densities, areas)]

def optimal_perf_fast(flops):
    nums = []
    perf_densities = []

    for mantissa, num in flops:
        area_per_flop = predictor(mantissa)
        perf_density = 1/area_per_flop
        nums.append(num)
        perf_densities.append(perf_density)

    # print(nums)
    # print(perf_densities)
    def eval(areas):
        return sum(get_fpu_runtimes(nums, perf_densities, areas))

    # Number of variables
    n = len(nums)

    # Initial guess: uniform distribution
    x0 = np.full(n, 1.0 / n)

    # Constraint: sum(x) = 1
    constraints = {
        'type': 'eq',
        'fun': lambda x: np.sum(x) - 1
    }

    # Bounds: x_i > 0 (or >= some small epsilon to avoid log(0))
    epsilon = 1e-8
    bounds = [(epsilon, 1) for _ in range(n)]

    # Minimize
    result = minimize(eval, x0,
                      method='SLSQP',
                      bounds=bounds, constraints=constraints,
                      #options={'disp': True, },
                     )

    return {'runtime': result.fun, 'areas': result.x}

def perf(flops, areas):
    nums = []
    perf_densities = []

    for mantissa, num in flops:
        area_per_flop = predictor(mantissa)
        perf_density = 1/area_per_flop
        nums.append(num)
        perf_densities.append(perf_density)

    return list(np.array(perf_densities) * np.array(areas)), sum(get_fpu_runtimes(nums, perf_densities, areas))

def optimal_perf(flops):
    nums = []
    perf_densities = []

    for mantissa, num in flops:
        area_per_flop = predictor(mantissa)
        perf_density = 1/area_per_flop
        nums.append(num)
        perf_densities.append(perf_density)

    # print(nums)
    # print(perf_densities)
    def eval(areas):
        return sum(get_fpu_runtimes(nums, perf_densities, areas))

    # Number of variables
    n = len(nums)

    # Initial guess: uniform distribution
    x0 = np.full(n, 1.0 / n)

    def eq_1(x):
        # print(f'{x} : {np.sum(x) -1 == 0}')
        return np.sum(x) - 1

    # Constraint: sum(x) = 1
    constraints = ({
        'type': 'eq',
        'fun': eq_1,
    })

    # Bounds: x_i > 0 (or >= some small epsilon to avoid log(0))

    epsilon = 1e-8
    bounds = [(epsilon, 1) for _ in range(n)]


    if fast:
        # Minimize
        result = minimize(eval, x0,
                        method='SLSQP',
                        bounds=bounds, constraints=constraints,
                        #options={'disp': True, },
                        )
    else:
        if False:
            result = shgo(eval,
                        bounds=bounds,
                        constraints=constraints,
                        )
        else:
            result = differential_evolution(eval,
                        bounds=bounds,
                        constraints=[NonlinearConstraint(lambda x: sum(x), 1, 1 + epsilon)],
                        )

    return {'runtime': result.fun, 'areas': result.x}

df = pd.read_csv(args.input, delim_whitespace=True)
idx = None
for c in df.columns:
    m = re.match('dens-([0-9]+)-trunc', c)
    if m is not None:
        idx = int(m.group(1))
if idx is None:
    print('could not find index')
    exit(1)
# print(idx)
# print(df)

dens = f'dens-{idx}-'

area_for_f64 = 58379.06971411343
area_for_trunc = 41852.663941914754

runtimes = []
perfs_trunc = []
perfs_double = []
for i, row in df.iterrows():
    m = (row['mantissa'])
    nt = (row[dens + 'trunc'])
    nd = (row[dens + 'double'])
    # print(f'for double: {nd},  {m}: {nt}')
    (perf_double, perf_trunc), rt = perf([(52, nd), (m, nt)], [area_for_f64, area_for_trunc])
    perfs_trunc.append(perf_trunc)
    perfs_double.append(perf_double)
    runtimes.append(rt)

df['perfs_double'] = perfs_double
df['perfs_trunc'] = perfs_trunc
df['compute_bound_runtime'] = runtimes
df['compute_bound_speedup'] = df[df['mantissa'] == args.baseline]['compute_bound_runtime'].iloc[0] / df['compute_bound_runtime']

# print(df)

in_trunc = df[dens + 'trunc-store'] + df[dens + 'trunc-load']
outside_trunc = df[dens + 'original-store'] + df[dens + 'original-load']

df['in_trunc'] = in_trunc
df['outside_trunc'] = outside_trunc


l = []
new_in_truncs = []
for i, row in df.iterrows():
    m = row['mantissa']
    e = round(predictor(m))
    # print(m + e + 1, m, e)
    w_orig = 64
    w_trunc = m + e + 1
    new_in_trunc = (w_trunc / w_orig) * row['in_trunc']
    new_in_truncs.append(new_in_trunc)
    memory_moved = row['outside_trunc'] + new_in_trunc
    l.append(memory_moved)

df['in_trunc'] = new_in_trunc
df['memory_moved'] = l
df['memory_bound_speedup'] = df[df['mantissa'] == args.baseline]['memory_moved'].iloc[0] / df['memory_moved']

# print(df)

# flops per byte
df['trunc_fp_intensity'] = df[dens + 'trunc'] / df['in_trunc']
df['double_fp_intensity'] = df[dens + 'double'] / df['outside_trunc']

# 1024 GB/s for fugaku
peak_bandwidth = 1024 * 1000 * 1000 * 1000
df['is_trunc_memory_bound'] = (df['trunc_fp_intensity'] * peak_bandwidth) < df['perfs_trunc']
df['is_double_memory_bound'] = (df['double_fp_intensity'] * peak_bandwidth) < df['perfs_double']
# print('perfs_memory_bound')
# print(df['trunc_fp_intensity'] * peak_bandwidth)
# print(df['double_fp_intensity'] * peak_bandwidth)
df['trunc_ridge'] = df['trunc_fp_intensity'] * peak_bandwidth

df['is_memory_bound'] = df['is_double_memory_bound'] | df['is_trunc_memory_bound']
df['speedup'] = df['compute_bound_speedup']
df.loc[df['is_memory_bound'], 'speedup'] = df['memory_bound_speedup']

pd.options.display.float_format = "{:,.2f}".format
pd.options.display.max_rows = None
pd.options.display.max_columns = None
# print(df)

df.to_csv(args.output, sep=' ', index=False)


# print(optimal_perf({52: 100, 12: 1600}))
# print(optimal_perf({52: 100, 12: 1}))
# print(optimal_perf({52: 1, 12: 100}))
# print(optimal_perf({52: 100, 4: 0, 12: 100}))
# print(optimal_perf({52: 100, 4: 100, 12: 100}))
