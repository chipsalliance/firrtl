#!/usr/bin/env python3

import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import argparse, json, os
from scipy import mean
from scipy.stats.mstats import gmean

#matplotlib.rcParams['font.serif'] = "LMRoman8"
matplotlib.rcParams['font.family'] = "serif"
matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42
from math import pow, log10, atan, pi
from functools import reduce

def addMeans(a):
    b = mean(a)
    c = gmean(a)
    a.append(b)
    a.append(c)

def normalize(control, experiment):
    return [e / c for e, c in zip(experiment, control)]

colorbrewer = {
    'Blues': {
        3: ['#deebf7', '#9ecae1', '#3182bd'],
        4: ['#eff3ff', '#bdd7e7', '#6baed6', '#2171b5'],
        5: ['#eff3ff', '#bdd7e7', '#6baed6', '#3182bd', '#08519c'],
        6: ['#eff3ff', '#c6dbef', '#9ecae1', '#6baed6', '#3182bd', '#08519c'],
        7: ['#eff3ff', '#c6dbef', '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#084594'],
        8: ['#f7fbff', '#deebf7', '#c6dbef', '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#084594'],
        9: ['#f7fbff', '#deebf7', '#c6dbef', '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#08519c', '#08306b'],
    },
    'Greens': {
        3: ['#e5f5e0', '#a1d99b', '#31a354'],
        4: ['#edf8e9', '#bae4b3', '#74c476', '#238b45'],
        5: ['#edf8e9', '#bae4b3', '#74c476', '#31a354', '#006d2c'],
        6: ['#edf8e9', '#c7e9c0', '#a1d99b', '#74c476', '#31a354', '#006d2c'],
        7: ['#edf8e9', '#c7e9c0', '#a1d99b', '#74c476', '#41ab5d', '#238b45', '#005a32'],
        8: ['#f7fcf5', '#e5f5e0', '#c7e9c0', '#a1d99b', '#74c476', '#41ab5d', '#238b45', '#005a32'],
        9: ['#f7fcf5', '#e5f5e0', '#c7e9c0', '#a1d99b', '#74c476', '#41ab5d', '#238b45', '#006d2c', '#00441b']
    },
    'Greys': {
        3: ['#f0f0f0', '#bdbdbd', '#636363'],
        4: ['#f7f7f7', '#cccccc', '#969696', '#525252'],
        5: ['#f7f7f7', '#cccccc', '#969696', '#636363', '#252525'],
        6: ['#f7f7f7', '#d9d9d9', '#bdbdbd', '#969696', '#636363', '#252525'],
        7: ['#f7f7f7', '#d9d9d9', '#bdbdbd', '#969696', '#737373', '#525252', '#252525'],
        8: ['#ffffff', '#f0f0f0', '#d9d9d9', '#bdbdbd', '#969696', '#737373', '#525252', '#252525'],
        9: ['#ffffff', '#f0f0f0', '#d9d9d9', '#bdbdbd', '#969696', '#737373', '#525252', '#252525', '#000000']
    },
    'Oranges': {
        3: ['#fee6ce', '#fdae6b', '#e6550d'],
        4: ['#feedde', '#fdbe85', '#fd8d3c', '#d94701'],
        5: ['#feedde', '#fdbe85', '#fd8d3c', '#e6550d', '#a63603'],
        6: ['#feedde', '#fdd0a2', '#fdae6b', '#fd8d3c', '#e6550d', '#a63603'],
        7: ['#feedde', '#fdd0a2', '#fdae6b', '#fd8d3c', '#f16913', '#d94801', '#8c2d04'],
        8: ['#fff5eb', '#fee6ce', '#fdd0a2', '#fdae6b', '#fd8d3c', '#f16913', '#d94801', '#8c2d04'],
        9: ['#fff5eb', '#fee6ce', '#fdd0a2', '#fdae6b', '#fd8d3c', '#f16913', '#d94801', '#a63603', '#7f2704']
    },
    'Purples': {
        3: ['#efedf5', '#bcbddc', '#756bb1'],
        4: ['#f2f0f7', '#cbc9e2', '#9e9ac8', '#6a51a3'],
        5: ['#f2f0f7', '#cbc9e2', '#9e9ac8', '#756bb1', '#54278f'],
        6: ['#f2f0f7', '#dadaeb', '#bcbddc', '#9e9ac8', '#756bb1', '#54278f'],
        7: ['#f2f0f7', '#dadaeb', '#bcbddc', '#9e9ac8', '#807dba', '#6a51a3', '#4a1486'],
        8: ['#fcfbfd', '#efedf5', '#dadaeb', '#bcbddc', '#9e9ac8', '#807dba', '#6a51a3', '#4a1486'],
        9: ['#fcfbfd', '#efedf5', '#dadaeb', '#bcbddc', '#9e9ac8', '#807dba', '#6a51a3', '#54278f', '#3f007d']
    },
    'Reds': {
        3: ['#fee0d2', '#fc9272', '#de2d26'],
        4: ['#fee5d9', '#fcae91', '#fb6a4a', '#cb181d'],
        5: ['#fee5d9', '#fcae91', '#fb6a4a', '#de2d26', '#a50f15'],
        6: ['#fee5d9', '#fcbba1', '#fc9272', '#fb6a4a', '#de2d26', '#a50f15'],
        7: ['#fee5d9', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#99000d'],
        8: ['#fff5f0', '#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#99000d'],
        9: ['#fff5f0', '#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15', '#67000d']
    }
}

colorwheel = [x
              for color in [ colorbrewer['Blues'],
                             colorbrewer['Greens'],
                             colorbrewer['Oranges'],
                             colorbrewer['Purples'],
                             colorbrewer['Reds'] ]
              for x in color[9]]

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--dir_build', type=str, help='output directory to use')
    parser.add_argument('file', type=argparse.FileType('r'), help='Input JSON file')
    # parser.add_argument('--normalize', type=str, help='Normalize to this revision')
    parser.add_argument('--stat', type=str, default='median', choices=['median', 'mean', 'geomean'])
    parser.add_argument('--normalize', type=str)
    args = parser.parse_args()

    stats = json.load(args.file)

    for jvm in stats:
        fig, ax = plt.subplots(len(stats[jvm].keys()), 1)
        i = 0
        for met in stats[jvm]:
            ax[i].set_axisbelow(True)
            ax[i].grid(True, axis='y')
            labels = []
            j = 0
            revCount = len(stats[jvm][met].keys())
            width = 1 / (revCount + 0.5)
            offset = [i * width/2 for i in range(-revCount + 1, revCount, 2)]
            if (revCount < 3):
                colors = colorbrewer['Blues'][3]
            elif (revCount <= 9):
                colors = colorbrewer['Blues'][revCount]
            else:
                colors = colorwheel
            for rev in stats[jvm][met]:
                labels = []
                data = []
                for des in stats[jvm][met][rev][args.stat]:
                    f, l = os.path.splitext(os.path.basename(des))
                    labels.append(f[:8])
                    if (args.normalize):
                        data.append(stats[jvm][met][rev][args.stat][des] / stats[jvm][met][args.normalize][args.stat][des])
                    else:
                        data.append(stats[jvm][met][rev][args.stat][des])
                x = np.arange(len(labels))
                ax[i].bar(x + offset[j],
                          data,
                          width * 0.9,
                          color=colors[j % len(colors)],
                          linewidth=1,
                          edgecolor='black',
                          label=rev,
                          align='center')
                j += 1
            ax[i].set_xticklabels(labels, fontsize=10)
            ax[i].set_xticks(x)
            if (args.normalize):
                title = '{} Normalized to {}'.format(met.capitalize(), args.normalize)
            else:
                title = '{}'.format(met.capitalize())
            ax[i].set_title(title)
            i += 1
        plt.tight_layout()
        ax[i-1].legend(bbox_to_anchor=(0.5, -0.2), loc='upper center', ncol=4)
        for extension in ['eps', 'png', 'pdf']:
            plt.savefig(str.format('{}/benchmark-{}.{}', args.dir_build, jvm, extension),
                        bbox_inches='tight',
                        transparent=True)

if __name__ == '__main__':
    main()
