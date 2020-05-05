import sys
import numpy as np
from PIL import Image


def name_of_file(file):
    nam = file.split('.')[0]
    return nam


def file_to_array(file):
    img = Image.open(file)
    blk = img.convert('L')
    arr = np.array(blk)
    return arr

def array_to_csv(arr, nam):
    np.savetxt(str(nam) + '.csv', arr, delimiter=',')

if __name__ == '__main__':
    arg = sys.argv[1]
    arr = file_to_array(arg)
    nam = name_of_file(arg)
    array_to_csv(arr, nam)
