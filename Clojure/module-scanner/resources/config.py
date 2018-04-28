# Many import statements (it's a Python module)
import sys
import logging
import math
from enum import IntEnum

# -----------------------------------------------------------------------------
# Section marker
# -----------------------------------------------------------------------------

# Some configuration parameter
# with a multi-line comment
FOO = 'BAR'

# A normal one line comment
CRITERION = "xXx"


class Value(IntEnum):
    A = 1
    B = 2
    C = 3
    D = 4
    E = 1


# The actual dictionary we are interested in. We will assume the name, equals
# sign and opening curly brance are all part of the first line
IMPORTANT_DICT = {
    'resources.module_1':              Value.C,
    'resources.sub_pack_1.module_1_1': Value.C,
    'resources.sub_pack_1.module_1_2': Value.C,
    'resources.sub_pack_1.module_1_3': Value.C,
    'resources.sub_pack_1.module_1_4': Value.C,
    'resources.sub_pack_1.module_1_5': Value.C,
    'resources.sub_pack_1.module_1_7': Value.C,
    'resources.sub_pack_1.module_1_8': Value.C,
    'resources.module_2':              Value.C,
    'resources.sub_pack_2.module_2_1': Value.C,
    'resources.sub_pack_2.module_2_2': Value.C,
    'resources.sub_pack_2.module_2_3': Value.C,
    'resources.sub_pack_2.module_2_4': Value.C,
    'resources.sub_pack_2.module_2_5': Value.C,
    'resources.sub_pack_3.module_3_1': Value.C,
    'resources.sub_pack_3.module_3_2': Value.C,
    'resources.sub_pack_3.module_3_3': Value.C,
    'resources.sub_pack_3.module_3_4': Value.C,
    'resources.sub_pack_3.module_3_5': Value.C,
    'resources.module_3':              Value.C,
    'resources.module_4':              Value.C,
    'resources.module_5':              Value.C,
    'resources.module_6':              Value.D,
}

# More data following the dictionary

DATA_X = 42

FLAG_A = True
FLAG_B = False
FLAG_C = True
