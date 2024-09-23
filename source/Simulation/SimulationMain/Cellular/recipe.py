#!/usr/bin/env python3

import FlashX_RecipeTools as flashx
from loguru import logger
import sys

from matplotlib import pyplot as plt


_FLASH_PAR_APPENDS = """

## Runtime Orchestration
or_nThreadTeams = 2
or_nThreadsPerTeam = 4
or_nBytesInCpuMemoryPool = 6442450944

or_nStreams = 4
or_nBytesInGpuMemoryPools = 6442450944

or_nThreads_1 = 1
or_nTilesPerPacket_1 = 320

"""


def load_recipe():

    recipe = flashx.TimeStepRecipe()

    # assuming [ModuleName]_[Routine] format
    # JSON files will be populated by reading [ModuleName]_interface.F90
    hydro_begin = recipe.begin_orchestration(after=recipe.root)
    hydro_prepBlock = recipe.add_work("Hydro_prepBlock", after=hydro_begin, map_to="gpu")
    hydro_advance = recipe.add_work("Hydro_advance", after=hydro_prepBlock, map_to="gpu")
    hydro_end = recipe.end_orchestration(begin_node=hydro_begin, after=hydro_advance)

    fluxCorrection = recipe.add_fluxCorrection(after=hydro_end)

    burn = recipe.add_tpl(tpl="cg-tpl.Burn.F90", after=fluxCorrection)

    return recipe


def append_orch_to_flash_par():
    with open("flash.par", "a") as file:
        file.write(_FLASH_PAR_APPENDS)


if __name__ == "__main__":

    # enabling logger to stdout
    logger.remove(0)
    logger.add(sys.stdout, level=0)
    logger.enable(flashx.__name__)

    # recipe
    recipe = load_recipe()

    ir = recipe.compile()
    ir.generate_all_codes()

    append_orch_to_flash_par()

    # print graphs to file
    fig = plt.figure(figsize=(16, 6))
    ax = plt.subplot(211)
    recipe.plot(nodeLabels=True, edgeLabels=True, ax=ax)
    ax = plt.subplot(212)
    ir.flowGraph.plot(nodeLabels=True, edgeLabels=True, ax=ax)
    fig.savefig("__graphs.pdf")

