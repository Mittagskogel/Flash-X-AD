#!/usr/bin/env python3

import FlashX_RecipeTools as flashx
from loguru import logger
import sys

from matplotlib import pyplot as plt


def load_recipe():

    recipe = flashx.TimeStepRecipe()

    # assuming [ModuleName]_[Routine] format
    # JSON files will be populated by reading [ModuleName]_interface.F90
    physics_begin = recipe.begin_orchestration(after=recipe.root)
    burn_block_cpu = recipe.add_work("Burn_block", after=physics_begin, map_to="cpu")
    hydro_prepBlock = recipe.add_work("Hydro_prepBlock", after=burn_block_cpu, map_to="cpu")
    hydro_advance = recipe.add_work("Hydro_advance", after=hydro_prepBlock, map_to="cpu")
    hydro_end = recipe.end_orchestration(begin_node=physics_begin, after=hydro_advance)

    fluxCorrection = recipe.add_fluxCorrection(after=hydro_end)

    return recipe


if __name__ == "__main__":

    # enabling logger to stdout
    logger.remove(0)
    logger.add(sys.stdout, level=0)
    logger.enable(flashx.__name__)

    # recipe
    recipe = load_recipe()

    ir = recipe.compile()
    ir.generate_all_codes()

    # print graphs to file
    # fig = plt.figure(figsize=(16, 6))
    # ax = plt.subplot(211)
    # recipe.plot(nodeLabels=True, edgeLabels=True, ax=ax)
    # ax = plt.subplot(212)
    # ir.flowGraph.plot(nodeLabels=True, edgeLabels=True, ax=ax)
    # fig.savefig("__graphs.pdf")

