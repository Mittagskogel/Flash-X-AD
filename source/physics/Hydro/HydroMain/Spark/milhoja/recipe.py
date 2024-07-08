#!/usr/bin/env python3

import FlashX_RecipeTools as flashx
from loguru import logger
import sys


def load_recipe():

    recipe = flashx.TimeStepRecipe()

    # assuming [ModuleName]_[Routine] format
    # JSON files will be populated by reading [ModuleName]_interface.F90
    hydro_begin = recipe.begin_orchestration(itor_type="LEAF", after=recipe.root)
    hydro_prepBlock = recipe.add_work("Hydro_prepBlock", after=hydro_begin, map_to="gpu")
    hydro_advance = recipe.add_work("Hydro_advance", after=hydro_prepBlock, map_to="gpu")
    hydro_end = recipe.end_orchestration(hydro_begin, after=hydro_advance)

    fluxCorrection = recipe.add_fluxCorrection(after=hydro_end)

    return recipe

# TODO:
# ./setup Sedov -auto -2d +milhoja --recipe=recipe.py
if __name__ == "__main__":

    logger.remove(0)
    logger.add(sys.stdout, level=0)
    logger.enable(flashx.__name__)

    recipe = load_recipe()
    ir = recipe.compile()

    ir.generate_all_codes()

