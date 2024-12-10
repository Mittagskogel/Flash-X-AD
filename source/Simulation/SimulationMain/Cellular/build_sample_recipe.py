#!/usr/bin/env python3
"""
NOTE: This is a sample script that constructs, builds, and compiles
      a recipe for Cellular detonation problem using `Hydro` and
      `nuclearBurn` units. The sample recipes are organized in
      `load_recipe_v*` functions, with various possible hardware configurations.
      Currently, it works only with `approx13` of `nuclearBurn` unit,
      as that is the only *Milhoja-ready* implementation
      for the nuclear burn unit, at this moment.

      This script is intended to be executed in the Flash-X object directory.
      It also **append** some runtime parameters in `flash.par` file located
      in the object directory, which are used by the Orchestration unit with Milhoja.

EXAMPLE: An example setup command would be,
    ```sh
    ./setup Cellular -auto -2d +a13 +sparklwf +sqr16 +mh_push +pm4dev -gridinterpolation=monotonic -parfile=tests/test_amr_2d_coldstart.par
    ```
"""

import FlashX_RecipeTools as flashx
from loguru import logger
import sys

from matplotlib import pyplot as plt

_PAR_HEADER = "## Following RPs are auto-inserted by recipe.py"

_FLASH_PAR_APPENDS = f"""
{_PAR_HEADER}
# Runtime Orchestration
or_nThreadTeams = 2
or_nThreadsPerTeam = 4
or_nBytesInCpuMemoryPool = 6442450944

or_nStreams = 4
or_nBytesInGpuMemoryPools = 6442450944

or_nThreads_1 = 1
or_nTilesPerPacket_1 = 320
or_nTilesPerCpuTurn_1 = 10

"""


def load_recipe_v1():
    """
    Computes Hydro on GPU with Milhoja,
    then FluxCorrection and Burn on CPU *outside* of Milhoja
    """

    recipe = flashx.TimeStepRecipe()

    # assuming [ModuleName]_[Routine] format
    # JSON files will be populated by reading [ModuleName]_interface.F90
    hydro_begin = recipe.begin_orchestration(after=recipe.root)
    hydro_prepBlock = recipe.add_work("Hydro_prepBlock", after=hydro_begin, map_to="gpu")
    hydro_advance = recipe.add_work("Hydro_advance", after=hydro_prepBlock, map_to="gpu")
    hydro_end = recipe.end_orchestration(begin_node=hydro_begin, after=hydro_advance)

    fluxCorrection = recipe.add_fluxCorrection(after=hydro_end)

    # adding a bare template for inserting
    # "call Burn()" line in TimeAdvance.F90. See cg-tpl.Burn.F90 file for details
    burn = recipe.add_tpl(tpl="cg-tpl.Burn.F90", after=fluxCorrection)

    return recipe


def load_recipe_v2():
    """
    Everything on Milhoja, on *CPU*,
    *NO* flux correction. Expect wrong results
    """
    recipe = flashx.TimeStepRecipe()

    orch_begin = recipe.begin_orchestration(after=recipe.root)
    hydro_prepBlock = recipe.add_work("Hydro_prepBlock", after=orch_begin, map_to="cpu")
    hydro_advance = recipe.add_work("Hydro_advance", after=hydro_prepBlock, map_to="cpu")
    burn_burner = recipe.add_work("Burn_burner", after=hydro_advance, map_to="cpu")
    burn_update = recipe.add_work("Burn_update", after=burn_burner, map_to="cpu")
    orch_end = recipe.end_orchestration(begin_node=orch_begin, after=burn_update)

    return recipe

def load_recipe_v3():
    """
    Everything on Milhoja, Hydro on GPU then Burn on CPU,
    *NO* flux correction. Expect wrong results
    """
    recipe = flashx.TimeStepRecipe()

    orch_begin = recipe.begin_orchestration(after=recipe.root)
    hydro_prepBlock = recipe.add_work("Hydro_prepBlock", after=orch_begin, map_to="gpu")
    hydro_advance = recipe.add_work("Hydro_advance", after=hydro_prepBlock, map_to="gpu")
    burn_burner = recipe.add_work("Burn_burner", after=hydro_advance, map_to="cpu")
    burn_update = recipe.add_work("Burn_update", after=burn_burner, map_to="cpu")
    orch_end = recipe.end_orchestration(begin_node=orch_begin, after=burn_update)

    return recipe

def load_recipe_v4():
    """
    Everything on Milhoja, Hydro on CPU and GPU then Burn on CPU,
    *NO* flux correction. Expect wrong results
    """
    recipe = flashx.TimeStepRecipe()

    orch_begin = recipe.begin_orchestration(after=recipe.root)
    hydro_prepBlock = recipe.add_work("Hydro_prepBlock", after=orch_begin, map_to="cpu,gpu")
    hydro_advance = recipe.add_work("Hydro_advance", after=hydro_prepBlock, map_to="cpu,gpu")
    burn_burner = recipe.add_work("Burn_burner", after=hydro_advance, map_to="cpu")
    burn_update = recipe.add_work("Burn_update", after=burn_burner, map_to="cpu")
    orch_end = recipe.end_orchestration(begin_node=orch_begin, after=burn_update)

    return recipe


def append_orch_to_flash_par():
    """
    Append required runtime parameters for orchestration in flash.par
    """
    with open("flash.par", "r") as file:
        for line in file:
            if _PAR_HEADER in line:
                return

    with open("flash.par", "a") as file:
        file.write(_FLASH_PAR_APPENDS)


if __name__ == "__main__":

    # enabling logger to stdout
    logger.remove(0)
    logger.add(sys.stdout, level=0)
    logger.enable(flashx.__name__)

    # choose a recipe
    load_recipe = load_recipe_v4

    # load a recipe
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

