"""
Animates 1D PDE solution
"""
from typing import Optional
import argparse
import json
import os

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

from visualizer.solution_reader import Solution1D, Solution1DReader


def get_argument() -> argparse.Namespace:
    """
    Defines and returns commandline arguments

    Returns
    -------
    argparse.Namespace
        commandline arguments
    """
    parser: argparse.ArgumentParser = argparse.ArgumentParser()
    parser.add_argument("mesh_file", type=str, help="Mesh file path.")
    parser.add_argument("solution_file", type=str, help="Solution file path.")
    parser.add_argument("output_file", type=str, help="Output file path.")
    parser.add_argument("--solution_list", type=str, help="Solution file list path. \
        If this argument is specified, the animation is created \
        from solution files given in the list, and `solution_file` is ignored."
        )
    parser.add_argument("--interval", type=int, default=1000, help="Delay between frames in milliseconds.")
    parser.add_argument("--repeat_delay", type=int, default=0, help="The delay in milliseconds between consecutive animation runs, if repeat is True.")
    parser.add_argument("--repeat", type=bool, default=True, help="Whether the animation repeats when the sequence of frames is completed.")
    parser.add_argument("--blit", type=bool, default=True, help="Whether blitting is used to optimize drawing.")
    parser.add_argument("--time_format", type=str, default=".2f", help="Format of the time shown above the graph.")

    return parser.parse_args()


def load_solution_file_list(list_file: str) -> dict[str, str]:
    """
    Returns a dict whose key is a solution name
    and value is solution file path.

    Parameters
    ----------
    list_file: str
        JSON file path whose key is a solution name
        and value is solution file path.

    Returns
    -------
    dict[str, str]
        Dict whose key is a solution name
        and value is solution file path.
    """
    try:
        with open(list_file, "r", encoding="utf8") as f:
            return json.load(f)
    except:
        raise


def main():
    """
    Animates solution
    """
    # Retrieve argumants
    args = get_argument()
    mesh_file: str = args.mesh_file
    solution_file: str = args.solution_file
    output_file: str = args.output_file
    solution_list_path: str = args.solution_list
    interval: int = args.interval
    repeat_delay: int = args.repeat_delay
    repeat: bool = args.repeat
    blit: bool = args.blit
    time_format: str = args.time_format

    fig, ax = plt.subplots()

    # List of frames
    frame_list = []

    # Read a mesh file
    mesh: np.ndarray = np.loadtxt(mesh_file, dtype=np.float64, delimiter=",")

    # Define a solution dict
    solution_dict: dict[str, str] = {}
    if solution_list_path:
        solution_dict = load_solution_file_list(solution_list_path)
    else:
        solution_dict["Solution"] = solution_file

    # Append frames to list.
    with Solution1DReader(solution_file) as reader:
        while True:
            # Read solution at time t.
            solution: Optional[Solution1D] = reader.readline() 
            if solution is None:
                break

            if mesh.size != solution.f.size:
                raise ValueError(f"The size of mesh and solution at time {solution.t} is different.")

            # Append a frame
            im, = ax.plot(mesh, solution.f, color='blue')
            title_text = ax.text(0.05, 1.05, f"t = {solution.t:{time_format}}", 
                transform=ax.transAxes, ha="left", fontsize=10)
            frame_list.append([im, title_text])
            
    # Make output directory
    output_directory: str = os.path.dirname(output_file)
    if not os.path.exists(output_directory):
        os.makedirs(output_directory)

    # Save animation
    ani = animation.ArtistAnimation(
        fig, frame_list,
        interval=interval,
        repeat_delay=repeat_delay,
        repeat=repeat,
        blit=blit
        )
    ani.save(output_file, writer="pillow")

if __name__ == "__main__":
    main()
