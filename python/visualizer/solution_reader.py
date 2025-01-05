from dataclasses import dataclass
from io import TextIOWrapper
from typing import Optional

import numpy as np


@dataclass
class Solution1D:
    """
    Solution of 1D PDE at time t
    """
    # Time
    t: float

    # Values on the nodes at time t
    f: np.ndarray     


class Solution1DReader:
    """
    File reader of 1D PDE solution.
    """
    def __init__(self, file_path: str) -> None:
        """
        Initialize object

        Parameters
        ----------
        file_path: str
            File path to read
        """
        self._line_count: int = 0
        self._file_path: str = file_path
        self._file: Optional[TextIOWrapper] = None


    def __enter__(self):
        self.open(self._file_path)
        return self


    def __exit__(self, exc_type, exc_value, traceback) -> None:
        self.close()


    def open(self, file_path: str) -> None:
        """
        Open a given file.

        Parameters
        ----------
        file_path: str
            File path to read
        """
        self._file_path = file_path
        self._file = open(self._file_path, "r")


    def close(self) -> None:
        """
        Close the opened file.
        If no file is opened, this method do nothing.
        """
        if self._file is not None:
            self._file.close()


    def readline(self) -> Optional[Solution1D]:
        """
        Returns a line. 

        Returns
        -------
        Optional[Solution1D]
            Solution object. 
            If a line is EOF, returns None.
        """
        if self._file is None:
            return None

        while True:
            input_line: str = self._file.readline()
            self._line_count += 1

            # The case of EOF
            if not input_line:
                return None

            # Skips blank line.
            if len(input_line.strip()) == 0:
                continue

            # Transforms str list into float array
            split_line: list[str] = input_line.split(",")
            value_array: np.ndarray = self._parse(split_line)

            return Solution1D(t=value_array[0], f=value_array[1:])


    def _parse(self, split_line: list[str]) -> np.ndarray:
        """
        Returns a ndarray transformed from a given str list.
        If it can not be transformed, returns None.

        Parameters
        ----------
        split_list: list[str]
            A list of str read from slution file.
        
        Returns
        -------
        np.ndarray
            Float ndarray.

        Raises
        ------
        ValueError
            If parse error occurs.
        """
        try:
            float_list: list[float] = [float(s) for s in split_line]
            return np.array(float_list)
        except:
            raise ValueError(f"An error occurs at line {self._line_count}.")
