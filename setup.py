# read the contents of your README file
from pathlib import Path

from setuptools import find_packages, setup

this_directory = Path(__file__).parent
long_description = (this_directory / "README.md").read_text()

setup(
    name="symcalc",
    version="0.1.0",
    packages=find_packages(include=["symexpr", "symexpr.*"]),
    scripts=["calc.py"],
    long_description=long_description,
    long_description_content_type="text/markdown",
)
