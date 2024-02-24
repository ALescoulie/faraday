import sys
import shutil

from glob import glob
from pathlib import Path
from typing import List


def copy_site(site_dir: Path, build_dir: Path) -> None:
    shutil.copytree(site_dir, build_dir, dirs_exist_ok=True)


def copy_scripts(ts_build_dir: Path, app_build_dir: Path) -> None:
    scripts_dir: Path = app_build_dir / "scripts"

    shutil.copytree(ts_build_dir, scripts_dir, dirs_exist_ok=True)

if __name__ == "__main__":
    site_dir = Path(sys.argv[1])
    code_dir = Path(sys.argv[2])
    build_dir = Path(sys.argv[3])

    shutil.rmtree(build_dir)

    copy_site(site_dir, build_dir)
    copy_scripts(code_dir, build_dir)

