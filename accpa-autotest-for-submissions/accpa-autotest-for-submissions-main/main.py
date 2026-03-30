import argparse
import os
import sys
from run_student_pipeline import run_student_pipeline


def parse_args():
    parser = argparse.ArgumentParser(description="Autotesting system entrypoint")

    parser.add_argument(
        "--homework", "--hw", required=True, help="Homework number, e.g., hw1, hw2"
    )

    return parser.parse_args()


def entrypoint():
    args = parse_args()

    hw_name = f"hw{args.homework}"
    print(f"Homework selected: {hw_name}")
    
    run_student_pipeline(hw_name)

if __name__ == "__main__":
    entrypoint()

