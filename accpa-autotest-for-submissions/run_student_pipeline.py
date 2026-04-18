import os
import time
import subprocess
import sys
from pathlib import Path
from report import print_report
from datetime import datetime

DOCKER_BUILD_TIMEOUT = 300


def run_student_pipeline(homework: str):
    project_dir = os.getenv("CI_PROJECT_DIR")
    branch = os.getenv("CI_COMMIT_REF_NAME")
    if not branch or not branch.startswith("hw") or not branch == homework:
        print("❌ Incorrect branch")
        sys.exit(1)

    print(f"Running pipeline for {branch}")

    test_folders = [
        d
        for d in os.listdir(project_dir)
        if os.path.isdir(os.path.join(project_dir, d)) and "public-tests" in d.lower()
    ]
    if not test_folders:
        print("❌ No test folder found in project directory")
        sys.exit(1)

    tests_dir = os.path.join(project_dir, test_folders[0], homework)
    if not os.path.exists(tests_dir):
        print(
            f"❌ No tests for {homework} in folder {tests_dir}, maybe you forgot to update tests submodule"
        )
        sys.exit(1)
    submission_dir = os.path.join(project_dir, "solution")
    dockerfile = os.path.join(project_dir, "Dockerfile")
    if not os.path.exists(dockerfile):
        print("❌ Missing Dockerfile")
        sys.exit(1)

    image_tag = f"student_solution_{homework}"
    print(f"Building Docker image {image_tag} from {project_dir}")

    build = subprocess.run(
        ["docker", "build", "-t", image_tag, "."],
        cwd=project_dir,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        env=os.environ,
    )

    if build.returncode != 0:
        print("❌ Docker build failed")
        print(build.stderr)
        sys.exit(1)
    else:
        print(f"Build Docker image successfully")

    # Prefer a checker in the current homework folder, but fall back to any
    # checker script available in the public-tests tree (e.g. hw1/check.sh).
    check_script = os.path.join(tests_dir, "check.sh")
    if not os.path.exists(check_script):
        fallback_checkers = sorted(Path(project_dir).glob("public-tests/hw*/check.sh"))
        if fallback_checkers:
            check_script = str(fallback_checkers[0])
            print(f"ℹ️ Using fallback checker script: {check_script}")
        else:
            print(
                f"❌ No checker script found for {homework} (expected {check_script})"
            )
            sys.exit(1)

    results = []
    start_time = time.time()

    for f in sorted(os.listdir(tests_dir)):
        if not f.endswith(".in"):
            continue
        test_name = f.replace(".in", "")
        input_file = os.path.join(tests_dir, f)
        expected_file = os.path.join(tests_dir, f"{test_name}.out")
        try:
            with open(input_file, "r") as infile:
                # test_content = infile.read()
                run = subprocess.Popen(
                    ["docker", "run", "--rm", "--network=none", "-i", image_tag],
                    stdin=infile,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    text=True,
                )
                check_proc = subprocess.run(
                    ["sh", check_script, input_file, expected_file],
                    stdin=run.stdout,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                    timeout=5,
                )
                run.stdout.close()
                run.wait(timeout=5)
        except subprocess.TimeoutExpired:
            if run:
                run.kill()
            results.append((test_name, {"status": "timeout"}))
            continue

        status = "passed" if check_proc.returncode == 0 else "failed"
        results.append(
            (
                test_name,
                {
                    "status": status,
                    "stdout": (check_proc.stdout + check_proc.stderr).strip(),
                },
            )
        )

    duration = time.time() - start_time

    report_path = os.path.join("reports", f"public_{homework}.json")
    print_report(
        homework,
        results,
        datetime.fromtimestamp(start_time).strftime("%Y-%m-%d"),
        duration,
        save_json=report_path,
    )

    os.makedirs(submission_dir, exist_ok=True)
    image_tar = os.path.join(submission_dir, f"student_{homework}_image.tar")
    subprocess.run(["docker", "save", "-o", str(image_tar), image_tag])
    print(f"Docker image saved to {image_tar}")
