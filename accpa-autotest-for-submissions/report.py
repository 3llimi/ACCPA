import json
import os


def print_report(homework, results, submission_time, duration, save_json=None):
    passed = sum(1 for _, r in results if r["status"] == "passed")
    total = len(results)

    print("=" * 50)
    print(f"Homework: {homework}")
    print(f"Duration: {duration:.2f}s")
    print(f"Submission time: {submission_time}")
    print("-" * 50)

    main_total = 0
    main_passed = 0
    extra_total = 0
    extra_passed = 0
    for test_name, r in results:
        status = r["status"]
        print(f"[{test_name}] {status}")
        if test_name.startswith("main"):
            main_total += 1
            if status == "passed":
                main_passed += 1

        elif test_name.startswith("extra"):
            extra_total += 1
            if status == "passed":
                extra_passed += 1

        if status == "failed":
            print(f"  Checker output:")
            print(f"    {r.get('stdout')}")
        if status == "runtime_error":
            print(f"  Error:    {r.get('stderr')}")
        if status == "timeout":
            print(f"  Timeout")
        if status in ("no_submission", "invalid_submission", "error"):
            print(f"  Info:     {r.get('stderr', '')}")
        if "duration_ms" in r:
            print(f"  Execution time: {r['duration_ms']:.1f} ms")
        if "perf_feedback" in r:
            print(f"  Performance: {r['perf_feedback']}")
        print()

    print("-" * 50)
    print(f"Overall summary: {passed}/{total} passed")
    print(f"Summary for main: {main_passed}/{main_total} passed")
    print(f"Summary for extra: {extra_passed}/{extra_total} passed")
    print("=" * 50)

    if save_json:
        os.makedirs(os.path.dirname(save_json), exist_ok=True)
        with open(save_json, "w") as f:
            json.dump(
                {
                    "homework": homework,
                    "duration": duration,
                    "time": submission_time,
                    "passed": passed,
                    "total": total,
                    "results": results,
                },
                f,
                indent=2,
            )
