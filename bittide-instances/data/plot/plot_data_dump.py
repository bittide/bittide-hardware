#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
import os
import sys
import csv
import multiprocessing
import matplotlib.pyplot as plt

WORDS_PER_SAMPLE = 11
BYTES_PER_WORD = 4

CLOCK_SPEED = 125_000_000  # 125 MHz
MS_PER_PULSE = 5
CYCLES_PER_PULSE = (CLOCK_SPEED * MS_PER_PULSE) // 1000


def parse(path):
    filesize_in_bytes = os.path.getsize(path)
    n_samples = filesize_in_bytes // (WORDS_PER_SAMPLE * BYTES_PER_WORD)

    with open(path, "rb") as fp:
        for _ in range(n_samples):
            local_clock_counter = int.from_bytes(fp.read(8), "little")
            number_of_sync_pulses_seen = int.from_bytes(fp.read(4), "little")
            cycles_since_sync_pulse = int.from_bytes(fp.read(4), "little")
            eb_counters = [
                int.from_bytes(fp.read(4), "little", signed=True) for _ in range(7)
            ]

            yield {
                "local_clock_counter": local_clock_counter,
                "number_of_sync_pulses_seen": number_of_sync_pulses_seen,
                "cycles_since_sync_pulse": cycles_since_sync_pulse,
                "eb_counter_0": eb_counters[0],
                "eb_counter_1": eb_counters[1],
                "eb_counter_2": eb_counters[2],
                "eb_counter_3": eb_counters[3],
                "eb_counter_4": eb_counters[4],
                "eb_counter_5": eb_counters[5],
                "eb_counter_6": eb_counters[6],
            }


def to_csv(data, output_path):
    first_sample = next(data)
    with open(output_path, "w", newline="") as csvfile:
        fieldnames = first_sample.keys()
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerow(first_sample)
        for sample in data:
            writer.writerow(sample)


def get_timestamp(number_of_sync_pulses_seen, cycles_since_sync_pulse):
    normalized_clock_cycles = (
        number_of_sync_pulses_seen * CYCLES_PER_PULSE + cycles_since_sync_pulse
    )
    return normalized_clock_cycles / CLOCK_SPEED  # in seconds


def plot(data, output_path):
    eb_counters = {f"eb_counter_{i}": [] for i in range(7)}
    timestamps = []

    for sample in data:
        timestamp = get_timestamp(
            sample["number_of_sync_pulses_seen"],
            sample["cycles_since_sync_pulse"],
        )
        timestamps.append(timestamp)
        for i in range(7):
            eb_counters[f"eb_counter_{i}"].append(sample[f"eb_counter_{i}"])

    plt.figure(figsize=(10, 6))
    for i in range(7):
        plt.scatter(timestamps, eb_counters[f"eb_counter_{i}"], s=0.5, label=f"EB {i}")

    plt.xlabel("Time (seconds)")
    plt.ylabel("Buffer occupancy")
    plt.legend()
    plt.grid()
    plt.xlim(left=0, right=max(timestamps))
    plt.savefig(output_path)
    plt.close()


def main(path):
    to_csv(parse(path), path + ".csv")
    plot(parse(path), path + ".pdf")


if __name__ == "__main__":
    pool = multiprocessing.Pool(4)
    pool.map(main, sys.argv[1:])
