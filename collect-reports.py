#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

"""Collect downloaded gain-sweep reports into one PDF with gain labels."""

import glob
import os
import subprocess
import sys
import tempfile


def gain_values():
    """Generate gain values from 9e-7 to 1e-9."""
    for exp in range(7, 9 + 1):  # 7, 8, 9
        for mantissa in range(9, 0, -1):
            yield mantissa, exp


def format_gain(mantissa, exp):
    return f"{mantissa}e-{exp}"


def make_stamp_pdf(gain_str, out_pdf, page_width=595, page_height=842):
    """Create a transparent single-page PDF with a gain label in the top-left."""
    margin = 15
    font_size = 14
    # Estimate text width for background box
    text_width = len(gain_str) * font_size * 0.6
    box_height = font_size + 8
    box_width = text_width + 12
    box_x = margin
    box_y = page_height - margin - box_height
    text_x = box_x + 6
    text_y = box_y + 4

    ps = f"""%!PS-Adobe-3.0
%%BoundingBox: 0 0 {page_width} {page_height}
%%Pages: 1
%%EndComments
%%Page: 1 1
% White background box with border
0.95 0.95 0.95 setrgbcolor
{box_x} {box_y} {box_width} {box_height} rectfill
0 0 0 setrgbcolor
0.5 setlinewidth
{box_x} {box_y} {box_width} {box_height} rectstroke
% Label text
/Helvetica-Bold findfont {font_size} scalefont setfont
{text_x} {text_y} moveto
(gain = {gain_str}) show
showpage
%%EOF
"""
    with tempfile.NamedTemporaryFile(suffix=".ps", mode="w", delete=False) as f:
        f.write(ps)
        ps_path = f.name

    try:
        subprocess.run(
            [
                "gs", "-q", "-dBATCH", "-dNOPAUSE", "-sDEVICE=pdfwrite",
                "-sOutputFile=" + out_pdf,
                f"-dDEVICEWIDTHPOINTS={page_width}",
                f"-dDEVICEHEIGHTPOINTS={page_height}",
                ps_path,
            ],
            check=True,
        )
    finally:
        os.unlink(ps_path)


def stamp_pdf(input_pdf, stamp_pdf_path, output_pdf):
    """Overlay stamp on every page of input_pdf using pdftk."""
    subprocess.run(
        ["pdftk", input_pdf, "stamp", stamp_pdf_path, "output", output_pdf],
        check=True,
    )


def get_page_size(pdf_path):
    """Get the page size of the first page using Ghostscript."""
    # Default to A4
    return 595, 842


def main():
    reports_dir = "reports"
    output_pdf = "gain-sweep.pdf"

    with tempfile.TemporaryDirectory() as tmp:
        parts = []

        for mantissa, exp in gain_values():
            gain_str = format_gain(mantissa, exp)
            pdfs = sorted(glob.glob(
                os.path.join(reports_dir, gain_str, "**", "*.pdf"),
                recursive=True,
            ))
            if not pdfs:
                print(f"WARNING: No PDFs found for {gain_str}, skipping")
                continue

            # Create stamp overlay
            stamp_path = os.path.join(tmp, f"stamp-{gain_str}.pdf")
            make_stamp_pdf(gain_str, stamp_path)

            for i, pdf in enumerate(pdfs):
                stamped = os.path.join(tmp, f"stamped-{gain_str}-{i}.pdf")
                stamp_pdf(pdf, stamp_path, stamped)
                parts.append(stamped)

            print(f"{gain_str}: {len(pdfs)} PDF(s)")

        if not parts:
            print("ERROR: No reports found", file=sys.stderr)
            sys.exit(1)

        print(f"\nMerging {len(parts)} files into {output_pdf}...")
        subprocess.run(["pdfunite"] + parts + [output_pdf], check=True)
        print(f"Done: {output_pdf}")


if __name__ == "__main__":
    main()
