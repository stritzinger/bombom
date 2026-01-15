#!/usr/bin/env python3
"""
Resolve musl runtime URL by scraping the beammachine page.

This script reads HTML content from stdin, parses anchor tags, and
resolves the musl runtime download URL matching the given architecture.
"""

import argparse
import html as htmllib
import os
import re
import sys
from urllib.parse import urljoin


def normalize_text(text: str) -> str:
    """Normalize text by removing HTML tags, unescaping, and normalizing whitespace."""
    text = re.sub(r"<[^>]+>", " ", text)
    text = htmllib.unescape(text)
    return re.sub(r"\s+", " ", text).strip().lower()


def resolve_musl_runtime_url(html: str, home_url: str, needle_arch: str) -> str:
    """
    Parse HTML content to find the musl runtime URL.

    Args:
        html: HTML content to parse
        home_url: Base URL for resolving relative links
        needle_arch: Architecture to match (e.g. "x86_64", "aarch64")

    Returns:
        Resolved musl runtime URL

    Raises:
        ValueError: If no matching runtime URL can be found
    """
    needle = needle_arch.lower()

    anchors = re.findall(
        r"<a\s+[^>]*href=['\"]([^'\"]+)['\"][^>]*>(.*?)</a>",
        html,
        flags=re.IGNORECASE | re.DOTALL,
    )

    for href, inner in anchors:
        href_unescaped = htmllib.unescape(href)
        haystack = href_unescaped.lower() + " " + normalize_text(inner)

        if "runtime" in haystack and needle in haystack:
            return urljoin(home_url, href_unescaped)

    raise ValueError(f"Could not find musl runtime URL for arch: {needle_arch}")


def main() -> None:
    """CLI entry point."""
    parser = argparse.ArgumentParser(
        description="Resolve musl runtime URL from HTML read on stdin"
    )
    parser.add_argument(
        "--home-url",
        type=str,
        help="Base URL for resolving relative links (or set HOME_URL)",
    )
    parser.add_argument(
        "--needle-arch",
        type=str,
        help="Architecture to match (or set NEEDLE_ARCH)",
    )

    args = parser.parse_args()

    if sys.stdin.isatty():
        parser.error("HTML content must be provided via stdin")

    html = sys.stdin.read()

    home_url = args.home_url or os.environ.get("HOME_URL")
    if not home_url:
        parser.error("HOME_URL must be provided via --home-url or HOME_URL")

    needle_arch = args.needle_arch or os.environ.get("NEEDLE_ARCH")
    if not needle_arch:
        parser.error("NEEDLE_ARCH must be provided via --needle-arch or NEEDLE_ARCH")

    try:
        url = resolve_musl_runtime_url(html, home_url, needle_arch)
        print(url)
    except Exception as exc:
        print(f"Error resolving musl runtime URL: {exc}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
