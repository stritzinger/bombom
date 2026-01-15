#!/usr/bin/env python3
"""
Resolve musl runtime URL by scraping the beammachine page.

This script reads HTML content from stdin (if piped) or from a positional argument,
parses anchor tags, and resolves the musl runtime download URL matching the given architecture.
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
        description="Resolve musl runtime URL from HTML content"
    )
    parser.add_argument(
        "html",
        nargs="?",
        type=str,
        default=sys.stdin if not sys.stdin.isatty() else None,
        help="HTML content to parse (reads from stdin if not provided)",
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

    # Handle HTML input: if default was sys.stdin, read from it; otherwise use provided value
    if args.html is sys.stdin:
        html = sys.stdin.read()
    elif args.html is not None:
        html = args.html
    else:
        # No argument and stdin is a TTY
        parser.error("HTML content must be provided via stdin or as a positional argument")
    
    if not html:
        parser.error("HTML content is empty")

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
        parser.error(f"Error resolving musl runtime URL: {exc}")


if __name__ == "__main__":
    main()
