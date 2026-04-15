"""
add_word_count.py
-----------------
Reads seminar_data.csv, fetches each URL, counts the words in the
article body, and writes seminar_data_with_wordcount.csv.

Requirements:
    pip install pandas requests beautifulsoup4

Usage:
    python add_word_count.py
    (place seminar_data.csv in the same folder first)
"""

import time
import pandas as pd
import requests
from bs4 import BeautifulSoup

INPUT_FILE  = "seminar_data.csv"
OUTPUT_FILE = "seminar_data_with_wordcount.csv"
DELAY       = 1.0   # seconds between requests — be polite to the servers

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0 Safari/537.36"
    )
}

# CSS selectors that target article body text for each domain.
# Add more domains here if needed.
SELECTORS = {
    "ynet.co.il":        ["div.article-body", "div[data-cy='article-body']", "article"],
    "israelhayom.co.il": ["div.article-content", "div.article__content",    "article"],
}

def get_word_count(url: str) -> int | str:
    """Fetch a URL and return a word count, or an error string."""
    try:
        r = requests.get(url, headers=HEADERS, timeout=15)
        r.raise_for_status()
        soup = BeautifulSoup(r.text, "html.parser")

        # Pick selectors for this domain, fall back to <article> then <body>
        domain = next((d for d in SELECTORS if d in url), None)
        selectors = SELECTORS.get(domain, ["article", "body"])

        text = ""
        for sel in selectors:
            el = soup.select_one(sel)
            if el:
                text = el.get_text(separator=" ")
                break
        if not text:
            text = soup.get_text(separator=" ")

        words = text.split()
        return len(words)

    except requests.exceptions.RequestException as e:
        return f"ERROR: {e}"


def main():
    df = pd.read_csv(INPUT_FILE)

    if "url" not in df.columns:
        raise ValueError("Column 'url' not found in CSV.")

    word_counts = []
    total = len(df)

    for i, url in enumerate(df["url"], start=1):
        print(f"[{i}/{total}] {url}")
        if pd.isna(url) or str(url).strip() == "":
            word_counts.append(None)
        else:
            count = get_word_count(str(url).strip())
            word_counts.append(count)
        time.sleep(DELAY)

    df["word_count"] = word_counts
    df.to_csv(OUTPUT_FILE, index=False)
    print(f"\nDone! Saved to {OUTPUT_FILE}")


if __name__ == "__main__":
    main()
