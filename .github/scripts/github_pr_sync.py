#!/usr/bin/env python3
"""Open or refresh the pull request for a regenerated-documentation branch.

Usage: github_pr_sync.py <branch> <base_commit> <document>...

Reads GITHUB_TOKEN and GITHUB_REPOSITORY (owner/name) from the environment.
With an open pull request from the branch, rewrites its description. Without
one, opens the pull request against the default branch.
"""
import json
import os
import sys
import urllib.error
import urllib.request

API = os.environ.get("GITHUB_API_URL", "https://api.github.com")
REPOSITORY = os.environ.get("GITHUB_REPOSITORY", "")
TARGET = os.environ.get("TARGET_BRANCH", "main")
TITLE = "Regenerate documentation"


def request(method, path, body=None):
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(API + path, data=data, method=method)
    req.add_header("Authorization", "Bearer " + os.environ["GITHUB_TOKEN"])
    req.add_header("Accept", "application/vnd.github+json")
    req.add_header("X-GitHub-Api-Version", "2022-11-28")
    if data is not None:
        req.add_header("Content-Type", "application/json")
    try:
        with urllib.request.urlopen(req, timeout=60) as resp:
            text = resp.read().decode()
    except urllib.error.HTTPError as e:
        text = e.read().decode(errors="replace")
        try:
            return e.code, json.loads(text)
        except ValueError:
            return e.code, {"message": text}
    return resp.status, (json.loads(text) if text else None)


def fail(what, status, payload):
    message = (payload or {}).get("message", payload)
    sys.exit(f"{what} failed with HTTP {status}: {message}")


def description(base, documents):
    lines = [f"Documentation rebuilt on {TARGET} at {base}.", "",
             "Documents whose text changed:"]
    lines += [f"- {document}" for document in documents]
    lines += ["", f"Each scheduled run replaces this branch with a fresh commit on the current {TARGET}. "
              "Do not push to this branch."]
    return "\n".join(lines)


def open_pull_requests(branch):
    owner = REPOSITORY.split("/")[0]
    status, payload = request(
        "GET", f"/repos/{REPOSITORY}/pulls?state=open&base={TARGET}&head={owner}:{branch}")
    if status != 200:
        fail("Listing pull requests", status, payload)
    return payload


def update(pr, text):
    status, payload = request("PATCH", f"/repos/{REPOSITORY}/pulls/{pr['number']}", {"body": text})
    if status != 200:
        fail(f"Updating pull request #{pr['number']}", status, payload)
    print(f"Updated the description of pull request #{pr['number']}.")


def create(branch, text):
    body = {"title": TITLE, "head": branch, "base": TARGET, "body": text}
    status, payload = request("POST", f"/repos/{REPOSITORY}/pulls", body)
    if status == 201:
        print(f"Opened pull request #{payload['number']} {payload.get('html_url', '')}".rstrip())
        return True
    if status == 422:
        return False  # opened by someone else since the listing
    if status == 403:
        fail("Opening the pull request (the repository must allow GitHub Actions to create pull requests)",
             status, payload)
    fail("Opening the pull request", status, payload)


def main(argv):
    if len(argv) < 3:
        sys.exit(__doc__)
    for name in ("GITHUB_TOKEN", "GITHUB_REPOSITORY"):
        if not os.environ.get(name):
            sys.exit(f"{name} is not set.")
    branch, base, documents = argv[1], argv[2], argv[3:]
    text = description(base, documents)
    prs = open_pull_requests(branch)
    if not prs:
        if create(branch, text):
            return
        prs = open_pull_requests(branch)
        if not prs:
            sys.exit(f"Opening the pull request from {branch} reported a duplicate, but none is open.")
    for pr in prs:
        update(pr, text)


if __name__ == "__main__":
    main(sys.argv)
