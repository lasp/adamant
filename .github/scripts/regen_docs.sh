#!/bin/bash -e

# Rebuild every document, keep the PDFs whose text changed, and put them up
# for review on a fixed branch. Runs in the Adamant environment inside a
# checkout that can push to its origin. GITHUB_TOKEN and GITHUB_REPOSITORY
# come from the workflow. DRY_RUN=1 stops after the local commit.

repo_dir=$(cd "$(dirname "$0")/../.." && pwd)
branch="nightly/documentation"
TARGET_BRANCH_NAME="main"

if [ -z "${GITHUB_TOKEN:-}" ] && [ "${DRY_RUN:-0}" != "1" ]; then
  echo "GITHUB_TOKEN is not set." >&2
  exit 1
fi

cd "$repo_dir"
base=$(git rev-parse HEAD)

# pdfTeX stamps the build time into the PDF. Pinning it to the commit time
# makes a rebuild of the same commit byte-identical.
SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
export SOURCE_DATE_EPOCH

# The publish rule reaches every doc directory below the root. The user guide
# and the architecture description document publish through their own rules.
redo publish doc/user_guide/publish doc/architecture_description_document/publish

# The PDF metadata differs on every build, so compare the document text. The
# user guide embeds its own build: test logs with the time of the run, generated
# code with its generation stamp, and tool output with the build path. Those
# fields are masked so that only document content decides.
text() {
  pdftotext -layout "$1" - | sed -E \
    -e 's/^[[:space:]]*[0-9]{10}\.[0-9]+ //' \
    -e 's/ at [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}/ at <time>/' \
    -e 's/(Generated from .*) on [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}/\1 on <time>/' \
    -e 's/(Report generation time: ).*/\1<time>/' \
    -e 's#/[[:alnum:]_./-]*/adamant/#<root>/#g'
}

# The text diff of every changed document goes under build/ for the run to
# attach, and into the job summary when GitHub provides one.
diff_file="$repo_dir/build/doc_regen/regen.diff"
mkdir -p "$(dirname "$diff_file")"
: > "$diff_file"
summary=${GITHUB_STEP_SUMMARY:-/dev/null}
work=$(mktemp -d)
changed=()
details=""
for pdf in $(git ls-files '*.pdf'); do
  git show "HEAD:$pdf" | text - > "$work/old.txt"
  text "$pdf" > "$work/new.txt"
  if cmp -s "$work/old.txt" "$work/new.txt"; then
    git checkout -- "$pdf"
  else
    name=$(basename "$pdf" .pdf)
    changed+=("$name")
    diff -u --label "a/$pdf" --label "b/$pdf" "$work/old.txt" "$work/new.txt" > "$work/one.diff" || true
    count=$(($(grep -c '^[-+]' "$work/one.diff") - 2))
    echo "  $name: $count differing text lines"
    cat "$work/one.diff" >> "$diff_file"
    details+="<details><summary>$name ($count lines)</summary>"$'\n\n'"\`\`\`diff"$'\n'"$(head -n 120 "$work/one.diff")"$'\n'"\`\`\`"$'\n\n'"</details>"$'\n'
  fi
done
rm -rf "$work"

if [ ${#changed[@]} -eq 0 ]; then
  echo "Documentation is current at $base."
  echo "Documentation is current at $base." >> "$summary"
  exit 0
fi
echo "Regenerated: ${changed[*]}"
{
  echo "## Regenerated documents"
  echo
  echo "${#changed[@]} of $(git ls-files '*.pdf' | wc -l) tracked PDFs differ in text from the copies on $TARGET_BRANCH_NAME. The full diff is the regenerated-documentation-diff artifact of this run; each block below shows up to 120 lines."
  echo
  echo "$details"
} >> "$summary"

git checkout -B "$branch" "$base"
git add -u -- '*.pdf'
git -c user.name="github-actions[bot]" \
    -c user.email="41898282+github-actions[bot]@users.noreply.github.com" \
  commit -q -m "Regenerate documentation"
git log -1 --stat --format='%h %s'

if [ "${DRY_RUN:-0}" = "1" ]; then
  echo "DRY_RUN=1: not pushing $branch."
  exit 0
fi

git push --force origin "HEAD:refs/heads/$branch"

python3 "$repo_dir/.github/scripts/github_pr_sync.py" "$branch" "$base" "${changed[@]}"
