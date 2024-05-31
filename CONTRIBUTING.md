# How to Contribute

Adamant welcomes any and all contributions! Do you have something to add?

This file provides information on how to contribute to the Adamant software framework.

While we encourage all contributions, remember that Adamant is used everyday to create reliable and efficient software for safety-critical applications including spacecraft. As such, code quality is important, and you may be asked to make adjustments to your submission prior to inclusion.

## Creating a Submission

Adamant follows the [GitHub flow](https://docs.github.com/en/get-started/quickstart/github-flow) development process. You should begin by [forking](https://docs.github.com/en/get-started/quickstart/fork-a-repo) this repository. When you have finished making changes on your fork, create a [pull request](https://github.com/lasp/adamant/pulls) back to this repository. For large or complex changes, consider starting a [discussion](https://github.com/lasp/adamant/discussions) before opening a pull request.

### Pull Request

We aim to cultivate a clean and useful commit history. Please adhere to the following when submitting a pull request:

- Pull requests should be made against `main`.
- Use `git rebase` when pulling changes from upstream.
    1.  `git rebase` actually _rebases_ your branch from the current main branch's endpoint. This localizes conflicts to the commits at which they actually appear, though it can become complicated when there are more than a few conflicts.
    2.  `git merge` pulls in all the updates that your branch does not have, and combines them with the updates you have made in a single merge commit. This allows you to deal with any and all conflicts at once, but information such as when conflicts originated is lost.
  
  For more info on `git merge` vs `git rebase` see [here](https://www.atlassian.com/git/tutorials/merging-vs-rebasing).
- Keep submissions small and focused on single fixes, enhancements, or additions. This makes changes easier to review and faster to merge.
- Pull request branches should have as "clean" of a history as possible.
    - Use `git rebase -i`, if needed, to revise the history of your request. Paint a clear story for the reviewers.
    - Each commit should present one change or idea.
    - Commits that merely "fix up" previous commits should be squashed into their appropriate target.
    - Write [good commit messages](https://cbea.ms/git-commit/).
- Include a summary of changes in the pull request that gives reviewers an idea of what they should pay attention to.
- All tests and checks should pass before submitting a pull request (see next section for details).

Once a pull request has been submitted, a maintainer will review your submission.

### Tests and Checks

Adamant leverages [GitHub Actions](https://docs.github.com/en/actions) to unit test and style check the code. These automated checks will be initiated when a pull request has been opened. All checks must pass successfully before your pull request can be included. It is recommended that you run these checks prior to submitting a pull request by either:

1. Running the actions on your fork of the repository on GitHub via the "Actions" tab, or
2. Running `redo style`, `redo test_all`, and `redo publish_all` from within the directories of code that you modify.

Note that many of these checks are annoyingly pedantic, but this helps ensure uniformity and quality in the Adamant code base. Adamant maintainers may make commits on your pull request to correct minor issues.

### Submission Review

All pull requests are reviewed by an Adamant maintainer and the community. Your code may fly in space, and will be held to a high standard. Our maintainers are happy to work with contributors to ensure changes meet these standards. Once the pull request is approved by a maintainer, the submission will be included within the repository.

### Approval and Merge

Once the pull request is passing all tests and checks and has been approved by a maintainer it will be merged into the Adamant repository!

Pull requests should be merged by a maintainer using the "create a merge commit" strategy. In combination with the pull request recommendations above, this ensures that features are neatly bracketed by merge commits on either side, making a clear hierarchical separation between features added to main and the work that went into each feature.

## Contribution Ownership

Contributions to Adamant will abide by the [GitHub Terms of Service](https://docs.github.com/en/site-policy/github-terms/github-terms-of-service#6-contributions-under-repository-license) which states:

> Whenever you add Content to a repository containing notice of a license, you license that Content under the same terms, and you agree that you have the right to license that Content under those terms.

In short, you will own the copyright to the portion of the code that you modify, and that code will be licensed under the Apache-2.0 license.

## Questions?

Ask using the project's [discussions](https://github.com/lasp/adamant/discussions).
