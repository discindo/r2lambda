# Contributing to r2lambda

There are many ways to contribute to **r2lambda**. Some are quick (fixing typos, improving documentation, filing bug reports or feature requests), others take more time (answering questions, submitting pull requests with code changes). Help in any form is appreciated.

## Filing issues

If you believe you found a bug, create a minimal [reprex](https://reprex.tidyverse.org) and post it to the [issue tracker](https://github.com/discindo/r2lambda/issues). Include only the code needed to reproduce the bug. A good reprex cuts down the back-and-forth needed to understand and run the problem.

## Answering questions

Answering questions is a great way to help. Questions are filed with the **Question** issue template. If you don't know the full answer, a partial one or a pointer to the relevant documentation is still useful. Be kind to everyone who asks.

## Making pull requests

Before opening a pull request (PR), please file an issue and describe the problem in some detail. For an enhancement, explain how the change makes things better for package users. For a bug fix, explain the bug and how the fix removes it, ideally with a [reprex](https://reprex.tidyverse.org). This upfront work opens a conversation that often leads to a better fix.

Once there is agreement that a PR would help, the following makes things go faster:

* Create a separate Git branch for each PR.
* Use a [Conventional Commits](https://www.conventionalcommits.org) PR title (e.g. `fix: ...`, `feat: ...`, `docs: ...`). Releases and `NEWS.md` are generated from these by release-please, so don't edit `NEWS.md` or `CHANGELOG.md` by hand.
* Format code with [air](https://posit-dev.github.io/air/) (`air format .`) and lint it with [jarl](https://jarl.etiennebacher.com) (`jarl check .`). Both run on every PR.
* Documentation uses [roxygen2](https://roxygen2.r-lib.org). Edit the roxygen comments in `R/` and run `devtools::document()`. Don't modify the `.Rd` files in `man/` by hand.
* Add [testthat](https://testthat.r-lib.org) tests for new functionality or fixed bugs. PRs with tests are easier to accept.
* Run `devtools::check()` before submitting, and check the GitHub Actions results on the PR. CI runs R CMD check with `--as-cran`, which also runs the examples inside `\donttest{}`.

## AI-assisted contributions

Contributions made with the help of AI coding assistants are welcome. We follow the spirit of the Linux kernel's [guidelines for AI coding assistants](https://docs.kernel.org/process/coding-assistants.html):

* The human submitting the PR is responsible for the contribution: review all AI-generated code, make sure it works and that you understand it.
* Disclose AI use in the PR description using the section in the PR template.
* Attribute AI help in commit messages with an `Assisted-by:` trailer, for example `Assisted-by: Claude Opus 5.5 <noreply@anthropic.com>`. Do not use `Co-Authored-By:` or `Signed-off-by:` for AI tools; only humans can take authorship and certify a contribution.
