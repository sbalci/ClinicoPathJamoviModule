# Claude review operational acceptance

A completed Claude review requires two independent observations:

1. The fresh Actions job succeeds and its execution guard accepts a terminal result with `subtype: success` and `is_error: false`.
2. Claude delivers review feedback to the corresponding pull request (a review submission, inline review comment, or conversation comment containing substantive review feedback).

A nonempty credential, successful GitHub App authentication, an initialization message, or a passed R check does not establish Claude model access or review delivery. A skipped execution must fail the terminal-result guard.

Use a small, same-repository documentation pull request created from the default branch after the reviewed workflow change is merged. The workflow must remain identical to the default branch. Observe the new run rather than rerunning a pre-merge workflow revision. Preserve GitHub App protections.

For failures, retain only fixed diagnostic categories and public run links. Categories are hints and can overlap; they do not prove credential expiration or model entitlement. Follow [the troubleshooting guide](claude-review-troubleshooting.md) for private credential/account checks. Never publish raw execution files, tokens, environment dumps, or account-identifying error text.

Local negative fixtures should reject missing/malformed results, initialization without a result, explicit Claude errors and failed action outcomes. Keep transcript suppression and strict failure handling enabled.
