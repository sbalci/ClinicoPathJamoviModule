# Claude review execution troubleshooting

## Findings from 2026-10-15

The October 12 runs for [Mixed Cox](https://github.com/sbalci/ClinicoPathJamoviModule/actions/runs/38231857350)
and [classical survival power](https://github.com/sbalci/ClinicoPathJamoviModule/actions/runs/38231834613)
both installed Claude Code 2.1.296 and used action commit
`1d6de8cb0c237e7c15e9e1bdf973826ebae490cc`.
GitHub OIDC exchange, Claude GitHub App token creation and actor authorization succeeded.
The OAuth secret input was nonempty and masked. These checks do **not** validate
the separate Claude subscription credential or model entitlement.

Both initialized with `claude-sonnet-5-5`, then returned `subtype: success`,
`is_error: true`, one turn, zero reported cost, empty model usage and no recorded
permission denials (2174 ms / 2284 ms). The action correctly exited 1.
No successful model inference or completed review is evidenced. Zero cost alone
is not an authentication test. Successful R validation is a separate check.

The action's sanitized result logger omits `result` text. No execution artifacts
were retained for these runs, so expiration, revocation, account access, model
availability, quota/service failure and CLI/SDK issues cannot be distinguished.
There is no evidence supporting a model change or broader GitHub permissions.

## Workflow changes

- Check only whether the OAuth secret is available, without printing it.
- Use `--setting-sources user,project`: preserve action-created user settings and
  shared project settings; exclude tracked workstation-specific local settings
  containing a macOS PATH and local permissions. This is a portability correction,
  not a proven cause of the October 12 failures.
- Keep full transcript logging disabled. Read the execution JSON in memory after
  success/failure and emit only fixed status labels and heuristic error categories.
  On action failure, fall back to its documented implementation path in RUNNER_TEMP
  when the execution_file output is unavailable. Do not upload raw transcripts.
- Require both a successful action outcome and a terminal success result with
  `is_error: false`. Missing/malformed results and startup errors fail verification.
  This verifies execution, not review quality or comment delivery.
- Limit the job to 30 minutes; retain existing GitHub App authentication and permissions.

## Live validation of this change

[Run 38734392859](https://github.com/sbalci/ClinicoPathJamoviModule/actions/runs/38734392859)
passed the credential-availability preflight. The Claude GitHub App then skipped
execution because the modified workflow did not match the default-branch copy.
The action step reported success for that skip, but the new verification step
correctly failed because no terminal review result existed. This verifies the
missing-result guard on GitHub; it does not test Claude OAuth or model access.
Do not bypass the App's workflow-validation protection.

## Maintainer recovery and validation

1. After this workflow change is reviewed and merged to the default branch,
   start a **new** PR synchronize run with an identical workflow copy on the PR
   branch. The Claude GitHub App will skip execution on the workflow-changing PR
   before merge. Rerunning an old run uses its old workflow.
2. Use the new step summary's categories as hints only. Do not publish raw JSON,
   debug logs, environment dumps, tokens or account-identifying error text.
3. If authentication is indicated, or the credential's origin/validity is unknown:
   on a trusted local computer, sign into the intended Claude subscription and run
   `claude setup-token`. Replace the **existing** repository Actions secret named
   `CLAUDE_CODE_OAUTH_TOKEN` at Settings → Secrets and variables → Actions.
   Do not paste the token in chat, source, an issue or a PR. A normal interactive
   login does not update the GitHub secret. No secret value was read during this audit.
4. Verify that the subscription/account is active and the selected model is allowed.
   Test a minimal `claude -p` request outside this repository, using the intended
   CI credential supplied privately as CLAUDE_CODE_OAUTH_TOKEN (not a command-line
   literal). Use `--setting-sources user,project` and the model shown in the failing
   init record. Inspect errors locally. A cached interactive login alone is not
   proof that the stored CI credential works. If model access is denied, choose a
   model confirmed available to that account before adding an explicit --model.
5. For access/policy, quota/billing or service hints, resolve the corresponding
   account restriction, usage limit or service problem. Do not blindly rotate a
   token for every error. If the isolated request works but CI still fails, compare
   CLI/SDK versions and remaining shared project settings; inspect the raw execution
   file only in a controlled private debugging environment.
6. Confirm a fresh Actions run is green with a terminal non-error result and actual
   review feedback. Do not weaken the check with continue-on-error or treat init as
   a successful review. Fork/Dependabot PR secret unavailability is intentional;
   do not switch to pull_request_target to expose credentials.

Sources: [action setup](https://github.com/anthropics/claude-code-action/blob/main/docs/setup.md),
[SDK result handling at the failing revision](https://github.com/anthropics/claude-code-action/blob/1d6de8cb0c237e7c15e9e1bdf973826ebae490cc/base-action/src/run-claude-sdk.ts),
[execution-file handling](https://github.com/anthropics/claude-code-action/blob/1d6de8cb0c237e7c15e9e1bdf973826ebae490cc/base-action/src/execution-file.ts),
[CLI reference](https://code.claude.com/docs/en/cli-reference).
