---
title: Submit a Module
description: "Submit your jamovi module for review and inclusion in the jamovi library."
type: article
---

<!-- Vendored from https://github.com/jamovi/dev.jamovi.org/blob/07352e9b/src/pages/submit.astro
     (fetched 2026-10-08; published at https://dev.jamovi.org/submit). The page is an HTML form;
     its prose and fields are transcribed here, the hCaptcha/fetch script is omitted. -->

Submit your module for review and inclusion in the jamovi library. Before submitting, check that your
module meets the [library requirements](/tutorial/tuts0111-distributing-modules#step-1-preparing-for-submission).

The module's maintainer, as listed in its `jamovi/0000.yaml`, will receive an email asking them to
confirm the submission, and will be emailed again with the outcome.

## Form fields

| Field | Required | Notes |
| :--- | :--- | :--- |
| GitHub repository URL | Yes | `type="url"`, placeholder `https://github.com/owner/module` |
| Subdirectory | No | "if the module isn't at the root of the repository"; placeholder `e.g. jpower`; pattern `[A-Za-z0-9._\-]+(/[A-Za-z0-9._\-]+)*/?` |
| Comments | No | "an introduction to the module, or anything the reviewers should know"; max 5000 characters |

An hCaptcha must be completed. The form POSTs to `/submit` (a Cloudflare Worker) and shows the
server's `message` or `error` inline; captcha tokens are single use.
