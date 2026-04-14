NixCon contributor vouchers
===========================

This repository contains two things:
- a web server that generates vouchers for eligible contributors
- a generation script that generates the list of eligible contributors

## Eligibility criteria

We consider a contributor eligible when they have made *recent contributions* to any *official Nix project*.
 - We count the number of *commits* in *merged* pull requests that have been *opened in the past 4 years*
 - We attribute all commits of a PR to the PR author
 - The more contributions you've made, the higher your discount will be
 - Official Nix projects are those described here: https://github.com/NixOS/org/blob/main/doc/github.md
 - Suspended contributors are removed from the list
 - NixCon organisers and others who have done significant work outside PRs are always eligible
