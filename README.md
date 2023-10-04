# CDEPS
Community Data Models for Earth Prediction Systems

For documentation see

https://escomp.github.io/CDEPS/versions/master/html/index.html

## A note on github tag action

This repository is setup to automatically create tags on merge to
main using .github/workflows/bumpversion.yml It uses
https://github.com/mathieudutour/github-tag-action  to look for
keywords in commit messages and determine what the new version should
be.  The default if no keywords is found is to bump the patch version.
