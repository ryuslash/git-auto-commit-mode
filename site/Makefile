.PHONY: export publish

all:

export:
	emacs -batch -l project.el -f org-publish-all

publish: export
	scp _publish/* \
		ryuslash.org:public_html/orgweb/projects/git-auto-commit-mode
