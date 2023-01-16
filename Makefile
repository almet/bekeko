publish:
	rm -rf build && elm-app build && echo "bekeko.notmyidea.org" > build/CNAME && ghp-import build && git push origin gh-pages