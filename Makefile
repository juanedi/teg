build:
	ghc \
    --make \
    -isrc \
    -odir _build \
    -hidir _build \
    -o bin/teg \
    -fwarn-unused-imports -Wno-name-shadowing \
    -XOverloadedStrings \
    app/Main.hs

push-release:
	@[ "${TAG}" ] || (echo "TAG variable not set"; exit 1)
	git tag -a ${TAG} -m "release version ${TAG}"
	git push origin ${TAG}
	@echo The GH tag was pushed! 🎉
	@echo Check out https://circleci.com/dashboard to see how the build/publish process is going.
