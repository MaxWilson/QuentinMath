pushd ~%dp0
git worktree add ..\BMathPublish gh-pages
git checkout master && npm run build && (robocopy /s deploy ..\BMathPublish || echo OK) && pushd ..\BMathPublish && git add . && git commit -m "New Version" && git push && popd
