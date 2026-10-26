all: build

.PHONY: all build clean frontend generate-static run dev-start dev-stop tests e2e mobile

SBCL   = sbcl --noinform --non-interactive
SHADOW = npx shadow-cljs

build: clean frontend generate-static
	$(SBCL) \
	  --eval '(asdf:load-asd (truename "mtm-server.asd"))' \
	  --eval '(ql:quickload (list "clack" "clack-handler-wookie" "yason" "conskivi-fileonly" "bordeaux-threads" "quri" "babel" "cl-base64"))' \
	  --eval '(asdf:load-system "mtm-server")' \
	  --eval '(push (lambda () (format t "~&dropping ~d saved shared object(s)~%" (length sb-sys:*shared-objects*)) (setf sb-sys:*shared-objects* nil)) uiop:*image-dump-hook*)' \
	  --eval '(ensure-directories-exist "build")' \
	  --eval '(asdf:make "mtm-server")'
	@if [ -n "$(BUILD_SUFFIX)" ]; then mv build/math-training-machine build/math-training-machine$(BUILD_SUFFIX); fi

clean:
	rm -rf build

frontend:
	@test -d node_modules || npm install
	$(SHADOW) release app

generate-static:
	$(SBCL) --load tools/build-static.lisp

# run straight from source (fresh assets, no image dump)
run:
	$(SBCL) --load server/run.lisp

dev-start:
	nohup ./build/math-training-machine > /tmp/mtm.log 2>&1 &

dev-stop:
	-pkill -f ./build/math-training-machine

# API smoke test against a throwaway server instance
tests:
	./test/server-tests.sh

# browser end-to-end test, needs `make tests`-style server already running
e2e:
	node test/e2e.mjs

# phone-sized touch probe (390x844) against a running server
mobile:
	node test/mobile.mjs
