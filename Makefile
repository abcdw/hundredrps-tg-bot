do:
	echo hi

check:
	clojure -X:test:test-runner

update-profile: rde/channels-lock.scm
	guix time-machine -C rde/channels-lock.scm -- \
	shell -Df ./rde/guix.scm -r ./rde/guix-profile -- echo hi

channels-update-lock:
	guix time-machine -C ./rde/channels.scm -- \
	describe -f channels > ./rde/channels-lock.scm

clean:
	rm ./rde/guix-profile
