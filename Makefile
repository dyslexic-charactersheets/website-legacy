stage:
	./activator stage

run:
	killall -9 java
	[ ! -e target/universal/stage/RUNNING_PID ] || rm target/universal/stage/RUNNING_PID
	mkdir -p logs
	(target/universal/stage/bin/character-sheets-new-website > logs/application.log &)

start: run

stop:
	killall -9 java
	[ ! -e target/universal/stage/RUNNING_PID] || rm target/universal/stage/RUNNING_PID

log:
	mkdir -p logs
	tailf logs/application.log

help:
	@echo
	@echo "  make        Build the application"
	@echo "  make run    Run the application"
	@echo "  make log    Show the application logs"
	@echo
