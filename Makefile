all:
	activator stage

run:
	killall -9 java
	rm target/universal/stage/RUNNING_PID
	(target/universal/stage/bin/character-sheets-new-website > logs/application.log &)

log:
	tailf logs/application.log

help:
	@echo
	@echo "  make        Build the application"
	@echo "  make run    Run the application"
	@echo "  make log    Show the application logs"
	@echo