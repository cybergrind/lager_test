
USER=$(shell whoami)
CGNAME=lager
CG_SUBSYSTEMS=cpuset,memory,blkio,cpu,cpuacct,devices,freezer,net_cls


cgroups_init:
	sudo cgcreate -a $(USER):$(USER) -f 777 -d 755 -s 777 -g $(CG_SUBSYSTEMS):$(CGNAME)
cgroups_clean:
	sudo cgdelete $(CG_SUBSYSTEMS):$(CGNAME)

compile:
	rebar compile

dev: compile
	@erl -config test -pa deps/*/ebin -pa apps/*/ebin -s lager -s t

cg_dev: compile
	cgexec -g *:$(CGNAME) erl -config test -pa deps/*/ebin -pa apps/*/ebin -s lager -s t
