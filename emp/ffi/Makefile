PREFIX ?=
CMAKEOPT ?=

ifdef PREFIX
CMAKEOPT="-DCMAKE_INSTALL_PREFIX=$(PREFIX)"
endif

define run-install
mkdir -p $</build
cd $</build; cmake $(CMAKEOPT) -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..; make -j$(shell nproc) install
endef

install: install-emp-tool install-emp-ot install-emp-sh2pc install-ffi

install-emp-tool: emp-tool
	$(run-install)

install-emp-ot: emp-ot
	$(run-install)

install-emp-sh2pc: emp-sh2pc
	$(run-install)

install-ffi: emp-ffi
	$(run-install)

clean:
	rm -rf emp-tool/build emp-ot/build emp-sh2pc/build emp-ffi/build

.PHONY: install install-emp-tool install-emp-ot install-emp-sh2pc install-ffi clean
