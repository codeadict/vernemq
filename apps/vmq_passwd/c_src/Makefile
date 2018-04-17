CC? = gcc
CFLAGS_EXTRA =

ifeq ($(shell uname -s), Darwin)
	# OSX with homebrew
	OPENSSL_DIR ?= /usr/local/opt/openssl
else
	OPENSSL_DIR ?= /usr
endif
CFLAGS_EXTRA += -L${OPENSSL_DIR}/lib -I${OPENSSL_DIR}/include

all: compile

compile:
	$(CC) $(CFLAGS) $(CFLAGS_EXTRA) vmq_passwd.c -lcrypto -o ../priv/vmq_passwd
