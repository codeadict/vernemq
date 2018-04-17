CC? = gcc
CFLAGS_EXTRA =


ifeq ($(shell uname -s), Darwin)
	OPENSSL_PATH = $(shell which openssl)/../..
	CFLAGS_EXTRA += -L$(OPENSSL_PATH)/lib -I$(OPENSSL_PATH)/include
endif

all: compile

compile:
	$(CC) $(CFLAGS) $(CFLAGS_EXTRA) vmq_passwd.c -lcrypto -o ../priv/vmq_passwd
