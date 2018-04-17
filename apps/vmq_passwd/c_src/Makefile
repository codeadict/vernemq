CC?=gcc

OPENSSL_PATH=$(shell which openssl)/../..
CFLAGS=-L$(OPENSSL_PATH)/lib -I$(OPENSSL_PATH)/include

all: compile

compile:
	$(CC) $(CFLAGS) vmq_passwd.c -lcrypto -o ../priv/vmq_passwd
