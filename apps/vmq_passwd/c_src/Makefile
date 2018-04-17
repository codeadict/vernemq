CC? = gcc
OPENSSL_PATH := $(which openssl)

CFLAGS="-L$(OPENSSL_PATH)/lib -I$(OPENSSL_PATH)/include"

compile:
	$(CC) $(CFLAGS) vmq_passwd.c -lcrypto -o ../priv/vmq_passwd
