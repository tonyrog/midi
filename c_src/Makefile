#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

APP=midi
CC=gcc
CFLAGS+=-std=c99 -Wall -O3 -DALSA
CFLAGS+= $(shell pkg-config --cflags alsa)
ALSA_LIBS=$(shell pkg-config --libs alsa)
LDFLAGS+=-shared

OBJS = midi_nif.o

all: ../priv/$(APP)_alsa.so

../priv/$(APP)_alsa.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(ALSA_LIBS) -o ../priv/$(APP)_alsa.so

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
