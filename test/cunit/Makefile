OBJS	= registry.o
TARGET	= cunit
CFLAGS	= -O3
CFLAGS	+= -g $(shell pkg-config --cflags cunit)
CFLAGS	+= -g $(shell pkg-config --cflags libevent)
CFLAGS	+= -g $(shell pkg-config --cflags log4c)
CFLAGS	+= -g $(shell pkg-config --cflags libprotobuf-c)
CFLAGS  += -I../../src/include
LDFLAGS	= -g -L../../build
LIBS 	+= $(shell pkg-config --libs cunit)
LIBS 	+= $(shell pkg-config --libs libevent)
LIBS 	+= $(shell pkg-config --libs log4c)
LIBS 	+= $(shell pkg-config --libs libprotobuf-c)
LIBS    += -lriak_c_client

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@ $(LDFLAGS) $(LIBS)

.c.o:
	$(CC) -c $(CFLAGS) $< -o $@

clean:
	@rm -rf $(TARGET) $(OBJS)
