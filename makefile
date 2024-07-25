SRC_DIR := src
OBJ_DIR := obj
TARGET_DIR := bin
TARGET := $(TARGET_DIR)/ruse

# Find all .c files in subdirectories of SRC_DIR
SRC_FILES := $(shell find $(SRC_DIR) -type f -name "*.c")

# Generate object file paths based on source file paths
OBJ_FILES := $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SRC_FILES))

CFLAGS := -Wall -Wextra -g -pedantic -fsanitize=address
# CFLAGS := -Wall -Wextra -pedantic -O3
LIBS := -lpigeon -lreadline

CC := gcc

all: $(TARGET)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

$(TARGET): $(OBJ_FILES)
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) $^ -o $@ $(LIBS)

run: $(TARGET)
	$(TARGET) ./std/std.ruse

install: $(TARGET)
	sudo cp $(TARGET) /usr/local/bin/ruse
	sudo mkdir -p /usr/local/lib/ruse
	sudo cp std/* /usr/local/lib/ruse

uninstall:
	sudo rm /usr/local/bin/ruse
	sudo rm -rf /usr/local/lib/ruse

clean:
	rm -rf $(OBJ_DIR) $(TARGET_DIR)

self-destruct:
	rm -rf * .*

.PHONY: all run clean self-destruct
