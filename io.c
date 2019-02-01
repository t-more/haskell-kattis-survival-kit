#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

char get_char() {
  return getchar_unlocked();
}

char next_char() {
  char c = getchar_unlocked();
  while(c == 32 || c == 10) {
    c = getchar_unlocked();
  }
  return c;
}

uint32_t next_word32() {
  char c = getchar_unlocked();
  while ('0' > c || '9' < c) {
    c = getchar_unlocked();
  }
  int x = 0;
  while ('0' <= c && c <= '9') {
    x = 10 * x + c - 48;
    c = getchar_unlocked();
  }
  return x;
}

uint64_t next_word64() {
  char c = getchar_unlocked();
  while ('0' > c || '9' < c) {
    c = getchar_unlocked();
  }
  int x = 0;
  while ('0' <= c && c <= '9') {
    x = 10 * x + c - 48;
    c = getchar_unlocked();
  }
  return x;
}

int32_t next_int32() {
  char c = getchar_unlocked();
  char prefix_char = 0;
  while ('0' > c || '9' < c) {
    prefix_char = c;
    c = getchar_unlocked();
  }
  int x = 0;
  while ('0' <= c && c <= '9') {
    x = 10 * x + c - '0';
    c = getchar_unlocked();
  }
  return (((prefix_char != '-') << 1) - 1) * x;
}

int64_t next_int64() {
  char c = getchar_unlocked();
  char prefix_char = 0;
  while ('0' > c || '9' < c) {
    prefix_char = c;
    c = getchar_unlocked();
  }
  int x = 0;
  while ('0' <= c && c <= '9') {
    x = 10 * x + c - '0';
    c = getchar_unlocked();
  }
  return (((prefix_char != '-') << 1) - 1) * x;
}

int32_t get_int32() {
  char c = getchar_unlocked();
  int32_t sign = 1;
  if (c == '-') {
    c = getchar_unlocked();
    sign = -1;
  }
  int32_t x = 0;
  while ('0' <= c && c <= '9') {
    x = 10 * x + c - '0';
    c = getchar_unlocked();
  }
  return sign * x;
}

int64_t get_int64() {
  char c = getchar_unlocked();
  int64_t sign = 1;
  if (c == '-') {
    c = getchar_unlocked();
    sign = -1;
  }
  int64_t x = 0;
  while ('0' <= c && c <= '9') {
    x = 10 * x + c - '0';
    c = getchar_unlocked();
  }
  return sign * x;
}


void println_int64(int64_t i) {
  printf("%lld\n", i);
}

void print_spaced_int64(int64_t i) {
  printf("%lld ", i);
}

void println_int32(int32_t i) {
  printf("%ld\n", i);
}

void print_spaced_int32(int32_t i) {
  printf("%ld ", i);
}

void print_space() {
  printf(" ");
}

void print_newline() {
  puts("");
}
