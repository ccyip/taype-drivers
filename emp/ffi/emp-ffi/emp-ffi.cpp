#include "emp-ffi.h"
#include "emp-tool/circuits/integer.h"
#include <emp-sh2pc/emp-sh2pc.h>
#include <stdio.h>
#include <stdlib.h>

using namespace emp;

inline const Bit &obliv_int_to_bit(const obliv_int m) {
  auto m_ = static_cast<Integer *>(m);
  return m_->bits[0];
}

inline obliv_int obliv_int_of_bit(const Bit &b) {
  auto n = new Integer;
  Bit zero(false, PUBLIC);
  n->bits.resize(DRIVER_INT_SIZE, zero);
  n->bits[0] = b;
  return n;
}

void setup_driver(const char *addr, int port, int party, bool quiet) {
  NetIO *io = new NetIO(party == ALICE ? nullptr : addr, port, quiet);
  setup_semi_honest(io, party);
}

void finalize_driver() { finalize_semi_honest(); }

obliv_int obliv_int_new(int n, int party) {
  return new Integer(DRIVER_INT_SIZE, n, party);
}

void obliv_int_destroy(obliv_int n) {
  if (nullptr == n) {
    return;
  }

  delete static_cast<Integer *>(n);
}

int obliv_int_reveal(obliv_int m) {
  auto m_ = static_cast<Integer *>(m);
  return m_->reveal<int>();
}

bool obliv_bool_reveal(obliv_int m) {
  return obliv_int_to_bit(m).reveal<bool>();
}

obliv_int obliv_int_add(obliv_int m, obliv_int n) {
  auto m_ = static_cast<Integer *>(m);
  auto n_ = static_cast<Integer *>(n);
  return new Integer((*m_) + (*n_));
}

obliv_int obliv_int_sub(obliv_int m, obliv_int n) {
  auto m_ = static_cast<Integer *>(m);
  auto n_ = static_cast<Integer *>(n);
  return new Integer((*m_) - (*n_));
}

obliv_int obliv_int_mul(obliv_int m, obliv_int n) {
  auto m_ = static_cast<Integer *>(m);
  auto n_ = static_cast<Integer *>(n);
  return new Integer((*m_) * (*n_));
}

obliv_int obliv_int_div(obliv_int m, obliv_int n) {
  auto m_ = static_cast<Integer *>(m);
  auto n_ = static_cast<Integer *>(n);
  return new Integer((*m_) / (*n_));
}

obliv_int obliv_int_eq(obliv_int m, obliv_int n) {
  auto m_ = static_cast<Integer *>(m);
  auto n_ = static_cast<Integer *>(n);
  return obliv_int_of_bit((*m_) == (*n_));
}

obliv_int obliv_int_le(obliv_int m, obliv_int n) {
  auto m_ = static_cast<Integer *>(m);
  auto n_ = static_cast<Integer *>(n);
  return obliv_int_of_bit((*m_) <= (*n_));
}

obliv_int obliv_bool_not(obliv_int m) {
  auto m_ = static_cast<Integer *>(m);
  return obliv_int_of_bit(!obliv_int_to_bit(m_));
}

obliv_int obliv_bool_and(obliv_int m, obliv_int n) {
  auto m_ = static_cast<Integer *>(m);
  auto n_ = static_cast<Integer *>(n);
  return obliv_int_of_bit(obliv_int_to_bit(m_) & obliv_int_to_bit(n_));
}

obliv_int obliv_bool_or(obliv_int m, obliv_int n) {
  auto m_ = static_cast<Integer *>(m);
  auto n_ = static_cast<Integer *>(n);
  return obliv_int_of_bit(obliv_int_to_bit(m_) | obliv_int_to_bit(n_));
}

obliv_int obliv_int_mux(obliv_int s, obliv_int m, obliv_int n) {
  auto m_ = static_cast<Integer *>(m);
  auto n_ = static_cast<Integer *>(n);
  return new Integer(If(obliv_int_to_bit(s), *m_, *n_));
}
