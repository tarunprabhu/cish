#ifndef CISH_DEREF_ITER_H
#define CISH_DEREF_ITER_H

#include <cstddef>
#include <type_traits>

namespace cish {

template <typename IterT>
class DerefIter : public IterT {
public:
  using value_type =
      typename std::remove_pointer<typename IterT::value_type>::type;
  using pointer = value_type*;
  using reference = value_type&;

  DerefIter(const IterT& other) : IterT(other) {
    ;
  }

  reference operator*() {
    return *(this->IterT::operator*());
  }

  pointer operator->() {
    return this->IterT::operator*();
  }
};

} // namespace cish

#endif // CISH_DEREF_ITER_H
