#ifndef CISH_STACK_H
#define CISH_STACK_H

#include <stack>

namespace cish {

template <typename T>
class Stack {
protected:
  std::stack<T> _impl;

public:
  using value_type = typename std::stack<T>::value_type;
  using size_type = typename std::stack<T>::size_type;
  using reference = typename std::stack<T>::reference;
  using const_reference = typename std::stack<T>::const_reference;

protected:
  std::stack<T>& getImpl() {
    return _impl;
  }

  const std::stack<T>& getImpl() const {
    return _impl;
  }

public:
  Stack() {
    ;
  }

  template <typename InputIt>
  Stack(InputIt first, InputIt last) : _impl(first, last) {
    ;
  }

  Stack(const Stack<T>& other) : _impl(other._impl) {
    ;
  }

  Stack(Stack<T>&& other) : _impl(other._impl) {
    ;
  }

  Stack(std::initializer_list<T> init) : _impl(init) {
    ;
  }

  virtual ~Stack() = default;

  Stack<T>& operator=(const Stack<T>& other) {
    _impl = other.getImpl();
    return *this;
  }

  Stack<T>& operator=(Stack<T>&& other) {
    _impl = other.getImpl();
    return *this;
  }

  reference top() {
    return _impl.top();
  }

  const_reference top() const {
    return _impl.top();
  }

  bool empty() const {
    return _impl.empty();
  }

  size_type size() const {
    return _impl.size();
  }

  template <typename... Args>
  reference emplace(Args&&... args) {
    _impl.emplace(args...);
    return top();
  }

  void push(const T& value) {
    _impl.push(value);
  }

  value_type pop() {
    T value = std::move(_impl.top());
    _impl.pop();
    return value;
  }

  void clear() {
    while(not empty())
      pop();
  }

  bool operator==(const Stack<T>& c2) const {
    return _impl == c2._impl;
  }

  bool operator!=(const Stack<T>& c2) const {
    return _impl != c2._impl;
  }

  bool operator<(const Stack<T>& c2) const {
    return _impl < c2._impl;
  }

  bool operator<=(const Stack<T>& c2) const {
    return _impl <= c2._impl;
  }

  bool operator>(const Stack<T>& c2) const {
    return _impl > c2._impl;
  }

  bool operator>=(const Stack<T>& c2) const {
    return _impl >= c2._impl;
  }
};

} // namespace cish

#endif // CISH_STACK_H
