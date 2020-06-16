#ifndef CISH_VECTOR_H
#define CISH_VECTOR_H

#include <algorithm>
#include <vector>

namespace cish {

template <typename T,
          typename std::enable_if_t<!std::is_same<T, bool>::value, int> = 0>
class Vector {
protected:
  std::vector<T> _impl;

public:
  using value_type = typename std::vector<T>::value_type;
  using size_type = typename std::vector<T>::size_type;
  using reference = typename std::vector<T>::reference;
  using const_reference = typename std::vector<T>::const_reference;
  using pointer = typename std::vector<T>::pointer;
  using const_pointer = typename std::vector<T>::const_pointer;
  using iterator = typename std::vector<T>::iterator;
  using const_iterator = typename std::vector<T>::const_iterator;
  using reverse_iterator = typename std::vector<T>::reverse_iterator;
  using const_reverse_iterator =
      typename std::vector<T>::const_reverse_iterator;

protected:
  std::vector<T>& getImpl() {
    return _impl;
  }

  const std::vector<T>& getImpl() const {
    return _impl;
  }

public:
  Vector() {
    ;
  }

  explicit Vector(size_type count) : _impl(count) {
    ;
  }

  explicit Vector(size_type count, const T& value) : _impl(count, value) {
    ;
  }

  Vector(const Vector& other) : _impl(other.getImpl()) {
    ;
  }

  Vector(Vector&& other) : _impl(other.getImpl()) {
    ;
  }

  template <typename Iterator>
  Vector(Iterator first, Iterator last) : _impl(first, last) {
    ;
  }

  Vector(std::initializer_list<T> init) : _impl(init) {
    ;
  }

  Vector<T>& operator=(const Vector<T>& other) {
    _impl = other.getImpl();
    return *this;
  }

  Vector<T>& operator=(Vector<T>&& other) {
    _impl = other.getImpl();
    return *this;
  }

  Vector<T>& operator=(std::initializer_list<T> init) {
    _impl = init;
    return *this;
  }

  T& at(size_type i) {
    return _impl.at(i);
  }

  const T& at(size_type i) const {
    return _impl.at(i);
  }

  T& operator[](size_type i) {
    return _impl[i];
  }

  const T& operator[](size_type i) const {
    return _impl[i];
  }

  T& front() {
    return _impl.front();
  }

  const T& front() const {
    return _impl.front();
  }

  T& back() {
    return _impl.back();
  }

  const T& back() const {
    return _impl.back();
  }

  T* data() {
    return _impl.data();
  }

  const T* data() const {
    return _impl.data();
  }

  iterator begin() {
    return _impl.begin();
  }

  const_iterator begin() const {
    return _impl.begin();
  }

  reverse_iterator rbegin() {
    return _impl.rbegin();
  }

  const_reverse_iterator rbegin() const {
    return _impl.rbegin();
  }

  iterator end() {
    return _impl.end();
  }

  const_iterator end() const {
    return _impl.end();
  }

  reverse_iterator rend() {
    return _impl.rend();
  }

  const_reverse_iterator rend() const {
    return _impl.rend();
  }

  bool empty() const {
    return _impl.empty();
  }

  size_type size() const {
    return _impl.size();
  }

  bool contains(const T& value) const {
    for(const auto& e : _impl)
      if(e == value)
        return true;
    return false;
  }

  iterator find(const T& value) const {
    return this->find(value, _impl.begin(), _impl.end());
  }

  iterator find(const T& value, const_iterator begin) const {
    return this->find(value, begin, _impl.end());
  }

  iterator
  find(const T& value, const_iterator begin, const_iterator end) const {
    for(auto it = begin; it != end; it++)
      if(*it == value)
        return it;
    return end;
  }

  reverse_iterator rfind(const T& value) const {
    return this->rfind(value, _impl.rbegin(), _impl.rend());
  }

  reverse_iterator rfind(const T& value, const_reverse_iterator begin) const {
    return this->rfind(value, begin, _impl.rend());
  }

  reverse_iterator rfind(const T& value,
                         const_reverse_iterator begin,
                         const_reverse_iterator end) const {
    for(auto it = begin; it != end; it++)
      if(*it == value)
        return it;
    return end;
  }

  void clear() {
    return _impl.clear();
  }

  template <typename... Args>
  reference emplace_back(Args&&... args) {
    _impl.emplace_back(args...);
    return _impl.back();
  }

  void push_back(const T& value) {
    _impl.push_back(value);
  }

  void push_back(const Vector<T>& other) {
    for(const T& e : other)
      push_back(e);
  }

  value_type pop_back() {
    value_type v = back();
    _impl.pop_back();
    return v;
  }

  iterator erase(const_iterator pos) {
    return _impl.erase(pos);
  }

  iterator erase(const_iterator begin, const_iterator end) {
    return _impl.erase(begin, end);
  }

  iterator erase(const T& value) {
    return this->erase(value, _impl.begin(), _impl.end());
  }

  iterator erase(const T& value, const_iterator begin) {
    return this->erase(value, begin, _impl.end());
  }

  iterator erase(const T& value, const_iterator begin, const_iterator end) {
    for(auto it = begin; it != end; it++)
      if(*it == value)
        return _impl.erase(it);
    return _impl.end();
  }

  reverse_iterator rerase(const T& value) {
    return this->rerase(value, _impl.rbegin(), _impl.rend());
  }

  reverse_iterator rerase(const T& value, const_reverse_iterator begin) {
    return this->rerase(value, begin, _impl.rend());
  }

  reverse_iterator rerase(const T& value,
                          const_reverse_iterator begin,
                          const_reverse_iterator end) {
    for(auto it = begin; it != end; it++)
      if(*it == value)
        return it;
    return _impl.rend();
  }

  void erase_all(const T& value) {
    this->erase_all(value, _impl.begin(), _impl.end());
  }

  void erase_all(const T& value, const_iterator begin) {
    this->erase_all(value, begin, _impl.end());
  }

  void erase_all(const T& value, const_iterator begin, const_iterator end) {
    auto it = begin;
    do {
      it = this->erase(value, it, end);
    } while(it != end);
  }

  void reverse() {
    std::reverse(_impl.begin(), _impl.end());
  }

  bool operator==(const Vector<T>& c2) const {
    return _impl == c2._impl;
  }

  bool operator!=(const Vector<T>& c2) const {
    return _impl != c2._impl;
  }

  bool operator<(const Vector<T>& c2) const {
    return _impl < c2._impl;
  }

  bool operator<=(const Vector<T>& c2) const {
    return _impl <= c2._impl;
  }

  bool operator>(const Vector<T>& c2) const {
    return _impl > c2._impl;
  }

  bool operator>=(const Vector<T>& c2) const {
    return _impl >= c2._impl;
  }
};

} // namespace cish

#endif // CISH_VECTOR_H
