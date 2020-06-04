#ifndef CISH_SET_H
#define CISH_SET_H

#include <set>

namespace cish {

template <typename T, typename Compare = std::less<T>>
class Set {
protected:
  std::set<T, Compare> _impl;

public:
  using key_type = typename std::set<T>::key_type;
  using value_type = typename std::set<T>::value_type;
  using size_type = typename std::set<T>::size_type;
  using reference = typename std::set<T>::reference;
  using const_reference = typename std::set<T>::const_reference;
  using pointer = typename std::set<T>::pointer;
  using const_pointer = typename std::set<T>::const_pointer;
  using iterator = typename std::set<T>::iterator;
  using const_iterator = typename std::set<T>::const_iterator;

protected:
  std::set<T>& getImpl() {
    return _impl;
  }

  const std::set<T>& getImpl() const {
    return _impl;
  }

public:
  Set() {
    ;
  }

  explicit Set(const Compare& comp) : _impl(comp) {
    ;
  }

  explicit Set(const T& value) {
    _impl.insert(value);
  }

  template <typename InputIt>
  Set(InputIt first, InputIt last) : _impl(first, last) {
    ;
  }

  Set(const Set& other) : _impl(other.getImpl()) {
    ;
  }

  Set(Set&& other) : _impl(other.getImpl()) {
    ;
  }

  Set(std::initializer_list<T> init) : _impl(init) {
    ;
  }

  virtual ~Set() = default;

  Set<T>& operator=(const Set<T>& other) {
    _impl = other.getImpl();
    return *this;
  }

  Set<T>& operator=(Set<T>&& other) {
    _impl = other.getImpl();
    return *this;
  }

  Set<T>& operator=(std::initializer_list<T> init) {
    _impl = init;
    return *this;
  }

  iterator begin() {
    return _impl.begin();
  }

  const_iterator begin() const {
    return _impl.begin();
  }

  iterator end() {
    return _impl.end();
  }

  const_iterator end() const {
    return _impl.end();
  }

  bool empty() const {
    return _impl.empty();
  }

  size_type size() const {
    return _impl.size();
  }

  void clear() {
    return _impl.clear();
  }

  template <typename... Args>
  bool emplace(Args&&... args) {
    return _impl.emplace(args...).second;
  }

  bool insert(const T& value) {
    return _impl.insert(value).second;
  }

  template <typename InputIt>
  void insert(InputIt first, InputIt last) {
    _impl.insert(first, last);
  }

  bool insert(const Set<T>& other) {
    bool changed = false;

    for(const T e : other)
      changed |= _impl.insert(e).second;

    return changed;
  }

  iterator erase(const_iterator pos) {
    return _impl.erase(pos);
  }

  iterator erase(const_iterator first, const_iterator last) {
    return _impl.erase(first, last);
  }

  size_type erase(const T& key) {
    return _impl.erase(key);
  }

  bool contains(const T& e) const {
    return (_impl.find(e) != _impl.end());
  }

  bool equals(const Set<T>& other) const {
    if(this->size() != other.size())
      return false;
    for(auto e : other)
      if(not contains(e))
        return false;
    return true;
  }

  bool disjoint(const Set<T>& other) const {
    for(const T& e : other)
      if(contains(e))
        return false;
    return true;
  }

  bool operator==(const Set<T>& c2) const {
    return _impl == c2.getImpl();
  }

  bool operator!=(const Set<T>& c2) const {
    return _impl != c2.getImpl();
  }

  bool operator<(const Set<T>& c2) const {
    return _impl < c2.getImpl();
  }

  bool operator<=(const Set<T>& c2) const {
    return _impl <= c2.getImpl();
  }

  bool operator>(const Set<T>& c2) const {
    return _impl > c2.getImpl();
  }

  bool operator>=(const Set<T>& c2) const {
    return _impl >= c2.getImpl();
  }
};

} // namespace cish

#endif // CISH_SET_H
