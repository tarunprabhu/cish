#ifndef CISH_LIST_H
#define CISH_LIST_H

#include <algorithm>
#include <list>

namespace cish {

template <typename T>
class List {
protected:
  std::list<T> _impl;

public:
  using value_type = typename std::list<T>::value_type;
  using size_type = typename std::list<T>::size_type;
  using reference = typename std::list<T>::reference;
  using const_reference = typename std::list<T>::const_reference;
  using pointer = typename std::list<T>::pointer;
  using const_pointer = typename std::list<T>::const_pointer;
  using iterator = typename std::list<T>::iterator;
  using const_iterator = typename std::list<T>::const_iterator;
  using reverse_iterator = typename std::list<T>::reverse_iterator;
  using const_reverse_iterator = typename std::list<T>::const_reverse_iterator;

protected:
  std::list<T>& getImpl() {
    return _impl;
  }

  const std::list<T>& getImpl() const {
    return _impl;
  }

public:
  List() {
    ;
  }

  explicit List(size_type count) : _impl(count) {
    ;
  }

  explicit List(size_type count, const T& value) : _impl(count, value) {
    ;
  }

  template <typename InputIt>
  List(InputIt first, InputIt last) : _impl(first, last) {
    ;
  }

  List(const List& other) : _impl(other.getImpl()) {
    ;
  }

  List(List&& other) : _impl(other.getImpl()) {
    ;
  }

  List(std::initializer_list<T> init) : _impl(init) {
    ;
  }

  List<T>& operator=(const List& other) {
    _impl = other._impl;
    return *this;
  }

  List<T>& operator=(List&& other) {
    _impl = other._impl;
    return *this;
  }

  List<T>& operator=(std::initializer_list<T> init) {
    _impl = init;
    return *this;
  }

  void assign(size_type count, const_reference value) {
    _impl.assign(count, value);
  }

  template <typename InputIt>
  void assign(InputIt first, InputIt last) {
    _impl.assign(first, last);
  }

  void assign(std::initializer_list<T> init) {
    _impl.assign(init);
  }

  reference operator[](size_t n) {
    return *std::next(begin(), n);
  }

  reference at(size_t n) {
    return *std::next(begin(), n);
  }

  const_reference operator[](size_t n) const {
    return *std::next(begin(), n);
  }

  const_reference at(size_t n) const {
    return *std::next(begin(), n);
  }

  reference front() {
    return _impl.front();
  }

  const_reference front() const {
    return _impl.front();
  }

  reference back() {
    return _impl.back();
  }

  const_reference back() const {
    return _impl.back();
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

  size_type max_size() const {
    return _impl.max_size();
  }

  bool contains(const_reference value) const {
    for(const auto& e : _impl)
      if(e == value)
        return true;
    return false;
  }

  iterator find(T& value) {
    return this->find(value, _impl.begin(), _impl.end());
  }

  iterator find(T& value, iterator first) {
    return this->find(value, first, _impl.end());
  }

  iterator find(T& value, iterator first, iterator last) {
    for(auto it = first; it != last; it++)
      if(*it == value)
        return iterator(it);
    return _impl.end();
  }

  const_iterator find(const T& value) const {
    return this->find(value, _impl.begin(), _impl.end());
  }

  const_iterator find(const T& value, const_iterator first) const {
    return this->find(value, first, _impl.end());
  }

  const_iterator
  find(const T& value, const_iterator first, const_iterator last) const {
    for(auto it = first; it != last; it++)
      if(*it == value)
        return iterator(it);
    return _impl.end();
  }

  reverse_iterator rfind(const T& value) const {
    return this->rfind(value, _impl.begin(), _impl.end());
  }

  reverse_iterator rfind(const T& value, const_reverse_iterator first) const {
    return this->rfind(value, first, _impl.end());
  }

  reverse_iterator rfind(const T& value,
                         const_reverse_iterator first,
                         const_reverse_iterator last) const {
    for(auto it = first; it != last; it++)
      if(*it == value)
        return it;
    return _impl.rend();
  }

  void clear() {
    return _impl.clear();
  }

  iterator insert(const_iterator pos, const T& value) {
    return _impl.insert(pos, value);
  }

  iterator insert(const_iterator pos, T&& value) {
    return _impl.insert(pos, value);
  }

  iterator insert(const_iterator pos, size_type count, const T& value) {
    return _impl.insert(pos, count, value);
  }

  template <class InputIt>
  iterator insert(const_iterator pos, InputIt first, InputIt last) {
    return _impl.insert(pos, first, last);
  }

  iterator insert(const_iterator pos, std::initializer_list<T> init) {
    return _impl.insert(pos, init);
  }

  template <typename... Args>
  iterator emplace(const_iterator pos, Args&&... args) {
    return _impl.emplace(args...);
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
        return this->erase(it);
    return _impl.end();
  }

  reverse_iterator rerase(const T& value) {
    return this->rerase(value, _impl.rbegin(), _impl.rend());
  }

  reverse_iterator rerase(const T& value, const_reverse_iterator begin) {
    return this->rerase(value, begin, _impl.end());
  }

  reverse_iterator rerase(const T& value,
                          const_reverse_iterator begin,
                          const_reverse_iterator end) {
    for(auto it = begin; it != end; it++)
      if(*it == value)
        return this->erase(it);
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

  void push_back(const T& value) {
    _impl.push_back(value);
  }

  void push_back(T&& value) {
    _impl.push_back(value);
  }

  void push_back(const List<T>& other) {
    for(const T& value : other.getImpl())
      _impl.push_back(value);
  }

  template <typename... Args>
  reference emplace_back(Args&&... args) {
    _impl.emplace_back(args...);
    return _impl.back();
  }

  value_type pop_back() {
    T ret = _impl.back();
    _impl.pop_back();
    return std::move(ret);
  }

  void push_front(const T& value) {
    _impl.push_front(value);
  }

  void push_front(T&& value) {
    _impl.push_front(value);
  }

  void push_front(const List<T>& other) {
    for(const T& value : other.getImpl())
      _impl.push_front(value);
  }

  template <typename... Args>
  reference emplace_front(Args&&... args) {
    _impl.emplace_front(args...);
    return _impl.front();
  }

  value_type pop_front() {
    T ret = _impl.front();
    _impl.pop_front();
    return std::move(ret);
  }

  void resize(size_type count) {
    _impl.resize(count);
  }

  void resize(size_type count, const T& value) {
    _impl.resize(count, value);
  }

  void swap(List<T>& other) {
    _impl.swap(other.getImpl());
  }

  bool replace(const reference val) {
    bool replaced = contains(val);
    std::replace(begin(), end(), val);
    return replaced;
  }

  void merge(List<T>&& other) {
    _impl.merge(other);
  }

  template <typename Compare>
  void merge(List<T>&& other, Compare comp) {
    _impl.merge(other, comp);
  }

  void splice(const_iterator pos, List<T>&& other) {
    _impl.splice(pos, other);
  }

  void splice(const_iterator pos, List<T>&& other, const_iterator it) {
    _impl.splice(pos, other, it);
  }

  void splice(const_iterator pos,
              List<T>&& other,
              const_iterator first,
              const_iterator last) {
    _impl.splice(pos, other, first, last);
  }

  size_type remove(const T& value) {
    _impl.remove(value);
    return _impl.size();
  }

  template <typename UnaryPredicate>
  size_type remove_if(UnaryPredicate p) {
    _impl.remove_if(p);
    return _impl.size();
  }

  void reverse() {
    _impl.reverse();
  }

  size_type unique() {
    _impl.unique();
    return _impl.size();
  }

  template <typename BinaryPredicate>
  size_type unique(BinaryPredicate b) {
    _impl.unique(b);
    return _impl.size();
  }

  void sort() {
    _impl.sort();
  }

  template <typename Compare>
  void sort(Compare comp) {
    _impl.sort(comp);
  }

  bool operator==(const List<T>& c2) const {
    return _impl == c2.getImpl();
  }

  bool operator!=(const List<T>& c2) const {
    return _impl != c2.getImpl();
  }

  bool operator<(const List<T>& c2) const {
    return _impl < c2.getImpl();
  }

  bool operator<=(const List<T>& c2) const {
    return _impl <= c2.getImpl();
  }

  bool operator>(const List<T>& c2) const {
    return _impl > c2.getImpl();
  }

  bool operator>=(const List<T>& c2) const {
    return _impl >= c2.getImpl();
  }
};

} // namespace cish

#endif // CISH_LIST_H
