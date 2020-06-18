#ifndef CISH_MAP_H
#define CISH_MAP_H

#include "Vector.h"
#include <map>

namespace cish {

template <typename K, typename V, typename Compare = std::less<K>>
class Map {
protected:
  std::map<K, V, Compare> _impl;

public:
  using key_type = typename std::map<K, V>::key_type;
  using mapped_type = typename std::map<K, V>::mapped_type;
  using value_type = typename std::map<K, V>::value_type;
  using size_type = typename std::map<K, V>::size_type;
  using iterator = typename std::map<K, V>::iterator;
  using const_iterator = typename std::map<K, V>::const_iterator;

protected:
  template <typename IteratorT>
  class key_iterator_t : public IteratorT {
  public:
    using value_type = typename IteratorT::value_type::first_type;

  public:
    key_iterator_t() : IteratorT() {}
    key_iterator_t(IteratorT i) : IteratorT(i) {}
    auto& operator-> () {
      return IteratorT::operator->()->first;
    }
    auto& operator*() {
      return IteratorT::operator*().first;
    }
  };

  template <typename IteratorT>
  class mapped_iterator_t : public IteratorT {
  public:
    using value_type = typename IteratorT::value_type::second_type;

  public:
    mapped_iterator_t() : IteratorT() {}
    mapped_iterator_t(IteratorT i) : IteratorT(i) {}
    auto& operator-> () {
      return IteratorT::operator->()->second;
    }
    auto& operator*() {
      return IteratorT::operator*().second;
    }
  };

public:
  template <typename IteratorT>
  class iterator_range {
  protected:
    IteratorT b, e;

  public:
    iterator_range(IteratorT begin, IteratorT end)
        : b(std::move(begin)), e(std::move(end)) {}

    IteratorT begin() const {
      return b;
    }
    IteratorT end() const {
      return e;
    }
  };

public:
  using key_iterator = key_iterator_t<iterator>;
  using const_key_iterator = key_iterator_t<const_iterator>;
  using mapped_iterator = mapped_iterator_t<iterator>;
  using const_mapped_iterator = mapped_iterator_t<const_iterator>;

protected:
  std::map<K, V>& getImpl() {
    return _impl;
  }

  const std::map<K, V>& getImpl() const {
    return _impl;
  }

public:
  Map() {
    ;
  }

  explicit Map(const Compare& cmp) : _impl(cmp) {
    ;
  }

  template <typename InputIt>
  Map(InputIt first, InputIt last) : _impl(first, last) {
    ;
  }

  Map(const Map<K, V>& other) : _impl(other.getImpl()) {
    ;
  }

  Map(Map<K, V>&& other) : _impl(other.getImpl()) {
    ;
  }

  Map(std::initializer_list<value_type> init) : _impl(init) {
    ;
  }

  Map<K, V>& operator=(const Map<K, V>& other) {
    _impl = other.getImpl();
    return *this;
  }

  Map<K, V>& operator=(Map<K, V>&& other) {
    _impl = other.getImpl();
    return *this;
  }

  Map<K, V>& operator=(std::initializer_list<value_type> init) {
    _impl = init;
    return *this;
  }

  V& at(const K& key) {
    return _impl.at(key);
  }

  const V& at(const K& key) const {
    return _impl.at(key);
  }

  V& operator[](const K& key) {
    return _impl[key];
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
    _impl.clear();
  }

  template <typename... Args>
  bool emplace(Args&&... args) {
    return _impl.emplace(args...).second;
  }

  bool insert(const value_type& value) {
    return _impl.insert(value).second;
  }

  template <typename InputIt>
  void insert(InputIt first, InputIt last) {
    _impl.insert(first, last);
  }

  bool insert(const Map<K, V>& other) {
    bool changed = false;

    for(const auto& kv : other)
      changed |= _impl.insert(kv).second;

    return changed;
  }

  iterator erase(const_iterator pos) {
    return _impl.erase(pos);
  }

  iterator erase(const_iterator first, const_iterator last) {
    return _impl.erase(first, last);
  }

  size_type erase(const K& key) {
    return _impl.erase(key);
  }

  bool contains(const K& k) const {
    return _impl.find(k) != _impl.end();
  }

  iterator_range<key_iterator> keys() {
    return iterator_range<key_iterator>(key_iterator(begin()),
                                        key_iterator(end()));
  }

  iterator_range<const_key_iterator> keys() const {
    return iterator_range<const_key_iterator>(const_key_iterator(begin()),
                                              const_key_iterator(end()));
  }

  iterator_range<mapped_iterator> values() {
    return iterator_range<mapped_iterator>(mapped_iterator(begin()),
                                           mapped_iterator(end()));
  }

  iterator_range<const_mapped_iterator> values() const {
    return iterator_range<const_mapped_iterator>(const_mapped_iterator(begin()),
                                                 const_mapped_iterator(end()));
  }

  bool operator==(const Map<K, V>& c2) const {
    return _impl == c2.getImpl();
  }

  bool operator!=(const Map<K, V>& c2) const {
    return _impl != c2.getImpl();
  }

  bool operator<(const Map<K, V>& c2) const {
    return _impl < c2.getImpl();
  }

  bool operator<=(const Map<K, V>& c2) const {
    return _impl <= c2.getImpl();
  }

  bool operator>(const Map<K, V>& c2) const {
    return _impl > c2.getImpl();
  }

  bool operator>=(const Map<K, V>& c2) const {
    return _impl >= c2.getImpl();
  }
};

} // namespace cish

#endif // CISH_MAP_H
