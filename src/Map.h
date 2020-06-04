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

  virtual ~Map() = default;

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

  cish::Vector<K> keys() const {
    cish::Vector<K> ret;

    for(const auto& it : *this)
      ret.push_back(it.first);

    return std::move(ret);
  }

  cish::Vector<V> values() const {
    cish::Vector<V> ret;

    for(const auto& it : *this)
      ret.push_back(it.second);

    return ret;
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
