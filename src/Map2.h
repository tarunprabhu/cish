#ifndef CISH_MAP2_H
#define CISH_MAP2_H

#include "Map.h"

/// This is a 2-way associative map that maps keys to values and values back
/// to keys. The complexity of looking up either is the same as that of a
/// Map.

namespace cish {

// FIXME: This is an incomplete implementation
template <typename LeftT,
          typename RightT,
          typename CompareLeft = std::less<LeftT>,
          typename CompareRight = std::less<RightT>,
          std::enable_if_t<!std::is_same<LeftT, RightT>::value, int> = 0>
class Map2 {
protected:
  Map<LeftT, RightT, CompareLeft> _ltr;
  Map<RightT, LeftT, CompareRight> _rtl;

public:
  using left_type = typename Map<LeftT, RightT>::key_type;
  using right_type = typename Map<RightT, LeftT>::key_type;
  using size_type = typename Map<LeftT, RightT>::size_type;
  using ltr_iterator = typename Map<LeftT, RightT>::iterator;
  using const_ltr_iterator = typename Map<LeftT, RightT>::const_iterator;
  using rtl_iterator = typename Map<RightT, LeftT>::iterator;
  using const_rtl_iterator = typename Map<RightT, LeftT>::const_iterator;

protected:
  template <typename IteratorT, typename KeyT>
  class key_iterator_t : public IteratorT {
  public:
    key_iterator_t() : IteratorT() {}
    key_iterator_t(IteratorT i) : IteratorT(i) {}
    KeyT& operator->() {
      return IteratorT::operator->()->first;
    }
    KeyT& operator*() {
      return IteratorT::operator*().first;
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
  using left_iterator = key_iterator_t<ltr_iterator, left_type>;
  using const_left_iterator = key_iterator_t<const_ltr_iterator, left_type>;
  using right_iterator = key_iterator_t<rtl_iterator, right_type>;
  using const_right_iterator = key_iterator_t<const_rtl_iterator, right_type>;

public:
  Map2() {
    ;
  }

  explicit Map2(const CompareLeft& cmpLeft, const CompareRight& cmpRight)
      : _ltr(cmpLeft), _rtl(cmpRight) {
    ;
  }

  Map2(const Map2<LeftT, RightT>& other) : _ltr(other._ltr), _rtl(other._rtl) {
    ;
  }

  Map2(Map2<LeftT, RightT>&& other) : _ltr(other._ltr), _rtl(other._rtl) {
    ;
  }

  Map2(std::initializer_list<std::pair<LeftT, RightT>> init) {
    for(const std::pair<LeftT, RightT>& p : init)
      insert(p.first, p.second);
  }

  Map2(std::initializer_list<std::pair<RightT, LeftT>> init) {
    for(const std::pair<RightT, LeftT>& p : init)
      insert(p.first, p.second);
  }

  // template <typename L = LeftT,
  //           typename R = RightT,
  //           std::enable_if_t<std::is_same<L, LeftT>::value
  //                                && std::is_same<R, RightT>::value
  //                                && std::is_same<L, R>::value,
  //                            int> = 0>
  // Map2(std::initializer_list<std::pair<RightT, LeftT>> init) {
  //   for(const std::pair<RightT, LeftT>& p : init)
  //     insert(p.first, p.second);
  // }

  Map2<LeftT, RightT>& operator=(const Map2<LeftT, RightT>& other) {
    this->_ltr = other._ltr;
    this->_rtl = other._rtl;
    return *this;
  }

  Map2<LeftT, RightT>& operator=(const Map2<RightT, LeftT>&& other) {
    this->_ltr = other._ltr;
    this->_rtl = other._rtl;
    return *this;
  }

  Map<LeftT, RightT>&
  operator=(std::initializer_list<std::pair<LeftT, RightT>> init) {
    for(const std::pair<LeftT, RightT>& p : init)
      insert(p.first, p.second);
    return *this;
  }

  Map<LeftT, RightT>&
  operator=(std::initializer_list<std::pair<RightT, LeftT>> init) {
    for(const std::pair<RightT, LeftT>& p : init)
      insert(p.first, p.second);
    return *this;
  }

  RightT& at(const LeftT& left) {
    return _ltr.at(left);
  }

  LeftT& at(const RightT& right) {
    return _rtl.at(right);
  }

  // template <typename L = LeftT,
  //           typename R = RightT,
  //           std::enable_if_t<std::is_same<L, LeftT>::value
  //                                && std::is_same<R, RightT>::value
  //                                && std::is_same<L, R>::value,
  //                            int> = 0>
  // R& at(const L& key) {
  //   if(contains(key))
  //     return _ltr.at(key);
  //   return _rtl.at(key);
  // }

  const RightT& at(const LeftT& left) const {
    return _ltr.at(left);
  }

  template <typename L = LeftT,
            typename R = RightT,
            std::enable_if_t<std::is_same<L, LeftT>::value
                                 && std::is_same<R, RightT>::value
                                 && !std::is_same<L, R>::value,
                             int> = 0>
  const L& at(const R& right) const {
    return _rtl.at(right);
  }

  template <typename L = LeftT,
            typename R = RightT,
            std::enable_if_t<std::is_same<L, LeftT>::value
                                 && std::is_same<R, RightT>::value
                                 && std::is_same<L, R>::value,
                             int> = 0>
  const R& at(const L& key) const {
    if(contains(key))
      return _ltr.at(key);
    return _rtl.at(key);
  }

  bool empty() const {
    return _ltr.empty();
  }

  size_type size() const {
    return _ltr.size();
  }

  void clear() {
    _ltr.clear();
    _rtl.clear();
  }

  bool insert(const LeftT& left, const RightT& right) {
    bool l = _ltr.emplace(left, right);
    bool r = _rtl.emplace(right, left);
    return l or r;
  }

  bool insert(const RightT& right, const LeftT& left) {
    bool l = _ltr.insert(std::make_pair(left, right));
    bool r = _rtl.insert(std::make_pair(right, left));
    return l or r;
  }

  bool insert(const Map2<LeftT, RightT>& other) {
    bool changed = false;

    for(const auto& lr : other.ltr())
      changed |= _ltr.insert(lr.first, lr.second).second;

    return changed;
  }

  bool insert(const Map2<RightT, LeftT>& other) {
    bool changed = false;

    for(const auto& lr : other.ltr())
      changed |= _ltr.insert(lr.first, lr.second).second;

    return changed;
  }

  left_iterator erase(const_left_iterator pos) {
    if(contains(*pos))
      _rtl.erase(*pos);
    return _ltr.erase(pos);
  }

  right_iterator erase(const_right_iterator pos) {
    if(contains(*pos))
      _ltr.erase(*pos);
    return _rtl.erase(pos);
  }

  // template <typename T = LeftT,
  //           std::enable_if_t<std::is_same<T, LeftT>::value
  //                                && std::is_same<LeftT, RightT>::value,
  //                            int> = 0>
  // size_type erase(const T& key) {
  //   static_assert(std::is_same<T, LeftT>::value
  //                     && std::is_same<LeftT, RightT>::value,
  //                 "Boom!");
  //   if(contains(key))
  //     _rtl.erase(key);
  //   return _ltr.erase(key);
  // }

  size_type erase(const LeftT& key) {
    if(contains(key))
      _rtl.erase(at(key));
    return _ltr.erase(key);
  }

  size_type erase(const RightT& key) {
    if(contains(key))
      _ltr.erase(at(key));
    return _rtl.erase(key);
  }

  bool contains(const LeftT& left) const {
    return _ltr.contains(left);
  }

  bool contains(const RightT& right) const {
    return _rtl.contains(right);
  }

  iterator_range<ltr_iterator> ltr() {
    return iterator_range<ltr_iterator>(ltr_iterator(_ltr.begin()),
                                        ltr_iterator(_ltr.end()));
  }

  iterator_range<const_ltr_iterator> ltr() const {
    return iterator_range<ltr_iterator>(const_ltr_iterator(_ltr.begin()),
                                        const_ltr_iterator(_ltr.end()));
  }

  iterator_range<left_iterator> left() {
    return iterator_range<left_iterator>(key_iterator(_ltr.begin()),
                                         key_iterator(_ltr.end()));
  }

  iterator_range<const_left_iterator> left() const {
    return iterator_range<const_left_iterator>(const_key_iterator(_ltr.begin()),
                                               const_key_iterator(_ltr.end()));
  }

  iterator_range<rtl_iterator> rtl() {
    return iterator_range<rtl_iterator>(rtl_iterator(_rtl.begin()),
                                        rtl_iterator(_rtl.end()));
  }

  iterator_range<const_rtl_iterator> rtl() const {
    return iterator_range<rtl_iterator>(const_rtl_iterator(_rtl.begin()),
                                        const_rtl_iterator(_rtl.end()));
  }

  iterator_range<right_iterator> right() {
    return iterator_range<right_iterator>(right_iterator(_rtl.begin()),
                                          right_iterator(_rtl.end()));
  }

  iterator_range<const_right_iterator> right() const {
    return iterator_range<const_right_iterator>(
        const_right_iterator(_rtl.begin()), const_right_iterator(_rtl.end()));
  }

  bool operator==(const Map2<LeftT, RightT>& other) const {
    return (_ltr == other._ltr);
  }

  bool operator!=(const Map2<LeftT, RightT>& other) const {
    return (_ltr != other._ltr);
  }

  bool operator<(const Map2<LeftT, RightT>& other) const {
    return (_ltr < other._ltr);
  }

  bool operator<=(const Map2<LeftT, RightT>& other) const {
    return (_ltr <= other._ltr);
  }

  bool operator>(const Map2<LeftT, RightT>& other) const {
    return (_ltr > other._ltr);
  }

  bool operator>=(const Map2<LeftT, RightT>& other) const {
    return (_ltr >= other._ltr);
  }
};

// FIXME: This implementation is incomplete
template <typename T, typename Compare = std::less<T>>
class Map2S {
protected:
  Map<T, T, Compare> _ltr;
  Map<T, T, Compare> _rtl;

public:
  using value_type = typename Map<T, T>::key_type;
  using size_type = typename Map<T, T>::size_type;
  using iterator = typename Map<T, T>::iterator;
  using const_iterator = typename Map<T, T>::const_iterator;

protected:
  template <typename IteratorT, typename KeyT>
  class key_iterator_t : public IteratorT {
  public:
    key_iterator_t() : IteratorT() {}
    key_iterator_t(IteratorT i) : IteratorT(i) {}
    KeyT& operator->() {
      return IteratorT::operator->()->first;
    }
    KeyT& operator*() {
      return IteratorT::operator*().first;
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
  Map2S() {
    ;
  }

  explicit Map2S(const Compare& cmp) : _ltr(cmp), _rtl(cmp) {
    ;
  }

  Map2S(const Map2S<T, T>& other) : _ltr(other._ltr), _rtl(other._rtl) {
    ;
  }

  Map2S(Map2S<T, T>&& other) : _ltr(other._ltr), _rtl(other._rtl) {
    ;
  }

  Map2S(std::initializer_list<std::pair<T, T>> init) {
    for(const std::pair<T, T>& p : init)
      insert(p.first, p.second);
  }

  Map2S<T, T>& operator=(const Map2S<T, T>& other) {
    this->_ltr = other._ltr;
    this->_rtl = other._rtl;
    return *this;
  }

  Map2S<T, T>& operator=(std::initializer_list<std::pair<T, T>> init) {
    for(const std::pair<T, T>& p : init)
      insert(p.first, p.second);
    return *this;
  }

  bool insert(const T& left, const T& right) {
    bool l = _ltr.insert(std::make_pair(left, right));
    bool r = _rtl.insert(std::make_pair(right, left));
    return l or r;
  }

  bool insert(const Map2S<T, T>& other) {
    bool changed = false;

    for(const auto& i : other)
      changed |= this->insert(i.first, i.second);

    return changed;
  }

  T& at(const T& v) {
    if(_ltr.contains(v))
      return _ltr.at(v);
    return _rtl.at(v);
  }

  const T& at(const T& v) const {
    if(_ltr.contains(v))
      return _ltr.at(v);
    return _rtl.at(v);
  }
};

} // namespace cish

#endif // CISH_MAP2_H
