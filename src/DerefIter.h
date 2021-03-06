//  ---------------------------------------------------------------------------
//  Copyright (C) 2020 Tarun Prabhu <tarun.prabhu@acm.org>
//
//  This file is part of Cish.
//
//  Cish is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  Cish is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Cish.  If not, see <https://www.gnu.org/licenses/>.
//  ---------------------------------------------------------------------------

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
