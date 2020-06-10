#ifndef CISH_FORMAT_OPTIONS_H
#define CISH_FORMAT_OPTIONS_H

#include <string>

namespace cish {

enum class StripCasts {
  Never = 0x0,
  Function = 0x1,
  Pointer = 0x2,
  Scalar = 0x4,
  Vector = 0x8,
  All = 0xffff,
};

enum class Annotations {
  None = 0x0,
  Source = 0x1,
  Cish = 0x2,
  All = 0xffff,
};

enum class IndentStyle {
  KR,         // K&R style
  Allman,     // Allman style
  Stroustrup, // Stroustrup style (like K&R but without the "cuddled else")
};

enum class Parens {
  Always, // Always add parentheses to operands of oeprators
  Smart,  // Be "smart" about adding parentheses to operands of operators
};

class FormatOptions {
private:
  unsigned ignoreCasts;
  unsigned annotations;

public:
  // The prefix string to use for generated variable names
  std::string prefix;
  IndentStyle indentation;
  Parens parens;
  unsigned offset;
  bool quiet;

public:
  FormatOptions();
  FormatOptions(const FormatOptions&) = delete;
  FormatOptions(FormatOptions&&) = delete;

  void set(StripCasts cst);
  void set(Annotations ann);
  bool has(StripCasts cst) const;
  bool has(Annotations ann) const;

public:
  static constexpr const char* defPrefix = "c__";
  static constexpr StripCasts defStripCasts = StripCasts::Never;
  static constexpr Annotations defAnnotations = Annotations::None;
  static constexpr IndentStyle defIndentStyle = IndentStyle::KR;
  static constexpr Parens defParens = Parens::Smart;
  static constexpr unsigned defOffset = 4;
  static constexpr bool defQuiet = false;
};

} // namespace cish

#endif // CISH_FORMAT_OPTIONS_H
