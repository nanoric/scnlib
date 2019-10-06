// Copyright 2017-2019 Elias Kosunen
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// This file is a part of scnlib:
//     https://github.com/eliaskosunen/scnlib

#if defined(SCN_HEADER_ONLY) && SCN_HEADER_ONLY
#define SCN_READER_CPP
#endif

#include <scn/detail/reader.h>

#include <algorithm>
#include <cerrno>

namespace scn {
    SCN_BEGIN_NAMESPACE

    namespace detail {
        // Based on https://github.com/google/double-conversion
        // Copyright 2006-2012 the V8 project authors
        // BSD 3-clause license

        template <typename T, typename CharT>
        T do_float_conversion(span<const CharT> s, int exponent)
        {
            return T{0.0};
        }

        // Max number of significant digits in decimal representation
        static constexpr int max_significant_digits = 772;

        template <typename CharT>
        basic_string_view<CharT> nan_string();
        template <>
        SCN_FUNC string_view nan_string<char>()
        {
            return {"nan"};
        }
        template <>
        SCN_FUNC wstring_view nan_string<wchar_t>()
        {
            return {L"nan"};
        }

        template <typename CharT>
        basic_string_view<CharT> inf_string();
        template <>
        SCN_FUNC string_view inf_string<char>()
        {
            return {"inf"};
        }
        template <>
        SCN_FUNC wstring_view inf_string<wchar_t>()
        {
            return {L"inf"};
        }

        template <typename CharT>
        basic_string_view<CharT> infinity_string();
        template <>
        SCN_FUNC string_view infinity_string<char>()
        {
            return {"infinity"};
        }
        template <>
        SCN_FUNC wstring_view infinity_string<wchar_t>()
        {
            return {L"infinity"};
        }

        template <typename T, typename CharT>
        expected<T> string_to_ieee(span<const CharT> in, size_t& chars)
        {
            SCN_EXPECT(in.size() != 0);
            chars = 0;
            using lim = std::numeric_limits<T>;
            auto it = in.begin();

            bool sign = false;
            if (*it == ascii_widen<CharT>('+') ||
                *it == ascii_widen<CharT>('-')) {
                sign = (*it == ascii_widen<CharT>('+'));
                ++it;
                if (it == in.end()) {
                    return error(
                        error::invalid_scanned_value,
                        "A sign (+ or -) is not a valid floating point number");
                }
            }

            auto case_insensitive_cmp = [](CharT l, CharT r) {
                if (l >= ascii_widen<CharT>('A') &&
                    l <= ascii_widen<CharT>('Z')) {
                    l += 32;
                }
                if (r >= ascii_widen<CharT>('A') &&
                    r <= ascii_widen<CharT>('Z')) {
                    r += 32;
                }
                return l == r;
            };

            // NaN?
            if (*it == ascii_widen<CharT>('n') ||
                *it == ascii_widen<CharT>('N')) {
                auto str = nan_string<CharT>();
                if (in.end() - it != str.size() ||
                    !std::equal(it, in.end(), str.begin(),
                                case_insensitive_cmp)) {
                    return error(
                        error::invalid_scanned_value,
                        "Invalid parsed NaN in a floating point number");
                }
                return sign ? -lim::quiet_NaN() : lim::quiet_NaN();
            }

            // Infinity?
            if (*it == ascii_widen<CharT>('i') ||
                *it == ascii_widen<CharT>('I')) {
                auto inf = inf_string<CharT>();
                if (in.end() - it != inf.size() ||
                    !std::equal(it, in.end(), inf.begin(),
                                case_insensitive_cmp)) {
                    auto infinity = infinity_string<CharT>();
                    if (in.end() - it != infinity.size() ||
                        !std::equal(it, in.end(), infinity.begin(),
                                    case_insensitive_cmp)) {
                        return error(
                            error::invalid_scanned_value,
                            "Invalid parsed NaN in a floating point number");
                    }
                }
                return sign ? -lim::infinity() : lim::infinity();
            }

            bool leading_zero = false;
            if (*it == ascii_widen<CharT>('0')) {
                ++it;
                if (it == in.end()) {
                    // just a (signed) zero
                    return sign ? T{-0.0} : T{0.0};
                }
                leading_zero = true;

                // hex float?
                if (*it == ascii_widen<CharT>('x') ||
                    *it == ascii_widen<CharT>('X')) {
                    ++it;
                    if (it == in.end()) {
                        return error(error::invalid_scanned_value,
                                     "Unexpected end of hex float");
                    }
                }
                // TODO
            }

            array<CharT, max_significant_digits + 10> buffer{};
            auto buffer_it = buffer.begin();

            int significant_digits{0}, insignificant_digits{0}, exponent{0};
            bool nonzero_digit_dropped{false};

            while (*it >= ascii_widen<CharT>('0') &&
                   *it <= ascii_widen<CharT>('9')) {
                if (significant_digits < max_significant_digits) {
                    *buffer_it = *it;
                    ++buffer_it;
                    ++significant_digits;
                }
                else {
                    ++insignificant_digits;
                    nonzero_digit_dropped =
                        nonzero_digit_dropped || *it != ascii_widen<CharT>('0');
                }
                ++it;
                if (it == in.end()) {
                    goto parsing_done;
                }
            }

            SCN_ENSURE(it != in.end());
            if (*it == ascii_widen<CharT>('.')) {
                ++it;
                if (it == in.end()) {
                    if (significant_digits == 0 && !leading_zero) {
                        return error(error::invalid_scanned_value,
                                     "Invalid floating point value");
                    }
                    goto parsing_done;
                }

                if (significant_digits == 0) {
                    while (*it == ascii_widen<CharT>('0')) {
                        ++it;
                        if (it == in.end()) {
                            chars = in.size();
                            return sign ? T{-0.0} : T{0.0};
                        }
                        --exponent;
                    }
                }

                while (*it >= ascii_widen<CharT>('0') &&
                       *it <= ascii_widen<CharT>('9')) {
                    if (significant_digits < max_significant_digits) {
                        *buffer_it = *it;
                        ++buffer_it;
                        ++significant_digits;
                        --exponent;
                    }
                    else {
                        nonzero_digit_dropped = nonzero_digit_dropped ||
                                                *it != ascii_widen<CharT>('0');
                    }
                    ++it;
                    if (it == in.end()) {
                        goto parsing_done;
                    }
                }
            }

            if (!leading_zero && exponent == 0 && significant_digits == 0) {
                return error(error::invalid_scanned_value,
                             "Invalid floating point value");
            }

            if (*it == ascii_widen<CharT>('e') ||
                *it == ascii_widen<CharT>('E')) {
                auto junk_begin = it;
                ++it;
                if (it == in.end()) {
                    it = junk_begin;
                    goto parsing_done;
                }

                auto esign = ascii_widen<CharT>('+');
                if (*it == ascii_widen<CharT>('+') ||
                    *it == ascii_widen<CharT>('-')) {
                    esign = *it;
                    ++it;
                    if (it == in.end()) {
                        it = junk_begin;
                        goto parsing_done;
                    }
                }

                if (it == in.end() || *it < ascii_widen<CharT>('0') ||
                    *it > ascii_widen<CharT>('9')) {
                    it = junk_begin;
                    goto parsing_done;
                }

                constexpr int max_exponent =
                    std::numeric_limits<int>::max() / 2;
                SCN_EXPECT(-max_exponent / 2 <= exponent &&
                           exponent <= max_exponent / 2);
                int num = 0;
                do {
                    int digit = *it - ascii_widen<CharT>('0');
                    if (num >= max_exponent / 10 &&
                        !(num == max_exponent / 10 &&
                          digit <= max_exponent % 10)) {
                        num = max_exponent;
                    }
                    else {
                        num = num * 10 + digit;
                    }
                    ++it;
                } while (it != in.end() && *it >= ascii_widen<CharT>('0') &&
                         *it <= ascii_widen<CharT>('9'));

                exponent += (esign == ascii_widen<CharT>('-') ? -num : num);
            }

        parsing_done:
            exponent += insignificant_digits;

            if (nonzero_digit_dropped) {
                *buffer_it = ascii_widen<CharT>('1');
                ++buffer_it;
                --exponent;
            }

            SCN_ENSURE(buffer_it != buffer.end());
            T converted = do_float_conversion<T>(
                make_span(buffer.begin(), buffer_it).as_const(), exponent);
            chars = it - in.begin();
            return sign ? -converted : converted;
        }

        template <typename CharT, typename T>
        struct read_float_impl {
            static expected<T> get(span<const CharT> s, size_t& chars)
            {
                return string_to_ieee<T>(s, chars);
            }  // namespace detail
        };     // namespace scn

        template <>
        struct read_float_impl<char, long double> {
            static expected<long double> get(span<const char> s, size_t& chars)
            {
                std::string str{s.data(), s.size()};
                errno = 0;

                char* end{};
                long double ld = std::strtold(str.data(), &end);
                chars = static_cast<size_t>(end - str.data());
                if (errno == ERANGE) {
                    errno = 0;
                    return error(error::value_out_of_range,
                                 "strtold range error");
                }
                SCN_GCC_PUSH
                // bogus warning, == with 0.0 is safe
                SCN_GCC_IGNORE("-Wfloat-equal")
                if (ld == 0.0l && end == str) {
                    return error(error::invalid_scanned_value, "strtold");
                }
                SCN_GCC_POP
                return ld;
            }
        };
        template <>
        struct read_float_impl<wchar_t, long double> {
            static expected<long double> get(span<const wchar_t> s,
                                             size_t& chars)
            {
                std::wstring str{s.data(), s.size()};
                errno = 0;

                wchar_t* end{};
                long double ld = std::wcstold(str.data(), &end);
                chars = static_cast<size_t>(end - str.data());
                if (errno == ERANGE) {
                    errno = 0;
                    return error(error::value_out_of_range,
                                 "wcstold range error");
                }
                SCN_GCC_PUSH
                // bogus warning, == with 0.0 is safe
                SCN_GCC_IGNORE("-Wfloat-equal")
                if (ld == 0.0l && end == str) {
                    return error(error::invalid_scanned_value, "wcstold");
                }
                SCN_GCC_POP
                return ld;
            }
        };

        template <typename T>
        template <typename CharT>
        expected<T> float_scanner<T>::_read_float_impl(span<const CharT> s,
                                                       size_t& chars)
        {
            return read_float_impl<CharT, T>::get(s, chars);
        }

        template expected<float> float_scanner<float>::_read_float_impl(
            span<const char>,
            size_t&);
        template expected<double> float_scanner<double>::_read_float_impl(
            span<const char>,
            size_t&);
        template expected<long double>
        float_scanner<long double>::_read_float_impl(span<const char>, size_t&);
        template expected<float> float_scanner<float>::_read_float_impl(
            span<const wchar_t>,
            size_t&);
        template expected<double> float_scanner<double>::_read_float_impl(
            span<const wchar_t>,
            size_t&);
        template expected<long double>
        float_scanner<long double>::_read_float_impl(span<const wchar_t>,
                                                     size_t&);
    }  // namespace detail

    SCN_END_NAMESPACE
}  // namespace scn
