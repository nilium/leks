// Copyright Noel Cower 2015.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

package leks

import (
	"io"
	"sort"
	"unicode"
	"unicode/utf8"
)

// Writing runes to writers (typically only a bytes.Buffer)

type runeWriter interface {
	WriteRune(rune) (int, error)
}

func writeRune(w io.Writer, r rune) (int, error) {
	if w, ok := w.(runeWriter); ok {
		return w.WriteRune(r)
	}

	var encoded [4]byte
	subset := encoded[:utf8.EncodeRune(encoded[:], r)]
	if len(subset) == 0 {
		return 0, nil
	}

	return w.Write(subset)
}

// Rune sorting

type runeSort []rune

func (r runeSort) Len() int {
	return len(r)
}

func (r runeSort) Swap(i, j int) {
	r[i], r[j] = r[j], r[i]
}

func (r runeSort) Less(i, j int) bool {
	return r[i] < r[j]
}

// Rune tests (i.e., matching runes for bits of the lexer)

func runeNeverMatches(rune) bool { return false }
func isWhitespace(r rune) bool   { return r != '\n' && unicode.IsSpace(r) }
func isNewline(r rune) bool      { return r == '\n' }
func isPeriod(r rune) bool       { return r == '.' }
func isExp(r rune) bool          { return r == 'e' || r == 'E' }
func isSign(r rune) bool         { return r == '+' || r == '-' }
func isBinSep(r rune) bool       { return r == 'b' || r == 'B' }
func isHexSep(r rune) bool       { return r == 'x' || r == 'X' }

// Only permit ASCII subset of numbers because I don't want to try to read anything else.
var isDec = runeInRange('0', '9')
var isAlpha = unicode.IsLetter
var isAlnum = runeMatchesSome(
	unicode.IsLetter,
	isDec,
)
var isWordInitial = runeMatchesSome(
	isAlpha,
	runeInSeq([]rune("~_@$")),
)
var isWord = runeMatchesSome(
	isAlnum,
	runeInSeq([]rune("~_@$-<>=")),
)
var isWordSentinel = runeInSeq([]rune("?!"))
var isBin = runeInRange('0', '1')
var isOct = runeInRange('0', '7')
var isHex = runeMatchesSome(
	isDec,
	runeInRange('a', 'f'),
	runeInRange('A', 'F'),
)

var isSingleSym = runeInSeq([]rune("\n#@$()[]{}\\,;'"))
var isDoubleSym = runeInSeq([]rune("!&*+-:<=>?|/%^~"))

var symMap = map[rune]TokenType{
	'\'': TQuote,
	'.':  TDot,
	'#':  THash,
	'%':  TPercent,
	'(':  TParenOpen,
	')':  TParenClose,
	'[':  TBracketOpen,
	']':  TBracketClose,
	'{':  TCurlOpen,
	'}':  TCurlClose,
	'^':  TCaret,
	'\\': TBackslash,
	'/':  TSlash,
	',':  TComma,
	';':  TSemicolon,
	'?':  TQuestion,
	'!':  TBang,
	'>':  TGreaterThan,
	'<':  TLessThan,
	'=':  TEquals,
	'|':  TPipe,
	'&':  TAmp,
	':':  TColon,
	'-':  TMinus,
	'+':  TPlus,
	'*':  TMul,
	'\n': TNewline,
}

func runeMatchesSome(fns ...func(rune) bool) func(rune) bool {
	if len(fns) == 0 {
		return runeNeverMatches
	}

	fns = append([]func(rune) bool(nil), fns...)
	return func(r rune) bool {
		for _, fn := range fns {
			if fn(r) {
				return true
			}
		}
		return false
	}
}

func runesMatchSeq(seq string) func(rune) bool {
	if len(seq) == 0 {
		return runeNeverMatches
	}

	runes := []rune(seq)
	if len(runes) == 1 {
		match := runes[0]
		return func(r rune) bool { return r == match }
	}

	window := make([]rune, len(runes))
	wh := 0

	return func(r rune) bool {
		// Move the window ahead
		copy(window, window[1:])

		window[len(window)-1] = r
		if wh != len(window) {
			wh++
			return false
		}

		for i := range window {
			if window[i] != runes[i] {
				return false
			}
		}

		return true
	}
}

// runeInRange returns a rune test that tests whether the input rune is in the range of min and max, inclusive.
func runeInRange(min, max rune) func(rune) bool {
	if max < min {
		min, max = max, min
	}

	return func(r rune) bool {
		return r >= min && r <= max
	}
}

func runeInSeq(runes []rune) func(rune) bool {
	if len(runes) == 0 {
		return runeNeverMatches
	}

	runes = append([]rune(nil), runes...)
	sort.Sort(runeSort(runes))

	rangeTest := runeInRange(runes[0], runes[len(runes)-1])
	return func(r rune) bool {
		if !rangeTest(r) {
			return false
		}
		n := len(runes)
		i := sort.Search(n, func(i int) bool { return runes[i] >= r })
		return n != i && runes[i] == r
	}
}

func escapeRuneTest(escaped bool, test func(rune) bool) func(rune) bool {
	isEscaped := false
	return func(r rune) bool {
		if isEscaped {
			isEscaped = false
			return escaped
		}

		if r == '\\' {
			isEscaped = true
			return escaped
		}

		return test(r)
	}
}

func negateRuneTest(test func(rune) bool) func(rune) bool {
	return func(r rune) bool {
		return !test(r)
	}
}
