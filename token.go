// Copyright Noel Cower 2015.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

package leks

import (
	"errors"
	"fmt"
	"strconv"
	"unicode/utf8"
)

var (
	ErrInvalidKind = errors.New("cannot convert token of this kind")
)

// TokenType is the integer identifier for specific types of tokens.
type TokenType int

const (
	tPunctStart, tPunctEnd   = TDot, TPlusArrow
	tNumStart, tNumEnd       = TIntLit, TBinLit
	tQuotedStart, tQuotedEnd = TRawStringLit, TStringLit
	tKWStart, tKWEnd         = TTrue, TNil
)

// TError is the token type for any failed attempt to parse a token. The token's Err field should be set when this is its type.
const TError = TokenType(-1)
const (
	TEOF = 1 + TokenType(iota)

	TTrue  // true
	TFalse // false
	TNil   // nil

	TDot       // .
	TDoubleDot // ..
	TTripleDot // ...

	THash         // #
	TAt           // @ (word initial)
	TDollar       // $ (word initial)
	TParenOpen    // (
	TParenClose   // )
	TBracketOpen  // [
	TBracketClose // ]
	TCurlOpen     // {
	TCurlClose    // }
	TBackslash    // \
	TComma        // ,
	TSemicolon    // ;
	TNewline      // \n
	TQuote        // '

	// Repeats / combinations

	TBang     // !
	TNotEqual // !=

	TSlash       // /
	TSlashEquals // /=

	TPercent       // %
	TPercentEquals // %=

	TCaret       // ^
	TCaretEquals // ^=

	TTilde       // ~ (word initial)
	TTildeEquals // ~=

	TGreaterThan  // >
	TShiftRight   // >>
	TGreaterEqual // >=

	TLessThan    // <
	TShiftLeft   // <<
	TLesserEqual // <=
	TInequality  // <>

	TQuestion       // ?
	TDoubleQuestion // ??
	TQuestionEquals // ?=

	TEquals   // =
	TEquality // ==
	TShovel   // =>

	TPipe       // |
	TPipeEquals // |=
	TOr         // ||
	TOrEquals   // ||=

	TAmp       // &
	TAmpEquals // &=
	TAnd       // &&
	TAndEquals // &&=

	TMul       // *
	TDoubleMul // **
	TMulAssign // *=

	// Single, Repeat, Assignment, Arrow

	TColon       // :
	TDoubleColon // ::
	TColonEqual  // :=
	TColonArrow  // :>

	TMinus       // -
	TDoubleMinus // --
	TMinusEquals // -=
	TArrow       // ->

	TPlus       // +
	TDoublePlus // ++
	TPlusEquals // +=
	TPlusArrow  // +>

	TIdent // any identifier

	TIntLit       // 5
	TIntExpLit    // 5e+8
	TFloatLit     // 0.5
	TFloatExpLit  // 0.5e+8
	TOctLit       // 0655
	THexLit       // 0x1234abcd 0X1234ABCD
	TBinLit       // 0b1101 0B1101
	TRawStringLit // `raw string`
	TRuneLit      // 'c'
	TStringLit    // "string"

	TLineComment  // // comment
	TBlockComment // /* comment */
)

var tokenTypeNames = map[TokenType]string{
	TError:          "Error",
	TEOF:            "EOF",
	TTrue:           "True",
	TFalse:          "False",
	TNil:            "Nil",
	TDot:            "Dot",
	TDoubleDot:      "DoubleDot",
	TTripleDot:      "TripleDot",
	THash:           "Hash",
	TAt:             "At",
	TDollar:         "Dollar",
	TParenOpen:      "ParenOpen",
	TParenClose:     "ParenClose",
	TBracketOpen:    "BracketOpen",
	TBracketClose:   "BracketClose",
	TCurlOpen:       "CurlOpen",
	TCurlClose:      "CurlClose",
	TBackslash:      "Backslash",
	TComma:          "Comma",
	TSemicolon:      "Semicolon",
	TQuote:          "Quote",
	TNewline:        "Newline",
	TBang:           "Bang",
	TNotEqual:       "NotEqual",
	TSlash:          "Slash",
	TSlashEquals:    "SlashEquals",
	TPercent:        "Percent",
	TPercentEquals:  "PercentEquals",
	TCaret:          "Caret",
	TCaretEquals:    "CaretEquals",
	TTilde:          "Tilde",
	TTildeEquals:    "TildeEquals",
	TGreaterThan:    "GreaterThan",
	TShiftRight:     "ShiftRight",
	TGreaterEqual:   "GreaterEqual",
	TLessThan:       "LessThan",
	TShiftLeft:      "ShiftLeft",
	TLesserEqual:    "LesserEqual",
	TInequality:     "TInequality",
	TQuestion:       "Question",
	TDoubleQuestion: "DoubleQuestion",
	TQuestionEquals: "QuestionEquals",
	TEquals:         "Equals",
	TEquality:       "Equality",
	TShovel:         "Shovel",
	TPipe:           "Pipe",
	TPipeEquals:     "PipeEquals",
	TOr:             "Or",
	TOrEquals:       "OrEquals",
	TAmp:            "Amp",
	TAmpEquals:      "AmpEquals",
	TAnd:            "And",
	TAndEquals:      "AndEquals",
	TMul:            "Mul",
	TDoubleMul:      "DoubleMul",
	TMulAssign:      "MulAssign",
	TColon:          "Colon",
	TDoubleColon:    "DoubleColon",
	TColonEqual:     "ColonEqual",
	TColonArrow:     "ColonArrow",
	TMinus:          "Minus",
	TDoubleMinus:    "DoubleMinus",
	TMinusEquals:    "MinusEquals",
	TArrow:          "Arrow",
	TPlus:           "Plus",
	TDoublePlus:     "DoublePlus",
	TPlusEquals:     "PlusEquals",
	TPlusArrow:      "PlusArrow",
	TIdent:          "Ident",
	TIntLit:         "IntegerLit",
	TIntExpLit:      "IntegerExpLit",
	TFloatLit:       "FloatLit",
	TFloatExpLit:    "FloatExpLit",
	TOctLit:         "OctLit",
	THexLit:         "HexLit",
	TBinLit:         "BinLit",
	TRawStringLit:   "RawStringLit",
	TRuneLit:        "RuneLit",
	TStringLit:      "StringLit",
	TLineComment:    "LineComment",
	TBlockComment:   "BlockComment",
}

func (t TokenType) String() string {
	if s, ok := tokenTypeNames[t]; ok {
		return s
	}
	return "Unknown(" + strconv.FormatInt(int64(t), 16) + ")"
}

func (t TokenType) GoString() string {
	return "TokenType(" + t.String() + ")"
}

type Token struct {
	Start, End Location
	Kind       TokenType
	Text       []byte
	Err        error // Only likely to be set if Kind == TError
}

// IsComment returns whether the token is a comment token (i.e., you might want to discard it).
func (t Token) IsComment() bool {
	return t.Kind == TBlockComment || t.Kind == TLineComment
}

// IsPunct returns whether the token is a punctuation token (newlines, parentheses, brackets, symbols, etc.).
func (t Token) IsPunct() bool {
	return tPunctStart <= t.Kind && t.Kind <= tPunctEnd
}

// IsNumeric returns whether the token is numeric. Check its Kind field to determine the specific type of numeric.
func (t Token) IsNumeric() bool {
	return tNumStart <= t.Kind && t.Kind <= tNumEnd
}

// IsQuoted returns whether the token is a quoted string or rune. If true, the rune's Text field does not include the quotation marks.
func (t Token) IsQuoted() bool {
	return tQuotedStart <= t.Kind && t.Kind <= tQuotedEnd
}

// IsKeyword returns whether the token is a keyword.
func (t Token) IsKeyword() bool {
	return tKWStart <= t.Kind && t.Kind <= tKWEnd
}

// IsID returns whether the token is an identifier. This is shorthand for t.Kind == TIdent.
func (t Token) IsID() bool {
	return t.Kind == TIdent
}

// IsIdent returns whether the token is an error. This is shorthand for t.Kind == TError.
func (t Token) IsError() bool {
	return t.Kind == TError
}

func (t Token) Before(o Token) bool {
	return t.End.Off <= o.Start.Off
}

func (t Token) After(o Token) bool {
	return t.Start.Off >= o.End.Off
}

func (t Token) Near(o Token, byLines, byColumns int) bool {
	if o.Before(t) {
		t, o = o, t
	}

	l := o.Start.Sub(t.End)
	return l.Line <= byLines && l.Col <= byColumns
}

func (t Token) String() string {
	if t.Kind == TError {
		return fmt.Sprintf("%T(%s)", t.Err, t.Err)
	} else {
		return string(t.Text)
	}
}

func (t Token) Int() (int64, error) {
	switch t.Kind {
	case TIntLit, TIntExpLit:
		return strconv.ParseInt(string(t.Text), 10, 64)
	case TOctLit:
		return strconv.ParseInt(string(t.Text[1:]), 8, 64)
	case THexLit:
		return strconv.ParseInt(string(t.Text[2:]), 16, 64)
	case TBinLit:
		return strconv.ParseInt(string(t.Text[2:]), 2, 64)
	}
	return 0, ErrInvalidKind
}

func (t Token) Uint() (uint64, error) {
	switch t.Kind {
	case TIntLit, TIntExpLit, TOctLit, THexLit, TBinLit:
		return strconv.ParseUint(string(t.Text), 0, 64)
	}
	return 0, ErrInvalidKind
}

func (t Token) Rune() (r rune, size int) {
	if t.Kind != TRuneLit {
		return utf8.RuneError, 1
	}
	return utf8.DecodeRune(t.Text)
}

func (t Token) Bytes() []byte {
	return t.Text
}

func (t Token) GoString() string {
	if t.Err != nil {
		return fmt.Sprintf("%v(%v->%v Text:%q Err:%v)", t.Kind, t.Start, t.End, t.Text, t.Err)
	} else {
		return fmt.Sprintf("%v(%v->%v %q)", t.Kind, t.Start, t.End, t.Text)
	}
}

// Location is a location in a token's source unit.
type Location struct {
	Off  int // Offset in bytes from the unit's start.
	Line int // Line number (starting at 1).
	Col  int // Column number (starting at 1).
}

func (l Location) Sub(r Location) Location {
	return Location{
		Off:  l.Off - r.Off,
		Line: l.Line - r.Line,
		Col:  l.Col - r.Col,
	}
}

func (l Location) String() string {
	return fmt.Sprintf("[%d:%d:%d]", l.Line, l.Col, l.Off)
}
