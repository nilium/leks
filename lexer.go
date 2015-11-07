// Copyright Noel Cower 2015.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

package leks

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"log"
	"math/big"
	"strconv"
	"unicode/utf8"
)

var (
	errBlockCommentEOF = errors.New("Lexer: readBlockComment: expected closing */ but encountered EOF")
	errNoComment       = errors.New("Lexer: readComment: not a comment")
	errInvalidUnread   = errors.New("Lexer: unreadRune: last operation was not readRune")
	errMalformedHex    = errors.New("Lexer: readNumber: malformed hex number")
	errMalformedBin    = errors.New("Lexer: readNumber: malformed binary number")
)

type Lexer struct {
	last    rune
	lastSz  int
	lastErr error
	lastLoc Location // Used to unread runes and maintain positions

	loc  Location
	mark Location
	in   io.RuneScanner

	writeBufferCount int
	writeBuffer      uint64
	buf              bytes.Buffer
}

// New allocates a new Lexer that reads its input from the given io.RuneScanner, r. If r is nil, New panics.
func New(r io.RuneScanner) *Lexer {
	if r == nil {
		panic("New: r (RuneScanner) is nil")
	}

	return &Lexer{
		loc:    Location{Line: 1, Col: 1},
		lastSz: -1,
		in:     r,
	}
}

// ReadToken reads the next token from the Lexer's rune stream. If an error occurs, it is returned. If a parsing error occurs, it will
// typically be returned as a token of kind TError with its Err field set. Subsequent reads will attempt to resume from wherever the last
// token stopped. If the last token was an error, the next token may be in the middle of the previous erroneous token.
func (l *Lexer) ReadToken() (Token, error) {
	l.pushReadCapture(true)
	defer l.popReadCapture()
	for {
		var err error
		var r rune
		if err = l.skipWS(); err == nil {
			l.mark = l.loc
			r, _, err = l.readRune()
		}

		if err == io.EOF {
			return l.token(TEOF, false, nil), nil
		} else if err != nil {
			return l.token(TError, false, err), err
		}

		var tok Token
		switch {
		case r == '`' || r == '"' || r == '\'':
			tok, err = l.readString()

		case isWordInitial(r):
			tok, err = l.readWord()

			if n := len(tok.Text); tok.Kind == TIdent && n == 1 {
				switch tok.Text[0] {
				case '@':
					tok.Kind = TAt
				case '$':
					tok.Kind = TDollar
				case '~':
					tok.Kind = TTilde
					if l.readSingleRune('=') {
						tok.Kind = TTildeEquals
					}
				}
			}

		case isDec(r):
			tok, err = l.readNumber()

		case r == '.':
			if r, _ := l.peekRune(); isDec(r) {
				tok, err = l.readNumber()
				break
			}

			kind := TDot
			for kind < TTripleDot && l.readOneOf(isPeriod) {
				kind++
			}
			tok = l.token(kind, true, nil)

		case isSingleSym(r):
			tok = l.token(symMap[r], true, nil)

		case r == '/':
			if next, err := l.peekRune(); err == nil && (next == '/' || next == '*') {
				if tok, err = l.readComment(); err != nil {
					return l.token(TError, false, err), err
				}
				break
			}
			fallthrough

		case isDoubleSym(r):
			kind := symMap[r]

			switch r {
			case '!', '/', '%', '^', '~': // suffix: =
				if l.readSingleRune('=') {
					kind += 1
				}

			case '&', '|': // repeat plus suffix: =
				if l.readSingleRune(r) {
					kind += 2
				}

				if l.readSingleRune('=') {
					kind += 1
				}

			// Repeat into self or equals
			case '>', '?', '*':
				if !l.readOneOf(runeInSeq([]rune{r, '='})) {
					break
				}

				switch l.last {
				case r:
					kind += 1
				case '=':
					kind += 2
				}

			// Repeat into self, equals, or arrow
			case '-', '+', ':', '<':
				if !l.readOneOf(runeInSeq([]rune{r, '=', '>'})) {
					break
				}

				switch l.last {
				case r:
					kind += 1
				case '=':
					kind += 2
				case '>':
					kind += 3
				}

			case '=':
				if !l.readOneOfStr("=>") {
					break
				}

				switch l.last {
				case r:
					kind += 1
				case '>':
					kind += 2
				}
			}

			tok = l.token(kind, true, nil)

		default:
			tok = l.token(TError, false, fmt.Errorf("Lexer: readToken: unrecognized token starting with %q", r))
		}

		if err == nil {
			err = tok.Err
		}
		return tok, err
	}
}

// pushReadCapture pushes a capture bit signalling whether or not to capture tokens read. This is currently only used to discard
// insignificant whitespace.
func (l *Lexer) pushReadCapture(capture bool) {
	if l.writeBufferCount >= 64 {
		panic(fmt.Errorf("Lexer: pushReadCapture: writeBuffer bit overflow (count=%d >= 64)", l.writeBufferCount))
	}

	l.writeBufferCount++
	bit := uint64(1)
	if !capture {
		bit = 0
	}
	l.writeBuffer = (l.writeBuffer << 1) | bit
}

func (l *Lexer) popReadCapture() bool {
	if l.writeBufferCount <= 0 {
		panic(fmt.Errorf("Lexer: popReadCapture: writeBuffer bit underflow (count=%d <= 0)", l.writeBufferCount))
	}
	l.writeBuffer = l.writeBuffer >> 1
	l.writeBufferCount--
	return l.shouldCaptureReads()
}

func (l *Lexer) shouldCaptureReads() bool {
	return l.writeBuffer&0x1 == 0x1
}

// unreadRune unreads the last rune that the Lexer read from its RuneScanner. If there is nothing to unread, it returns errInvalidUnread,
// otherwise it returns any error from the RuneScanner's UnreadRune method.
func (l *Lexer) unreadRune() error {
	if l.lastSz != -1 && l.loc.Off > 0 {
		l.loc = l.lastLoc
		l.last = 0
		l.lastSz = -1
		l.lastErr = l.in.UnreadRune()
		return l.lastErr
	}
	return errInvalidUnread
}

// takeRune counts a rune towards line/column/offset measurements for tokens. Unless an error occurs, all runes read should be passed,
// in order, to this function.
func (l *Lexer) takeRune(r rune, size int) error {
	if size <= 0 {
		size = utf8.RuneLen(r)
	}

	if size == -1 {
		panic(fmt.Errorf("Lexer: takeRune: rune %q has encoded length %d", r, size))
	}

	l.loc.Off += size
	if isNewline(l.last) {
		l.loc.Line++
		l.loc.Col = 1
	} else {
		l.loc.Col++
	}

	if l.shouldCaptureReads() {
		n, err := writeRune(&l.buf, r)
		if size != n || err != nil {
			log.Printf("Error buffering rune %q: (size=%d written=%d) %v", r, size, n, err)
		}

		return err
	}

	return nil
}

func (l *Lexer) readRune() (rune, int, error) {
	r, n, err := l.in.ReadRune()
	l.last, l.lastSz, l.lastErr, l.lastLoc = r, n, err, l.loc
	if n > 0 {
		l.takeRune(r, n)
	}
	return r, n, err
}

// peekRune attempts to read the next rune out of the input scanner, then immediately unreads it.
func (l *Lexer) peekRune() (rune, error) {

	sym, _, err := l.in.ReadRune()
	if err != nil && err != io.EOF {
		return 0, err
	}
	return sym, l.in.UnreadRune()
}

func (l *Lexer) skipWS() error {
	l.pushReadCapture(false)
	defer l.popReadCapture()
	_, err := l.readWhile(false, isWhitespace)
	return err
}

func (l *Lexer) readSingleRune(r rune) bool {
	return l.readOneOf(func(q rune) bool { return q == r })
}

func (l *Lexer) readOneOfStr(oneOf string) bool {
	return l.readOneOf(runeInSeq([]rune(oneOf)))
}

func (l *Lexer) readOneOf(test func(rune) bool) bool {
	r, n, err := l.readRune()
	if err != nil {
		if err == io.EOF {
			return false
		}

		goto unread
	} else if test(r) {
		return true
	}

unread:
	if err = l.unreadRune(); err != nil {
		log.Printf("readOneOf: error unreading rune: %v", err)
	}

	if l.shouldCaptureReads() {
		l.buf.Truncate(l.buf.Len() - n)
	}

	return false
}

func (l *Lexer) readUntil(inclusive bool, stop func(rune) bool) (n int, err error) {
	defer func(start int) {
		if err != nil {
			n = l.loc.Off - start
			return
		}

		// Drop buffered rune for exclusive reads
		if !inclusive {
			if l.shouldCaptureReads() {
				l.buf.Truncate(l.buf.Len() - l.lastSz)
			}
			err = l.unreadRune()
		}

		n = l.loc.Off - start
	}(l.loc.Off)

	for {
		r, _, err := l.readRune()
		switch {
		case err != nil:
			return 0, err
		case stop(r):
			return 0, nil
		}
	}
}

func (l *Lexer) readWhile(inclusive bool, while func(rune) bool) (n int, err error) {
	return l.readUntil(inclusive, negateRuneTest(while))
}

func (l *Lexer) readBlockComment() (Token, error) {
	l.pushReadCapture(true)
	defer l.popReadCapture()

	_, err := l.readUntil(true, runesMatchSeq("*/"))
	if err == io.EOF {
		return l.token(TError, false, errBlockCommentEOF), err
	}

	return l.token(TBlockComment, true, nil), err
}

func (l *Lexer) readLineComment() (Token, error) {
	l.pushReadCapture(true)
	defer l.popReadCapture()
	_, err := l.readUntil(false, isNewline)
	if err == io.EOF {
		err = nil
	}
	return l.token(TLineComment, true, nil), err
}

func (l *Lexer) readComment() (Token, error) {
	l.pushReadCapture(true)
	defer l.popReadCapture()

	var terr error
	last := l.last
	r, _, err := l.readRune()
	switch {
	case last == '/' && r == '/':
		return l.readLineComment()
	case last == '/' && r == '*':
		return l.readBlockComment()
	case err != nil:
		// nop
	default:
		terr = errNoComment
	}
	return l.token(TError, false, terr), err
}

func (l *Lexer) token(kind TokenType, useBuffer bool, err error) Token {
	var txt []byte
	if l.buf.Len() > 0 {
		if useBuffer {
			txt = make([]byte, l.buf.Len())
			copy(txt, l.buf.Bytes())
		}

		l.buf.Reset()
	}

	return Token{Start: l.mark, End: l.loc, Kind: kind, Text: txt, Err: err}
}

func (l *Lexer) readNumber() (Token, error) {
	var terr, err error
	kind := TIntLit

	l.pushReadCapture(true)
	defer l.popReadCapture()

	if l.last == '0' {
		switch {
		case l.readOneOf(isHexSep):
			var n int
			n, err = l.readWhile(false, isHex)
			kind = THexLit
			if n == 0 {
				terr = errMalformedHex
			}
			goto done
		case l.readOneOf(isBinSep):
			var n int
			n, err = l.readWhile(false, isBin)
			kind = TBinLit
			if n == 0 {
				terr = errMalformedBin
			}
			goto done
		case l.readOneOf(isPeriod):
			goto parseDecimal
		default:
			// Try octal
			var n int
			n, err = l.readWhile(false, isOct)
			if n > 0 {
				kind = TOctLit
			}
		}
	} else if l.last == '.' {
		goto parseDecimal
	}

	_, err = l.readWhile(false, isDec)
	if err == io.EOF {
		err = nil
	}

	if err != nil {
		goto done
	}

	if !l.readOneOf(isPeriod) {
		goto checkExponent
	}

parseDecimal:
	kind = TFloatLit
	{
		var decLen int
		decLen, err = l.readWhile(false, isDec)
		if decLen == 0 || err != nil {
			terr = fmt.Errorf("Lexer: readNumber: expected digits following decimal point")
			goto done
		}
	}

checkExponent:
	{
		if !l.readOneOf(isExp) {
			goto done
		}

		if kind != TFloatLit && kind != TIntLit {
			kind = TIntLit
		}

		// We actually don't care if one of these is read.
		if l.readOneOf(isSign) && l.last == '-' {
			kind = TFloatLit
		}

		kind++
		var expLength int
		expLength, err = l.readWhile(false, isDec)
		if expLength == 0 {
			terr = fmt.Errorf("Lexer: readNumber: expected digits for exponent")
			goto done
		}
	}

done:
	if err == io.EOF {
		err = nil
	}
	if terr != nil {
		kind = TError
	}

	tok := l.token(kind, true, terr)
	if kind == TFloatLit || kind == TFloatExpLit {
		var f big.Float
		_, _, terr = f.Parse(tok.String(), 10)
	} else {
		var i big.Int
		_, ok := i.SetString(tok.String(), 0)
		if !ok {
			tok.Err = fmt.Errorf("Lexer: readNumber: malformed %v %q", kind, tok.String())
			tok.Kind = TError
		}
	}
	return tok, err
}

func (l *Lexer) readString() (Token, error) {
	quote := l.last

	_, err := l.readUntil(true, escapeRuneTest(false, func(r rune) bool { return r == quote }))
	if err == io.EOF {
		return l.token(TError, false, fmt.Errorf("unmatched closing %q before EOF", quote)), nil
	} else if err != nil {
		return l.token(TError, false, err), err
	}

	tok := l.token(TStringLit, true, nil)
	if quote == '\'' {
		tok.Kind = TRuneLit
	}

	if quote == '`' {
		tok.Kind = TRawStringLit
		for i := 1; i+2 < len(tok.Text)-1; i++ {
			if tok.Text[i] == '\\' && (tok.Text[i+1] == '`' || tok.Text[i+1] == '\\') {
				copy(tok.Text[i:], tok.Text[i+1:])
				tok.Text = tok.Text[:len(tok.Text)-1]
			}
		}
		tok.Text = tok.Text[1 : len(tok.Text)-1]
	} else {
		unquoted, err := strconv.Unquote(string(tok.Text))
		if err == nil {
			tok.Text = []byte(unquoted)
		} else {
			tok.Kind = TError
			tok.Err = err
		}
	}

	return tok, nil
}

var nilKW = []byte("nil")
var trueKW = []byte("true")
var falseKW = []byte("false")

// readWord reads an identifier or known keyword. Keywords are currently hardcoded to only be nil, true, and false (see the above global
// byte slices). This could be rewritten to check a map of strings to token types if there are enough keywords.
func (l *Lexer) readWord() (Token, error) {
	l.pushReadCapture(true)
	defer l.popReadCapture()
	_, err := l.readWhile(false, isWord)
	if err == nil {
		l.readOneOf(isWordSentinel)
	}

	tok := l.token(TIdent, true, nil)
	if bytes.Equal(tok.Text, nilKW) {
		tok.Kind = TNil
	} else if bytes.Equal(tok.Text, trueKW) {
		tok.Kind = TTrue
	} else if bytes.Equal(tok.Text, falseKW) {
		tok.Kind = TFalse
	}

	return tok, err
}
