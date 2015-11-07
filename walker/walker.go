// Copyright Noel Cower 2015.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

package walker

import (
	"bytes"
	crand "crypto/rand"
	"errors"
	"fmt"
	"strconv"

	"github.com/nilium/leks"
)

var (
	// Reader-specific errors

	// ErrBadWalkerState may be returned by a method when the reader wasn't allocated with walker.New.
	ErrBadWalkerState = errors.New("invalid reader state")
	ErrInvalidRead    = errors.New("last op was not ReadToken")

	// Mark errors

	ErrMarkNil       = errors.New("mark is nil")
	ErrMarkUnderflow = errors.New("mark depth underflow")
	ErrNoMarks       = errors.New("walker has no marks")
	ErrMarkNotFound  = errors.New("mark not found")
)

func IsFatalErr(err error) bool {
	return err != nil &&
		(err == ErrMarkNil ||
			err == ErrMarkUnderflow ||
			err == ErrBadWalkerState)
}

type TokenReader interface {
	ReadToken() (leks.Token, error)
}

type Mark struct {
	i, head, seq int
}

type readOp struct {
	tok leks.Token
	err error
}

type Reader struct {
	r     TokenReader
	last  readOp
	log   []readOp
	marks []*Mark
	seq   int
}

func New(r TokenReader) *Reader {
	seq, err := newSeq()
	if err != nil {
		panic(err)
	}

	if r == nil {
		panic("NewWalker: TokenReader is nil")
	}

	return &Reader{
		r:     r,
		last:  readOp{err: ErrInvalidRead},
		marks: []*Mark{&Mark{i: 0, head: 0, seq: seq - 1024}},
		seq:   seq,
	}
}

// GoString implements GoStringer. This is a more or less debug-heavy way to get walker state, so avoid using it unless you're debugging.
func (w *Reader) GoString() string {
	var b bytes.Buffer

	fmt.Fprintf(&b, "%T{\n\tmarks: %d\n\tlog-size: %d\n", w, len(w.marks)-1, len(w.log))

	if len(w.log) > 0 {
		b.WriteString("\tlog: [\n\t   TOK  LVL| TOKEN\n")

		marked := make(map[int]int, len(w.marks)*2)
		// Most recent mark wins when displaying marks
		for _, m := range w.marks {
			marked[m.i], marked[m.head] = m.seq, m.seq
		}

		visible := func(i int) bool {
			const context = 3
			if i < context || i >= len(w.log)-(context+1) {
				return true
			}
			min, max := i-context, i+context
			for line := range marked {
				if min <= line && line <= max {
					return true
				}
			}
			return false
		}

		wroteLast := false
		for i, p := range w.log {
			if !visible(i) {
				if !wroteLast {
					wroteLast = true
					b.WriteString("\t..... (omitted) ....:\n")
				}

				continue
			}

			fmt.Fprintf(&b, "\t%6d ", i)
			if m, ok := marked[i]; ok {
				fmt.Fprintf(&b, "%13s* ", strconv.FormatInt(int64(m), 36))
				wroteLast = false
			} else if i == len(w.log)-1 {
				b.WriteString("         LAST| ")
			} else {
				b.WriteString("             | ")
			}

			fmt.Fprintf(&b, "%15v", p.tok.Kind)
			if p.tok.Kind == leks.TError {
				fmt.Fprintf(&b, "(%v)", p.tok.Err)
			} else if p.tok.Kind != leks.TEOF {
				fmt.Fprintf(&b, " %q", p.tok.Text)
			}

			if p.err != nil {
				fmt.Fprintf(&b, " (%T: %v)", p.err, p.err)
			}
			b.WriteByte('\n')
		}
		b.WriteString("\t]\n")
	}
	b.WriteByte('}')
	return b.String()
}

func (w *Reader) Mark() *Mark {
	head := len(w.log)
	m := &Mark{
		i:    head,
		head: head,
		seq:  w.seq,
	}
	w.seq++
	w.marks = append(w.marks, m)
	return m
}

func (w *Reader) findMark(m *Mark) (int, error) {
	switch {
	case len(w.marks) == 0:
		return -1, ErrBadWalkerState
	case len(w.marks) == 1:
		return -1, ErrMarkNotFound
	case m == nil:
		return -1, ErrMarkNil
	case m.i < 0 || m.i > len(w.log):
		return -1, ErrMarkUnderflow
	}

	for seq, i := m.seq, len(w.marks)-1; i > 0; i-- {
		if w.marks[i].seq == seq {
			return i, nil
		}
	}

	return -1, ErrMarkNotFound
}

func (w *Reader) pop(m *Mark, commit bool) error {
	markIdx, err := w.findMark(m)
	if err != nil {
		return err
	}

	pred := markIdx - 1
	next := w.marks[markIdx].head // Restore iterator to head unless the mark is committed
	if commit {
		if markIdx == 2 || w.marks[markIdx].i == len(w.log) {
			for i := range w.log {
				w.log[i] = readOp{}
			}
			w.log = w.log[:0]
			next = 0
		} else {
			next = w.marks[markIdx].i
		}
	}

	cur := w.marks[pred]
	cur.i = next
	for n := markIdx; n < len(w.marks); n++ {
		w.marks[n] = nil
	}
	w.marks = w.marks[:markIdx]

	return nil
}

func (w *Reader) Commit(m *Mark) error {
	return w.pop(m, true)
}

func (w *Reader) Reset(m *Mark) error {
	return w.pop(m, false)
}

func (w *Reader) Discard(m *Mark) error {
	markIdx, err := w.findMark(m)
	if err != nil {
		return err
	}

	// Drop the mark entirely.
	copy(w.marks[markIdx:], w.marks[markIdx+1:])
	last := len(w.marks) - 1
	w.marks[last] = nil
	w.marks = w.marks[:last]

	return nil
}

func (w *Reader) CommitAll() {
	w.log = nil
	for i := 1; i < len(w.marks); i++ {
		w.marks[i] = nil
	}
	w.marks = w.marks[0:1]
}

func (w *Reader) DiscardAll() {
	w.log = nil
	for i := 1; i < len(w.marks); i++ {
		w.marks[i] = nil
	}
	w.marks = w.marks[0:1]
}

func (w *Reader) LastToken() (leks.Token, error) {
	if w.last.tok.Kind == 0 && w.last.err == nil {
		return w.last.tok, ErrBadWalkerState
	}

	if m := w.marks[len(w.marks)-1]; len(w.log) > 0 {
		if m.i == 0 {
			return leks.Token{}, ErrInvalidRead
		}

		op := w.log[m.i-1]
		return op.tok, op.err
	}

	return w.last.tok, w.last.err
}

func (w *Reader) ReadToken() (tok leks.Token, err error) {
	defer func() {
		w.last = readOp{tok: tok, err: err}
	}()

	depth := len(w.marks) - 1
	cur := w.marks[depth]
	n := len(w.log)

	if cur.i < n {
		i := cur.i

		tok, err = w.log[i].tok, w.log[i].err
		if err == nil && tok.Kind != leks.TEOF && tok.Kind != leks.TError {
			// Don't advance past sentinel tokens
			cur.i++
		}

		if depth == 0 && cur.i == n {
			w.log = w.log[:0]
		} else if depth == 0 && cur.i > 8 {
			copy(w.log, w.log[cur.i-1:])
			w.log = w.log[:n-(cur.i-1)]
			cur.i = 1
		}

		return tok, err
	}

	tok, err = w.r.ReadToken()
	if depth > 0 {
		// Log the token if there are any outstanding marks
		w.log = append(w.log, readOp{tok, err})
		cur.i = len(w.log)
	}

	if err != nil {
		return tok, err
	}

	return tok, err
}

func newSeq() (int, error) {
	var b [8]byte
	n, err := crand.Read(b[:])
	if err != nil {
		return 0, err
	}

	var ui uint64
	for i, t := 0, n-1; i < t; i++ {
		ui = ui | uint64(b[i])<<uint64(i*8)
	}

	return int(ui), err
}
