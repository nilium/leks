package walker

import "strconv"

// Error is any error returned by a Reader.
type Error int

const (
	// Reader-specific errors

	// ErrBadWalkerState may be returned by a method when the reader wasn't allocated with walker.New.
	ErrBadWalkerState = Error(iota) // invalid reader state
	ErrInvalidRead                  // last op was not ReadToken

	// Mark errors

	ErrMarkNil       // mark is nil
	ErrMarkUnderflow // mark depth underflow
	ErrNoMarks       // walker has no marks
	ErrMarkNotFound  // mark not found
)

func (e Error) Error() string {
	desc, ok := errorDescriptions[e]
	if !ok {
		return "walker: unknown error " + strconv.Itoa(int(e))
	}
	return "walker: " + desc
}

func IsFatalErr(err error) bool {
	return err != nil &&
		(err == ErrMarkNil ||
			err == ErrMarkUnderflow ||
			err == ErrBadWalkerState)
}

var errorDescriptions = map[Error]string{
	ErrBadWalkerState: "invalid reader state",
	ErrInvalidRead:    "last op was not ReadToken",
	ErrMarkNil:        "mark is nil",
	ErrMarkUnderflow:  "mark depth underflow",
	ErrNoMarks:        "walker has no marks",
	ErrMarkNotFound:   "mark not found",
}
