#!/usr/bin/env python3
"""delimiter_doctor — locate WHERE a Racket file's brackets go wrong and WHERE they started going wrong.

Exit codes: 0 balanced, 1 imbalanced / read error, 2 usage/IO error.
"""
from __future__ import annotations

import argparse
import bisect
import difflib
import json
import re
import subprocess
import sys
from dataclasses import dataclass
from typing import Optional

OPEN = {"(": ")", "[": "]", "{": "}"}
CLOSE = {v: k for k, v in OPEN.items()}
WS = " \t\r\n\f\v"
DELIMS = set(WS + "()[]{}\",'`;")
BOUND = set(WS + "()[]{}',`@")
PREFIX_RE = re.compile(r"(?:#[A-Za-z0-9]*|[',`@])*$")
HEAD_RE = re.compile(r"""[ \t]*([^\s()\[\]{}",'`;]{1,32})""")
LANG_RE = re.compile(r"#(?:lang|!)[^\n]*")
CONF_RANK = {"strong": 3, "medium": 2, "weak": 1, "low": 0}
SIMPLE_ESC = set("abtnvfre\"'\\")
OCT = set("01234567")
HEX = set("0123456789abcdefABCDEF")
TRAIL_CLOSERS = re.compile(r"[)\]}]+$")
ATOM_LINE = re.compile(r"""\s*['`]?(?:[^\s()\[\]{}",;]+|"(?:[^"\\]|\\.)*")\s*""")

RACKET_SNIPPET = (
    "(define in (current-input-port)) (port-count-lines! in) "
    "(parameterize ([read-accept-reader #f] [read-accept-lang #f]) "
    "(with-handlers ([exn:fail? (lambda (e) (eprintf \"~a\\n\" (exn-message e)) (exit 3))]) "
    "(let loop () (unless (eof-object? (read-syntax 'src in)) (loop)))))"
)

PROTOCOL = """\
AGENT PROTOCOL (Qwen)
  1. Only `STATUS: BALANCED` ends the task. READ-ERROR and IMBALANCED never do.
  2. Fix the EARLIEST origin first; later errors are often cascades. Re-run after every edit.
  3. DECISION APPLY_PLAN -> apply every listed edit, then tell the user.
     DECISION ASK_USER   -> edit nothing yet; paste QUESTIONS FOR USER verbatim, ONE message, wait.
     Where a question has no default, do not choose for the user.
  4. "Lexes BALANCED" proves bracket COUNTS and KINDS only, not that a closer ends the intended
     form. After any non-strong edit show the diff and ask the user to confirm that the last
     element before each inserted closer belongs inside that form.
  5. A string problem comes first: the reader's site is the END of a cascade. Use the
     ORIGIN-CANDIDATES, ask, edit the confirmed one, re-run. A candidate that lexes BALANCED is
     not necessarily right (an edit can hide a bracket inside a string); prefer candidates that
     leave no multi-line strings, and tell the user when the best one still leaves problems.
  6. Change ONLY the characters named. No re-indenting, reformatting or refactors.
  7. Never append closers at EOF unless the user chose that option.
  8. Never guess intent inside quoted data or macro templates: ask.
  9. SMELL lines are informational. Mention one only if you just wrote or edited that string.
 10. READER: DISAGREES -> trust the reader and tell the user the script may be wrong.
 11. At STATUS: BALANCED ask the user to run `raco make FILE` and the tests and paste the output.
  Exit codes: 0 balanced, 1 imbalanced / read error, 2 usage/IO error.
"""


@dataclass
class Opener:
    ch: str
    line: int
    col: int
    vcol: int          # visual column of the form's first char incl. quote/#hash prefix
    leading: bool      # form is the first thing on its line
    head: str
    snippet: str
    off: int
    evidence: Optional[dict] = None

    def label(self) -> str:
        return "`%s%s` at %d:%d" % (self.ch, self.head, self.line, self.col)


def literal_kind(t: str, i: int) -> tuple:
    """(is_byte_string, is_regexp) for the string literal whose opening quote is t[i]."""
    w = t[max(0, i - 5):i]
    for pre in ("#rx#", "#px#", "#rx", "#px", "#"):
        if w.endswith(pre):
            s = i - len(pre)
            if s == 0 or t[s - 1] in BOUND:
                return pre.endswith("#"), pre.startswith(("#rx", "#px"))
    return False, False


class Scanner:
    def __init__(self, text: str, repair: bool = False):
        self.t = text
        self.n = len(text)
        self.repair = repair
        self.lines = text.split("\n")
        self.starts = [0] + [m.end() for m in re.finditer("\n", text)]
        self.stack: list[Opener] = []
        self.problems: list[dict] = []
        self.edits: list[dict] = []
        self.dedent_ev: list[dict] = []
        self.premature_ev: list[dict] = []
        self.recent: list[tuple] = []
        self.seen_line = 0
        self.last_end = 0
        self.depth_at: dict[int, int] = {}
        self.top_at: dict[int, tuple] = {}
        self.depth_filled = 0
        self.strings: list[tuple] = []        # (open_offset, end_offset | None)
        self.escape_errs: list[dict] = []
        self.code_end: dict[int, int] = {}    # line -> offset just after its last code token

    # ---------- position helpers ----------
    def line_of(self, off: int) -> int:
        return bisect.bisect_right(self.starts, off)

    def col_of(self, off: int) -> int:
        return off - self.starts[self.line_of(off) - 1] + 1

    def vcol(self, off: int) -> int:
        ln = self.line_of(off)
        return len(self.t[self.starts[ln - 1]:off].expandtabs(8))

    def sync(self, ln: int) -> None:
        while self.depth_filled < ln:
            self.depth_filled += 1
            self.depth_at[self.depth_filled] = len(self.stack)
            self.top_at[self.depth_filled] = tuple(self.stack[-2:])

    def prob(self, kind: str, off: int, **kw) -> dict:
        p = dict(kind=kind, off=off, line=self.line_of(off), col=self.col_of(off), **kw)
        self.problems.append(p)
        return p

    def edit(self, op, off, text, conf, reason, at_end=None, meta=None, ev=None) -> dict:
        if at_end is not None:
            ln = self.line_of(at_end - 1)
            col = self.col_of(at_end - 1) + 1
        else:
            ln = self.line_of(off)
            col = self.col_of(off)
        e = dict(op=op, off=off, text=text, conf=conf, reason=reason,
                 line=ln, col=col, meta=meta or {}, ev=ev)
        self.edits.append(e)
        return e

    # ---------- token bookkeeping ----------
    def tok(self, i: int, check: bool = True) -> int:
        ln = self.line_of(i)
        self.sync(ln)
        if ln != self.seen_line:
            self.seen_line = ln
            if check:
                self.line_start(i, ln)
        return ln

    def end_tok(self, j: int, code: bool = True) -> None:
        if code:
            self.last_end = j
            self.code_end[self.line_of(max(j - 1, 0))] = j
        self.seen_line = max(self.seen_line, self.line_of(max(j - 1, 0)))

    # ---------- indentation evidence ----------
    def line_start(self, i: int, ln: int) -> None:
        k = self.vcol(i)
        if self.stack:
            sus = []
            for o in reversed(self.stack):
                if o.vcol >= k:
                    sus.append(o)
                else:
                    break
            if sus:
                self.on_dedent(sus, k, ln)
                return
        self.check_premature(k, ln)

    def on_dedent(self, sus: list, k: int, ln: int) -> None:
        leading = [o for o in sus if o.leading]
        conf = "strong" if k == 0 else ("medium" if leading else "weak")
        prev_end = self.last_end
        pl = self.line_of(prev_end - 1)
        pc = self.col_of(prev_end - 1) + 1
        closers = "".join(OPEN[o.ch] for o in sus)
        ev = dict(kind="dedent", line=ln, indent=k, conf=conf, count=len(sus),
                  closers=closers, insert_line=pl, insert_col=pc, inner_off=sus[0].off,
                  openers=[[o.line, o.col, o.label()] for o in sus])
        new = False
        for o in sus:
            if o.evidence is None:
                o.evidence = ev
                new = True
        if new:
            self.dedent_ev.append(ev)
        if self.repair and conf in ("strong", "medium"):
            for _ in sus:
                self.stack.pop()
            self.edit("insert", prev_end, closers, conf,
                      "line %d starts at col %d, at/left of %d open form(s)" % (ln, k, len(sus)),
                      at_end=prev_end, ev=ev)

    def check_premature(self, k: int, ln: int) -> None:
        run = []
        expect = self.last_end
        for op, off in reversed(self.recent):
            if self.t[off + 1:expect].strip() == "":
                run.append((op, off))
                expect = off
            else:
                break
        if not run:
            return
        op, off = run[0]  # outermost, popped last
        if not op.leading or k <= op.vcol:
            return
        if not self.stack:
            conf = "strong"
        else:
            conf = "medium" if k - op.vcol >= 2 else "weak"
        ev = dict(kind="premature", line=ln, indent=k, conf=conf, closer_off=off,
                  closer_line=self.line_of(off), closer_col=self.col_of(off),
                  closer=self.t[off], run="".join(self.t[o] for _, o in reversed(run)),
                  op_line=op.line, op_col=op.col, op_label=op.label(), op_vcol=op.vcol)
        self.premature_ev.append(ev)
        if self.repair and conf in ("strong", "medium"):
            self.stack.append(op)
            self.recent.pop()
            self.edit("delete", off, self.t[off], conf,
                      "line %d is indented deeper than %s, closed just before" % (ln, op.label()),
                      ev=ev)

    # ---------- lexemes ----------
    def do_open(self, i: int) -> None:
        ln = self.tok(i)
        start = self.starts[ln - 1]
        line = self.lines[ln - 1]
        pre = line[: i - start]
        m = PREFIX_RE.search(pre)
        ps = m.start() if m else len(pre)
        vc = len(pre[:ps].expandtabs(8))
        leading = pre[:ps].strip() == ""
        hm = HEAD_RE.match(line[i - start + 1:])
        head = hm.group(1) if hm else ""
        self.stack.append(Opener(self.t[i], ln, i - start + 1, vc, leading, head,
                                 line.strip()[:90], i))
        self.end_tok(i + 1)

    def do_close(self, i: int) -> None:
        ln = self.tok(i, check=False)
        ch = self.t[i]
        snippet = self.lines[ln - 1].strip()[:90]
        if not self.stack:
            self.prob("extra_closer", i, ch=ch, snippet=snippet)
            if self.repair:
                self.edit("delete", i, ch, "low", "closer with no open form")
            self.end_tok(i + 1)
            return
        top = self.stack[-1]
        if OPEN[top.ch] == ch:
            self.stack.pop()
            self.recent.append((top, i))
            if len(self.recent) > 256:
                del self.recent[:128]
        else:
            depth = None
            for d, o in enumerate(reversed(self.stack)):
                if OPEN[o.ch] == ch:
                    depth = d
                    break
            skipped = list(reversed(self.stack))[:depth] if depth else []
            self.prob("mismatch", i, ch=ch, expected=OPEN[top.ch], opener=top,
                      deeper=depth, skipped=skipped, snippet=snippet)
            if depth is not None:
                if self.repair:
                    self.edit("insert", i, "".join(OPEN[o.ch] for o in skipped), "low",
                              "closer matches an outer opener; inner forms unclosed",
                              meta=dict(kind="deeper", ch=ch,
                                        skipped=[o.label() for o in skipped]))
                for _ in range(depth + 1):
                    self.stack.pop()
            else:
                if self.repair:
                    self.edit("replace", i, OPEN[top.ch], "low",
                              "closer kind differs from its opener",
                              meta=dict(kind="subst", old=ch,
                                        opener=[top.line, top.col, top.ch]))
                self.stack.pop()
        self.end_tok(i + 1)

    def esc_err(self, off: int, kind: str, msg: str, rx: bool, sure: bool = True) -> None:
        self.escape_errs.append(dict(off=off, line=self.line_of(off), col=self.col_of(off),
                                     kind=kind, msg=msg, rx=rx, sure=sure, esc=self.t[off:off + 2]))

    def check_escapes(self, i: int, j: int) -> None:
        t = self.t
        byte, rx = literal_kind(t, i)
        what = "byte string" if byte else "string"
        a, b = i + 1, j - 1          # body is t[a:b]; t[b] is the closing quote
        k = a
        while k < b:
            if t[k] != "\\":
                k += 1
                continue
            s = k
            k += 1
            if k >= b:
                break
            c = t[k]
            if c in SIMPLE_ESC or c in "\r\n":
                k += 1
            elif c in OCT:
                v, d = 0, 0
                while k < b and d < 3 and t[k] in OCT:
                    v = v * 8 + int(t[k])
                    k += 1
                    d += 1
                if v > 255:
                    self.esc_err(s, "octal", "octal escape `%s` is above 255 in %s" % (t[s:k], what), rx)
            elif c == "x":
                k += 1
                d = 0
                while k < b and d < 2 and t[k] in HEX:
                    k += 1
                    d += 1
                if d == 0:
                    self.esc_err(s, "hex", "no hex digit after `\\x` in %s" % what, rx)
            elif c in "uU" and not byte:
                lim = 4 if c == "u" else 8
                k += 1
                st = k
                while k < b and k - st < lim and t[k] in HEX:
                    k += 1
                if k == st:
                    self.esc_err(s, "hex", "no hex digit after `\\%s` in %s" % (c, what), rx)
                else:
                    v = int(t[st:k], 16)
                    if c == "U" and v > 0x10FFFF:
                        self.esc_err(s, "range", "`%s` is beyond U+10FFFF in %s" % (t[s:k], what), rx, sure=False)
                    elif 0xD800 <= v <= 0xDFFF:
                        paired = False
                        if c == "u" and v <= 0xDBFF and t.startswith("\\u", k):
                            e2 = k + 2
                            while e2 < b and e2 - (k + 2) < 4 and t[e2] in HEX:
                                e2 += 1
                            if e2 > k + 2 and 0xDC00 <= int(t[k + 2:e2], 16) <= 0xDFFF:
                                paired = True
                                k = e2
                        if not paired:
                            self.esc_err(s, "surrogate", "unpaired surrogate `%s` in %s" % (t[s:k], what), rx, sure=False)
            else:
                self.esc_err(s, "unknown", "unknown escape sequence `\\%s` in %s" % (c, what), rx)
                k += 1

    def do_string(self, i: int) -> int:
        t, n = self.t, self.n
        self.tok(i)
        j = i + 1
        closed = False
        while j < n:
            c = t[j]
            if c == "\\":
                j += 2
                continue
            if c == '"':
                closed = True
                break
            j += 1
        if not closed:
            self.strings.append((i, None))
            self.prob("unterminated", i, what="string")
            self.end_tok(n)
            return n
        j += 1
        self.strings.append((i, j))
        self.check_escapes(i, j)
        self.end_tok(j)
        return j

    def do_block(self, i: int) -> int:
        t, n = self.t, self.n
        self.tok(i, check=False)
        depth, j = 1, i + 2
        while j < n and depth:
            if t.startswith("#|", j):
                depth += 1
                j += 2
            elif t.startswith("|#", j):
                depth -= 1
                j += 2
            else:
                j += 1
        if depth:
            self.prob("unterminated", i, what="block comment #| ... |#")
            j = n
        self.end_tok(j, code=False)
        return j

    def do_here(self, i: int) -> int:
        t, n = self.t, self.n
        self.tok(i)
        eol = t.find("\n", i)
        if eol < 0:
            self.prob("unterminated", i, what="here-string")
            self.end_tok(n)
            return n
        tag = t[i + 3:eol].rstrip("\r")
        pos = eol + 1
        while pos <= n:
            e = t.find("\n", pos)
            e = n if e < 0 else e
            if t[pos:e].rstrip("\r") == tag:
                self.end_tok(e)
                return e
            if e >= n:
                break
            pos = e + 1
        self.prob("unterminated", i, what="here-string (terminator line '%s' never found)" % tag)
        self.end_tok(n)
        return n

    def do_char(self, i: int) -> int:
        t, n = self.t, self.n
        self.tok(i)
        j = i + 2
        if j < n:
            if t[j].isalnum():
                while j < n and t[j].isalnum():
                    j += 1
            else:
                j += 1
        self.end_tok(j)
        return j

    def do_atom(self, i: int) -> int:
        t, n = self.t, self.n
        self.tok(i)
        j = i
        while j < n:
            c = t[j]
            if c == "\\":
                j += 2
                continue
            if c == "|":
                k = t.find("|", j + 1)
                if k < 0:
                    self.prob("unterminated", j, what="|symbol| quote")
                    j = n
                    break
                j = k + 1
                continue
            if c in DELIMS:
                break
            j += 1
        j = min(max(j, i + 1), n)
        self.end_tok(j)
        return j

    # ---------- driver ----------
    def run(self) -> None:
        t, n = self.t, self.n
        i = 1 if t.startswith("\ufeff") else 0
        while True:
            m = LANG_RE.match(t, i)
            if not m:
                break
            i = m.end()
            self.seen_line = self.line_of(max(i - 1, 0))
            while i < n and t[i] in WS:
                i += 1
        while i < n:
            c = t[i]
            if c in WS:
                i += 1
            elif c == ";":
                self.tok(i, check=False)
                e = t.find("\n", i)
                i = n if e < 0 else e
            elif c == "#":
                nx = t[i + 1:i + 2]
                if nx == "|":
                    i = self.do_block(i)
                elif t.startswith("#<<", i):
                    i = self.do_here(i)
                elif nx == "\\":
                    i = self.do_char(i)
                elif nx == ";":
                    self.tok(i)
                    self.end_tok(i + 2)
                    i += 2
                else:
                    i = self.do_atom(i)
            elif c == '"':
                i = self.do_string(i)
            elif c in OPEN:
                self.do_open(i)
                i += 1
            elif c in CLOSE:
                self.do_close(i)
                i += 1
            elif c in "'`,":
                self.tok(i)
                self.end_tok(i + 1)
                i += 1
            else:
                i = self.do_atom(i)
        self.sync(len(self.lines) + 1)
        if self.stack:
            labels = [o.label() for o in self.stack]
            for o in self.stack:
                self.prob("unclosed", o.off, opener=o)
            if self.repair:
                closers = "".join(OPEN[o.ch] for o in reversed(self.stack))
                self.edit("insert", self.last_end, closers, "low",
                          "append after the last code token",
                          at_end=self.last_end,
                          meta=dict(kind="eof", labels=labels, inner_off=self.stack[-1].off))


# ====================== analysis ======================
def apply_edits(text: str, edits: list) -> str:
    out = text
    for e in sorted(edits, key=lambda e: e["off"], reverse=True):
        o = e["off"]
        if e["op"] == "insert":
            out = out[:o] + e["text"] + out[o:]
        elif e["op"] == "delete":
            out = out[:o] + out[o + len(e["text"]):]
        else:
            out = out[:o] + e["text"] + out[o + 1:]
    return out


def continues(t: str, i: int, k: int) -> bool:
    """True if the newline at t[k] inside the string opened at t[i] is an elided `\\<newline>`."""
    p = k - 1
    if p > i and t[p] == "\r":
        p -= 1
    n = 0
    while p > i and t[p] == "\\":
        n += 1
        p -= 1
    return n % 2 == 1


def multiline_strings(s: Scanner) -> list:
    out = []
    t = s.t
    for i, j in s.strings:
        if j is None:
            continue
        l0, l1 = s.line_of(i), s.line_of(j - 1)
        if l1 == l0:
            continue
        only_cont = True
        k = t.find("\n", i, j)
        while k >= 0:
            if not continues(t, i, k):
                only_cont = False
                break
            k = t.find("\n", k + 1, j)
        if not only_cont:
            out.append(dict(line=l0, col=s.col_of(i), end=l1, off=i,
                            snippet=s.lines[l0 - 1].strip()[:70]))
    return out


def quote_hypotheses(text: str, s1: Scanner) -> list:
    """Apply one candidate quote edit per suspect string, re-lex, rank by what remains broken."""
    cap = 6 if len(text) <= 300000 else 3
    ends = {i: j for i, j in s1.strings}
    ml = [i for i, j in s1.strings if j is not None and s1.line_of(j - 1) > s1.line_of(i)]
    suspects = ml[:cap]
    for i, j in s1.strings:
        if j is None and i not in suspects:
            suspects.append(i)
    out, seen = [], set()
    for i in suspects:
        eol = text.find("\n", i)
        eol = len(text) if eol < 0 else eol
        if eol > i and text[eol - 1] == "\r":
            eol -= 1
        e = eol
        while e > i + 1 and text[e - 1] in " \t":
            e -= 1
        plans = [("close_eol", e)]
        m = TRAIL_CLOSERS.search(text, i + 1, e)
        if m and m.start() != e:
            plans.append(("close_before_closers", m.start()))
        plans.append(("delete", i))
        for kind, p in plans:
            if (kind, p) in seen:
                continue
            seen.add((kind, p))
            new = text[:i] + text[i + 1:] if kind == "delete" else text[:p] + '"' + text[p:]
            sc = Scanner(new)
            sc.run()
            rem = sc.problems
            ml_after = len(multiline_strings(sc))
            un = sum(1 for q in rem if q["kind"] == "unterminated")
            j = ends.get(i)
            at = i if kind == "delete" else p
            out.append(dict(kind=kind, off=at, line=s1.line_of(at), col=s1.col_of(at),
                            string_line=s1.line_of(i), string_col=s1.col_of(i),
                            span_end=(s1.line_of(j - 1) if j is not None else len(s1.lines)),
                            balanced=not rem, unterm=un, remaining=len(rem),
                            ml_after=ml_after,
                            reader_site=j is None, idx=len(out)))
    out.sort(key=lambda h: (h["unterm"] > 0, h["ml_after"], not h["balanced"], h["remaining"], h["idx"]))
    return out


def next_code_line(s: Scanner, k: int):
    for idx in range(k, len(s.lines)):
        raw = s.lines[idx].rstrip("\r")
        st = raw.strip()
        if st and not st.startswith(";"):
            return (idx + 1, len(raw[:len(raw) - len(raw.lstrip())].expandtabs(8)), st[:60])
    return None


def placements(res: dict):
    """Single closers that each balance an unclosed-only file but build different trees."""
    s1, t = res["s1"], res["text"]
    if not s1.stack or any(p["kind"] != "unclosed" for p in s1.problems):
        return None
    m, first = len(s1.stack), s1.stack[0]
    cands = []
    for k in range(first.line, len(s1.lines) + 1):
        end = s1.code_end.get(k)
        tail = s1.top_at.get(k + 1)
        if end is None or end <= first.off or not tail:
            continue
        T = tail[-1]
        parent = tail[-2] if len(tail) > 1 else None
        nxt = next_code_line(s1, k)
        cands.append(dict(line=k, off=end, col=s1.col_of(end - 1) + 1, closes=T, parent=parent,
                          nxt=nxt, agrees=(nxt is None or nxt[1] <= T.vcol), closer=OPEN[T.ch]))
    agree = [c for c in cands if c["agrees"]]
    other = [c for c in cands if not c["agrees"]][:6]
    if len(agree) > 8:
        agree = agree[:4] + agree[-4:]
    ok = []
    for c in agree + other:
        new = t[:c["off"]] + c["closer"] + t[c["off"]:]
        ns = Scanner(new)
        ns.run()
        if len(ns.problems) == m - 1 and all(p["kind"] == "unclosed" for p in ns.problems):
            ok.append(c)
    ok.sort(key=lambda c: (not c["agrees"], c["line"]))
    return ok or None


def tail_alt(res: dict, e: dict):
    """If a bare atom sits right before an inserted closer, try moving the innermost closer one code line up."""
    s1, t, s2 = res["s1"], res["text"], res["s2"]
    if e["op"] != "insert":
        return None
    inner = (e.get("ev") or {}).get("inner_off")
    if inner is None:
        inner = e.get("meta", {}).get("inner_off")
    if inner is None:
        return None
    P = e["off"]
    if P <= 0:
        return None
    L = s1.line_of(P - 1)
    core = TRAIL_CLOSERS.sub("", t[s1.starts[L - 1]:P].rstrip())
    if not ATOM_LINE.fullmatch(core):
        return None
    k = L - 1
    while k >= 1 and k not in s1.code_end:
        k -= 1
    if k < 1:
        return None
    alt_off = s1.code_end[k]
    if alt_off <= inner:
        return None
    c = e["text"]
    edits = [x for x in s2.edits if x is not e]
    edits.append(dict(op="insert", off=alt_off, text=c[0]))
    if len(c) > 1:
        edits.append(dict(op="insert", off=P, text=c[1:]))
    ns = Scanner(apply_edits(t, edits))
    ns.run()
    if ns.problems:
        return None
    tail = s1.top_at.get(k + 1)
    return dict(atom=core.strip(), atom_line=L, line=k, col=s1.col_of(alt_off - 1) + 1,
                off=alt_off, moved=c[0], closes=(tail[-1].label() if tail else None))


def analyze(path: str, text: str) -> dict:
    s1 = Scanner(text)
    s1.run()
    res = dict(path=path, text=text, s1=s1, s2=None, repaired=text, s3=None, verified=True,
               suppressed=None, hyp=[], place=None, reader=None)
    if not s1.problems:
        return res
    if any(p["kind"] == "unterminated" for p in s1.problems):
        res["verified"] = False
        res["suppressed"] = ("a swallowed region (unterminated string/comment) makes every later "
                             "bracket observation unreliable, so no bracket plan is simulated; fix the "
                             "quote/comment first and re-run")
        if any(p.get("what") == "string" for p in s1.problems):
            res["hyp"] = quote_hypotheses(text, s1)
        return res
    s2 = Scanner(text, repair=True)
    s2.run()
    repaired = apply_edits(text, s2.edits)
    s3 = Scanner(repaired)
    s3.run()
    res.update(s2=s2, repaired=repaired, s3=s3, verified=not s3.problems)
    if res["verified"]:
        for e in s2.edits:
            e["alt"] = tail_alt(res, e)
    if any(e.get("meta", {}).get("kind") == "eof" for e in s2.edits):
        res["place"] = placements(res)
    return res


# ====================== reader cross-check ======================
def mask_lang(text: str) -> str:
    i = 1 if text.startswith("\ufeff") else 0
    out = text
    while True:
        m = LANG_RE.match(out, i)
        if not m:
            break
        out = out[:m.start()] + " " * (m.end() - m.start()) + out[m.end():]
        i = m.end()
        while i < len(out) and out[i] in WS:
            i += 1
    return out


def reader_check(text: str, label: str) -> tuple:
    try:
        r = subprocess.run(["racket", "-e", RACKET_SNIPPET], input=mask_lang(text).encode("utf-8"),
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=60)
    except FileNotFoundError:
        return (None, "`racket` is not on PATH; cross-check skipped")
    except subprocess.TimeoutExpired:
        return (None, "racket timed out; cross-check skipped")
    err = r.stderr.decode("utf-8", "replace").strip()
    if r.returncode == 0:
        return (True, "the Racket reader accepted the file (module header masked; reads only, nothing evaluated)")
    if r.returncode == 3:
        return (False, re.sub(r"^src(?=:)", label, err))
    return (None, "racket failed unexpectedly (exit %d): %s" % (r.returncode, err[:200]))


def status_of(res: dict) -> str:
    s1, rd = res["s1"], res.get("reader")
    if s1.problems:
        return "IMBALANCED"
    if s1.escape_errs or (rd and rd[0] is False):
        return "READ-ERROR"
    return "BALANCED"


def reader_lines(res: dict) -> list:
    rd = res.get("reader")
    if rd is None:
        return []
    ok, msg = rd
    s1 = res["s1"]
    mine = bool(s1.problems or s1.escape_errs)
    if ok is None:
        return ["READER: " + msg]
    if ok and not mine:
        return ["READER: agrees — " + msg]
    if (not ok) and mine:
        return ["READER: agrees the file is unreadable — " + msg]
    if ok and mine:
        return ["READER: DISAGREES — the reader accepts the file but this script reports problems; "
                "the script is probably wrong here. Show the user the first error and ask before editing."]
    return ["READER: DISAGREES — the reader rejects the file but this script found nothing: " + msg,
            "Trust the reader and ask the user about that location."]


# ====================== text rendering ======================
def ev_text(ev: dict) -> str:
    if ev["kind"] == "dedent":
        ops = ", ".join("%d:%d" % (a, b) for a, b, _ in ev["openers"])
        return ("[%s] line %d starts at col %d, at/left of %d still-open form(s) (%s); they probably "
                "should have closed earlier → insert `%s` at %d:%d (right after the last code token "
                "before line %d)" % (ev["conf"], ev["line"], ev["indent"], ev["count"], ops,
                                     ev["closers"], ev["insert_line"], ev["insert_col"], ev["line"]))
    return ("[%s] line %d is indented to col %d, deeper than %s, which was closed at %d:%d "
            "(closer run `%s`) → that closer is probably premature: delete one `%s` at %d:%d"
            % (ev["conf"], ev["line"], ev["indent"], ev["op_label"], ev["closer_line"],
               ev["closer_col"], ev["run"], ev["closer"], ev["closer_line"], ev["closer_col"]))


def surface_line(path: str, p: dict, nl: int) -> str:
    k = p["kind"]
    if k == "extra_closer":
        return "%s:%d:%d: error: extra `%s` — no form is open here" % (path, p["line"], p["col"], p["ch"])
    if k == "mismatch":
        o = p["opener"]
        s = "%s:%d:%d: error: `%s` found where `%s` was needed to close %s" % (
            path, p["line"], p["col"], p["ch"], p["expected"], o.label())
        if p["deeper"] is not None:
            s += "; `%s` matches an outer opener, so %d inner form(s) are unclosed: %s" % (
                p["ch"], p["deeper"], ", ".join(x.label() for x in p["skipped"]))
        else:
            s += "; no open form takes `%s` (probable wrong bracket kind)" % p["ch"]
        return s
    if k == "unclosed":
        o = p["opener"]
        return "%s:%d:%d: error: unclosed %s — EOF (line %d) reached while still inside it" % (
            path, o.line, o.col, o.label(), nl)
    s = "%s:%d:%d: error: unterminated %s starting here — everything after it is swallowed" % (
        path, p["line"], p["col"], p["what"])
    if p["what"] == "string":
        s += " (this is where the reader gives up, not necessarily where the defect is)"
    return s


def escape_line(path: str, x: dict) -> str:
    return "%s:%d:%d: error: %s%s" % (path, x["line"], x["col"], x["msg"],
                                      "" if x["sure"] else "  [unsure — confirm with --reader]")


def escape_fix(x: dict) -> str:
    if x["kind"] != "unknown":
        return ""
    dbl, drop = "\\" + x["esc"], x["esc"][1:]
    a = "double the backslash → `%s`%s" % (dbl, " (regexp literals need this)" if x["rx"] else "")
    b = "delete the backslash → `%s`" % drop
    return (a + "  |  " + b) if x["rx"] else (b + "  |  " + a)


def hyp_edit_text(h: dict) -> str:
    if h["kind"] == "delete":
        return "delete the stray `\"` at %d:%d" % (h["line"], h["col"])
    how = "end of line" if h["kind"] == "close_eol" else "before the trailing closers"
    return "insert `\"` at %d:%d (%s)" % (h["line"], h["col"], how)


def hyp_outcome(h: dict) -> str:
    if h["balanced"]:
        base = "file then lexes BALANCED"
    elif h["unterm"]:
        base = "an unterminated string remains; %d problem(s) in all" % h["remaining"]
    else:
        base = "quotes then pair up, but %d bracket problem(s) remain" % h["remaining"]
    if h["ml_after"]:
        return base + "; %d multi-line string(s) remain, so a stray quote may still be hiding" % h["ml_after"]
    return base + "; no multi-line strings remain"
#+end_


def hyp_line(h: dict) -> str:
    return "%s — string opened at %d:%d currently runs to line %d%s → %s" % (
        hyp_edit_text(h), h["string_line"], h["string_col"], h["span_end"],
        " (the reader's own site)" if h["reader_site"] else "", hyp_outcome(h))


def origin_for(res: dict, p: dict) -> list:
    s1 = res["s1"]
    out = []
    k = p["kind"]
    if k == "unclosed":
        o = p["opener"]
        out.append("ORIGIN: opened at %d:%d | %s" % (o.line, o.col, o.snippet))
        if o.evidence:
            out.append("EVIDENCE: " + ev_text(o.evidence))
        else:
            out.append("EVIDENCE: none — no later line starts at/left of this opener, so either the "
                       "closer is missing at the END of the enclosed code, or this opener is stray")
    elif k in ("extra_closer", "mismatch"):
        cands = [e for e in s1.premature_ev if e["closer_off"] < p["off"]][-2:]
        for e in reversed(cands):
            out.append("ORIGIN-CANDIDATE: " + ev_text(e))
        if k == "mismatch" and p["opener"].evidence:
            out.append("ORIGIN-CANDIDATE: " + ev_text(p["opener"].evidence))
        if not out:
            out.append("ORIGIN-CANDIDATE: none from indentation; see CONTEXT and QUESTIONS")
    else:
        out.append("ORIGIN: the construct itself starts at %d:%d" % (p["line"], p["col"]))
        if p.get("what") == "string" and res["hyp"]:
            out.append("The reader pairs quotes from the front, so this is only where pairing ran out; "
                       "the quote that is really wrong is earlier.")
            out.append("ORIGIN-CANDIDATES (each = one edit applied, whole file re-lexed; best first):")
            for n, h in enumerate(res["hyp"][:6], 1):
                out.append("  H%d %s" % (n, hyp_line(h)))
            if not any(h["balanced"] for h in res["hyp"]):
                out.append("  note: no single quote edit balances the file (several stray quotes, or "
                           "brackets are also wrong); expect a second round")
    return out


def edit_preview(s: Scanner, e: dict) -> tuple:
    txt = s.lines[e["line"] - 1].rstrip("\r")
    col = e["col"]
    if e["op"] == "insert":
        new = txt[:col - 1] + e["text"] + txt[col - 1:]
    elif e["op"] == "delete":
        new = txt[:col - 1] + txt[col - 1 + len(e["text"]):]
    else:
        new = txt[:col - 1] + e["text"] + txt[col:]
    return txt, new


def closes_text(e: dict) -> str:
    ev, m = e.get("ev"), e.get("meta", {})
    if e["op"] != "insert":
        return ""
    if ev and ev["kind"] == "dedent":
        return ", ".join(x[2] for x in ev["openers"])
    if m.get("kind") == "eof":
        return ", ".join(reversed(m["labels"]))
    if m.get("kind") == "deeper":
        return ", ".join(m["skipped"])
    return ""


def place_text(c: dict) -> str:
    T, par, nxt = c["closes"], c["parent"], c["nxt"]
    head = "[indentation %s] insert `%s` at %d:%d — closes %s" % (
        "agrees" if c["agrees"] else "contradicts", c["closer"], c["line"], c["col"], T.label())
    if nxt is None:
        return head + "; nothing follows"
    nl_, ni, snip = nxt
    if c["agrees"]:
        return head + "; line %d (`%s`) then lies outside it, as its indent %d suggests" % (nl_, snip, ni)
    where = ("an argument of the enclosing %s" % par.label()) if par else "a top-level form"
    return head + "; line %d (`%s`, indent %d) is indented inside it yet would become %s" % (
        nl_, snip, ni, where)


def smell_lines(sm: list) -> list:
    if not sm:
        return ["SMELL: none (no string literal spans lines)"]
    out = ["SMELL: string literal(s) spanning lines (legal; a stray `\"` often hides here) — informational"]
    for s in sm[:12]:
        out.append("  %d:%d → line %d (%d lines): %s" % (s["line"], s["col"], s["end"],
                                                         s["end"] - s["line"] + 1, s["snippet"]))
    if len(sm) > 12:
        out.append("  ... %d more" % (len(sm) - 12))
    return out


def build_questions(res: dict) -> list:
    s1, s2 = res["s1"], res["s2"]
    qs: list = []

    def add(where, ask, opts, default=None):
        qs.append(dict(id="Q%d" % (len(qs) + 1), where=where, ask=ask, options=opts, default=default))

    if res["hyp"]:
        opts = []
        for n, h in enumerate(res["hyp"][:3]):
            opts.append("%s: %s  (%s)" % ("ABC"[n], hyp_edit_text(h), hyp_outcome(h)))
        opts.append("Z: none of these — tell me the line and column where the string must end, or which quote is stray")
        p0 = [p for p in s1.problems if p["kind"] == "unterminated"][0]
        add("line %d" % res["hyp"][0]["line"],
            "The reader gives up at %d:%d, but it pairs quotes from the front, so that is the END of a "
            "cascade. Which single edit matches your intent?" % (p0["line"], p0["col"]), opts)
    edits = sorted(s2.edits, key=lambda e: e["off"]) if s2 else []
    handled_ev = set()
    eof_alt = False
    for e in edits:
        ev, m = e.get("ev"), e.get("meta", {})
        if ev:
            handled_ev.add((ev["kind"], ev["line"]))
        if e.get("alt") and m.get("kind") == "eof":
            eof_alt = True
        if ev and ev["kind"] == "dedent" and e["conf"] != "strong":
            ops = ", ".join("%d:%d" % (a, b) for a, b, _ in ev["openers"])
            add("line %d" % ev["line"],
                "Line %d starts at column %d, level with or left of %d still-open form(s) opened at %s. "
                "Should they be closed before line %d?" % (ev["line"], ev["indent"], ev["count"], ops, ev["line"]),
                ["A: yes — insert `%s` at %d:%d (script proposal)" % (ev["closers"], ev["insert_line"], ev["insert_col"]),
                 "B: no — the forms continue; line %d is merely mis-indented" % ev["line"],
                 "C: other — tell me after which line each form must close"], "A")
        elif ev and ev["kind"] == "premature" and e["conf"] != "strong":
            add("line %d" % ev["line"],
                "Line %d is indented to column %d, deeper than %s, which closes at %d:%d. "
                "Was that closing bracket meant to end the form?" % (
                    ev["line"], ev["indent"], ev["op_label"], ev["closer_line"], ev["closer_col"]),
                ["A: no — delete one `%s` at %d:%d so the form keeps going (script proposal)" % (
                    ev["closer"], ev["closer_line"], ev["closer_col"]),
                 "B: yes — the form ends there; line %d should be re-indented to column %d" % (ev["line"], ev["op_vcol"]),
                 "C: other — tell me which form line %d belongs to" % ev["line"]], "A")
        elif e["conf"] == "low":
            k = m.get("kind")
            where = "line %d" % e["line"]
            if e["op"] == "delete":
                add(where, "`%s` at %d:%d closes nothing. What is intended?" % (e["text"], e["line"], e["col"]),
                    ["A: delete it",
                     "B: an opener is missing earlier — tell me which form should start where",
                     "C: other"])
            elif k == "subst":
                ol, oc, och = m["opener"]
                add(where, "`%s` at %d:%d ends the form opened by `%s` at %d:%d. Which bracket is wrong?" % (
                    m["old"], e["line"], e["col"], och, ol, oc),
                    ["A: closer should be `%s`" % e["text"],
                     "B: the opener `%s` at %d:%d should be `%s`" % (och, ol, oc, CLOSE.get(m["old"], "?")),
                     "C: other"])
            elif k == "deeper":
                add(where, "`%s` at %d:%d matches an outer form, but inner form(s) are still open: %s. "
                    "Where do those inner forms end?" % (m["ch"], e["line"], e["col"], ", ".join(m["skipped"])),
                    ["A: right before this `%s` — insert `%s`" % (m["ch"], e["text"]),
                     "B: earlier — tell me the line after which each should close",
                     "C: this `%s` is the wrong bracket — tell me what it should be" % m["ch"]])
            elif k == "eof" and not res["place"]:
                add("end of file",
                    "These forms are never closed and no indentation shows where they end: %s. What now?"
                    % "; ".join(m["labels"]),
                    ["A: append `%s` after line %d (only if the whole rest of the file belongs inside)" % (
                        e["text"], e["line"]),
                     "B: some end earlier — tell me the line after which each should close",
                     "C: one of these openers is stray — tell me which to delete"])
        a = e.get("alt")
        if a:
            add("line %d" % a["atom_line"],
                "`%s` stands alone on line %d, directly before the inserted closer `%s` (%d:%d). Is it the LAST "
                "element of the form being closed, or an argument of the form that encloses it?" % (
                    a["atom"], a["atom_line"], e["text"], e["line"], e["col"]),
                ["A: last element of the form being closed — keep `%s` at %d:%d" % (e["text"], e["line"], e["col"]),
                 "B: argument of the enclosing form — put `%s` at %d:%d instead (file still lexes BALANCED)" % (
                     a["moved"], a["line"], a["col"]),
                 "C: other — tell me where the closer goes"])
    pl = res.get("place")
    if pl and len(pl) >= 2 and not eof_alt:
        opts = ["%s: insert `%s` at %d:%d — closes %s" % ("ABCDE"[n], c["closer"], c["line"], c["col"], c["closes"].label())
                for n, c in enumerate(pl[:5])]
        opts.append("Z: other — tell me the line after which each unclosed form must end")
        add("end of file",
            "%d closer(s) are missing and indentation cannot say where the first one goes (several placements "
            "balance the file but build different trees). Which form should end where?" % len(s1.stack), opts)
    if not res["suppressed"]:
        for ev in s1.dedent_ev + s1.premature_ev:
            if ev["conf"] == "weak" and (ev["kind"], ev["line"]) not in handled_ev:
                if ev["kind"] == "dedent":
                    ops = ", ".join("%d:%d" % (a, b) for a, b, _ in ev["openers"])
                    add("line %d" % ev["line"],
                        "(weak signal) Line %d starts at column %d, left of mid-line form(s) opened at %s. "
                        "Intentional dedent, or should they have closed earlier?" % (ev["line"], ev["indent"], ops),
                        ["A: intentional — leave as is (default)",
                         "B: close them earlier — insert `%s` at %d:%d" % (ev["closers"], ev["insert_line"], ev["insert_col"])], "A")
                else:
                    add("line %d" % ev["line"],
                        "(weak signal) Line %d is indented one column deeper than %s which closes at %d:%d. "
                        "Is the closing bracket correct?" % (ev["line"], ev["op_label"], ev["closer_line"], ev["closer_col"]),
                        ["A: correct — leave as is (default)",
                         "B: premature — delete one `%s` at %d:%d" % (ev["closer"], ev["closer_line"], ev["closer_col"])], "A")
    for p in s1.problems:
        if p["kind"] == "unterminated" and not (p.get("what") == "string" and res["hyp"]):
            add("line %d" % p["line"],
                "An unterminated %s begins at %d:%d. What is intended?" % (p["what"], p["line"], p["col"]),
                ["A: it is a stray delimiter — delete it",
                 "B: it should end — tell me the line and column where it must end",
                 "C: an earlier string/comment ended too early (look for an unescaped `\"`) — tell me where"])
    for x in s1.escape_errs[:8]:
        where = "line %d" % x["line"]
        if x["kind"] == "unknown":
            dbl, drop = "\\" + x["esc"], x["esc"][1:]
            add(where, "`%s` at %d:%d is not an escape the reader accepts%s. What was meant?" % (
                x["esc"], x["line"], x["col"],
                " (in #rx/#px literals a regexp metacharacter needs a doubled backslash)" if x["rx"] else ""),
                ["A: a literal backslash followed by `%s` — write `%s`%s" % (drop, dbl, "  (script proposal)" if x["rx"] else ""),
                 "B: just `%s` — delete the backslash" % drop,
                 "C: other — tell me the intended text"], "A" if x["rx"] else None)
        else:
            add(where, "%s at %d:%d. Which character was intended?" % (x["msg"], x["line"], x["col"]),
                ["A: the character itself — write it directly instead of an escape",
                 "B: tell me the intended code point"])
    rd = res.get("reader")
    if rd and rd[0] is False and not s1.problems and not s1.escape_errs:
        add("reader", "The Racket reader rejects the file (%s) but this script's lexer saw nothing wrong. "
            "What is intended at that location?" % rd[1][:160],
            ["A: tell me the intended text",
             "B: leave it — the reader message is not a real defect"])
    return qs


def decision(res: dict) -> str:
    s1, s2 = res["s1"], res["s2"]
    if res["suppressed"]:
        return "ASK_USER (bracket plan suppressed: fix the quote/comment problem first; see ORIGIN-CANDIDATES and QUESTIONS)"
    if s2 is None:
        if status_of(res) != "BALANCED":
            return "ASK_USER (see QUESTIONS FOR USER; nothing is edited before the answers)"
        return "NO_ACTION"
    if (res["verified"] and s2.edits
            and all(e["conf"] == "strong" and not e.get("alt") for e in s2.edits)):
        return "APPLY_PLAN (all edits strong, re-lex balanced, no tail ambiguity; tell the user afterwards)"
    return "ASK_USER (see QUESTIONS FOR USER; do not edit non-strong or ambiguous items before the answer)"


def ctx(s: Scanner, center: int, radius: int, bad_from=None) -> list:
    lo, hi = max(1, center - radius), min(len(s.lines), center + radius)
    out = []
    for ln in range(lo, hi + 1):
        if bad_from is not None and ln > bad_from:
            d = "?"
        else:
            d = s.depth_at.get(ln, "?")
        txt = s.lines[ln - 1].rstrip("\r")[:160]
        out.append("%s%6d d=%-3s| %s" % (">" if ln == center else " ", ln, d, txt))
    return out


def render(res: dict, radius: int, show_diff: bool, max_problems: int, smells: bool) -> list:
    path, s1, s2 = res["path"], res["s1"], res["s2"]
    nl = len(s1.lines)
    st = status_of(res)
    sm = multiline_strings(s1)
    if st == "BALANCED":
        L = ["%s: STATUS: BALANCED (%d lines)" % (path, nl)]
        L.extend(reader_lines(res))
        if smells:
            L.extend(smell_lines(sm))
        return L
    L = ["=== delimiter_doctor: %s ===" % path,
         "STATUS: %s (%d lines, %d delimiter error(s), %d escape error(s))" % (
             st, nl, len(s1.problems), len(s1.escape_errs)),
         "DECISION: " + decision(res)]
    L.extend(reader_lines(res))
    L.append("")
    probs = sorted(s1.problems, key=lambda p: p["off"] if p["kind"] != "unclosed" else s1.n + p["off"])
    hidden = 0
    if res["suppressed"]:
        keep = [p for p in probs if p["kind"] == "unterminated"]
        hidden = len(probs) - len(keep)
        probs = keep
    if probs:
        L.append("-- SURFACE (where the reader notices) and ORIGIN (where it likely started) --")
        L.append("Only the first error is fully reliable; later ones may be cascades.")
        for i, p in enumerate(probs[:max_problems], 1):
            L.append("S%d  %s" % (i, surface_line(path, p, nl)))
            for o in origin_for(res, p):
                L.append("      " + o)
        if len(probs) > max_problems:
            L.append("... %d more error(s) omitted (--max-problems)" % (len(probs) - max_problems))
        if hidden:
            L.append("... %d later error(s) hidden: unreliable after a swallowed region (--json lists all)" % hidden)
        L.append("")
    if s1.escape_errs:
        L.append("-- STRING ESCAPES the reader rejects --")
        if probs:
            L.append("(delimiter problems exist: escapes inside a swallowed or phase-shifted string may be "
                     "cascade noise; re-run after fixing them)")
        for i, x in enumerate(s1.escape_errs[:max_problems], 1):
            L.append("X%d  %s" % (i, escape_line(path, x)))
            fx = escape_fix(x)
            if fx:
                L.append("      fix-candidates: " + fx)
        L.append("")
    if probs and not res["suppressed"]:
        L.append("-- INDENTATION EVIDENCE (all, by line) --")
        evs = sorted(s1.dedent_ev + s1.premature_ev, key=lambda e: (e["line"], -CONF_RANK[e["conf"]]))
        if not evs:
            L.append("none")
        for e in evs[:15]:
            L.append("  " + ev_text(e))
        L.append("")
    if res["place"]:
        L.append("-- PLACEMENTS: single closers that each balance the file but build different trees --")
        L.append("(%d closer(s) are missing in total; these are candidates for the FIRST one — re-run after applying it)"
                 % len(s1.stack))
        for n, c in enumerate(res["place"], 1):
            L.append("P%d %s" % (n, place_text(c)))
        L.append("")
    if probs:
        L.append("-- REPAIR PLAN (simulated; bracket edits only) --")
        if res["suppressed"]:
            L.append("suppressed: " + res["suppressed"] + ".")
        else:
            if not s2.edits:
                L.append("no automatic edit available")
            for i, e in enumerate(sorted(s2.edits, key=lambda e: e["off"]), 1):
                before, after = edit_preview(s2, e)
                L.append("E%d [%s] %s `%s` at %d:%d — %s" % (i, e["conf"], e["op"], e["text"], e["line"], e["col"], e["reason"]))
                L.append("    - %s" % before[:160])
                L.append("    + %s" % after[:160])
                cl = closes_text(e)
                if cl:
                    L.append("    closes: " + cl)
                a = e.get("alt")
                if a:
                    L.append("    alt: `%s` stands alone on line %d and may belong to the enclosing form, not the one being closed." % (
                        a["atom"], a["atom_line"]))
                    L.append("         Moving `%s` to %d:%d instead closes %s and the file still lexes BALANCED." % (
                        a["moved"], a["line"], a["col"], a["closes"] or "an outer form"))
            if res["verified"]:
                weakest = min([CONF_RANK[e["conf"]] for e in s2.edits] or [3])
                L.append("SIMULATION: after E1..E%d the file re-lexes BALANCED (weakest edit: %s). This checks bracket "
                         "COUNTS and KINDS only; it does not check which form each closer ends, nor arity or meaning."
                         % (len(s2.edits), [k for k, v in CONF_RANK.items() if v == weakest][0]))
            else:
                L.append("SIMULATION: still broken after the planned edits; remaining:")
                for p in res["s3"].problems[:3]:
                    L.append("    " + surface_line(path, p, len(res["s3"].lines)))
        L.append("")
    if smells or res["hyp"]:
        L.extend(smell_lines(sm))
        L.append("")
    qs = build_questions(res)
    L.append("-- QUESTIONS FOR USER (paste verbatim, one message, then wait) --")
    if not qs:
        L.append("none")
    for q in qs:
        L.append("%s [%s] %s" % (q["id"], q["where"], q["ask"]))
        for o in q["options"]:
            mark = "   <- default" if q["default"] and o.startswith(q["default"] + ":") else ""
            L.append("      " + o + mark)
        if not q["default"]:
            L.append("      (no default: the answer is required)")
    L.append("")
    centers = []
    if probs:
        p0 = probs[0]
        if p0["kind"] == "unclosed":
            centers.append(("unclosed opener", p0["opener"].line))
            if p0["opener"].evidence:
                centers.append(("evidence line", p0["opener"].evidence["line"]))
        elif p0["kind"] == "unterminated":
            if res["hyp"]:
                centers.append(("best origin candidate", res["hyp"][0]["line"]))
            centers.append(("reader site", p0["line"]))
        else:
            centers.append(("first surface error", p0["line"]))
            for e in s1.premature_ev:
                if e["closer_off"] < p0["off"]:
                    centers.append(("premature-close candidate", e["closer_line"]))
    elif s1.escape_errs:
        centers.append(("first escape error", s1.escape_errs[0]["line"]))
    bad_from = None
    if res["suppressed"]:
        bad_from = min([p["line"] for p in probs] + [x["line"] for x in sm])
    seen = set()
    if centers:
        L.append("-- CONTEXT (d = open-bracket depth at start of line; ? = unreliable after a swallowed region) --")
    for name, ln in centers[:4]:
        if ln in seen:
            continue
        seen.add(ln)
        L.append("[%s]" % name)
        L.extend(ctx(s1, ln, radius, bad_from))
    if show_diff and s2 is not None and s2.edits:
        L.append("")
        L.append("-- DIFF of simulated repair --")
        L.extend(l.rstrip("\n") for l in difflib.unified_diff(
            res["text"].split("\n"), res["repaired"].split("\n"),
            "a/" + path, "b/" + path, lineterm="", n=2))
    L.append("")
    L.append(PROTOCOL.rstrip("\n"))
    return L


def odict(o: Opener) -> dict:
    return dict(ch=o.ch, line=o.line, col=o.col, head=o.head, snippet=o.snippet)


def pjson(p: dict) -> dict:
    q = {k: v for k, v in p.items() if k not in ("opener", "skipped")}
    if "opener" in p:
        q["opener"] = odict(p["opener"])
    if "skipped" in p:
        q["skipped"] = [odict(o) for o in p["skipped"]]
    return q


def to_json(res: dict) -> dict:
    s1, s2 = res["s1"], res["s2"]
    return dict(
        path=res["path"],
        status=status_of(res),
        decision=decision(res),
        surface=[pjson(p) for p in s1.problems],
        escape_errors=s1.escape_errs,
        evidence=(s1.dedent_ev + s1.premature_ev) if s1.problems and not res["suppressed"] else [],
        edits=[{k: v for k, v in e.items() if k != "ev"} for e in (s2.edits if s2 else [])],
        plan_suppressed=res["suppressed"],
        re_lexes_balanced=res["verified"],
        hypotheses=res["hyp"],
        placements=[dict(line=c["line"], col=c["col"], closer=c["closer"], closes=c["closes"].label(),
                         indentation_agrees=c["agrees"], next_code=c["nxt"]) for c in (res["place"] or [])],
        smells=multiline_strings(s1),
        reader=res["reader"],
        questions=build_questions(res),
    )


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        prog="delimiter_doctor",
        description="Find where Racket brackets go wrong and where they started going wrong.",
        epilog=PROTOCOL, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("files", nargs="*", help="Racket files; '-' reads stdin")
    ap.add_argument("--protocol", action="store_true", help="print the agent protocol and exit")
    ap.add_argument("--json", action="store_true", help="structured output")
    ap.add_argument("--brief", action="store_true", help="compiler-style lines only")
    ap.add_argument("--diff", action="store_true", help="show unified diff of the simulated repair")
    ap.add_argument("--smells", action="store_true", help="list string literals that span lines (informational)")
    ap.add_argument("--reader", action="store_true", help="cross-check read errors with the real Racket reader")
    ap.add_argument("--context", type=int, default=3, help="context radius in lines (default 3)")
    ap.add_argument("--max-problems", type=int, default=12)
    a = ap.parse_args(argv)
    if a.protocol:
        print(PROTOCOL.rstrip("\n"))
        return 0
    if not a.files:
        ap.error("no files given")

    results, rc = [], 0
    for f in a.files:
        try:
            if f == "-":
                text, label = sys.stdin.read(), "<stdin>"
            else:
                with open(f, encoding="utf-8", errors="replace", newline="") as fh:
                    text, label = fh.read(), f
        except OSError as exc:
            print("%s: cannot read: %s" % (f, exc), file=sys.stderr)
            return 2
        res = analyze(label, text)
        if a.reader:
            res["reader"] = reader_check(text, label)
        results.append(res)
        if status_of(res) != "BALANCED":
            rc = max(rc, 1)

    if a.json:
        print(json.dumps([to_json(r) for r in results], indent=2, ensure_ascii=False))
        return rc
    for r in results:
        if a.brief:
            s1 = r["s1"]
            if status_of(r) == "BALANCED":
                print("%s: BALANCED" % r["path"])
            for p in s1.problems:
                print(surface_line(r["path"], p, len(s1.lines)))
            for x in s1.escape_errs:
                print(escape_line(r["path"], x))
            for ln in reader_lines(r):
                print(ln)
        else:
            print("\n".join(render(r, a.context, a.diff, a.max_problems, a.smells)))
            print()
    return rc


if __name__ == "__main__":
    sys.exit(main())
