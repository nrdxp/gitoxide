use crate::blob::BuiltinDriver;

impl BuiltinDriver {
    /// Return the name of this instance.
    pub fn as_str(&self) -> &str {
        match self {
            BuiltinDriver::Text => "text",
            BuiltinDriver::Binary => "binary",
            BuiltinDriver::Union => "union",
        }
    }

    /// Get all available built-in drivers.
    pub fn all() -> &'static [Self] {
        &[BuiltinDriver::Text, BuiltinDriver::Binary, BuiltinDriver::Union]
    }

    /// Try to match one of our variants to `name`, case-sensitive, and return its instance.
    pub fn by_name(name: &str) -> Option<Self> {
        Self::all().iter().find(|variant| variant.as_str() == name).copied()
    }
}

///
pub mod binary {
    /// What to do when having to pick a side to resolve a conflict.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    pub enum ResolveWith {
        /// Chose the ancestor to resolve a conflict.
        Ancestor,
        /// Chose our side to resolve a conflict.
        Ours,
        /// Chose their side to resolve a conflict.
        Theirs,
    }

    /// Tell the caller of [`merge()`](function::merge) which side was picked.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    pub enum Pick {
        /// Chose the ancestor.
        Ancestor,
        /// Chose our side.
        Ours,
        /// Chose their side.
        Theirs,
    }

    pub(super) mod function {
        use crate::blob::builtin_driver::binary::{Pick, ResolveWith};
        use crate::blob::Resolution;

        /// As this algorithm doesn't look at the actual data, it returns a choice solely based on logic.
        ///
        /// It always results in a conflict with `current` being picked unless `on_conflict` is not `None`.
        pub fn merge(on_conflict: Option<ResolveWith>) -> (Pick, Resolution) {
            match on_conflict {
                None => (Pick::Ours, Resolution::Conflict),
                Some(resolve) => (
                    match resolve {
                        ResolveWith::Ours => Pick::Ours,
                        ResolveWith::Theirs => Pick::Theirs,
                        ResolveWith::Ancestor => Pick::Ancestor,
                    },
                    Resolution::Complete,
                ),
            }
        }
    }
}
pub use binary::function::merge as binary;

///
pub mod text {
    /// The way the built-in [text driver](crate::blob::BuiltinDriver::Text) will express
    /// merge conflicts in the resulting file.
    #[derive(Default, Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    pub enum ConflictStyle {
        /// Only show the zealously minified conflicting lines of the local changes and the incoming (other) changes,
        /// hiding the base version entirely.
        ///
        /// ```
        /// line1-changed-by-both
        /// <<<<<<< local
        /// line2-to-be-changed-in-incoming
        /// =======
        /// line2-changed
        /// >>>>>>> incoming
        ///```
        #[default]
        Merge,
        /// Show non-minimized hunks of local changes, the base, and the incoming (other) changes.
        ///
        /// This mode does not hide any information.
        /// ```
        /// <<<<<<< local
        /// line1-changed-by-both
        /// line2-to-be-changed-in-incoming
        /// ||||||| 9a8d80c
        /// line1-to-be-changed-by-both
        /// line2-to-be-changed-in-incoming
        /// =======
        /// line1-changed-by-both
        /// line2-changed
        /// >>>>>>> incoming
        ///```
        Diff3,
        /// Like [`Diff3](Self::Diff3), but will show *minimized* hunks of local change and the incoming (other) changes,
        /// as well as non-minimized hunks of the base.
        ///
        /// ```
        /// line1-changed-by-both
        /// <<<<<<< local
        /// line2-to-be-changed-in-incoming
        /// ||||||| 9a8d80c
        /// line1-to-be-changed-by-both
        /// line2-to-be-changed-in-incoming
        /// =======
        /// line2-changed
        /// >>>>>>> incoming
        /// ```
        ZealousDiff3,
    }

    /// Options for the builtin [text driver](crate::blob::BuiltinDriver::Text).
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub struct Options {
        /// Determine of the diff will be performed.
        /// Defaults to [`imara_diff::Algorithm::Myers`].
        pub diff_algorithm: imara_diff::Algorithm,
        /// How to visualize conflicts in merged files.
        pub conflict_style: ConflictStyle,
        /// The amount of markers to draw, defaults to 7, i.e. `<<<<<<<`
        pub marker_size: usize,
        /// Decide what to do to automatically resolve conflicts.
        /// If `None`, add conflict markers according to `conflict_style` and `marker_size`.
        pub on_conflict: Option<ResolveWith>,
    }

    impl Default for Options {
        fn default() -> Self {
            Options {
                conflict_style: Default::default(),
                marker_size: 7,
                on_conflict: None,
                diff_algorithm: imara_diff::Algorithm::Myers,
            }
        }
    }

    /// What to do to resolve a conflict.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    pub enum ResolveWith {
        /// Chose our side to resolve a conflict.
        Ours,
        /// Chose their side to resolve a conflict.
        Theirs,
        /// Place our and their lines one after another, in any order
        Union,
    }

    pub(super) mod function {
        use crate::blob::builtin_driver::text::{ConflictStyle, Options, ResolveWith};
        use crate::blob::Resolution;
        use bstr::{BStr, ByteSlice, ByteVec};
        use std::iter::Peekable;
        use std::ops::Range;

        /// Merge `current` and `other` with `ancestor` as base according to `opts`.
        /// Use `current_label`, `other_label` and `ancestor_label` to annotate conflict sections.
        ///
        /// `input` is for reusing memory of lists of tokens, and `other_tokens` is memory
        /// for storing tokens for `other`.
        /// Place the merged result in `out` (cleared before use) and return the resolution.
        ///
        /// # Important
        ///
        /// *The caller* is responsible for clearing it, otherwise tokens will accumulate.
        /// This idea is to save time if the input is known to be very similar.
        #[allow(clippy::too_many_arguments)]
        pub fn merge<'a>(
            out: &mut Vec<u8>,
            input: &mut imara_diff::intern::InternedInput<&'a [u8]>,
            current: &'a [u8],
            current_label: Option<&BStr>,
            ancestor: &'a [u8],
            ancestor_label: Option<&BStr>,
            other: &'a [u8],
            other_label: Option<&BStr>,
            opts: Options,
        ) -> Resolution {
            out.clear();
            input.update_before(tokens(ancestor));
            input.update_after(tokens(current));

            let current_hunks = imara_diff::diff(
                opts.diff_algorithm,
                input,
                CollectHunks {
                    side: Side::Current,
                    hunks: Default::default(),
                },
            );

            let current_tokens = std::mem::take(&mut input.after);
            input.update_after(tokens(other));

            let mut hunks = imara_diff::diff(
                opts.diff_algorithm,
                input,
                CollectHunks {
                    side: Side::Other,
                    hunks: current_hunks,
                },
            );

            hunks.sort_by(|a, b| a.before.start.cmp(&b.before.start));
            let mut hunks = hunks.into_iter().peekable();
            let mut intersecting = Vec::new();
            let mut ancestor_integrated_until = 0;
            let mut resolution = Resolution::Complete;
            // TODO(performance): instead of skipping hunks, let's not compute these ones at all, but only once all tests are there.
            let mut filled_hunks = Vec::with_capacity(2);
            while let Some(hunk) = hunks.next() {
                if take_intersecting(&hunk, &mut hunks, &mut intersecting) {
                    fill_ancestor(&hunk.before, &mut intersecting);

                    let filled_hunks_side = hunk.side;
                    filled_hunks.clear();
                    filled_hunks.push(hunk);
                    fill_ancestor(
                        &intersecting
                            .first()
                            .zip(intersecting.last())
                            .map(|(f, l)| f.before.start..l.before.end)
                            .expect("at least one entry"),
                        &mut filled_hunks,
                    );
                    match opts.on_conflict {
                        None => {
                            let (hunks_front_and_back, num_hunks_front) = match opts.conflict_style {
                                ConflictStyle::Merge | ConflictStyle::ZealousDiff3 => zealously_contract_hunks(
                                    &mut filled_hunks,
                                    &mut intersecting,
                                    input,
                                    &current_tokens,
                                ),
                                ConflictStyle::Diff3 => (Vec::new(), 0),
                            };
                            let (our_hunks, their_hunks) = match filled_hunks_side {
                                Side::Current => (&filled_hunks, &intersecting),
                                Side::Other => (&intersecting, &filled_hunks),
                                Side::Ancestor => {
                                    unreachable!("initial hunks are never ancestors")
                                }
                            };
                            // TODO: dedup preamble, simplify this - we know that our and their hunks aren't empty.
                            let (front_hunks, back_hunks) = hunks_front_and_back.split_at(num_hunks_front);
                            let first_hunk = front_hunks
                                .first()
                                .or(our_hunks.first())
                                .expect("at least one hunk to write");
                            let last_hunk = back_hunks
                                .last()
                                .or(their_hunks.last())
                                .or(our_hunks.last())
                                .or(front_hunks.last())
                                .expect("at least one hunk");
                            write_ancestor(input, ancestor_integrated_until, first_hunk.before.start as usize, out);

                            write_hunks(front_hunks, input, &current_tokens, out);
                            if their_hunks.is_empty() {
                                // TODO: assure we run into this
                                write_hunks(our_hunks, input, &current_tokens, out);
                            } else if our_hunks.is_empty() {
                                // TODO: assure we run into this
                                write_hunks(their_hunks, input, &current_tokens, out);
                            } else {
                                let our_nl = detect_line_ending(our_hunks, input, &current_tokens);
                                let their_nl = detect_line_ending(their_hunks, input, &current_tokens);
                                match opts.conflict_style {
                                    ConflictStyle::Merge => {
                                        if contains_lines(our_hunks) || contains_lines(their_hunks) {
                                            resolution = Resolution::Conflict;
                                            write_conflict_marker(out, b'<', current_label, opts.marker_size, our_nl);
                                            write_hunks(our_hunks, input, &current_tokens, out);
                                            write_conflict_marker(out, b'=', None, opts.marker_size, their_nl);
                                            write_hunks(their_hunks, input, &current_tokens, out);
                                            write_conflict_marker(out, b'>', other_label, opts.marker_size, their_nl);
                                        }
                                    }
                                    ConflictStyle::Diff3 | ConflictStyle::ZealousDiff3 => {
                                        if contains_lines(our_hunks) || contains_lines(their_hunks) {
                                            if hunks_differ_in_diff3(
                                                opts.conflict_style,
                                                our_hunks,
                                                their_hunks,
                                                input,
                                                &current_tokens,
                                            ) {
                                                resolution = Resolution::Conflict;
                                                write_conflict_marker(
                                                    out,
                                                    b'<',
                                                    current_label,
                                                    opts.marker_size,
                                                    our_nl,
                                                );
                                                write_hunks(our_hunks, input, &current_tokens, out);
                                                let ancestor_hunk = Hunk {
                                                    before: first_hunk.before.start..last_hunk.before.end,
                                                    after: Default::default(),
                                                    side: Side::Ancestor,
                                                };
                                                let ancestor_hunk = std::slice::from_ref(&ancestor_hunk);
                                                let ancestor_nl =
                                                    detect_line_ending(ancestor_hunk, input, &current_tokens);
                                                write_conflict_marker(
                                                    out,
                                                    b'|',
                                                    ancestor_label,
                                                    opts.marker_size,
                                                    ancestor_nl,
                                                );
                                                write_hunks(ancestor_hunk, input, &current_tokens, out);
                                                write_conflict_marker(out, b'=', None, opts.marker_size, their_nl);
                                                write_hunks(their_hunks, input, &current_tokens, out);
                                                write_conflict_marker(
                                                    out,
                                                    b'>',
                                                    other_label,
                                                    opts.marker_size,
                                                    their_nl,
                                                );
                                            } else {
                                                write_hunks(our_hunks, input, &current_tokens, out);
                                            }
                                        }
                                    }
                                }
                            }
                            write_hunks(back_hunks, input, &current_tokens, out);
                            // TODO: have a sample that validates this!
                            ancestor_integrated_until = last_hunk.before.end;
                        }
                        Some(resolve) => {
                            match resolve {
                                ResolveWith::Ours | ResolveWith::Theirs => {
                                    let (our_hunks, their_hunks) = match filled_hunks_side {
                                        Side::Current => (&filled_hunks, &intersecting),
                                        Side::Other => (&intersecting, &filled_hunks),
                                        Side::Ancestor => {
                                            unreachable!("initial hunks are never ancestors")
                                        }
                                    };
                                    let hunks_to_write = if resolve == ResolveWith::Ours {
                                        our_hunks
                                    } else {
                                        their_hunks
                                    };
                                    if let Some(first_hunk) = hunks_to_write.first() {
                                        write_ancestor(
                                            input,
                                            ancestor_integrated_until,
                                            first_hunk.before.start as usize,
                                            out,
                                        );
                                    }
                                    write_hunks(hunks_to_write, input, &current_tokens, out);
                                    if let Some(last_hunk) = hunks_to_write.last() {
                                        ancestor_integrated_until = last_hunk.before.end;
                                    }
                                }
                                ResolveWith::Union => {
                                    let (hunks_front_and_back, num_hunks_front) = zealously_contract_hunks(
                                        &mut filled_hunks,
                                        &mut intersecting,
                                        input,
                                        &current_tokens,
                                    );

                                    let (our_hunks, their_hunks) = match filled_hunks_side {
                                        Side::Current => (&filled_hunks, &intersecting),
                                        Side::Other => (&intersecting, &filled_hunks),
                                        Side::Ancestor => {
                                            unreachable!("initial hunks are never ancestors")
                                        }
                                    };
                                    let (front_hunks, back_hunks) = hunks_front_and_back.split_at(num_hunks_front);
                                    let first_hunk = front_hunks
                                        .first()
                                        .or(our_hunks.first())
                                        .expect("at least one hunk to write");
                                    write_ancestor(
                                        input,
                                        ancestor_integrated_until,
                                        first_hunk.before.start as usize,
                                        out,
                                    );
                                    write_hunks(front_hunks, input, &current_tokens, out);
                                    assure_ends_with_nl(out, detect_line_ending(front_hunks, input, &current_tokens));
                                    write_hunks(our_hunks, input, &current_tokens, out);
                                    assure_ends_with_nl(out, detect_line_ending(our_hunks, input, &current_tokens));
                                    write_hunks(their_hunks, input, &current_tokens, out);
                                    if !back_hunks.is_empty() {
                                        assure_ends_with_nl(
                                            out,
                                            detect_line_ending(their_hunks, input, &current_tokens),
                                        );
                                    }
                                    write_hunks(back_hunks, input, &current_tokens, out);
                                    // TODO: have a sample that validates this!
                                    let last_hunk = back_hunks
                                        .last()
                                        .or(their_hunks.last())
                                        .or(our_hunks.last())
                                        .or(front_hunks.last())
                                        .expect("at least one hunk");
                                    ancestor_integrated_until = last_hunk.before.end;
                                }
                            };
                        }
                    }
                } else {
                    write_ancestor(input, ancestor_integrated_until, hunk.before.start as usize, out);
                    ancestor_integrated_until = hunk.before.end;
                    write_hunks(std::slice::from_ref(&hunk), input, &current_tokens, out);
                }
            }
            write_ancestor(input, ancestor_integrated_until, input.before.len(), out);

            resolution
        }

        /// Used only when `diff3` is the conflict style as `zdiff3` automatically reduces hunks into nothing.
        /// Here we check if all hunks are the same.
        fn hunks_differ_in_diff3(
            style: ConflictStyle,
            a: &[Hunk],
            b: &[Hunk],
            input: &imara_diff::intern::InternedInput<&[u8]>,
            current_tokens: &[imara_diff::intern::Token],
        ) -> bool {
            if style != ConflictStyle::Diff3 {
                return true;
            }

            let tokens_for_hunk = |hunk: &Hunk| -> &[imara_diff::intern::Token] {
                match hunk.side {
                    Side::Current => current_tokens,
                    Side::Other => &input.after,
                    Side::Ancestor => &input.before,
                }
            };

            a.iter()
                .flat_map(tokens_for_hunk)
                .ne(b.iter().flat_map(tokens_for_hunk))
        }

        fn contains_lines(hunks: &[Hunk]) -> bool {
            hunks.iter().any(|h| !h.after.is_empty())
        }

        // TODO: find actual line ending based on hunks
        fn detect_line_ending(
            _hunks: &[Hunk],
            _input: &mut imara_diff::intern::InternedInput<&[u8]>,
            _current_tokens: &[imara_diff::intern::Token],
        ) -> &'static BStr {
            b"\n".into()
        }

        fn assure_ends_with_nl(out: &mut Vec<u8>, nl: &BStr) {
            if !out.is_empty() && !out.ends_with(b"\n") {
                out.push_str(nl);
            }
        }

        fn write_conflict_marker(out: &mut Vec<u8>, marker: u8, label: Option<&BStr>, marker_size: usize, nl: &BStr) {
            assure_ends_with_nl(out, nl);
            out.extend(std::iter::repeat(marker).take(marker_size));
            if let Some(label) = label {
                out.push(b' ');
                out.extend_from_slice(label);
            }
            out.push_str(nl);
        }

        fn write_ancestor(input: &imara_diff::intern::InternedInput<&[u8]>, from: u32, to: usize, out: &mut Vec<u8>) {
            if to < from as usize {
                return;
            }
            if let Some(tokens) = input.before.get(from as usize..to) {
                write_tokens(&input.interner, tokens, out);
            }
        }

        /// Look at all hunks in `in_out` and fill in the ancestor in the range of `ancestor_range`.
        /// This is all based on knowing the ranges are sequences of tokens.
        fn fill_ancestor(Range { start, end }: &Range<u32>, in_out: &mut Vec<Hunk>) {
            if in_out.is_empty() {
                return;
            }

            fn ancestor_hunk(start: u32, num_lines: u32) -> Hunk {
                let range = start..start + num_lines;
                Hunk {
                    before: range.clone(),
                    after: range,
                    side: Side::Ancestor,
                }
            }

            fn is_nonzero(num: &u32) -> bool {
                *num > 0
            }

            let first = &in_out[0];
            let mut first_idx = 0;
            if let Some(lines_to_add) = first.before.start.checked_sub(*start).filter(is_nonzero) {
                in_out.insert(0, ancestor_hunk(*start, lines_to_add));
                first_idx += 1;
            }

            let mut added_hunks = false;
            for (idx, next_idx) in (first_idx..in_out.len()).map(|idx| (idx, idx + 1)) {
                let Some(next_hunk) = in_out.get(next_idx) else { break };
                let hunk = &in_out[idx];
                if let Some(lines_to_add) = next_hunk.after.start.checked_sub(hunk.after.end).filter(is_nonzero) {
                    in_out.push(ancestor_hunk(hunk.after.end, lines_to_add));
                    added_hunks = true;
                }
            }
            let in_out_len = in_out.len();
            if added_hunks {
                in_out[first_idx..in_out_len].sort_by_key(|hunk| hunk.before.start);
            }

            let last = &in_out[in_out_len - 1];
            if let Some(lines_to_add) = end.checked_sub(last.before.end).filter(is_nonzero) {
                in_out.push(ancestor_hunk(last.before.end, lines_to_add));
            }
        }

        /// Reduce the area of `a_hunks` and the hunks in `b_hunks` so that only those lines that are
        /// actually different remain. Note that we have to compare the resolved values, not only the tokens,
        /// so `current_tokens` is expected to be known to the `input` (and its `interner`).
        /// Hunks from all input arrays maybe removed in the process from the front and back, in case they
        /// are entirely equal to what's in `hunk`. Note also that `a_hunks` and `b_hunks` are treated to be consecutive,
        /// so [`fill_ancestor()`] must have been called beforehand, and are assumed to covert the same space in the
        /// ancestor buffer.
        /// Use `mode` to determine how hunks may be handled.
        ///
        /// Return a new vector of all the hunks that were removed from front and back, with partial hunks inserted,
        /// along with the amount of hunks that go front, with the remaining going towards the back.
        // TODO: refactor so hunks and their associated data can go into an array for easier handling.
        #[must_use]
        fn zealously_contract_hunks(
            a_hunks: &mut Vec<Hunk>,
            b_hunks: &mut Vec<Hunk>,
            input: &imara_diff::intern::InternedInput<&[u8]>,
            current_tokens: &[imara_diff::intern::Token],
        ) -> (Vec<Hunk>, usize) {
            let line_content = |token_idx: u32, side: Side| {
                let tokens = match side {
                    Side::Current => current_tokens,
                    Side::Other => &input.after,
                    Side::Ancestor => &input.before,
                };
                &input.interner[tokens[token_idx as usize]]
            };
            fn range_by_side(hunk: &mut Hunk) -> &mut Range<u32> {
                match hunk.side {
                    Side::Current | Side::Other => &mut hunk.after,
                    Side::Ancestor => &mut hunk.before,
                }
            }
            fn truncate_hunks_from_from_front(
                hunks: &mut Vec<Hunk>,
                hunks_to_remove_until_idx: Option<usize>,
                hunk_token_equal_till: Option<u32>,
                mut out_hunks: Option<&mut Vec<Hunk>>,
            ) {
                let Some(hunks_to_remove_until_idx) = hunks_to_remove_until_idx else {
                    assert!(hunk_token_equal_till.is_none());
                    return;
                };
                let mut last_index_to_remove = Some(hunks_to_remove_until_idx);
                let hunk = &mut hunks[hunks_to_remove_until_idx];
                let range = range_by_side(hunk);
                if let Some(hunk_token_equal_till) = hunk_token_equal_till {
                    let orig_start = range.start;
                    let new_start = hunk_token_equal_till + 1;
                    range.start = new_start;
                    if Range::<u32>::is_empty(range) {
                        range.start = orig_start;
                    } else if let Some(out) = out_hunks.as_deref_mut() {
                        last_index_to_remove = hunks_to_remove_until_idx.checked_sub(1);
                        let mut removed_hunk = hunk.clone();
                        let new_range = range_by_side(&mut removed_hunk);

                        new_range.start = orig_start;
                        new_range.end = new_start;

                        out.push(removed_hunk);
                    } else {
                        last_index_to_remove = hunks_to_remove_until_idx.checked_sub(1);
                    }
                }
                if let Some(last_index_to_remove) = last_index_to_remove {
                    let mut current_idx = 0;
                    hunks.retain(|hunk| {
                        if current_idx > last_index_to_remove {
                            true
                        } else {
                            current_idx += 1;
                            if let Some(out) = out_hunks.as_deref_mut() {
                                out.push(hunk.clone());
                            }
                            false
                        }
                    });
                }
            }

            fn truncate_hunks_from_from_back(
                hunks: &mut Vec<Hunk>,
                remove_trailing_hunks_from_idx: Option<usize>,
                hunk_token_equal_from: Option<u32>,
                mut out_hunks: Option<&mut Vec<Hunk>>,
            ) {
                let Some(mut remove_trailing_hunks_from_idx) = remove_trailing_hunks_from_idx else {
                    assert!(hunk_token_equal_from.is_none());
                    return;
                };

                let hunk = &mut hunks[remove_trailing_hunks_from_idx];
                let range = range_by_side(hunk);
                if let Some(hunk_token_equal_from) = hunk_token_equal_from {
                    let orig_end = range.end;
                    let new_end = hunk_token_equal_from;
                    range.end = new_end;
                    if Range::<u32>::is_empty(range) {
                        range.end = orig_end;
                    } else if let Some(out) = out_hunks.as_deref_mut() {
                        remove_trailing_hunks_from_idx += 1;
                        let mut removed_hunk = hunk.clone();
                        let new_range = range_by_side(&mut removed_hunk);

                        new_range.start = new_end;
                        new_range.end = orig_end;

                        out.push(removed_hunk);
                    } else {
                        remove_trailing_hunks_from_idx += 1;
                    }
                }
                if let Some(out) = out_hunks {
                    out.extend_from_slice(&hunks[remove_trailing_hunks_from_idx..]);
                }
                hunks.truncate(remove_trailing_hunks_from_idx);
            }

            let (mut last_a_hunk_idx, mut last_b_hunk_idx) = (0, 0);
            let (mut out, hunks_in_front) = {
                let (mut remove_leading_a_hunks_from, mut remove_leading_b_hunks_from) = (None, None);
                let (mut a_hunk_token_equal_till, mut b_hunk_token_equal_till) = (None, None);
                for ((a_token_idx, a_hunk_idx, a_hunk_side), (b_token_idx, b_hunk_idx, b_hunk_side)) in
                    iterate_hunks(a_hunks).zip(iterate_hunks(b_hunks))
                {
                    let a_line = line_content(a_token_idx, a_hunk_side).as_bstr();
                    let b_line = line_content(b_token_idx, b_hunk_side).as_bstr();

                    if last_a_hunk_idx != a_hunk_idx {
                        a_hunk_token_equal_till = None;
                        last_a_hunk_idx = a_hunk_idx;
                    }
                    if last_b_hunk_idx != b_hunk_idx {
                        b_hunk_token_equal_till = None;
                        last_b_hunk_idx = b_hunk_idx;
                    }
                    if a_line == b_line {
                        (remove_leading_a_hunks_from, remove_leading_b_hunks_from) =
                            (Some(a_hunk_idx), Some(b_hunk_idx));
                        (a_hunk_token_equal_till, b_hunk_token_equal_till) = (Some(a_token_idx), Some(b_token_idx));
                    } else {
                        break;
                    }
                }

                let mut out = Vec::with_capacity(remove_leading_a_hunks_from.unwrap_or_else(|| {
                    if a_hunk_token_equal_till.is_some() {
                        1
                    } else {
                        0
                    }
                }));
                truncate_hunks_from_from_front(
                    a_hunks,
                    remove_leading_a_hunks_from,
                    a_hunk_token_equal_till,
                    Some(&mut out),
                );
                truncate_hunks_from_from_front(b_hunks, remove_leading_b_hunks_from, b_hunk_token_equal_till, None);
                let hunks_in_front = out.len();
                (out, hunks_in_front)
            };

            (last_a_hunk_idx, last_b_hunk_idx) = (0, 0);
            {
                let (mut remove_trailing_a_hunks_from, mut remove_trailing_b_hunks_from) = (None, None);
                let (mut a_hunk_token_equal_from, mut b_hunk_token_equal_from) = (None, None);
                for ((a_token_idx, a_hunk_idx, a_hunk_side), (b_token_idx, b_hunk_idx, b_hunk_side)) in
                    iterate_hunks_rev(a_hunks).zip(iterate_hunks_rev(b_hunks))
                {
                    let a_line = line_content(a_token_idx, a_hunk_side).as_bstr();
                    let b_line = line_content(b_token_idx, b_hunk_side).as_bstr();

                    if last_a_hunk_idx != a_hunk_idx {
                        a_hunk_token_equal_from = None;
                        last_a_hunk_idx = a_hunk_idx;
                    }
                    if last_b_hunk_idx != b_hunk_idx {
                        b_hunk_token_equal_from = None;
                        last_b_hunk_idx = b_hunk_idx;
                    }

                    if a_line == b_line {
                        (remove_trailing_a_hunks_from, remove_trailing_b_hunks_from) =
                            (Some(a_hunk_idx), Some(b_hunk_idx));
                        (a_hunk_token_equal_from, b_hunk_token_equal_from) = (Some(a_token_idx), Some(b_token_idx));
                    } else {
                        break;
                    }
                }

                truncate_hunks_from_from_back(
                    a_hunks,
                    remove_trailing_a_hunks_from,
                    a_hunk_token_equal_from,
                    Some(&mut out),
                );
                truncate_hunks_from_from_back(b_hunks, remove_trailing_b_hunks_from, b_hunk_token_equal_from, None);
            }

            (out, hunks_in_front)
        }

        /// Return an iterator over `(token_idx, hunk_idx, hunk_side)` from `hunks`.
        fn iterate_hunks(hunks: &[Hunk]) -> impl Iterator<Item = (u32, usize, Side)> + '_ {
            hunks.iter().enumerate().flat_map(|(hunk_idx, hunk)| {
                match hunk.side {
                    Side::Current | Side::Other => &hunk.after,
                    Side::Ancestor => &hunk.before,
                }
                .clone()
                .map(move |idx| (idx, hunk_idx, hunk.side))
            })
        }

        /// Return a reverse iterator over `(token_idx, hunk_idx, hunk_side)` from `hunks`.
        fn iterate_hunks_rev(hunks: &[Hunk]) -> impl Iterator<Item = (u32, usize, Side)> + '_ {
            hunks.iter().enumerate().rev().flat_map(|(hunk_idx, hunk)| {
                match hunk.side {
                    Side::Current | Side::Other => &hunk.after,
                    Side::Ancestor => &hunk.before,
                }
                .clone()
                .rev()
                .map(move |idx| (idx, hunk_idx, hunk.side))
            })
        }

        fn write_hunks(
            hunks: &[Hunk],
            input: &imara_diff::intern::InternedInput<&[u8]>,
            current_tokens: &[imara_diff::intern::Token],
            out: &mut Vec<u8>,
        ) {
            for hunk in hunks {
                let (tokens, range) = match hunk.side {
                    Side::Current => (current_tokens, &hunk.after),
                    Side::Other => (input.after.as_slice(), &hunk.after),
                    Side::Ancestor => (input.before.as_slice(), &hunk.before),
                };
                write_tokens(&input.interner, &tokens[usize_range(range)], out);
            }
        }

        fn usize_range(range: &Range<u32>) -> Range<usize> {
            range.start as usize..range.end as usize
        }

        fn write_tokens(
            interner: &imara_diff::intern::Interner<&[u8]>,
            tokens: &[imara_diff::intern::Token],
            out: &mut Vec<u8>,
        ) {
            for token in tokens {
                out.extend_from_slice(interner[*token]);
            }
        }

        /// Find all hunks in `iter` which aren't from the same side as `hunk` and intersect with it.
        /// Return `true` if `out` is non-empty after the operation, indicating overlapping hunks were found.
        fn take_intersecting(
            hunk: &Hunk,
            iter: &mut Peekable<impl Iterator<Item = Hunk>>,
            out: &mut Vec<Hunk>,
        ) -> bool {
            out.clear();
            while iter
                .peek()
                .filter(|b_hunk| {
                    b_hunk.side != hunk.side
                        && (hunk.before.contains(&b_hunk.before.start)
                            || (hunk.before.is_empty() && hunk.before.start == b_hunk.before.start))
                })
                .is_some()
            {
                out.extend(iter.next());
            }
            !out.is_empty()
        }

        fn tokens(input: &[u8]) -> imara_diff::sources::ByteLines<'_, true> {
            imara_diff::sources::byte_lines_with_terminator(input)
        }

        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        enum Side {
            Current,
            Other,
            /// A special marker that is just used to be able to mix-in hunks that only point to the ancestor.
            /// Only `before` matters then.
            Ancestor,
        }

        #[derive(Debug, Clone)]
        struct Hunk {
            before: Range<u32>,
            after: Range<u32>,
            side: Side,
        }

        struct CollectHunks {
            hunks: Vec<Hunk>,
            side: Side,
        }

        impl imara_diff::Sink for CollectHunks {
            type Out = Vec<Hunk>;

            fn process_change(&mut self, before: Range<u32>, after: Range<u32>) {
                self.hunks.push(Hunk {
                    before,
                    after,
                    side: self.side,
                });
            }

            fn finish(self) -> Self::Out {
                self.hunks
            }
        }
    }
}
pub use text::function::merge as text;
