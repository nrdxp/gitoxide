//!
#![allow(clippy::empty_docs)]

/// The kind of repository.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Kind {
    /// A submodule worktree, whose `git` repository lives in `.git/modules/**/<name>` of the parent repository.
    ///
    /// Note that 'old-form' submodule will register as `Worktree {is_linked: false}`.
    Submodule,
    /// A bare repository does not have a work tree, that is files on disk beyond the `git` repository itself.
    Bare,
    /// A `git` repository along with a checked out files in a work tree.
    WorkTree {
        /// If true, this is the git dir associated with this _linked_ worktree, otherwise it is a repository with _main_ worktree.
        is_linked: bool,
    },
}

#[cfg(any(feature = "attributes", feature = "excludes"))]
pub mod attributes;
mod cache;
mod config;
///
#[cfg(feature = "blob-diff")]
pub mod diff;
///
#[cfg(feature = "dirwalk")]
mod dirwalk;
///
#[cfg(feature = "attributes")]
pub mod filter;
///
pub mod freelist;
mod graph;
pub(crate) mod identity;
mod impls;
#[cfg(feature = "index")]
mod index;
pub(crate) mod init;
mod kind;
mod location;
#[cfg(feature = "mailmap")]
mod mailmap;
mod object;
#[cfg(feature = "attributes")]
mod pathspec;
mod reference;
mod remote;
mod revision;
mod shallow;
mod state;
#[cfg(feature = "attributes")]
mod submodule;
mod thread_safe;
mod worktree;

///
#[cfg(feature = "tree-editor")]
pub mod edit_tree {
    /// The error returned by [Repository::edit_tree()](crate::Repository::edit_tree).
    #[derive(Debug, thiserror::Error)]
    #[allow(missing_docs)]
    pub enum Error {
        #[error(transparent)]
        FindTree(#[from] crate::object::find::existing::with_conversion::Error),
        #[error(transparent)]
        InitEditor(#[from] crate::object::tree::editor::init::Error),
    }
}

///
#[cfg(feature = "revision")]
pub mod merge_base {
    /// The error returned by [Repository::merge_base()](crate::Repository::merge_base()).
    #[derive(Debug, thiserror::Error)]
    #[allow(missing_docs)]
    pub enum Error {
        #[error(transparent)]
        OpenCache(#[from] crate::repository::commit_graph_if_enabled::Error),
        #[error(transparent)]
        FindMergeBase(#[from] gix_revision::merge_base::Error),
        #[error("Could not find a merge-base between commits {first} and {second}")]
        NotFound {
            first: gix_hash::ObjectId,
            second: gix_hash::ObjectId,
        },
    }
}

///
#[cfg(feature = "revision")]
pub mod merge_base_with_graph {
    /// The error returned by [Repository::merge_base_with_cache()](crate::Repository::merge_base_with_graph()).
    #[derive(Debug, thiserror::Error)]
    #[allow(missing_docs)]
    pub enum Error {
        #[error(transparent)]
        FindMergeBase(#[from] gix_revision::merge_base::Error),
        #[error("Could not find a merge-base between commits {first} and {second}")]
        NotFound {
            first: gix_hash::ObjectId,
            second: gix_hash::ObjectId,
        },
    }
}

///
pub mod commit_graph_if_enabled {
    /// The error returned by [Repository::commit_graph_if_enabled()](crate::Repository::commit_graph_if_enabled()).
    #[derive(Debug, thiserror::Error)]
    #[allow(missing_docs)]
    pub enum Error {
        #[error(transparent)]
        ConfigBoolean(#[from] crate::config::boolean::Error),
        #[error(transparent)]
        OpenCommitGraph(#[from] gix_commitgraph::init::Error),
    }
}

///
#[cfg(feature = "index")]
pub mod index_from_tree {
    /// The error returned by [Repository::index_from_tree()](crate::Repository::index_from_tree).
    #[derive(Debug, thiserror::Error)]
    #[allow(missing_docs)]
    pub enum Error {
        #[error("Could not create index from tree at {id}")]
        IndexFromTree {
            id: gix_hash::ObjectId,
            source: gix_index::init::from_tree::Error,
        },
        #[error("Couldn't obtain configuration for core.protect*")]
        BooleanConfig(#[from] crate::config::boolean::Error),
    }
}

///
pub mod branch_remote_ref_name {

    /// The error returned by [Repository::branch_remote_ref_name()](crate::Repository::branch_remote_ref_name()).
    #[derive(Debug, thiserror::Error)]
    #[allow(missing_docs)]
    pub enum Error {
        #[error("The configured name of the remote ref to merge wasn't valid")]
        ValidateFetchRemoteRefName(#[from] gix_validate::reference::name::Error),
        #[error(transparent)]
        PushDefault(#[from] crate::config::key::GenericErrorWithValue),
        #[error(transparent)]
        FindPushRemote(#[from] crate::remote::find::existing::Error),
    }
}

///
pub mod branch_remote_tracking_ref_name {

    /// The error returned by [Repository::branch_remote_tracking_ref_name()](crate::Repository::branch_remote_tracking_ref_name()).
    #[derive(Debug, thiserror::Error)]
    #[allow(missing_docs)]
    pub enum Error {
        #[error("The name of the tracking reference was invalid")]
        ValidateTrackingRef(#[from] gix_validate::reference::name::Error),
        #[error("Could not get the remote reference to translate into the local tracking branch")]
        RemoteRef(#[from] super::branch_remote_ref_name::Error),
        #[error("Couldn't find remote to obtain fetch-specs for mapping to the tracking reference")]
        FindRemote(#[from] crate::remote::find::existing::Error),
    }
}

///
#[cfg(feature = "attributes")]
pub mod pathspec_defaults_ignore_case {
    /// The error returned by [Repository::pathspec_defaults_ignore_case()](crate::Repository::pathspec_defaults_inherit_ignore_case()).
    #[derive(Debug, thiserror::Error)]
    #[allow(missing_docs)]
    pub enum Error {
        #[error("Filesystem configuration could not be obtained to learn about case sensitivity")]
        FilesystemConfig(#[from] crate::config::boolean::Error),
        #[error(transparent)]
        Defaults(#[from] gix_pathspec::defaults::from_environment::Error),
    }
}

///
#[cfg(feature = "index")]
pub mod index_or_load_from_head {
    /// The error returned by [`Repository::index_or_load_from_head()`][crate::Repository::index_or_load_from_head()].
    #[derive(thiserror::Error, Debug)]
    #[allow(missing_docs)]
    pub enum Error {
        #[error(transparent)]
        HeadCommit(#[from] crate::reference::head_commit::Error),
        #[error(transparent)]
        TreeId(#[from] gix_object::decode::Error),
        #[error(transparent)]
        TraverseTree(#[from] crate::repository::index_from_tree::Error),
        #[error(transparent)]
        OpenIndex(#[from] crate::worktree::open_index::Error),
    }
}

///
#[cfg(feature = "worktree-stream")]
pub mod worktree_stream {
    /// The error returned by [`Repository::worktree_stream()`][crate::Repository::worktree_stream()].
    #[derive(Debug, thiserror::Error)]
    #[allow(missing_docs)]
    pub enum Error {
        #[error(transparent)]
        FindTree(#[from] crate::object::find::existing::Error),
        #[error(transparent)]
        OpenTree(#[from] crate::repository::index_from_tree::Error),
        #[error(transparent)]
        AttributesCache(#[from] crate::config::attribute_stack::Error),
        #[error(transparent)]
        FilterPipeline(#[from] crate::filter::pipeline::options::Error),
        #[error(transparent)]
        CommandContext(#[from] crate::config::command_context::Error),
        #[error("Needed {id} to be a tree to turn into a workspace stream, got {actual}")]
        NotATree {
            id: gix_hash::ObjectId,
            actual: gix_object::Kind,
        },
    }
}

///
#[cfg(feature = "worktree-archive")]
pub mod worktree_archive {
    /// The error returned by [`Repository::worktree_archive()`][crate::Repository::worktree_archive()].
    pub type Error = gix_archive::Error;
}
