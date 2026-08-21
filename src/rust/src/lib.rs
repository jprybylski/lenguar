use std::collections::BTreeMap;

use extendr_api::prelude::*;
use lengua_core::{DiffTag, Library, Query, TemplateMeta, UpdateStatus, diff_text, template};
use serde_yaml::Value as YamlValue;

/// Reads a named R character vector (or NULL) into `(key, value)` pairs.
fn robj_to_pairs(robj: &Robj) -> extendr_api::Result<Vec<(String, String)>> {
    if robj.is_null() {
        return Ok(Vec::new());
    }
    let names = robj
        .names()
        .ok_or_else(|| Error::Other("expected a named character vector".to_string()))?;
    let values = robj
        .as_str_vector()
        .ok_or_else(|| Error::Other("expected a character vector".to_string()))?;
    Ok(names
        .map(|s| s.to_string())
        .zip(values.into_iter().map(|s| s.to_string()))
        .collect())
}

fn build_meta(title: Nullable<String>, fields: &Robj) -> extendr_api::Result<TemplateMeta> {
    let mut meta = TemplateMeta {
        title: title.into_option(),
        fields: BTreeMap::new(),
    };
    for (k, v) in robj_to_pairs(fields)? {
        meta.fields.insert(k, YamlValue::String(v));
    }
    Ok(meta)
}

/// Initialize a new lengua template library at `path`, or adopt an existing
/// store as its first source via `from_dir`/`from_repo`.
/// @noRd
#[extendr]
fn rs_init(
    path: &str,
    name: Nullable<String>,
    from_dir: Nullable<String>,
    from_repo: Nullable<String>,
    git_ref: Nullable<String>,
    subdir: Nullable<String>,
    force: bool,
) -> extendr_api::Result<String> {
    let name = name.into_option();
    let from_dir = from_dir.into_option();
    let from_repo = from_repo.into_option();
    let git_ref = git_ref.into_option();
    let subdir = subdir.into_option();
    let library = if from_dir.is_some() || from_repo.is_some() {
        Library::init_from(
            path,
            name.as_deref(),
            from_dir.as_deref().map(std::path::Path::new),
            from_repo.as_deref(),
            git_ref.as_deref(),
            subdir.as_deref(),
            force,
        )
        .map_err(|e| e.to_string())?
    } else {
        Library::init(path, name.as_deref()).map_err(|e| e.to_string())?
    };
    Ok(library.manifest_order().remove(0))
}

/// Add another source to an already-initialized library. Returns a
/// `source`/`warnings` list (`warnings` a character vector, possibly empty).
/// @noRd
#[extendr]
fn rs_fetch(
    path: &str,
    name: Nullable<String>,
    from_dir: Nullable<String>,
    from_repo: Nullable<String>,
    git_ref: Nullable<String>,
    subdir: Nullable<String>,
    force: bool,
) -> extendr_api::Result<Robj> {
    let mut library = Library::open(path).map_err(|e| e.to_string())?;
    let outcome = library
        .fetch(
            name.into_option().as_deref(),
            from_dir.into_option().as_deref().map(std::path::Path::new),
            from_repo.into_option().as_deref(),
            git_ref.into_option().as_deref(),
            subdir.into_option().as_deref(),
            force,
        )
        .map_err(|e| e.to_string())?;
    let warnings: Vec<String> = outcome
        .warnings
        .iter()
        .map(|w| {
            format!(
                "'{}' is now shadowed by '{}' (also defined in '{}')",
                w.name, w.winner, w.loser
            )
        })
        .collect();
    Ok(list!(source = outcome.source, warnings = warnings).into())
}

/// Refresh one source (`source`) or every source in the library. Returns a
/// `source`/`status`/`detail` data frame — never errors on a per-source
/// failure, so R code can inspect every row and decide what to do; a
/// genuine fast-forward failure is reported as `status = "error"`.
/// @noRd
#[extendr]
fn rs_update(path: &str, source: Nullable<String>) -> extendr_api::Result<Robj> {
    let library = Library::open(path).map_err(|e| e.to_string())?;
    let results = library.update(source.into_option().as_deref());

    let mut sources = Vec::with_capacity(results.len());
    let mut statuses = Vec::with_capacity(results.len());
    let mut details: Vec<Option<String>> = Vec::with_capacity(results.len());
    for (name, result) in results {
        sources.push(name);
        match result {
            Ok(UpdateStatus::UpToDate) => {
                statuses.push("up-to-date".to_string());
                details.push(None);
            }
            Ok(UpdateStatus::FastForwarded { from, to }) => {
                statuses.push("fast-forwarded".to_string());
                details.push(Some(format!(
                    "{}..{}",
                    &from[..from.len().min(12)],
                    &to[..to.len().min(12)]
                )));
            }
            Err(err @ lengua_core::Error::NotFastForward { .. }) => {
                statuses.push("error".to_string());
                details.push(Some(err.to_string()));
            }
            Err(err) => {
                statuses.push("not-updatable".to_string());
                details.push(Some(err.to_string()));
            }
        }
    }

    let sources: Vec<Rstr> = sources.into_iter().map(Rstr::from).collect();
    let statuses: Vec<Rstr> = statuses.into_iter().map(Rstr::from).collect();
    let details: Vec<Rstr> = details.into_iter().map(Rstr::from).collect();
    Ok(data_frame!(source = sources, status = statuses, detail = details))
}

/// Add or update a template, committing the change. Returns the commit sha.
/// @noRd
#[extendr]
fn rs_add(
    path: &str,
    source: Nullable<String>,
    name: &str,
    title: Nullable<String>,
    fields: Robj,
    body: &str,
    message: &str,
) -> extendr_api::Result<String> {
    let library = Library::open(path).map_err(|e| e.to_string())?;
    let meta = build_meta(title, &fields)?;
    let commit = library
        .add(source.into_option().as_deref(), name, &meta, body, message)
        .map_err(|e| e.to_string())?;
    Ok(commit)
}

/// Render a template with variables substituted, or return its raw body.
/// `rev`, if given, reads the template as it existed at that revision (a
/// lengua tag name, or any revspec gix understands) instead of the working
/// tree. `source`, if given, reads that source directly, bypassing merge
/// precedence across sources.
/// @noRd
#[extendr]
fn rs_get(
    path: &str,
    source: Nullable<String>,
    name: &str,
    vars: Robj,
    raw: bool,
    rev: Nullable<String>,
) -> extendr_api::Result<String> {
    let library = Library::open(path).map_err(|e| e.to_string())?;
    let (entry, _source) = library
        .get(
            source.into_option().as_deref(),
            name,
            rev.into_option().as_deref(),
        )
        .map_err(|e| e.to_string())?;
    if raw {
        return Ok(entry.body);
    }
    let ctx: BTreeMap<String, String> = robj_to_pairs(&vars)?.into_iter().collect();
    let rendered = template::render(&entry.body, &ctx).map_err(|e| e.to_string())?;
    Ok(rendered)
}

/// List every template in the library (merged across sources unless
/// `source` scopes it to one) as a `name`/`title`/`source` data frame.
/// @noRd
#[extendr]
fn rs_list(path: &str, source: Nullable<String>) -> extendr_api::Result<Robj> {
    let library = Library::open(path).map_err(|e| e.to_string())?;
    let entries = library
        .list(source.into_option().as_deref())
        .map_err(|e| e.to_string())?;
    let names: Vec<Rstr> = entries
        .iter()
        .map(|(e, _)| Rstr::from(e.name.clone()))
        .collect();
    let titles: Vec<Rstr> = entries
        .iter()
        .map(|(e, _)| Rstr::from(e.meta.title.clone()))
        .collect();
    let sources: Vec<Rstr> = entries
        .iter()
        .map(|(_, source)| Rstr::from(source.clone()))
        .collect();
    Ok(data_frame!(name = names, title = titles, source = sources))
}

/// Filter templates by frontmatter field (AND of all pairs in `fields`).
/// @noRd
#[extendr]
fn rs_search(path: &str, source: Nullable<String>, fields: Robj) -> extendr_api::Result<Robj> {
    let library = Library::open(path).map_err(|e| e.to_string())?;
    let mut query = Query::new();
    for (k, v) in robj_to_pairs(&fields)? {
        query = query.with(k, v);
    }
    let entries = library
        .search(source.into_option().as_deref(), &query)
        .map_err(|e| e.to_string())?;
    let names: Vec<Rstr> = entries
        .iter()
        .map(|(e, _)| Rstr::from(e.name.clone()))
        .collect();
    let titles: Vec<Rstr> = entries
        .iter()
        .map(|(e, _)| Rstr::from(e.meta.title.clone()))
        .collect();
    let sources: Vec<Rstr> = entries
        .iter()
        .map(|(_, source)| Rstr::from(source.clone()))
        .collect();
    Ok(data_frame!(name = names, title = titles, source = sources))
}

/// Show the commit history for a template as a `commit`/`message` data frame.
/// @noRd
#[extendr]
fn rs_log(path: &str, source: Nullable<String>, name: &str) -> extendr_api::Result<Robj> {
    let library = Library::open(path).map_err(|e| e.to_string())?;
    let entries = library
        .log(source.into_option().as_deref(), name)
        .map_err(|e| e.to_string())?;
    let commits: Vec<Rstr> = entries
        .iter()
        .map(|e| Rstr::from(e.commit.clone()))
        .collect();
    let messages: Vec<Rstr> = entries
        .iter()
        .map(|e| Rstr::from(e.message.clone()))
        .collect();
    Ok(data_frame!(commit = commits, message = messages))
}

/// Diff a template's content between two revisions as a `tag`/`line` data frame.
/// @noRd
#[extendr]
fn rs_diff(
    path: &str,
    source: Nullable<String>,
    name: &str,
    from: &str,
    to: &str,
) -> extendr_api::Result<Robj> {
    let library = Library::open(path).map_err(|e| e.to_string())?;
    let store = library
        .resolve_source(source.into_option().as_deref())
        .map_err(|e| e.to_string())?;
    let old = store.read_at_revision(name, from).map_err(|e| e.to_string())?;
    let new = store.read_at_revision(name, to).map_err(|e| e.to_string())?;
    let rows = diff_text(&old, &new);
    let tags: Vec<Rstr> = rows
        .iter()
        .map(|l| {
            Rstr::from(
                match l.tag {
                    DiffTag::Equal => "equal",
                    DiffTag::Insert => "insert",
                    DiffTag::Delete => "delete",
                }
                .to_string(),
            )
        })
        .collect();
    let lines: Vec<Rstr> = rows.iter().map(|l| Rstr::from(l.line.clone())).collect();
    Ok(data_frame!(tag = tags, line = lines))
}

/// Point a lengua tag at `name`'s current revision (or `rev`). Returns the
/// tagged commit sha.
/// @noRd
#[extendr]
fn rs_tag(
    path: &str,
    source: Nullable<String>,
    name: &str,
    tag: &str,
    rev: Nullable<String>,
    force: bool,
) -> extendr_api::Result<String> {
    let library = Library::open(path).map_err(|e| e.to_string())?;
    let entry = library
        .tag_create(
            source.into_option().as_deref(),
            name,
            tag,
            rev.into_option().as_deref(),
            force,
        )
        .map_err(|e| e.to_string())?;
    Ok(entry.commit)
}

/// List every lengua tag on a template as a `tag`/`commit` data frame.
/// @noRd
#[extendr]
fn rs_tag_list(path: &str, source: Nullable<String>, name: &str) -> extendr_api::Result<Robj> {
    let library = Library::open(path).map_err(|e| e.to_string())?;
    let entries = library
        .tag_list(source.into_option().as_deref(), name)
        .map_err(|e| e.to_string())?;
    let tags: Vec<Rstr> = entries.iter().map(|e| Rstr::from(e.tag.clone())).collect();
    let commits: Vec<Rstr> = entries
        .iter()
        .map(|e| Rstr::from(e.commit.clone()))
        .collect();
    Ok(data_frame!(tag = tags, commit = commits))
}

/// Remove a lengua tag from a template.
/// @noRd
#[extendr]
fn rs_tag_rm(path: &str, source: Nullable<String>, name: &str, tag: &str) -> extendr_api::Result<()> {
    let library = Library::open(path).map_err(|e| e.to_string())?;
    library
        .tag_remove(source.into_option().as_deref(), name, tag)
        .map_err(|e| e.to_string())?;
    Ok(())
}

extendr_module! {
    mod lenguar;
    fn rs_init;
    fn rs_fetch;
    fn rs_update;
    fn rs_add;
    fn rs_get;
    fn rs_list;
    fn rs_search;
    fn rs_log;
    fn rs_diff;
    fn rs_tag;
    fn rs_tag_list;
    fn rs_tag_rm;
}
