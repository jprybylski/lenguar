use std::collections::BTreeMap;

use extendr_api::prelude::*;
use lengua_core::{DiffTag, Query, Store, TemplateMeta, diff_text, template};
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

/// Initialize a new template-library git repo at `path`.
/// @noRd
#[extendr]
fn rs_init(path: &str) -> extendr_api::Result<()> {
    Store::init(path).map_err(|e| e.to_string())?;
    Ok(())
}

/// Add or update a template, committing the change. Returns the commit sha.
/// @noRd
#[extendr]
fn rs_add(
    path: &str,
    name: &str,
    title: Nullable<String>,
    fields: Robj,
    body: &str,
    message: &str,
) -> extendr_api::Result<String> {
    let store = Store::open(path).map_err(|e| e.to_string())?;
    let meta = build_meta(title, &fields)?;
    let commit = store
        .add(name, &meta, body, message)
        .map_err(|e| e.to_string())?;
    Ok(commit)
}

/// Render a template with variables substituted, or return its raw body.
/// @noRd
#[extendr]
fn rs_get(path: &str, name: &str, vars: Robj, raw: bool) -> extendr_api::Result<String> {
    let store = Store::open(path).map_err(|e| e.to_string())?;
    let entry = store.get(name).map_err(|e| e.to_string())?;
    if raw {
        return Ok(entry.body);
    }
    let ctx: BTreeMap<String, String> = robj_to_pairs(&vars)?.into_iter().collect();
    let rendered = template::render(&entry.body, &ctx).map_err(|e| e.to_string())?;
    Ok(rendered)
}

/// List every template in the store as a `name`/`title` data frame.
/// @noRd
#[extendr]
fn rs_list(path: &str) -> extendr_api::Result<Robj> {
    let store = Store::open(path).map_err(|e| e.to_string())?;
    let entries = store.list().map_err(|e| e.to_string())?;
    let names: Vec<Rstr> = entries.iter().map(|e| Rstr::from(e.name.clone())).collect();
    let titles: Vec<Rstr> = entries
        .iter()
        .map(|e| Rstr::from(e.meta.title.clone()))
        .collect();
    Ok(data_frame!(name = names, title = titles))
}

/// Filter templates by frontmatter field (AND of all pairs in `fields`).
/// @noRd
#[extendr]
fn rs_search(path: &str, fields: Robj) -> extendr_api::Result<Robj> {
    let store = Store::open(path).map_err(|e| e.to_string())?;
    let mut query = Query::new();
    for (k, v) in robj_to_pairs(&fields)? {
        query = query.with(k, v);
    }
    let entries = store.list().map_err(|e| e.to_string())?;
    let matched: Vec<_> = entries
        .into_iter()
        .filter(|e| query.matches(&e.meta))
        .collect();
    let names: Vec<Rstr> = matched.iter().map(|e| Rstr::from(e.name.clone())).collect();
    let titles: Vec<Rstr> = matched
        .iter()
        .map(|e| Rstr::from(e.meta.title.clone()))
        .collect();
    Ok(data_frame!(name = names, title = titles))
}

/// Show the commit history for a template as a `commit`/`message` data frame.
/// @noRd
#[extendr]
fn rs_log(path: &str, name: &str) -> extendr_api::Result<Robj> {
    let store = Store::open(path).map_err(|e| e.to_string())?;
    let entries = store.log(name).map_err(|e| e.to_string())?;
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
fn rs_diff(path: &str, name: &str, from: &str, to: &str) -> extendr_api::Result<Robj> {
    let store = Store::open(path).map_err(|e| e.to_string())?;
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

extendr_module! {
    mod lenguar;
    fn rs_init;
    fn rs_add;
    fn rs_get;
    fn rs_list;
    fn rs_search;
    fn rs_log;
    fn rs_diff;
}
