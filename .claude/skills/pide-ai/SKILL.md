---
name: pide-ai
description: Using the Isabelle proof assistant with PIDE AI/PIDE MCP. Use this whenever working with theory files (*.thy) or ML files (*.ML) or when you are dealing with Isabelle in the general sense and have access to PIDE AI.
---

PIDE AI's tools and services (extensions) are customisable. Some named below may be unavailable. Also load skills of extensions.
Call tools via MCP or CLI `pide_ai_cli PORT TOOL 'JSON'`. `list_extensions '{"show_input_schema":true}'` lists tools.
Unlike MCP, the CLI shows progress and you can filter output (e.g. with jq).
Paths are relative to a working directory (MCP: dir at connection, CLI: CWD).

## Sessions
- Sessions may already be running.
- Each has its own base session, directories, options, and state.
- A session has a limited number of worker threads. Commands may be unprocessed only because all workers are busy, e.g. with commands in other theories: check get_session_state.
- Prefer unloading a stuck theory over session restart.

## Edits and synchronisation
- file_watcher service synchronises disk edits of loaded files. Most tools synchronise the file they work on immediately.
- Edits re-check all later commands, also in dependent theories: check beyond the edited range.
- PIDE AI uses symbols (e.g. ∀ vs. \<forall>) as per options pide_ai_unicode_symbols (server) and pide_ai_unicode_symbols_disk (disk). Inputs may use either. Non-PIDE Isabelle tools may require ASCII: recode with `isabelle symbols`.

## Checking state
- Check your changes: get_state for local information (errors, goals, sorrys in a file), get_session_state for global information (all theories' status, processing commands), edit_get_state to edit and get_state for edited commands.
- processing means a command has not finished yet, not that it succeeded.
- A theory status ok means all commands finished without errors. It may still contain sorrys (reported as bad, not as errors), proofs aborted with oops (not reported), and warnings.
- Errors appear in two forms: error messages and failed commands. A failed command might not have an error message.
- Request only information you need and set limits: details for a whole theory flood your context.
- Only commands in the PIDE perspective run automatically tried tools (e.g. quickcheck, solve_direct).
- Isar diagnostic commands (try0, sledgehammer, print_facts, find_theorems,...) work: their output appears under infos in get_state (poll or use timeout).
