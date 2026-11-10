PIDE AI NEWS -- history of user-relevant changes
================================================

New in this PIDE AI version
---------------------------

* Update to Isabelle version as specified in [ISABELLE\_VERSION](./ISABELLE_VERSION)

New in PIDE AI 2026
----------------------------

* Tool arguments and outputs now consistently take milliseconds. INCOMPATIBILITY.
* Show slow commands in `get_state`.
* `get_session_state` can now show bad positions and slow commands.
* "origin" is consistently renamed to "node". INCOMPATIBILITY.
* Tool `find_entities` is renamed to `ranged_origins`. INCOMPATIBILITY.
* New tools `export` and `build_document`.
* `pide_ai_await_option_sessions` now defaults to false. INCOMPATIBILITY.
* `pide_ai_unicode_symbols_disk` now defaults to `true`: files are written with Unicode symbols. Use `isabelle symbols` to encode. INCOMPATIBILITY.
* CLI client now takes arguments as `KEY=STRING` and `KEY:=JSON` besides a JSON object.
* Command protocol requests now use MCP tools/call format. INCOMPATIBILITY.
* CLI client now returns result on stdout, and progress and errors on stderr. INCOMPATIBILITY.
* New `blocked_by` in states for theories that cannot be processed.
* Key `cmds_bad` is renamed to `bad` and `occurrences` to `items`. `cmds_processing` lists `long_running` commands with source and timing, the rest as `short_running_lines`. INCOMPATIBILITY.
* New state tool summary reports. 
* `get_state`: new `show_ancestor_thys`, `show_descendant_thys`, `show_other_cmds_processing`, and sendbacks support.
* Tool handles now receive the client's context. INCOMPATIBILITY.
* Improved all tools and skills for more efficient token usage. INCOMPATIBILITY.
* New client context, including a client's working directory (cwd). Paths are now parsed and printed relative to cwd. INCOMPATIBILITY.
* Option `pide_ai_long_running_threshold` is split into `pide_ai_build_long_running_threshold` and `pide_ai_log_long_running_threshold` for the server's console and log file. INCOMPATIBILITY.
* New option `pide_ai_build_report_nodes_status_delay` to set the minimum delay between progress reports of session builds.
* New option `pide_ai_unicode_symbols` to represent Isabelle symbols in the server via Unicode or ASCII.
* Repository renamed to `isabelle-pide-ai` (old URLs redirect).
* PIDE MCP is renamed to PIDE AI.
* Skill `pide-mcp` is renamed to `pide-ai`. INCOMPATIBILITY.
* Core moved to `AI` (`isabelle pide_ai`). `MCP` only contains the MCP front end (`isabelle pide_mcp`).
* Options `pide_mcp_*` are renamed to `pide_ai_*`. INCOMPATIBILITY.
* Extension API: package `isabelle.pide.mcp` is renamed to `isabelle.pide.ai`, classes `PIDE_MCP_Tool`, `PIDE_MCP_Service`, etc. to `PIDE_AI_*`;
    extensions require `env:ISABELLE_PIDE_AI_JAR` instead of `env:ISABELLE_PIDE_MCP_JAR`. INCOMPATIBILITY.
* Clients moved from `MCP/clients` to `AI/clients`.
* `pide_mcp -p PORT` also serves all enabled protocols on TCP port `PORT`.
    If `PORT` is bound, `pide_mcp -p PORT` connects to the server running there.
* New Isabelle tool `isabelle pide_ai` to run a server on a TCP port.
* New command-based protocol and clients (in `MCP/clients`).
* New TCP support and protocol extension mechanism, configurable via `pide_mcp_protocols_include` and `pide_mcp_protocols_exclude`.
    The TCP port serves all enabled protocols.
* Tool calls with unknown arguments now fail. INCOMPATIBILITY.
* `pide_mcp_progress_threshold` renamed to `pide_mcp_long_running_threshold`. INCOMPATIBILITY.
* New tool `edit_get_state`, combining `edit` and `get_state` to simplify a common request pairing.
* `get_state` now takes `timeout` to await termination or failure of commands. Commands edited during the request are omitted.
    New option `pide_mcp_get_state_delay` to set delay.
* `get_state`, `find_entities`, and `sledgehammer` synchronise with disk.
* New tool `list_extensions` to list enabled services and tools.
* New service `file_watcher`: changes of loaded files on disk are synced with their PIDE state.
    Use option `pide_mcp_file_watcher_delay` to configure the polling delay.
* New `service` extensions with per-session start and stop hooks, configurable via options `pide_mcp_services_include` and `pide_mcp_services_exclude`.
* New option `pide_mcp_unicode_symbols_disk` to store Isabelle symbols in source files via Unicode or ASCII. Symbols are now also recoded in `.sml` and `.sig` files.
* New Isabelle tool `isabelle symbols` to encode or decode Isabelle symbols in files.
* New option `pide_mcp_tools_exclude` to disable tools at startup time. Option `pide_mcp_tools` is renamed to `pide_mcp_tools_include`. INCOMPATIBILITY.
* `get_session_state` no longer reports summary statistics over all sessions. INCOMPATIBILITY.
* More compact `get_state`: by default, only a summary is returned instead of full commands.
    Empty commands are neither returned nor counted. New `details` replacing `include_infos`, `include_types`, `include_facts`, and `include_full_markup`.
    New `details_problematic_limit`, `summary_problematic_limit`, and `include_summary_warnings`.
    `commands_limit` and `snippet_lines` are renamed to `details_unproblematic_limit` and `summary_snippet_lines`. INCOMPATIBILITY.
* Running commands are now reported as `processing`. Shorter output keys (e.g. `cmds_*`, `thys_*`, `src`). INCOMPATIBILITY.
* `get_session_state`: `include_commands_running` is renamed to `include_cmds_processing`; `include_theory_status` to `include_thy_status`; `include_loaded_theories` to `include_loaded_thys`.
    Processing commands are grouped by origin. INCOMPATIBILITY.
* `find_entities`: new `kinds` to select entity kinds. Results are grouped by definition site.
* `create_scratch`: `extension` defaults to `.thy`. Files are named `Scratch_<timestamp>`. INCOMPATIBILITY.
* Edits now decode automatically.
* Add Isabelle/ML skill file.
* Reading or editing a theory loads its unloaded files.
* `unload` now also unloads files of unloaded theories unless a remaining theory still loads them.
* Add [Tool\_Extensions](./Tool_Extensions) with experimental sledgehammer tool as an example user-level tool extension.
* Add overlay and query operations support.
* Remove `show_results` default value. Set options manually if required. INCOMPATIBILITY.
* Enable `show_states` only for live sessions, not for base session builds. Disable `editor_output_state` to avoid duplicate states. INCOMPATIBILITY.
* More robust session start and stop. Introduce new phase `building`. `stop_session` can now also stop sessions that are not running.
* Failing tool calls now return a result with `isError` instead of a JSON-RPC error. INCOMPATIBILITY.
* Tool results are now also returned as `structuredContent`.
* New options `pide_mcp_await_option_sessions` and `pide_mcp_exit_on_failed_option_sessions` control whether sessions given as command-line options are prepared before the MCP server starts
    and whether the MCP server stops if one such sessions fails to start.
* Command-line option `-L` now logs to the file in addition to the console. INCOMPATIBILITY.
* New command-line option `-w` logs MCP requests and responses while `-v` no longer does. INCOMPATIBILITY.
* Support of `notifications/cancelled` for long-running tool calls.
* Long-running operations now report progress, configurable with options `pide_mcp_session_progress_delay` 
    and `pide_mcp_progress_threshold`.
* New option `pide_mcp_tools` to enable/disable tools at startup time.
* New tool `get_session_state` shows session information, status, progress, and resource statistics, superseding tools `list_loaded_theories` and `list_session_directories`. INCOMPATIBILITY.
    Runtime statistics are reported as averages and maxima over a sampling window,
    configurable via option `pide_mcp_session_statistics_limit`.
* New command-line option `-S` starts further sessions on startup.
    Isabelle system options given outside `-S` are inherited by the sessions given via `-S`.
    An invocation with only Isabelle system options does not start a session.
    Use `-S ""` to force a session start in such cases. INCOMPATIBILITY.
* Support for multi session handling and new tools `start_session`, `stop_session`, and `list_sessions`.
    Existing tools now take an optional session id, which may be omitted when exactly one session runs.
    `PIDE_MCP_Tool`s now receive several sessions. INCOMPATIBILITY.
* Update `get_state`: include tracing, remove unprocessed command details, and fix wrong message line number attribution for blobs.
* Set visible perspective on last read or edited origin only. Dependencies are hidden. Use option `pide_mcp_range_context` to adjust number of lines around range that stay visible.
* Await stable snapshot after editing operations (without blocking other tools)
* Reworked loading and editing: closes several issues reported by Diego Marmsoler and Yong Kiam Tan (stuck process due to invalid imports and de-synchronisation of session and resources, edit race conditions, etc.)
* New tool `unload`: Remove ("clean") a theory and its dependents.
* Add tool plugin system: users can freely add and remove (new) tools.
* The `edit` tool now matches substrings within a given range and can edit multiple occurrences at once.
    This is in line with how most coding agents implement their edit tools.

New in PIDE MCP 2025-2
----------------------------

* Port of PIDE MCP c13a4bd3c018 compatible with Isabelle2025-2
* Includes all post PIDE MCP c13a4bd3c018 updates until cd672ac6e31 (19.10.2026).

New in PIDE MCP c13a4bd3c018
----------------------------

* Initial release compatible with Isabelle/c13a4bd3c018
